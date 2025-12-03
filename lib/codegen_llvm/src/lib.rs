use core::cell::RefCell;
use std::borrow::Cow;
use std::collections::HashMap;
use std::sync::{Arc, OnceLock};

use hir::expr::{CmpOp, ExpressionKind};
use hir::node_map::HirNodeKind;
use hir::stmt::StatementKind;
use hir::{Constness, HirId, ItemKind, Param, PathDef, Statement, Type};
use interner::Symbol;
use lexer::unescaped::Unescaped;
use llvm::core::{BasicBlock, Function, Global};
use llvm::ffi::{LLVMIntPredicate, LLVMLinkage, LLVMRealPredicate, LLVMShutdown};
use llvm::Value;
use semantic::{PrimitiveType, TypeId, TypeKind};
use span::source::{FileId, SourceMap};
use tiny_str::TinyString;
use tiny_vec::TinyVec;

pub struct Codegen {
    llvm_ctx: llvm::Context
}

impl Codegen {
    pub fn new() -> Self {
        Self { llvm_ctx: llvm::Context::new() }
    }

    pub fn codegen<'llvm, 'hir>(
        &'llvm self,
        hir: &hir::Session<'hir>,
        semantic: &semantic::Semantic<'hir>,
        src: &SourceMap,
    ) -> HashMap<FileId, llvm::Module<'llvm>> {
        let mut ctx = CodegenState::new(hir, semantic, src, &self.llvm_ctx);
        let root = hir.get_root();
        root.codegen(&mut ctx);
        RefCell::into_inner(Arc::into_inner(ctx.modules).unwrap())
    }
}

impl Default for Codegen {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for Codegen {
    fn drop(&mut self) {
        static SHUTDOWN_ONCE: OnceLock<()> = OnceLock::new();
        SHUTDOWN_ONCE.get_or_init(|| {
            unsafe { LLVMShutdown(); }
        });
    }
}

struct CodegenState<'cg, 'llvm, 'hir> {
    curr_mod: Option<llvm::Module<'llvm>>,
    curr_func: Option<Function<'llvm>>,
    curr_builder: Option<llvm::Builder<'llvm>>,
    hir: &'cg hir::Session<'hir>,

    llvm_ctx: &'llvm llvm::Context,

    current_loop_end: Option<BasicBlock<'llvm>>,
    current_loop_cond: Option<BasicBlock<'llvm>>,

    mangling: Vec<Symbol>,
    semantic: &'cg semantic::Semantic<'hir>,

    mangled_syms: Arc<RefCell<HashMap<HirId, String>>>,
    globals: Arc<RefCell<HashMap<HirId, Value<'llvm>>>>,
    allocas: HashMap<HirId, Value<'llvm>>,
    params: HashMap<HirId, Value<'llvm>>,

    src: &'cg SourceMap,
    modules: Arc<RefCell<HashMap<FileId, llvm::Module<'llvm>>>>,

    types: HashMap<TypeId, llvm::Type<'llvm>>,
}

impl<'cg, 'llvm, 'hir> CodegenState<'cg, 'llvm, 'hir> {
    pub fn new(
        hir: &'cg hir::Session<'hir>,
        semantic: &'cg semantic::Semantic<'hir>,
        src: &'cg SourceMap,
        llvm_ctx: &'llvm llvm::Context
    ) -> Self
    {
        Self {
            hir,
            curr_mod: None,
            curr_builder: None,
            curr_func: None,
            mangling: Vec::new(),
            mangled_syms: Default::default(),
            semantic,
            llvm_ctx,
            src,
            params: HashMap::new(),
            allocas: HashMap::new(),
            globals: Default::default(),
            modules: Default::default(),
            types: Default::default(),
            current_loop_end: None,
            current_loop_cond: None,
        }
    }

    fn module(&mut self) -> &mut llvm::Module<'llvm> {
        self.curr_mod.as_mut().unwrap()
    }

    fn get_mangled(&mut self, id: HirId) -> Option<String> {
        self.mangled_syms.borrow().get(&id).cloned()
    }

    fn mangle_symbol(&mut self, id: HirId, name: Symbol) -> String {
        use core::fmt::Write;

        self.mangled_syms.borrow_mut().entry(id).or_insert_with(|| {
            let mut mangled = String::new();
            let symbols = self.mangling.iter().chain([&name]);

            for (i, pref) in symbols.enumerate() {
                if i > 0 {
                    mangled.push('.');
                }
                write!(mangled, "{pref}").unwrap();
            }

            mangled
        })
        .clone()
    }

    fn enter(&mut self, sym: Symbol) {
        self.mangling.push(sym);
    }

    fn exit(&mut self) {
        self.mangling.pop();
    }

    fn enter_extern(mut self, module: &'hir hir::Module<'hir>) -> Self {
        let name = self.mangle_symbol(module.id, module.name.ident.sym);
        self.curr_mod = Some(self.llvm_ctx.create_module(&name));
        if module.name.ident.sym != "root" {
            self.enter(module.name.ident.sym);
        }
        self.curr_func = None;
        self.curr_builder = None;
        self
    }

    fn builder(&mut self) -> &mut llvm::Builder<'llvm> { self.curr_builder.as_mut().unwrap() }

    fn function(&mut self) -> &mut Function<'llvm> { self.curr_func.as_mut().unwrap() }

    fn address_of(&mut self, id: HirId, name: Symbol) -> Value<'llvm> {
        debug_assert!(matches!(self.hir.get_node(&id), HirNodeKind::Param(_) | HirNodeKind::Item(_)));

        if let Some(param) = self.params.get(&id).copied() {
            let ty = param.get_type();
            let builder = self.builder();
            let alloca = name.borrow(|name| {
                builder.alloca(ty, name)
            });
            builder.store(param, alloca);
            self.allocas.insert(id, alloca);
            self.params.remove(&id);
            alloca
        }
        else if let Some(global) = self.get_global_value(id) {
            global
        }
        else {
            *self.allocas.get(&id).unwrap()
        }
    }

    fn global_var(&mut self, vdecl: &'hir hir::Item<'hir>) -> Global<'llvm> {
        let ItemKind::Variable { name, init, constness, .. } = vdecl.kind else {
            unreachable!()
        };

        let name = self.mangle_symbol(vdecl.id, name.ident.sym);

        let ty = self.semantic.type_of(&vdecl.id).unwrap().codegen(self);
        let mut global = self.module().add_global(ty, &name);

        if let Some(init) = init {
            let old_b = self.curr_builder.replace(self.llvm_ctx.create_builder());
            let init = init.value(self);
            global.set_intializer(init);
            self.curr_builder = old_b;
        }

        global.set_constant(
            matches!(constness, Constness::Const)
        );

        self.globals.borrow_mut().insert(vdecl.id, *global.as_value());

        global
    }

    fn get_global_value(&self, id: HirId) -> Option<Value<'llvm>> {
        self.globals.borrow().get(&id).copied()
    }
}

impl Clone for CodegenState<'_, '_, '_> {
    fn clone(&self) -> Self {
        Self {
            curr_mod: None,
            curr_func: None,
            curr_builder: None,
            current_loop_cond: None,
            current_loop_end: None,
            llvm_ctx: self.llvm_ctx,
            mangled_syms: Arc::clone(&self.mangled_syms),
            hir: self.hir,
            semantic: self.semantic,
            params: HashMap::new(),
            src: self.src,
            allocas: HashMap::new(),
            mangling: self.mangling.clone(),
            modules: Arc::clone(&self.modules),
            globals: Arc::clone(&self.globals),
            types: self.types.clone(),
        }
    }
}

trait CG<'hir, 'llvm> {
    type Output;

    fn codegen(&self, cg: &mut CodegenState<'_, 'llvm, 'hir>) -> Self::Output;
}

impl<'hir> CG<'hir, '_> for &'hir hir::Module<'hir> {
    type Output = ();

    fn codegen(&self, cg: &mut CodegenState<'_, '_, 'hir>) {
        if let Some(id) = self.extern_file {
            let mut cg = cg.clone().enter_extern(self);
            for item in self.items {
                item.codegen(&mut cg);
            }
            cg.modules.borrow_mut().insert(id, cg.curr_mod.unwrap());
        } else {
            let sym = self.name.ident.sym;
            if sym != "root" {
                cg.enter(sym);
            }
            for item in self.items {
                item.codegen(cg);
            }
            cg.exit();
        }
    }
}

trait Address<'cg, 'hir> {
    fn address(&'hir self, cg: &mut CodegenState<'_, 'cg, 'hir>) -> llvm::Value<'cg>;
}

impl<'cg, 'hir> Address<'cg, 'hir> for hir::Item<'hir> {
    fn address(&self, cg: &mut CodegenState<'_, 'cg, '_>) -> llvm::Value<'cg> {
        match self.kind {
            ItemKind::Variable { name, .. } => {
                cg.address_of(self.id, name.ident.sym)
            }
            ItemKind::Function { is_extern, name, .. } => {
                name.ident.sym.borrow(|name| {
                    let name = if is_extern {
                        Cow::Borrowed(name)
                    } else {
                        Cow::Owned(cg.get_mangled(self.id).unwrap())
                    };
                    if let Some(func) = cg.module().get_function(&name) {
                        *func.as_value()
                    } else {
                        let fty = cg.semantic.type_of(&self.id).unwrap();
                        let fty = fty.codegen(cg);
                        *cg.module().add_function(&name, fty).as_value()
                    }
                })
            }
            ItemKind::Use(u) => {
                let id = u.def().expect_resolved();
                let node = cg.hir.get_node(&id);
                match node {
                    HirNodeKind::Expr(expression) => expression.address(cg),
                    HirNodeKind::Item(item) => item.address(cg),
                    HirNodeKind::Param(_) | HirNodeKind::PathDef(_) |
                    HirNodeKind::Stmt(_) | HirNodeKind::Field(_) |
                    HirNodeKind::Ty(_) | HirNodeKind::Module(_)
                        => unreachable!("Can't get address of node with kind: {}", node.get_name())
                }
            }
            ItemKind::TypeAlias { .. } => unreachable!("Can't get address of a type alias"),
            ItemKind::Mod(_) => unreachable!("Can't get address of a module"),
            ItemKind::Struct { .. } => unreachable!("Can't get address of a struct definition"),
        }
    }
}

impl<'cg, 'hir> Address<'cg, 'hir> for hir::Expression<'hir> {
    fn address(&'hir self, cg: &mut CodegenState<'_, 'cg, 'hir>) -> llvm::Value<'cg> {
        use hir::expr::ExpressionKind as EK;
        match &self.kind {
            EK::Variable(path) => {
                let def = path.def().expect_resolved();
                match cg.hir.get_node(&def) {
                    HirNodeKind::Item(item) => item.address(cg),
                    HirNodeKind::Param(p) => {
                        cg.address_of(p.id, p.name.ident.sym)
                    }
                    _ => unreachable!(),
                }
            },
            EK::ArrayAccess { arr, index } => {
                let arr_ty = cg.semantic.type_of(&arr.id).unwrap();
                let arr_ptr = if arr_ty.is_pointer() {
                    arr.value(cg)
                } else {
                    arr.address(cg)
                };

                let index = index.value(cg);
                let ty = cg.semantic.type_of(&self.id).unwrap().codegen(cg);
                cg.builder().gep(ty, arr_ptr, &mut [index], "tmp_gep")
            },
            EK::StructAccess { st, field } => {
                let sty = cg.semantic.type_of(&st.id).unwrap();
                let idx = sty.field_index_of(field.sym).unwrap();
                let st = st.address(cg);
                let sty = *cg.types.get(&sty.id).unwrap();

                let zero = Value::const_int(llvm::Type::int_32(cg.llvm_ctx), 0);
                let idx = Value::const_int(llvm::Type::int_32(cg.llvm_ctx), idx as u64);
                cg.builder().gep(sty, st, &mut [zero, idx], "tmp_gep")

            },
            EK::Deref(expr) => expr.value(cg),
            _ => unreachable!("Can't get address of {self:?}")
        }
    }
}

trait CGValue<'cg, 'hir, 'llvm> {
    fn value_opt(&'hir self, cg: &mut CodegenState<'cg, 'llvm, 'hir>) -> Option<llvm::Value<'llvm>>;
    fn value(&'hir self, cg: &mut CodegenState<'cg, 'llvm, 'hir>) -> llvm::Value<'llvm> {
        self.value_opt(cg).expect("Expected a value")
    }
}

trait CGExecute<'hir> {
    fn execute(&'hir self, cg: &mut CodegenState<'_, '_, 'hir>);
}

fn cmp_op_to_int_pred(op: CmpOp) -> LLVMIntPredicate {
    match op {
        CmpOp::Gt => LLVMIntPredicate::LLVMIntSGT,
        CmpOp::Ge => LLVMIntPredicate::LLVMIntSGE,
        CmpOp::Lt => LLVMIntPredicate::LLVMIntSLT,
        CmpOp::Le => LLVMIntPredicate::LLVMIntSLE,
        CmpOp::Eq => LLVMIntPredicate::LLVMIntEQ,
        CmpOp::Neq => LLVMIntPredicate::LLVMIntNE,
    }
}

fn cmp_op_to_uint_pred(op: CmpOp) -> LLVMIntPredicate {
    match op {
        CmpOp::Gt => LLVMIntPredicate::LLVMIntUGT,
        CmpOp::Ge => LLVMIntPredicate::LLVMIntUGE,
        CmpOp::Lt => LLVMIntPredicate::LLVMIntULT,
        CmpOp::Le => LLVMIntPredicate::LLVMIntULE,
        CmpOp::Eq => LLVMIntPredicate::LLVMIntEQ,
        CmpOp::Neq => LLVMIntPredicate::LLVMIntNE,
    }
}

fn cmp_op_to_real_pred(op: CmpOp) -> LLVMRealPredicate {
    match op {
        CmpOp::Gt => LLVMRealPredicate::LLVMRealOGT,
        CmpOp::Ge => LLVMRealPredicate::LLVMRealOGE,
        CmpOp::Lt => LLVMRealPredicate::LLVMRealOLT,
        CmpOp::Le => LLVMRealPredicate::LLVMRealOLE,
        CmpOp::Eq => LLVMRealPredicate::LLVMRealOEQ,
        CmpOp::Neq => LLVMRealPredicate::LLVMRealONE,
    }
}

impl<'cg, 'llvm, 'hir> CGValue<'cg, 'hir, 'llvm> for hir::Expression<'hir> {
    #[allow(clippy::too_many_lines)]
    fn value_opt(&'hir self, cg: &mut CodegenState<'cg, 'llvm, 'hir>) -> Option<llvm::Value<'llvm>> {
        use hir::expr::ExpressionKind as EK;
        use hir::expr::{UnaryOp, LogicalOp, ArithmeticOp, LitValue};

        Some(
        match &self.kind {
            EK::Array(_) => todo!(),
            EK::Unary { op, expr } => {
                let val = expr.value(cg);
                match op {
                    UnaryOp::Not => cg.builder().not(val, "tmp_not"),
                    UnaryOp::Neg => cg.builder().neg(val, "tmp_neg"),
                }
            },
            EK::Ref(expr) => {
                expr.address(cg)
            }
            EK::Deref(expr) => {
                let ty = cg.semantic.type_of(&self.id).unwrap().codegen(cg);
                let expr = expr.value(cg);
                cg.builder().load(expr, ty, "tmp_deref")
            }
            EK::Logical { left, op, right } => {
                let left = left.value(cg);
                let right = right.value(cg);
                match op {
                    LogicalOp::And => cg.builder().and(left, right, "tmp_and"),
                    LogicalOp::Or => cg.builder().or(left, right, "tmp_or"),
                }
            }
            EK::Comparison { left, op, right } => {
                let lty = cg.semantic.type_of(&left.id).unwrap();
                let is_int = lty.is_integer();
                let is_signed = lty.is_signed();

                let left = left.value(cg);
                let right = right.value(cg);
                if is_int {
                    let pred = if is_signed {
                        cmp_op_to_int_pred(*op)
                    } else {
                        cmp_op_to_uint_pred(*op)
                    };
                    cg.builder().integer_cmp(left, right, pred, "tmp_cmp")
                } else {
                    let pred = cmp_op_to_real_pred(*op);
                    cg.builder().real_cmp(left, right, pred, "tmp_cmp")
                }
            }
            EK::Arithmetic { left, op, right } => {
                let lhs = left.value(cg);
                let rhs = right.value(cg);
                match op {
                    ArithmeticOp::Add => cg.builder().add(lhs, rhs, "tmp_add"),
                    ArithmeticOp::Sub => cg.builder().sub(lhs, rhs, "tmp_sub"),
                    ArithmeticOp::Mul => cg.builder().mul(lhs, rhs, "tmp_mul"),
                    ArithmeticOp::Div => {
                        let lty = cg.semantic.type_of(&left.id).unwrap();
                        let is_int = lty.is_integer();
                        let is_signed = lty.is_signed();
                        if is_int {
                            if is_signed {
                                cg.builder().sint_div(lhs, rhs, "tmp_div")
                            } else {
                                cg.builder().uint_div(lhs, rhs, "tmp_div")
                            }
                        } else {
                            cg.builder().fdiv(lhs, rhs, "tmp_div")
                        }
                    }
                    ArithmeticOp::Mod => {
                        let lty = cg.semantic.type_of(&left.id).unwrap();
                        let is_int = lty.is_integer();
                        let is_signed = lty.is_signed();
                        if is_int {
                            if is_signed {
                                cg.builder().sint_rem(lhs, rhs, "tmp_rem")
                            } else {
                                cg.builder().uint_rem(lhs, rhs, "tmp_rem")
                            }
                        } else {
                            cg.builder().frem(lhs, rhs, "tmp_rem")
                        }
                    }
                }
            }
            EK::Block { stmts, tail } => {
                for stmt in *stmts {
                    stmt.codegen(cg);
                }

                return tail.map(|t| t.value(cg))
            }
            EK::If { cond, if_true, if_false } => {
                return codgen_if(cg, cond, if_true, *if_false)
            }
            EK::Assignment { left, right } => {
                let left = left.address(cg);
                let right = right.value(cg);
                cg.builder().store(right, left);
                right
            }
            EK::Variable(path) => {
                let id = path.def().expect_resolved();
                let node = cg.hir.get_node(&id);
                if let HirNodeKind::Param(p) = node
                    && let Some(p) = cg.params.get(&p.id)
                {
                        return Some(*p)
                }
                let addr = match node {
                    HirNodeKind::Item(item) => item.address(cg),
                    HirNodeKind::Param(param) => cg.address_of(param.id, param.name.ident.sym),
                    _ => unreachable!(),
                };
                let ty = cg.semantic.type_of(&self.id).unwrap().codegen(cg);
                let name = match cg.hir.get_node(&id) {
                    HirNodeKind::Item(item) => item.get_name().unwrap(),
                    HirNodeKind::Param(param) => param.name.ident.sym,
                    _ => unreachable!()
                };
                let name = name.borrow(|name| {
                    format!("load_{name}")
                });
                cg.builder().load(addr, ty, &name)
            }
            EK::Literal(val) => {
                match val {
                    LitValue::Int(ival) => {
                        let bytes = i64::from(*ival).to_le_bytes();
                        llvm::Value::const_int32(u64::from_le_bytes(bytes), cg.llvm_ctx)
                    },
                    LitValue::Float(f) => llvm::Value::const_f64(*f, cg.llvm_ctx),
                    LitValue::Bool(val) => llvm::Value::const_int1(u64::from(*val), cg.llvm_ctx),
                    LitValue::Str(s) => s.borrow(|literal| {
                        let literal = &literal[1..literal.len() - 1];
                        let literal: TinyString = Unescaped::from(literal).collect();
                        let s = llvm::Value::const_string(&literal, true, cg.llvm_ctx);
                        let ty = s.get_type();
                        let mut global = cg.curr_mod.as_mut().unwrap().add_global(ty, "LitStr");
                        global.set_intializer(s);
                        global.set_constant(true);
                        global.set_linkage(LLVMLinkage::LLVMLinkerPrivateLinkage);
                        let val = *global.as_value();
                        let zero = llvm::Value::const_int(llvm::Type::int_32(cg.llvm_ctx), 0);
                        cg.builder().gep(ty, val, &mut [zero, zero][..], "lit_ptr")
                    }),
                    LitValue::Char(val) => llvm::Value::const_int1(u64::from(*val), cg.llvm_ctx),
                }
            }
            EK::Call { callee, args } => {
                let fty = cg.semantic.type_of(&callee.id).unwrap();
                let func_ty = fty.codegen(cg);
                let func = callee.address(cg);
                let mut args: TinyVec<_, 8> = args.iter().map(|expr| {
                    expr.value(cg)
                }).collect();

                let name = if fty.as_function_type().unwrap().2.is_empty_type() {
                    ""
                } else {
                    "tmp_call"
                };
                cg.builder().call(func_ty, func, &mut args, name)
            }
            EK::Cast { expr, .. } => {
                // TODO: There are other types of casts. Look them up!
                let value = expr.value(cg);
                let src_ty = cg.semantic.type_of(&expr.id).unwrap();
                let dst_ty = cg.semantic.type_of(&self.id).unwrap();

                let to = dst_ty.codegen(cg);

                if src_ty.is_integer() && dst_ty.is_integer() {
                   cg.builder().int_cast(&value, &to, dst_ty.is_signed(), "tmp_cast")
                } else {
                    cg.builder().bit_cast(&value, &to, "tmp_cast")
                }
            }
            EK::ArrayAccess { .. } | EK::StructAccess { .. } =>
            {
                let gep = self.address(cg);
                let ty = cg.semantic.type_of(&self.id).unwrap().codegen(cg);
                cg.builder().load(gep, ty, "tmp_load")
            },
        }
        )
    }
}

impl<'hir> CGExecute<'hir> for hir::Expression<'hir> {
    fn execute(&'hir self, cg: &mut CodegenState<'_, '_, 'hir>) {
        use hir::expr::ExpressionKind as EK;

        match &self.kind {
            EK::Array(expressions) => expressions.iter().for_each(|expr| expr.execute(cg)),
            EK::Unary { expr, .. } => expr.execute(cg),
            EK::Ref(expression) | EK::Deref(expression) => {
                expression.execute(cg);
            }
            EK::Logical { left, right, .. } |
                EK::Comparison { left, right, .. } |
                EK::Arithmetic { left, right, .. } => {
                    left.execute(cg);
                    right.execute(cg);
                }
            EK::Assignment { .. } | EK::Call { .. } => {
                self.value(cg);
            }
            EK::Variable(_) | EK::Literal(_) => {},
            EK::Cast { expr, .. } => {
                expr.execute(cg);
            }
            EK::ArrayAccess { arr, .. } => arr.execute(cg),
            EK::StructAccess { st, .. } => st.execute(cg),
            EK::Block { .. } |
            EK::If { .. } => {
                self.value_opt(cg);
            }
        }
    }
}

fn codgen_if<'cg, 'hir, 'llvm>(
    cg: &mut CodegenState<'cg, 'llvm, 'hir>,
    cond: &'hir hir::Expression<'hir>,
    if_true: &'hir hir::Expression<'hir>,
    if_false: Option<&'hir hir::Expression<'hir>>
) -> Option<Value<'llvm>>
{
    let mut if_block = cg.llvm_ctx.create_basic_block("if.then");
    let mut else_block = cg.llvm_ctx.create_basic_block("if.else");
    let mut end_block = cg.llvm_ctx.create_basic_block("if.end");

    let alloca =
        if if_false.is_some() {
            let ty = cg.semantic.type_of(&if_true.id).unwrap().codegen(cg);
            Some((ty, cg.builder().alloca(ty, "synthetic_if_value")))
        } else {
            None
        };

    let cond = cond.value(cg);
    cg.builder().cond_br(cond, &if_block, &else_block);
    cg.function().append_existing_basic_block(&if_block);
    cg.builder().position_at_end(&mut if_block);

    let ExpressionKind::Block { stmts, tail } = if_true.kind else { unreachable!() };
    stmts.iter().for_each(|stmt| stmt.codegen(cg));
    if let Some((_, alloca)) = alloca {
        let val = tail.unwrap().value(cg);
        cg.builder().store(val, alloca);
    }
    cg.builder().branch(&mut end_block);

    cg.function().append_existing_basic_block(&else_block);
    cg.builder().position_at_end(&mut else_block);

    if let Some(if_false) = if_false {
        let ExpressionKind::Block { stmts, tail } = if_false.kind else { unreachable!() };
        stmts.iter().for_each(|stmt| stmt.codegen(cg));
        if let Some((_, alloca)) = alloca {
            let val = tail.unwrap().value(cg);
            cg.builder().store(val, alloca);
        }
    }
    cg.function().append_existing_basic_block(&end_block);
    cg.builder().branch(&mut end_block);
    cg.builder().position_at_end(&mut end_block);

    alloca.map(|(ty, a)| cg.builder().load(a, ty, "synthetic_load"))
}

impl<'hir, 'cg> CG<'hir, 'cg> for &'hir hir::Statement<'hir> {
    type Output = ();

    #[allow(clippy::too_many_lines)]
    fn codegen(&self, cg: &mut CodegenState<'_, 'cg, 'hir>) {
        match &self.kind {
            StatementKind::Expr(expr) => {
                expr.execute(cg);
            },
            StatementKind::Return(expr) => {
                let val = expr.map(|expr| expr.value(cg));
                cg.builder().ret(val);
            },
            StatementKind::While { cond, body } => {
                let mut loop_cond = cg.llvm_ctx.create_basic_block("while.cond");
                let mut loop_body = cg.llvm_ctx.create_basic_block("while.body");
                let mut loop_end = cg.llvm_ctx.create_basic_block("while.end");

                let old_loop_cond = cg.current_loop_cond.replace(loop_cond);
                let old_loop_end = cg.current_loop_cond.replace(loop_end);

                cg.builder().branch(&mut loop_cond);
                cg.function().append_existing_basic_block(&loop_cond);
                cg.builder().position_at_end(&mut loop_cond);

                let cond = cond.value(cg);
                cg.builder().cond_br(cond, &loop_body, &loop_end);

                cg.function().append_existing_basic_block(&loop_body);
                cg.builder().position_at_end(&mut loop_body);
                body.codegen(cg);
                cg.builder().branch(&mut loop_cond);

                cg.function().append_existing_basic_block(&loop_end);
                cg.builder().position_at_end(&mut loop_end);

                cg.current_loop_cond = old_loop_cond;
                cg.current_loop_end = old_loop_end;
            }
            StatementKind::For { init, cond, inc, body } => {

                if let Some(init) = init {
                    let mut for_init = cg.llvm_ctx.create_basic_block("for.init");
                    cg.builder().branch(&mut for_init);
                    cg.function().append_existing_basic_block(&for_init);
                    cg.builder().position_at_end(&mut for_init);
                    init.codegen(cg);
                }


                let mut for_cond = cg.llvm_ctx.create_basic_block("for.cond");
                let mut for_body = cg.llvm_ctx.create_basic_block("for.body");
                let mut for_end = cg.llvm_ctx.create_basic_block("for.end");
                let mut for_inc = None;

                let old_end = cg.current_loop_end.replace(for_end);
                let old_cond = if inc.is_some() {
                    for_inc = Some(cg.function().append_basic_block("for.inc"));
                    cg.current_loop_cond.replace(for_inc.unwrap())
                } else {
                    cg.current_loop_cond.replace(for_cond)
                };

                cg.builder().branch(&mut for_cond);
                cg.function().append_existing_basic_block(&for_cond);
                cg.builder().position_at_end(&mut for_cond);

                if let Some(cond) = cond {
                    let cond = cond.value(cg);
                    cg.builder().cond_br(cond, &for_body, &for_end);
                } else {
                    cg.builder().branch(&mut for_body);
                }

                cg.function().append_existing_basic_block(&for_body);
                cg.builder().position_at_end(&mut for_body);
                body.codegen(cg);
                if let Some(inc) = inc {
                    cg.builder().branch(&mut for_inc.unwrap());
                    cg.builder().position_at_end(&mut for_inc.unwrap());
                    inc.value(cg);
                }
                cg.builder().branch(&mut for_cond);

                cg.function().append_existing_basic_block(&for_end);
                cg.builder().position_at_end(&mut for_end);

                cg.current_loop_cond = old_cond;
                cg.current_loop_end = old_end;
            }
            StatementKind::Empty => {}
            StatementKind::Break => {
                let mut b = cg.current_loop_end.unwrap();
                cg.builder().branch(&mut b);
            }
            StatementKind::Continue => {
                let mut b = cg.current_loop_cond.unwrap();
                cg.builder().branch(&mut b);
            },
            StatementKind::Item(item) => {
                item.codegen(cg);
            }
        }
    }
}

impl<'hir> CG<'hir, '_> for &'hir hir::Item<'hir> {
    type Output = ();

    fn codegen(&self, cg: &mut CodegenState<'_, '_, 'hir>) {
        match self.kind {
            hir::ItemKind::Mod(module) => module.codegen(cg),
            hir::ItemKind::Variable { name, constness, init, .. } => {
                match constness {
                    Constness::Const => {
                        cg.global_var(self);
                    }
                    Constness::Default => {
                        if cg.curr_func.is_none() {
                            cg.global_var(self);
                            return
                        }
                        let ty = cg.semantic.type_of(&self.id).unwrap();
                        let ty = ty.codegen(cg);
                        name.ident.sym.borrow(|name| {
                            let alloca = cg.builder().alloca(ty, name);
                            cg.allocas.insert(self.id, alloca);
                            if let Some(init) = init {
                                let initializer = init.value(cg);
                                cg.builder().store(initializer, alloca);
                            }
                        });
                    },
                }
            },
            hir::ItemKind::Function { is_extern, is_variadic: _, name, params, ret_ty, body } =>
                codegen_function(cg, self, is_extern, name, params, ret_ty, body),
            hir::ItemKind::Struct { .. } => {
                let struct_type = cg.semantic.type_of(&self.id).unwrap();
                let (name, fields) = struct_type.as_struct_type().unwrap();
                let name = cg.mangle_symbol(self.id, name);
                let mut fields: TinyVec<_, 8> = fields.iter().map(|f| {
                    f.ty.codegen(cg)
                }).collect();
                let s = llvm::Type::struct_named(&name, &mut fields, false, cg.llvm_ctx);
                cg.types.insert(struct_type.id, s);
            }
            hir::ItemKind::TypeAlias { .. } |
            hir::ItemKind::Use(_) => {},
        }
    }
}

impl<'cg> CG<'_, 'cg> for semantic::Ty<'_> {
    type Output = llvm::Type<'cg>;

    fn codegen(&self, cg: &mut CodegenState<'_, 'cg, '_>) -> Self::Output {
        match &self.kind {
            TypeKind::Primitive(prim) => match prim {
                PrimitiveType::I8 => llvm::Type::int(8, cg.llvm_ctx),
                PrimitiveType::I16 => llvm::Type::int(16, cg.llvm_ctx),
                PrimitiveType::I32 => llvm::Type::int(32, cg.llvm_ctx),
                PrimitiveType::I64 => llvm::Type::int(64, cg.llvm_ctx),
                PrimitiveType::U8 => llvm::Type::int(8, cg.llvm_ctx),
                PrimitiveType::U16 => llvm::Type::int(16, cg.llvm_ctx),
                PrimitiveType::U32 => llvm::Type::int(32, cg.llvm_ctx),
                PrimitiveType::U64 => llvm::Type::int(64, cg.llvm_ctx),
                PrimitiveType::F32 => llvm::Type::float_32(cg.llvm_ctx),
                PrimitiveType::F64 => llvm::Type::float_64(cg.llvm_ctx),
                PrimitiveType::Bool => llvm::Type::int_1(cg.llvm_ctx),
                PrimitiveType::Char => llvm::Type::int_1(cg.llvm_ctx),
                PrimitiveType::Empty => llvm::Type::void(cg.llvm_ctx),
            },
            TypeKind::Function { is_variadic, params, ret_ty } => {
                let mut params_tys: TinyVec<_, 8> = params.iter().map(|param| {
                    param.codegen(cg)
                }).collect();
                let ret = ret_ty.codegen(cg);

                llvm::Type::function(ret, &mut params_tys, *is_variadic)
            },
            TypeKind::Ref(to) => {
                llvm::Type::pointer(to.codegen(cg))
            }
            TypeKind::Array(ty, len) => {
                llvm::Type::array(ty.codegen(cg), *len as _)
            }
            TypeKind::Struct { .. } => {
                *cg.types.get(&self.id).unwrap()
            }
        }
    }
}

fn codegen_function<'hir>(
    cg: &mut CodegenState<'_, '_, 'hir>,
    item: &'hir hir::Item<'hir>,
    is_extern: bool,
    name: &PathDef,
    params: &'hir [Param<'hir>],
    ret_ty: &'hir Type<'hir>,
    body: Option<&'hir[Statement<'hir>]>,
) {
    let ty = cg.semantic.type_of(&item.id).unwrap();
    let fty = ty.codegen(cg);

    if is_extern {
        // TODO: Add a no_mangle attribute instead
        name.ident.sym.borrow(|name| {
            cg.module().add_function(name, fty);
        });
        return;
    }

    let mangled_name = cg.mangle_symbol(item.id, name.ident.sym).clone();
    let mut function = cg.module().add_function(&mangled_name, fty);

    cg.enter(name.ident.sym);

    cg.allocas.clear();
    cg.params.clear();

    let mut entry = function.append_basic_block("entry");

    {
        let mut builder = cg.llvm_ctx.create_builder();
        builder.position_at_end(&mut entry);

        for i in 0..function.n_params() {
            let param = function.param(i);
            cg.params.insert(params[i as usize].id, param);
        }

        debug_assert!(cg.curr_builder.is_none());
        cg.curr_builder = Some(builder);

        let old_f = cg.curr_func.replace(function);

        for stmt in body.expect("A non-extern function MUST have a block") {
            stmt.codegen(cg);
        }

        if ret_ty.is_empty() {
            cg.builder().ret(None);
        }

        cg.curr_builder.take();
        cg.curr_func = old_f;
    }

    cg.exit();
    cg.allocas.clear();
    cg.params.clear();
}
