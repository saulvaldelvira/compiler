use core::cell::RefCell;
use std::collections::HashMap;
use std::sync::Arc;

use hir::expr::CmpOp;
use hir::node_map::HirNodeKind;
use hir::stmt::StatementKind;
use hir::{Constness, HirId, ItemKind, Param, PathDef, Statement, Type};
use interner::Symbol;
use llvm::core::Function;
use llvm::ffi::LLVMIntPredicate;
use llvm::Value;
use semantic::{PrimitiveType, TypeId, TypeKind};
use span::source::{FileId, SourceMap};
use tiny_vec::TinyVec;

pub fn codegen<'hir>(hir: &hir::Session<'hir>, semantic: &semantic::Semantic<'hir>, src: &SourceMap) -> HashMap<FileId, llvm::Module> {
    let mut ctx = CodegenCtx::new(hir, semantic, src);
    let root = hir.get_root();
    root.codegen(&mut ctx);
    RefCell::into_inner(Arc::into_inner(ctx.modules).unwrap())
}

struct CodegenCtx<'cg, 'hir> {
    curr_mod: Option<llvm::Module>,
    curr_func: Option<Function>,
    curr_builder: Option<llvm::Builder>,
    hir: &'cg hir::Session<'hir>,

    mangling: Vec<Symbol>,
    semantic: &'cg semantic::Semantic<'hir>,

    mangled_syms: Arc<RefCell<HashMap<HirId, String>>>,
    allocas: HashMap<HirId, Value>,
    params: HashMap<HirId, Value>,

    src: &'cg SourceMap,
    modules: Arc<RefCell<HashMap<FileId, llvm::Module>>>,

    types: HashMap<TypeId, llvm::Type>,
}

impl<'cg, 'hir> CodegenCtx<'cg, 'hir> {
    pub fn new(hir: &'cg hir::Session<'hir>, semantic: &'cg semantic::Semantic<'hir>, src: &'cg SourceMap) -> Self {
        Self {
            hir,
            curr_mod: None,
            curr_builder: None,
            curr_func: None,
            mangling: Vec::new(),
            mangled_syms: Default::default(),
            semantic,
            src,
            params: HashMap::new(),
            allocas: HashMap::new(),
            modules: Default::default(),
            types: Default::default(),
        }
    }

    pub fn module(&mut self) -> &mut llvm::Module {
        self.curr_mod.as_mut().unwrap()
    }

    fn get_mangled(&mut self, id: HirId) -> Option<String> {
        self.mangled_syms.borrow().get(&id).cloned()
    }

    fn mangle_symbol(&mut self, id: HirId, name: Symbol) -> String {
        use core::fmt::Write;

        self.mangled_syms.borrow_mut().entry(id).or_insert_with(|| {
            let mut mangled = String::new();
            let symbols = self.mangling.iter()
                .chain([&name]);

            for (i, pref) in symbols.enumerate() {
                if i > 0 {
                    mangled.push('_');
                }
                write!(mangled, "{pref}").unwrap();
            }

            mangled
        })
        .clone()
    }

    fn enter(&mut self, module: &'hir hir::Module<'hir>) {
        if module.name.ident.sym != "root" {
            self.mangling.push(module.name.ident.sym);
        }
    }

    fn exit(&mut self) {
        self.mangling.pop();
    }

    fn enter_extern(mut self, module: &'hir hir::Module<'hir>) -> Self {
        let name = self.mangle_symbol(module.id, module.name.ident.sym);
        self.curr_mod = Some(llvm::Module::new(&name));
        if module.name.ident.sym != "root" {
            self.mangling.push(module.name.ident.sym);
        }
        self.curr_func = None;
        self.curr_builder = None;
        self
    }

    fn builder(&mut self) -> &mut llvm::Builder { self.curr_builder.as_mut().unwrap() }

    fn address_of(&mut self, id: HirId, name: Symbol) -> Value {
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
        } else {
            *self.allocas.get(&id).unwrap()
        }
    }
}

impl Clone for CodegenCtx<'_, '_> {
    fn clone(&self) -> Self {
        Self {
            curr_mod: None,
            curr_func: None,
            curr_builder: None,
            mangled_syms: Arc::clone(&self.mangled_syms),
            hir: self.hir,
            semantic: self.semantic,
            params: HashMap::new(),
            src: self.src,
            allocas: HashMap::new(),
            mangling: self.mangling.clone(),
            modules: Arc::clone(&self.modules),
            types: self.types.clone(),
        }
    }
}

trait Codegen<'hir> {
    type Output;

    fn codegen(&self, ctx: &mut CodegenCtx<'_, 'hir>) -> Self::Output;
}

impl<'hir> Codegen<'hir> for &'hir hir::Module<'hir> {
    type Output = ();

    fn codegen(&self, ctx: &mut CodegenCtx<'_, 'hir>) {
        if let Some(id) = self.extern_file {
            let mut ctx = ctx.clone().enter_extern(self);
            for item in self.items {
                item.codegen(&mut ctx);
            }
            ctx.modules.borrow_mut().insert(id, ctx.curr_mod.unwrap());
        } else {
            ctx.enter(self);
            for item in self.items {
                item.codegen(ctx);
            }
            ctx.exit();
        }
    }
}

trait Address {
    fn addess(&self, ctx: &mut CodegenCtx) -> llvm::Value;
}

impl Address for hir::Item<'_> {
    fn addess(&self, ctx: &mut CodegenCtx) -> llvm::Value {
        match self.kind {
            ItemKind::Variable { name, .. } => {
                ctx.address_of(self.id, name.ident.sym)
            }
            ItemKind::Function { .. } => {
                let name = ctx.get_mangled(self.id).unwrap().to_string();
                if let Some(func) = ctx.module().get_function(&name) {
                    func.into_value()
                } else {
                    let fty = ctx.semantic.type_of(&self.id).unwrap();
                    let fty = fty.codegen(ctx);
                    ctx.module().add_function(&name, fty).into_value()
                }
            }
            ItemKind::Use(_) => todo!(),
            ItemKind::Mod(_) => todo!(),
            ItemKind::Struct { .. } => todo!(),
        }
    }
}

impl Address for hir::Expression<'_> {
    fn addess(&self, ctx: &mut CodegenCtx) -> llvm::Value {
        use hir::expr::ExpressionKind as EK;
        match &self.kind {
            EK::Variable(path) => {
                let def = path.def().expect_resolved();
                match ctx.hir.get_node(&def) {
                    HirNodeKind::Item(item) => item.addess(ctx),
                    HirNodeKind::Param(p) => {
                        ctx.address_of(p.id, p.name.ident.sym)
                    }
                    _ => unreachable!(),
                }
            },
            EK::ArrayAccess { arr, index } => {
                let arr_ptr = arr.addess(ctx);
                let index = index.value(ctx);
                let ty = ctx.semantic.type_of(&self.id).unwrap().codegen(ctx);
                ctx.builder().gep(ty, arr_ptr, &mut [index], "tmp_gep")
            },
            EK::StructAccess { st, field } => {
                let sty = ctx.semantic.type_of(&st.id).unwrap();
                let idx = sty.field_index_of(field.sym).unwrap();
                let st = st.addess(ctx);
                let sty = *ctx.types.get(&sty.id).unwrap();

                let zero = Value::const_int(llvm::Type::int_32(), 0);
                let idx = Value::const_int(llvm::Type::int_32(), idx as u64);
                ctx.builder().gep(sty, st, &mut [zero, idx], "tmp_gep")

            }
            _ => unreachable!("Can't get address of {self:?}")
        }
    }
}

trait CGValue {
    fn value(&self, ctx: &mut CodegenCtx<'_, '_>) -> llvm::Value;
}

trait CGExecute {
    fn execute(&self, ctx: &mut CodegenCtx<'_, '_>);
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

impl CGValue for hir::Expression<'_> {
    #[allow(clippy::too_many_lines)]
    fn value(&self, ctx: &mut CodegenCtx<'_, '_>) -> llvm::Value {
        use hir::expr::ExpressionKind as EK;
        use hir::expr::{UnaryOp, LogicalOp, ArithmeticOp, LitValue};

        match &self.kind {
            EK::Array(_) => todo!(),
            EK::Unary { op, expr } => {
                let val = expr.value(ctx);
                match op {
                    UnaryOp::Not => ctx.builder().not(val, "tmp_not"),
                    UnaryOp::Neg => ctx.builder().neg(val, "tmp_neg"),
                }
            },
            EK::Ref(_) => todo!(),
            EK::Deref(_) => todo!(),
            EK::Logical { left, op, right } => {
                let left = left.value(ctx);
                let right = right.value(ctx);
                match op {
                    LogicalOp::And => ctx.builder().and(left, right, "tmp_and"),
                    LogicalOp::Or => ctx.builder().or(left, right, "tmp_or"),
                }
            }
            EK::Comparison { left, op, right } => {
                let lty = ctx.semantic.type_of(&left.id).unwrap();
                let is_int = lty.is_integer();
                let is_signed = lty.is_signed();

                let left = left.value(ctx);
                let right = right.value(ctx);
                if is_int {
                    let pred = if is_signed {
                        cmp_op_to_int_pred(*op)
                    } else {
                        cmp_op_to_uint_pred(*op)
                    };
                    ctx.builder().signed_integer_cmp(left, right, pred, "tmp_cmp")
                } else {
                    todo!()
                }
            }
            EK::Arithmetic { left, op, right } => {
                let left = left.value(ctx);
                let right = right.value(ctx);
                match op {
                    ArithmeticOp::Add => ctx.builder().add(left, right, "tmp_add"),
                    ArithmeticOp::Sub => ctx.builder().sub(left, right, "tmp_sub"),
                    ArithmeticOp::Mul => ctx.builder().mul(left, right, "tmp_mul"),
                    ArithmeticOp::Div => todo!(),
                    ArithmeticOp::Mod => todo!(),
                }
            }
            EK::Ternary { .. } => todo!(),
            EK::Assignment { left, right } => {
                let left = left.addess(ctx);
                let right = right.value(ctx);
                ctx.builder().store(right, left);
                right
            }
            EK::Variable(path) => {
                let id = path.def().expect_resolved();
                let node = ctx.hir.get_node(&id);
                if let HirNodeKind::Param(p) = node {
                    if let Some(p) = ctx.params.get(&p.id) {
                        return *p;
                    }
                }
                let addr = match node {
                    HirNodeKind::Item(item) => item.addess(ctx),
                    HirNodeKind::Param(param) => ctx.address_of(param.id, param.name.ident.sym),
                    _ => unreachable!(),
                };
                let ty = ctx.semantic.type_of(&self.id).unwrap().codegen(ctx);
                let name = match ctx.hir.get_node(&id) {
                    HirNodeKind::Item(item) => item.get_name(),
                    HirNodeKind::Param(param) => param.name.ident.sym,
                    _ => unreachable!()
                };
                let name = name.borrow(|name| {
                    format!("load_{name}")
                });
                ctx.builder().load(addr, ty, &name)
            }
            EK::Literal(val) => {
                match val {
                    LitValue::Int(ival) => llvm::Value::const_int32(*ival as u64),
                    LitValue::Float(f) => llvm::Value::const_f64(*f),
                    LitValue::Bool(val) => llvm::Value::const_int1(u64::from(*val)),
                    LitValue::Str(_) => todo!(),
                    LitValue::Char(_) => todo!(),
                }
            }
            EK::Call { callee, args } => {
                let fty = ctx.semantic.type_of(&callee.id).unwrap();
                let func_ty = fty.codegen(ctx);
                let func = callee.addess(ctx);
                let mut args: TinyVec<_, 8> = args.iter().map(|expr| {
                    expr.value(ctx)
                }).collect();

                let name = if fty.as_function_type().unwrap().1.is_empty_type() {
                    ""
                } else {
                    "tmp_call"
                };
                ctx.builder().call(func_ty, func, &mut args, name)
            }
            EK::Cast { expr, to } => {
                todo!("{expr:?}, {to:?}")
            }
            EK::ArrayAccess { .. } | EK::StructAccess { .. } =>
            {
                let gep = self.addess(ctx);
                let ty = ctx.semantic.type_of(&self.id).unwrap().codegen(ctx);
                ctx.builder().load(gep, ty, "tmp_load")
            },
        }
    }
}

impl CGExecute for hir::Expression<'_> {
    fn execute(&self, ctx: &mut CodegenCtx<'_, '_>) {
        use hir::expr::ExpressionKind as EK;

        match &self.kind {
            EK::Array(expressions) => expressions.iter().for_each(|expr| expr.execute(ctx)),
            EK::Unary { expr, .. } => expr.execute(ctx),
            EK::Ref(expression) | EK::Deref(expression) => {
                expression.execute(ctx);
            }
            EK::Logical { left, right, .. } |
            EK::Comparison { left, right, .. } |
            EK::Arithmetic { left, right, .. } => {
                left.execute(ctx);
                right.execute(ctx);
            }
            EK::Ternary { cond, if_true, if_false } => {
                cond.execute(ctx);
                if_true.execute(ctx);
                if_false.execute(ctx);
            }
            EK::Assignment { .. } | EK::Call { .. } => {
                self.value(ctx);
            }
            EK::Variable(_) | EK::Literal(_) => {},
            EK::Cast { expr, .. } => {
                expr.execute(ctx);
            }
            EK::ArrayAccess { arr, .. } => arr.execute(ctx),
            EK::StructAccess { st, .. } => st.execute(ctx),
        }
    }
}

impl<'hir> Codegen<'hir> for &'hir hir::Statement<'hir> {
    type Output = ();

    fn codegen(&self, ctx: &mut CodegenCtx<'_, 'hir>) {
        match &self.kind {
            StatementKind::Expr(expr) => {
                expr.execute(ctx);
            },
            StatementKind::Block(stmts) => {
                for stmt in *stmts {
                    stmt.codegen(ctx);
                };
            },
            StatementKind::Return(expr) => {
                let val = expr.map(|expr| expr.value(ctx));
                ctx.builder().ret(val);
            },
            StatementKind::If { .. } => todo!(),
            StatementKind::While {.. } => todo!(),
            StatementKind::For { .. } => todo!(),
            StatementKind::Empty => {}
            StatementKind::Break => todo!(),
            StatementKind::Continue => todo!(),
            StatementKind::Print(_) => todo!(),
            StatementKind::Read(_) => todo!(),
            StatementKind::Item(item) => {
                item.codegen(ctx);
            }
        }
    }
}

impl<'hir> Codegen<'hir> for &'hir hir::Item<'hir> {
    type Output = ();

    fn codegen(&self, ctx: &mut CodegenCtx<'_, 'hir>) {
        match self.kind {
            hir::ItemKind::Mod(module) => module.codegen(ctx),
            hir::ItemKind::Variable { name, constness, init, .. } => {
                match constness {
                    Constness::Const => todo!(),
                    Constness::Default => {
                        let ty = ctx.semantic.type_of(&self.id).unwrap();
                        let ty = ty.codegen(ctx);
                        name.ident.sym.borrow(|name| {
                            let alloca = ctx.builder().alloca(ty, name);
                            ctx.allocas.insert(self.id, alloca);
                            if let Some(init) = init {
                                let initializer = init.value(ctx);
                                ctx.builder().store(initializer, alloca);
                            }
                        });
                    },
                }
            },
            hir::ItemKind::Function { name, params, ret_ty, body } =>
                codegen_function(ctx, self, name, params, ret_ty, body),
            hir::ItemKind::Struct { .. } => {
                let struct_type = ctx.semantic.type_of(&self.id).unwrap();
                let (name, fields) = struct_type.as_struct_type().unwrap();
                let name = ctx.mangle_symbol(self.id, name);
                let mut fields: TinyVec<_, 8> = fields.iter().map(|f| {
                    f.ty.codegen(ctx)
                }).collect();
                let s = llvm::Type::struct_named(&name, &mut fields, false);
                ctx.types.insert(struct_type.id, s);
            }
            hir::ItemKind::Use(_) => {},
        }
    }
}

impl Codegen<'_> for semantic::Ty<'_> {
    type Output = llvm::Type;

    fn codegen(&self, ctx: &mut CodegenCtx) -> Self::Output {
        match &self.kind {
            TypeKind::Primitive(prim) => match prim {
                PrimitiveType::Int => llvm::Type::int_32(),
                PrimitiveType::Float => llvm::Type::float_64(),
                PrimitiveType::Bool => llvm::Type::int_1(),
                PrimitiveType::Char => todo!(),
                PrimitiveType::Empty => llvm::Type::void(),
            },
            TypeKind::Function { params, ret_ty } => {
                let mut params_tys: TinyVec<_, 8> = params.iter().map(|param| {
                    param.codegen(ctx)
                }).collect();
                let ret = ret_ty.codegen(ctx);

                llvm::Type::function(ret, &mut params_tys, false)
            },
            TypeKind::Ref(_) => todo!(),
            TypeKind::Array(ty, len) => {
                llvm::Type::array(ty.codegen(ctx), *len as _)
            }
            TypeKind::Struct { .. } => {
                *ctx.types.get(&self.id).unwrap()
            }
        }
    }
}

fn codegen_function<'hir>(
    ctx: &mut CodegenCtx<'_, 'hir>,
    item: &'hir hir::Item<'hir>,
    name: &PathDef,
    params: &'hir [Param<'hir>],
    ret_ty: &'hir Type<'hir>,
    body: &'hir[Statement<'hir>],
) {
    let ty = ctx.semantic.type_of(&item.id).unwrap();
    let fty = ty.codegen(ctx);

    let name = ctx.mangle_symbol(item.id, name.ident.sym).to_string();
    let mut function = ctx.module().add_function(&name, fty);

    ctx.allocas.clear();
    ctx.params.clear();

    let mut entry = function.append_basic_block("entry");

    {
        let mut builder = llvm::Builder::new();
        builder.position_at_end(&mut entry);

        for i in 0..function.n_params() {
            let param = function.param(i);
            ctx.params.insert(params[i as usize].id, param);
        }

        debug_assert!(ctx.curr_builder.is_none());
        ctx.curr_builder = Some(builder);
        for stmt in body {
            stmt.codegen(ctx);
        }

        if ret_ty.is_empty() {
            ctx.builder().ret(None);
        }

        ctx.curr_builder.take();
    }

    ctx.allocas.clear();
    ctx.params.clear();
}
