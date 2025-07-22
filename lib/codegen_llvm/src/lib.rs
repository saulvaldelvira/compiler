use core::cell::RefCell;
use core::ops::Index;
use core::usize;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::sync::Arc;

use hir::hir_id::HirNode;
use hir::node_map::HirNodeKind;
use hir::stmt::StatementKind;
use hir::{Constness, HirId, Item, ItemKind, Module, Param, PathDef, Statement, Type};
use interner::Symbol;
use llvm::core::Function;
use llvm::Value;
use semantic::rules::expr::SideEffect;
use semantic::{PrimitiveType, TypeKind};
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
        if module.name.ident.sym != *"root" {
            self.mangling.push(module.name.ident.sym);
        }
    }

    fn exit(&mut self) {
        self.mangling.pop();
    }

    fn enter_extern(mut self, module: &'hir hir::Module<'hir>) -> Self {
        let name = self.mangle_symbol(module.id, module.name.ident.sym);
        self.curr_mod = Some(llvm::Module::new(&name));
        if module.name.ident.sym != *"root" {
            self.mangling.push(module.name.ident.sym);
        }
        self.curr_func = None;
        self.curr_builder = None;
        self
    }

    /* pub fn codegen(&mut self, module: &'hir hir::Module<'hir>) -> llvm::Module { */
    /*     let name = self.mangle(module.name.ident.sym); */
    /*     self.mangling.push(module.name.ident.sym); */

    /*     let ctx = CodegenCtx { */
    /*         mangling: self.mangling.clone(), */
    /*         hir: self.hir, */
    /*         curr_mod: Some(llvm_mod), */
    /*         curr_builder: None, */
    /*         curr_func: None, */
    /*         params: HashMap::new(), */
    /*     }; */

    /* } */

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
            modules: Arc::clone(&self.modules)
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
                match ctx.module().get_function(&name) {
                    Some(func) => func.into_value(),
                    None => {
                        let fty = ctx.semantic.type_of(&self.id).unwrap();
                        let fty = fty.codegen(ctx);
                        ctx.module().add_function(&name, fty).into_value()
                    }
                }
            }
            ItemKind::Use(use_item) => todo!(),
            ItemKind::Mod(module) => todo!(),
            ItemKind::Struct { name, fields } => todo!(),
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
            _ => unreachable!()
        }
    }
}

trait CGValue {
    fn value(&self, ctx: &mut CodegenCtx<'_, '_>) -> llvm::Value;
}

trait CGExecute {
    fn execute(&self, ctx: &mut CodegenCtx<'_, '_>);
}

impl CGValue for hir::Expression<'_> {
    fn value(&self, ctx: &mut CodegenCtx<'_, '_>) -> llvm::Value {
        use hir::expr::ExpressionKind as EK;
        use hir::expr::{ArithmeticOp, LitValue};

        match &self.kind {
            EK::Array(expressions) => todo!(),
            EK::Unary { op, expr } => todo!(),
            EK::Ref(expression) => todo!(),
            EK::Deref(expression) => todo!(),
            EK::Logical { left, op, right } => todo!(),
            EK::Comparison { left, op, right } => todo!(),
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
            EK::Ternary { cond, if_true, if_false } => todo!(),
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
                        return p.clone();
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
                    LitValue::Str(symbol) => todo!(),
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
            EK::Cast { expr, to } => todo!(),
            EK::ArrayAccess { arr, index } => todo!(),
            EK::StructAccess { st, field } => todo!(),
        }
    }
}

impl CGExecute for hir::Expression<'_> {
    fn execute(&self, ctx: &mut CodegenCtx<'_, '_>) {
        use hir::expr::ExpressionKind as EK;

        match &self.kind {
            EK::Array(expressions) => expressions.iter().for_each(|expr| expr.execute(ctx)),
            EK::Unary { expr, .. } => expr.execute(ctx),
            EK::Ref(expression) => expression.execute(ctx),
            EK::Deref(expression) => expression.execute(ctx),
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
            EK::Assignment { left, right } => {
                self.value(ctx);
            }
            EK::Variable(_) |
            EK::Literal(_) => { }
            EK::Call { .. } => {
                self.value(ctx);
            }
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
                expr.value(ctx);
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
            StatementKind::If { cond, if_true, if_false } => todo!(),
            StatementKind::While { cond, body } => todo!(),
            StatementKind::For { init, cond, inc, body } => todo!(),
            StatementKind::Empty => todo!(),
            StatementKind::Break => todo!(),
            StatementKind::Continue => todo!(),
            StatementKind::Print(expression) => todo!(),
            StatementKind::Read(expression) => todo!(),
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
            hir::ItemKind::Variable { name, ty, init, constness } => {
                match constness {
                    Constness::Const => todo!(),
                    Constness::Default => {
                        let ty = ctx.semantic.type_of(&self.id).unwrap();
                        let ty = ty.codegen(ctx);
                        name.ident.sym.borrow(|name| {
                            let alloca = ctx.builder().alloca(ty, name);
                            ctx.allocas.insert(self.id, alloca);
                        });
                    },
                }
            },
            hir::ItemKind::Function { name, params, ret_ty, body } =>
                codegen_function(ctx, self, name, params, ret_ty, body),
            hir::ItemKind::Struct { name, fields } => todo!(),
            hir::ItemKind::Use(use_item) => todo!(),
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
            TypeKind::Array(_, _) => todo!(),
            TypeKind::Struct { name, fields } => todo!(),
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
    let mut sum_func = ctx.module().add_function(&name, fty);

    ctx.allocas.clear();
    ctx.params.clear();

    /* for i in 0..sum_func.n_params() { */
    /*     let mut param = sum_func.param(i); */
    /*     params[i as usize].get_name().borrow(|name| { */
    /*         param.set_name(name); */
    /*     }); */
    /*     ctx.params.insert(params[i as usize].id, param); */
    /* } */

    let mut entry = sum_func.append_basic_block("entry");

    {
        let mut builder = llvm::Builder::new();
        builder.position_at_end(&mut entry);

        for i in 0..sum_func.n_params() {
            let param = sum_func.param(i);
            params[i as usize].get_name().borrow(|name| {
                /* param.set_name(name); */
                ctx.params.insert(params[i as usize].id, param);
                /* let alloca = builder.alloca(param.get_type(), name); */
                /* ctx.allocas.insert(params[i as usize].id, alloca); */
            });
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
