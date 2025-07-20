use core::cell::RefCell;
use core::ops::Index;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::sync::Arc;

use hir::node_map::HirNodeKind;
use hir::stmt::StatementKind;
use hir::{HirId, ItemKind, Module, Param, PathDef, Statement, Type};
use interner::Symbol;
use llvm::core::Function;
use llvm::Value;
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
    params: HashMap<HirId, Value>,
    hir: &'cg hir::Session<'hir>,

    mangling: Vec<Symbol>,
    semantic: &'cg semantic::Semantic<'hir>,

    mangled_syms: Arc<RefCell<HashMap<HirId, String>>>,

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
            params: HashMap::new(),
            mangling: Vec::new(),
            mangled_syms: Default::default(),
            semantic,
            src,
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

    fn enter(mut self, module: &'hir hir::Module<'hir>) -> Self {
        let name = self.mangle_symbol(module.id, module.name.ident.sym);
        self.curr_mod = Some(llvm::Module::new(&name));
        if module.name.ident.sym != *"root" {
            self.mangling.push(module.name.ident.sym);
        }
        self.params.clear();
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
}

impl Clone for CodegenCtx<'_, '_> {
    fn clone(&self) -> Self {
        Self {
            curr_mod: None,
            curr_func: None,
            curr_builder: None,
            mangled_syms: Arc::clone(&self.mangled_syms),
            params: HashMap::new(),
            hir: self.hir,
            semantic: self.semantic,
            src: self.src,
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
            let mut ctx = ctx.clone().enter(self);
            for item in self.items {
                item.codegen(&mut ctx);
            }
            ctx.modules.borrow_mut().insert(id, ctx.curr_mod.unwrap());
        } else {
            for item in self.items {
                item.codegen(ctx);
            }
        }
    }
}


impl Codegen<'_> for hir::Expression<'_> {
    type Output = llvm::Value;

    fn codegen(&self, ctx: &mut CodegenCtx) -> llvm::Value {
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
                let left = left.codegen(ctx);
                let right = right.codegen(ctx);
                match op {
                    ArithmeticOp::Add => ctx.builder().add(left, right, "tmp_add"),
                    ArithmeticOp::Sub => ctx.builder().sub(left, right, "tmp_sub"),
                    ArithmeticOp::Mul => ctx.builder().mul(left, right, "tmp_mul"),
                    ArithmeticOp::Div => todo!(),
                    ArithmeticOp::Mod => todo!(),
                }
            }
            EK::Ternary { cond, if_true, if_false } => todo!(),
            EK::Assignment { left, right } => todo!(),
            EK::Variable(path) => {
                let def = path.def().expect_resolved();
                match ctx.hir.get_node(&def) {
                    HirNodeKind::Param(param) => ctx.params.get(&def).unwrap().clone(),
                    HirNodeKind::Item(item) => {
                        match &item.kind {
                            ItemKind::Function { name, .. } => {
                                let name = ctx.get_mangled(item.id).unwrap().to_string();
                                match ctx.module().get_function(&name) {
                                    Some(func) => func.into_value(),
                                    None => {
                                        let fty = ctx.semantic.type_of(&item.id).unwrap();
                                        let fty = fty.codegen(ctx);
                                        ctx.module().add_function(&name, fty).into_value()
                                    }
                                }
                            },
                            ItemKind::Mod(module) => todo!(),
                            ItemKind::Variable { name, ty, init, constness } => todo!(),
                            ItemKind::Struct { name, fields } => todo!(),
                            ItemKind::Use(use_item) => todo!(),
                        }
                    }
                    _ => unreachable!(),
                }
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
                let func = callee.codegen(ctx);
                let mut args: TinyVec<_, 8> = args.iter().map(|expr| {
                    expr.codegen(ctx)
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

impl Codegen<'_> for hir::Statement<'_> {
    type Output = ();

    fn codegen(&self, ctx: &mut CodegenCtx) {
        match &self.kind {
            StatementKind::Expr(expr) => {
                expr.codegen(ctx);
            },
            StatementKind::Block(stmts) => {
                for stmt in *stmts {
                    stmt.codegen(ctx);
                };
            },
            StatementKind::Return(expr) => {
                let val = expr.map(|expr| expr.codegen(ctx));
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
            StatementKind::Item(item) => todo!(),
        }
    }
}

impl<'hir> Codegen<'hir> for &'hir hir::Item<'hir> {
    type Output = ();

    fn codegen(&self, ctx: &mut CodegenCtx<'_, 'hir>) {
        match self.kind {
            hir::ItemKind::Mod(module) => module.codegen(ctx),
            hir::ItemKind::Variable { name, ty, init, constness } => todo!(),
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

fn codegen_function(
    ctx: &mut CodegenCtx,
    item: &hir::Item<'_>,
    name: &PathDef,
    params: &[Param<'_>],
    ret_ty: &Type<'_>,
    body: &[Statement<'_>],
) {

    let ty = ctx.semantic.type_of(&item.id).unwrap();
    let fty = ty.codegen(ctx);

    let name = ctx.mangle_symbol(item.id, name.ident.sym).to_string();
    let mut sum_func = ctx.module().add_function(&name, fty);

    ctx.params.clear();
    for i in 0..sum_func.n_params() {
        let mut param = sum_func.param(i);
        params[i as usize].get_name().borrow(|name| {
            param.set_name(name);
        });
        ctx.params.insert(params[i as usize].id, param);
    }

    let mut entry = sum_func.append_basic_block("entry");

    {
        let mut builder = llvm::Builder::new();
        builder.position_at_end(&mut entry);

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

}
