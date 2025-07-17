use std::collections::HashMap;

use hir::node_map::HirNodeKind;
use hir::stmt::StatementKind;
use hir::types::{PrimitiveType, TypeKind};
use hir::{HirId, Param, PathDef, Statement, Type};
use llvm::core::Function;
use llvm::Value;
use tiny_vec::TinyVec;

pub struct CodegenCtx<'cg, 'hir> {
    curr_mod: &'cg mut llvm::Module,
    curr_func: Option<Function>,
    curr_builder: Option<llvm::Builder>,
    params: HashMap<HirId, Value>,

    hir: &'cg hir::Session<'hir>,
}

impl<'cg, 'hir> CodegenCtx<'cg, 'hir> {
    pub fn new(hir: &'cg hir::Session<'hir>, module: &'cg mut llvm::Module) -> Self {
        Self {
            hir,
            curr_mod: module,
            curr_builder: None,
            curr_func: None,
            params: HashMap::new(),
        }
    }
    fn builder(&mut self) -> &mut llvm::Builder { self.curr_builder.as_mut().unwrap() }
}

pub trait Codegen {
    type Output;

    fn codegen(&self, ctx: &mut CodegenCtx) -> Self::Output;
}

impl Codegen for hir::Expression<'_> {
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
                    HirNodeKind::Item(item) => todo!(),
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
            EK::Call { callee, args } => todo!(),
            EK::Cast { expr, to } => todo!(),
            EK::ArrayAccess { arr, index } => todo!(),
            EK::StructAccess { st, field } => todo!(),
        }
    }
}

impl Codegen for hir::Statement<'_> {
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

impl Codegen for hir::Item<'_> {
    type Output = ();

    fn codegen(&self, module: &mut CodegenCtx) {
        match self.kind {
            hir::ItemKind::Mod(module) => todo!(),
            hir::ItemKind::Variable { name, ty, init, constness } => todo!(),
            hir::ItemKind::Function { name, params, ret_ty, body } =>
                codegen_function(module, self, name, params, ret_ty, body),
            hir::ItemKind::Struct { name, fields } => todo!(),
            hir::ItemKind::Use(use_item) => todo!(),
        }
    }
}

impl Codegen for hir::Type<'_> {
    type Output = llvm::Type;

    fn codegen(&self, _: &mut CodegenCtx) -> Self::Output {
        match &self.kind {
            TypeKind::Primitive(prim) => match prim {
                PrimitiveType::Int => llvm::Type::int_32(),
                PrimitiveType::Float => llvm::Type::float_64(),
                PrimitiveType::Bool => llvm::Type::int_1(),
                PrimitiveType::Char => todo!(),
                PrimitiveType::Empty => todo!(),
            },
            TypeKind::Function { params, ret_ty } => {
               todo!()
            },
            TypeKind::Ref(_) => todo!(),
            TypeKind::Array(_, _) => todo!(),
            TypeKind::Path(path) => todo!(),
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
    let mut params_tys: TinyVec<_, 8> = params.iter().map(|param| {
        let ty = param.ty;
        ty.codegen(ctx)
    }).collect();

    let ret = ret_ty.codegen(ctx);
    let fty = llvm::Type::function(ret, &mut params_tys, false);

    let mut sum_func = name.ident.sym.borrow(|name| {
        ctx.curr_mod.add_function(name, fty)
    });

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
    }
}
