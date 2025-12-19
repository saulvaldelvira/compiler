use hir::expr::ExpressionKind;
use hir::stmt::StatementKind;
use hir::{Constness, Function, ItemKind};
use json::{json, Json};
use semantic::rules::stmt;
use semantic::Semantic;
use span::SourceMap;

pub trait SerializeJson {
    fn serialize(&self, sem: &Semantic<'_>) -> Json;
}

impl SerializeJson for hir::Module<'_> {
    fn serialize(&self, sem: &Semantic<'_>) -> Json {
        let items: Vec<Json> = self.items.iter()
            .map(|i| i.serialize(sem))
            .collect();

        json!({
            "kind": "Module",
            "items": {items}
        })
    }
}

impl SerializeJson for hir::Item<'_> {
    fn serialize(&self, sem: &Semantic<'_>) -> Json {
        match self.kind {
            ItemKind::Mod(module) => module.serialize(sem),
            ItemKind::Variable { name, init, constness, .. } => {
                let ty = sem.type_of(&self.id).unwrap();
                let mut val = json!{{
                    "kind": {self.kind.get_repr()},
                    "name": {name.ident.sym.to_string()},
                    "const": {matches!(constness, Constness::Const)},
                    "type": {format!("{ty}")}
                }};
                if let Some(init) = init {
                    val.expect_object_mut().insert("init".into(), init.serialize(sem));
                }
                val
            }
            ItemKind::Function(Function { is_extern, is_variadic, name, params, body, .. }) => {
                let body = body.map(|b| b.serialize(sem)).unwrap_or(Json::Null);

                let params: Vec<_> = params.iter().map(|p| p.serialize(sem)).collect();
                let (_, _, ret_ty) = sem.type_of(&self.id).unwrap().as_function_type().unwrap();

                json!{{
                    "kind": "Function",
                    "name": {name.ident.sym.to_string()},
                    "params": {params},
                    "return_type": {format!("{ret_ty}")},
                    "extern": {*is_extern},
                    "variadic": {*is_variadic},
                    "body": {body}
                }}

            }
            _ =>
        Json::Null
            /* ItemKind::Struct { name, fields } => todo!(), */
            /* ItemKind::Use(use_item) => todo!(), */
            /* ItemKind::TypeAlias { ty, name } => todo!(), */
        }
    }
}

impl SerializeJson for hir::Expression<'_> {
    fn serialize(&self, sem: &Semantic<'_>) -> Json {
        match &self.kind {
            ExpressionKind::Array(expressions) =>
                expressions.iter()
                    .map(|e| e.serialize(sem))
                    .collect::<Vec<_>>().into(),
            ExpressionKind::Unary { op, expr } => todo!(),
            ExpressionKind::Block(block_expr) => {
                let stmts: Vec<_> = block_expr.stmts.iter().map(|s| s.serialize(sem)).collect();
                json!{{
                    "stmts": {stmts},
                    "tail": {block_expr.tail.map(|t| t.serialize(sem)).unwrap_or(Json::Null)}
                }}
            }
            ExpressionKind::If { cond, if_true, if_false } => todo!(),
            ExpressionKind::Ref(expression) => todo!(),
            ExpressionKind::Deref(expression) => todo!(),
            ExpressionKind::Logical { left, op, right } => todo!(),
            ExpressionKind::Comparison { left, op, right } => todo!(),
            ExpressionKind::Arithmetic { left, op, right } => todo!(),
            ExpressionKind::Assignment { left, right } => todo!(),
            ExpressionKind::Variable(path) => todo!(),
            ExpressionKind::Literal(lit_value) => todo!(),
            ExpressionKind::Call { callee, args } => todo!(),
            ExpressionKind::Cast { expr, to } => todo!(),
            ExpressionKind::ArrayAccess { arr, index } => todo!(),
            ExpressionKind::TupleAccess { tuple, index } => todo!(),
            ExpressionKind::StructAccess(struct_access) => todo!(),
        }
    }
}

impl SerializeJson for hir::Param<'_> {
    fn serialize(&self, sem: &Semantic<'_>) -> Json {
        json!{{
            "name": {self.name.ident.sym.to_string()},
            "ty": {format!("{:?}", self.ty.kind)}
        }}
    }
}

impl SerializeJson for hir::Field<'_> {
    fn serialize(&self, sem: &Semantic<'_>) -> Json {
        json!{{
            "name": {self.name.ident.sym.to_string()},
            "ty": {format!("{:?}", self.ty.kind)}
        }}
    }
}

impl SerializeJson for hir::Statement<'_> {
    fn serialize(&self, sem: &Semantic<'_>) -> Json {
        match self.kind {
            StatementKind::Expr(expression) => expression.serialize(sem),
            StatementKind::While { cond, body } => todo!(),
            StatementKind::For(for_stmt) => todo!(),
            StatementKind::Empty => todo!(),
            StatementKind::Break => todo!(),
            StatementKind::Continue => todo!(),
            StatementKind::Return(expression) => todo!(),
            StatementKind::Item(item) => todo!(),
        }
    }
}

pub fn hir_serialize_json(hir: &hir::Session<'_>, sem: &Semantic<'_>, src: &SourceMap) -> Json {
    let prog = hir.get_root();
    prog.serialize(sem)
}
