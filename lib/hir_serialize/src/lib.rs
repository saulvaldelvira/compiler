#[macro_use]
extern crate json;

use hir::BlockExpr;
use hir::expr::{ArithmeticOp, CmpOp, LitValue, LogicalOp, StructAccess, UnaryOp};
use hir::stmt::{ForStmt, StatementKind};
use json::Json;
use semantic::Semantic;
use span::Span;
use span::source::SourceMap;

trait JsonSerialize {
    fn serialize(&self, ctx: &SerializeCtx<'_, '_>) -> Json;
}

struct SerializeCtx<'a, 'sem> {
    sem: &'a Semantic<'sem>,
    sm: &'a SourceMap,
}

pub fn serialize_json<'a, 'hirsem>(hir: &'a hir::Session<'hirsem>, sm: &'a SourceMap, sem: &'a Semantic<'hirsem>) -> String
{
    let json = hir.get_root().serialize(&SerializeCtx{sem, sm});
    json.to_string()
}

impl JsonSerialize for hir::Module<'_> {
    fn serialize(&self, ctx: &SerializeCtx<'_, '_>) -> Json {
        json!({
            "kind": "Module",
            "name": {self.name.ident.sym.to_string()},
            "items": {Json::Array(self.items.iter().map(|i| i.serialize(ctx)).collect())}
        })
    }
}

impl JsonSerialize for Span {
    #[expect(clippy::cast_precision_loss)]
    fn serialize(&self, ctx: &SerializeCtx<'_, '_>) -> Json {
        let filepos = ctx.sm.file_position(self).unwrap();
        json!({
            "start_line": {filepos.start_line as f64},
            "start_col": {filepos.start_col as f64},
            "end_line": {filepos.end_line as f64},
            "end_col": {filepos.end_col as f64},
        })
    }
}

impl JsonSerialize for hir::Function<'_> {
    fn serialize(&self, ctx: &SerializeCtx<'_, '_>) -> Json {
        json!({
            "kind": "Function",
            "is_extern": {self.is_extern},
            "is_variadic": {self.is_variadic},
            "name": {self.name.ident.sym.to_string()},
            "params": {
                Json::Array(self.params.iter().map(|p| p.serialize(ctx)).collect())
            },
            "ret_type": {self.ret_ty.map(|t| t.serialize(ctx)).unwrap_or(Json::Null)},
            "body": {self.body.map(|b| b.serialize(ctx)).unwrap_or(Json::Null)},
        })
    }
}

impl JsonSerialize for hir::Param<'_> {
    fn serialize(&self, ctx: &SerializeCtx<'_, '_>) -> Json {
        json!({
            "kind": "Param",
            "name": {self.name.ident.sym.to_string()},
            "type": {self.ty.serialize(ctx)},
        })
    }
}

impl JsonSerialize for hir::UseItem<'_> {
    fn serialize(&self, ctx: &SerializeCtx<'_, '_>) -> Json {
        let mut j = json!({
            "kind": "Use",
            "path": {self.path.serialize(ctx)},
            "is_wildcard": {self.is_wildcard},
        });
        if let Some(newname) = self.new_name {
            j.expect_object_mut().insert("new_name".into(), newname.ident.sym.to_string().into());
        }
        j
    }
}

impl JsonSerialize for hir::Item<'_> {
    fn serialize(&self, ctx: &SerializeCtx<'_, '_>) -> Json {
        let mut obj = match self.kind {
            hir::ItemKind::Mod(module) => module.serialize(ctx),
            hir::ItemKind::Variable { name, ty, init, constness } => json!({
                    "kind": "Variable",
                    "name": {name.ident.sym.to_string()},
                    "type": {ty.map(|ty| ty.serialize(ctx)).unwrap_or(Json::Null)},
                    "init": {init.map(|init| init.serialize(ctx)).unwrap_or(Json::Null)},
                    "is_const": {matches!(constness, hir::Constness::Const)}
                }),
            hir::ItemKind::Function(function) => function.serialize(ctx),
            hir::ItemKind::Struct { name, fields } => json!({
                    "kind": "Struct",
                    "name": {name.ident.sym.to_string()},
                    "fields": {
                        Json::Array(fields.iter().map(|field| {
                            field.serialize(ctx)
                        }).collect())
                    }
                }),
            hir::ItemKind::Use(use_item) => use_item.serialize(ctx),
            hir::ItemKind::TypeAlias { ty, name } => json!({
                "kind": "TypeAlias",
                "name": {name.ident.sym.to_string()},
                "ty": {ty.serialize(ctx)},
            })
        };

        if let Some(obj) = obj.object_mut() {
            // TODO: expect
            obj.insert("span".into(), self.span.serialize(ctx));
        }
        obj
    }
}

impl JsonSerialize for hir::Type<'_> {
    fn serialize(&self, ctx: &SerializeCtx<'_, '_>) -> Json {
        let mut j = match &self.kind {
            hir::types::TypeKind::Primitive(primitive_type) => json!({
                "kind": {match primitive_type {
                hir::types::PrimitiveType::I8 => "i8",
                hir::types::PrimitiveType::I16 => "i16",
                hir::types::PrimitiveType::I32 => "i32",
                hir::types::PrimitiveType::I64 => "i64",
                hir::types::PrimitiveType::U8 => "u8 ",
                hir::types::PrimitiveType::U16 => "u16",
                hir::types::PrimitiveType::U32 => "u32",
                hir::types::PrimitiveType::U64 => "u64",
                hir::types::PrimitiveType::Char => "cha",
                hir::types::PrimitiveType::F32 => "f32",
                hir::types::PrimitiveType::F64 => "f64",
                hir::types::PrimitiveType::Bool => "bool"
                }}}),
            hir::types::TypeKind::Ref(to) => json!({
                "kind": "Ref",
                "to": {to.serialize(ctx)},
            }),
            hir::types::TypeKind::Array(of, len) => json!({
                "kind": "Array",
                "of": {of.serialize(ctx)},
                "len": {f64::from(*len)},
            }),
            hir::types::TypeKind::Tuple(items) => json!({
                "kind": "Tuple",
                "items": { Json::Array(items.iter().map(|it| it.serialize(ctx)).collect()) }
            }),
            hir::types::TypeKind::Path(path) => path.serialize(ctx),
        };
        j.expect_object_mut().insert("span".into(), self.span.serialize(ctx));
        j
    }
}

impl JsonSerialize for hir::Path {
    fn serialize(&self, _: &SerializeCtx<'_, '_>) -> Json {
        json!({
            "kind": "Path",
            "segments": {Json::Array(self.segments.iter().map(|seg| Json::String(seg.ident.sym.to_string().into())).collect())}
        })
    }
}

impl JsonSerialize for hir::Expression<'_> {
    fn serialize(&self, ctx: &SerializeCtx<'_, '_>) -> Json {
        let mut obj = match &self.kind {
            hir::expr::ExpressionKind::Array(expressions) => json!({
                "kind": "Array",
                "expressions": {Json::Array(expressions.iter().map(|e| e.serialize(ctx)).collect())}
            }),
            hir::expr::ExpressionKind::Unary { op, expr } => json!({
                "kind": "Unary",
                "op": {match op {
                    UnaryOp::Not => "!",
                    UnaryOp::Neg => "-",
                }},
                "expr": {expr.serialize(ctx)},
            }),
            hir::expr::ExpressionKind::Block(block_expr) => block_expr.serialize(ctx),
            hir::expr::ExpressionKind::If { cond, if_true, if_false } => json!({
                "kind": "If",
                "condition": {cond.serialize(ctx)},
                "then": {if_true.serialize(ctx)},
                "else": {if_false.map(|i| i.serialize(ctx)).unwrap_or(Json::Null)}
            }),
            hir::expr::ExpressionKind::Ref(expression) => json!({
                "kind": "Ref",
                "operand": {expression.serialize(ctx)},
            }),
            hir::expr::ExpressionKind::Deref(expression) => json!({
                "kind": "Deref",
                "operand": {expression.serialize(ctx)},
            }),
            hir::expr::ExpressionKind::Logical { left, op, right } => json!({
                "kind": "Logical",
                "left": {left.serialize(ctx)},
                "op": {match op {
                    LogicalOp::Or => "||",
                    LogicalOp::And => "&&",
                }},
                "right": {right.serialize(ctx)},
            }),
            hir::expr::ExpressionKind::Comparison { left, op, right } => json!({
                "kind": "Comparison",
                "left": {left.serialize(ctx)},
                "op": {match op {
                    CmpOp::Gt => ">",
                    CmpOp::Ge => ">=",
                    CmpOp::Lt => "<",
                    CmpOp::Le => "<=",
                    CmpOp::Eq => "==",
                    CmpOp::Neq => "!=",
                }},
                "right": {right.serialize(ctx)},
            }),

            hir::expr::ExpressionKind::Arithmetic { left, op, right } =>  json!({
                "kind": "Arithmetic",
                "left": {left.serialize(ctx)},
                "op": {match op {
                    ArithmeticOp::Add => "+",
                    ArithmeticOp::Sub => "-",
                    ArithmeticOp::Mul => "*",
                    ArithmeticOp::Div => "/",
                    ArithmeticOp::Mod => "%",
                }},
                "right": {right.serialize(ctx)},
            }),
            hir::expr::ExpressionKind::Assignment { left, right } => json!({
                "kind": "Assignment",
                "left": {left.serialize(ctx)},
                "right": {right.serialize(ctx)},
            }),
            hir::expr::ExpressionKind::Variable(path) => json!({
                "kind": "Variable",
                "path": {path.serialize(ctx)},
            }),
            hir::expr::ExpressionKind::Literal(lit_value) => json!({
                "kind": "Literal",
                "value": {lit_value.serialize(ctx)},
            }),
            hir::expr::ExpressionKind::Call { callee, args } => json!({
                "kind": "Call",
                "callee": {callee.serialize(ctx)},
                "args": {Json::Array(args.iter().map(|a| a.serialize(ctx)).collect())},
            }),
            hir::expr::ExpressionKind::Cast { expr, to } => json!({
                "kind": "Cast",
                "expr": {expr.serialize(ctx)},
                "to": {to.serialize(ctx)},
            }),
            hir::expr::ExpressionKind::ArrayAccess { arr, index } => json!({
                "kind": "ArrayAccess",
                "array": {arr.serialize(ctx)},
                "index": {index.serialize(ctx)},
            }),
            hir::expr::ExpressionKind::TupleAccess { tuple, index } => json!({
                "kind": "TupleAccess",
                "tuple": {tuple.serialize(ctx)},
                "index": {*index}
            }),
            hir::expr::ExpressionKind::StructAccess(struct_access) => struct_access.serialize(ctx),
        };
        let fields = obj.expect_object_mut();
        fields.insert("span".into(), self.span.serialize(ctx));

        let sem_ty = ctx.sem.type_of(&self.id).unwrap();
        fields.insert("type".into(), sem_ty.serialize(ctx));
        obj
    }
}

impl JsonSerialize for StructAccess<'_> {
    fn serialize(&self, ctx: &SerializeCtx<'_, '_>) -> Json {
        json!({
            "kind": "StructAccess",
            "struct": {self.st.serialize(ctx)},
            "field": {self.field.sym.to_string()},
        })
    }
}

impl JsonSerialize for LitValue {
    fn serialize(&self, _: &SerializeCtx<'_, '_>) -> Json {
        match self {
            LitValue::Int(i) => Json::Number((*i).into()),
            LitValue::Float(f) => Json::Number(*f),
            LitValue::Str(symbol) => Json::String(symbol.to_string().into()),
            LitValue::Bool(b) => Json::from(*b),
            LitValue::Char(c) => Json::String(c.to_string().into())
        }
    }
}

impl JsonSerialize for BlockExpr<'_> {
    fn serialize(&self, ctx: &SerializeCtx<'_, '_>) -> Json {
        let mut j = json!({
            "kind": "Block",
            "stmts": {Json::Array(self.stmts.iter().map(|s| s.serialize(ctx)).collect())},
        });
        if let Some(t) = &self.tail {
            j.expect_object_mut().insert("tail".into(), t.serialize(ctx));
        }
        j
    }
}

impl JsonSerialize for hir::Field<'_> {
    fn serialize(&self, ctx: &SerializeCtx<'_, '_>) -> Json {
        json!({
            "name": {self.name.ident.sym.to_string()},
            "type": {self.ty.serialize(ctx)}
        })
    }
}

impl JsonSerialize for hir::Statement<'_> {
    fn serialize(&self, ctx: &SerializeCtx<'_, '_>) -> Json {
        let mut j = match &self.kind {
            StatementKind::Expr(expression) => expression.serialize(ctx),
            StatementKind::While { cond, body } => json!({
                "kind": "While",
                "cond": {cond.serialize(ctx)},
                "body": {body.serialize(ctx)},
            }),
            StatementKind::For(for_stmt) => for_stmt.serialize(ctx),
            StatementKind::Empty => json!({"kind": "Empty"}),
            StatementKind::Break => json!({"kind": "Break"}),
            StatementKind::Continue => json!({"kind": "Continue"}),
            StatementKind::Return(expression) => json!({
                "kind": "Return",
                "expr": {expression.map(|e| e.serialize(ctx)).unwrap_or(Json::Null)},
            }),
            StatementKind::Item(item) => item.serialize(ctx),
        };
        j.expect_object_mut().insert("span".into(), self.span.serialize(ctx));
        j
    }
}

impl JsonSerialize for ForStmt<'_> {
    fn serialize(&self, ctx: &SerializeCtx<'_, '_>) -> Json {
        json!({
            "kind": "ForStmt",
            "init": {self.init.map(|i| i.serialize(ctx)).unwrap_or(Json::Null)},
            "cond": {self.cond.map(|i| i.serialize(ctx)).unwrap_or(Json::Null)},
            "inc": {self.inc.map(|i| i.serialize(ctx)).unwrap_or(Json::Null)},
            "body": {self.body.serialize(ctx)}
        })
    }
}

impl JsonSerialize for semantic::Ty<'_> {
    fn serialize(&self, ctx: &SerializeCtx<'_, '_>) -> Json {
        use semantic::types::{TypeKind, PrimitiveType};
        match &self.kind {
            TypeKind::Primitive(primitive_type) => Json::String(match primitive_type {
                PrimitiveType::I8 => "i8",
                PrimitiveType::I16 => "i16",
                PrimitiveType::I32 => "i32",
                PrimitiveType::I64 => "i64",
                PrimitiveType::U8 => "u8 ",
                PrimitiveType::U16 => "u16",
                PrimitiveType::U32 => "u32",
                PrimitiveType::U64 => "u64",
                PrimitiveType::Char => "cha",
                PrimitiveType::F32 => "f32",
                PrimitiveType::F64 => "f64",
                PrimitiveType::Bool => "bool"
            }.into()),
            TypeKind::Ref(to) => json!({
                "kind": "Ref",
                "to": {to.serialize(ctx)},
            }),
            TypeKind::Array(of, len) => json!({
                "kind": "Array",
                "of": {of.serialize(ctx)},
                "len": {f64::from(*len)},
            }),
            TypeKind::Tuple(items) => json!({
                "kind": "Tuple",
                "items": { Json::Array(items.iter().map(|it| it.serialize(ctx)).collect()) }
            }),
            TypeKind::Function { is_variadic, params, ret_ty } => json!({
                "kind": "Function",
                "is_variadic": {*is_variadic},
                "params": {Json::Array(params.iter().map(|p| p.serialize(ctx)).collect())},
                "ret_type": {ret_ty.serialize(ctx)},
            }),
            TypeKind::Struct { name, fields } => json!({
                "kind": "Struct",
                "name": {name.to_string()},
                "fields": { Json::Array(fields.iter().map(|f| f.serialize(ctx)).collect()) },
            })
        }
    }
}

impl JsonSerialize for semantic::Field<'_> {
    fn serialize(&self, ctx: &SerializeCtx<'_, '_>) -> Json {
        json!({
            "name": {self.name.to_string()},
            "type": {self.ty.serialize(ctx)},
        })
    }
}
