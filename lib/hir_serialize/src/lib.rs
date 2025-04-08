mod node;

use hir::def::DefinitionKind;
use hir::expr::ExpressionKind;
use hir::stmt::StatementKind;
use hir::{Definition, Expression, Program, Statement};
use semantic::Semantic;
use node::Node;

pub fn hir_serialize(hir: &hir::Session<'_>, sem: &Semantic<'_>, src: &str) -> String {
    let prog = hir.get_root_program();
    let node = prog.serialize(sem);
    let mut html = String::from("<html><body>");
    node.write_to(&mut html, src).unwrap();
    html.push_str("</body></html>");
    html
}

trait HirSerialize {
    fn serialize(&self, sem: &Semantic<'_>) -> Node;
}

impl HirSerialize for Program<'_> {
    fn serialize(&self, sem: &Semantic<'_>) -> Node {
        let mut defs = vec![];

        for def in self.defs {
            defs.push(def.serialize(sem));
        }

        Node::Ul(defs)
    }
}

macro_rules! keyval {
    ($n:ident, $k:expr => $val:expr) => {
        {
            let val = Node::from($val).into();
            $n.push(Node::KeyVal($k, val));
        }
    };
}

impl HirSerialize for Definition<'_> {
    fn serialize(&self, sem: &Semantic<'_>) -> Node {
        let mut nodes = vec![Node::Id(self.id)];
        let mut ul = vec![];

        let h1 = match self.kind {
            DefinitionKind::Variable { .. } => "VariableDefinition",
            DefinitionKind::Function { .. } => "FunctionDefinition",
            DefinitionKind::Struct { .. } => "StructDefinition",
        };
        nodes.push(Node::Title(h1));
        nodes.push(Node::Span(self.span));

        keyval!(ul, "name" => self.name.ident.sym.to_string());

        let ty = match self.ty {
            Some(ty) => format!("{ty:?}"),
            None => "???".to_string(),
        };

        keyval!(ul, "type" => ty);

        match &self.kind {
            DefinitionKind::Variable { constness, init } => {
                keyval!(ul, "const" => format!("{constness:?}"));
                if let Some(init) = init {
                    let init = init.serialize(sem).into();
                    ul.push(Node::KeyVal("init", init));
                }
            },
            DefinitionKind::Function { params, body } => {
                if !params.is_empty() {
                    let mut p = vec![];
                    for param in *params {
                        p.push(Node::Text(format!("{name} : {ty:?}",
                                    name = param.name.ident.sym,
                                    ty = param.ty.unwrap()
                        )));
                    }

                    ul.push(Node::KeyVal("params", Node::Ul(p).into()));
                }
                let mut b = vec![];
                for stmt in *body {
                    b.push(stmt.serialize(sem));
                }
                ul.push(Node::KeyVal("body", Node::Ul(b).into()));
            },
            DefinitionKind::Struct { fields } => {
                let mut flist = vec![];
                for field in *fields {
                    flist.push(Node::Text(format!("{} : {:?}", field.name.ident.sym, field.ty)));
                }
                ul.push(Node::KeyVal("fields", Node::Ul(flist).into()));
            },
        }

        nodes.push(Node::Ul(ul));

        Node::List(nodes)

    }
}

impl HirSerialize for Statement<'_> {
    fn serialize(&self, sem: &Semantic<'_>) -> Node {
        let mut list = vec![/* Node::Id(self.id) */];
        let mut attrs = vec![];

        let attr = match self.kind {
            StatementKind::Expr(expr) => {
                list.push(Node::Title("ExprAsStmt"));
                let expr = expr.serialize(sem);
                Node::KeyVal("expr", expr.into())
            }
            StatementKind::Block(stmts) => {
                list.push(Node::Title("BlockStmt"));
                let mut stmts_list = vec![];
                for stmt in stmts {
                    stmts_list.push(stmt.serialize(sem));
                }
                let stmts = Node::Ul(stmts_list);
                Node::KeyVal("stmts", stmts.into())
            }
            StatementKind::If { cond, if_true, if_false } => {
                list.push(Node::Title("IfStmt"));

                attrs.push(Node::KeyVal("cond", cond.serialize(sem).into()));
                attrs.push(Node::KeyVal("true_branch", if_true.serialize(sem).into()));

                Node::KeyVal("false_branch", match if_false {
                    Some(fb) => fb.serialize(sem),
                    None => Node::Text("None".into()),
                }.into())
            }
            StatementKind::While { cond, body } => {
                list.push(Node::Title("WhileStmt"));

                attrs.push(Node::KeyVal("cond", cond.serialize(sem).into()));
                Node::KeyVal("body", body.serialize(sem).into())
            },
            StatementKind::For { init, cond, inc, body } => {
                list.push(Node::Title("ForStmt"));

                macro_rules! serialize_expr {
                    ($e:expr) => {
                        match $e {
                            Some(e) => e.serialize(sem),
                            None => Node::Text("None".into())
                        }.into()
                    };
                }

                attrs.push(Node::KeyVal("init", serialize_expr!(init)));
                attrs.push(Node::KeyVal("cond", serialize_expr!(cond)));
                attrs.push(Node::KeyVal("inc", serialize_expr!(inc)));

                Node::KeyVal("body", body.serialize(sem).into())
            },
            StatementKind::Empty => Node::Empty,
            StatementKind::Break => Node::Text("break".to_string()),
            StatementKind::Continue => Node::Text("continue".to_string()),
            StatementKind::Return(expr) => {
                list.push(Node::Title("Return"));
                if let Some(expr) = expr {
                    Node::KeyVal("expr", expr.serialize(sem).into())
                } else {
                    Node::Empty
                }
            }
            StatementKind::Print(expr) => {
                list.push(Node::Title("PrintStmt"));
                let expr = expr.serialize(sem);
                Node::KeyVal("expr", expr.into())
            }
            StatementKind::Read(expr) => {
                list.push(Node::Title("ReadStmt"));
                let expr = expr.serialize(sem);
                Node::KeyVal("expr", expr.into())
            }
            StatementKind::Def(def) => return def.serialize(sem)
        };

        if !matches!(attr, Node::Empty) {
            attrs.push(attr);
        }

        list.push(Node::Span(self.span));
        list.push(Node::Ul(attrs));
        Node::List(list)
    }
}

impl HirSerialize for Expression<'_> {
    fn serialize(&self, sem: &Semantic<'_>) -> Node {
        let mut list = vec![/* Node::Id(self.id) */];

        let mut attrs = vec![];
        match &self.kind {
            ExpressionKind::Literal(lit_value) => {
                list.push(Node::Title("LiteralExpression"));
                let val = Node::Text(format!("{lit_value:?}"));
                attrs.push(Node::KeyVal("value", val.into()));
            },
            ExpressionKind::Ref(expr) => {
                list.push(Node::Title("RefExpression"));
                let val = expr.serialize(sem);
                attrs.push(Node::KeyVal("of", val.into()));
            }
            ExpressionKind::Assignment { left, right } => {
                list.push(Node::Title("AssignmentExpression"));
                let left = left.serialize(sem);
                let right = right.serialize(sem);
                attrs.push(Node::KeyVal("left", left.into()));
                attrs.push(Node::KeyVal("right", right.into()));
            },
            ExpressionKind::Unary { op, expr } => {
                list.push(Node::Title("UnaryExpr"));
                attrs.push(Node::KeyVal("op", Node::Text(format!("{op:?}")).into()));
                attrs.push(Node::KeyVal("expr", expr.serialize(sem).into()));
            }
            ExpressionKind::Deref(expr) => {
                list.push(Node::Title("DerefExpr"));
                attrs.push(Node::KeyVal("expr", expr.serialize(sem).into()));
            }
            ExpressionKind::Array(_) => todo!(),
            ExpressionKind::Logical { left, op, right } => {
                list.push(Node::Title("LogicalExpr"));
                let left = left.serialize(sem);
                let right = right.serialize(sem);
                attrs.push(Node::KeyVal("op", Node::Text(format!("{op:?}")).into()));
                attrs.push(Node::KeyVal("left", left.into()));
                attrs.push(Node::KeyVal("right", right.into()));
            }
            ExpressionKind::Comparison { left, op, right } => {
                list.push(Node::Title("ComparisonExpr"));
                let left = left.serialize(sem);
                let right = right.serialize(sem);
                attrs.push(Node::KeyVal("op", Node::Text(format!("{op:?}")).into()));
                attrs.push(Node::KeyVal("left", left.into()));
                attrs.push(Node::KeyVal("right", right.into()));
            }
            ExpressionKind::Arithmetic { left, op, right } => {
                list.push(Node::Title("ArithmeticExpr"));
                let left = left.serialize(sem);
                let right = right.serialize(sem);
                attrs.push(Node::KeyVal("op", Node::Text(format!("{op:?}")).into()));
                attrs.push(Node::KeyVal("left", left.into()));
                attrs.push(Node::KeyVal("right", right.into()));
            }
            ExpressionKind::Ternary { cond, if_true, if_false } => {
                list.push(Node::Title("TernaryExpr"));
                let cond = cond.serialize(sem);
                let if_true = if_true.serialize(sem);
                let if_false = if_false.serialize(sem);
                attrs.push(Node::KeyVal("cond", cond.into()));
                attrs.push(Node::KeyVal("if_true", if_true.into()));
                attrs.push(Node::KeyVal("if_false", if_false.into()));
            }
            ExpressionKind::Variable(path) => {
                list.push(Node::Title("VariableExpression"));
                let name = Node::Text(path.ident.sym.to_string());
                let def = Node::Text(path.def.expect_resolved().id.to_string());
                attrs.push(Node::KeyVal("name", name.into()));
                attrs.push(Node::KeyVal("definition", def.into()));
            }
            ExpressionKind::Call { callee, args } => {
                list.push(Node::Title("Call"));
                let callee = callee.serialize(sem);
                attrs.push(Node::KeyVal("callee", callee.into()));

                let mut argl = vec![];
                for arg in *args {
                    argl.push(arg.serialize(sem));
                }

                let args = Node::List(argl);
                attrs.push(Node::KeyVal("args", args.into()));
            },
            ExpressionKind::Cast { expr, to } => {
                list.push(Node::Title("Call"));
                attrs.push(Node::KeyVal("expr", expr.serialize(sem).into()));
                attrs.push(Node::KeyVal("to", Node::Text(format!("{to:?}")).into()));

            },
            ExpressionKind::ArrayAccess { arr, index } => {
                list.push(Node::Title("ArrayAccess"));
                attrs.push(Node::KeyVal("arr", arr.serialize(sem).into()));
                attrs.push(Node::KeyVal("index", index.serialize(sem).into()));
            }
            ExpressionKind::StructAccess { st, field } => {
                list.push(Node::Title("StructAccess"));
                attrs.push(Node::KeyVal("struct", st.serialize(sem).into()));
                attrs.push(Node::KeyVal("field", Node::Text(field.sym.to_string()).into()));
            }
        }

        let ty = match sem.type_of(&self.id) {
            Some(t) => Node::Text(format!("{t}")),
            None => Node::Text("???".to_string()),
        };
        attrs.push(Node::KeyVal("type", ty.into()));

        list.push(Node::Span(self.span));
        list.push(Node::Ul(attrs));

        Node::List(list)
    }
}
