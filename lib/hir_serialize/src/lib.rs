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
    let mut html = String::from(r#"<html>
        <script>
        function expandAll() {
            document.querySelectorAll("details")
                    .forEach((n) => n.setAttribute("open", ""))
        }
        function collapseAll() {
            document.querySelectorAll("details")
                    .forEach((n) => n.removeAttribute("open"))
        }
        </script>
        <body>

        <button onclick="expandAll();"> Expand all </button>
        <button onclick="collapseAll();"> Collapse all </button>"#);
    node.write_to(&mut html, src).unwrap();
    node.write_spans_full(&mut html, src).unwrap();
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
                        p.push(Node::text(format!("{name} : {ty:?}",
                                    name = param.name.ident.sym,
                                    ty = param.ty.unwrap()
                        )));
                    }

                    ul.push(Node::collapse("params", Node::Ul(p)));
                }
                let mut b = vec![];
                for stmt in *body {
                    b.push(stmt.serialize(sem));
                }
                ul.push(Node::collapse("body", Node::Ul(b)));
            },
            DefinitionKind::Struct { fields } => {
                let mut flist = vec![];
                for field in *fields {
                    flist.push(Node::text(format!("{} : {:?}", field.name.ident.sym, field.ty)));
                }
                ul.push(Node::collapse("fields", Node::Ul(flist)));
            },
        }

        Node::Collapse(Node::List(nodes).into(), Node::Ul(ul).into())
    }
}

impl HirSerialize for Statement<'_> {
    fn serialize(&self, sem: &Semantic<'_>) -> Node {
        let mut list = vec![/* Node::Id(self.id) */];
        let mut attrs = vec![];

        let attr = match self.kind {
            StatementKind::Expr(expr) => {
                list.push(Node::Title("ExprAsStmt"));
                let expr = expr.serialize(sem).into();
                Node::KeyVal("expr", expr)
            }
            StatementKind::Block(stmts) => {
                list.push(Node::Title("BlockStmt"));
                let mut stmts_list = vec![];
                for stmt in stmts {
                    stmts_list.push(stmt.serialize(sem));
                }
                let stmts = Node::Ul(stmts_list);
                Node::collapse("stmts", stmts)
            }
            StatementKind::If { cond, if_true, if_false } => {
                list.push(Node::Title("IfStmt"));

                attrs.push(Node::KeyVal("cond", cond.serialize(sem).into()));
                attrs.push(Node::KeyVal("true_branch", if_true.serialize(sem).into()));

                match if_false {
                    Some(fb) => Node::KeyVal("false_branch", fb.serialize(sem).into()),
                    None => Node::KeyVal("false_branch", Node::Text("None".into()).into()),
                }
            }
            StatementKind::While { cond, body } => {
                list.push(Node::Title("WhileStmt"));

                attrs.push(Node::KeyVal("cond", cond.serialize(sem).into()));
                Node::collapse("body", body.serialize(sem))
            },
            StatementKind::For { init, cond, inc, body } => {
                list.push(Node::Title("ForStmt"));

                macro_rules! serialize_expr {
                    ($n:literal, $e:expr) => {
                        match $e {
                            Some(e) => Node::KeyVal($n, e.serialize(sem).into()),
                            None => Node::KeyVal($n, Node::Text("None".into()).into()),
                        }
                    };
                }

                attrs.push(serialize_expr!("init", init));
                attrs.push(serialize_expr!("cond", cond));
                attrs.push(serialize_expr!("inc", inc));

                Node::KeyVal("body", body.serialize(sem).into())
            },
            StatementKind::Empty => Node::Empty,
            StatementKind::Break => Node::text("break"),
            StatementKind::Continue => Node::text("continue"),
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
                let expr = expr.serialize(sem).into();
                Node::KeyVal("expr", expr)
            }
            StatementKind::Read(expr) => {
                list.push(Node::Title("ReadStmt"));
                let expr = expr.serialize(sem).into();
                Node::KeyVal("expr", expr)
            }
            StatementKind::Def(def) => return def.serialize(sem)
        };

        if !matches!(attr, Node::Empty) {
            attrs.push(attr);
        }

        list.push(Node::Span(self.span));

        Node::Collapse(Node::List(list).into(), Node::Ul(attrs).into())
    }
}

impl HirSerialize for Expression<'_> {
    fn serialize(&self, sem: &Semantic<'_>) -> Node {
        let mut title = vec![/* Node::Id(self.id) */];

        let mut attrs = vec![];
        match &self.kind {
            ExpressionKind::Literal(lit_value) => {
                title.push(Node::Title("LiteralExpression"));
                let val = Node::text(format!("{lit_value:?}"));
                attrs.push(Node::KeyVal("value", val.into()));
            },
            ExpressionKind::Ref(expr) => {
                title.push(Node::Title("RefExpression"));
                let val = expr.serialize(sem).into();
                attrs.push(Node::KeyVal("of", val));
            }
            ExpressionKind::Assignment { left, right } => {
                title.push(Node::Title("AssignmentExpression"));
                let left = left.serialize(sem).into();
                let right = right.serialize(sem).into();
                attrs.push(Node::KeyVal("left", left));
                attrs.push(Node::KeyVal("right", right));
            },
            ExpressionKind::Unary { op, expr } => {
                title.push(Node::Title("UnaryExpr"));
                attrs.push(Node::KeyVal("op", Node::text(format!("{op:?}")).into()));
                attrs.push(Node::KeyVal("expr", expr.serialize(sem).into()));
            }
            ExpressionKind::Deref(expr) => {
                title.push(Node::Title("DerefExpr"));
                attrs.push(Node::KeyVal("expr", expr.serialize(sem).into()));
            }
            ExpressionKind::Array(_) => todo!(),
            ExpressionKind::Logical { left, op, right } => {
                title.push(Node::Title("LogicalExpr"));
                let left = left.serialize(sem).into();
                let right = right.serialize(sem).into();
                attrs.push(Node::KeyVal("op", Node::text(format!("{op:?}")).into()));
                attrs.push(Node::KeyVal("left", left));
                attrs.push(Node::KeyVal("right", right));
            }
            ExpressionKind::Comparison { left, op, right } => {
                title.push(Node::Title("ComparisonExpr"));
                let left = left.serialize(sem).into();
                let right = right.serialize(sem).into();
                attrs.push(Node::KeyVal("op", Node::text(format!("{op:?}")).into()));
                attrs.push(Node::KeyVal("left", left));
                attrs.push(Node::KeyVal("right", right));
            }
            ExpressionKind::Arithmetic { left, op, right } => {
                title.push(Node::Title("ArithmeticExpr"));
                let left = left.serialize(sem).into();
                let right = right.serialize(sem).into();
                attrs.push(Node::KeyVal("op", Node::text(format!("{op:?}")).into()));
                attrs.push(Node::KeyVal("left", left));
                attrs.push(Node::KeyVal("right", right));
            }
            ExpressionKind::Ternary { cond, if_true, if_false } => {
                title.push(Node::Title("TernaryExpr"));
                let cond = cond.serialize(sem).into();
                let if_true = if_true.serialize(sem).into();
                let if_false = if_false.serialize(sem).into();
                attrs.push(Node::KeyVal("cond", cond));
                attrs.push(Node::KeyVal("if_true", if_true));
                attrs.push(Node::KeyVal("if_false", if_false));
            }
            ExpressionKind::Variable(path) => {
                title.push(Node::Title("VariableExpression"));
                let name = Node::text(path.ident.sym.to_string());
                let def = Node::text(path.def.expect_resolved().id.to_string());
                attrs.push(Node::KeyVal("name", name.into()));
                attrs.push(Node::KeyVal("definition", def.into()));
            }
            ExpressionKind::Call { callee, args } => {
                title.push(Node::Title("Call"));
                let callee = callee.serialize(sem).into();
                attrs.push(Node::KeyVal("callee", callee));

                let mut argl = vec![];
                for arg in *args {
                    argl.push(arg.serialize(sem));
                }

                let args = Node::List(argl);
                attrs.push(Node::collapse("args", args));
            },
            ExpressionKind::Cast { expr, to } => {
                title.push(Node::Title("Call"));
                attrs.push(Node::KeyVal("expr", expr.serialize(sem).into()));
                attrs.push(Node::KeyVal("to", Node::text(format!("{to:?}")).into()));

            },
            ExpressionKind::ArrayAccess { arr, index } => {
                title.push(Node::Title("ArrayAccess"));
                attrs.push(Node::KeyVal("arr", arr.serialize(sem).into()));
                attrs.push(Node::KeyVal("index", index.serialize(sem).into()));
            }
            ExpressionKind::StructAccess { st, field } => {
                title.push(Node::Title("StructAccess"));
                attrs.push(Node::KeyVal("struct", st.serialize(sem).into()));
                attrs.push(Node::KeyVal("field", Node::text(field.sym.to_string()).into()));
            }
        }

        let ty = match sem.type_of(&self.id) {
            Some(t) => Node::text(format!("{t}")),
            None => Node::text("???".to_string()),
        };
        attrs.push(Node::KeyVal("type", ty.into()));

        title.push(Node::Span(self.span));

        Node::Collapse(Node::List(title).into(), Node::Ul(attrs).into())
    }
}
