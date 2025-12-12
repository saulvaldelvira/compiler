mod node;

use hir::{BlockExpr, Item, ItemKind};
use hir::{
    Expression, Module, Statement, expr::ExpressionKind,
    stmt::StatementKind,
};
use node::Node;
use semantic::Semantic;
use span::source::SourceMap;

pub fn hir_print_html(hir: &hir::Session<'_>, sem: &Semantic<'_>, src: &SourceMap) -> String {
    let prog = hir.get_root();
    let serializer = HirPrinter { sem, sm: src };
    let node = serializer.serialize_module(prog);
    let mut html = String::from(
        r#"<html>
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
        <button onclick="collapseAll();"> Collapse all </button>"#,
    );
    node.write_to(&mut html, src).unwrap();
    node.write_spans_full(&mut html, src).unwrap();
    html.push_str("</body></html>");
    html
}

struct HirPrinter<'ser, 'sem> {
    sem: &'ser Semantic<'sem>,
    sm: &'ser SourceMap,
}

macro_rules! keyval {
    ($n:ident, $k:expr => $val:expr) => {{
        let val = Node::from($val).into();
        $n.push(Node::KeyVal($k, val));
    }};
}

impl HirPrinter<'_, '_> {
    fn serialize_module(&self, prog: &Module<'_>) -> Node {
        let mut defs = vec![];
        for def in prog.items {
            defs.push(self.serialize_item(def));
        }
        let nodes = vec![
            Node::Title("Module"),
            prog.name.ident.sym.borrow(|name| {
                Node::Text(format!(" ({name}) ").into())
            }),
            Node::Span(prog.span)
        ];

        let mut childs = vec![];

        if let Some(file) = prog.extern_file {
            let file = self.sm.get_file_for_id(&file).unwrap();
            if let Some(fname) = file.filename() {
                childs.push(Node::KeyVal("source_file", Node::Text(format!("{fname:?}").into()).into()));
            }
        }

        childs.push(Node::Collapse(Node::Text("items".into()).into(), Node::Ul(defs).into()));

        Node::Collapse(Node::List(nodes).into(), Node::Ul(childs).into())
    }

    fn serialize_item(&self, def: &Item<'_>) -> Node {
        let mut nodes = vec![Node::DefId(def.id)];
        let mut ul = vec![];

        let h1 = match def.kind {
            ItemKind::Variable { .. } => "VariableDefinition",
            ItemKind::Function { .. } => "FunctionDefinition",
            ItemKind::Struct { .. } => "StructDefinition",
            ItemKind::Mod { .. } => "Module",
            ItemKind::Use(_) |
            ItemKind::TypeAlias { .. } => "Use",
        };
        nodes.push(Node::Title(h1));
        nodes.push(Node::Span(def.span));

        keyval!(ul, "name" => def.get_name().map(|s| s.to_string()).unwrap_or_else(|| String::from("???")));

        if let Some(ty) = self.sem.type_of(&def.id) {
            keyval!(ul, "type" => ty.to_string());
        }

        match &def.kind {
            ItemKind::Variable {
                constness, init, ..
            } => {
                keyval!(ul, "const" => format!("{constness:?}"));
                if let Some(init) = init {
                    let init = self.serialize_expr(init).into();
                    ul.push(Node::KeyVal("init", init));
                }
            }
            ItemKind::Use(u) => {
                let mut path = String::new();
                for (i, seg) in u.path.segments.iter().enumerate() {
                    if i > 0 {
                        path.push_str("::");
                    }
                    seg.ident.sym.borrow(|name| {
                        path.push_str(name);
                    });
                }
                ul.push(Node::KeyVal("path", Node::Text(path.into()).into()));
                if let Some(new_name) = u.new_name {
                    ul.push(Node::KeyVal("as", Node::Text(new_name.ident.sym.to_string().into()).into()));
                }
            }
            ItemKind::TypeAlias { .. } => {}
            ItemKind::Function { params, body, is_extern, is_variadic, .. } => {
                if *is_extern {
                    ul.push(Node::Text("extern = true".into()));
                }
                if *is_variadic {
                    ul.push(Node::Text("variadic = true".into()));
                }
                if !params.is_empty() {
                    let mut p = vec![];
                    for param in *params {
                        let ty = self.sem.type_of(&param.id).unwrap();
                        p.push(Node::text(format!(
                            "{name} : {ty}",
                            name = param.get_name(),
                        )));
                    }

                    ul.push(Node::collapse("params", Node::Ul(p)));
                }
                if let Some(body) = body {
                    let mut stmts = Vec::new();
                    self.serialize_block_expr(&mut stmts, body);
                    ul.push(Node::collapse("body", Node::Ul(stmts)));
                }
            }
            ItemKind::Struct { fields, .. } => {
                let mut flist = vec![];
                for field in *fields {
                    flist.push(Node::text(format!(
                        "{} : {:?}",
                        field.name.ident.sym, field.ty
                    )));
                }
                ul.push(Node::collapse("fields", Node::Ul(flist)));
            },
            ItemKind::Mod(module) => {
                return self.serialize_module(module)
            }
        }

        Node::Collapse(Node::List(nodes).into(), Node::Ul(ul).into())
    }

    fn serialize_block_expr(&self, attrs: &mut Vec<Node>, block: &BlockExpr<'_>) {
        let mut stmts_list = vec![];
        for stmt in block.stmts {
            stmts_list.push(self.serialize_stmt(stmt));
        }
        let stmts = Node::Ul(stmts_list);
        attrs.push(Node::collapse("stmts", stmts));
        if let Some(tail) = block.tail {
            let tail = self.serialize_expr(tail).into();
            attrs.push(Node::KeyVal("tail", tail));
        }
    }

    #[allow(clippy::too_many_lines)]
    fn serialize_expr(&self, expr: &Expression<'_>) -> Node {
        let mut title = vec![/* Node::Id(expr.id) */];

        let mut attrs = vec![];
        match &expr.kind {
            ExpressionKind::Literal(lit_value) => {
                title.push(Node::Title("LiteralExpression"));
                let val = Node::text(format!("{lit_value:?}"));
                attrs.push(Node::KeyVal("value", val.into()));
            }
            ExpressionKind::Ref(expr) => {
                title.push(Node::Title("RefExpression"));
                let val = self.serialize_expr(expr).into();
                attrs.push(Node::KeyVal("of", val));
            }
            ExpressionKind::Assignment { left, right } => {
                title.push(Node::Title("AssignmentExpression"));
                let left = self.serialize_expr(left).into();
                let right = self.serialize_expr(right).into();
                attrs.push(Node::KeyVal("left", left));
                attrs.push(Node::KeyVal("right", right));
            }
            ExpressionKind::Unary { op, expr } => {
                title.push(Node::Title("UnaryExpr"));
                attrs.push(Node::KeyVal("op", Node::text(format!("{op:?}")).into()));
                attrs.push(Node::KeyVal("expr", self.serialize_expr(expr).into()));
            }
            ExpressionKind::Deref(expr) => {
                title.push(Node::Title("DerefExpr"));
                attrs.push(Node::KeyVal("expr", self.serialize_expr(expr).into()));
            }
            ExpressionKind::Array(_) => todo!(),
            ExpressionKind::Logical { left, op, right } => {
                title.push(Node::Title("LogicalExpr"));
                let left = self.serialize_expr(left).into();
                let right = self.serialize_expr(right).into();
                attrs.push(Node::KeyVal("op", Node::text(format!("{op:?}")).into()));
                attrs.push(Node::KeyVal("left", left));
                attrs.push(Node::KeyVal("right", right));
            }
            ExpressionKind::Comparison { left, op, right } => {
                title.push(Node::Title("ComparisonExpr"));
                let left = self.serialize_expr(left).into();
                let right = self.serialize_expr(right).into();
                attrs.push(Node::KeyVal("op", Node::text(format!("{op:?}")).into()));
                attrs.push(Node::KeyVal("left", left));
                attrs.push(Node::KeyVal("right", right));
            }
            ExpressionKind::Arithmetic { left, op, right } => {
                title.push(Node::Title("ArithmeticExpr"));
                let left = self.serialize_expr(left).into();
                let right = self.serialize_expr(right).into();
                attrs.push(Node::KeyVal("op", Node::text(format!("{op:?}")).into()));
                attrs.push(Node::KeyVal("left", left));
                attrs.push(Node::KeyVal("right", right));
            }
            ExpressionKind::If {
                cond,
                if_true,
                if_false,
            } => {
                title.push(Node::Title("If"));
                let cond = self.serialize_expr(cond).into();
                attrs.push(Node::KeyVal("cond", cond));
                let mut block = Vec::new();
                self.serialize_block_expr(&mut block, if_true);
                attrs.push(Node::Collapse(Node::Title("if_true").into(), Node::Ul(block).into()));
                if let Some(if_false) = if_false {
                    let mut block = Vec::new();
                    self.serialize_block_expr(&mut block, if_false);
                    attrs.push(Node::Collapse(Node::Title("if_true").into(), Node::Ul(block).into()));
                }
            }
            ExpressionKind::Block(block) => {
                self.serialize_block_expr(&mut attrs, block);
            }
            ExpressionKind::Variable(path) => {
                title.push(Node::Title("VariableExpression"));
                let name = Node::text(path.segments.first().unwrap().ident.sym.to_string());
                let def = Node::Id(path.def().expect_resolved());
                attrs.push(Node::KeyVal("name", name.into()));
                attrs.push(Node::KeyVal("definition", def.into()));
            }
            ExpressionKind::Call { callee, args } => {
                title.push(Node::Title("Call"));
                let callee = self.serialize_expr(callee).into();
                attrs.push(Node::KeyVal("callee", callee));

                let mut argl = vec![];
                for arg in *args {
                    argl.push(self.serialize_expr(arg));
                }

                let args = Node::List(argl);
                attrs.push(Node::collapse("args", args));
            }
            ExpressionKind::Cast { expr, to } => {
                title.push(Node::Title("Call"));
                attrs.push(Node::KeyVal("expr", self.serialize_expr(expr).into()));
                attrs.push(Node::KeyVal("to", Node::text(format!("{to:?}")).into()));
            }
            ExpressionKind::ArrayAccess { arr, index } => {
                title.push(Node::Title("ArrayAccess"));
                attrs.push(Node::KeyVal("arr", self.serialize_expr(arr).into()));
                attrs.push(Node::KeyVal("index", self.serialize_expr(index).into()));
            }
            ExpressionKind::StructAccess { st, field } => {
                title.push(Node::Title("StructAccess"));
                attrs.push(Node::KeyVal("struct", self.serialize_expr(st).into()));
                attrs.push(Node::KeyVal(
                    "field",
                    Node::text(field.sym.to_string()).into(),
                ));
            }
        }

        let ty = match self.sem.type_of(&expr.id) {
            Some(t) => Node::text(format!("{t}")),
            None => Node::text("???".to_string()),
        };
        attrs.push(Node::KeyVal("type", ty.into()));

        title.push(Node::Span(expr.span));

        Node::Collapse(Node::List(title).into(), Node::Ul(attrs).into())
    }

    fn serialize_stmt(&self, stmt: &Statement<'_>) -> Node {
        let mut list = vec![/* Node::Id(self.id) */];
        let mut attrs = vec![];

        let attr = match stmt.kind {
            StatementKind::Expr(expr) => {
                list.push(Node::Title("ExprAsStmt"));
                let expr = self.serialize_expr(expr).into();
                Node::KeyVal("expr", expr)
            }
            StatementKind::While { cond, body } => {
                list.push(Node::Title("WhileStmt"));

                attrs.push(Node::KeyVal("cond", self.serialize_expr(cond).into()));
                Node::collapse("body", self.serialize_stmt(body))
            }
            StatementKind::For {
                init,
                cond,
                inc,
                body,
            } => {
                list.push(Node::Title("ForStmt"));

                macro_rules! serialize_expr {
                    ($n:literal, $e:expr) => {
                        match $e {
                            Some(e) => Node::KeyVal($n, self.serialize_expr(e).into()),
                            None => Node::KeyVal($n, Node::Text("None".into()).into()),
                        }
                    };
                }

                attrs.push(match init {
                    Some(e) => Node::KeyVal("init", self.serialize_item(e).into()),
                    None => Node::KeyVal("init", Node::Text("None".into()).into()),
                });
                attrs.push(serialize_expr!("cond", cond));
                attrs.push(serialize_expr!("inc", inc));

                Node::KeyVal("body", self.serialize_stmt(body).into())
            }
            StatementKind::Empty => Node::Empty,
            StatementKind::Break => Node::text("break"),
            StatementKind::Continue => Node::text("continue"),
            StatementKind::Return(expr) => {
                list.push(Node::Title("Return"));
                if let Some(expr) = expr {
                    Node::KeyVal("expr", self.serialize_expr(expr).into())
                } else {
                    Node::Empty
                }
            }
            StatementKind::Item(def) => return self.serialize_item(def),
        };

        if !matches!(attr, Node::Empty) {
            attrs.push(attr);
        }

        list.push(Node::Span(stmt.span));

        Node::Collapse(Node::List(list).into(), Node::Ul(attrs).into())
    }
}
