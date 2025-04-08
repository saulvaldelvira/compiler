use core::fmt;
use hir::HirId;
use span::{FilePosition, Span};

pub enum Node {
    List(Vec<Node>),
    Ul(Vec<Node>),
    KeyVal(&'static str,Box<Node>),
    Text(String),
    Title(&'static str),
    Id(HirId),
    Span(Span),
    Empty,
}

impl From<String> for Node {
    fn from(value: String) -> Self {
        Node::Text(value)
    }
}

impl Node {
    pub fn write_to(&self, f: &mut dyn fmt::Write, src: &str) -> fmt::Result {
        match self {
            Node::List(list) => {
                for node in list {
                    node.write_to(f, src)?;
                }
                Ok(())
            },
            Node::Ul(nodes) => {
                writeln!(f, "<ul>")?;
                for node in nodes {
                    writeln!(f, "<li>")?;
                    node.write_to(f, src)?;
                    writeln!(f, "</li>")?;
                }
                writeln!(f, "</ul>")
            }
            Node::KeyVal(k, v) => {
                write!(f, "{k} = ")?;
                v.write_to(f, src)
            },
            Node::Id(id) => writeln!(f, "({id}) "),
            Node::Title(v) => writeln!(f, "<b>{v}</b>"),
            Node::Text(txt) => write!(f, "{txt}"),
            Node::Empty => Ok(()),
            Node::Span(span) => {
                let FilePosition { start_line, start_col, .. } = span.file_position(src);
                let txt = &src[span.offset..];
                let n = txt.chars().take(span.len).take_while(|&c| c != '\n').map(|c| c.len_utf8()).sum();
                let txt = &txt[..n];

                write!(f, "[{start_line}:{start_col}]: \"{txt}\"")
            }
        }
    }
}

