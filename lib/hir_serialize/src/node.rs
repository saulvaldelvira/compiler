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


    fn __write_to(&self, f: &mut dyn fmt::Write, src: &str, span_count: &mut usize) -> fmt::Result {
        match self {
            Node::List(list) => {
                for node in list {
                    node.__write_to(f, src, span_count)?;
                }
                Ok(())
            },
            Node::Ul(nodes) => {
                writeln!(f, "<ul>")?;
                for node in nodes {
                    writeln!(f, "<li>")?;
                    node.__write_to(f, src, span_count)?;
                    writeln!(f, "</li>")?;
                }
                writeln!(f, "</ul>")
            }
            Node::KeyVal(k, v) => {
                write!(f, "{k} = ")?;
                v.__write_to(f, src, span_count)
            },
            Node::Id(id) => writeln!(f, "({id}) "),
            Node::Title(v) => writeln!(f, "<b>{v}</b>"),
            Node::Text(txt) => write!(f, "{txt}"),
            Node::Empty => Ok(()),
            Node::Span(span) => {
                *span_count += 1;
                let FilePosition { start_line, start_col, .. } = span.file_position(src);
                write!(f, "(<a id=\"back_{s}\" href=\"#span_{s}\">[{start_line}:{start_col}]</a> : \"", s = *span_count)?;

                let n = span.len.min(50);
                for c in span.slice(src).chars().filter(|&c| c != '\n').take(n) {
                    write!(f, "{c}")?;
                }
                if n < span.len {
                    write!(f, "... ")?;
                }
                write!(f, "\"")
            }
        }
    }

    pub fn write_to(&self, f: &mut dyn fmt::Write, src: &str) -> fmt::Result {
        let mut span_count = 0;
        self.__write_to(f, src, &mut span_count)
    }

    pub fn write_spans_full(&self, f: &mut dyn fmt::Write, src: &str) -> fmt::Result {
        let mut span_count = 0;
        write!(f, "<h2> Spans </h2>")?;
        write!(f, "<ol>")?;
        self.__write_spans_full(f, src, &mut span_count)?;
        write!(f, "</ol>")
    }

    fn __write_spans_full(&self, f: &mut dyn fmt::Write, src: &str, span_count: &mut usize) -> fmt::Result {
        match self {
            Node::List(list) => {
                for node in list {
                    node.__write_spans_full(f, src, span_count)?;
                }
                Ok(())
            },
            Node::Ul(nodes) => {
                for node in nodes {
                    node.__write_spans_full(f, src, span_count)?;
                }
                Ok(())
            }
            Node::KeyVal(_, v) => {
                v.__write_spans_full(f, src, span_count)
            },
            Node::Id(_) |
            Node::Title(_) |
            Node::Text(_) |
            Node::Empty => Ok(()),
            Node::Span(span) => {
                *span_count += 1;
                let FilePosition { start_line, start_col, end_line, end_col } = span.file_position(src);
                write!(f, "<li id=\"span_{}\">", *span_count)?;
                write!(f, "<p> [{start_line}:{start_col}] .. [{end_line}:{end_col}] ")?;
                write!(f, "<a href=\"#back_{}\"> ^ </a> </p>", *span_count)?;
                let slice = span.slice(src);
                write!(f, "<pre>{slice}</pre>")?;
                write!(f, "</li>")
            }
        }
    }
}

