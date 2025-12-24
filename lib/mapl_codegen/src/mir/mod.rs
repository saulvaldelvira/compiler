use std::fmt::Display;

use semantic::Ty;

use crate::code_generator::MemoryAddress;

#[derive(Debug, Clone, Copy)]
pub enum MaplArithmetic {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl Display for MaplArithmetic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            MaplArithmetic::Add => "ADD",
            MaplArithmetic::Sub => "SUB",
            MaplArithmetic::Mul => "MUL",
            MaplArithmetic::Div => "DIV",
            MaplArithmetic::Mod => "MOD",
        })
    }
}

impl From<hir::expr::ArithmeticOp> for MaplArithmetic {
    fn from(value: hir::expr::ArithmeticOp) -> Self {
        use hir::expr::ArithmeticOp;
        match value {
            ArithmeticOp::Add => MaplArithmetic::Add,
            ArithmeticOp::Sub => MaplArithmetic::Sub,
            ArithmeticOp::Mul => MaplArithmetic::Mul,
            ArithmeticOp::Div => MaplArithmetic::Div,
            ArithmeticOp::Mod => MaplArithmetic::Mod,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MaplComparison {
    Gt,
    Ge,
    Lt,
    Le,
    Eq,
    Neq,
}

impl Display for MaplComparison {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            MaplComparison::Gt => "GT",
            MaplComparison::Ge => "GE",
            MaplComparison::Lt => "LT",
            MaplComparison::Le => "LE",
            MaplComparison::Eq => "EQ",
            MaplComparison::Neq => "NE",
        })
    }
}

impl From<hir::expr::CmpOp> for MaplComparison {
    fn from(value: hir::expr::CmpOp) -> Self {
        use hir::expr::CmpOp;
        match value {
            CmpOp::Gt => MaplComparison::Gt,
            CmpOp::Ge => MaplComparison::Ge,
            CmpOp::Lt => MaplComparison::Lt,
            CmpOp::Le => MaplComparison::Le,
            CmpOp::Eq => MaplComparison::Eq,
            CmpOp::Neq => MaplComparison::Neq,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MaplLogical {
    And,
    Or,
}

impl From<hir::expr::LogicalOp> for MaplLogical {
    fn from(value: hir::expr::LogicalOp) -> Self {
        use hir::expr::LogicalOp;
        match value {
            LogicalOp::And => MaplLogical::And,
            LogicalOp::Or => MaplLogical::Or,
        }
    }
}

impl Display for MaplLogical {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            MaplLogical::And => "And",
            MaplLogical::Or => "OR",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MaplType {
    Int,
    Float,
    Byte,
}

impl Display for MaplType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::Int => "I",
            Self::Float => "F",
            Self::Byte => "B",
        })
    }
}

#[derive(Debug)]
pub enum MaplLiteral {
    Int(i16),
    Float(f32),
    Byte(u8),
}

impl Display for MaplLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MaplLiteral::Int(i) => write!(f, "{i}"),
            MaplLiteral::Float(n) => write!(f, "{n}"),
            MaplLiteral::Byte(b) => write!(f, "{b}"),
        }
    }
}

impl MaplLiteral {
    fn get_type(&self) -> MaplType {
        match self {
            MaplLiteral::Int(_) => MaplType::Int,
            MaplLiteral::Float(_) => MaplType::Float,
            MaplLiteral::Byte(_) => MaplType::Byte,
        }
    }
}

impl From<&'_ Ty<'_>> for MaplType {
    fn from(value: &'_ Ty<'_>) -> Self {
        use semantic::types::{PrimitiveType, TypeKind};
        match &value.kind {
            TypeKind::Primitive(primitive_type) => {
                match primitive_type {
                    PrimitiveType::I16 |
                    PrimitiveType::Bool => MaplType::Int,
                    PrimitiveType::Char => MaplType::Byte,
                    PrimitiveType::F32 => MaplType::Float,
                    PrimitiveType::I8 |
                    PrimitiveType::I32 |
                    PrimitiveType::I64 |
                    PrimitiveType::U8 |
                    PrimitiveType::U16 |
                    PrimitiveType::U32 |
                    PrimitiveType::U64 |
                    PrimitiveType::F64 => panic!("Unsupported numeric type"),
                }
            }
            TypeKind::Ref(_) => MaplType::Int,
            TypeKind::Tuple(_) |
            TypeKind::Array(..) => todo!(),
            TypeKind::Struct { .. } => todo!(),
            TypeKind::Function { .. } => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum MaplInstruction {
    DefineLabel(String),
    Enter(usize),
    Jmp(String),
    Jz(String),
    Arithmetic {
        left: Box<MaplInstruction>,
        right: Box<MaplInstruction>,
        op: MaplArithmetic,
        ty: MaplType,
    },
    Comparison {
        left: Box<MaplInstruction>,
        right: Box<MaplInstruction>,
        op: MaplComparison,
        ty: MaplType,
    },
    Logical {
        left: Box<MaplInstruction>,
        right: Box<MaplInstruction>,
        op: MaplLogical,
    },
    Cast {
        mapl: Box<MaplInstruction>,
        from: MaplType,
        to: MaplType,
    },
    Not(Box<MaplInstruction>),
    Push(MaplLiteral),
    Pushaddr(MemoryAddress),
    #[expect(unused)]
    Out(MaplType),
    #[expect(unused)]
    In(MaplType),
    Pop(MaplType),
    Load(MaplType),
    Store(MaplType),
    Compose(Box<[MaplInstruction]>),
    Call(String),
    Return {
        locals: u16,
        params: u16,
        ret_size: u16,
    },
    Halt,

    Literal(String),
    Empty,
}

impl Display for MaplInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MaplInstruction::DefineLabel(l) => writeln!(f, "{l}: "),
            MaplInstruction::Enter(n) => writeln!(f, "enter {n}"),
            MaplInstruction::Jmp(label) => writeln!(f, "jmp {label}"),
            MaplInstruction::Jz(label) => writeln!(f, "jz {label}"),
            MaplInstruction::Arithmetic {
                left,
                right,
                op,
                ty,
            } => {
                Display::fmt(&left, f)?;
                Display::fmt(&right, f)?;
                writeln!(f, "{op}{ty}")
            }
            MaplInstruction::Comparison {
                left,
                right,
                op,
                ty,
            } => {
                Display::fmt(&left, f)?;
                Display::fmt(&right, f)?;
                writeln!(f, "{op}{ty}")
            }
            MaplInstruction::Logical { left, right, op } => {
                Display::fmt(&left, f)?;
                Display::fmt(&right, f)?;
                writeln!(f, "{op}")
            }
            MaplInstruction::Cast { mapl, from, to } => {
                Display::fmt(mapl, f)?;
                writeln!(f, "{from}2{to}")
            }
            MaplInstruction::Push(lit) => {
                writeln!(f, "PUSH{suffix} {lit}", suffix = lit.get_type())
            }
            MaplInstruction::Pushaddr(addr) => {
                match addr {
                    MemoryAddress::Absolute(abs) => writeln!(f, "PUSHA {abs}"),
                    MemoryAddress::Relative(rel) => writeln!(f, "PUSHA bp\nPUSHI {rel}\nADDI"),
                }
            }
            MaplInstruction::Pop(ty) => writeln!(f, "POP{ty}"),
            MaplInstruction::Load(ty) => writeln!(f, "LOAD{ty}"),
            MaplInstruction::Store(ty) => writeln!(f, "STORE{ty}"),
            MaplInstruction::Compose(c) => {
                for ins in c {
                    Display::fmt(ins, f)?;
                }
                Ok(())
            }
            MaplInstruction::Call(l) => writeln!(f, "CALL {l}"),
            MaplInstruction::Halt => writeln!(f, "halt"),
            MaplInstruction::Out(ty) => writeln!(f, "OUT{ty}"),
            MaplInstruction::In(ty) => writeln!(f, "IN{ty}"),
            MaplInstruction::Empty => Ok(()),
            MaplInstruction::Return {
                locals,
                params,
                ret_size,
            } => {
                writeln!(f, "ret {ret_size}, {locals}, {params}")
            }
            MaplInstruction::Literal(c) => writeln!(f, "{c}"),
            MaplInstruction::Not(ins) => {
                Display::fmt(ins, f)?;
                writeln!(f, "NOT")
            }
        }
    }
}
