use std::fmt;

use crate::Position;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
}
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Eq,
    Add,
    Sub,
    Mul,
    Div,
    Pow,

    And,
    Or,
    EqEq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    In,
}

#[derive(Clone, PartialEq)]
pub enum NodeType<'a> {
    Int(u32),
    Float(f64),
    Bool(bool),
    Str(String),
    Char(char),
    Identifier(String),

    Unary(UnaryOp, Box<Node<'a>>),
    Binary(Box<Node<'a>>, BinaryOp, Box<Node<'a>>),

    Array(Vec<Node<'a>>),

    If(Box<Node<'a>>, Box<Node<'a>>, Option<Box<Node<'a>>>),
    While(Box<Node<'a>>, Box<Node<'a>>),
    For(Box<Node<'a>>, Box<Node<'a>>, Box<Node<'a>>),
    Fn(Box<Node<'a>>, Vec<Node<'a>>, Box<Node<'a>>),

    Return(Box<Node<'a>>),
    Call(Box<Node<'a>>, Vec<Node<'a>>),
    Index(Box<Node<'a>>, Box<Node<'a>>),

    Statements(Vec<Node<'a>>),
    EOF,
}

impl<'a> fmt::Display for NodeType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use NodeType::*;
        match self {
            Int(value) => write!(f, "{}", value),
            Float(value) => write!(f, "{}f", value),
            Bool(value) => write!(f, "{}", value),
            Str(value) => write!(f, "\"{}\"", value),
            Char(value) => write!(f, "'{}'", value),
            Identifier(name) => write!(f, "{}", name),

            Unary(op, node) => {
                use UnaryOp::*;
                match op {
                    Pos => write!(f, "(+{})", node),
                    Neg => write!(f, "(-{})", node),
                    Not => write!(f, "(not {})", node),
                }
            }
            Binary(left, op, right) => {
                use BinaryOp::*;
                match op {
                    Eq => write!(f, "({} = {})", left, right),
                    Add => write!(f, "({} + {})", left, right),
                    Sub => write!(f, "({} - {})", left, right),
                    Mul => write!(f, "({} * {})", left, right),
                    Div => write!(f, "({} / {})", left, right),
                    Pow => write!(f, "({} ^ {})", left, right),

                    And => write!(f, "({} and {})", left, right),
                    Or => write!(f, "({} or {})", left, right),
                    EqEq => write!(f, "({} == {})", left, right),
                    Neq => write!(f, "({} != {})", left, right),
                    Lt => write!(f, "({} < {})", left, right),
                    Lte => write!(f, "({} <= {})", left, right),
                    Gt => write!(f, "({} > {})", left, right),
                    Gte => write!(f, "({} >= {})", left, right),
                    In => write!(f, "({} in {})", left, right),
                }
            }

            Array(nodes) => {
                write!(
                    f,
                    "[{}]",
                    nodes
                        .iter()
                        .map(|node| format!("{}", node))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }

            If(condition, body, else_body) => match else_body {
                Some(else_body) => write!(f, "(if {}: {} else: {})", condition, body, else_body),
                _ => write!(f, "(if {}: {})", condition, body),
            },
            While(condition, body) => write!(f, "(while {}: {})", condition, body),
            For(identifier, iterable, body) => {
                write!(f, "(for {} in {}: {})", identifier, iterable, body)
            }
            Fn(name, args, body) => {
                write!(
                    f,
                    "(fn {} ({}) -> {})",
                    name,
                    args.iter()
                        .map(|node| format!("{}", node))
                        .collect::<Vec<_>>()
                        .join(", "),
                    body
                )
            }

            Return(node) => write!(f, "(return {})", node),
            Call(name, args) => write!(
                f,
                "{}({})",
                name,
                args.iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Index(name, index) => write!(f, "{}[{}]", name, index),

            Statements(nodes) => write!(
                f,
                "[\n  {}\n]",
                nodes
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join("\n  ")
            ),
            EOF => write!(f, "<eof>"),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Node<'a> {
    pub ty: NodeType<'a>,
    pub start: Position<'a>,
    pub end: Position<'a>,
}

impl<'a> Node<'a> {
    pub fn new(ty: NodeType<'a>, start: Position<'a>, end: Position<'a>) -> Self {
        Self { ty, start, end }
    }
}

impl fmt::Display for Node<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{} {}", self.start, self.end, self.ty)
    }
}
