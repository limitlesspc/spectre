use std::fmt;

use crate::Position;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
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
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum IdentifierOp {
    Eq,
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

#[derive(Clone, PartialEq)]
pub enum NodeType<'a> {
    Int(u32),
    Float(f64),
    Bool(bool),
    Str(String),
    Char(char),
    Array(Vec<Node<'a>>),

    Identifier(String),

    Unary(UnaryOp, Box<Node<'a>>),
    Binary(Box<Node<'a>>, BinaryOp, Box<Node<'a>>),
    IdentifierBinary(Box<Node<'a>>, IdentifierOp, Box<Node<'a>>),

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

            Identifier(name) => write!(f, "{}", name),

            Unary(op, node) => {
                use UnaryOp::*;
                match op {
                    Neg => write!(f, "(-{})", node),
                    Not => write!(f, "(not {})", node),
                }
            }
            Binary(left, op, right) => {
                use BinaryOp::*;
                match op {
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
            IdentifierBinary(name, op, value) => {
                use IdentifierOp::*;
                match op {
                    Eq => write!(f, "({} = {})", name, value),
                    Add => write!(f, "({} += {})", name, value),
                    Sub => write!(f, "({} -= {})", name, value),
                    Mul => write!(f, "({} *= {})", name, value),
                    Div => write!(f, "({} /= {})", name, value),
                    Pow => write!(f, "({} ^= {})", name, value),
                }
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
            Call(node, args) => write!(
                f,
                "{}({})",
                node,
                args.iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Index(node, index) => write!(f, "{}[{}]", node, index),

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
        write!(f, "{}", self.ty)
    }
}
