use std::fmt;

use crate::Position;

#[derive(Clone, PartialEq)]
pub enum TokenType {
    Int(u32),
    Float(f64),
    Bool(bool),
    Str(String),
    Char(char),
    Identifier(String),

    Eq,
    Add,
    AddEq,
    Sub,
    SubEq,
    Mul,
    MulEq,
    Div,
    DivEq,
    Pow,
    PowEq,

    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,

    Colon,

    Not,
    And,
    Or,
    EqEq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    In,

    If,
    Else,
    While,
    For,
    Return,

    Newline,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenType::*;
        match self {
            Int(value) => write!(f, "{}", value),
            Float(value) => write!(f, "{}f", value),
            Bool(value) => write!(f, "{}", value),
            Str(value) => write!(f, "\"{}\"", value),
            Char(value) => write!(f, "'{}'", value),
            Identifier(name) => write!(f, "{}", name),

            Eq => write!(f, "'='"),
            Add => write!(f, "'+'"),
            AddEq => write!(f, "'+='"),
            Sub => write!(f, "'-'"),
            SubEq => write!(f, "'-='"),
            Mul => write!(f, "'*'"),
            MulEq => write!(f, "'*='"),
            Div => write!(f, "'/'"),
            DivEq => write!(f, "'/='"),
            Pow => write!(f, "'^'"),
            PowEq => write!(f, "'^='"),

            LParen => write!(f, "'('"),
            RParen => write!(f, "')'"),
            LBracket => write!(f, "'['"),
            RBracket => write!(f, "']'"),
            LBrace => write!(f, "'{{'"),
            RBrace => write!(f, "'}}'"),

            Colon => write!(f, "':'"),

            Not => write!(f, "'not'"),
            And => write!(f, "'and'"),
            Or => write!(f, "'or'"),
            EqEq => write!(f, "'=='"),
            Neq => write!(f, "'!='"),
            Lt => write!(f, "'<'"),
            Lte => write!(f, "'<='"),
            Gt => write!(f, "'>'"),
            Gte => write!(f, "'>='"),
            In => write!(f, "'in'"),

            If => write!(f, "'if'"),
            Else => write!(f, "'else'"),
            While => write!(f, "'while'"),
            For => write!(f, "'for'"),
            Return => write!(f, "'return'"),

            Newline => write!(f, "'\\n'"),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Token<'a> {
    pub ty: TokenType,
    pub start: Position<'a>,
    pub end: Position<'a>,
}

impl<'a> Token<'a> {
    pub fn new(ty: TokenType, start: Position<'a>, end: Position<'a>) -> Self {
        Self { ty, start, end }
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{} {}", self.start, self.end, self.ty)
    }
}
