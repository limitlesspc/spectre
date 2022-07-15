use std::fmt;

use crate::Position;

#[derive(Clone, Copy, PartialEq)]
pub enum TokenType<'a> {
    Int(u32),
    Float(f64),
    Bool(bool),
    Str(&'a str),
    Char(char),
    Identifier(&'a str),

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
    Whitespace,
    EOF,
}

impl<'a> fmt::Display for TokenType<'a> {
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
            Whitespace => write!(f, "' '"),
            EOF => write!(f, "<eof>"),
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct Token<'a> {
    pub ty: TokenType<'a>,
    pub start: Position<'a>,
    pub end: Position<'a>,
}

pub static EOF_TOKEN: Token = Token::new(TokenType::EOF, Position::new(""), Position::new(""));

impl<'a> Token<'a> {
    pub fn new(ty: TokenType<'a>, start: Position<'a>, end: Position<'a>) -> Self {
        Self { ty, start, end }
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{} {}", self.start, self.end, self.ty)
    }
}
