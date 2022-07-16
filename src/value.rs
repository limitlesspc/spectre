use std::fmt;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Value<'a> {
    Int(i32),
    Float(f64),
    Bool(bool),
    Str(&'a str),
    Char(char),
    Array(Vec<Value<'a>>),
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;
        match self {
            Int(value) => write!(f, "{}", value),
            Float(value) => write!(f, "{}f", value),
            Bool(value) => write!(f, "{}", value),
            Str(value) => write!(f, "\"{}\"", value),
            Char(value) => write!(f, "'{}'", value),
            Array(values) => write!(f, "[{}]", values),
        }
    }
}
