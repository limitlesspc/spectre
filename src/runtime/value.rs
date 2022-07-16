use std::fmt;

use crate::{Node, RuntimeResult};

#[derive(Clone, PartialEq)]
pub enum Value<'a> {
    Int(i32),
    Float(f64),
    Bool(bool),
    Str(String),
    Char(char),
    Array(Vec<Value<'a>>),
    Fn(String, Vec<String>, Box<Node<'a>>),
    None,
}

use Value::*;

impl<'a> Value<'a> {
    pub fn to_bool(&self) -> bool {
        match self.clone() {
            Int(value) => value != 0,
            Float(value) => value != 0.0,
            Bool(value) => value,
            Str(value) => !value.is_empty(),
            Char(value) => value != '\0',
            Array(_) => true,
            Fn(_, _, _) => true,
            None => false,
        }
    }

    pub fn neg(&self) -> RuntimeResult<'a> {
        match self.clone() {
            Int(value) => Ok(Int(-value)),
            Float(value) => Ok(Float(-value)),
            _ => unimplemented!(),
        }
    }

    pub fn not(&self) -> RuntimeResult<'a> {
        match self.clone() {
            Int(value) => Ok(Bool(value == 0)),
            Float(value) => Ok(Bool(value == 0.0)),
            Bool(value) => Ok(Bool(!value)),
            _ => unimplemented!(),
        }
    }

    pub fn add(&self, other: &Value<'a>) -> RuntimeResult<'a> {
        match self.clone() {
            Int(value) => match other.clone() {
                Int(other) => Ok(Int(value + other)),
                Float(other) => Ok(Float(value as f64 + other)),
                _ => unimplemented!(),
            },
            Float(value) => match other.clone() {
                Int(other) => Ok(Float(value + other as f64)),
                Float(other) => Ok(Float(value + other)),
                _ => unimplemented!(),
            },
            Str(value) => match other.clone() {
                Int(other) => Ok(Str(format!("{}{}", value, other))),
                Float(other) => Ok(Str(format!("{}{}", value, other))),
                Str(other) => Ok(Str(format!("{}{}", value, other))),
                _ => unimplemented!(),
            },
            Array(value) => match other.clone() {
                Int(other) => Ok(Array(
                    value.clone().into_iter().chain(vec![Int(other)]).collect(),
                )),
                Float(other) => Ok(Array(
                    value
                        .clone()
                        .into_iter()
                        .chain(vec![Float(other)])
                        .collect(),
                )),
                Bool(other) => Ok(Array(
                    value.clone().into_iter().chain(vec![Bool(other)]).collect(),
                )),
                Str(other) => Ok(Array(
                    value.clone().into_iter().chain(vec![Str(other)]).collect(),
                )),
                Char(other) => Ok(Array(
                    value.clone().into_iter().chain(vec![Char(other)]).collect(),
                )),
                Array(other) => Ok(Array(value.clone().into_iter().chain(other).collect())),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    pub fn sub(&self, other: &Value<'a>) -> RuntimeResult<'a> {
        match self.clone() {
            Int(value) => match other.clone() {
                Int(other) => Ok(Int(value - other)),
                Float(other) => Ok(Float(value as f64 - other)),
                _ => unimplemented!(),
            },
            Float(value) => match other.clone() {
                Int(other) => Ok(Float(value - other as f64)),
                Float(other) => Ok(Float(value - other)),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    pub fn mul(&self, other: &Value<'a>) -> RuntimeResult<'a> {
        match self.clone() {
            Int(value) => match other.clone() {
                Int(other) => Ok(Int(value * other)),
                Float(other) => Ok(Float(value as f64 * other)),
                Str(other) => Ok(Str(other.repeat(value as usize))),
                Char(other) => Ok(Str(other.to_string().repeat(value as usize))),
                _ => unimplemented!(),
            },
            Float(value) => match other.clone() {
                Int(other) => Ok(Float(value * other as f64)),
                Float(other) => Ok(Float(value * other)),
                _ => unimplemented!(),
            },
            Str(value) => match other.clone() {
                Int(other) => Ok(Str(value.repeat(other as usize))),
                _ => unimplemented!(),
            },
            Char(value) => match other.clone() {
                Int(other) => Ok(Str(value.to_string().repeat(other as usize))),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    pub fn div(&self, other: &Value<'a>) -> RuntimeResult<'a> {
        match self.clone() {
            Int(value) => match other.clone() {
                Int(other) => Ok(Int(value / other)),
                Float(other) => Ok(Float(value as f64 / other)),
                _ => unimplemented!(),
            },
            Float(value) => match other.clone() {
                Int(other) => Ok(Float(value / other as f64)),
                Float(other) => Ok(Float(value / other)),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    pub fn pow(&self, other: &Value<'a>) -> RuntimeResult<'a> {
        match self.clone() {
            Int(value) => match other.clone() {
                Int(other) => Ok(Int(value.pow(other as u32))),
                Float(other) => Ok(Float((value as f64).powf(other))),
                _ => unimplemented!(),
            },
            Float(value) => match other.clone() {
                Int(other) => Ok(Float(value.powf(other as f64))),
                Float(other) => Ok(Float(value.powf(other))),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    pub fn and(&self, other: &Value<'a>) -> RuntimeResult<'a> {
        Ok(Bool(self.to_bool() && other.to_bool()))
    }

    pub fn or(&self, other: &Value<'a>) -> RuntimeResult<'a> {
        Ok(Bool(self.to_bool() || other.to_bool()))
    }

    pub fn eqeq(&self, other: &Value<'a>) -> RuntimeResult<'a> {
        match self.clone() {
            Int(value) => match other.clone() {
                Int(other) => Ok(Bool(value == other)),
                Float(other) => Ok(Bool(value as f64 == other)),
                _ => Ok(Bool(false)),
            },
            Float(value) => match other.clone() {
                Int(other) => Ok(Bool(value == other as f64)),
                Float(other) => Ok(Bool(value == other)),
                _ => Ok(Bool(false)),
            },
            Bool(value) => match other.clone() {
                Bool(other) => Ok(Bool(value == other)),
                _ => Ok(Bool(false)),
            },
            Str(value) => match other.clone() {
                Str(other) => Ok(Bool(value == other)),
                _ => Ok(Bool(false)),
            },
            Char(value) => match other.clone() {
                Char(other) => Ok(Bool(value == other)),
                _ => Ok(Bool(false)),
            },
            _ => unimplemented!(),
        }
    }

    pub fn neq(&self, other: &Value<'a>) -> RuntimeResult<'a> {
        match self.clone() {
            Int(value) => match other.clone() {
                Int(other) => Ok(Bool(value != other)),
                Float(other) => Ok(Bool(value as f64 != other)),
                _ => Ok(Bool(true)),
            },
            Float(value) => match other.clone() {
                Int(other) => Ok(Bool(value != other as f64)),
                Float(other) => Ok(Bool(value != other)),
                _ => Ok(Bool(true)),
            },
            Bool(value) => match other.clone() {
                Bool(other) => Ok(Bool(value != other)),
                _ => Ok(Bool(true)),
            },
            Str(value) => match other {
                Str(other) => Ok(Bool(value.eq(other))),
                _ => Ok(Bool(true)),
            },
            Char(value) => match other.clone() {
                Char(other) => Ok(Bool(value != other)),
                _ => Ok(Bool(true)),
            },
            _ => unimplemented!(),
        }
    }

    pub fn lt(&self, other: &Value<'a>) -> RuntimeResult<'a> {
        match self.clone() {
            Int(value) => match other.clone() {
                Int(other) => Ok(Bool(value < other)),
                Float(other) => Ok(Bool((value as f64) < other)),
                _ => unimplemented!(),
            },
            Float(value) => match other.clone() {
                Int(other) => Ok(Bool(value < other as f64)),
                Float(other) => Ok(Bool(value < other)),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    pub fn lte(&self, other: &Value<'a>) -> RuntimeResult<'a> {
        match self.clone() {
            Int(value) => match other.clone() {
                Int(other) => Ok(Bool(value <= other)),
                Float(other) => Ok(Bool((value as f64) <= other)),
                _ => unimplemented!(),
            },
            Float(value) => match other.clone() {
                Int(other) => Ok(Bool(value <= other as f64)),
                Float(other) => Ok(Bool(value <= other)),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    pub fn gt(&self, other: &Value<'a>) -> RuntimeResult<'a> {
        match self.clone() {
            Int(value) => match other.clone() {
                Int(other) => Ok(Bool(value > other)),
                Float(other) => Ok(Bool((value as f64) > other)),
                _ => unimplemented!(),
            },
            Float(value) => match other.clone() {
                Int(other) => Ok(Bool(value > other as f64)),
                Float(other) => Ok(Bool(value > other)),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    pub fn gte(&self, other: &Value<'a>) -> RuntimeResult<'a> {
        match self.clone() {
            Int(value) => match other.clone() {
                Int(other) => Ok(Bool(value >= other)),
                Float(other) => Ok(Bool((value as f64) >= other)),
                _ => unimplemented!(),
            },
            Float(value) => match other.clone() {
                Int(other) => Ok(Bool(value >= other as f64)),
                Float(other) => Ok(Bool(value >= other)),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    pub fn r#in(&self, other: &Value<'a>) -> RuntimeResult<'a> {
        match other {
            Array(other) => {
                for value in other {
                    if self.eqeq(value)?.to_bool() {
                        return Ok(Bool(true));
                    }
                }
                Ok(Bool(false))
            }
            _ => unimplemented!(),
        }
    }
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Int(value) => write!(f, "{}", value),
            Float(value) => write!(f, "{}", value),
            Bool(value) => write!(f, "{}", value),
            Str(value) => write!(f, "\"{}\"", value),
            Char(value) => write!(f, "'{}'", value),
            Array(values) => write!(
                f,
                "{}",
                values
                    .iter()
                    .map(|value| format!("{}", value))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Fn(name, _, _) => write!(f, "<fn {}>", name),
            None => write!(f, "none"),
        }
    }
}
