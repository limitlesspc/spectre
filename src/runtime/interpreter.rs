use crate::{BinaryOp, IdentifierOp, Node, NodeType, Scope, UnaryOp, Value};
use NodeType::*;

pub struct Interpreter<'a> {
    ast: Node<'a>,
    pub scope: Scope<'a>,
    return_value: Option<Value<'a>>,
}

pub type RuntimeResult<'a> = Result<Value<'a>, String>;

impl<'a> Interpreter<'a> {
    pub fn new(ast: Node<'a>) -> Self {
        Self {
            ast,
            scope: Scope::new(None),
            return_value: None,
        }
    }

    pub fn run(&mut self) -> RuntimeResult<'a> {
        self.visit(self.ast.clone())
    }

    fn visit(&mut self, node: Node<'a>) -> RuntimeResult<'a> {
        match node.ty {
            Int(value) => Ok(Value::Int(value as i32)),
            Float(value) => Ok(Value::Float(value)),
            Bool(value) => Ok(Value::Bool(value)),
            Str(value) => Ok(Value::Str(value)),
            Char(value) => Ok(Value::Char(value)),
            Array(values) => {
                let mut array = Vec::new();
                for value in values {
                    array.push(self.visit(value)?);
                }
                Ok(Value::Array(array))
            }

            Identifier(name) => self.scope.get(&name),

            Unary(op, node) => {
                let value = self.visit(*node)?;
                match op {
                    UnaryOp::Neg => Ok(value.neg()?),
                    UnaryOp::Not => Ok(value.not()?),
                }
            }
            Binary(left, op, right) => {
                let left = &self.visit(*left)?;
                let right = &self.visit(*right)?;
                match op {
                    BinaryOp::Add => Ok(left.add(right)?),
                    BinaryOp::Sub => Ok(left.sub(right)?),
                    BinaryOp::Mul => Ok(left.mul(right)?),
                    BinaryOp::Div => Ok(left.div(right)?),
                    BinaryOp::Pow => Ok(left.pow(right)?),

                    BinaryOp::And => Ok(left.and(right)?),
                    BinaryOp::Or => Ok(left.or(right)?),
                    BinaryOp::EqEq => Ok(left.eqeq(right)?),
                    BinaryOp::Neq => Ok(left.neq(right)?),
                    BinaryOp::Lt => Ok(left.lt(right)?),
                    BinaryOp::Lte => Ok(left.lte(right)?),
                    BinaryOp::Gt => Ok(left.gt(right)?),
                    BinaryOp::Gte => Ok(left.gte(right)?),
                    BinaryOp::In => Ok(left.r#in(right)?),
                }
            }
            IdentifierBinary(name, op, value) => {
                let right = &self.visit(*value)?;
                match name.clone().ty {
                    NodeType::Identifier(name_str) => match op {
                        IdentifierOp::Eq => self.scope.set(name_str, right.clone()),
                        IdentifierOp::Add => {
                            let left = &self.visit(*name.clone())?;
                            self.scope.set(name_str, left.add(right)?)
                        }
                        IdentifierOp::Sub => {
                            let left = &self.visit(*name.clone())?;
                            self.scope.set(name_str, left.sub(right)?)
                        }
                        IdentifierOp::Mul => {
                            let left = &self.visit(*name.clone())?;
                            self.scope.set(name_str, left.mul(right)?)
                        }
                        IdentifierOp::Div => {
                            let left = &self.visit(*name.clone())?;
                            self.scope.set(name_str, left.div(right)?)
                        }
                        IdentifierOp::Pow => {
                            let left = &self.visit(*name.clone())?;
                            self.scope.set(name_str, left.pow(right)?)
                        }
                    },
                    _ => Err(format!("Invalid identifier: {}", name)),
                }
            }

            If(condition, body, else_body) => {
                let condition = self.visit(*condition)?;
                if condition.to_bool() {
                    self.visit(*body)
                } else {
                    match else_body {
                        Some(else_body) => self.visit(*else_body),
                        None => Ok(Value::None),
                    }
                }
            }
            While(condition, body) => {
                loop {
                    let condition = self.visit(*condition.clone())?;
                    if !condition.to_bool() {
                        break;
                    }
                    self.visit(*body.clone())?;
                }
                Ok(Value::None)
            }
            For(name, iterable, body) => {
                let iterable = self.visit(*iterable)?;
                match iterable {
                    Value::Array(values) => {
                        for value in values {
                            self.scope.set(name.to_string(), value)?;
                            self.visit(*body.clone())?;
                        }
                        Ok(Value::None)
                    }
                    _ => Err("Invalid iterable: {}".to_string()),
                }
            }
            Fn(name, args, body) => match name.ty {
                Identifier(name) => {
                    let mut arg_names: Vec<String> = vec![];
                    for arg in args {
                        match arg.ty {
                            Identifier(name) => arg_names.push(name),
                            _ => return Err("Invalid argument name".to_string()),
                        };
                    }
                    Ok(Value::Fn(name, arg_names, body))
                }
                _ => unimplemented!(),
            },

            Return(node) => {
                self.return_value = Some(self.visit(*node)?);
                Ok(Value::None)
            }
            Call(node, args) => {
                let value = self.visit(*node)?;
                let mut arg_values: Vec<Value> = vec![];
                for arg in args {
                    arg_values.push(self.visit(arg)?);
                }
                match value {
                    Value::Int(_) => value.mul(&arg_values[0]),
                    Value::Float(_) => value.mul(&arg_values[0]),
                    Value::Fn(_, arg_names, body) => {
                        for (arg_name, arg_value) in arg_names.iter().zip(arg_values.iter()) {
                            self.scope.set(arg_name.to_string(), arg_value.clone())?;
                        }
                        self.visit(*body)
                    }
                    _ => Err("Invalid call".to_string()),
                }
            }
            Index(node, index) => {
                let value = self.visit(*node)?;
                let index = self.visit(*index)?;
                match value {
                    Value::Array(values) => match index {
                        Value::Int(index) => Ok({
                            if index < 0 {
                                Value::None
                            } else {
                                values[index as usize].clone()
                            }
                        }),
                        _ => Err("Invalid index".to_string()),
                    },
                    _ => Err("Invalid index".to_string()),
                }
            }

            Statements(nodes) => {
                for node in nodes {
                    self.visit(node)?;
                }
                Ok(Value::None)
            }

            EOF => Ok(Value::None),
        }
    }
}
