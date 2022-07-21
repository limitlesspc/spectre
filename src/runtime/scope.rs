use std::collections::HashMap;

use crate::{RuntimeResult, Value};

pub struct Scope<'a> {
    pub variables: HashMap<String, Value<'a>>,
    parent: Option<&'a Scope<'a>>,
}

impl<'a> Scope<'a> {
    pub fn new(parent: Option<&'a Scope<'a>>) -> Self {
        Self {
            variables: HashMap::new(),
            parent,
        }
    }

    pub fn get(&self, name: &str) -> RuntimeResult<'a> {
        match self.variables.get(name) {
            Some(value) => Ok(value.clone()),
            None => match self.parent {
                Some(parent) => parent.get(name),
                None => Err(format!("Variable not defined: {}", name)),
            },
        }
    }

    pub fn set(&mut self, name: String, value: Value<'a>) -> RuntimeResult<'a> {
        self.variables.insert(name, value.clone());
        Ok(value)
    }

    pub fn set_fn(&mut self, name: &str, func: fn(Vec<Value<'a>>) -> RuntimeResult<'a>) {
        let name = name.to_string();
        let value = Value::BuiltinFn(name.clone(), func);
        self.variables.insert(name, value.clone());
    }
}
