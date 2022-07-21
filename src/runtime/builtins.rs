use crate::{Scope, Value};

pub fn add_builtins(scope: &mut Scope) {
    scope.set_fn("print", |args| {
        println!(
            "{}",
            args.iter()
                .map(|arg| arg.to_string())
                .collect::<Vec<String>>()
                .join(" ")
        );
        Ok(Value::None)
    });
}
