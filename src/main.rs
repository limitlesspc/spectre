use std::fs;
use std::time::Instant;

mod lexer;
mod node;
mod parser;
mod position;
mod runtime;
mod token;

pub use lexer::*;
pub use node::*;
pub use parser::*;
pub use position::*;
pub use runtime::Interpreter;
pub use runtime::*;
pub use token::*;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    match args.len() {
        1 => panic!("Too few many arguments passed"),
        2 => run_file(&args[1], false),
        3 => match args[2].as_str() {
            "--log" => run_file(&args[1], true),
            _ => panic!("Too many arguments passed"),
        },
        _ => panic!("Too many arguments passed"),
    };
}

fn run_file(path: &str, log: bool) {
    let source = fs::read_to_string(path).unwrap();
    run(&source, log);
}

fn run(source: &str, log: bool) {
    let begin = Instant::now();

    let mut lexer = Lexer::new(source);
    let tokens = match lexer.lex() {
        Ok(tokens) => tokens,
        Err(error) => panic!("Lexer error: {}", error),
    };
    if log {
        println!("tokens:");
        for token in tokens.iter() {
            println!("{}", token);
        }
        println!();
    }

    let mut parser = Parser::new(tokens);
    let ast = match parser.parse() {
        Ok(node) => node,
        Err(error) => panic!("Parser error: {}", error),
    };
    if log {
        println!("ast:");
        println!("{}\n", ast);
    }

    let mut interpreter = Interpreter::new(ast);
    add_builtins(&mut interpreter.scope);
    match interpreter.run() {
        Ok(_) => {}
        Err(error) => panic!("Interpreter error: {}", error),
    };

    let end = Instant::now();
    println!("Ran in {:.3}s", end.duration_since(begin).as_secs_f64());
}
