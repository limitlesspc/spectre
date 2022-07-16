use std::fs;
use std::time::Instant;

mod lexer;
// mod node;
// mod parser;
mod position;
mod token;

pub use lexer::Lexer;
// pub use node::*;
// pub use parser::Parser;
pub use position::Position;
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
    let tokens = lexer.lex();
    match tokens {
        Ok(tokens) => {
            if log {
                println!("tokens:");
                for token in &tokens {
                    print!(" {}", token);
                }
                print!("\n");
            }

            // let mut parser = Parser::new(tokens);
            // let ast = parser.parse();
            // match ast {
            //     Ok(ast) => {
            //         println!("{}", ast);
            //     }
            //     Err(error) => println!("{}", error),
            // }
        }
        Err(error) => println!("{}", error),
    };

    let end = Instant::now();
    println!("Ran in {:.3}s", end.duration_since(begin).as_secs_f64());
}
