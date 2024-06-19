// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#![deny(elided_lifetimes_in_paths)]

mod interpreter;
mod scope;

pub use babbelaar::*;

pub use self::{
    interpreter::Interpreter,
    scope::Scope,
};

fn main() {
    let source_code = std::fs::read_to_string("test.bab").unwrap();

    println!("Lexeren...");
    let lexer = Lexer::new(&source_code);
    let tokens: Vec<_> = lexer.collect();

    println!("Ontleden...");
    let mut parser = Parser::new(&tokens);

    println!("Aan het interpreteren...");
    let mut interpreter = Interpreter::new();

    loop {
        let statement = parser.parse_statement();

        match statement {
            Ok(statement) => {
                interpreter.execute(&statement);
            }
            Err(ParseError::EndOfFile) => break,
            Err(e) => {
                println!("Fout: {e}");
                break;
            }
        }
    }
}
