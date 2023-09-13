// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#![deny(elided_lifetimes_in_paths)]

mod expression;
mod interpreter;
mod keyword;
mod lexer;
mod parser;
mod statement;
mod token;
mod value;

pub use self::{
    expression::*,
    interpreter::Interpreter,
    keyword::Keyword,
    lexer::Lexer,
    parser::{Parser, ParseError},
    statement::{FunctionStatement, Statement},
    token::{Token, TokenKind},
    value::Value,
};

fn main() {
    let source_code = std::fs::read_to_string("test.bab").unwrap();

    println!("Lexing...");
    let lexer = Lexer::new(&source_code);
    let tokens: Vec<_> = lexer.collect();

    println!("Parsing...");
    let mut parser = Parser::new(&tokens);

    println!("Interpreting...");
    let mut interpreter = Interpreter::new();

    loop {
        let statement = parser.parse_statement();

        match statement {
            Ok(statement) => {
                interpreter.execute(&statement);
            }
            Err(ParseError::EndOfFile) => break,
            Err(e) => {
                println!("Error: {e}");
                break;
            }
        }
    }
}
