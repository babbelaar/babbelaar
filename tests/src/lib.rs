// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use babbelaar::{Expression, Lexer, ParseDiagnostic, Parser, Ranged, Statement, Token, Value};
use babbelaar_interpreter::Interpreter;

pub fn parse<'a>(input: &'a str) -> Vec<Statement<'a>> {
    let mut result = Vec::new();

    let tokens: Vec<Token> = Lexer::new(input).collect();
    let mut parser = Parser::new(&tokens);
    loop {
        match parser.parse_statement() {
            Ok(statement) => result.push(statement),
            Err(ParseDiagnostic::EndOfFile) => break,
            Err(e) => panic!("Failed to compile: {e}"),
        }
    }

    result
}

pub fn parse_expression<'a>(input: &'a str) -> Ranged<Expression<'a>> {
    let tokens: Vec<Token> = Lexer::new(input).collect();
    let mut parser = Parser::new(&tokens);

    let expr = parser.parse_expression().unwrap();
    assert!(parser.is_at_end());

    expr
}

pub fn interpret_statements(input: &str) {
    let mut interpreter = Interpreter::new(());
    for statement in parse(input) {
        interpreter.execute(&statement)
    }
}

pub fn interpret_expression(input: &str) -> Value {
    let expression = parse_expression(input);
    Interpreter::new(()).execute_expression(&expression)
}
