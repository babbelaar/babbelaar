// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::path::PathBuf;

use babbelaar::*;
use rstest::rstest;

#[rstest]
#[case("functie hallo(afafg: Slinger) {
    als
}
")]
fn test(#[case] input: &str) {
    let tokens: Vec<Token<'_>> = Lexer::new(input).collect();
    let mut parser = Parser::new(PathBuf::new(), &tokens).attempt_to_ignore_errors();

    loop {
        match parser.parse_statement() {
            Ok(stmt) => println!("Stmt: {stmt:#?}"),
            Err(ParseDiagnostic::EndOfFile) => break,
            Err(e) => panic!("Unexpected error: {e}"),
        }
    }

    assert!(parser.errors.is_empty());
}
