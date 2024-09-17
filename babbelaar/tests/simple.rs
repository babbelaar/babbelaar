// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::path::PathBuf;

use babbelaar::*;
use rstest::rstest;

#[rstest]
#[case("werkwijze hallo(afafg: Slinger) {
    als
}
")]
fn test(#[case] input: &str) {
    let input = SourceCode::new(PathBuf::new(), input.to_string());
    let tokens: Vec<Token> = Lexer::new(&input).collect();
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
