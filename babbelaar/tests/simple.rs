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
    let input = SourceCode::new(PathBuf::new(), 0, input.to_string());
    let tokens: Vec<Token> = Lexer::new(&input).collect();
    let mut parser = Parser::new(PathBuf::new(), &tokens);

    loop {
        match parser.parse_statement() {
            Ok(stmt) => println!("Stmt: {stmt:#?}"),
            Err(ParseError::EndOfFile) => break,
        }
    }

    assert!(parser.diagnostics().is_empty());
}
