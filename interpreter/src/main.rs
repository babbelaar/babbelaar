// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#![deny(elided_lifetimes_in_paths)]

mod builtin;
mod expression;
mod interpreter;
mod keyword;
mod lexer;
mod parser;
mod scope;
mod semantics;
mod statement;
mod token;
mod type_;
mod util;
mod value;

pub use self::{
    builtin::{Builtin, BuiltinFunction, BuiltinType},
    expression::*,
    interpreter::Interpreter,
    keyword::Keyword,
    lexer::Lexer,
    parser::{Parser, ParseError},
    scope::Scope,
    semantics::{SemanticAnalyzer, SemanticDiagnostic, SemanticDiagnosticKind, SemanticLocalKind, SemanticReference, SemanticType},
    statement::{ForStatement, FunctionStatement, IfStatement, ReturnStatement, Statement, StatementKind, VariableStatement},
    token::{Punctuator, TemplateStringToken, Token, TokenKind},
    type_::{Parameter, Type, TypeSpecifier},
    util::{DocumentationProvider, FileLocation, FileRange, LspCompletion, Ranged, StringExt},
    value::{FunctionId, Value},
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
