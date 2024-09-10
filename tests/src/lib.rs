// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{path::PathBuf, sync::{Arc, Mutex}};

use babbelaar::{Expression, Lexer, ParseTree, Parser, Ranged, SemanticAnalyzer, SemanticDiagnosticSeverity, Token, Value};
use babbelaar_interpreter::{Debugger, Interpreter};

fn parse<'a>(input: &'a str) -> ParseTree<'a> {
    let tokens: Vec<Token> = Lexer::new(input).collect();
    let mut parser = Parser::new(PathBuf::new(), &tokens);
    let tree = parser.parse_tree().unwrap();

    let mut semantics = SemanticAnalyzer::new();
    semantics.analyze_tree(&tree);
    let diagnostics = semantics.into_diagnostics();
    assert!(diagnostics.iter().find(|x| x.severity() == SemanticDiagnosticSeverity::Error).is_none(), "Diagnostics: {diagnostics:#?}");

    tree
}

pub fn parse_expression<'a>(input: &'a str) -> Ranged<Expression<'a>> {
    let tokens: Vec<Token> = Lexer::new(input).collect();
    let mut parser = Parser::new(PathBuf::new(), &tokens);

    let expr = parser.parse_expression().unwrap();
    assert!(parser.is_at_end());

    expr
}

pub fn interpret_statements(input: &str) {
    let mut interpreter = Interpreter::new(());
    for statement in parse(input).all() {
        interpreter.execute(&statement)
    }
}

pub fn interpret_expression(input: &str) -> Value {
    let expression = parse_expression(input);
    Interpreter::new(()).execute_expression(&expression)
}

pub fn interpret_and_return_stdout(input: &str) -> Vec<String> {
    let buffer = Arc::new(Mutex::new(Vec::new()));

    {
        let mut interpreter = Interpreter::new(TestDebugger {
            buffer: Arc::clone(&buffer),
        });

        for statement in parse(input).all() {
            interpreter.execute(&statement)
        }
    }


    Arc::try_unwrap(buffer).unwrap().into_inner().unwrap()
}

struct TestDebugger {
    buffer: Arc<Mutex<Vec<String>>>,
}

impl Debugger for TestDebugger {
    fn enter_function(&mut self, function: babbelaar_interpreter::DebuggerFunction<'_>, args: &[Value]) {
        if function.name == "schrijf" {
            self.buffer.lock().unwrap().push(args[0].to_string());
        }
    }
}
