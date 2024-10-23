// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use babbelaar::{parse_string_to_tree, BabString};
use babbelaar_compiler::{Compiler, Immediate, Interpreter};

fn compile_and_interpret(code: &str, function: &'static str) -> Option<Immediate> {
    let tree = parse_string_to_tree(code).unwrap();

    let mut compiler = Compiler::new();
    compiler.compile_trees(&[tree]);

    let program = compiler.finish();

    println!("{program}");

    let mut interpreter = Interpreter::new(program);
    interpreter.execute_function(&BabString::new_static(function), Vec::new())
}

#[test]
fn function_that_returns_two() {
    let value = compile_and_interpret("
    werkwijze twee() -> g32 {
        bekeer 2;
    }
    ", "twee");

    debug_assert_eq!(value.map(|x| x.as_i64()), Some(2));
}

#[test]
fn function_that_adds_two_and_three() {
    let value = compile_and_interpret("
    werkwijze twee_plus_drie() -> g32 {
        bekeer 2 + 3;
    }
    ", "twee_plus_drie");

    debug_assert_eq!(value.map(|x| x.as_i64()), Some(5));
}
