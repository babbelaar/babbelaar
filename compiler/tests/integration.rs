// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use babbelaar::{parse_string_to_tree, BabString};
use babbelaar_compiler::{Compiler, Interpreter};

#[test]
fn function_that_returns_two() {
    let tree = parse_string_to_tree("
        werkwijze twee() -> g32 {
            bekeer 2;
        }
    ").unwrap();

    let mut compiler = Compiler::new();
    compiler.compile_trees(&[tree]);

    let program = compiler.finish();

    println!("{program}");

    let mut interpreter = Interpreter::new(program);
    let value = interpreter.execute_function(&BabString::new_static("twee"), Vec::new());

    debug_assert_eq!(value.map(|x| x.as_i64()), Some(2));
}
