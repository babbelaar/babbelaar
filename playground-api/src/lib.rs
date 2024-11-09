// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use babbelaar::{parse_string_to_tree, FileRange, ParseDiagnostic};
use babbelaar_compiler::Compiler;
use wasm_bindgen::prelude::*;


#[wasm_bindgen]
pub fn compile_to_ir(code: &str) -> IrCompileResult {
    let mut result = IrCompileResult::default();

    let tree = match parse_string_to_tree(code) {
        Ok(tree) => tree,
        Err(e) => {
            let mut error = CompileError {
                text: e.to_string(),
                range: None,
            };

            if let Ok(err) = e.downcast::<ParseDiagnostic>() {
                error.range = Some(err.range().into());
            }

            result.errors.push(error);
            return result;
        }
    };

    let mut compiler = Compiler::new();
    compiler.compile_trees(&[tree]);

    result.program = Some(compiler.finish().to_string());

    result
}

#[wasm_bindgen(getter_with_clone)]
#[derive(Debug, Default)]
pub struct IrCompileResult {
    pub errors: Vec<CompileError>,
    pub program: Option<String>,
}

#[wasm_bindgen(getter_with_clone)]
#[derive(Debug, Clone)]
pub struct CompileError {
    pub text: String,
    pub range: Option<WasmRange>,
}

#[wasm_bindgen]
#[derive(Debug, Clone)]
pub struct WasmRange {
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

impl From<FileRange> for WasmRange {
    fn from(value: FileRange) -> Self {
        Self {
            start_line: value.start().line(),
            start_column: value.start().column(),
            end_line: value.end().line(),
            end_column: value.end().column(),
        }
    }
}
