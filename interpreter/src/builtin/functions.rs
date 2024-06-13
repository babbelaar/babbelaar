// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::io::stdin;

use crate::{Interpreter, Value};

pub type BuiltinFunctionSignature = &'static dyn Fn(&mut Interpreter<'_>, Vec<Value>) -> Value;

pub struct BuiltinFunction {
    pub name: &'static str,
    pub documentation: &'static str,
    pub inline_detail: &'static str,
    pub function: BuiltinFunctionSignature,
    pub lsp_completion: Option<&'static str>,
    pub parameters: &'static [BuiltinFunctionParameter],
}

pub struct BuiltinFunctionParameter {
    pub name: &'static str,
}

pub fn schrijf(_: &mut Interpreter<'_>, args: Vec<Value>) -> Value {
    for (arg_idx, arg) in args.into_iter().enumerate() {
        if arg_idx != 0 {
            print!(" ");
        }

        print!("{arg}");
    }

    println!();

    Value::Null
}

pub fn lees(_: &mut Interpreter<'_>, _: Vec<Value>) -> Value {
    let mut line = String::new();
    stdin().read_line(&mut line).unwrap();
    line.truncate(line.trim_end().len());
    Value::String(line)
}
