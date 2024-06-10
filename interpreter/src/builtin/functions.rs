// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{Interpreter, Value};

pub type BuiltinFunctionSignature = &'static dyn Fn(&mut Interpreter<'_>, Vec<Value>) -> Value;

pub struct BuiltinFunction {
    pub name: &'static str,
    pub documentation: &'static str,
    pub function: BuiltinFunctionSignature,
}

pub fn schrijf(_: &mut Interpreter<'_>, args: Vec<Value>) -> Value {
    for (arg_idx, arg) in args.into_iter().enumerate() {
        if arg_idx != 0 {
            print!(" ");
        }

        match arg {
            Value::Null => print!("null"),
            Value::Integer(integer) => print!("{integer}"),
            Value::String(str) => print!("{str}"),
        }
    }

    println!();

    Value::Null
}
