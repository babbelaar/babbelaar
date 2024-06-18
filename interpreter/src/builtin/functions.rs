// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{borrow::Cow, fmt::{Debug, Display}, io::stdin};

use crate::{BuiltinType, Interpreter, Value};

pub type BuiltinFunctionSignature = &'static dyn Fn(&mut Interpreter<'_>, Vec<Value>) -> Value;

#[derive(Clone, Copy)]
pub struct BuiltinFunction {
    pub name: &'static str,
    pub documentation: &'static str,
    pub inline_detail: &'static str,
    pub function: BuiltinFunctionSignature,
    pub lsp_completion: Option<&'static str>,
    pub parameters: &'static [BuiltinFunctionParameter],
    pub return_type: &'static BuiltinType,
}

impl BuiltinFunction {
    pub fn lsp_completion(&self) -> Cow<'static, str> {
        match self.lsp_completion {
            Some(completion) => Cow::Borrowed(&completion),
            None => {
                let mut result = format!("{}(", self.name);

                for (idx, parameter) in self.parameters.iter().enumerate() {
                    result += &format!("${{{index}:{name}}}", index = idx + 1, name = parameter.name);
                }

                result += ")$0";
                Cow::Owned(result)
            }
        }
    }
}

impl Display for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}()", self.name))
    }
}

impl Debug for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltinFunction")
            .field("name", &self.name)
            .field("documentation", &self.documentation)
            .field("inline_detail", &self.inline_detail)
            .field("function", &"(native)")
            .field("lsp_completion", &self.lsp_completion)
            .field("parameters", &self.parameters)
            .finish()
    }
}

impl PartialEq for BuiltinFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(Debug)]
pub struct BuiltinFunctionParameter {
    pub name: &'static str,
    pub typ: &'static BuiltinType,
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

pub fn slinger_lengte(_: &mut Interpreter<'_>, parameter: Vec<Value>) -> Value {
    Value::Integer(parameter[0].to_string().len() as _)
}
