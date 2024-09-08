// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{borrow::Cow, fmt::{Debug, Display}, io::stdin};

use crate::{BuiltinType, Interpreter, Value};

pub type BuiltinFunctionSignature = &'static (dyn Fn(&mut dyn Interpreter, Vec<Value>, Option<Value>) -> Value + Send + Sync);

#[derive(Clone, Copy)]
pub struct BuiltinFunction {
    pub name: &'static str,
    pub documentation: &'static str,
    pub inline_detail: &'static str,
    pub function: BuiltinFunctionSignature,
    pub lsp_completion: Option<&'static str>,
    pub parameters: &'static [BuiltinFunctionParameter],
    pub return_type: BuiltinType,
    pub must_use: bool,
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

    #[must_use]
    pub fn lsp_label(&self) -> String {
        let params = if self.parameters.is_empty() { "" } else { "…" };
        format!("{}({params})", self.name)
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
    pub typ: BuiltinType,
}

pub fn schrijf(_: &mut dyn Interpreter, args: Vec<Value>, _this: Option<Value>) -> Value {
    for (arg_idx, arg) in args.into_iter().enumerate() {
        if arg_idx != 0 {
            print!(" ");
        }

        print!("{arg}");
    }

    println!();

    Value::Null
}

pub fn lees(_: &mut dyn Interpreter, _: Vec<Value>, _: Option<Value>) -> Value {
    let mut line = String::new();
    stdin().read_line(&mut line).unwrap();
    line.truncate(line.trim_end().len());
    Value::String(line)
}
