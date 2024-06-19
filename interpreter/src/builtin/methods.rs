// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{BuiltinFunction, BuiltinType, Interpreter, Value};

use super::BuiltinFunctionParameter;

pub(super) static METHODS_BOOL: &'static [BuiltinFunction] = &[];
pub(super) static METHODS_G32: &'static [BuiltinFunction] = &[];
pub(super) static METHODS_NULL: &'static [BuiltinFunction] = &[];

pub(super) static METHODS_SLINGER: &'static [BuiltinFunction] = &[
    BuiltinFunction {
        name: "bevat",
        documentation: "Krijg de lengte van de slinger.\n## Voorbeeld\n```bab\n\"Hallo\".lengte() // = 5\n```",
        inline_detail: "",
        function: &slinger_bevat,
        lsp_completion: None,
        parameters: &[
            BuiltinFunctionParameter {
                name: "naald",
                typ: BuiltinType::Slinger,
            },
        ],
        return_type: BuiltinType::Bool,
    },
    BuiltinFunction {
        name: "lengte",
        documentation: "Krijg de lengte van de slinger.\n## Voorbeeld\n```bab\n\"Hallo\".lengte() // = 5\n```",
        inline_detail: "",
        function: &slinger_lengte,
        lsp_completion: None,
        parameters: &[],
        return_type: BuiltinType::G32,
    }
];

pub fn slinger_lengte(_: &mut Interpreter<'_>, parameter: Vec<Value>) -> Value {
    Value::Integer(parameter[0].to_string().len() as _)
}

pub fn slinger_bevat(_: &mut Interpreter<'_>, parameter: Vec<Value>) -> Value {
    Value::Bool(parameter[0].to_string().contains(&parameter[1].to_string()))
}
