// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Debug;

use crate::{BuiltinFunction, BuiltinType, Interpreter, Value};

use super::{functions::BuiltinFunctionSignature, PointerMethod, ArrayMethod, BuiltinFunctionParameter};

#[derive(Clone, Copy, PartialEq)]
pub enum BuiltinMethodReference {
    Function(&'static BuiltinFunction),
    Array(&'static ArrayMethod),
    Pointer(&'static PointerMethod),
}

impl BuiltinMethodReference {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Function(func) => func.name,
            Self::Array(array) => array.name,
            Self::Pointer(pointer) => pointer.name,
        }
    }

    pub fn function(&self) -> BuiltinFunctionSignature {
        match self {
            Self::Function(f) => f.function,
            Self::Array(f) => f.function,
            Self::Pointer(f) => f.function,
        }
    }
}

impl Debug for BuiltinMethodReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltinMethodReference").finish_non_exhaustive()
    }
}

impl From<&'static BuiltinFunction> for BuiltinMethodReference {
    fn from(value: &'static BuiltinFunction) -> Self {
        Self::Function(value)
    }
}

impl From<&'static ArrayMethod> for BuiltinMethodReference {
    fn from(value: &'static ArrayMethod) -> Self {
        Self::Array(value)
    }
}

impl From<&'static PointerMethod> for BuiltinMethodReference {
    fn from(value: &'static PointerMethod) -> Self {
        Self::Pointer(value)
    }
}

pub(super) static METHODS_BOOL: &'static [BuiltinFunction] = &[];
pub(super) static METHODS_G8: &'static [BuiltinFunction] = &[];
pub(super) static METHODS_G16: &'static [BuiltinFunction] = &[];
pub(super) static METHODS_G32: &'static [BuiltinFunction] = &[];
pub(super) static METHODS_NULL: &'static [BuiltinFunction] = &[];

pub(super) static METHODS_SLINGER: &'static [BuiltinFunction] = &[
    BuiltinFunction {
        name: "bevat",
        documentation: "Controleer of deze slinger een andere slinger bevat.\n## Voorbeeld\n```babbelaar\n\"Hallo\".bevat(\"allo\") // = waar\n```",
        inline_detail: "Controleer de subtekst.",
        function: &slinger_bevat,
        lsp_completion: None,
        parameters: &[
            BuiltinFunctionParameter {
                name: "naald",
                typ: BuiltinType::Slinger,
            },
        ],
        return_type: BuiltinType::Bool,
        must_use: true,
    },
    BuiltinFunction {
        name: "isLeeg",
        documentation: "Geeft `waar` terug als er karakters bestaan in de slinger, anders `onwaar`.",
        inline_detail: "Bevat de slinger tekst.",
        function: &slinger_is_leeg,
        lsp_completion: None,
        parameters: &[],
        return_type: BuiltinType::Bool,
        must_use: true,
    },
    BuiltinFunction {
        name: "begintMet",
        documentation: "Controleer of deze slinger start met een andere slinger.",
        inline_detail: "Controleer de subtekst.",
        function: &slinger_begint_met,
        lsp_completion: None,
        parameters: &[
            BuiltinFunctionParameter {
                name: "subtekst",
                typ: BuiltinType::Slinger,
            },
        ],
        return_type: BuiltinType::Bool,
        must_use: true,
    },
    BuiltinFunction {
        name: "eindigtMet",
        documentation: "Controleer of deze slinger eindigt met een andere slinger.",
        inline_detail: "Controleer de subtekst.",
        function: &slinger_eindigt_met,
        lsp_completion: None,
        parameters: &[
            BuiltinFunctionParameter {
                name: "subtekst",
                typ: BuiltinType::Slinger,
            },
        ],
        return_type: BuiltinType::Bool,
        must_use: true,
    },
    BuiltinFunction {
        name: "naarKleineLetters",
        documentation: "Verander hoofdletters in de slinger naar kleine letters (`A -> a`)",
        inline_detail: "Kleine letters",
        function: &slinger_naar_kleine_letters,
        lsp_completion: None,
        parameters: &[],
        return_type: BuiltinType::Slinger,
        must_use: true,
    },
    BuiltinFunction {
        name: "naarHoofdletters",
        documentation: "Verander kleine letters in de slinger naar kleine letters (`A -> a`)",
        inline_detail: "Hoofdletters",
        function: &slinger_naar_hoofdletters,
        lsp_completion: None,
        parameters: &[],
        return_type: BuiltinType::Slinger,
        must_use: true,
    },
    BuiltinFunction {
        name: "lengte",
        documentation: "Krijg de lengte van de slinger.\n## Voorbeeld\n```babbelaar\n\"Hallo\".lengte() // = 5\n```",
        inline_detail: "Krijg de lengte van de slinger.",
        function: &slinger_lengte,
        lsp_completion: None,
        parameters: &[],
        return_type: BuiltinType::G32,
        must_use: true,
    },
];

pub(super) static METHODS_TEKEN: &'static [BuiltinFunction] = &[];

pub fn slinger_lengte(_: &mut dyn Interpreter, _parameters: Vec<Value>, this: Option<Value>) -> Value {
    Value::Integer(this.unwrap().to_string().len() as _)
}

pub fn slinger_bevat(_: &mut dyn Interpreter, parameters: Vec<Value>, this: Option<Value>) -> Value {
    Value::Bool(this.unwrap().to_string().contains(&parameters[0].to_string()))
}

pub fn slinger_is_leeg(_: &mut dyn Interpreter, _parameters: Vec<Value>, this: Option<Value>) -> Value {
    Value::Bool(this.unwrap().to_string().is_empty())
}

pub fn slinger_begint_met(_: &mut dyn Interpreter, parameters: Vec<Value>, this: Option<Value>) -> Value {
    Value::Bool(this.unwrap().to_string().starts_with(&parameters[0].to_string()))
}

pub fn slinger_eindigt_met(_: &mut dyn Interpreter, parameters: Vec<Value>, this: Option<Value>) -> Value {
    Value::Bool(this.unwrap().to_string().ends_with(&parameters[0].to_string()))
}

pub fn slinger_naar_kleine_letters(_: &mut dyn Interpreter, _parameters: Vec<Value>, this: Option<Value>) -> Value {
    Value::String(this.unwrap().to_string().to_lowercase())
}

pub fn slinger_naar_hoofdletters(_: &mut dyn Interpreter, _parameters: Vec<Value>, this: Option<Value>) -> Value {
    Value::String(this.unwrap().to_string().to_uppercase())
}
