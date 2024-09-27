// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod array;
mod functions;
mod methods;
mod types;

pub use self::{
    array::{BuiltinArray, ArrayMethod, ArrayMethodParameter, ArrayTypeRef},
    functions::{BuiltinFunction, BuiltinFunctionParameter},
    methods::BuiltinMethodReference,
    types::BuiltinType,
};

pub struct Builtin;

impl Builtin {
    #[must_use]
    pub fn type_by_name(name: &str) -> Option<BuiltinType> {
        Self::TYPES.iter().find(|x| x.name() == name).copied()
    }

    pub fn array() -> BuiltinArray {
        BuiltinArray
    }

    pub const FUNCTIONS: &'static [BuiltinFunction] = &[
        BuiltinFunction {
            name: "schrijf",
            documentation: "Schrijf tekst naar de uitvoer",
            inline_detail: "Schrijf tekst naar de uitvoer",
            function: &functions::schrijf,
            lsp_completion: Some("schrijf(\"$1\");$0"),
            parameters: &[
                BuiltinFunctionParameter {
                    name: "uitvoer",
                    typ: BuiltinType::Slinger,
                }
            ],
            return_type: BuiltinType::Null,
            must_use: true,
        },
        BuiltinFunction {
            name: "lees",
            documentation: "Lees tekst vanuit de invoer",
            inline_detail: "Lees tekst",
            function: &functions::lees,
            lsp_completion: Some("lees();$0"),
            parameters: &[],
            return_type: BuiltinType::Null,
            must_use: true,
        }
    ];

    pub const TYPES: &'static [BuiltinType] = &[
        BuiltinType::Bool,
        BuiltinType::G32,
        BuiltinType::Slinger,
    ];
}
