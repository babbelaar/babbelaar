// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod functions;
mod types;

pub use self::{
    functions::{BuiltinFunction, BuiltinFunctionParameter},
    types::BuiltinType,
};

pub struct Builtin;

impl Builtin {
    pub const FUNCTIONS: &'static [BuiltinFunction] = &[
        BuiltinFunction {
            name: "schrijf",
            documentation: "Schrijf tekst naar de uitvoer",
            inline_detail: "Schrijf tekst naar de uitvoer",
            function: &functions::schrijf,
            lsp_completion: Some("schrijf(\"$1\")$0"),
            parameters: &[
                BuiltinFunctionParameter {
                    name: "uitvoer",
                }
            ]
        }
    ];

    pub const TYPES: &'static [BuiltinType] = &[
        BuiltinType {
            name: "g32",
            documentation: "Een geheel getal met 32-bits precisie.",
        },
        BuiltinType {
            name: "g32",
            documentation: "Een geheel getal met 32-bits precisie.",
        },
    ];
}
