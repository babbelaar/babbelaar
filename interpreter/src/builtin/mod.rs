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
            lsp_completion: Some("schrijf(\"$1\");$0"),
            parameters: &[
                BuiltinFunctionParameter {
                    name: "uitvoer",
                    typ: Self::TYPE_SLINGER,
                }
            ]
        },
        BuiltinFunction {
            name: "lees",
            documentation: "Lees tekst vanuit de invoer",
            inline_detail: "Lees tekst",
            function: &functions::lees,
            lsp_completion: Some("lees();$0"),
            parameters: &[]
        }
    ];

    pub const TYPES: &'static [&'static BuiltinType] = &[
        Self::TYPE_BOOL,
        Self::TYPE_G32,
        Self::TYPE_SLINGER,
    ];

    pub const TYPE_BOOL: &'static BuiltinType = &BuiltinType {
        name: "bool",
        documentation: "Een schakeling tussen `waar` en `onwaar`.",
    };

    pub const TYPE_G32: &'static BuiltinType = &BuiltinType {
        name: "g32",
        documentation: "Een geheel getal met 32-bits precisie.",
    };

    pub const TYPE_NULL: &'static BuiltinType = &BuiltinType {
        name: "null",
        documentation: "Tijdelijk type, niet gebruiken",
    };

    pub const TYPE_SLINGER: &'static BuiltinType = &BuiltinType {
        name: "Slinger",
        documentation: "Een stuk tekst, schrijfbaar met bijvoorbeeld: \"Hallo, slinger!\"",
    };
}
