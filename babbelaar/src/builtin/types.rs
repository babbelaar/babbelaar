// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use crate::BuiltinFunction;

use super::methods::{METHODS_BOOL, METHODS_G32, METHODS_NULL, METHODS_SLINGER};

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum BuiltinType {
    Bool,
    G32,
    Null,
    Slinger,
}

impl BuiltinType {
    #[must_use]
    pub const fn name(&self) -> &'static str {
        match self {
            Self::Bool => "bool",
            Self::G32 => "g32",
            Self::Null => "null",
            Self::Slinger => "Slinger",
        }
    }

    #[must_use]
    pub fn documentation(&self) -> String {
        format!("# {name}\n{description}\n## Voorvertoning\n```babbelaar\n{preview}\n```\n",
            name = self.name(),
            description = self.inline_detail(),
            preview = self.structure_preview(),
        )
    }

    #[must_use]
    pub fn structure_preview(&self) -> String {
        let mut str = format!("structuur {} {{\n", self.name());

        for method in self.methods() {
            for docu_line in method.documentation.lines() {
                str += "\n    /// ";
                str += docu_line;
            }

            str += "\n    werkwijze ";
            str += method.name;
            str += "(";
            for (idx, param) in method.parameters.iter().enumerate() {
                if idx != 0 {
                    str += ", ";
                }
                str += param.name;
                str += ": ";
                str += param.typ.name();
            }

            str += ") -> ";
            str += method.return_type.name();
            str += " { /* (ingebouwd) */ }\n";
        }

        str += "\n}";

        str
    }

    #[must_use]
    pub const fn inline_detail(&self) -> &'static str {
        match self {
            Self::Bool => "Een schakeling tussen `waar` en `onwaar`.",
            Self::G32 => "Een geheel getal met 32-bits precisie.",
            Self::Null => "Tijdelijk type, niet gebruiken",
            Self::Slinger => "Een stuk tekst, schrijfbaar met bijvoorbeeld: \"Hallo, slinger!\"",
        }
    }

    #[must_use]
    pub fn methods(&self) -> &'static [BuiltinFunction] {
        match self {
            Self::Bool => METHODS_BOOL,
            Self::G32 => METHODS_G32,
            Self::Null => METHODS_NULL,
            Self::Slinger => METHODS_SLINGER,
        }
    }
}

impl Display for BuiltinType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}
