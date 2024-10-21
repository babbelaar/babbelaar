// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use crate::{BabString, BuiltinFunction};

use super::methods::{METHODS_BOOL, METHODS_G16, METHODS_G32, METHODS_G8, METHODS_NULL, METHODS_SLINGER, METHODS_TEKEN};

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
#[repr(u8)]
pub enum BuiltinType {
    Bool,
    G8,
    G16,
    G32,
    Null,
    Slinger,
    Teken,
}

impl BuiltinType {
    #[must_use]
    pub const fn name(&self) -> BabString {
        match self {
            Self::Bool => BabString::new_static("bool"),
            Self::G8 => BabString::new_static("g8"),
            Self::G16 => BabString::new_static("g16"),
            Self::G32 => BabString::new_static("g32"),
            Self::Null => BabString::new_static("null"),
            Self::Slinger => BabString::new_static("Slinger"),
            Self::Teken => BabString::new_static("teken"),
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
                str += &param.typ.name();
            }

            str += ") -> ";
            str += &method.return_type.name();
            str += " { /* (ingebouwd) */ }\n";
        }

        str += "\n}";

        str
    }

    #[must_use]
    pub const fn inline_detail(&self) -> BabString {
        BabString::new_static(match self {
            Self::Bool => "Een schakeling tussen `waar` en `onwaar`.",
            Self::G8 => "Een geheel getal met 8-bits precisie.",
            Self::G16 => "Een geheel getal met 16-bits precisie.",
            Self::G32 => "Een geheel getal met 32-bits precisie.",
            Self::Null => "Tijdelijk type, niet gebruiken",
            Self::Slinger => "Een stuk tekst, schrijfbaar met bijvoorbeeld: \"Hallo, slinger!\"",
            Self::Teken => "Een letter, cijfer of speciaal teken.",
        })
    }

    #[must_use]
    pub fn methods(&self) -> &'static [BuiltinFunction] {
        match self {
            Self::Bool => METHODS_BOOL,
            Self::G8 => METHODS_G8,
            Self::G16 => METHODS_G16,
            Self::G32 => METHODS_G32,
            Self::Null => METHODS_NULL,
            Self::Slinger => METHODS_SLINGER,
            Self::Teken => METHODS_TEKEN,
        }
    }
}

impl Display for BuiltinType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name())
    }
}
