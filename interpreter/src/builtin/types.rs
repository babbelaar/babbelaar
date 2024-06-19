// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use crate::BuiltinFunction;

use super::methods::{METHODS_BOOL, METHODS_G32, METHODS_NULL, METHODS_SLINGER};

#[derive(Debug, Clone, Copy, PartialEq)]
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
    pub const fn documentation(&self) -> &'static str {
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
