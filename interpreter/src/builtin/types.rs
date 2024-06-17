// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use crate::BuiltinFunction;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BuiltinType {
    pub name: &'static str,
    pub documentation: &'static str,
    pub methods: &'static [BuiltinFunction],
}

impl Display for BuiltinType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name)
    }
}
