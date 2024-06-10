// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinType {
    pub name: &'static str,
    pub documentation: &'static str,
}
