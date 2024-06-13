// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

#[derive(Clone, Debug)]
pub enum Value {
    /// hehe 5 billion dollar problem
    Null,

    Bool(bool),
    Integer(i64),
    String(String),
}
impl Value {
    #[must_use]
    pub fn is_true(&self) -> bool {
        matches!(self, Self::Bool(true))
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => f.write_str("null"),
            Self::Bool(b) => b.fmt(f),
            Self::Integer(i) => i.fmt(f),
            Self::String(str) => f.write_str(str),
        }
    }
}
