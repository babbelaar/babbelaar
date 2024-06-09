// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

#[derive(Clone, Debug)]
pub enum Value {
    /// hehe 5 billion dollar problem
    Null,

    Integer(i64),
    String(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => f.write_str("null"),
            Self::Integer(i) => i.fmt(f),
            Self::String(str) => f.write_str(str),
        }
    }
}
