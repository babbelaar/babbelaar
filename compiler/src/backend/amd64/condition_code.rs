// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Amd64ConditionCode {
    Equal,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
    NotEqual,
}

impl Amd64ConditionCode {
    #[must_use]
    pub const fn jcc_short_code(&self) -> u8 {
        match self {
            Self::Equal => 0x74,
            Self::Greater => 0x7f,
            Self::GreaterOrEqual => 0x7d,
            Self::Less => 0x7c,
            Self::LessOrEqual => 0x7e,
            Self::NotEqual => 0x75,
        }
    }
}

impl Display for Amd64ConditionCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Equal => f.write_str("e"),
            Self::Greater => f.write_str("g"),
            Self::GreaterOrEqual => f.write_str("ge"),
            Self::Less => f.write_str("l"),
            Self::LessOrEqual => f.write_str("le"),
            Self::NotEqual => f.write_str("ne"),
        }
    }
}
