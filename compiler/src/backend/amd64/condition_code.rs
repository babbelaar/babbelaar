// Copyright (C) 2024 - 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use crate::JumpCondition;

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

    #[must_use]
    pub const fn setcc_op_code_part(&self) -> u8 {
        match self {
            Self::Equal => 0x94,
            Self::Greater => 0x9F,
            Self::GreaterOrEqual => 0x9D,
            Self::Less => 0x9C,
            Self::LessOrEqual => 0x9E,
            Self::NotEqual => 0x95,
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

impl From<JumpCondition> for Amd64ConditionCode {
    fn from(value: JumpCondition) -> Self {
        match value {
            JumpCondition::Equal => Amd64ConditionCode::Equal,
            JumpCondition::Greater => Amd64ConditionCode::Greater,
            JumpCondition::GreaterOrEqual => Amd64ConditionCode::GreaterOrEqual,
            JumpCondition::Less => Amd64ConditionCode::Less,
            JumpCondition::LessOrEqual => Amd64ConditionCode::LessOrEqual,
            JumpCondition::NotEqual => Amd64ConditionCode::NotEqual,
        }
    }
}

impl From<Amd64ConditionCode> for JumpCondition {
    fn from(value: Amd64ConditionCode) -> Self {
        match value {
            Amd64ConditionCode::Equal => JumpCondition::Equal,
            Amd64ConditionCode::Greater => JumpCondition::Greater,
            Amd64ConditionCode::GreaterOrEqual => JumpCondition::GreaterOrEqual,
            Amd64ConditionCode::Less => JumpCondition::Less,
            Amd64ConditionCode::LessOrEqual => JumpCondition::LessOrEqual,
            Amd64ConditionCode::NotEqual => JumpCondition::NotEqual,
        }
    }
}
