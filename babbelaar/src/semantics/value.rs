// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{BabString, FileRange};

use super::SemanticType;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PureValue {
    ConstantValue,
    FieldReference {
        declaration: FileRange,
        name: BabString,
    },
    Operator {
        operator_range: FileRange,
    },
    ReturnValue,
    IndexReference,
}

#[derive(Debug)]
pub struct SemanticValue {
    pub ty: SemanticType,
    pub usage: SemanticUsage,
}

impl SemanticValue {
    #[must_use]
    pub fn null() -> Self {
        Self {
            ty: SemanticType::null(),
            usage: SemanticUsage::Indifferent,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemanticUsage {
    Indifferent,
    Pure(PureValue),
}
