// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{BabString, FileRange, PrimaryExpression, Ranged};

#[derive(Debug, Clone)]
pub struct Attribute {
    pub at_range: FileRange,
    pub name: Ranged<BabString>,
    pub arguments: Vec<AttributeArgument>
}

#[derive(Debug, Clone)]
pub struct AttributeArgument {
    pub name: Ranged<BabString>,
    pub value: Ranged<PrimaryExpression>,
}
