// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{BabString, FileRange, PrimaryExpression, Ranged};

pub type AttributeList = Vec<Ranged<Attribute>>;

#[derive(Debug, Clone)]
pub struct Attribute {
    pub at_range: FileRange,
    pub name: Ranged<BabString>,
    pub arguments: Ranged<Vec<AttributeArgument>>,
}

impl Attribute {
    pub const NAME_EXTERN: &'static str = "uitheems";
}

#[derive(Debug, Clone)]
pub struct AttributeArgument {
    pub name: Ranged<BabString>,
    pub value: Ranged<PrimaryExpression>,
}
