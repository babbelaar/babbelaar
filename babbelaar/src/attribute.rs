// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{PrimaryExpression, Ranged};

#[derive(Debug, Clone)]
pub struct Attribute<'source_code> {
    pub name: Ranged<&'source_code str>,
    pub arguments: Vec<AttributeArgument<'source_code>>
}

#[derive(Debug, Clone)]
pub struct AttributeArgument<'source_code> {
    pub name: Ranged<&'source_code str>,
    pub value: Ranged<PrimaryExpression<'source_code>>,
}
