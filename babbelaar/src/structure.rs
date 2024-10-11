// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{Attribute, BabString, Expression, FileRange, FunctionStatement, Ranged, Type};

#[derive(Debug, Clone)]
pub struct Field {
    pub attributes: Vec<Attribute>,
    pub name: Ranged<BabString>,
    pub ty: Ranged<Type>,
    pub default_value: Option<Ranged<Expression>>,
}

#[derive(Debug, Clone)]
pub struct Method {
    pub range: FileRange,
    pub function: FunctionStatement,
}

#[derive(Debug, Clone)]
pub struct Structure {
    pub name: Ranged<BabString>,
    pub generic_types: Vec<Ranged<BabString>>,
    pub left_curly_range: FileRange,
    pub right_curly_range: FileRange,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
}
