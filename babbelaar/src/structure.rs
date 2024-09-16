// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::rc::Rc;

use crate::{Attribute, FileRange, FunctionStatement, Ranged, Type};

#[derive(Debug, Clone)]
pub struct Field<'source_code> {
    pub attributes: Vec<Attribute<'source_code>>,
    pub name: Ranged<&'source_code str>,
    pub ty: Ranged<Type<'source_code>>,
}

#[derive(Debug, Clone)]
pub struct Method<'source_code> {
    pub range: FileRange,

    /// This is an [`Rc`] for the interpreter's convenience sake.
    pub function: Rc<FunctionStatement<'source_code>>,
}

#[derive(Debug, Clone)]
pub struct Structure<'source_code> {
    pub name: Ranged<&'source_code str>,
    pub left_curly_range: FileRange,
    pub fields: Vec<Field<'source_code>>,
    pub methods: Vec<Method<'source_code>>,
}
