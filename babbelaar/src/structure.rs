// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{Attribute, Ranged, Type};

#[derive(Debug, Clone)]
pub struct Field<'source_code> {
    pub attributes: Vec<Attribute<'source_code>>,
    pub name: Ranged<&'source_code str>,
    pub ty: Ranged<Type<'source_code>>,
}

#[derive(Debug, Clone)]
pub struct Structure<'source_code> {
    pub name: Ranged<&'source_code str>,
    pub fields: Vec<Field<'source_code>>,
}
