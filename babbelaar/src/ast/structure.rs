// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{AttributeList, BabString, Expression, FileRange, FunctionStatement, Ranged, Type};

#[derive(Debug, Clone)]
pub struct Field {
    pub attributes: AttributeList,
    pub name: Ranged<BabString>,
    pub ty: Ranged<Type>,
    pub default_value: Option<Ranged<Expression>>,
}

#[derive(Debug, Clone)]
pub struct InterfaceStatement {
    pub name: Ranged<BabString>,
    pub generic_types: Vec<Ranged<BabString>>,
    pub left_curly_range: FileRange,
    pub methods: Vec<Method>,
    pub right_curly_range: FileRange,
}

#[derive(Debug, Clone)]
pub struct Method {
    pub range: FileRange,
    pub function: FunctionStatement,
    pub is_static: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructureId {
    id: usize,
}

impl StructureId {
    #[must_use]
    pub fn new(id: usize) -> Self {
        Self { id }
    }

    #[must_use]
    pub const fn id(&self) -> usize {
        self.id
    }
}
#[derive(Debug, Clone)]
pub struct Structure {
    pub id: StructureId,
    pub name: Ranged<BabString>,
    pub generic_types: Vec<Ranged<BabString>>,
    pub left_curly_range: FileRange,
    pub right_curly_range: FileRange,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
}
