// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{AttributeList, BabString, BuiltinType, Expression, FileRange, FunctionStatement, Ranged, Type};

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

#[derive(Debug, Clone)]
pub struct Structure {
    pub name: Ranged<BabString>,
    pub generic_types: Vec<Ranged<BabString>>,
    pub left_curly_range: FileRange,
    pub right_curly_range: FileRange,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
}

impl Structure {
    /// Dit wordt gebruikt als hulpmiddel voor de interpreter.
    #[must_use]
    pub fn from_builtin_type(ty: BuiltinType) -> Self {
        Self {
            name: Ranged::new(FileRange::INTERNAL, ty.name()),
            generic_types: Vec::new(),
            left_curly_range: FileRange::INTERNAL,
            right_curly_range: FileRange::INTERNAL,
            fields: Vec::new(),
            methods: Vec::new(),
        }
    }
}
