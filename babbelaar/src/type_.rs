// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{BabString, BuiltinType, Ranged};

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: Ranged<BabString>,
    pub ty: Ranged<Type>,
}

#[derive(Debug, Clone)]
pub struct Type {
    pub specifier: Ranged<TypeSpecifier>,
    pub qualifiers: Vec<Ranged<TypeQualifier>>,
}

#[derive(Debug, Clone)]
pub enum TypeSpecifier {
    BuiltIn(BuiltinType),
    Custom {
        name: Ranged<BabString>,
    },
}

impl TypeSpecifier {
    #[must_use]
    pub fn name(&self) -> BabString {
        match self {
            Self::BuiltIn(ty) => ty.name(),
            Self::Custom { name } => name.value().clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeQualifier {
    Array,
}
