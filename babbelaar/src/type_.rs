// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{BuiltinType, Ranged};

#[derive(Debug, Clone)]
pub struct Parameter<'source_code> {
    pub name: Ranged<String>,
    pub ty: Ranged<Type<'source_code>>,
}

#[derive(Debug, Clone)]
pub struct Type<'source_code> {
    pub specifier: Ranged<TypeSpecifier<'source_code>>,
}

#[derive(Debug, Clone)]
pub enum TypeSpecifier<'source_code> {
    BuiltIn(BuiltinType),
    Custom {
        name: Ranged<&'source_code str>,
    },
}

impl<'source_code> TypeSpecifier<'source_code> {
    #[must_use]
    pub const fn name(&self) -> &str {
        match self {
            Self::BuiltIn(ty) => ty.name(),
            Self::Custom { name } => name.value(),
        }
    }
}
