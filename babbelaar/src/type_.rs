// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

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

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.specifier.value(), f)?;

        for qual in &self.qualifiers {
            Display::fmt(qual.value(), f)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum TypeSpecifier {
    BuiltIn(BuiltinType),
    Custom {
        name: Ranged<BabString>,
    },
}

impl Display for TypeSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BuiltIn(ty) => f.write_str(&ty.name()),
            Self::Custom { name } => f.write_str(&name),
        }
    }
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

impl Display for TypeQualifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Array => f.write_str("[]"),
        }
    }
}
