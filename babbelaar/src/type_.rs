// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::{Display, Write};

use crate::{AttributeList, BabString, BuiltinType, Ranged};

#[derive(Debug, Clone)]
pub struct Parameter {
    pub attributes: AttributeList,
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
    BuiltIn(Ranged<BuiltinType>),
    Custom {
        name: Ranged<BabString>,
        type_parameters: Vec<Ranged<Type>>,
    },
}

impl Display for TypeSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BuiltIn(ty) => f.write_str(&ty.name()),
            Self::Custom { name, type_parameters } => {
                f.write_str(&name)?;

                if !type_parameters.is_empty() {
                    f.write_char('<')?;

                    for (idx, param) in type_parameters.iter().enumerate() {
                        if idx != 0 {
                            f.write_str(", ")?;
                        }
                        param.fmt(f)?;
                    }

                    f.write_char('>')?;
                }

                Ok(())
            }
        }
    }
}

impl TypeSpecifier {
    #[must_use]
    pub fn fully_qualified_name(&self) -> BabString {
        match self {
            Self::BuiltIn(ty) => ty.name(),
            Self::Custom { .. } => {
                BabString::new(self.to_string())
            }
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
