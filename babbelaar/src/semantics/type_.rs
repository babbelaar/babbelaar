// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{fmt::{Display, Write}, sync::Arc};

use crate::{BabString, BuiltinType, FileRange};

use super::{FunctionReference, SemanticFunction, SemanticInterface, SemanticStructure};

#[derive(Debug, Clone, PartialEq)]
pub struct SemanticGenericType {
    pub index: usize,
    pub name: BabString,
    pub declaration_range: FileRange,
}

impl Display for SemanticGenericType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SemanticType {
    Array(Box<SemanticType>),
    Builtin(BuiltinType),
    Custom { base: Arc<SemanticStructure>, parameters: Vec<SemanticType> },
    Function(SemanticFunction),
    FunctionReference(FunctionReference),
    Interface { base: Arc<SemanticInterface>, parameters: Vec<SemanticType> },
    Generic(SemanticGenericType),
    IndexReference(Box<SemanticType>),
    Pointer(Box<SemanticType>),
}

impl SemanticType {
    #[must_use]
    pub fn null() -> Self {
        Self::Builtin(BuiltinType::Null)
    }

    #[must_use]
    pub fn is_null(&self) -> bool {
        matches!(self, Self::Builtin(BuiltinType::Null))
    }

    pub fn declaration_range(&self) -> FileRange {
        match self {
            Self::Array(ty) => ty.declaration_range(),
            Self::Builtin(..) => FileRange::default(),
            Self::Custom { base, .. } => base.name.range(),
            Self::Function(func) => func.name.range(),
            Self::FunctionReference(func) => func.declaration_range(),
            Self::IndexReference(ty) => ty.declaration_range(),
            Self::Interface { base, .. } => base.name.range(),
            Self::Generic(ty) => ty.declaration_range,
            Self::Pointer(ty) => ty.declaration_range(),
        }
    }

    pub fn parameter_count(&self) -> Option<usize> {
        match self {
            Self::Array(..) => None,
            Self::Builtin(..) => None,
            Self::Custom { .. } => None,
            Self::Function(func) => Some(func.parameters.len()),
            Self::FunctionReference(func) => Some(func.parameter_count()),
            Self::IndexReference(..) => None,
            Self::Interface { .. } => None,
            Self::Generic(..) => None,
            Self::Pointer(..) => None,
        }
    }

    /// A hint to a name that could be used as the name for a field or value.
    #[must_use]
    pub fn value_or_field_name_hint(&self) -> BabString {
        match self {
            Self::Array(..) => BabString::new_static("opeenvolging"),
            Self::Builtin(BuiltinType::Slinger) => BabString::new_static("tekst"),
            Self::Builtin(BuiltinType::G32) => BabString::new_static("getal"),
            Self::Builtin(builtin) => builtin.name().to_lowercase().into(),
            Self::Custom { base, .. } => base.name.value().to_lowercase().into(),
            Self::Interface { base, .. } => base.name.value().to_lowercase().into(),

            Self::Function(..) => BabString::empty(),
            Self::FunctionReference(..) => BabString::empty(),
            Self::IndexReference(ty) => ty.value_or_field_name_hint(),
            Self::Generic(ty) => ty.name.clone(),
            Self::Pointer(..) => BabString::new_static("wijzer"),
        }
    }

    /// A hint to a value that could be used as the default value.
    #[must_use]
    pub fn default_value_hint(&self) -> &str {
        match self {
            Self::Builtin(BuiltinType::Slinger) => "\"\"",
            Self::Builtin(BuiltinType::G32) => "0",
            Self::Builtin(BuiltinType::Bool) => "onwaar",

            _ => "",
        }
    }

    #[must_use]
    pub fn name(&self) -> BabString {
        match self {
            Self::Array(..) => BabString::new_static("opeenvolging-naam"),
            Self::Builtin(builtin) => builtin.name().into(),
            Self::Custom { base, .. } => base.name.value().clone(),
            Self::Function(func) => func.name.value().clone(),
            Self::FunctionReference(func) => func.name(),
            Self::IndexReference(ty) => ty.name(),
            Self::Interface { base, .. } => base.name.value().clone(),
            Self::Generic(ty) => ty.name.clone(),
            Self::Pointer(..) => BabString::new_static("wijzer-naam"),
        }
    }

    /// Returns the element type, if it is subscriptable.
    pub fn subscript(&self) -> Option<SemanticType> {
        match self {
            Self::Array(ty) => Some(ty.as_ref().clone()),
            Self::Builtin(BuiltinType::Slinger) => Some(SemanticType::Builtin(BuiltinType::Teken)),
            _ => None,
        }
    }

    pub fn resolve_against(self, ty: &SemanticType) -> Self {
        let generic_index = match self {
            Self::Array(element_type) => {
                let element_type = *element_type;
                let element_type = element_type.resolve_against(ty);
                return Self::Array(Box::new(element_type));
            }

            Self::Generic(ref generic) => generic.index,

            other => return other,
        };

        let SemanticType::Custom { parameters, .. } = ty else {
            log::error!("Kan generieke parameters niet resolveren met type: {ty:#?}");
            return self;
        };

        if parameters.len() <= generic_index {
            // Te doen: kan ik vreemde omstandigheden gebeuren.
            return self;
        }

        parameters[generic_index].clone()
    }

    #[must_use]
    pub fn can_be_extended(&self) -> bool {
        !self.is_null()
    }

    #[must_use]
    pub fn is_primitive_number(&self) -> bool {
        match self {
            Self::Builtin(BuiltinType::G8) => true,
            Self::Builtin(BuiltinType::G16) => true,
            Self::Builtin(BuiltinType::G32) => true,
            _ => false,
        }
    }

    #[must_use]
    pub fn is_compatible_with(&self, other: &SemanticType) -> bool {
        if self == other {
            return true;
        }

        self.is_primitive_number() && other.is_primitive_number()
    }
}

impl Display for SemanticType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Array(arr) => {
                arr.fmt(f)?;
                f.write_str("[]")
            }
            Self::Builtin(typ) => typ.fmt(f),
            Self::Custom { base, parameters } => {
                base.fmt(f)?;

                if parameters.is_empty() {
                    return Ok(());
                }

                f.write_char('<')?;

                for (idx, param) in parameters.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }

                    param.fmt(f)?;
                }

                f.write_char('>')
            }
            Self::Function(func) => func.fmt(f),
            Self::FunctionReference(func) => func.fmt(f),
            Self::IndexReference(ty) => ty.fmt(f),
            Self::Interface { base, parameters } => {
                base.fmt(f)?;

                if parameters.is_empty() {
                    return Ok(());
                }

                f.write_char('<')?;

                for (idx, param) in parameters.iter().enumerate() {
                    if idx != 0 {
                        f.write_str(", ")?;
                    }

                    param.fmt(f)?;
                }

                f.write_char('>')
            }
            Self::Generic(ty) => ty.fmt(f),
            Self::Pointer(ty) => {
                ty.fmt(f)?;
                f.write_char('*')
            }
        }
    }
}

impl PartialEq<BuiltinType> for SemanticType {
    fn eq(&self, other: &BuiltinType) -> bool {
        self == &Self::Builtin(*other)
    }
}
