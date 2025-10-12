// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{fmt::Display, sync::Arc};

use crate::{BabString, BuiltinFunction, FileRange, SemanticType};

use super::SemanticFunction;

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionReference {
    Builtin(&'static BuiltinFunction),
    Custom(Arc<SemanticFunction>),
}

impl Display for FunctionReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Builtin(func) => func.fmt(f),
            Self::Custom(func) => func.fmt(f),
        }
    }
}

impl FunctionReference {
    #[must_use]
    pub fn parameter_count(&self) -> usize {
        match self {
            Self::Builtin(func) => func.parameters.len(),
            Self::Custom(func) => func.parameters.len(),
        }
    }

    #[must_use]
    pub fn name(&self) -> BabString {
        match self {
            Self::Builtin(func) => BabString::new_static(func.name),
            Self::Custom(func) => func.name.value().to_owned(),
        }
    }

    #[must_use]
    pub fn documentation(&self) -> Option<BabString> {
        match self {
            Self::Builtin(func) => Some(BabString::new_static(func.documentation)),
            Self::Custom(..) => None,
        }
    }

    #[must_use]
    pub fn lsp_completion(&self) -> BabString {
        if let Some(completion) = self.lsp_completion_raw() {
            return completion.into();
        }

        let mut insert = format!("{}(", self.name());

        for i in 0..self.parameter_count() {
            let comma = if i == 0 { "" } else { ", " };
            insert += &format!("{comma}${}", i + 1);
        }

        insert += ");$0";
        insert.into()
    }

    #[must_use]
    pub fn lsp_completion_raw(&self) -> Option<BabString> {
        match self {
            Self::Builtin(func) => func.lsp_completion.map(|x| BabString::new_static(x)),
            Self::Custom(..) => None,
        }
    }

    #[must_use]
    pub fn declaration_range(&self) -> FileRange {
        match self {
            Self::Builtin(..) => FileRange::default(),
            Self::Custom(func) => func.name.range(),
        }
    }

    #[must_use]
    pub fn inline_detail(&self) -> Option<BabString> {
        match self {
            Self::Builtin(func) => Some(BabString::new_static(func.inline_detail)),
            Self::Custom(..) => None,
        }
    }

    #[must_use]
    pub fn return_type(&self) -> SemanticType {
        match self {
            Self::Builtin(func) => SemanticType::Builtin(func.return_type),
            Self::Custom(func) => func.return_type.as_ref().clone(),
        }
    }
}
