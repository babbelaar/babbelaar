// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::FileRange;

use super::SemanticType;

#[derive(Debug, Clone)]
pub struct SemanticLocal {
    pub kind: SemanticLocalKind,
    pub name_declaration_range: FileRange,
    pub typ: SemanticType,
    pub usage_count: usize,
    pub full_declaration_range: Option<FileRange>,
}

impl SemanticLocal {
    #[must_use]
    pub fn new(kind: SemanticLocalKind, typ: SemanticType, declaration_range: FileRange) -> Self {
        Self {
            kind,
            name_declaration_range: declaration_range,
            typ,
            usage_count: 0,
            full_declaration_range: None,
        }
    }

    pub fn with_declaration_range(self, range: FileRange) -> Self {
        Self {
            full_declaration_range: Some(range),
            ..self
        }
    }

    pub fn add_usage(&mut self) {
        self.usage_count += 1;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticLocalKind {
    Parameter,
    FieldReference,
    StructureReference,
    Iterator,
    Function,
    FunctionReference,
    Variable,
    Method,
    ReferenceThis,
}

impl SemanticLocalKind {
    #[must_use]
    pub const fn is_function(&self) -> bool {
        match self {
            Self::Parameter => false,
            Self::FieldReference => false,
            Self::StructureReference => true,
            Self::Iterator => false,
            Self::Function => true,
            Self::FunctionReference => true,
            Self::Variable => false,
            Self::Method => true,
            Self::ReferenceThis => false,
        }
    }

    #[must_use]
    pub fn name(&self) -> &'static str {
        match self {
            Self::Parameter => "parameter",
            Self::FieldReference => "veld",
            Self::StructureReference => "structuur",
            Self::Iterator => "iterator",
            Self::Function => "werkwijze",
            Self::FunctionReference => "werkwijze",
            Self::Variable => "variabele",
            Self::Method => "structuurwerkwijze",
            Self::ReferenceThis => "dit",
        }
    }
}
