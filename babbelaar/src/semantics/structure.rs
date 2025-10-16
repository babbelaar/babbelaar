// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use crate::{AttributeList, BabString, FileRange, Ranged};

use super::{SemanticMethod, SemanticType};

#[derive(Debug, Clone)]
pub struct SemanticField {
    pub attributes: AttributeList,
    pub name: Ranged<BabString>,
    pub ty: SemanticType,
    pub has_default_value: bool,
}

#[derive(Debug, Clone)]
pub struct SemanticStructure {
    pub attributes: AttributeList,
    pub name: Ranged<BabString>,
    pub generic_types: Vec<Ranged<BabString>>,
    pub left_curly_range: FileRange,
    pub right_curly_range: FileRange,
    pub fields: Vec<SemanticField>,
    pub methods: Vec<SemanticMethod>,
}

impl SemanticStructure {
    pub fn index_of_generic_type(&self, name: &BabString) -> Option<usize> {
        for (idx, generic_name) in self.generic_types.iter().enumerate() {
            if generic_name.value() == name {
                return Some(idx);
            }
        }

        None
    }
}

impl Display for SemanticStructure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

impl PartialEq for SemanticStructure {
    fn eq(&self, other: &Self) -> bool {
        self.name.range() == other.name.range() && self.name.value() == other.name.value()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SemanticFieldId {
    id: usize,
}

impl SemanticFieldId {
    #[must_use]
    pub fn new(id: usize) -> Self {
        Self { id }
    }

    #[must_use]
    pub const fn id(&self) -> usize {
        self.id
    }
}
