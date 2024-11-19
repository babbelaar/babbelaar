// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use crate::{AttributeList, BabString, FileRange, Ranged};

use super::SemanticMethod;

#[derive(Debug)]
pub struct SemanticInterface {
    pub attributes: AttributeList,
    pub name: Ranged<BabString>,
    pub generic_types: Vec<Ranged<BabString>>,
    pub left_curly_range: FileRange,
    pub right_curly_range: FileRange,
    pub methods: Vec<SemanticMethod>,
}

impl Display for SemanticInterface {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

impl PartialEq for SemanticInterface {
    fn eq(&self, other: &Self) -> bool {
        self.name.range() == other.name.range() && self.name.value() == other.name.value()
    }
}
