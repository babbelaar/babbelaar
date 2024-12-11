// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use crate::{Attribute, BabString, FileRange, Ranged};

use super::SemanticType;

#[derive(Debug, Clone)]
pub struct SemanticExternFunction {
    #[allow(unused)]
    pub name: BabString,
}

#[derive(Debug, Clone)]
pub struct SemanticFunction {
    pub attributes: Vec<Ranged<Attribute>>,
    pub name: Ranged<BabString>,
    pub parameters: Vec<SemanticParameter>,

    /// Signifies that the function allows more arguments than specified in the
    /// functions argument list. (Not intended to be a standard Babbelaar feature,
    /// but for compatibility with C).
    pub has_variable_arguments: bool,

    pub parameters_right_paren_range: FileRange,
    pub extern_function: Option<SemanticExternFunction>,
    pub return_type: Box<SemanticType>,
}

impl Display for SemanticFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("werkwijze ")?;
        f.write_str(&self.name)?;
        f.write_str("()")
    }
}

impl PartialEq for SemanticFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name.value() == other.name.value()
    }
}

#[derive(Debug, Clone)]
pub struct SemanticParameter {
    pub name: Ranged<BabString>,
    pub ty: Ranged<SemanticType>,
}
