// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use strum::{EnumIter, IntoEnumIterator};

use super::SemanticType;

#[derive(Debug, Default)]
pub struct SemanticFunctionAnalysis {
    pub parameters: Vec<SemanticType>,
    pub return_type: Option<SemanticType>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumIter)]
pub enum SemanticAnalysisPhase {
    Phase1,
    Phase2,
    Phase3,
    Phase4,
}

impl SemanticAnalysisPhase {
    pub fn iter() -> impl Iterator<Item = SemanticAnalysisPhase> {
        <Self as IntoEnumIterator>::iter()
    }
}

#[derive(Default, Debug)]
pub struct StatementAnalysisState {
    pub assignment_type: Option<SemanticType>,
}
