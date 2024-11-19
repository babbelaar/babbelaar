// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{BabString, FileRange, Ranged};

use super::{SemanticField, SemanticInterface, SemanticStructure};

pub trait StructureOrInterface {
    #[must_use]
    fn fields(&self) -> &[SemanticField];

    #[must_use]
    fn name(&self) -> &Ranged<BabString>;

    #[must_use]
    fn left_curly_range(&self) -> FileRange;
}

impl StructureOrInterface for SemanticStructure {
    fn fields(&self) -> &[SemanticField] {
        &self.fields
    }

    fn name(&self) -> &Ranged<BabString> {
        &self.name
    }

    fn left_curly_range(&self) -> FileRange {
        self.left_curly_range
    }
}

impl StructureOrInterface for SemanticInterface {
    fn fields(&self) -> &[SemanticField] {
        &[]
    }

    fn name(&self) -> &Ranged<BabString> {
        &self.name
    }

    fn left_curly_range(&self) -> FileRange {
        self.left_curly_range
    }
}
