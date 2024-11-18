// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use thiserror::Error;

use super::BabbelaarFixKind;

#[derive(Debug, Clone, Copy, Error)]
pub enum BabbelaarCommand {
    #[error("Veld hernoemen")]
    RenameField,

    #[error("Parameter hernoemen")]
    RenameParameter,

    #[error("Werkwijze hernoemen")]
    RenameFunction,
}

impl BabbelaarCommand {
    #[must_use]
    pub const fn fix_kind(&self) -> BabbelaarFixKind {
        match self {
            Self::RenameField => BabbelaarFixKind::Refactor,
            Self::RenameParameter => BabbelaarFixKind::Refactor,
            Self::RenameFunction => BabbelaarFixKind::Refactor,
        }
    }
}
