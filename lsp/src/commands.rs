// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use strum::{AsRefStr, EnumIter, IntoEnumIterator};

#[derive(Debug, Clone, Copy, PartialEq, Eq, AsRefStr, EnumIter)]
pub enum LspCommand {
    #[strum(serialize = "babbelaar.stortAbstracteSyntaxisboom")]
    StortAbstracteSyntaxisboom,

    #[strum(serialize = "babbelaar.stortSemantischeBoom")]
    StortSemantischeBoom,
}

impl LspCommand {
    #[must_use]
    pub fn from_str(str: impl AsRef<str>) -> Option<Self> {
        let str = str.as_ref();
        Self::iter().find(|x| x.name() == str)
    }

    #[must_use]
    pub fn name(&self) -> &str {
        self.as_ref()
    }

    #[must_use]
    pub fn names() -> Vec<String> {
        Self::iter().map(|x| x.name().to_string()).collect()
    }
}
