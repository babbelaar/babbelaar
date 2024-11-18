// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::{Display, Formatter};

use crate::{BabString, FileLocation, FileRange, Ranged};

use super::TokenKind;

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub begin: FileLocation,
    pub end: FileLocation,
}

impl Token {
    pub fn as_identifier(&self) -> Option<Ranged<BabString>> {
        let TokenKind::Identifier(ident) = &self.kind else {
            return None;
        };

        Some(Ranged::new(self.range(), ident.clone()))
    }
}

impl Token {
    pub fn range(&self) -> FileRange {
        (self.begin, self.end).into()
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}
