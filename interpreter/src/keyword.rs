// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use strum::IntoEnumIterator;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[derive(strum::AsRefStr, strum::EnumIter)]
#[strum(serialize_all = "snake_case")]
pub enum Keyword {
    Als,
    Functie,
    In,
    Reeks,
    Structuur,
    Volg,
}

impl Keyword {
    pub fn iter_variants() -> impl Iterator<Item = Self> {
        Self::iter()
    }

    pub fn parse(input: &str) -> Option<Self> {
        Self::iter().find(|x| x.as_ref() == input)
    }
}
