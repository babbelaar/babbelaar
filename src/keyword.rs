// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[derive(strum::AsRefStr, strum::EnumIter)]
#[strum(serialize_all = "snake_case")]
pub enum Keyword {
    Als,
    Functie,
    Structuur,
}

impl Keyword {
    pub fn parse(input: &str) -> Option<Self> {
        use strum::IntoEnumIterator;

        Self::iter().find(|x| x.as_ref() == input)
    }
}
