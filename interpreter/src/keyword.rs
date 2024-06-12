// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use strum::IntoEnumIterator;

use crate::{DocumentationProvider, LspCompletion};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[derive(strum::AsRefStr, strum::EnumIter)]
#[strum(serialize_all = "snake_case")]
pub enum Keyword {
    Als,
    Bekeer,
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

    pub fn lsp_completion(&self) -> Option<LspCompletion<'_>> {
        match self {
            Self::Functie => Some(LspCompletion {
                completion: "functie ${1:naam}() {\n\t$0\n}",
                inline_detail: "Een nieuwe functie.",
            }),
            Self::Volg => Some(LspCompletion {
                completion: "volg ${1:element} in reeks(${2:start}, ${3:eind}) {\n\t$0\n}",
                inline_detail: "Volg in reeks."
            }),
            _ => None,
        }
    }
}

impl DocumentationProvider for Keyword {
    fn provide_documentation(&self) -> std::borrow::Cow<'_, str> {
        match self {
            Self::Als => "Evalueer sectie als een voorwaarde geldt.",
            Self::Bekeer => "Geef een waarde terug aan de aanroeper van de functie.",
            Self::Functie => "Definieer een nieuwe functie.",
            Self::In => "Herhaal over een stel waardes met [`for`].",
            Self::Reeks => {
                r#"Stel een reeks op van getallen.
## Voorbeeld
```babbelaar
volg i in reeks(0, 10) {
    // Gebruik het getal tussen 0 en 10.
}
```"#
            }
            Self::Structuur => "Definieer een datastructuur.",
            Self::Volg => "Herhaal de sectie per waarde van de reeks.",
        }.into()
    }
}
