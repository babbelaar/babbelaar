// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use strum::IntoEnumIterator;

use crate::{DocumentationProvider, LspCompletion};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[derive(strum::AsRefStr, strum::EnumIter)]
#[strum(serialize_all = "snake_case")]
pub enum Keyword {
    Als,
    Bekeer,
    Dit,
    In,
    Koppelvlak,
    Nieuw,
    Onwaar,
    Op,
    Reeks,
    Stel,
    Structuur,
    Uitbreiding,
    Veld,
    Volg,
    Waar,
    Werkwijze,
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
            Self::Als => Some(LspCompletion {
                completion: "als ${1:conditie} {\n\t${0:dan}\n}",
                inline_detail: "Voorwaardelijk een sectie uitvoeren."
            }),
            Self::Werkwijze => Some(LspCompletion {
                completion: "werkwijze ${1:naam}() {\n\t$0\n}",
                inline_detail: "Een nieuwe werkwijze.",
            }),
            Self::Koppelvlak => Some(LspCompletion {
                completion: "koppelvlak ${1:naam} {\n\t$0\n}",
                inline_detail: "Een nieuw koppelvlak",
            }),
            Self::Nieuw => Some(LspCompletion {
                completion: "nieuw ${1:structuurnaam} {\n\t${0:velden}\n}",
                inline_detail: "Een nieuw object",
            }),
            Self::Stel => Some(LspCompletion {
                completion: "stel ${1:variabele} = ${2:waarde};\n${0}",
                inline_detail: "Een nieuwe variabele",
            }),
            Self::Structuur => Some(LspCompletion {
                completion: "structuur ${1:naam} {\n\t${0:velden}\n}",
                inline_detail: "Een nieuwe structuur",
            }),
            Self::Veld => Some(LspCompletion {
                completion: "veld ${1:naam}: ${0:type},",
                inline_detail: "Een nieuw veld",
            }),
            Self::Volg => Some(LspCompletion {
                completion: "volg ${1:element} in reeks(${2:start}, ${3:eind}) {\n\t$0\n}",
                inline_detail: "Volg in reeks."
            }),
            Self::Dit => Some(LspCompletion {
                completion: "dit.$0",
                inline_detail: "De huidige waarde binnen een werkwijze van een structuur",
            }),
            _ => None,
        }
    }
}

impl DocumentationProvider for Keyword {
    fn provide_documentation(&self) -> std::borrow::Cow<'_, str> {
        match self {
            Self::Als => "Evalueer sectie als een voorwaarde geldt.",
            Self::Bekeer => "Geef een waarde terug aan de aanroeper van de werkwijze.",
            Self::Dit => r#"De huidige waarde binnen een werkwijze van een structuur.
## Voorbeeld
Gegeven is het volgende stukje Babbelaar:
```babbelaar
structuur Persoon {
    veld voornaam: Slinger,
    veld achternaam: Slinger,

    werkwijze zegGedag() {
        schrijf(€"Hallo, ik ben {dit.voornaam} {dit.achternaam}");
    }
}

stel anoniemePersoon = Persoon {
    voornaam: "John",
    achternaam: "Doe",
};

anoniemePersoon.zegGedag();
```
### Uitleg
`dit` in deze context verwijst naar de `Persoon`, en als `zegGedag` aangeroepen wordt, is `dit` hetzelfde als de `anoniemePersoon`.

### Uitvoer
De uitvoer is als volgt:
```
Hallo, ik ben John Doe
```
"#,
            Self::In => "Herhaal over een stel waardes met `volg`.",
            Self::Koppelvlak => "Definieer een nieuw koppelvlak, een soort contract voor structuren.",
            Self::Nieuw => "Maak een nieuw object aan.",
            Self::Onwaar => "Een waarde van het type `booleaan`. Tegenovergestelde van `waar`",
            Self::Op => "Op welke structuur moet de het koppelvlak uitgebreid worden.",
            Self::Reeks => {
                r#"Stel een reeks op van getallen.
## Voorbeeld
```babbelaar
volg i in reeks(0, 10) {
    // Gebruik het getal tussen 0 en 10.
}
```"#
            }
            Self::Stel => "Bepaal een aanpaswaarde in deze scoop",
            Self::Structuur => "Definieer een datastructuur.",
            Self::Uitbreiding => "Definieer een uitbreiding op een datastructuur.",
            Self::Veld => "Een onderdeel van een `structuur`.",
            Self::Volg => "Herhaal de sectie per waarde van de reeks.",
            Self::Waar => "Een waarde van het type `booleaan`. Tegenovergestelde van `onwaar`",
            Self::Werkwijze => "Definieer een nieuwe werkwijze.",
        }.into()
    }
}
