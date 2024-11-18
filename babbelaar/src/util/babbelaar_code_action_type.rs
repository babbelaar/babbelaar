// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use thiserror::Error;

use crate::BabString;

use super::BabbelaarCommand;

#[derive(Debug, Clone, Error)]
pub enum BabbelaarCodeActionType {
    #[error("Sleutelwoord `{keyword}` toevoegen")]
    AddKeyword { keyword: &'static str, },

    #[error("Voeg parameter{} toe",  if *residual_args == 1 { "" } else { "s" })]
    AddParameter {
        residual_args: usize,
    },

    #[error("Bekeertype `{typ}` toevoegen aan werkwijze")]
    AddReturnType {
        typ: BabString,
    },

    #[error("Wijs waarde toe aan een nieuwe variabele")]
    AssignToNewVariable,

    #[error("Hernoem naar `_{name}` om het probleem te negeren")]
    AppendUnderscoreToName {
        name: BabString,
    },

    #[error("Verander bekeertype naar `{typ}`")]
    ChangeReturnType { typ: BabString },

    #[error("Zet Slinger `\"{number}\"` om naar getal `{number}`")]
    ChangeStringToNumber { number: isize },

    #[error("Maak veld `{name}` met type `{ty}` aan")]
    CreateField { name: String, ty: BabString },

    #[error("Maak veld `{name}` met generiek parametertype `{ty}` aan")]
    CreateFieldGeneric { name: String, ty: BabString },

    #[error("Maak werkwijze `{name}` aan binnen structuur `{structure}`")]
    CreateMethod { name: BabString, structure: BabString },

    #[error("Breid `{structure}` uit met werkwijze `{name}`{}", if *is_explicitly_new { " in nieuwe uitbreiding" } else { "" })]
    CreateMethodExtension {
        name: BabString,
        structure: BabString,
        is_explicitly_new: bool,
    },

    #[error("Maak werkwijze `{name}` aan")]
    CreateFunction { name: BabString },

    #[error("Vul structuurvelden van `{structure}`")]
    FillStructureFields { structure: String },

    #[error("Werkwijzelichaam aanmaken met `{{` en `}}`")]
    StartFunctionBody,

    #[error("`{text}` invoegen")]
    Insert { text: &'static str, },

    #[error("Extra {} verwijderen", if *residual_args == 1 { "argument" } else { "argumenten" })]
    RemoveArgument {
        residual_args: usize,
    },

    #[error("Verwijder overbodige generieke parameters")]
    RemoveExtraneousGenericTypes,

    #[error("Verwijder generieke parameters")]
    RemoveGenericParameters,

    #[error("Verwijder puur statement")]
    RemovePureStatement,

    #[error("Verwijder extra tekens")]
    RemoveResidualTokens,

    #[error("Verwijder attribute `@{name}`")]
    RemoveAttribute { name: BabString },

    #[error("Haal bekeerwaarde weg")]
    RemoveExpression,

    #[error("Haal bekeerwaarde weg")]
    RemoveReturnType,

    #[error("{0}")]
    Command(BabbelaarCommand),

    #[error("Verwijder {kind} `{name}`")]
    RemoveGenericStatement {
        kind: &'static str,
        name: BabString,
    },

    #[error("Gebruik methode `{method_name}`")]
    UseMethod { method_name: BabString },

    #[error("Maak structuur `{name}` aan")]
    CreateStructure { name: BabString },

    #[error("Maak structuur `{name}` aan in nieuw bestand")]
    CreateStructureInNewFile { name: BabString },

    #[error("Verplaats structuur naar nieuw bestand")]
    MoveStructureToNewFile,

    #[error("Verplaats koppelvlak naar nieuw bestand")]
    MoveInterfaceToNewFile,

    #[error("Breid `{structure}` uit met koppelvlak `{interface}`")]
    ExtendStructureWithInterface {
        structure: BabString,
        interface: BabString,
    },
}
