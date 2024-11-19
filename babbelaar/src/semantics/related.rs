// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use strum::AsRefStr;
use thiserror::Error;

use crate::{BabString, FileRange};

use super::SemanticType;

#[derive(Debug, Clone)]
pub struct SemanticRelatedInformation {
    range: FileRange,
    message: SemanticRelatedMessage,
}

impl SemanticRelatedInformation {
    #[must_use]
    pub fn new(range: FileRange, message: SemanticRelatedMessage) -> Self {
        Self {
            range,
            message: message.into(),
        }
    }

    #[must_use]
    pub fn range(&self) -> FileRange {
        self.range
    }

    #[must_use]
    pub fn message(&self) -> &SemanticRelatedMessage {
        &self.message
    }
}

#[derive(Debug, Clone, Error, AsRefStr)]
pub enum SemanticRelatedMessage {
    #[error("Bestemming is van type `{ty}`")]
    DestinationOfType { ty: SemanticType },

    #[error("`{name}` is hier voor het eerst geïnitialiseerd")]
    DuplicateFieldFirstUse { name: BabString },

    #[error("werkwijze `{name}` is hier voor het eerst aangemaakt")]
    DuplicateMethodFirstDefinedHere { name: BabString },

    #[error("expressie is van het type `{ty}`")]
    ExpressionIsOfType { ty: SemanticType },

    #[error("veld `{name}` is hier gedefinieerd")]
    FieldDefinedHere { name: BabString },

    #[error("werkwijze `{name}` is hier gedefinieerd")]
    FunctionDefinedHere { name: BabString },

    #[error("koppelvlak `{name}` is hier gedefinieerd")]
    InterfaceDefinedHere { name: BabString },

    #[error("parameter `{name}` is hier gedeclareerd")]
    ParameterDeclaredHere { name: BabString },

    #[error("structuur `{name}` is hier gedefinieerd")]
    StructureDefinedHere { name: BabString },

    #[error("werkwijze zit niet in een structuur")]
    WerkwijzeNotInsideStructuur,

    #[error("los gebruik van het veld `{name}` heeft geen effect")]
    UsageOfPureValueField { name: BabString },

    #[error("bekeertype `{typ}` is hier gedefinieerd")]
    ReturnTypeDefinedHere { typ: BabString },

    #[error("Bronwaarde is van type `{ty}`")]
    SourceOfType { ty: SemanticType },

    #[error("type `{typ}` is hier gedefinieerd")]
    TypeDefinedHere { typ: BabString },
}
