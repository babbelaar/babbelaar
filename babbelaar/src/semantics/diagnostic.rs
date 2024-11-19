// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{error::Error, fmt::Display};

use strum::AsRefStr;
use thiserror::Error;

use crate::{BabString, BabbelaarCodeAction, Expression, FileRange};

use super::{SemanticRelatedInformation, SemanticType};

#[derive(Debug, Clone)]
pub struct SemanticDiagnostic {
    range: FileRange,
    kind: SemanticDiagnosticKind,
    severity: SemanticDiagnosticSeverity,
    related: Vec<SemanticRelatedInformation>,
    actions: Vec<BabbelaarCodeAction>,
}

impl Display for SemanticDiagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl Error for SemanticDiagnostic {
}

impl SemanticDiagnostic {
    #[must_use]
    pub fn new(range: FileRange, kind: SemanticDiagnosticKind) -> Self {
        Self {
            range,
            kind,
            severity: SemanticDiagnosticSeverity::Error,
            related: Vec::new(),
            actions: Vec::new(),
        }
    }

    #[must_use]
    pub fn range(&self) -> FileRange {
        self.range
    }

    #[must_use]
    pub fn kind(&self) -> &SemanticDiagnosticKind {
        &self.kind
    }

    #[must_use]
    pub fn related_info(&self) -> &[SemanticRelatedInformation] {
        &self.related
    }

    #[must_use]
    pub fn with_related(mut self, info: impl Into<Option<SemanticRelatedInformation>>) -> Self {
        if let Some(info) = info.into() {
            self.related.push(info);
        }

        self
    }

    #[must_use]
    pub fn actions(&self) -> &[BabbelaarCodeAction] {
        &self.actions
    }

    #[must_use]
    pub fn with_action(mut self, action: impl Into<Option<BabbelaarCodeAction>>) -> Self {
        if let Some(action) = action.into() {
            self.actions.push(action);
        }

        self
    }

    #[must_use]
    pub fn with_actions(mut self, action: impl AsRef<[BabbelaarCodeAction]>) -> Self {
        self.actions.extend_from_slice(action.as_ref());
        self
    }

    #[must_use]
    pub fn severity(&self) -> SemanticDiagnosticSeverity {
        self.severity
    }

    #[must_use]
    pub fn warn(mut self) -> Self {
        self.severity = SemanticDiagnosticSeverity::Warning;
        self
    }
}

#[derive(Debug, Clone, Error, AsRefStr)]
#[strum(serialize_all = "kebab-case")]
pub enum SemanticDiagnosticKind {
    #[error("Expressie is geen geldige toewijzing: `{expression:?}`")]
    ExpressionCannotBeUsedAsAssignmentDestination {
        expression: Expression,
    },

    #[error("Werkwijze `{name}` bestaat niet.")]
    InvalidFunctionReference { name: BabString },

    #[error("Kon waarde `{identifier}` niet vinden binnen deze scoop.")]
    InvalidIdentifierReference { identifier: BabString },

    #[error("`_` kan alleen gebruikt worden om een bepaalde waarde weg te gooien.")]
    DiscardingIdentifierUsedAsReference,

    #[error("Te weinig argumenten gegeven aan werkwijze `{function_name}` ({arg_count} gegeven maar {param_count} verwacht)")]
    TooFewArguments {
        function_name: BabString,
        param_count: usize,
        arg_count: usize,
    },

    #[error("Te veel argumenten gegeven aan werkwijze `{function_name}` ({arg_count} gegeven maar {param_count} verwacht)")]
    TooManyArguments {
        function_name: BabString,
        param_count: usize,
        arg_count: usize,
    },

    #[error("Type `{name}` is onbekend")]
    UnknownType {
        name: BabString,
    },

    #[error("Types `{lhs_type}` en `{rhs_type}` zijn niet gelijksoortig.")]
    IncompatibleTypes {
        lhs_type: SemanticType,
        rhs_type: SemanticType,
    },

    #[error("Ongeldig argument gegeven voor werkwijze: argument van type `{argument_type}` is niet gelijksoortig met parameter van type `{parameter_type}`.")]
    IncompatibleArgumentParameterType {
        argument_type: SemanticType,
        parameter_type: SemanticType,
    },

    #[error("Lid `{name}` bestaat niet binnen type `{typ}`")]
    InvalidMember {
        typ: SemanticType,
        name: BabString,
    },

    #[error("Methode `{name}` bestaat niet binnen type `{typ}`")]
    InvalidMethod {
        typ: SemanticType,
        name: BabString,
    },

    #[error("Type `{typ}` is een werkwijze, en kan geen methodes bevatten.")]
    FunctionCannotHaveMethod {
        typ: SemanticType,
        name: BabString,
    },

    #[error("Attribuut `{name}` is onbekend")]
    UnknownAttribute { name: BabString, range: FileRange },

    #[error("Veldnaam `{name}` wordt meerdere keren gebruikt")]
    DuplicateFieldName { name: BabString },

    #[error("Werkwijzenaam `{name}` in koppelvlak `{interface}` wordt meerdere keren gebruikt")]
    DuplicateMethodNameInInterface { name: BabString, interface: BabString },

    #[error("Werkwijzenaam `{name}` in structuur `{structure}` wordt meerdere keren gebruikt")]
    DuplicateMethodNameInStructure { name: BabString, structure: BabString },

    #[error("Veld met naam `{name}` wordt meerdere keren een waarde toegekend")]
    DuplicateFieldInstantiation { name: BabString },

    #[error("Structuur `{struct_name}` heeft geen veld genaamd `{field_name}`")]
    InvalidFieldInstantiation {
        struct_name: BabString,
        field_name: BabString,
    },

    #[error("Ongeldige waarde gegeven voor veld `{field_name}` in structuur `{struct_name}`. Veldtype `{declaration_type}` is niet gelijksoortig met definitie van `{definition_type}`.")]
    IncompatibleFieldTypes {
        struct_name: BabString,
        field_name: BabString,
        declaration_type: String,
        definition_type: String,
    },

    #[error("Pure waarde van type `{ty}` ongebruikt. Stelling heeft geen gevolg.")]
    UnusedPureValue { ty: BabString },

    #[error("`dit` kan uitsluitend gebruikt worden binnen een werkwijze van een `structuur`")]
    ThisOutsideStructure,

    #[error("{field_word} `{names}` {verb} een toewijzing")]
    MissingFieldInitializers {
        names: String,
        field_word: &'static str,
        verb: &'static str,
    },

    #[error("De naam van de uitheemse werkwijze is meerdere keren geven.")]
    AttributeExternDuplicateName,

    #[error("Onbekende argument voor het @uitheems-attribuut.")]
    AttributeExternInvalidArgument,

    #[error("Een uitheemse werkwijze vereist een `naam` argument.")]
    AttributeExternRequiresName,

    #[error("Het attribuut `@uitheems` kan alleen gebruikt worden op werkwijzen.")]
    AttributeExternOnlyOnFunctions,

    #[error("Het attribuut `@uitheems` kan alleen gebruikt worden op werkwijzen zonder lichaam {{ .. }}")]
    AttributeExternOnlyOnFunctionsWithoutBody,

    #[error("De naam van werkwijzeattribuut `@uitheems` moet een slinger zijn.")]
    AttributeExternNameMustBeString,

    #[error("De naam van werkwijzeattribuut `@uitheems` moet een niet-lege slinger zijn.")]
    AttributeExternNameMustBeNonEmpty,

    #[error("Onbekend argument `{name}` is niet toegestaan binnen attribuut `@uitheems`")]
    AttributeExternUnexpectedArgument { name: BabString },

    #[error("Attribuut `@uitheems` kan maar één keer gebruikt worden per werkwijzen.")]
    AttributeExternOnlyOnce,

    #[error("De werkwijze genaamd `{name}` is meerdere keren gedefinieerd.")]
    DuplicateFunction { name: BabString },

    #[error("`bekeer` verwacht een waarde van type `{typ}`")]
    ReturnStatementExpectedValue { typ: BabString },

    #[error("`bekeer` verwacht geen waarde")]
    ReturnStatementExpectedNoValue,

    #[error("`bekeer` verwachtte een waarde van type `{expected}`, maar expressie is van type `{actual}`")]
    ReturnStatementIncompatibleTypes {
        expected: SemanticType,
        actual: SemanticType,
    },

    #[error("Parameternaam `{name}` wordt meerdere keren gedefinieerd.")]
    DuplicateParameterName {
        name: BabString,
    },

    #[error("Werkwijze `{name}` wordt nergens gebruikt.")]
    UnusedFunction { name: BabString },

    #[error("Iterator `{name}` wordt nergens gebruikt.")]
    UnusedIterator { name: BabString },

    #[error("Parameter `{name}` wordt nergens gebruikt.")]
    UnusedParameter { name: BabString },

    #[error("Variabele `{name}` wordt nergens gebruikt.")]
    UnusedVariable { name: BabString },

    #[error("Expressie resulteert niet in een getal, wat nodig is om de grootte van de opeenvolging te bepalen.")]
    SizedArrayInitializerInvalidSize,

    #[error("Een `{ty}` kan niet worden geïndexeerd. Types zoals `Slinger` en opeenvolgingen wel.")]
    CannotSubscriptNonArray { ty: SemanticType },

    #[error("Je kunt opeenvolgingen alleen met getallen indexeren, maar de index is van type {ty}.")]
    CannotIndexArrayWithNonInteger { ty: SemanticType },

    #[error("Toewijzingsbron en -bestemming zijn niet van hetzelfde type.")]
    IncompatibleAssignmentTypes,

    #[error("{name} moet van het type `g32` zijn, maar dit is een `{ty}`")]
    RangeExpectsInteger { name: &'static str, ty: SemanticType },

    #[error("Kan deze waarde niet gebruiken als een doorloper, gebruik een opeenvolging of `reeks`.")]
    ExpressionNotIterable,

    #[error("Deze structuur is niet uitgebreid met het `Doorloper`-koppelvlak. Implementeer dit, gebruik een opeenvolging of `reeks`.")]
    ExpressionNotIterableStructure,

    #[error("Attribuut `@{name}` verwacht geen argumenten.")]
    AttributeCannotHaveArguments { name: &'static str },

    #[error("Attribuut `@{name}` kan alleen gebruikt worden op werkwijzen die `@uitheems` zijn.")]
    AttributeCanOnlyBeUsedOnExternFunctions { name: &'static str },

    #[error("Type `{name}` kan niet uitgebreid worden.")]
    TypeCannotBeExtended { name: BabString },

    #[error("Werkwijzenaam `{name}` bestaat al in structuur `{structure}`")]
    DuplicateMethodNameInExtension { name: BabString, structure: BabString },

    #[error("Bij type `{ty}` waren geen generieke typen verwacht")]
    TypeParametersUnexpected { ty: BabString },

    #[error("Te weinig generieke parameters meegegeven aan `{ty}`")]
    TooFewGenericTypes { ty: BabString },

    #[error("Te veel generieke parameters meegegeven aan `{ty}`")]
    TooManyGenericTypes { ty: BabString },

    #[error("Koppelvlak `{name}` is onbekend")]
    UnknownInterface { name: BabString },

    #[error("werkwijze{} {names} {}", if *count == 1 { "" } else { "n" }, if *count == 1 { "ontbreekt" } else { "ontbreken" })]
    MissingMethodsInInterfaceExtension { names: String, count: usize },

    #[error("Koppelvlak `{interface}` heeft geen werkwijze opgesteld met naam `{name}`")]
    InvalidInterfaceExtensionMethod { name: BabString, interface: BabString },

    #[error("Werkwijze in koppelvlak heeft parameter `{expected}`, maar deze is van type `{actual}`")]
    InterfaceDeclarationHasDifferentParameters { expected: String, actual: String },

    #[error("Werkwijze in koppelvlak heeft bekeertype `{expected}`, maar deze is van type `{actual}`")]
    InterfaceDeclarationHasDifferentReturnType { expected: SemanticType, actual: SemanticType },

    #[error("Werkwijze in koppelvlak heeft {expected} parameter{}, maar deze werkwijze heeft {actual} parameter{}", if *expected == 1 { "" } else { "s" }, if *actual == 1 { "" } else { "s" })]
    TooManyParametersForInterfaceMethod { expected: usize, actual: usize },

    #[error("Werkwijze in koppelvlak heeft {expected} parameter{}, maar deze werkwijze heeft {actual} parameter{}", if *expected == 1 { "" } else { "s" }, if *actual == 1 { "" } else { "s" })]
    TooFewParametersForInterfaceMethod { expected: usize, actual: usize },

    #[error("Kan alleen getallen negatief keren")]
    CannotNegateNonInteger,

    #[error("Kan alleen het adres nemen van een lokale variabele")]
    CannotTakeAddressOfNonIdentifier,
}

impl SemanticDiagnosticKind {
    pub fn name(&self) -> &str {
        self.as_ref()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticDiagnosticSeverity {
    Error,
    Warning,
}

impl Display for SemanticDiagnosticSeverity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Error => f.write_str("Fout"),
            Self::Warning => f.write_str("Waarschuwing"),
        }
    }
}

#[derive(Default, Debug)]
pub struct SemanticDiagnosticsList {
    contents: Option<Vec<SemanticDiagnostic>>,
}

impl SemanticDiagnosticsList {
    pub fn new(should_produce_diagnostics: bool) -> Self {
        Self {
            contents: if should_produce_diagnostics {
                Some(Vec::new())
            } else {
                None
            }
        }
    }

    #[inline]
    pub fn create<F: FnOnce() -> SemanticDiagnostic>(&mut self, f: F) {
        let Some(contents) = &mut self.contents else { return };

        contents.push(f());
    }

    pub fn to_vec(self) -> Vec<SemanticDiagnostic> {
        self.contents.unwrap_or_default()
    }

    pub fn as_slice(&self) -> &[SemanticDiagnostic] {
        self.contents.as_ref().map(|x| x.as_slice()).unwrap_or_default()
    }
}
