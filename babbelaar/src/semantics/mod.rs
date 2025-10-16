// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod analysis;
mod context;
mod diagnostic;
mod extension;
mod function_reference;
mod function;
mod interface;
mod local;
mod method;
mod reference;
mod related;
mod scope;
mod analyzer;
mod structure_or_interface;
mod structure;
mod type_;
mod value;

pub use self::{
    analysis::{
        SemanticAnalysisPhase,
        SemanticFunctionAnalysis,
    },
    analyzer::SemanticAnalyzer,
    context::SemanticContext,
    diagnostic::{
        SemanticDiagnostic,
        SemanticDiagnosticKind,
        SemanticDiagnosticSettings,
        SemanticDiagnosticSeverity,
        SemanticDiagnosticsList,
    },
    extension::{
        SemanticExtension,
        SemanticExtensionId,
    },
    function_reference::FunctionReference,
    function::{
        SemanticExternFunction,
        SemanticFunction,
        SemanticParameter,
    },
    interface::SemanticInterface,
    local::{
        SemanticLocal,
        SemanticLocalKind,
    },
    method::SemanticMethod,
    reference::SemanticReference,
    related::{
        SemanticRelatedInformation,
        SemanticRelatedMessage,
    },
    scope::{
        SemanticScope,
        SemanticScopeId,
        SemanticScopeKind,
    },
    structure_or_interface::StructureOrInterface,
    structure::{
        SemanticField,
        SemanticFieldId,
        SemanticStructure,
    },
    type_::{
        SemanticGenericType,
        SemanticType,
        SemanticTypeResolution,
    },
    value::{
        PureValue,
        SemanticUsage,
        SemanticValue,
    },
};
