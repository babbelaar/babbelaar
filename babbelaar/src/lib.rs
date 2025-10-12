// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#![deny(elided_lifetimes_in_paths)]

mod ast;
mod builtin;
mod config;
mod constants;
mod interpreter;
mod lexer;
mod semantics;
mod util;

pub use self::{
    ast::{
        AssignKind,
        AssignStatement,
        Attribute,
        AttributeArgument,
        AttributeName,
        AttributeList,
        BiExpression,
        BiOperator,
        Comparison,
        ComputedPathExpression,
        Expression,
        ExtensionStatement,
        Field,
        FieldInstantiation,
        ForIterableKind,
        ForStatement,
        FunctionCallExpression,
        FunctionStatement,
        IfStatement,
        InterfaceSpecifier,
        InterfaceStatement,
        MathOperator,
        Method,
        MethodCallExpression,
        Parameter,
        ParseDiagnostic,
        ParseError,
        Parser,
        ParseTree,
        PathExpression,
        PostfixExpression,
        PostfixExpressionKind,
        PrimaryExpression,
        RangeExpression,
        ReturnStatement,
        Statement,
        StatementKind,
        Structure,
        StructureInstantiationExpression,
        TemplateStringExpressionPart,
        Type,
        TypeQualifier,
        TypeSpecifier,
        UnaryExpression,
        UnaryExpressionKind,
        VariableStatement
    },
    builtin::{
        ArrayMethod,
        ArrayMethodParameter,
        ArrayTypeRef,
        Builtin,
        BuiltinFunction,
        BuiltinMethodReference,
        BuiltinType,
    },
    config::{
        ArchiveKind,
        ConfigRoot,
        ConfigSectionArch,
        ConfigSectionArchAarch64,
        ConfigSectionBuild,
        ConfigSectionLog,
        ConfigSectionProject,
    },
    constants::Constants,
    interpreter::Interpreter,
    lexer::{
        Keyword,
        Lexer,
        LexerError,
        LexerErrorKind,
        Punctuator,
        TemplateStringToken,
        Token,
        TokenKind,
    },
    semantics::{
        SemanticAnalysisPhase,
        SemanticAnalyzer,
        SemanticDiagnostic,
        SemanticDiagnosticKind,
        SemanticDiagnosticSettings,
        SemanticDiagnosticSeverity,
        SemanticLocal,
        SemanticLocalKind,
        SemanticReference,
        SemanticScope,
        SemanticScopeKind,
        SemanticStructure,
        SemanticType,
    },
    util::{
        BabbelaarCodeAction,
        BabbelaarCodeActionType,
        BabbelaarCommand,
        BabbelaarFixKind,
        BabString,
        DocumentationProvider,
        ExtensionId,
        FileEdit,
        FileId,
        FileLocation,
        FileRange,
        FunctionId,
        InterfaceId,
        IntoBabString,
        LspCompletion,
        MethodId,
        MethodOwnerId,
        OptionExt,
        Ranged,
        Slice,
        SourceCode,
        StrExt,
        StrIterExt,
        StructureId,
        Value,
        ValueType,
    },
};

use std::{
    collections::HashMap,
    error::Error,
    path::PathBuf,
};

pub fn parse_string_to_tree(text: &str) -> Result<ParseTree, Box<dyn Error>> {
    let source_code = SourceCode::new(PathBuf::new(), 0, BabString::new(text));
    let (tokens, errors) = Lexer::new(&source_code).collect_all();
    if !errors.is_empty() {
        return Err(Box::new(errors[0].clone()));
    }

    let mut parser = Parser::new(source_code.path().to_path_buf(), &tokens);
    let tree = parser.parse_tree();

    let errors =  parser.into_diagnostics();
    if !errors.is_empty() {
        return Err(Box::new(errors[0].clone()));
    }

    let mut files = HashMap::new();
    files.insert(source_code.file_id(), source_code);

    let mut sema = SemanticAnalyzer::new(files, true);
    for phase in SemanticAnalysisPhase::iter() {
        sema.analyze_tree(&tree, phase);
    }

    let errors = sema.into_diagnostics();
    if !errors.is_empty() {
        return Err(Box::new(errors[0].clone()));
    }

    Ok(tree)
}
