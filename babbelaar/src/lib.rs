// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#![deny(elided_lifetimes_in_paths)]

mod attribute;
mod builtin;
mod constants;
mod expression;
mod interpreter;
mod keyword;
mod lexer;
mod parser;
mod semantics;
mod statement;
mod string;
mod structure;
mod token;
mod tree;
mod type_;
mod util;
mod value;

pub use self::{
    attribute::{
        Attribute,
        AttributeArgument,
        AttributeList,
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
    constants::Constants,
    expression::{
        BiExpression,
        BiOperator,
        Comparison,
        Expression,
        FieldInstantiation,
        FunctionCallExpression,
        MathOperator,
        MethodCallExpression,
        PostfixExpression,
        PostfixExpressionKind,
        PrimaryExpression,
        RangeExpression,
        StructureInstantiationExpression,
        TemplateStringExpressionPart,
        UnaryExpression,
        UnaryExpressionKind,
    },
    interpreter::Interpreter,
    keyword::Keyword,
    lexer::{
        Lexer,
        LexerError,
        LexerErrorKind,
    },
    parser::{
        Parser,
        ParseDiagnostic,
        ParseError,
    },
    semantics::{
        SemanticAnalysisPhase,
        SemanticAnalyzer,
        SemanticDiagnostic,
        SemanticDiagnosticKind,
        SemanticDiagnosticSeverity,
        SemanticLocal,
        SemanticLocalKind,
        SemanticReference,
        SemanticScope,
        SemanticStructure,
        SemanticType,
    },
    statement::{
        AssignStatement,
        ExtensionStatement,
        ForIterableKind,
        ForStatement,
        FunctionStatement,
        IfStatement,
        ReturnStatement,
        Statement,
        StatementKind,
        VariableStatement
    },
    string::{
        BabString,
        IntoBabString,
        Slice,
    },
    structure::{
        Field,
        InterfaceStatement,
        Method,
        Structure,
    },
    token::{
        Punctuator,
        TemplateStringToken,
        Token,
        TokenKind
    },
    tree::ParseTree,
    type_::{
        InterfaceSpecifier,
        Parameter,
        Type,
        TypeQualifier,
        TypeSpecifier,
    },
    util::{
        BabbelaarCodeAction,
        BabbelaarCodeActionType,
        BabbelaarCommand,
        BabbelaarFixKind,
        DocumentationProvider,
        FileEdit,
        FileId,
        FileLocation,
        FileRange,
        LspCompletion,
        OptionExt,
        Ranged,
        SourceCode,
        StrExt,
        StrIterExt,
    },
    value::{
        ExtensionId,
        FunctionId,
        InterfaceId,
        MethodId,
        MethodOwnerId,
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
