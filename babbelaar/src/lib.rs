// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#![deny(elided_lifetimes_in_paths)]

mod attribute;
mod builtin;
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
    attribute::{Attribute, AttributeArgument},
    builtin::{ArrayMethod, ArrayMethodParameter, ArrayTypeRef, Builtin, BuiltinFunction, BuiltinMethodReference, BuiltinType},
    expression::*,
    interpreter::Interpreter,
    keyword::Keyword,
    lexer::{Lexer, LexerError, LexerErrorKind},
    parser::{Parser, ParseDiagnostic, ParseError},
    semantics::{SemanticAnalyzer, SemanticDiagnostic, SemanticDiagnosticKind, SemanticDiagnosticSeverity, SemanticLocal, SemanticLocalKind, SemanticReference, SemanticScope, SemanticStructure, SemanticType},
    statement::{AssignStatement, ForIterableKind, ForStatement, FunctionStatement, IfStatement, ReturnStatement, Statement, StatementKind, VariableStatement},
    string::{BabString, IntoBabString, Slice},
    structure::{Field, Method, Structure},
    token::{Punctuator, TemplateStringToken, Token, TokenKind},
    tree::ParseTree,
    type_::{Parameter, Type, TypeQualifier, TypeSpecifier},
    util::{BabbelaarCodeAction, BabbelaarCodeActionType, BabbelaarCommand, BabbelaarFixKind, DocumentationProvider, FileEdit, FileId, FileLocation, FileRange, LspCompletion, OptionExt, Ranged, SourceCode, StrExt, StrIterExt},
    value::{FunctionId, MethodId, StructureId, Value, ValueType},
};
