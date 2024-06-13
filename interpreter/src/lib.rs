// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#![deny(elided_lifetimes_in_paths)]

mod builtin;
mod expression;
mod interpreter;
mod keyword;
mod lexer;
mod parser;
mod scope;
mod semantics;
mod statement;
mod token;
mod type_;
mod util;
mod value;

pub use self::{
    builtin::{Builtin, BuiltinFunction, BuiltinType},
    expression::*,
    interpreter::Interpreter,
    keyword::Keyword,
    lexer::Lexer,
    parser::{Parser, ParseError},
    scope::Scope,
    semantics::{SemanticAnalyzer, SemanticDiagnostic, SemanticDiagnosticKind},
    statement::{ForStatement, FunctionStatement, IfStatement, ReturnStatement, Statement, StatementKind},
    token::{Punctuator, TemplateStringToken, Token, TokenKind},
    type_::{Parameter, Type, TypeSpecifier},
    util::{DocumentationProvider, FileLocation, FileRange, LspCompletion, Ranged, StringExt},
    value::Value,
};
