// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#![deny(elided_lifetimes_in_paths)]

mod attribute;
mod builtin;
mod context;
mod expression;
mod interpreter;
mod keyword;
mod lexer;
mod parser;
mod semantics;
mod statement;
mod token;
mod tree;
mod type_;
mod util;
mod value;

pub use self::{
    attribute::{Attribute, AttributeArgument},
    builtin::{Builtin, BuiltinFunction, BuiltinType},
    context::BabbelaarContext,
    expression::*,
    interpreter::Interpreter,
    keyword::Keyword,
    lexer::Lexer,
    parser::{Parser, ParseDiagnostic},
    semantics::{SemanticAnalyzer, SemanticDiagnostic, SemanticDiagnosticKind, SemanticLocalKind, SemanticReference, SemanticType},
    statement::{ForStatement, FunctionStatement, IfStatement, ReturnStatement, Statement, StatementKind, VariableStatement},
    token::{Punctuator, TemplateStringToken, Token, TokenKind},
    tree::ParseTree,
    type_::{Parameter, Type, TypeSpecifier},
    util::{DocumentationProvider, FileLocation, FileRange, LspCompletion, OptionExt, Ranged},
    value::{FunctionId, Value},
};
