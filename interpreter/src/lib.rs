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
mod statement;
mod token;
mod util;
mod value;

pub use self::{
    builtin::Builtin,
    expression::*,
    interpreter::Interpreter,
    keyword::Keyword,
    lexer::Lexer,
    parser::{Parser, ParseError},
    scope::Scope,
    statement::{ForStatement, FunctionStatement, Statement},
    token::{TemplateStringToken, Token, TokenKind},
    util::{FileLocation, StringExt},
    value::Value,
};
