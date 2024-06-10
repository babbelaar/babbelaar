// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use strum::IntoStaticStr;

use crate::{FileLocation, FileRange, Keyword};

#[derive(Debug, Clone, Copy, PartialEq, Eq, IntoStaticStr)]
pub enum Punctuator {
    Colon,
    Comma,
    LeftParenthesis,
    RightParenthesis,
    LeftCurlyBracket,
    RightCurlyBracket,
    LeftSquareBracket,
    RightSquareBracket,
    Semicolon,
    PlusSign,
    EqualsSign,
    HyphenMinus,
    Solidus,
    Asterisk,
    PercentageSign,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind<'source_code> {
    Keyword(Keyword),

    Identifier(&'source_code str),
    StringLiteral(&'source_code str),
    TemplateString(Vec<TemplateStringToken<'source_code>>),
    Integer(i64),

    Punctuator(Punctuator),
    IllegalCharacter(char),
}

impl<'source_code> TokenKind<'source_code> {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Keyword(..) => "Keyword",

            Self::Identifier(..) => "Identifier",
            Self::StringLiteral(..) => "StringLiteral",
            Self::TemplateString(..) => "TemplateString",
            Self::Integer(..) => "Integer",

            Self::Punctuator(punctuator) => punctuator.into(),
            Self::IllegalCharacter(..) => "InvalidCharacter",
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token<'source_code> {
    pub kind: TokenKind<'source_code>,
    pub begin: FileLocation,
    pub end: FileLocation,
}

impl<'source_code> Token<'source_code> {
    pub fn range(&self) -> FileRange {
        (self.begin, self.end).into()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TemplateStringToken<'source_code> {
    Plain {
        begin: FileLocation,
        end: FileLocation,
        str: &'source_code str,
    },
    Expression(Vec<Token<'source_code>>),
}
