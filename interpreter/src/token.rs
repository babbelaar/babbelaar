// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{FileLocation, Keyword};

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind<'source_code> {
    Keyword(Keyword),

    Identifier(&'source_code str),
    StringLiteral(&'source_code str),
    TemplateString(Vec<TemplateStringToken<'source_code>>),
    Integer(i64),

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

impl<'source_code> TokenKind<'source_code> {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Keyword(..) => "Keyword",

            Self::Identifier(..) => "Identifier",
            Self::StringLiteral(..) => "StringLiteral",
            Self::TemplateString(..) => "TemplateString",
            Self::Integer(..) => "Integer",

            Self::Comma => "Comma",
            Self::LeftParenthesis => "LeftParenthesis",
            Self::RightParenthesis => "RightParenthesis",
            Self::LeftCurlyBracket => "LeftCurlyBracket",
            Self::RightCurlyBracket => "RightCurlyBracket",
            Self::LeftSquareBracket => "LeftSquareBracket",
            Self::RightSquareBracket => "RightSquareBracket",
            Self::Semicolon => "Semicolon",
            Self::PlusSign => "PlusSign",
            Self::EqualsSign => "EqualsSign",
            Self::HyphenMinus => "HyphenMinus",
            Self::Solidus => "Solidus",
            Self::Asterisk => "Asterisk",
            Self::PercentageSign => "PercentageSign",
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token<'source_code> {
    pub kind: TokenKind<'source_code>,
    pub begin: FileLocation,
    pub end: FileLocation,
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
