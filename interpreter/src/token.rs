// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::Keyword;

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

#[derive(Clone, Debug, PartialEq)]
pub struct Token<'source_code> {
    pub kind: TokenKind<'source_code>,
    pub begin: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TemplateStringToken<'source_code> {
    Plain {
        begin: usize,
        end: usize,
        str: &'source_code str,
    },
    Expression(Vec<Token<'source_code>>),
}
