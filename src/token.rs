// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::Keyword;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenKind<'source_code> {
    Keyword(Keyword),

    Identifier(&'source_code str),
    StringLiteral(&'source_code str),
    Integer(i64),

    Comma,
    LeftParenthesis,
    RightParenthesis,
    Semicolon,
    PlusSign,
    EqualsSign,
    HyphenMinus,
    Solidus,
    Asterisk,
    PercentageSign,
}

#[derive(Copy, Clone, Debug)]
pub struct Token<'source_code> {
    pub kind: TokenKind<'source_code>,
    pub begin: usize,
    pub end: usize,
}
