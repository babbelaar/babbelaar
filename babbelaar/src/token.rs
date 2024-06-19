// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::{Display, Formatter, Write};

use strum::IntoStaticStr;

use crate::{FileLocation, FileRange, Keyword};

#[derive(Debug, Clone, Copy, PartialEq, Eq, IntoStaticStr)]
pub enum Punctuator {
    #[strum(serialize = "dubbele punt")]
    Colon,
    #[strum(serialize = "komma")]
    Comma,
    #[strum(serialize = "open rond haakje")]
    LeftParenthesis,
    #[strum(serialize = "gesloten rond haakje")]
    RightParenthesis,
    #[strum(serialize = "open accolade")]
    LeftCurlyBracket,
    #[strum(serialize = "gesloten accolade")]
    RightCurlyBracket,
    #[strum(serialize = "open blokhaakje")]
    LeftSquareBracket,
    #[strum(serialize = "gesloten blokhaakje")]
    RightSquareBracket,
    #[strum(serialize = "puntkomma")]
    Semicolon,
    #[strum(serialize = "plus")]
    PlusSign,
    #[strum(serialize = "aanwijzing")]
    Assignment,
    #[strum(serialize = "vergelijking")]
    Equals,
    #[strum(serialize = "min")]
    HyphenMinus,
    #[strum(serialize = "schuine streep")]
    Solidus,
    #[strum(serialize = "sterretje")]
    Asterisk,
    #[strum(serialize = "procent")]
    PercentageSign,
    #[strum(serialize = "punt")]
    Period,
}

impl Punctuator {
    #[must_use]
    pub const fn as_str(&self) -> &str {
        match self {
            Self::Colon => ":",
            Self::Comma => ",",
            Self::LeftParenthesis => "(",
            Self::RightParenthesis => ")",
            Self::LeftCurlyBracket => "{",
            Self::RightCurlyBracket => "}",
            Self::LeftSquareBracket => "[",
            Self::RightSquareBracket => "]",
            Self::Semicolon => ";",
            Self::PlusSign => "+",
            Self::Assignment => "=",
            Self::Equals => "==",
            Self::HyphenMinus => "-",
            Self::Solidus => "/",
            Self::Asterisk => "*",
            Self::PercentageSign => "%",
            Self::Period => ".",
        }
    }
}

impl Display for Punctuator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
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
            Self::Keyword(..) => "sleutelwoord",

            Self::Identifier(..) => "identifier",
            Self::StringLiteral(..) => "slinger",
            Self::TemplateString(..) => "sjabloonslinger",
            Self::Integer(..) => "getal",

            Self::Punctuator(punctuator) => punctuator.into(),
            Self::IllegalCharacter(..) => "ongeldig teken",
        }
    }
}

impl<'source_code> Display for TokenKind<'source_code> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(ident) => ident.fmt(f),
            Self::IllegalCharacter(ch) => ch.fmt(f),
            Self::Integer(int) => int.fmt(f),
            Self::Keyword(keyword) => f.write_str(keyword.as_ref()),
            Self::Punctuator(punctuator) => punctuator.fmt(f),
            Self::StringLiteral(str) => f.write_fmt(format_args!("\"{str}\"")),
            Self::TemplateString(ts) => {
                f.write_char('"')?;

                for part in ts {
                    part.fmt(f)?;
                }

                f.write_char('"')
            }
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

impl<'source_code> Display for Token<'source_code> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
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

impl<'source_code> Display for TemplateStringToken<'source_code> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TemplateStringToken::Expression(expr) => {
                f.write_char('{')?;
                for token in expr {
                    token.kind.fmt(f)?;
                }
                f.write_char('}')
            }

            TemplateStringToken::Plain { str, .. } => str.fmt(f),
        }
    }
}
