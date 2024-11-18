// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::{Display, Formatter, Write};

use crate::BabString;

use super::{Keyword, Punctuator, TemplateStringToken};

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Keyword(Keyword),

    Identifier(BabString),
    CharacterLiteral(char),
    StringLiteral(BabString),
    TemplateString(Vec<TemplateStringToken>),
    // TODO change to unsigned
    Integer(i64),

    Punctuator(Punctuator),
    IllegalCharacter(char),
}

impl TokenKind {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Keyword(..) => "sleutelwoord",

            Self::Identifier(..) => "identifier",
            Self::CharacterLiteral(..) => "teken",
            Self::StringLiteral(..) => "slinger",
            Self::TemplateString(..) => "sjabloonslinger",
            Self::Integer(..) => "getal",

            Self::Punctuator(punctuator) => punctuator.into(),
            Self::IllegalCharacter(..) => "ongeldig teken",
        }
    }

    #[must_use]
    pub fn can_be_variable(&self) -> bool {
        match self {
            Self::Identifier(..) => true,
            Self::Keyword(Keyword::Dit) => true,
            _ => false,
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(ident) => ident.fmt(f),
            Self::IllegalCharacter(ch) => ch.fmt(f),
            Self::Integer(int) => int.fmt(f),
            Self::Keyword(keyword) => f.write_str(keyword.as_ref()),
            Self::Punctuator(punctuator) => punctuator.fmt(f),
            Self::CharacterLiteral(c) => f.write_fmt(format_args!("'{c}'")),
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
