// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::{Display, Formatter, Write};

use crate::{BabString, FileLocation};

use super::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum TemplateStringToken {
    Plain {
        begin: FileLocation,
        end: FileLocation,
        str: BabString,
    },
    Expression(Vec<Token>),
}

impl Display for TemplateStringToken {
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
