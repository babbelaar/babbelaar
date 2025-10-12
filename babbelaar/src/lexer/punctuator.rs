// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::{Display, Formatter};

use strum::IntoStaticStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, IntoStaticStr)]
pub enum Punctuator {
    #[strum(serialize = "pijl")]
    Arrow,
    #[strum(serialize = "apenstaartje")]
    AtSign,
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
    #[strum(serialize = "minder-dan")]
    LessThan,
    #[strum(serialize = "minder-dan-of-gelijk")]
    LessThanOrEqual,
    #[strum(serialize = "meer-dan")]
    GreaterThan,
    #[strum(serialize = "meer-dan-of-gelijk")]
    GreaterThanOrEqual,
    #[strum(serialize = "bitgewijs-en")]
    BitwiseAnd,
    #[strum(serialize = "bitgewijs-inclusieve-of")]
    BitwiseOr,
    #[strum(serialize = "bitgewijs-exclusieve-of")]
    BitwiseXor,
    #[strum(serialize = "logische-en")]
    LogicalAnd,
    #[strum(serialize = "logische-of")]
    LogicalOr,
    #[strum(serialize = "schuif-links")]
    LeftShift,
    #[strum(serialize = "schuif-rechts")]
    RightShift,

    #[strum(serialize = "plus-is")]
    AddAssign,
    #[strum(serialize = "min-is")]
    SubtractAssign,
    #[strum(serialize = "keer-is")]
    MultiplyAssign,
    #[strum(serialize = "gedeeld-door-is")]
    DivideAssign,
    #[strum(serialize = "modulus-is")]
    ModuloAssign,
    #[strum(serialize = "bitgewijs-en-is")]
    BitwiseAndAssign,
    #[strum(serialize = "bitgewijs-inclusieve-of-is")]
    BitwiseOrAssign,
    #[strum(serialize = "bitgewijs-exclusieve-of-is")]
    BitwiseXorAssign,
    #[strum(serialize = "schuif-links-is")]
    LeftShiftAssign,
    #[strum(serialize = "schuif-links-rechts")]
    RightShiftAssign,
    #[strum(serialize = "niet")]
    Not,
    #[strum(serialize = "niet-gelijk-aan")]
    NotEquals,
    #[strum(serialize = "dubbele-dubbele-punt")]
    DoubleColon,
}

impl Punctuator {
    #[must_use]
    pub const fn as_str(&self) -> &str {
        match self {
            Self::Arrow => ".",
            Self::AtSign => "@",
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
            Self::LessThan => "<",
            Self::LessThanOrEqual => "<=",
            Self::GreaterThan => ">",
            Self::GreaterThanOrEqual => ">=",
            Self::BitwiseAnd => "&",
            Self::BitwiseOr => "|",
            Self::BitwiseXor => "^",
            Self::LogicalAnd => "&&",
            Self::LogicalOr => "||",
            Self::LeftShift => "<<",
            Self::RightShift => ">>",
            Self::AddAssign => "+=",
            Self::SubtractAssign => "-=",
            Self::MultiplyAssign => "*=",
            Self::DivideAssign => "/=",
            Self::ModuloAssign => "%=",
            Self::BitwiseAndAssign => "&=",
            Self::BitwiseOrAssign => "|=",
            Self::BitwiseXorAssign => "^=",
            Self::LeftShiftAssign => "<<=",
            Self::RightShiftAssign => ">>=",
            Self::Not => "!",
            Self::NotEquals => "!=",
            Self::DoubleColon => "::",
        }
    }
}

impl Display for Punctuator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}
