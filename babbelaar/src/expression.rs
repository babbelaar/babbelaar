// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::{Debug, Display, Write};

use crate::{BabString, FileRange, Ranged, Type};

#[derive(Clone, Debug)]
pub enum PrimaryExpression {
    Boolean(bool),
    CharacterLiteral(char),
    StringLiteral(BabString),
    IntegerLiteral(i64),
    Reference(Ranged<BabString>),
    ReferenceThis,
    StructureInstantiation(StructureInstantiationExpression),
    TemplateString {
        parts: Vec<TemplateStringExpressionPart>,
    },
    Parenthesized(Box<Ranged<Expression>>),
    SizedArrayInitializer {
        typ: Ranged<Type>,
        size: Box<Ranged<Expression>>,
    },
}

impl Display for PrimaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimaryExpression::Boolean(b) => f.write_str(if *b { "waar" } else { "onwaar" }),
            PrimaryExpression::CharacterLiteral(c) => {
                f.write_char('\'')?;
                f.write_char(*c)?;
                f.write_char('\'')
            }
            PrimaryExpression::StringLiteral(bab_string) => {
                f.write_char('"')?;
                f.write_str(bab_string.as_str())?;
                f.write_char('"')
            }
            PrimaryExpression::IntegerLiteral(i) => f.write_fmt(format_args!("{i}")),
            PrimaryExpression::Reference(bab_string) => {
                f.write_str(bab_string.as_str())
            }
            PrimaryExpression::ReferenceThis => f.write_str("dit"),
            PrimaryExpression::StructureInstantiation(..) => todo!(),
            PrimaryExpression::TemplateString { .. } => todo!(),
            PrimaryExpression::Parenthesized(expr) => {
                f.write_char('(')?;
                Display::fmt(expr.value(), f)?;
                f.write_char(')')
            }
            PrimaryExpression::SizedArrayInitializer { typ, size } => {
                f.write_fmt(format_args!("nieuw {}[{}]", typ.value(), size.value()))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructureInstantiationExpression {
    pub name: Ranged<BabString>,
    pub fields: Vec<FieldInstantiation>,
    pub range: FileRange,

    pub left_curly_bracket: FileRange,
    pub right_curly_bracket: FileRange,
}

#[derive(Debug, Clone)]
pub struct FieldInstantiation {
    pub name: Ranged<BabString>,
    pub value: Box<Ranged<Expression>>,
}

#[derive(Debug, Clone)]
pub enum TemplateStringExpressionPart {
    String(BabString),
    Expression(Ranged<Expression>),
}

#[derive(Clone, Debug)]
pub enum Expression {
    BiExpression(BiExpression),
    Postfix(PostfixExpression),
    Primary(PrimaryExpression),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BiExpression(expr) => Display::fmt(expr, f),
            Self::Postfix(expr) => Display::fmt(expr, f),
            Self::Primary(expr) => Display::fmt(expr, f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PostfixExpression {
    pub lhs: Box<Ranged<Expression>>,
    pub kind: Ranged<PostfixExpressionKind>,
}

impl Display for PostfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.lhs.value(), f)?;

        match self.kind.value() {
            PostfixExpressionKind::Call(call) => {
                Display::fmt(call, f)
            }

            PostfixExpressionKind::Member(member) => {
                f.write_char('.')?;
                f.write_str(member.as_str())
            }

            PostfixExpressionKind::MethodCall(method) => {
                f.write_char('.')?;
                f.write_str(method.method_name.as_str())?;
                Display::fmt(&method.call, f)
            }

            PostfixExpressionKind::Subscript(expr) => {
                f.write_char('[')?;
                Display::fmt(expr.value(), f)?;
                f.write_char(']')
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum PostfixExpressionKind {
    Call(FunctionCallExpression),
    Member(Ranged<BabString>),
    MethodCall(MethodCallExpression),
    Subscript(Box<Ranged<Expression>>),
}

#[derive(Clone, Debug)]
pub struct FunctionCallExpression {
    pub arguments: Vec<Ranged<Expression>>,

    pub token_left_paren: FileRange,
    pub token_right_paren: FileRange,
}

impl Display for FunctionCallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('(')?;

        for (idx, arg) in self.arguments.iter().enumerate() {
            if idx != 0 {
                f.write_str(", ")?;
            }

            Display::fmt(arg.value(), f)?;
        }

        f.write_char(')')
    }
}

#[derive(Debug, Clone)]
pub struct MethodCallExpression {
    pub method_name: Ranged<BabString>,
    pub call: FunctionCallExpression,
}

#[derive(Clone, Debug)]
pub struct BiExpression {
    pub operator: Ranged<BiOperator>,
    pub lhs: Box<Ranged<Expression>>,
    pub rhs: Box<Ranged<Expression>>,
}

impl Display for BiExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.lhs.value(), f)?;

        f.write_char(' ')?;
        f.write_str(self.operator.as_str())?;
        f.write_char(' ')?;

        Display::fmt(self.rhs.value(), f)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BiOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Comparison(Comparison),
}

impl BiOperator {
    #[must_use]
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Subtract => "-",
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::Modulo => "%",
            Self::Comparison(comp) => comp.as_str(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Comparison {
    Equality,
    Inequality,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

impl Comparison {
    #[must_use]
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Equality => "==",
            Self::Inequality => "!=",
            Self::LessThan => "<",
            Self::LessThanOrEqual => "<=",
            Self::GreaterThan => ">",
            Self::GreaterThanOrEqual => ">=",
        }
    }
}

#[derive(Clone, Debug)]
pub struct RangeExpression {
    /// Start, inclusive
    pub start: Ranged<PrimaryExpression>,

    /// End, exclusive
    pub end: Ranged<PrimaryExpression>,
}
