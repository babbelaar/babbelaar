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
    pub type_parameters: Ranged<Vec<Ranged<Type>>>,
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
    Unary(UnaryExpression),
}

impl Expression {
    #[must_use]
    pub fn as_identifier(&self) -> Option<&BabString> {
        match self {
            Self::Primary(PrimaryExpression::Reference(ident)) => Some(ident.value()),
            _ => None,
        }
    }

    #[must_use]
    pub const fn can_be_taken_address_of(&self) -> bool {
        if let Expression::Primary(PrimaryExpression::Reference(..)) = self {
            return true;
        }

        if let Expression::Postfix(postfix) = self {
            return matches!(postfix.kind.value(), PostfixExpressionKind::Member(..));
        }

        false
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BiExpression(expr) => Display::fmt(expr, f),
            Self::Postfix(expr) => Display::fmt(expr, f),
            Self::Primary(expr) => Display::fmt(expr, f),
            Self::Unary(expr) => Display::fmt(expr, f),
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

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pub kind: Ranged<UnaryExpressionKind>,
    pub rhs: Box<Ranged<Expression>>,
}

impl Display for UnaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind.value() {
            UnaryExpressionKind::AddressOf => f.write_char('&')?,
            UnaryExpressionKind::Negate => f.write_char('-')?,
        }

        Display::fmt(self.rhs.value(), f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryExpressionKind {
    AddressOf,
    Negate,
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
    Comparison(Comparison),
    Math(MathOperator),
}

impl BiOperator {
    #[must_use]
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Comparison(comp) => comp.as_str(),
            Self::Math(math) => math.as_str(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MathOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LogicalAnd,
    LogicalOr,
    LeftShift,
    RightShift,
}

impl MathOperator {
    #[must_use]
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Subtract => "-",
            Self::Multiply => "*",
            Self::Divide => "/",
            Self::Modulo => "%",
            Self::BitwiseAnd => "&",
            Self::BitwiseOr => "|",
            Self::BitwiseXor => "^",
            Self::LogicalAnd => "&&",
            Self::LogicalOr => "||",
            Self::LeftShift => "<<",
            Self::RightShift => ">>",
        }
    }
}

impl From<MathOperator> for BiOperator {
    fn from(value: MathOperator) -> Self {
        Self::Math(value)
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

impl From<Comparison> for BiOperator {
    fn from(value: Comparison) -> Self {
        Self::Comparison(value)
    }
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
    pub start: Box<Ranged<Expression>>,

    /// End, exclusive
    pub end: Box<Ranged<Expression>>,
}
