// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{BabString, FileRange, Ranged};

#[derive(Clone, Debug)]
pub enum PrimaryExpression {
    Boolean(bool),
    StringLiteral(BabString),
    IntegerLiteral(i64),
    Reference(Ranged<BabString>),
    ReferenceThis,
    StructureInstantiation(StructureInstantiationExpression),
    TemplateString {
        parts: Vec<TemplateStringExpressionPart>,
    },
    Parenthesized(Box<Ranged<Expression>>),
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

#[derive(Debug, Clone)]
pub struct PostfixExpression {
    pub lhs: Box<Ranged<Expression>>,
    pub kind: PostfixExpressionKind,
}

#[derive(Debug, Clone)]
pub enum PostfixExpressionKind {
    Call(FunctionCallExpression),
    Member(Ranged<BabString>),
    MethodCall(MethodCallExpression),
}

#[derive(Clone, Debug)]
pub struct FunctionCallExpression {
    pub arguments: Vec<Ranged<Expression>>,

    pub token_left_paren: FileRange,
    pub token_right_paren: FileRange,
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
