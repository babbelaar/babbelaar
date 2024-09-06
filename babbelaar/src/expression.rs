// Copyright (C) 2023 - 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{FileRange, Ranged};

#[derive(Clone, Debug)]
pub enum PrimaryExpression<'source_code> {
    Boolean(bool),
    StringLiteral(&'source_code str),
    IntegerLiteral(i64),
    Reference(Ranged<&'source_code str>),
    StructureInstantiation(StructureInstantiationExpression<'source_code>),
    TemplateString {
        parts: Vec<TemplateStringExpressionPart<'source_code>>,
    },
    Parenthesized(Box<Ranged<Expression<'source_code>>>),
}

#[derive(Debug, Clone)]
pub struct StructureInstantiationExpression<'source_code> {
    pub name: Ranged<&'source_code str>,
    pub fields: Vec<FieldInstantiation<'source_code>>,
    pub range: FileRange,

    pub left_curly_bracket: FileRange,
    pub right_curly_bracket: FileRange,
}

#[derive(Debug, Clone)]
pub struct FieldInstantiation<'source_code> {
    pub name: Ranged<&'source_code str>,
    pub value: Box<Ranged<Expression<'source_code>>>,
}

#[derive(Debug, Clone)]
pub enum TemplateStringExpressionPart<'source_code> {
    String(&'source_code str),
    Expression(Ranged<Expression<'source_code>>),
}

#[derive(Clone, Debug)]
pub enum Expression<'source_code> {
    BiExpression(BiExpression<'source_code>),
    Postfix(PostfixExpression<'source_code>),
    Primary(PrimaryExpression<'source_code>),
}

#[derive(Debug, Clone)]
pub struct PostfixExpression<'source_code> {
    pub lhs: Box<Ranged<Expression<'source_code>>>,
    pub kind: PostfixExpressionKind<'source_code>,
}

#[derive(Debug, Clone)]
pub enum PostfixExpressionKind<'source_code> {
    Call(FunctionCallExpression<'source_code>),
    Member(Ranged<&'source_code str>),
    MethodCall(MethodCallExpression<'source_code>),
}

#[derive(Clone, Debug)]
pub struct FunctionCallExpression<'source_code> {
    pub arguments: Vec<Ranged<Expression<'source_code>>>,

    pub token_left_paren: FileRange,
    pub token_right_paren: FileRange,
}

#[derive(Debug, Clone)]
pub struct MethodCallExpression<'source_code> {
    pub method_name: Ranged<&'source_code str>,
    pub call: FunctionCallExpression<'source_code>,
}

#[derive(Clone, Debug)]
pub struct BiExpression<'source_code> {
    pub operator: Ranged<BiOperator>,
    pub lhs: Box<Ranged<Expression<'source_code>>>,
    pub rhs: Box<Ranged<Expression<'source_code>>>,
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
pub struct RangeExpression<'source_code> {
    /// Start, inclusive
    pub start: Ranged<PrimaryExpression<'source_code>>,

    /// End, exclusive
    pub end: Ranged<PrimaryExpression<'source_code>>,
}
