// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{FileRange, Ranged};

#[derive(Clone, Debug)]
pub enum PrimaryExpression<'source_code> {
    Boolean(bool),
    StringLiteral(&'source_code str),
    IntegerLiteral(i64),
    Reference(Ranged<&'source_code str>),
    TemplateString {
        parts: Vec<TemplateStringExpressionPart<'source_code>>,
    },
}

#[derive(Debug, Clone)]
pub enum TemplateStringExpressionPart<'source_code> {
    String(&'source_code str),
    Expression(Expression<'source_code>),
}

#[derive(Clone, Debug)]
pub enum Expression<'source_code> {
    BiExpression(BiExpression<'source_code>),
    Function(FunctionCallExpression<'source_code>),
    Primary(PrimaryExpression<'source_code>),
}

#[derive(Clone, Debug)]
pub struct FunctionCallExpression<'source_code> {
    pub function_identifier: Ranged<String>,
    pub arguments: Vec<Expression<'source_code>>,

    pub token_left_paren: FileRange,
    pub token_right_paren: FileRange,
}

#[derive(Clone, Debug)]
pub struct BiExpression<'source_code> {
    pub operator: BiOperator,
    pub lhs: Box<Expression<'source_code>>,
    pub rhs: Box<Expression<'source_code>>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BiOperator {
    Add,
    Subtract,
    Multiply,
    Comparison(Comparison),
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

#[derive(Clone, Debug)]
pub struct RangeExpression<'source_code> {
    /// Start, inclusive
    pub start: Ranged<PrimaryExpression<'source_code>>,

    /// End, exclusive
    pub end: Ranged<PrimaryExpression<'source_code>>,
}
