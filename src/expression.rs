// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#[derive(Clone, Debug)]
pub enum PrimaryExpression<'source_code> {
    StringLiteral(&'source_code str),
    IntegerLiteral(i64),
    Reference(&'source_code str),
}

#[derive(Clone, Debug)]
pub enum Expression<'source_code> {
    Primary(PrimaryExpression<'source_code>),
    Function(FunctionCallExpression<'source_code>),
    BiExpression(BiExpression<'source_code>),
}

#[derive(Clone, Debug)]
pub struct FunctionCallExpression<'source_code> {
    pub function_identifier: String,
    pub arguments: Vec<Expression<'source_code>>,
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
}

#[derive(Clone, Debug)]
pub struct RangeExpression<'source_code> {
    /// Start, inclusive
    pub start: PrimaryExpression<'source_code>,

    /// End, exclusive
    pub end: PrimaryExpression<'source_code>,
}
