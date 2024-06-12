// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{util::FileRange, Expression, Parameter, RangeExpression, Ranged};

#[derive(Debug, Clone)]
pub struct Statement<'source_code> {
    pub range: FileRange,
    pub kind: StatementKind<'source_code>,
}

#[derive(Debug, Clone)]
pub enum StatementKind<'source_code> {
    Expression(Expression<'source_code>),
    Function(FunctionStatement<'source_code>),
    For(ForStatement<'source_code>),
    Return(ReturnStatement<'source_code>),
}

#[derive(Clone, Debug)]
pub struct ForStatement<'source_code> {
    pub keyword: FileRange,
    pub iterator_name: Ranged<&'source_code str>,
    pub range: RangeExpression<'source_code>,
    pub body: Vec<Statement<'source_code>>,
}

#[derive(Clone, Debug)]
pub struct FunctionStatement<'source_code> {
    pub name: &'source_code str,
    pub parameters: Vec<Parameter<'source_code>>,
    pub body: Vec<Statement<'source_code>>,
}

#[derive(Clone, Debug)]
pub struct ReturnStatement<'source_code> {
    pub expression: Option<Expression<'source_code>>,
}
