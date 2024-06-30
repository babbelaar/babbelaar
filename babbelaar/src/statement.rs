// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{util::FileRange, Attribute, Expression, Parameter, RangeExpression, Ranged};

#[derive(Debug, Clone)]
pub struct Statement<'source_code> {
    pub range: FileRange,
    pub attributes: Vec<Attribute<'source_code>>,
    pub kind: StatementKind<'source_code>,
}

#[derive(Debug, Clone)]
pub enum StatementKind<'source_code> {
    Expression(Ranged<Expression<'source_code>>),
    Function(FunctionStatement<'source_code>),
    For(ForStatement<'source_code>),
    If(IfStatement<'source_code>),
    Return(ReturnStatement<'source_code>),
    Variable(VariableStatement<'source_code>),
}

impl<'source_code> StatementKind<'source_code> {
    #[must_use]
    pub const fn is_expression(&self) -> bool {
        matches!(self, Self::Expression(..))
    }
}

#[derive(Clone, Debug)]
pub struct ForStatement<'source_code> {
    pub file_range: FileRange,
    pub keyword: FileRange,
    pub iterator_name: Ranged<&'source_code str>,
    pub range: RangeExpression<'source_code>,
    pub body: Vec<Statement<'source_code>>,
}

#[derive(Clone, Debug)]
pub struct FunctionStatement<'source_code> {
    pub range: FileRange,
    pub name: Ranged<&'source_code str>,
    pub parameters: Vec<Parameter<'source_code>>,
    pub body: Option<Vec<Statement<'source_code>>>,
}

#[derive(Clone, Debug)]
pub struct IfStatement<'source_code> {
    pub range: FileRange,
    pub condition: Ranged<Expression<'source_code>>,
    pub body: Vec<Statement<'source_code>>,
}

#[derive(Clone, Debug)]
pub struct ReturnStatement<'source_code> {
    pub expression: Option<Ranged<Expression<'source_code>>>,
}

#[derive(Clone, Debug)]
pub struct VariableStatement<'source_code> {
    pub range: FileRange,
    pub name: Ranged<&'source_code str>,
    pub expression: Ranged<Expression<'source_code>>,
}
