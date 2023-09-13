// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::{Expression, RangeExpression};

#[derive(Clone, Debug)]
pub enum Statement<'source_code> {
    Expression(Expression<'source_code>),
    Function(FunctionStatement<'source_code>),
    For(ForStatement<'source_code>),
}

#[derive(Clone, Debug)]
pub struct ForStatement<'source_code> {
    pub iterator_name: &'source_code str,
    pub range: RangeExpression<'source_code>,
    pub body: Vec<Statement<'source_code>>,
}

#[derive(Clone, Debug)]
pub struct FunctionStatement<'source_code> {
    pub name: &'source_code str,
    pub body: Vec<Statement<'source_code>>,
}
