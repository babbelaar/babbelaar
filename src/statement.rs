// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::Expression;

#[derive(Clone, Debug)]
pub enum Statement<'source_code> {
    Expression(Expression<'source_code>),
    Function(FunctionStatement<'source_code>),
}

#[derive(Clone, Debug)]
pub struct FunctionStatement<'source_code> {
    pub name: &'source_code str,
    pub body: Vec<Statement<'source_code>>,
}
