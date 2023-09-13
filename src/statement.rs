// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use crate::Expression;

#[derive(Clone, Debug)]
pub enum Statement<'source_code> {
    Expression(Expression<'source_code>),
}
