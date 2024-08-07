// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{borrow::Cow, path::{Path, PathBuf}};

use crate::{Statement, StatementKind};

#[derive(Debug, Default, Clone)]
pub struct ParseTree<'source_code> {
    pub(crate) path: PathBuf,
    functions: Vec<Statement<'source_code>>,
    statements: Vec<Statement<'source_code>>,
}

impl<'source_code> ParseTree<'source_code> {
    #[must_use]
    pub fn new(path: PathBuf) -> Self {
        Self {
            path,
            functions: Vec::new(),
            statements: Vec::new(),
        }
    }

    #[must_use]
    pub fn path(&self) -> &Path {
        &self.path
    }

    #[must_use]
    pub fn module_name(&self) -> Cow<'_, str> {
        self.path.to_string_lossy()
    }

    #[must_use]
    pub fn statements(&self) -> &[Statement<'source_code>] {
        &self.statements
    }

    #[must_use]
    pub fn functions(&self) -> &[Statement<'source_code>] {
        &self.functions
    }

    pub fn all(&self) -> impl Iterator<Item = &Statement<'source_code>> {
        self.functions.iter().chain(self.statements.iter())
    }

    pub fn push(&mut self, statement: Statement<'source_code>) {
        if matches!(statement.kind, StatementKind::Function(..)) {
            self.functions.push(statement);
        } else {
            self.statements.push(statement);
        }
    }
}
