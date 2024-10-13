// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{borrow::Cow, path::{Path, PathBuf}};

use crate::{Statement, StatementKind};

#[derive(Debug, Default, Clone)]
pub struct ParseTree {
    pub(crate) path: PathBuf,
    functions: Vec<Statement>,
    statements: Vec<Statement>,
    structures: Vec<Statement>,
}

impl ParseTree {
    #[must_use]
    pub fn new(path: PathBuf) -> Self {
        Self {
            path,
            functions: Vec::new(),
            statements: Vec::new(),
            structures: Vec::new(),
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
    pub fn statements(&self) -> &[Statement] {
        &self.statements
    }

    #[must_use]
    pub fn functions(&self) -> &[Statement] {
        &self.functions
    }

    #[must_use]
    pub fn structures(&self) -> &[Statement] {
        &self.structures
    }

    pub fn all(&self) -> impl Iterator<Item = &Statement> {
        self.functions.iter()
            .chain(self.statements.iter())
            .chain(self.structures.iter())
    }

    pub fn push(&mut self, statement: Statement) {
        match &statement.kind {
            StatementKind::Function(..) => self.functions.push(statement),
            StatementKind::Structure(..) => self.structures.push(statement),
            StatementKind::Extension(..) => self.functions.push(statement),
            _ => self.statements.push(statement),
        }
    }
}
