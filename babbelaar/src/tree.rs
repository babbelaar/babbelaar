// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{borrow::Cow, path::{Path, PathBuf}};

use crate::Statement;

#[derive(Debug, Clone)]
pub struct ParseTree<'source_code> {
    pub(crate) path: PathBuf,
    pub(crate) statements: Vec<Statement<'source_code>>,
}

impl<'source_code> ParseTree<'source_code> {
    #[must_use]
    pub fn new(path: PathBuf) -> Self {
        Self {
            path,
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
}
