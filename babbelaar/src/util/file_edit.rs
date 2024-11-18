// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::path::{Path, PathBuf};

use super::FileRange;

#[derive(Debug, Clone)]
pub struct FileEdit {
    pub(crate) replacement_range: FileRange,
    pub(crate) new_text: String,
    pub(crate) new_file_path: Option<PathBuf>,
}

impl FileEdit {
    #[must_use]
    pub fn new(replacement_range: FileRange, new_text: impl Into<String>) -> Self {
        Self {
            replacement_range,
            new_text: new_text.into(),
            new_file_path: None,
        }
    }

    #[must_use]
    pub fn with_new_file(self, path: PathBuf) -> Self {
        Self {
            new_file_path: Some(path),
            ..self
        }
    }

    #[must_use]
    pub fn replacement_range(&self) -> FileRange {
        self.replacement_range
    }

    #[must_use]
    pub fn new_text(&self) -> &str {
        &self.new_text
    }

    #[must_use]
    pub fn new_file_path(&self) -> Option<&Path> {
        self.new_file_path.as_ref().map(|x| x.as_path())
    }
}
