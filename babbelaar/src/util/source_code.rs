// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{ops::Deref, path::{Path, PathBuf}, sync::Arc};

use crate::BabString;

use super::FileId;

#[derive(Debug, Clone)]
pub struct SourceCode {
    id: FileId,
    version: i32,
    path: Arc<PathBuf>,
    contents: BabString,
}

impl SourceCode {
    #[must_use]
    #[cfg(test)]
    pub fn new_test(contents: impl Into<BabString>) -> Self {
        Self::new(PathBuf::new(), 0, contents.into())
    }

    #[must_use]
    pub fn new(path: impl Into<PathBuf>, version: i32, contents: impl Into<BabString>) -> Self {
        let path = Arc::new(path.into());
        let id = FileId::from_path(&path);
        let contents = contents.into();

        Self {
            id,
            version,
            path,
            contents,
        }
    }

    #[must_use]
    pub const fn file_id(&self) -> FileId {
        self.id
    }

    #[must_use]
    pub fn path(&self) -> &Path {
        &self.path
    }

    #[must_use]
    pub fn contents(&self) -> &BabString {
        &self.contents
    }

    #[must_use]
    pub const fn version(&self) -> i32 {
        self.version
    }
}

impl Deref for SourceCode {
    type Target = BabString;

    fn deref(&self) -> &Self::Target {
        self.contents()
    }
}
