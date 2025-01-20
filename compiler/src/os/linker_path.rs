// Copyright (C) 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::path::PathBuf;

#[derive(Debug, Clone)]
pub enum LinkerPath {
    Object(PathBuf),
    StaticLibrary(PathBuf),
}

impl LinkerPath {
    #[must_use]
    pub fn path_buf(&self) -> &PathBuf {
        match self {
            Self::Object(path) => path,
            Self::StaticLibrary(path) => path,
        }
    }

    #[must_use]
    pub fn as_path_buf(self) -> PathBuf {
        match self {
            Self::Object(path) => path,
            Self::StaticLibrary(path) => path,
        }
    }
}
