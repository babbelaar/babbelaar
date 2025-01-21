// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{fmt::Debug, hash::{DefaultHasher, Hash, Hasher}, path::Path};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileId(usize);

impl FileId {
    pub const INTERNAL: Self = Self(usize::MAX);

    #[must_use]
    pub const fn is(&self, other: &Self) -> bool {
        self.0 == other.0
    }

    #[must_use]
    pub fn from_path(path: &Path) -> Self {
        let mut hasher = DefaultHasher::new();
        path.hash(&mut hasher);
        Self(hasher.finish() as _)
    }
}

impl Debug for FileId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if *self == Self::INTERNAL {
            f.write_str("FileId::INTERNAL")
        } else {
            f.debug_tuple("FileId").field(&self.0).finish()
        }
    }
}

impl Default for FileId {
    fn default() -> Self {
        Self::INTERNAL
    }
}
