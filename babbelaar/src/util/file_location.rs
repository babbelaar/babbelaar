// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use super::{FileId, FileRange};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileLocation {
    file_id: FileId,
    offset: usize,
    line: usize,

    // Currently, the `column` is actually the code point index into the line, which makes it
    // effectively UTF-32. This is maybe good, but maybe awful because the `offset` is UTF-8,
    // and the LSP engine expects UTF-16.
    column: usize,
}

impl FileLocation {
    pub const INTERNAL: Self = Self::new(FileId::INTERNAL, 0, 0, 0);

    #[must_use]
    pub const fn new(file_id: FileId, offset: usize, line: usize, column: usize) -> Self {
        Self {
            file_id,
            offset,
            line,
            column,
        }
    }

    /// Zero based byte index
    #[must_use]
    pub const fn offset(&self) -> usize {
        self.offset
    }

    /// Zero-based line number
    #[must_use]
    pub const fn line(&self) -> usize {
        self.line
    }

    /// Zero-based column number
    #[must_use]
    pub const fn column(&self) -> usize {
        self.column
    }

    #[must_use]
    pub const fn as_zero_range(&self) -> FileRange {
        FileRange::new(*self, *self)
    }

    #[must_use]
    pub const fn file_id(&self) -> FileId {
        self.file_id
    }
}

impl From<FileLocation> for (usize, usize) {
    fn from(value: FileLocation) -> Self {
        (value.line(), value.column())
    }
}

impl Display for FileLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}:{}", self.line() + 1, self.column() + 1))
    }
}
