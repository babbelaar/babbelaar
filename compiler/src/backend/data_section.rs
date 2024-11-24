// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct DataSection {
    kind: DataSectionKind,
    data: Vec<u8>,
}

impl DataSection {
    #[must_use]
    pub fn new(kind: DataSectionKind) -> Self {
        Self {
            kind,
            data: Vec::new(),
        }
    }

    #[must_use]
    pub fn data(&self) -> &[u8] {
        &self.data
    }

    #[must_use]
    pub fn add_null_terminated_string(&mut self, string: &str) -> DataSectionOffset {
        let offset = self.data.len();

        self.data.extend_from_slice(string.as_bytes());
        self.data.push(0);

        DataSectionOffset::new(self.kind, offset)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataSectionKind {
    ReadOnly,
}

impl DataSectionKind {
    #[must_use]
    pub const fn name(&self) -> &'static str {
        match self {
            Self::ReadOnly => ".rodata",
        }
    }
}

impl Display for DataSectionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DataSectionOffset {
    kind: DataSectionKind,
    offset: usize,
}

impl DataSectionOffset {
    #[must_use]
    pub const fn new(section: DataSectionKind, offset: usize) -> Self {
        Self {
            kind: section,
            offset,
        }
    }

    #[must_use]
    pub const fn offset(&self) -> usize {
        self.offset
    }

    #[must_use]
    pub const fn section_kind(&self) -> DataSectionKind {
        self.kind
    }
}
