// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use super::{FileId, FileLocation};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileRange {
    start: FileLocation,
    end: FileLocation,
}

impl FileRange {
    pub const INTERNAL: Self = Self::new(FileLocation::INTERNAL, FileLocation::INTERNAL);

    #[must_use]
    pub const fn new(start: FileLocation, end: FileLocation) -> Self {
        debug_assert!(start.file_id().is(& end.file_id()));
        debug_assert!(end.offset() >= start.offset());
        Self {
            start,
            end,
        }
    }

    #[must_use]
    pub const fn start(&self) -> FileLocation {
        self.start
    }

    #[must_use]
    pub const fn end(&self) -> FileLocation {
        self.end
    }

    #[must_use]
    pub const fn len(&self) -> usize {
        self.end.offset() - self.start.offset()
    }

    #[must_use]
    pub fn contains(&self, location: FileLocation) -> bool {
        if self.file_id() != location.file_id() && location.file_id() != FileId::INTERNAL && self.file_id() != FileId::INTERNAL {
            return false;
        }

        if location == FileLocation::default() {
            return self.start == location;
        }

        let start = if location.line() == self.start.line() {
            self.start.column() <= location.column()
        } else {
            location.line() > self.start.line()
        };

        let end = if location.line() == self.end.line() {
            self.end.column() >= location.column()
        } else {
            location.line() < self.end.line()
        };

        if start && end {
            return true;
        }

        self.start.offset() <= location.offset() && self.end.offset() >= location.offset()
    }

    #[must_use]
    pub fn as_full_line(&self) -> Self {
        Self {
            start: FileLocation::new(
                self.start.file_id(),
                self.start.offset() - self.start.column(),
                self.start.line(),
                0,
            ),
            end: FileLocation::new(
                self.start.file_id(),
                self.end.offset() + 1,
                self.end.line() + 1,
                0,
            )
        }
    }

    #[must_use]
    pub const fn file_id(&self) -> FileId {
        self.start.file_id()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

impl From<(FileLocation, FileLocation)> for FileRange {
    fn from(value: (FileLocation, FileLocation)) -> Self {
        Self::new(value.0, value.1)
    }
}
