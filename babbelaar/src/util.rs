// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{borrow::Cow, fmt::Display, ops::{Deref, DerefMut}};

use thiserror::Error;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileLocation {
    offset: usize,
    line: usize,
    column: usize,
}

impl FileLocation {
    #[must_use]
    pub const fn new(offset: usize, line: usize, column: usize) -> Self {
        Self {
            offset,
            line,
            column,
        }
    }

    #[must_use]
    pub const fn offset(&self) -> usize {
        self.offset
    }

    #[must_use]
    pub const fn line(&self) -> usize {
        self.line
    }

    #[must_use]
    pub const fn column(&self) -> usize {
        self.column
    }
}

impl From<(usize, usize, usize)> for FileLocation {
    fn from(value: (usize, usize, usize)) -> Self {
        Self::new(value.0, value.1, value.2)
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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FileRange {
    start: FileLocation,
    end: FileLocation,
}

impl FileRange {
    #[must_use]
    pub const fn new(start: FileLocation, end: FileLocation) -> Self {
        debug_assert!(end.offset >= start.offset);
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
        self.end.offset - self.start.offset
    }

    #[must_use]
    pub const fn contains(&self, location: FileLocation) -> bool {
        let start = if location.line == self.start.line {
            self.start.column <= location.column
        } else {
            location.line > self.start.line
        };

        let end = if location.line == self.end.line {
            self.end.column >= location.column
        } else {
            location.line < self.end.line
        };

        if start && end {
            return true;
        }

        self.start.offset <= location.offset && self.end.offset >= location.offset
    }
}

impl From<(FileLocation, FileLocation)> for FileRange {
    fn from(value: (FileLocation, FileLocation)) -> Self {
        Self::new(value.0, value.1)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Ranged<T> {
    range: FileRange,
    value: T,
}

impl<T> Ranged<T> {
    #[must_use]
    pub const fn new(range: FileRange, value: T) -> Self {
        Self {
            range,
            value,
        }
    }

    #[must_use]
    pub const fn range(&self) -> FileRange {
        self.range
    }

    #[must_use]
    pub const fn value(&self) -> &T {
        &self.value
    }

    #[must_use]
    pub fn map<N>(self, f: impl FnOnce(T) -> N) -> Ranged<N>  {
        Ranged {
            range: self.range,
            value: f(self.value),
        }
    }

    #[must_use]
    pub fn into_value(self) -> T {
        self.value
    }
}

impl<T> AsRef<T> for Ranged<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T> AsMut<T> for Ranged<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T> Deref for Ranged<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for Ranged<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

pub trait OptionExt<T> {
    #[must_use]
    fn as_inner_slice(&self) -> &[T];
}

impl<T> OptionExt<T> for Option<Vec<T>> {
    fn as_inner_slice(&self) -> &[T] {
        match self {
            Some(vec) => vec.as_slice(),
            None => &[]
        }
    }
}

impl<T> OptionExt<T> for Option<&[T]> {
    fn as_inner_slice(&self) -> &[T] {
        match self {
            Some(slice) => slice,
            None => &[]
        }
    }
}

pub trait DocumentationProvider {
    fn provide_documentation(&self) -> Cow<'_, str>;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LspCompletion<'a> {
    pub completion: &'a str,
    pub inline_detail: &'a str,
}

#[derive(Debug, Clone)]
pub struct BabbelaarCodeAction {
    ty: BabbelaarCodeActionType,
    edits: Vec<FileEdit>,
}

impl BabbelaarCodeAction {
    #[must_use]
    pub fn new(ty: BabbelaarCodeActionType, edits: Vec<FileEdit>) -> Self {
        Self {
            ty,
            edits,
        }
    }

    #[must_use]
    pub fn type_(&self) -> &BabbelaarCodeActionType {
        &self.ty
    }

    #[must_use]
    pub fn edits(&self) -> &[FileEdit] {
        &self.edits
    }
}

#[derive(Debug, Clone, Error)]
pub enum BabbelaarCodeActionType {
    #[error("zet Slinger `\"{number}\"` om naar getal `{number}`")]
    ChangeStringToNumber { number: isize },

    #[error("vul structuurvelden van `{structure}`")]
    FillStructureFields { structure: String },
}

#[derive(Debug, Clone)]
pub struct FileEdit {
    replacement_range: FileRange,
    new_text: String,
}

impl FileEdit {
    #[must_use]
    pub fn new(replacement_range: FileRange, new_text: String) -> Self {
        Self {
            replacement_range,
            new_text,
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
}
