// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{borrow::Cow, fmt::Display, hash::{DefaultHasher, Hash, Hasher}, ops::{Deref, DerefMut}, path::{Path, PathBuf}, sync::Arc};

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

    #[must_use]
    pub const fn as_zero_range(&self) -> FileRange {
        FileRange {
            start: *self,
            end: *self,
        }
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
    pub fn contains(&self, location: FileLocation) -> bool {
        if location == FileLocation::default() {
            return self.start == location;
        }

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

    #[must_use]
    pub fn as_full_line(&self) -> Self {
        Self {
            start: FileLocation {
                offset: self.start.offset - self.start.column,
                line: self.start.line,
                column: 0,
            },
            end: FileLocation {
                offset: self.end.offset + 1,
                line: self.end.line + 1,
                column: 0,
            }
        }
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
    #[error("Sleutelwoord `{keyword}` toevoegen")]
    AddKeyword { keyword: &'static str, },

    #[error("Zet Slinger `\"{number}\"` om naar getal `{number}`")]
    ChangeStringToNumber { number: isize },

    #[error("Maak veld `{name}` aan")]
    CreateField { name: String },

    #[error("Vul structuurvelden van `{structure}`")]
    FillStructureFields { structure: String },

    #[error("`{text}` invoegen")]
    Insert { text: &'static str, },

    #[error("Verwijder puur statement")]
    RemovePureStatement,

    #[error("Verwijder extra tekens")]
    RemoveResidualTokens,
}

#[derive(Debug, Clone)]
pub struct FileEdit {
    replacement_range: FileRange,
    new_text: String,
}

impl FileEdit {
    #[must_use]
    pub fn new(replacement_range: FileRange, new_text: impl Into<String>) -> Self {
        Self {
            replacement_range,
            new_text: new_text.into(),
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

pub trait StrIterExt: Sized {
    fn join(self, str: &str) -> String;
}

impl<'s, T> StrIterExt for T
        where T: Iterator<Item = &'s str> + 's {
    fn join(mut self, str: &str) -> String {
        let mut s = String::new();

        while let Some(item) = self.next() {
            if !s.is_empty() {
                s += str;
            }

            s += item;
        }

        s
    }
}

pub trait StrExt {
    #[must_use]
    fn count_whitespace_at_end(&self) -> usize;

    #[must_use]
    fn count_space_at_end(&self) -> usize;

    #[must_use]
    fn indentation_at(&self, start: FileLocation) -> Option<&str>;
}

impl StrExt for str {
    fn count_whitespace_at_end(&self) -> usize {
        self.len() - self.trim_end().len()
    }

    fn count_space_at_end(&self) -> usize {
        self.len() - self.trim_end_matches(|c: char| c == ' ').len()
    }

    fn indentation_at(&self, start: FileLocation) -> Option<&str> {
        let line = self.lines().nth(start.line())?;

        let line = &line[..start.column()];
        Some(&line[..line.len() - line.trim_start().len()])
    }
}

#[derive(Debug, Clone)]
pub struct SourceCode {
    id: FileId,
    path: Arc<PathBuf>,
    contents: Arc<str>,
}

impl SourceCode {
    #[must_use]
    #[cfg(test)]
    pub fn new_test(contents: impl Into<String>) -> Self {
        Self::new(PathBuf::new(), contents.into())
    }

    #[must_use]
    pub fn new(path: impl Into<PathBuf>, contents: String) -> Self {
        let path = Arc::new(path.into());
        let id = FileId::from_path(&path);
        let contents = Arc::<str>::from(contents);

        Self {
            id,
            path,
            contents,
        }
    }

    #[must_use]
    pub const fn id(&self) -> FileId {
        self.id
    }

    #[must_use]
    pub fn path(&self) -> &Path {
        &self.path
    }

    #[must_use]
    pub fn contents(&self) -> &str {
        &self.contents
    }
}

impl Deref for SourceCode {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.contents()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FileId(usize);

impl FileId {
    #[must_use]
    pub fn from_path(path: &Path) -> Self {
        let mut hasher = DefaultHasher::new();
        path.hash(&mut hasher);
        Self(hasher.finish() as _)
    }
}
