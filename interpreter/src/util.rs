// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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

pub trait StringExt {
    fn find_file_location(&self, offset: usize) -> Option<FileLocation>;
}

impl StringExt for &str {
    fn find_file_location(&self, offset: usize) -> Option<FileLocation> {
        let mut location = FileLocation::default();

        let mut iter = self.char_indices().peekable();
        while let Some((current_offset, ch)) = iter.next() {
            if current_offset == offset {
                return Some(location);
            }

            if current_offset > offset {
                return Some(location);
            }

            if ch == '\r' {
                if iter.peek().is_some_and(|(_, ch)| *ch == '\n') {
                    iter.next();
                }

                location.line += 1;
                location.column = 0;
            } else if ch == '\n' {
                location.line += 1;
                location.column = 0;
            } else {
                location.column += 1;
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("hello", 0, (0, 0, 0))]
    #[case("hello", 1, (1, 0, 1))]
    #[case("\n", 0, (0, 0, 0))]
    #[case("\n1", 1, (1, 1, 0))]
    #[case("hello\nworld", 7, (7, 1, 1))]
    #[case("\r\nhello\r\nworld", 9, (9, 2, 0))]
    fn find_file_location_tests(#[case] input: &str, #[case] offset: usize, #[case] expected: impl Into<Option<(usize, usize, usize)>>) {
        let expected = expected.into().map(|x| x.into());

        let actual = input.find_file_location(offset);

        assert_eq!(expected, actual);
    }
}
