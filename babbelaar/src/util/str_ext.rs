// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use super::FileLocation;

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
