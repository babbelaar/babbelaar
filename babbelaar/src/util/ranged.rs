// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::ops::{Deref, DerefMut};

use super::FileRange;

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
