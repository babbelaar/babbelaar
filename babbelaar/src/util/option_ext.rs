// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

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
