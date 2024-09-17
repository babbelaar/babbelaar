// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{fmt::{Debug, Display}, hash::Hash, ops::{Deref, Index, Range}, sync::Arc};

#[derive(Clone)]
pub struct BabString {
    inner: BabStringImpl,
}

impl BabString {
    #[must_use]
    pub const fn empty() -> Self {
        Self::new_static("")
    }

    #[must_use]
    pub const fn new_static(str: &'static str) -> BabString {
        Self {
            inner: BabStringImpl::Static { str },
        }
    }

    #[must_use]
    pub fn new(str: impl Into<Arc<str>>) -> Self {
        let str = str.into();
        let start = 0;
        let end = str.len();

        Self {
            inner: BabStringImpl::Dynamic {
                data: Arc::from(str),
                start,
                end,
            },
        }
    }

    #[must_use]
    pub fn sliced(&self, start: usize, end: usize) -> Self {
        if start == end {
            return Self::empty();
        }

        let (new_start, new_end) = (start, end);

        match &self.inner {
            BabStringImpl::Dynamic { data, start: cur_start, end: cur_end } => {
                let (cur_start, cur_end) = (*cur_start, *cur_end);

                let start = cur_start + new_start;
                let end = cur_start + new_end;

                debug_assert!(start <= data.len(), "Start {start} > length {}, for inputs=({new_start}, {new_end}) current string({cur_start}, {cur_end})='{old_str}'", data.len(), old_str = &data[cur_start..cur_end]);
                debug_assert!(end <= data.len(), "End {end} > length {}, for inputs=({new_start}, {new_end}) current string='{old_str}'", data.len(), old_str = &data[cur_start..cur_end]);

                Self {
                    inner: BabStringImpl::Dynamic {
                        data: Arc::clone(data),
                        start,
                        end,
                    }
                }
            }

            BabStringImpl::Static { str } => {
                debug_assert!(start <= str.len());
                debug_assert!(end <= str.len());

                Self {
                    inner: BabStringImpl::Static {
                        str: &str.index(start..end),
                    },
                }
            }
        }
    }

    #[must_use]
    pub fn as_str(&self) -> &str {
        match &self.inner {
            BabStringImpl::Dynamic { data, start, end } => {
                &data[*start..*end]
            }

            BabStringImpl::Static { str } => str,
        }
    }
}

impl Debug for BabString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.as_str(), f)
    }
}

#[derive(Debug, Clone)]
enum BabStringImpl {
    Dynamic {
        data: Arc<str>,
        start: usize,
        end: usize,
    },
    Static {
        str: &'static str,
    }
}

impl Deref for BabString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl From<String> for BabString {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}

impl PartialEq for BabString {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for BabString {}

impl Hash for BabString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state);
    }
}

impl PartialEq<str> for BabString {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<&str> for BabString {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<BabString> for str {
    fn eq(&self, other: &BabString) -> bool {
        self == other.as_str()
    }
}

impl Display for BabString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}

pub trait Slice<T> {
    fn slice(&self, t: T) -> Self;
}

impl Slice<Range<usize>> for BabString {
    fn slice(&self, t: Range<usize>) -> Self {
        self.sliced(t.start, t.end)
    }
}

pub trait IntoBabString<T> {
    fn into_bab_string(self) -> T;
}

impl IntoBabString<BabString> for String {
    fn into_bab_string(self) -> BabString {
        BabString::from(self)
    }
}

impl IntoBabString<Option<BabString>> for Option<String> {
    fn into_bab_string(self) -> Option<BabString> {
        self.map(|x| BabString::from(x))
    }
}
