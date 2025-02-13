// Copyright (C) 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NtStatus {
    Unknown(u32),
    AccessViolation,
}

impl NtStatus {
    #[must_use]
    pub fn new(value: u32) -> Self {
        match value {
            0xC0000005 => Self::AccessViolation,
            _ => Self::Unknown(value),
        }
    }
}

impl From<i32> for NtStatus {
    fn from(value: i32) -> Self {
        Self::new(value as _)
    }
}

impl From<u32> for NtStatus {
    fn from(value: u32) -> Self {
        Self::new(value)
    }
}
