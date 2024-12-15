// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{fmt::Display, marker::PhantomData};

use crate::BabString;

pub struct Constants {
    _marker: PhantomData<String>,
}

impl Constants {
    pub const LANGUAGE_ID: &'static str = "babbelaar";
    pub const ENV_LIBRARY_C: &'static str = "BABBELAAR_C_BIBLIOTHEEK";

    /// The discarding identifier `_` can only be used for discarding the result
    /// of an expression, parameter, etc. and cannot be used for a “get”.
    pub const DISCARDING_IDENT: BabString = BabString::new_static("_");

    pub const OBJECT_FILE_EXTENSION: &'static str = "bab-voorwerp";

    pub const MAIN_FUNCTION: &'static str = "hoofd";

    pub const MACOS_MINIMUM_VERSION: Version = Version {
        major: 11,
        minor: 0,
        patch: 0,
    };
}

#[derive(Debug, Clone, Copy)]
pub struct Version {
    pub major: u8,
    pub minor: u8,
    pub patch: u8,
}

impl Version {
    #[must_use]
    pub const fn to_macos_nibbles(&self) -> u32 {
        let mut version = self.patch as u32;

        version |= (self.minor as u32) << 8;
        version |= (self.major as u32) << 16;

        version
    }
}

impl Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}.{}.{}", self.major, self.minor, self.patch))
    }
}
