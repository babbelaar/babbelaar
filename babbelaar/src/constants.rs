// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::marker::PhantomData;

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
}
