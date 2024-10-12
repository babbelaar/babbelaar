// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::marker::PhantomData;

pub struct Constants {
    _marker: PhantomData<String>,
}

impl Constants {
    pub const LANGUAGE_ID: &'static str = "babbelaar";
    pub const ENV_LIBRARY_C: &'static str = "BABBELAAR_C_BIBLIOTHEEK";
}
