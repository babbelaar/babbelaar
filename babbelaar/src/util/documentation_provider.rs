// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::borrow::Cow;

pub trait DocumentationProvider {
    fn provide_documentation(&self) -> Cow<'_, str>;
}
