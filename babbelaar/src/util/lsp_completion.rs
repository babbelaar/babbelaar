// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LspCompletion<'a> {
    pub completion: &'a str,
    pub inline_detail: &'a str,
}
