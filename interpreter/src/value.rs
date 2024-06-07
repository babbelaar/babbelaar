// Copyright (C) 2023 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#[derive(Clone, Debug)]
pub enum Value {
    /// hehe 5 billion dollar problem
    Null,

    Integer(i64),
    String(String),
}
