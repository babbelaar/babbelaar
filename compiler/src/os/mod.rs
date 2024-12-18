// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod command;
mod signal;
pub mod linux;
pub mod macos;
pub mod windows;

pub use self::{
    command::CommandExt,
    signal::Signal,
};