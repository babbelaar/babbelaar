// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

//! LDD - print shared dependencies

use std::{marker::PhantomData, process::{Command, Stdio}};

use log::warn;

use crate::{os::CommandExt, Environment};

pub struct Ldd {
    _marker: PhantomData<()>,
}

impl Ldd {
    #[must_use]
    pub fn try_detect_environment() -> Option<Environment> {
        let mut command = Command::new("ldd");
        command.arg("--version");

        let stdout = command.to_stdout().ok()?.to_lowercase();

        if stdout.contains("glibc") {
            Some(Environment::Gnu)
        } else {
            warn!("Niet-ondersteunde omgeving: `{stdout}`");
            None
        }
    }
}
