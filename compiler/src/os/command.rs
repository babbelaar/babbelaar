// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{io, process::{Command, Stdio}};

pub trait CommandExt {
    fn to_stdout(self) -> io::Result<String>;
}

impl CommandExt for Command {
    fn to_stdout(mut self) -> io::Result<String> {
        self.stdout(Stdio::piped());

        let stdout = self.output()?.stdout;
        Ok(match String::from_utf8(stdout) {
            Ok(str) => str,
            Err(e) => String::from_utf8_lossy(e.as_bytes()).into_owned(),
        })
    }
}
