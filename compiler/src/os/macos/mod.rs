// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{error::Error, io::Read, path::{Path, PathBuf}, process::{Command, Stdio}};

use crate::LinkerError;

pub struct MacOsLdLinker {
    output_path: PathBuf,
    object_paths: Vec<PathBuf>,
}

impl MacOsLdLinker {
    #[must_use]
    pub fn new(output_path: impl Into<PathBuf>) -> Self {
        Self {
            output_path: output_path.into(),
            object_paths: Vec::new(),
        }
    }

    pub fn add_object(&mut self, path: &Path) {
        self.object_paths.push(path.to_path_buf());
    }

    pub fn run(self) -> Result<(), Box<dyn Error>> {
        let mut command = Command::new("ld");

        command.stderr(Stdio::piped());

        for object_path in &self.object_paths {
            command.arg(object_path);
        }

        command.arg("-o");
        command.arg(&self.output_path);

        command.arg("-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib");
        command.arg("-lSystem");

        let mut process = command.spawn()?;
        let exit_status = process.wait()?;

        if !exit_status.success() {
            let mut description = String::new();
            _ = process.stderr.unwrap().read_to_string(&mut description);
            return Err(LinkerError::UnexpectedError(description).into());
        }

        Ok(())
    }
}
