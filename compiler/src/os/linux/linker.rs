// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{error::Error, ffi::OsStr, io::Read, path::PathBuf, process::{Command, Stdio}};

use crate::{LinkerError, LinkerPath};

/// Linker for static libraries
pub struct LinuxArLinker {
    output_path: PathBuf,
    paths: Vec<LinkerPath>,
}

impl LinuxArLinker {
    #[must_use]
    pub fn new(output_path: impl Into<PathBuf>) -> Self {
        Self {
            output_path: output_path.into(),
            paths: Vec::new(),
        }
    }

    pub fn add_path(&mut self, path: LinkerPath) {
        self.paths.push(path);
    }

    pub fn run(self) -> Result<(), Box<dyn Error>> {
        let mut command = Command::new("ar");

        command.stderr(Stdio::piped());

        // r: Insert or replace files in the archive.
        // c: Create the archive if it doesn't exist.
        // s: Create an index to speed up symbol lookup.
        command.arg("rcs");

        command.arg(&self.output_path);

        for object_path in self.paths {
            command.arg(object_path.as_path_buf());
        }

        let mut process = command.spawn()?;
        let exit_status = process.wait()?;

        if !exit_status.success() {
            let mut description = String::new();
            _ = process.stderr.unwrap().read_to_string(&mut description);
            eprintln!("{description}");
            return Err(LinkerError::UnexpectedError(description).into());
        }

        Ok(())
    }
}


/// Linker for executables
pub struct LinuxGccLinker {
    output_path: PathBuf,
    paths: Vec<LinkerPath>,
}

impl LinuxGccLinker {
    #[must_use]
    pub fn new(output_path: impl Into<PathBuf>) -> Self {
        Self {
            output_path: output_path.into(),
            paths: Vec::new(),
        }
    }

    pub fn add_path(&mut self, path: LinkerPath) {
        self.paths.push(path);
    }

    pub fn run(self) -> Result<(), Box<dyn Error>> {
        let mut command = Command::new("gcc");

        command.stderr(Stdio::piped());

        for path in &self.paths {
            match path {
                LinkerPath::Object(object_path) => {
                    command.arg(object_path);
                }

                LinkerPath::StaticLibrary(..) => (),
            }
        }

        for path in &self.paths {
            match path {
                LinkerPath::Object(..) => (),

                LinkerPath::StaticLibrary(lib_path) => {
                    let Some(parent) = lib_path.parent() else { continue };

                    let Some(name) = lib_path.file_name().and_then(|x| x.to_str()) else { continue };

                    let name = if name.starts_with("lib") {
                        &name[3..]
                    } else {
                        name
                    };

                    let name = name.split('.').next().unwrap_or(name);

                    command.args([OsStr::new("-L"), parent.as_os_str()]);
                    command.arg(format!("-l{name}"));
                }
            }
        }

        command.arg("-o");
        command.arg(&self.output_path);

        let mut process = command.spawn()?;
        let exit_status = process.wait()?;

        if !exit_status.success() {
            let mut description = String::new();
            _ = process.stderr.unwrap().read_to_string(&mut description);
            eprintln!("{description}");
            return Err(LinkerError::UnexpectedError(description).into());
        }

        Ok(())
    }
}
