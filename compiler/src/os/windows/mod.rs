// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{error::Error, ffi::OsString, fs::read_dir, io::{self, Read}, path::{Path, PathBuf}, process::{Command, Stdio}};

use crate::{Architecture, LinkerError, Platform};

pub struct WindowsLinkLinker {
    platform: Platform,
    output_path: PathBuf,
    object_paths: Vec<PathBuf>,
}

impl WindowsLinkLinker {
    #[must_use]
    pub fn new(platform: Platform, output_path: impl Into<PathBuf>) -> Self {
        Self {
            platform,
            output_path: output_path.into(),
            object_paths: Vec::new(),
        }
    }

    pub fn add_object(&mut self, path: &Path) {
        self.object_paths.push(path.to_path_buf());
    }

    pub fn run(self) -> Result<(), Box<dyn Error>> {
        let mut command = Command::new(find_linker_path(&self.platform).unwrap());

        command.stderr(Stdio::piped());

        command.arg("/SUBSYSTEM:CONSOLE");
        command.arg("/ENTRY:main");
        command.arg("/EXPORT:main");

        for object_path in &self.object_paths {
            command.arg(object_path);
        }

        let mut output_arg = OsString::from("/OUT:");
        output_arg.push(self.output_path.as_os_str());

        command.arg(output_arg);

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

#[must_use]
fn find_linker_path(platform: &Platform) -> Result<PathBuf, io::Error> {
    // TODO: use vswhere/envvars to resolve this
    let root_dir = r#"C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC"#;

    for entry in read_dir(root_dir)?.flatten() {
        if entry.file_type().is_ok_and(|x| x.is_dir()) {
            let mut path = entry.path();
            path.push("bin");

            if cfg!(target_arch = "x86_64") {
                path.push(format!("Hostx64"));
            } else {
                panic!("Unsupported host architecture!");
            }

            match platform.architecture() {
                Architecture::AArch64 => path.push("arm64"),
                Architecture::X86_64 => path.push("x64"),
            }

            path.push("link.exe");

            return Ok(path);
        }
    }

    panic!("Cannot find linker")
}
