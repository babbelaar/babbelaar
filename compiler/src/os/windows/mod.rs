// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{error::Error, ffi::OsString, fs::read_dir, io::{self, Read}, path::{Path, PathBuf}, process::{Command, Stdio}};

use vs::VisualStudio;

use crate::{Architecture, LinkerError, Platform};

mod vs;


pub struct WindowsLibLinker {
    platform: Platform,
    output_path: PathBuf,
    object_paths: Vec<PathBuf>,
}

impl WindowsLibLinker {
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
        let mut command = Command::new(find_linker_path(&self.platform, "lib.exe").unwrap());

        let mut visual_studio = VisualStudio::new(self.output_path.parent().unwrap())?;

        command.stderr(Stdio::piped());
        command.stdout(Stdio::piped());

        command.env("EXTERNAL_INCLUDE", visual_studio.external_include()?);
        command.env("INCLUDE", visual_studio.include()?);
        command.env("LIB", visual_studio.lib()?);
        command.env("LIBPATH", visual_studio.libpath()?);

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
            _ = process.stdout.unwrap().read_to_string(&mut description);
            return Err(LinkerError::UnexpectedError(description).into());
        }

        Ok(())
    }
}

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
        let mut command = Command::new(find_linker_path(&self.platform, "link.exe").unwrap());

        let mut visual_studio = VisualStudio::new(self.output_path.parent().unwrap())?;

        command.stderr(Stdio::piped());
        command.stdout(Stdio::piped());

        command.arg("/SUBSYSTEM:CONSOLE");
        command.arg("/ENTRY:hoofd");
        command.arg("/EXPORT:hoofd");
        command.arg("/debug");

        command.env("EXTERNAL_INCLUDE", visual_studio.external_include()?);
        command.env("INCLUDE", visual_studio.include()?);
        command.env("LIB", visual_studio.lib()?);
        command.env("LIBPATH", visual_studio.libpath()?);

        for object_path in &self.object_paths {
            command.arg(object_path);
        }

        for lib in "ws2_32.lib msvcrt.lib ucrt.lib legacy_stdio_definitions.lib vcruntime.lib kernel32.lib user32.lib gdi32.lib winspool.lib shell32.lib ole32.lib oleaut32.lib uuid.lib comdlg32.lib advapi32.lib Userenv.lib ntdll.lib".split(' ') {
            command.arg(lib);
        }

        let mut output_arg = OsString::from("/OUT:");
        output_arg.push(self.output_path.as_os_str());

        command.arg(output_arg);

        let mut process = command.spawn()?;
        let exit_status = process.wait()?;

        if !exit_status.success() {
            let mut description = String::new();
            _ = process.stderr.unwrap().read_to_string(&mut description);
            _ = process.stdout.unwrap().read_to_string(&mut description);
            return Err(LinkerError::UnexpectedError(description).into());
        }

        Ok(())
    }
}

#[must_use]
fn find_linker_path(platform: &Platform, name: &str) -> Result<PathBuf, io::Error> {
    let mut root_dir = r#"C:\Program Files\Microsoft Visual Studio\2022\Community"#.to_string();

    if !Path::new(root_dir.as_str()).exists() {
        let mut vs = VisualStudio::new(Path::new(root_dir.as_str())).unwrap();
        vs.path_to_install_dir().unwrap().clone_into(&mut root_dir);
    }

    root_dir.push_str(r"\VC\Tools\MSVC");

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

            path.push(name);

            return Ok(path);
        }
    }

    panic!("Cannot find linker")
}
