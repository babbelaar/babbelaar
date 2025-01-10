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
        command.stdout(Stdio::piped());

        command.arg("/SUBSYSTEM:CONSOLE");
        command.arg("/ENTRY:babbelaar_hoofd");
        command.arg("/EXPORT:babbelaar_hoofd");
        command.arg("/debug");

        command.env("EXTERNAL_INCLUDE", r#"C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.41.34120\include;C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.41.34120\ATLMFC\include;C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\VS\include;C:\Program Files (x86)\Windows Kits\10\include\10.0.22621.0\ucrt;C:\Program Files (x86)\Windows Kits\10\\include\10.0.22621.0\\um;C:\Program Files (x86)\Windows Kits\10\\include\10.0.22621.0\\shared;C:\Program Files (x86)\Windows Kits\10\\include\10.0.22621.0\\winrt;C:\Program Files (x86)\Windows Kits\10\\include\10.0.22621.0\\cppwinrt;C:\Program Files (x86)\Windows Kits\NETFXSDK\4.8\include\um"#);
        command.env("INCLUDE", r#"C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.41.34120\include;C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.41.34120\ATLMFC\include;C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\VS\include;C:\Program Files (x86)\Windows Kits\10\include\10.0.22621.0\ucrt;C:\Program Files (x86)\Windows Kits\10\\include\10.0.22621.0\\um;C:\Program Files (x86)\Windows Kits\10\\include\10.0.22621.0\\shared;C:\Program Files (x86)\Windows Kits\10\\include\10.0.22621.0\\winrt;C:\Program Files (x86)\Windows Kits\10\\include\10.0.22621.0\\cppwinrt;C:\Program Files (x86)\Windows Kits\NETFXSDK\4.8\include\um"#);
        command.env("LIB", r#"C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.41.34120\ATLMFC\lib\x64;C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.41.34120\lib\x64;C:\Program Files (x86)\Windows Kits\NETFXSDK\4.8\lib\um\x64;C:\Program Files (x86)\Windows Kits\10\lib\10.0.22621.0\ucrt\x64;C:\Program Files (x86)\Windows Kits\10\\lib\10.0.22621.0\\um\x64"#);
        command.env("LIBPATH", r#"C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.41.34120\ATLMFC\lib\x64;C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.41.34120\lib\x64;C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.41.34120\lib\x86\store\references;C:\Program Files (x86)\Windows Kits\10\UnionMetadata\10.0.22621.0;C:\Program Files (x86)\Windows Kits\10\References\10.0.22621.0;C:\Windows\Microsoft.NET\Framework64\v4.0.30319"#);

        command.arg(find_builtin_lib_path().unwrap());

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

fn find_builtin_lib_path() -> Option<PathBuf> {
    let mut exe = std::env::current_exe().unwrap();

    while exe.pop() {
        if exe.file_name().unwrap_or_default() == "target" {
            break;
        }
    }

    exe.push("debug");
    exe.push("babbelaar_builtin.lib");
    Some(exe)
}

