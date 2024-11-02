// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{error::Error, fmt::Display, io::Read, mem::replace, path::{Path, PathBuf}, process::{Command, Stdio}};

use babbelaar::{Constants, ParseTree};

use crate::{AArch64CodeGenerator, CompiledObject, Compiler, Function, Platform};

#[derive(Debug)]
pub struct Pipeline {
    object: CompiledObject,
    paths_to_objects: Vec<PathBuf>,
}

impl Pipeline {
    #[must_use]
    pub fn new(platform: Platform) -> Self {
        Self {
            object: CompiledObject::new(platform),
            paths_to_objects: Vec::new(),
        }
    }

    pub fn compile_trees(&mut self, trees: &[ParseTree]) {
        let mut compiler = Compiler::new();
        compiler.compile_trees(trees);

        let program = compiler.finish();

        for function in program.functions() {
            self.code_gen(function);
        }
    }

    fn code_gen(&mut self, function: &Function) {
        let function = AArch64CodeGenerator::compile(function);
        self.object.add_function(function);
    }

    pub fn create_object(&mut self, directory: &Path, name: &str) -> Result<(), Box<dyn Error>> {
        let mut path = directory.to_path_buf();
        path.push(format!("{name}.{}", Constants::OBJECT_FILE_EXTENSION));

        let obj = CompiledObject::new(self.object.platform().clone());
        let obj = replace(&mut self.object, obj);

        obj.write_to(&path)?;

        self.paths_to_objects.push(path);
        Ok(())
    }

    /// TODO: this is solely macOS-specific
    pub fn link_to_executable(&mut self, directory: &Path, name: &str) -> Result<PathBuf, Box<dyn Error>> {
        let mut path = directory.to_path_buf();

        let ext = self.object.platform().operating_system().executable_extension();
        path.push(format!("{name}{ext}"));

        let mut command = Command::new("ld");

        command.stderr(Stdio::piped());

        for object_path in &self.paths_to_objects {
            command.arg(object_path);
        }

        command.arg("-o");
        command.arg(&path);

        command.arg("-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib");
        command.arg("-lSystem");

        let mut process = command.spawn()?;
        let exit_status = process.wait()?;

        if !exit_status.success() {
            let mut description = String::new();
            _ = process.stderr.unwrap().read_to_string(&mut description);
            return Err(LinkerError::UnexpectedError(description).into());
        }

        Ok(path)
    }
}


#[derive(Debug)]
pub enum LinkerError {
    UnexpectedError(String),
}

impl Display for LinkerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedError(description) => {
                f.write_str("onverwachte fout: ")?;
                f.write_str(&description)
            }
        }
    }
}

impl Error for LinkerError {}
