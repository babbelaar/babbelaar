// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{error::Error, fmt::Display, mem::replace, path::{Path, PathBuf}};

use babbelaar::ParseTree;
use log::debug;

use crate::{backend::Amd64CodeGenerator, os::{linux::LinuxGccLinker, macos::MacOsLdLinker, windows::WindowsLinkLinker}, AArch64CodeGenerator, Architecture, CompiledObject, Compiler, Function, OperatingSystem, Platform};

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

        let mut program = compiler.finish();
        debug!("Program: {program}");

        for function in program.functions() {
            self.code_gen(function);
        }

        self.object.set_function_aliases(program.take_function_aliases());
        self.object.set_read_only_data(program.take_read_only_data());
    }

    fn code_gen(&mut self, function: &Function) {
        let function = match self.object.platform().architecture() {
            Architecture::AArch64 => {
                AArch64CodeGenerator::compile(function, self.object.platform().clone())
            }

            Architecture::X86_64 => {
                Amd64CodeGenerator::compile(function, self.object.platform().clone())
            }
        };
        self.object.add_function(function);
    }

    pub fn create_object(&mut self, directory: &Path, name: &str) -> Result<(), Box<dyn Error>> {
        let mut path = directory.to_path_buf();
        path.push(format!("{name}.{}", self.object.platform().operating_system().object_extension()));

        debug!("Objectpad: {}", path.display());



        let obj = CompiledObject::new(self.object.platform().clone());
        let obj = replace(&mut self.object, obj);

        obj.write_to(&path)?;

        self.paths_to_objects.push(path);
        Ok(())
    }

    pub fn link_to_executable(&mut self, directory: &Path, name: &str) -> Result<PathBuf, Box<dyn Error>> {
        let mut path = directory.to_path_buf();

        let ext = self.object.platform().operating_system().executable_extension();
        path.push(format!("{name}{ext}"));

        match self.object.platform().operating_system() {
            OperatingSystem::Linux => {
                let mut linker = LinuxGccLinker::new(&path);

                for path in &self.paths_to_objects {
                    linker.add_object(path);
                }

                linker.run()?;
            }

            OperatingSystem::MacOs => {
                let mut linker = MacOsLdLinker::new(&path);

                for path in &self.paths_to_objects {
                    linker.add_object(path);
                }

                linker.run()?;
            }

            OperatingSystem::Windows => {
                let mut linker = WindowsLinkLinker::new(self.object.platform().clone(), &path);

                for path in &self.paths_to_objects {
                    linker.add_object(path);
                }

                linker.run()?;
            }
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
