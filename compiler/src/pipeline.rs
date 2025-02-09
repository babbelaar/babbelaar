// Copyright (C) 2024 - 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{error::Error, fmt::Display, mem::replace, path::{Path, PathBuf}};

use babbelaar::{ArchiveKind, ParseTree};
use log::debug;

use crate::{
    backend::Amd64CodeGenerator,
    cranelift_backend::CraneliftBackend,
    os::{
        linux::{LinuxArLinker, LinuxGccLinker},
        macos::MacOsLdLinker,
        windows::{WindowsLibLinker, WindowsLinkLinker},
    },
    AArch64CodeGenerator,
    Architecture,
    Backend,
    CompiledObject,
    Compiler,
    Function,
    LinkerPath,
    OperatingSystem,
    Platform,
};

#[derive(Debug)]
pub struct Pipeline {
    backend: Backend,
    object: CompiledObject,
    paths: Vec<LinkerPath>,
    function_counter: usize,
}

impl Pipeline {
    #[must_use]
    pub fn new(platform: Platform, include_babbib: bool) -> Self {
        let mut paths = Vec::new();

        if include_babbib {
            paths.push(LinkerPath::StaticLibrary(create_path_to_babbib(&platform)));
        }

        Self {
            backend: Backend::default(),
            object: CompiledObject::new(platform),
            paths,
            function_counter: 0,
        }
    }

    #[must_use]
    pub fn with_backend(self, backend: Backend) -> Self {
        Self {
            backend,
            ..self
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
        match self.backend {
            Backend::Babbelaar => self.code_gen_babbelaar(function),
            Backend::Cranelift => self.code_gen_cranelift(function),
        }
    }

    fn code_gen_babbelaar(&mut self, function: &Function) {
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

    fn code_gen_cranelift(&mut self, function: &Function) {
        let idx = self.function_counter;
        self.function_counter += 1;

        let function = CraneliftBackend::compile(idx, function, self.object.platform());
        self.object.add_function(function);
    }

    pub fn create_object(&mut self, directory: &Path, name: &str) -> Result<(), Box<dyn Error>> {
        let mut path = directory.to_path_buf();
        path.push(format!("{name}.{}", self.object.platform().operating_system().object_extension()));

        debug!("Objectpad: {}", path.display());



        let obj = CompiledObject::new(self.object.platform().clone());
        let obj = replace(&mut self.object, obj);

        obj.write_to(&path)?;

        self.paths.push(LinkerPath::Object(path));
        Ok(())
    }

    pub fn link(self, directory: &Path, name: &str, archive: ArchiveKind) -> Result<PathBuf, Box<dyn Error>> {
        let mut path = directory.to_path_buf();

        let prefix = self.object.platform().operating_system().archive_prefix(archive);
        let ext = self.object.platform().operating_system().archive_extension(archive);
        path.push(format!("{prefix}{name}{ext}"));

        match self.object.platform().operating_system() {
            OperatingSystem::Linux => {
                match archive {
                    ArchiveKind::Applicatie => {
                        let mut linker = LinuxGccLinker::new(&path);

                        for path in self.paths {
                            linker.add_path(path);
                        }

                        linker.run()?;
                    }

                    ArchiveKind::StatischeBibliotheek => {
                        let mut linker = LinuxArLinker::new(&path);

                        for path in self.paths {
                            linker.add_path(path);
                        }

                        linker.run()?;
                    }
                }
            }

            OperatingSystem::MacOs => {
                let mut linker = MacOsLdLinker::new(&path, archive);

                for path in &self.paths {
                    linker.add_object(&path.path_buf());
                }

                linker.run()?;
            }

            OperatingSystem::Windows => {
                match archive {
                    ArchiveKind::Applicatie => {
                        let mut linker = WindowsLinkLinker::new(self.object.platform().clone(), &path);

                        for path in self.paths {
                            linker.add_object(&path.as_path_buf());
                        }

                        linker.run()?;
                    }

                    ArchiveKind::StatischeBibliotheek => {
                        let mut linker = WindowsLibLinker::new(self.object.platform().clone(), &path);

                        for path in self.paths {
                            linker.add_object(&path.as_path_buf());
                        }

                        linker.run()?;
                    }
                }
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

#[must_use]
fn create_path_to_babbib(platform: &Platform) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.pop();

    path.push("babbib");
    path.push("uit");
    path.push(platform.architecture().name());

    let prefix = platform.operating_system().archive_prefix(ArchiveKind::StatischeBibliotheek);
    let suffix = platform.operating_system().archive_extension(ArchiveKind::StatischeBibliotheek);
    path.push(format!("{prefix}babbib{suffix}"));

    path
}
