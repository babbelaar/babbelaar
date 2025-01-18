// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{error::Error, io::Read, path::{Path, PathBuf}, process::{Command, Stdio}};

use babbelaar::{ArchiveKind, Constants};

use crate::LinkerError;

pub struct MacOsLdLinker {
    output_path: PathBuf,
    object_paths: Vec<PathBuf>,
    archive_kind: ArchiveKind,
}

impl MacOsLdLinker {
    #[must_use]
    pub fn new(output_path: impl Into<PathBuf>, archive_kind: ArchiveKind) -> Self {
        Self {
            output_path: output_path.into(),
            object_paths: Vec::new(),
            archive_kind,
        }
    }

    pub fn add_object(&mut self, path: &Path) {
        self.object_paths.push(path.to_path_buf());
    }

    pub fn run(self) -> Result<(), Box<dyn Error>> {
        let mut command = Command::new("ld");

        command.stderr(Stdio::piped());

        for object_path in &self.object_paths {
            log::info!("Object: {}", object_path.display());
            command.arg(object_path);
        }

        match self.archive_kind {
            ArchiveKind::Applicatie => command.arg("-execute"),
            ArchiveKind::StatischeBibliotheek => command.arg("-r"),
        };

        command.arg("-o");
        command.arg(&self.output_path);

        command.arg("-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib");
        command.arg("-lSystem");

        let version = format!("{}.{}", Constants::MACOS_MINIMUM_VERSION.major, Constants::MACOS_MINIMUM_VERSION.minor);
        command.args(["-platform_version", "macos", &version, &version]);

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
