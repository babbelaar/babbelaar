// Copyright (C) 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, fs::{File, remove_file}, io::Write, path::{Path, PathBuf}, process::{Command, Stdio}, rc::Rc, str::Utf8Error};

use log::{debug, warn};
use thiserror::Error;

pub struct VisualStudio {
    temp_dir: PathBuf,
    path_to_vs_where: PathBuf,
    path_to_install_dir: Option<Result<String, VisualStudioError>>,
    vs_dev_cmd_contents: Option<HashMap<String, String>>,
}

impl VisualStudio {
    #[must_use]
    pub fn new(temp_dir: &Path) -> Result<Self, VisualStudioError> {
        if !cfg!(target_os = "windows") {
            return Err(VisualStudioError::NotWindowsPlatform);
        }

        // https://github.com/microsoft/vswhere/wiki#installing
        let mut path_to_vs_where = get_program_files_x86();
        path_to_vs_where.push("Microsoft Visual Studio");
        path_to_vs_where.push("Installer");
        path_to_vs_where.push("vswhere.exe");

        Ok(Self {
            temp_dir: std::env::var("BABBELAAR_METAGEGEVENS_MAP").map(PathBuf::from).unwrap_or_else(|_| temp_dir.to_path_buf()),
            path_to_vs_where,
            path_to_install_dir: None,
            vs_dev_cmd_contents: None,
        })
    }

    #[must_use]
    pub fn path_to_install_dir(&mut self) -> Result<&str, VisualStudioError> {
        match self.path_to_install_dir.get_or_insert_with(|| impl_path_to_install_dir(&self.path_to_vs_where)) {
            Ok(path) => Ok(&*path),
            Err(e) => Err(e.clone())
        }
    }

    #[must_use]
    pub fn external_include(&mut self) -> Result<&str, VisualStudioError> {
        self.vs_dev_env("EXTERNAL_INCLUDE")
    }

    #[must_use]
    pub fn include(&mut self) -> Result<&str, VisualStudioError> {
        self.vs_dev_env("INCLUDE")
    }

    #[must_use]
    pub fn lib(&mut self) -> Result<&str, VisualStudioError> {
        self.vs_dev_env("LIB")
    }

    #[must_use]
    pub fn libpath(&mut self) -> Result<&str, VisualStudioError> {
        self.vs_dev_env("LIBPATH")
    }

    fn vs_dev_env(&mut self, key: &'static str) -> Result<&str, VisualStudioError> {
        self.vs_dev_cmd_contents()?.get(key).map(|x| x.as_str()).ok_or_else(|| VisualStudioError::VsDevEnvIncomplete(key))
    }

    fn vs_dev_cmd_contents(&mut self) -> Result<&HashMap<String, String>, VisualStudioError> {
        self.do_vs_dev_cmd_contents(false)
    }

    fn do_vs_dev_cmd_contents(&mut self, retry: bool) -> Result<&HashMap<String, String>, VisualStudioError> {
        if self.vs_dev_cmd_contents.is_some() {
            return Ok(self.vs_dev_cmd_contents.as_ref().unwrap());
        }

        let mut contents_file = self.temp_dir.clone();
        contents_file.push("VsDevCmd.uitvoer");
        let was_cached = contents_file.exists();

        let contents = if was_cached {
            debug!("VsDevCmd was al een keer opgehaald en gestopt in de cache: {}", contents_file.display());
            std::fs::read_to_string(&contents_file).map_err(|x| VisualStudioError::VsDevCmdContentsFileOpenError(x.into()))?
        } else {
            let mut batch_file_path = self.temp_dir.clone();
            batch_file_path.push("VsDevCmd.bat");
            debug!("VsDevCmd.bat invocatiescript schrijven naar '{}'...", batch_file_path.display());

            let install_dir = self.path_to_install_dir()?;
            File::create(&batch_file_path)
                .map_err(|x| VisualStudioError::VsDevCmdBatchFileCreateError(x.into()))?
                .write_all(format!("@call \"{install_dir}\\VC\\Auxiliary\\Build\\vcvars64.bat\"\r\nSET\r\nEXIT\r\n").as_bytes())
                .map_err(|x| VisualStudioError::VsDevCmdBatchFileWriteError(x.into()))?;

            let mut batch_file_path = batch_file_path.to_str().unwrap();
            let unc_prefix = "\\\\?\\";
            if batch_file_path.starts_with(unc_prefix) {
                batch_file_path = &batch_file_path[unc_prefix.len()..];
            }

            let output = Command::new("cmd.exe")
                .arg("/c")
                .arg(batch_file_path)
                .stdout(Stdio::piped())
                .output().map_err(|x| VisualStudioError::VsDevCmdBatchFileRunError(x.into()))?;

            if !output.status.success() {
                return Err(VisualStudioError::VsDevCmdBatchFileRunError2(String::from_utf8_lossy(&output.stderr).into_owned()));
            }

            let output = String::from_utf8_lossy(&output.stdout).into_owned();
            if let Err(e) = std::fs::write(&contents_file, &output) {
                log::warn!("Kon VsDevCmd-cache niet aanmaken: {e}");
            }
            output
        };

        let mut map = HashMap::new();
        let mut ready = false;
        for line in contents.lines() {
            if line.contains("Environment initialized") {
                ready = true;
                continue;
            }

            if !ready {
                continue;
            }


            let Some((key, value)) = line.split_once('=') else {
                warn!("Regel met omgevingsvariabele bevatte geen `=`-teken: {line}");
                continue;
            };

            let old_value = map.insert(key.to_string(), value.to_string());
            if let Some(old_value) = old_value {
                warn!("Omgevingsvariabele `{key}` bevatte twee waarden:");
                warn!("  Eerste: `{old_value}`");
                warn!("  Tweede: `{value}`");
            }
        }

        if !retry && was_cached && !self.validate_environment(&map) {
            _ = remove_file(contents_file);
            return self.do_vs_dev_cmd_contents(true);
        }

        self.vs_dev_cmd_contents = Some(map);
        Ok(self.vs_dev_cmd_contents.as_ref().unwrap())
    }

    #[must_use]
    fn validate_environment(&self, map: &HashMap<String, String>) -> bool {
        let Some(include) = map.get("INCLUDE") else {
            warn!("VsDevCmd validatie faalde: omgevingsvariabele `INCLUDE` bestaat niet");
            return false;
        };

        let Some(first_include_path) = include.split(';').next() else {
            warn!("VsDevCmd validatie faalde: omgevingsvariabele `INCLUDE` heeft geen inhoud");
            debug_assert!(false);
            return false;
        };

        if !Path::new(first_include_path).exists() {
            warn!("VsDevCmd validatie faalde: INCLUDE-pad bestaat niet: {first_include_path}");
            return false;
        }

        true
    }
}

fn impl_path_to_install_dir(path_to_vs_where: &Path) -> Result<String, VisualStudioError> {
    let output = Command::new(path_to_vs_where)
        .arg("-utf8")
        .arg("-nologo")
        .arg("-latest")
        .args(["-products", "*"])
        .args(["-requires", "Microsoft.VisualStudio.Component.VC.Tools.x86.x64"])
        .args(["-property", "installationPath"])
        .stdout(Stdio::piped())
        .output().map_err(|x| VisualStudioError::VsWhereError(x.into()))?
        .stdout;

    let output = String::from_utf8(output).map_err(|x| VisualStudioError::VsWhereNotUtf8(x.utf8_error()))?;
    let output = output.trim().to_string();
    debug!("Pad naar Visual Studio-installatie gevonden: '{output}'");
    if output.contains('\n') {
        log::warn!("Visual Studio-pad lijkt ongeldig: '{output}'!");
    }
    Ok(output)
}

#[must_use]
fn get_program_files_x86() -> PathBuf {
    match std::env::var("ProgramFiles(x86)") {
        Ok(path) => {
            PathBuf::from(path)
        }

        Err(..) => {
            log::warn!("Omgevingsvariabele `ProgramFiles(x86)` was niet gezet");
            PathBuf::from("C:\\Program Files (x86)\\")
        }
    }
}

#[derive(Debug, Clone, Error)]
pub enum VisualStudioError {
    #[error("Visual Studio bestaat alleen op Windows")]
    NotWindowsPlatform,

    #[error("Kon vswhere.exe niet uitvoeren: {0}")]
    VsWhereError(Rc<std::io::Error>),

    #[error("vswhere.exe gaf geen UTF-8 terug: {0}")]
    VsWhereNotUtf8(Utf8Error),

    #[error("Kon VsDevCmd-cache niet uitvragen: {0}")]
    VsDevCmdContentsFileOpenError(Rc<std::io::Error>),

    #[error("Kon VsDevCmd-hulpscript niet aanmaken: {0}")]
    VsDevCmdBatchFileCreateError(Rc<std::io::Error>),

    #[error("Kon VsDevCmd-hulpscripttekst niet schrijven: {0}")]
    VsDevCmdBatchFileWriteError(Rc<std::io::Error>),

    #[error("Kon VsDevCmd-hulpscript niet uitvoeren: {0}")]
    VsDevCmdBatchFileRunError(Rc<std::io::Error>),

    #[error("Kon VsDevCmd-hulpscript niet uitvoeren, fout: {0}")]
    VsDevCmdBatchFileRunError2(String),

    #[error("VsDevCmd was incompleet (sleutel '{0}' ontbrak!)")]
    VsDevEnvIncomplete(&'static str),
}


