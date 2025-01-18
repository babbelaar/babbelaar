// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use serde::Deserialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ArchiveKind {
    #[default]
    Applicatie,
    StatischeBibliotheek,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(default)]
#[serde(rename_all = "kebab-case")]
pub struct ConfigRoot {
    pub log: ConfigSectionLog,
    pub project: ConfigSectionProject,
    pub arch: ConfigSectionArch,
    pub bouwen: ConfigSectionBuild,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(default)]
#[serde(rename_all = "kebab-case")]
pub struct ConfigSectionArch {
    pub aarch64: ConfigSectionArchAarch64,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(default)]
#[serde(rename_all = "kebab-case")]
pub struct ConfigSectionBuild {
    pub geen_babbib: bool,
    pub alle_architecturen: bool,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(default)]
#[serde(rename_all = "kebab-case")]
pub struct ConfigSectionArchAarch64 {
    /// Pointer authentication
    pub arm64e: bool,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(default)]
#[serde(rename_all = "kebab-case")]
pub struct ConfigSectionLog {
    pub debug: bool,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(default)]
#[serde(rename_all = "kebab-case")]
pub struct ConfigSectionProject {
    pub naam: String,
    pub r#type: ArchiveKind,
}
