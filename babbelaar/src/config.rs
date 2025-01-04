// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use serde::Deserialize;

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(default)]
pub struct ConfigRoot {
    pub log: ConfigSectionLog,
    pub project: ConfigSectionProject,
    pub arch: ConfigSectionArch,
}

#[derive(Debug, Clone, Default, Deserialize)]
pub struct ConfigSectionArch {
    pub aarch64: ConfigSectionArchAarch64,
}

#[derive(Debug, Clone, Default, Deserialize)]
pub struct ConfigSectionArchAarch64 {
    /// Pointer authentication
    pub arm64e: bool,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(default)]
pub struct ConfigSectionLog {
    pub debug: bool,
}

#[derive(Debug, Clone, Default, Deserialize)]
#[serde(default)]
pub struct ConfigSectionProject {
    pub naam: String,
}
