// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Architecture {
    AArch64,
    X86_64,
}

impl Architecture {
    #[must_use]
    pub const fn endianness(&self) -> Endianness {
        Endianness::Little
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Environment {
    Darwin,
    Gnu,
    MsVC,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Endianness {
    Little,
    Big,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperatingSystem {
    Linux,
    MacOs,
    Windows,
}

impl OperatingSystem {
    #[must_use]
    pub const fn executable_extension(&self) -> &'static str {
        match self {
            Self::Windows => ".exe",
            _ => "",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Platform {
    architecture: Architecture,
    environment: Environment,
    operating_system: OperatingSystem,
}

impl Platform {
    #[must_use]
    pub fn host_platform() -> Self {
        if cfg!(target_os = "macos") {
            Self {
                architecture: Architecture::AArch64,
                environment: Environment::Darwin,
                operating_system: OperatingSystem::MacOs,
            }
        } else if cfg!(target_os = "windows") {
            Self {
                architecture: Architecture::X86_64,
                environment: Environment::Darwin,
                operating_system: OperatingSystem::MacOs,
            }
        } else {
            todo!()
        }
    }

    #[must_use]
    pub fn new(architecture: Architecture, environment: Environment, operating_system: OperatingSystem) -> Self {
        Self {
            architecture,
            environment,
            operating_system,
        }
    }

    #[must_use]
    pub fn architecture(&self) -> Architecture {
        self.architecture
    }

    #[must_use]
    pub fn environment(&self) -> Environment {
        self.environment
    }

    #[must_use]
    pub fn operating_system(&self) -> OperatingSystem {
        self.operating_system
    }
}

impl From<Architecture> for object::Architecture {
    fn from(value: Architecture) -> Self {
        match value {
            Architecture::AArch64 => object::Architecture::Aarch64,
            Architecture::X86_64 => object::Architecture::X86_64,
        }
    }
}

impl From<Endianness> for object::Endianness {
    fn from(value: Endianness) -> Self {
        match value {
            Endianness::Big => object::Endianness::Big,
            Endianness::Little => object::Endianness::Little,
        }
    }
}
