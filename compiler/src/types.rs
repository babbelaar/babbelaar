// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashSet;

use object::pe::{IMAGE_FILE_MACHINE_AMD64, IMAGE_FILE_MACHINE_ARM64};

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

    #[must_use]
    pub const fn is_64_bit(&self) -> bool {
        true
    }

    /// See <https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#machine-types>
    #[must_use]
    pub const fn windows_machine_type(&self) -> u16 {
        match self {
            Self::AArch64 => IMAGE_FILE_MACHINE_ARM64,
            Self::X86_64 => IMAGE_FILE_MACHINE_AMD64,
        }
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

    #[must_use]
    pub const fn object_extension(&self) -> &'static str {
        match self {
            Self::Linux => "o",
            Self::MacOs => "o",
            Self::Windows => "obj",
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
                environment: Environment::MsVC,
                operating_system: OperatingSystem::Windows,
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

#[derive(Debug, Clone)]
pub struct Graph<N: Eq> {
    nodes: Vec<N>,
    edges: HashSet<GraphEdge>
}

impl<N: Eq> Graph<N> {
    pub fn add_node(&mut self, node: N) {
        _ = self.add_or_get_node(node);
    }

    #[must_use]
    fn add_or_get_node(&mut self, node: N) -> usize {
        if let Some(pos) = self.nodes.iter().position(|n| *n == node) {
            pos
        } else {
            let idx = self.nodes.len();
            self.nodes.push(node);
            idx
        }
    }

    /// Returns:
    /// - `true` if the edge was newly inserted
    /// - `false` if there already was an edge between `from` and `to`
    pub fn add_edge(&mut self, from: N, to: N) -> bool {
        let from = self.add_or_get_node(from);
        let to = self.add_or_get_node(to);
        self.edges.insert(GraphEdge {
            from,
            to,
        })
    }

    #[must_use]
    pub fn has_edges_to(&self, node: &N) -> bool {
        let Some(pos) = self.nodes.iter().position(|n| n == node) else {
            return false;
        };

        self.edges.iter()
            .find(|x| x.to == pos)
            .is_some()
    }
}

impl<N: Eq> Default for Graph<N> {
    fn default() -> Self {
        Self {
            nodes: Default::default(),
            edges: Default::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct GraphEdge {
    from: usize,
    to: usize,
}

/// See [Microsoft Learn: Operating System Version](https://learn.microsoft.com/nl-nl/windows/win32/sysinfo/operating-system-version?redirectedfrom=MSDN)
#[derive(Debug, Clone, Copy)]
pub struct WindowsVersion {
    major: u16,
    minor: u16,
}

#[allow(unused)]
impl WindowsVersion {
    pub const WINDOWS_11: Self = Self { major: 10, minor: 0};
    pub const WINDOWS_10: Self = Self { major: 10, minor: 0};
    pub const WINDOWS_SERVER_2022: Self = Self { major: 10, minor: 0};
    pub const WINDOWS_SERVER_2019: Self = Self { major: 10, minor: 0};
    pub const WINDOWS_SERVER_2016: Self = Self { major: 10, minor: 0};
    pub const WINDOWS_8_1: Self = Self { major: 6, minor: 3};
    pub const WINDOWS_SERVER_2012_R2: Self = Self { major: 6, minor: 3};
    pub const WINDOWS_8: Self = Self { major: 6, minor: 2};
    pub const WINDOWS_SERVER_2012: Self = Self { major: 6, minor: 2};
    pub const WINDOWS_7: Self = Self { major: 6, minor: 1};
    pub const WINDOWS_SERVER_2008_R2: Self = Self { major: 6, minor: 1};
    pub const WINDOWS_SERVER_2008: Self = Self { major: 6, minor: 0};
    pub const WINDOWS_VISTA: Self = Self { major: 6, minor: 0};
    pub const WINDOWS_SERVER_2003_R2: Self = Self { major: 5, minor: 2};
    pub const WINDOWS_SERVER_2003: Self = Self { major: 5, minor: 2};
    pub const WINDOWS_XP_64_BIT_EDITION: Self = Self { major: 5, minor: 2};
    pub const WINDOWS_XP: Self = Self { major: 5, minor: 1};
    pub const WINDOWS_2000: Self = Self { major: 5, minor: 0};

    #[must_use]
    pub const fn major(&self) -> u16 {
        self.major
    }

    #[must_use]
    pub const fn minor(&self) -> u16 {
        self.minor
    }
}
