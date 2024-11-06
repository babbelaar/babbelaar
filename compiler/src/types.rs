// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashSet;

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
