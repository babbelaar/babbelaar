// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use babbelaar::BabString;
use object::{macho, RelocationEncoding, RelocationFlags, RelocationKind};

use super::{aarch64::ArmInstruction, DataSectionKind};

#[derive(Debug, Clone, PartialEq)]
pub struct Relocation {
    pub(super) ty: RelocationType,
    pub(super) offset: usize,
    pub(super) method: RelocationMethod,
}

impl Relocation {
    #[must_use]
    pub fn ty(&self) -> &RelocationType {
        &self.ty
    }

    #[must_use]
    pub fn offset(&self) -> usize {
        self.offset
    }

    #[must_use]
    pub(crate) fn flags(&self) -> RelocationFlags {
        self.method.relocation_flags()
    }

    pub(crate) fn addend(&self) -> i64 {
        self.method.addend()
    }

    pub fn write(&self, code: &mut [u8], offset: isize) {
        let code = &mut code[self.offset..];

        match self.method {
            RelocationMethod::AArch64BranchLink => {
                let offset = offset / 4;

                let instruction = ArmInstruction::Bl { offset: offset as i32, symbol_name: BabString::empty() }
                    .encode(0, &HashMap::new());
                code[0..4].copy_from_slice(&instruction.to_ne_bytes());
            }

            RelocationMethod::Aarch64PageOff12 => {
                let orig = u64_from_slice(code);
                let val = (orig & 0xFFFFF000) | (offset as u64 & 0xFFF);
                code[0..8].copy_from_slice(&val.to_le_bytes());
            }

            RelocationMethod::Aarch64Page21 => {
                let orig = u64_from_slice(code);
                let val = (orig & 0xFFF00000) | (offset as u64 & 0xFFFFF);
                code[0..8].copy_from_slice(&val.to_le_bytes());
            }

            RelocationMethod::Amd64CallNearRelative => {
                let offset = offset as u32 - 4;
                code[0..4].copy_from_slice(&offset.to_le_bytes());
            }

            RelocationMethod::Amd64RipRelative => {
                let offset = offset as u32 - 4;
                code[0..4].copy_from_slice(&offset.to_le_bytes());
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RelocationType {
    Data {
        section: DataSectionKind,
        offset: usize,
    },
    Function {
        name: BabString,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum RelocationMethod {
    AArch64BranchLink,
    Aarch64PageOff12,
    Aarch64Page21,
    Amd64CallNearRelative,
    Amd64RipRelative,
}

impl RelocationMethod {
    #[must_use]
    pub fn addend(&self) -> i64 {
        match self {
            Self::AArch64BranchLink => 0,
            Self::Aarch64PageOff12 => 0,
            Self::Aarch64Page21 => 0,
            Self::Amd64CallNearRelative => -4,
            Self::Amd64RipRelative => -4,
        }
    }

    #[must_use]
    fn relocation_flags(&self) -> RelocationFlags {
        match self {
            RelocationMethod::AArch64BranchLink => {
                RelocationFlags::Generic {
                    kind: RelocationKind::Relative,
                    encoding: RelocationEncoding::AArch64Call,
                    size: 32,
                }
            },

            RelocationMethod::Aarch64PageOff12 => {
                RelocationFlags::MachO {
                    r_type: macho::ARM64_RELOC_PAGEOFF12,
                    r_pcrel: false,
                    r_length: 2,
                }
            },

            RelocationMethod::Aarch64Page21 => {
                RelocationFlags::MachO {
                    r_type: macho::ARM64_RELOC_PAGE21,
                    r_pcrel: true,
                    r_length: 2,
                }
            },

            RelocationMethod::Amd64CallNearRelative => {
                RelocationFlags::Generic {
                    kind: RelocationKind::Relative,
                    encoding: RelocationEncoding::X86RipRelative,
                    size: 32,
                }
            },

            RelocationMethod::Amd64RipRelative => {
                RelocationFlags::Generic {
                    kind: RelocationKind::Relative,
                    encoding: RelocationEncoding::Generic,
                    size: 32,
                }
            }
        }
    }
}

#[must_use]
fn u64_from_slice(slice: &[u8]) -> u64 {
    let mut bytes = [0; 8];
    bytes.copy_from_slice(&slice[0..8]);
    u64::from_le_bytes(bytes)
}
