// Copyright (C) 2024 - 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{borrow::Cow, fmt::Display};

use crate::{backend::abstract_register::AbstractRegister, AllocatableRegister, Environment, Platform};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum Amd64Register {
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rsi,
    Rdi,
    Rbp,
    Rsp,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Amd64Register {
    #[must_use]
    pub const fn name64(&self) -> &'static str {
        self.name(Amd64RegisterNameMode::Quad)
    }

    #[must_use]
    pub const fn name32(&self) -> &'static str {
        self.name(Amd64RegisterNameMode::Long)
    }

    #[must_use]
    #[allow(unused)]
    pub const fn name16(&self) -> &'static str {
        self.name(Amd64RegisterNameMode::Word)
    }

    #[must_use]
    #[allow(unused)]
    pub const fn name8(&self) -> &'static str {
        self.name(Amd64RegisterNameMode::Byte)
    }

    #[must_use]
    pub const fn mod_rm_bits(&self) -> u8 {
        match self {
            Self::Rax | Self::R8 => 0b000,
            Self::Rcx | Self::R9 => 0b001,
            Self::Rdx | Self::R10 => 0b010,
            Self::Rbx | Self::R11 => 0b011,
            Self::Rsp | Self::R12 => 0b100,
            Self::Rbp | Self::R13 => 0b101,
            Self::Rsi | Self::R14 => 0b110,
            Self::Rdi | Self::R15 => 0b111,
        }
    }

    #[must_use]
    pub const fn sib_bits(&self) -> u8 {
        self.mod_rm_bits()
    }

    #[must_use]
    pub const fn name(&self, mode: Amd64RegisterNameMode) -> &'static str {
        match (mode, self) {
            (Amd64RegisterNameMode::Quad, Self::Rax) => "rax",
            (Amd64RegisterNameMode::Long, Self::Rax) => "eax",
            (Amd64RegisterNameMode::Word, Self::Rax) => "ax",
            (Amd64RegisterNameMode::Byte, Self::Rax) => "al",
            (Amd64RegisterNameMode::Quad, Self::Rbx) => "rbx",
            (Amd64RegisterNameMode::Long, Self::Rbx) => "ebx",
            (Amd64RegisterNameMode::Word, Self::Rbx) => "bx",
            (Amd64RegisterNameMode::Byte, Self::Rbx) => "bl",
            (Amd64RegisterNameMode::Quad, Self::Rcx) => "rcx",
            (Amd64RegisterNameMode::Long, Self::Rcx) => "ecx",
            (Amd64RegisterNameMode::Word, Self::Rcx) => "cx",
            (Amd64RegisterNameMode::Byte, Self::Rcx) => "cl",
            (Amd64RegisterNameMode::Quad, Self::Rdx) => "rdx",
            (Amd64RegisterNameMode::Long, Self::Rdx) => "edx",
            (Amd64RegisterNameMode::Word, Self::Rdx) => "dx",
            (Amd64RegisterNameMode::Byte, Self::Rdx) => "dl",
            (Amd64RegisterNameMode::Quad, Self::Rsi) => "rsi",
            (Amd64RegisterNameMode::Long, Self::Rsi) => "esi",
            (Amd64RegisterNameMode::Word, Self::Rsi) => "si",
            (Amd64RegisterNameMode::Byte, Self::Rsi) => "sil",
            (Amd64RegisterNameMode::Quad, Self::Rdi) => "rdi",
            (Amd64RegisterNameMode::Long, Self::Rdi) => "edi",
            (Amd64RegisterNameMode::Word, Self::Rdi) => "di",
            (Amd64RegisterNameMode::Byte, Self::Rdi) => "dil",
            (Amd64RegisterNameMode::Quad, Self::Rbp) => "rbp",
            (Amd64RegisterNameMode::Long, Self::Rbp) => "ebp",
            (Amd64RegisterNameMode::Word, Self::Rbp) => "bp",
            (Amd64RegisterNameMode::Byte, Self::Rbp) => "bpl",
            (Amd64RegisterNameMode::Quad, Self::Rsp) => "rsp",
            (Amd64RegisterNameMode::Long, Self::Rsp) => "esp",
            (Amd64RegisterNameMode::Word, Self::Rsp) => "sp",
            (Amd64RegisterNameMode::Byte, Self::Rsp) => "spl",
            (Amd64RegisterNameMode::Quad, Self::R8) => "r8",
            (Amd64RegisterNameMode::Long, Self::R8) => "r8l",
            (Amd64RegisterNameMode::Word, Self::R8) => "r8w",
            (Amd64RegisterNameMode::Byte, Self::R8) => "r8b",
            (Amd64RegisterNameMode::Quad, Self::R9) => "r9",
            (Amd64RegisterNameMode::Long, Self::R9) => "r9l",
            (Amd64RegisterNameMode::Word, Self::R9) => "r9w",
            (Amd64RegisterNameMode::Byte, Self::R9) => "r9b",
            (Amd64RegisterNameMode::Quad, Self::R10) => "r10",
            (Amd64RegisterNameMode::Long, Self::R10) => "r10l",
            (Amd64RegisterNameMode::Word, Self::R10) => "r10w",
            (Amd64RegisterNameMode::Byte, Self::R10) => "r10b",
            (Amd64RegisterNameMode::Quad, Self::R11) => "r11",
            (Amd64RegisterNameMode::Long, Self::R11) => "r11l",
            (Amd64RegisterNameMode::Word, Self::R11) => "r11w",
            (Amd64RegisterNameMode::Byte, Self::R11) => "r11b",
            (Amd64RegisterNameMode::Quad, Self::R12) => "r12",
            (Amd64RegisterNameMode::Long, Self::R12) => "r12l",
            (Amd64RegisterNameMode::Word, Self::R12) => "r12w",
            (Amd64RegisterNameMode::Byte, Self::R12) => "r12b",
            (Amd64RegisterNameMode::Quad, Self::R13) => "r13",
            (Amd64RegisterNameMode::Long, Self::R13) => "r13l",
            (Amd64RegisterNameMode::Word, Self::R13) => "r13w",
            (Amd64RegisterNameMode::Byte, Self::R13) => "r13b",
            (Amd64RegisterNameMode::Quad, Self::R14) => "r14",
            (Amd64RegisterNameMode::Long, Self::R14) => "r14l",
            (Amd64RegisterNameMode::Word, Self::R14) => "r14w",
            (Amd64RegisterNameMode::Byte, Self::R14) => "r14b",
            (Amd64RegisterNameMode::Quad, Self::R15) => "r15",
            (Amd64RegisterNameMode::Long, Self::R15) => "r15l",
            (Amd64RegisterNameMode::Word, Self::R15) => "r15w",
            (Amd64RegisterNameMode::Byte, Self::R15) => "r15b",
        }
    }

    #[must_use]
    pub const fn is_64_extended_register(&self) -> bool {
        *self as u8 >= Self::R8 as u8
    }

    /// <https://wiki.osdev.org/X86-64_Instruction_Encoding#Registers>
    #[must_use]
    pub fn has_8bit_lo_and_hi(&self) -> bool {
        match self {
            Self::Rsp => true,
            Self::Rbp => true,
            Self::Rsi => true,
            Self::Rdi => true,
            _ => false,
        }
    }
}

impl AllocatableRegister for Amd64Register {
    fn return_register(platform: &Platform) -> Self {
        _ = platform;
        Self::Rax
    }

    fn count() -> usize {
        24
    }

    fn callee_saved_registers(platform: &Platform) -> &'static [Amd64Register] {
        match platform.environment() {
            Environment::Darwin => {
                const REGISTERS: &'static [Amd64Register] = &[
                    Amd64Register::Rbx,
                    // Amd64Register::Rsp,
                    // Amd64Register::Rbp,
                    Amd64Register::R12,
                    Amd64Register::R13,
                    Amd64Register::R14,
                    Amd64Register::R15,
                ];

                REGISTERS
            }

            Environment::Gnu => {
                const REGISTERS: &'static [Amd64Register] = &[
                    Amd64Register::Rbx,
                    // Amd64Register::Rsp,
                    // Amd64Register::Rbp,
                    Amd64Register::R12,
                    Amd64Register::R13,
                    Amd64Register::R14,
                    Amd64Register::R15,
                ];

                REGISTERS
            }

            Environment::MsVC => {
                const REGISTERS: &'static [Amd64Register] = &[
                    Amd64Register::Rbx,
                    // Amd64Register::Rbp,
                    Amd64Register::Rdi,
                    Amd64Register::Rsi,
                    // Amd64Register::Rsp,
                    Amd64Register::R12,
                    Amd64Register::R13,
                    Amd64Register::R14,
                    Amd64Register::R15,
                ];

                REGISTERS
            }
        }
    }

    fn caller_saved_registers(platform: &Platform) -> &'static [Amd64Register] {
        match platform.environment() {
            Environment::Darwin => {
                const REGISTERS: &'static [Amd64Register] = &[
                    Amd64Register::Rdi,
                    Amd64Register::Rsi,
                    Amd64Register::Rdx,
                    Amd64Register::Rcx,
                    Amd64Register::R8,
                    Amd64Register::R9,
                    Amd64Register::Rax,
                    Amd64Register::R11,
                    Amd64Register::R10,
                ];

                REGISTERS
            }

            Environment::Gnu => {
                const REGISTERS: &'static [Amd64Register] = &[
                    Amd64Register::Rdi,
                    Amd64Register::Rsi,
                    Amd64Register::Rdx,
                    Amd64Register::Rcx,
                    Amd64Register::R8,
                    Amd64Register::R9,
                    Amd64Register::Rax,
                    Amd64Register::R11,
                    Amd64Register::R10,
                ];

                REGISTERS
            }

            Environment::MsVC => {
                const REGISTERS: &'static [Amd64Register] = &[
                    Amd64Register::Rax,
                    Amd64Register::Rcx,
                    Amd64Register::Rdx,
                    Amd64Register::R8,
                    Amd64Register::R9,
                    Amd64Register::R10,
                    Amd64Register::R11,
                ];

                REGISTERS
            }
        }
    }

    fn argument_nth_opt(platform: &Platform, n: usize) -> Option<Self> {
        Some(match platform.environment() {
            Environment::Darwin => {
                match n {
                    0 => Self::Rdi,
                    1 => Self::Rsi,
                    2 => Self::Rdx,
                    3 => Self::Rcx,
                    4 => Self::R8,
                    5 => Self::R9,
                    _ => return None,
                }
            }

            Environment::Gnu => {
                match n {
                    0 => Self::Rdi,
                    1 => Self::Rsi,
                    2 => Self::Rdx,
                    3 => Self::Rcx,
                    4 => Self::R8,
                    5 => Self::R9,
                    _ => return None,
                }
            }

            Environment::MsVC => {
                match n {
                    0 => Self::Rcx,
                    1 => Self::Rdx,
                    2 => Self::R8,
                    3 => Self::R9,
                    _ => return None,
                }
            }
        })
    }

    fn display(&self) -> impl Display {
        self.name64()
    }
}

impl Display for Amd64Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name(Amd64RegisterNameMode::Quad))
    }
}

impl AbstractRegister for Amd64Register {
    fn name8(&self) -> Cow<'_, str> {
        self.name8().into()
    }

    fn name16(&self) -> Cow<'_, str> {
        self.name16().into()
    }

    fn name32(&self) -> Cow<'_, str> {
        self.name32().into()
    }

    fn name64(&self) -> Cow<'_, str> {
        self.name64().into()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Amd64RegisterNameMode {
    Quad,
    Long,
    Word,
    Byte,
}
