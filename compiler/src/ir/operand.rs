// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::{Display, Write};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Immediate {
    Integer8(i8),
    Integer16(i16),
    Integer32(i32),
    Integer64(i64),
}

impl Immediate {
    #[must_use]
    pub const fn as_i8(&self) -> i8 {
        match self {
            Self::Integer8(i) => *i,
            Self::Integer16(i) => *i as i8,
            Self::Integer32(i) => *i as i8,
            Self::Integer64(i) => *i as i8,
        }
    }

    #[must_use]
    pub const fn as_i16(&self) -> i16 {
        match self {
            Self::Integer8(i) => *i as i16,
            Self::Integer16(i) => *i,
            Self::Integer32(i) => *i as i16,
            Self::Integer64(i) => *i as i16,
        }
    }

    #[must_use]
    pub const fn as_i32(&self) -> i32 {
        match self {
            Self::Integer8(i) => *i as i32,
            Self::Integer16(i) => *i as i32,
            Self::Integer32(i) => *i,
            Self::Integer64(i) => *i as i32,
        }
    }

    #[must_use]
    pub const fn as_i64(&self) -> i64 {
        match self {
            Self::Integer8(i) => *i as i64,
            Self::Integer16(i) => *i as i64,
            Self::Integer32(i) => *i as i64,
            Self::Integer64(i) => *i,
        }
    }
}

impl Display for Immediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer8(val) => val.fmt(f),
            Self::Integer16(val) => val.fmt(f),
            Self::Integer32(val) => val.fmt(f),
            Self::Integer64(val) => val.fmt(f),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Register {
    number: usize,
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char('r')?;
        self.number.fmt(f)
    }
}

#[derive(Debug)]
pub struct RegisterAllocator {
    next_number: usize,
}

impl RegisterAllocator {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            next_number: 1,
        }
    }

    #[must_use]
    pub const fn next(&mut self) -> Register {
        let reg = Register { number: self.next_number };

        self.next_number += 1;

        reg
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operand {
    Immediate(Immediate),
    Register(Register),
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Immediate(immediate) => immediate.fmt(f),
            Self::Register(register) => register.fmt(f),
        }
    }
}

impl From<Immediate> for Operand {
    fn from(value: Immediate) -> Self {
        Self::Immediate(value)
    }
}

impl From<Register> for Operand {
    fn from(value: Register) -> Self {
        Self::Register(value)
    }
}
