// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Immediate {
    Integer8(i8),
    Integer16(i16),
    Integer32(i32),
    Integer64(i64),
}

impl Immediate {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Register {
    number: usize,
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
