// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::fmt::Display;

use babbelaar::BabString;

use super::{Function, Immediate};

#[derive(Debug)]
pub struct Program {
    functions: Vec<Function>,
    function_symbols: Vec<BabString>,
    strings: Vec<u8>,
}

impl Program {
    #[must_use]
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            function_symbols: Vec::new(),
            strings: Vec::new(),
        }
    }

    pub fn add_function(&mut self, name: BabString, function: Function) {
        assert!(!name.is_empty(), "Kan geen lege naam als werkwijzenaam hebben.");

        self.functions.push(function);
        self.function_symbols.push(name);
    }

    #[must_use]
    pub fn add_string(&mut self, string: &str) -> Immediate {
        let offset = self.strings.len();

        self.strings.extend_from_slice(string.as_bytes());
        self.strings.push(0);

        let ptr = self.strings[offset..].as_ptr();
        let ptr = ptr as usize as i64;

        Immediate::Integer64(ptr)
    }

    #[must_use]
    pub(crate) fn function_index_by_symbol(&self, name: &BabString) -> Option<usize> {
        debug_assert_eq!(self.functions.len(), self.function_symbols.len());

        self.function_symbols.iter()
            .enumerate()
            .find(|(_, x)| *x == name)
            .map(|(index, _)| index)
    }

    #[must_use]
    pub(crate) fn function(&self, index: usize) -> &Function {
        &self.functions[index]
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "Programma met {} {}",
            self.functions.len(),
            if self.functions.len() == 1 { "werkwijze" } else { "werkwijzen" }
        ))?;

        for function in &self.functions {
            f.write_str("\n")?;
            function.fmt(f)?;
        }

        Ok(())
    }
}
