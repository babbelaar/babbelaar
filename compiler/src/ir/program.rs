// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, fmt::Display, mem::take};

use babbelaar::BabString;

use crate::{DataSection, DataSectionKind, DataSectionOffset};

use super::Function;

#[derive(Debug)]
pub struct Program {
    pub(crate) functions: Vec<Function>,
    function_aliases: HashMap<BabString, BabString>,
    function_symbols: Vec<BabString>,
    pub(crate) read_only_data: Option<DataSection>,
}

impl Program {
    #[must_use]
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            function_aliases: HashMap::new(),
            function_symbols: Vec::new(),
            read_only_data: Some(DataSection::new(DataSectionKind::ReadOnly)),
        }
    }

    pub fn add_function(&mut self, name: BabString, function: Function) {
        assert!(!name.is_empty(), "Kan geen lege naam als werkwijzenaam hebben.");

        self.functions.push(function);
        self.function_symbols.push(name);
    }

    pub fn add_function_alias(&mut self, name: &BabString, actual_name: &BabString) {
        self.function_aliases.insert(name.clone(), actual_name.clone());
    }

    pub fn add_string(&mut self, string: &str) -> DataSectionOffset {
        self.read_only_data.as_mut().unwrap().add_null_terminated_string(string)
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

    #[must_use]
    pub fn functions(&self) -> &[Function] {
        &self.functions
    }

    #[must_use]
    pub fn function_by_name(&self, name: impl Into<BabString>) -> Option<&Function> {
        let index = self.function_index_by_symbol(&name.into())?;
        Some(&self.functions[index])
    }

    #[must_use]
    pub fn read_only_data(&self) -> &DataSection {
        self.read_only_data.as_ref().unwrap()
    }

    #[must_use]
    pub fn take_read_only_data(&mut self) -> DataSection {
        self.read_only_data.take().unwrap()
    }

    #[must_use]
    pub fn take_function_aliases(&mut self) -> HashMap<BabString, BabString> {
        take(&mut self.function_aliases)
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
