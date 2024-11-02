// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, error::Error, fs::File, path::Path};

use babbelaar::BabString;
use object::{write::{Object, StandardSection, Symbol, SymbolSection}, SymbolFlags, SymbolKind, SymbolScope};

use crate::{OperatingSystem, Platform};

#[derive(Debug, Clone)]
pub struct CompiledFunction {
    pub(super) name: BabString,
    pub(super) byte_code: Vec<u8>,
}

impl CompiledFunction {
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn byte_code(&self) -> &[u8] {
        &self.byte_code
    }
}

#[derive(Debug, Clone)]
pub struct CompiledObject {
    pub(super) platform: Platform,
    pub(super) symbol_offsets: HashMap<BabString, usize>,
    pub(super) function_code: Vec<u8>,
}

impl CompiledObject {
    pub fn new(platform: Platform) -> Self {
        Self {
            platform,
            symbol_offsets: HashMap::new(),
            function_code: Vec::new(),
        }
    }

    pub fn add_function(&mut self, function: CompiledFunction) {
        debug_assert!(!function.byte_code().is_empty());

        let offset = self.function_code.len();
        self.symbol_offsets.insert(function.name, offset);
        self.function_code.extend_from_slice(&function.byte_code);
    }

    #[must_use]
    pub fn platform(&self) -> &Platform {
        &self.platform
    }

    pub fn write_to(&self, path: &Path) -> Result<(), Box<dyn Error>> {
        let mut obj = Object::new(self.object_format(), self.object_architecture(), self.object_endian());

        let main_symbol = obj.add_symbol(Symbol {
            name: b"main".into(),
            value: 0,
            size: 0,
            kind: SymbolKind::Text,
            scope: SymbolScope::Linkage,
            weak: false,
            section: SymbolSection::Undefined,
            flags: SymbolFlags::None,
        });
        // Add the main function in its own subsection (equivalent to -ffunction-sections).
        let main_section = obj.add_subsection(StandardSection::Text, b"main");
        let main_offset = obj.add_symbol_data(main_symbol, main_section, &self.function_code, 1);

        _ = main_offset;

        obj.write_stream(File::create(path)?)
    }

    fn object_format(&self) -> object::BinaryFormat {
        match self.platform.operating_system() {
            OperatingSystem::Linux => object::BinaryFormat::Elf,
            OperatingSystem::MacOs => object::BinaryFormat::MachO,
            OperatingSystem::Windows => object::BinaryFormat::Pe,
        }
    }

    fn object_architecture(&self) -> object::Architecture {
        self.platform().architecture().into()
    }

    fn object_endian(&self) -> object::Endianness {
        self.platform().architecture().endianness().into()
    }
}
