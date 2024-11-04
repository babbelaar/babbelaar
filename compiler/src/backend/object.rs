// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, error::Error, fs::File, path::Path};

use babbelaar::BabString;
use object::{write::{Object, Relocation, StandardSection, Symbol, SymbolSection}, SymbolFlags, SymbolKind, SymbolScope};

use crate::{OperatingSystem, Platform};

use super::FunctionLink;

#[derive(Debug, Clone)]
pub struct CompiledFunction {
    pub(super) name: BabString,
    pub(super) byte_code: Vec<u8>,
    pub(super) link_locations: Vec<FunctionLink>,
}

impl CompiledFunction {
    #[must_use]
    pub fn name(&self) -> &BabString {
        &self.name
    }

    #[must_use]
    pub fn byte_code(&self) -> &[u8] {
        &self.byte_code
    }

    #[must_use]
    pub fn link_locations(&self) -> &[FunctionLink] {
        &self.link_locations
    }

    fn final_name(&self) -> &str {
        if self.name() == "hoofd" {
            return "main";
        }
        &self.name
    }
}

#[derive(Debug, Clone)]
pub struct CompiledObject {
    platform: Platform,
    symbol_offsets: HashMap<BabString, usize>,
    functions: Vec<CompiledFunction>,
    functions_length: usize,
}

impl CompiledObject {
    pub fn new(platform: Platform) -> Self {
        Self {
            platform,
            symbol_offsets: HashMap::new(),
            functions: Vec::new(),
            functions_length: 0,
        }
    }

    pub fn add_function(&mut self, function: CompiledFunction) {
        debug_assert!(!function.byte_code().is_empty());

        self.symbol_offsets.insert(function.name.clone(), self.functions_length);

        self.functions_length += function.byte_code().len();

        self.functions.push(function);
    }

    #[must_use]
    pub fn platform(&self) -> &Platform {
        &self.platform
    }

    pub fn write_to(self, path: &Path) -> Result<(), Box<dyn Error>> {
        let mut obj = Object::new(self.object_format(), self.object_architecture(), self.object_endian());

        let code_section = obj.add_subsection(StandardSection::Text, b"main");

        for mut function in self.functions {
            let our_offset = self.symbol_offsets.get(&function.name).unwrap();

            let main_symbol = obj.add_symbol(Symbol {
                name: function.final_name().as_bytes().to_vec(),
                value: 0,
                size: 0,
                kind: SymbolKind::Text,
                scope: SymbolScope::Linkage,
                weak: false,
                section: SymbolSection::Undefined,
                flags: SymbolFlags::None,
            });

            for link in &function.link_locations {
                let Some(internal_offset) = self.symbol_offsets.get(&link.name) else {
                    continue;
                };

                link.write(function.byte_code.as_mut_slice(), *internal_offset as isize - *our_offset as isize);
            }

            let function_offset = obj.add_symbol_data(main_symbol, code_section, &function.byte_code(), 1);

            for link in &function.link_locations {
                if self.symbol_offsets.get(&link.name).is_some() {
                    continue;
                }

                let puts_symbol = obj.add_symbol(Symbol {
                    name: link.name.as_bytes().to_vec(),
                    value: 0,
                    size: 0,
                    kind: SymbolKind::Text,
                    scope: SymbolScope::Dynamic,
                    weak: false,
                    section: SymbolSection::Undefined,
                    flags: SymbolFlags::None,
                });

                // Relocation for the call to puts.
                obj.add_relocation(
                    code_section,
                    Relocation {
                        offset: function_offset + link.offset as u64,
                        symbol: puts_symbol,
                        addend: link.addend(),
                        flags: link.flags(),
                    },
                )?;
            }
        }

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
