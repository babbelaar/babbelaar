// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{collections::HashMap, error::Error, fs::File, io::Write, path::Path, time::SystemTime};

use babbelaar::BabString;
use object::{
    pe::{IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE, IMAGE_SUBSYSTEM_WINDOWS_CUI},
    write::{
        pe::{
            NtHeaders,
            SectionRange,
            Writer as PeWriter,
        },
        Object,
        Relocation,
        StandardSection,
        Symbol,
        SymbolSection,
    },
    SymbolFlags,
    SymbolKind,
    SymbolScope,
};

use crate::{OperatingSystem, Platform, WindowsVersion};

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
        // match self.platform.operating_system() {
        //     OperatingSystem::Linux => self.write_using_unified_api(path),
        //     OperatingSystem::MacOs => self.write_using_unified_api(path),
        //     OperatingSystem::Windows => self.write_using_pe_api(path),
        // }
        self.write_using_unified_api(path)
    }

    fn write_using_unified_api(self, path: &Path) -> Result<(), Box<dyn Error>> {
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

    fn write_using_pe_api(self, path: &Path) -> Result<(), Box<dyn Error>> {
        let section_alignment = 4096; // equal to the page size
        let file_alignment = 512; // default value according to MSDN

        //
        // Set up
        //

        let data_directory_num = 0;
        let image_base = 0x10000000;

        let mut buffer = Vec::new();
        let mut writer = PeWriter::new(self.platform.architecture().is_64_bit(), section_alignment, file_alignment, &mut buffer);

        //
        // Prepare
        //

        writer.reserve_dos_header_and_stub();
        writer.reserve_nt_headers(data_directory_num);

        let mut sections = Vec::new();
        let (text_section, address_of_entry_point) = self.create_pe_text_section(&mut writer);
        sections.push(text_section);

        writer.reserve_section_headers(sections.len() as _);

        //
        // Write
        //

        writer.write_dos_header_and_stub()?;
        writer.write_nt_headers(self.pe_nt_headers(address_of_entry_point, image_base));

        writer.write_section_headers();
        for section in sections {
            writer.write_section(section.range.file_offset, &section.data);
        }

        writer.write_reloc_section();

        drop(writer);
        File::create(path)?.write_all(&mut buffer)?;
        Ok(())
    }

    #[must_use]
    fn create_pe_text_section(&self, writer: &mut PeWriter) -> (PeSection, u32) {
        let mut address_of_entry_point = 0;

        let mut section = PeSection {
            data: Vec::new(),
            range: Default::default(),
        };

        for function in &self.functions {
            if function.final_name() == "main" {
                address_of_entry_point = section.data.len() as u32;
            }

            section.data.extend(function.byte_code());
        }

        section.range = writer.reserve_text_section(section.data.len() as _);

        address_of_entry_point += section.range.file_offset;

        (section, address_of_entry_point)
    }

    fn object_format(&self) -> object::BinaryFormat {
        match self.platform.operating_system() {
            OperatingSystem::Linux => object::BinaryFormat::Elf,
            OperatingSystem::MacOs => object::BinaryFormat::MachO,
            OperatingSystem::Windows => object::BinaryFormat::Coff,
        }
    }

    fn object_architecture(&self) -> object::Architecture {
        self.platform().architecture().into()
    }

    fn object_endian(&self) -> object::Endianness {
        self.platform().architecture().endianness().into()
    }

    #[must_use]
    fn pe_nt_headers(&self, address_of_entry_point: u32, image_base: u64) -> NtHeaders {
        NtHeaders {
            machine: self.platform.architecture().windows_machine_type(),
            time_date_stamp: (SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_secs()) as u32,
            characteristics: 0,
            major_linker_version: 1,
            minor_linker_version: 0,
            address_of_entry_point,
            image_base,
            major_operating_system_version: WindowsVersion::WINDOWS_VISTA.major(),
            minor_operating_system_version: WindowsVersion::WINDOWS_VISTA.minor(),
            major_image_version: 0,
            minor_image_version: 0,
            major_subsystem_version: WindowsVersion::WINDOWS_XP_64_BIT_EDITION.major(),
            minor_subsystem_version: WindowsVersion::WINDOWS_XP_64_BIT_EDITION.minor(),
            subsystem: IMAGE_SUBSYSTEM_WINDOWS_CUI,
            dll_characteristics: IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE,
            size_of_stack_reserve: 0x1000000,
            size_of_stack_commit: 0x1000,
            size_of_heap_reserve: 0x100000,
            size_of_heap_commit: 0x1000,
        }
    }
}

struct PeSection {
    data: Vec<u8>,
    range: SectionRange,
}

#[cfg(test)]
mod tests {
    use temp_dir::TempDir;

    use crate::{backend::aarch64::{ArmInstruction, ArmRegister}, Architecture, Environment, FunctionLinkMethod};

    use super::*;

    #[test]
    fn aarch64_link_test() {
        let mut object = CompiledObject::new(Platform::new(Architecture::AArch64, Environment::Darwin, OperatingSystem::MacOs));

        object.add_function(CompiledFunction {
            name: BabString::new_static("a"),
            byte_code: ArmInstruction::Ret.encode(0, &HashMap::new()).to_le_bytes().to_vec(),
            link_locations: Vec::new(),
        });
        object.add_function(CompiledFunction {
            name: BabString::new_static("b"),
            byte_code: [
                ArmInstruction::MovZ { register: ArmRegister::X0, imm16: 100 }.encode(0, &HashMap::new()).to_le_bytes(),
                ArmInstruction::Bl { offset: 0, symbol_name: BabString::new_static("a") }.encode(0, &HashMap::new()).to_le_bytes(),
            ].into_iter().flatten().collect(),
            link_locations: [
                FunctionLink {
                    name: BabString::new_static("a"),
                    offset: 4,
                    method: FunctionLinkMethod::AArch64BranchLink,
                }
            ].to_vec(),
        });

        let dir = TempDir::new().unwrap();

        object.write_to(&dir.child("test")).unwrap();
    }
}
