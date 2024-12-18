// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

// Useful tool for testing the byte code: <http://shell-storm.org/online/Online-Assembler-and-Disassembler/>
// Intel SDM: <https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html>

// As it's an integration test, we can only run this on x86-64 (AMD64)
#![cfg(target_arch = "x86_64")]

use core::slice;
use std::collections::HashMap;

use babbelaar::{parse_string_to_tree, BabString};
use babbelaar_compiler::{Amd64CodeGenerator, Compiler, Platform, Program, RelocationType};

#[test]
fn function_that_returns_0() {
    let result = compile_and_execute(
        "bekeer_0",
        "
            werkwijze bekeer_0() -> g32 {
                bekeer 0;
            }
        ",
    );

    assert_eq!(result, 0);
}

#[test]
fn function_that_returns_5() {
    let result = compile_and_execute("bekeer_5", "werkwijze bekeer_5() -> g32 {
        bekeer 5;
    }");

    assert_eq!(result, 5);
}

#[test]
fn function_that_returns_9_plus_10() {
    let result = compile_and_execute("bekeer_negen_plus_tien", "werkwijze bekeer_negen_plus_tien() -> g32 {
        stel negen = 9;
        stel tien = 10;
        als 1 == 1 {
            bekeer negen + tien;
        }
        bekeer 55;
    }");

    assert_eq!(result, 19);
}

#[test]
fn function_that_returns_6_if_3_is_equal_to_3() {
    let result = compile_and_execute("bekeer_6", "werkwijze bekeer_6() -> g32 {
        als 3 == 3 {
            bekeer 6;
        }
        bekeer 2;
    }");

    assert_eq!(result, 6);
}

#[test]
fn function_with_for_statement() {
    let value = compile_and_execute("volg_i_in_reeks_1_tot_10", "
    werkwijze volg_i_in_reeks_1_tot_10() -> g32 {
        volg i in reeks(0, 10) {
            als i == 10 {
                bekeer 50;
            }
        }
        bekeer 100;
    }
    ");

    assert_eq!(value, 100);
}

#[test]
fn structure_and_using_function() {
    let value = compile_and_execute("gebruik_nummertjes", "
    structuur Nummertjes {
        veld geboortejaar: g32,
        veld huidigJaar: g32,
    }

    werkwijze gebruik_nummertjes() -> g32 {
        stel nummertjes = nieuw Nummertjes {
            geboortejaar: 1980,
            huidigJaar: 2024,
        };

        bekeer nummertjes.huidigJaar - nummertjes.geboortejaar;
    }
    ");

    assert_eq!(value, 44);
}

#[test]
fn two_functions() {
    let value = compile_and_execute("stuurGetalDoor", "
    werkwijze krijgGetal() -> g32 {
        bekeer 8;
    }

    werkwijze stuurGetalDoor() -> g32 {
        bekeer krijgGetal();
    }
    ");

    assert_eq!(value, 8);
}

#[test]
fn method_call() {
    let value = compile_and_execute("gebruikStructuur", "
    structuur MijnGeavanceerdeStructuur {
        werkwijze krijgGetal() -> g32 {
            bekeer 3;
        }
    }

    werkwijze gebruikStructuur() -> g32 {
        stel a = nieuw MijnGeavanceerdeStructuur {};
        bekeer a.krijgGetal();
    }
    ");

    assert_eq!(value, 3);
}

#[test]
fn method_call_with_this() {
    let value = compile_and_execute("gebruikStructuurMetGetal", "
    structuur MijnStructuurMetGetal {
        veld getal: g32,

        werkwijze gebruikGetal() -> g32 {
            bekeer dit.getal + 3;
        }
    }

    werkwijze gebruikStructuurMetGetal() -> g32 {
        stel a = nieuw MijnStructuurMetGetal {
            getal: 5,
        };
        bekeer a.gebruikGetal();
    }
    ");

    assert_eq!(value, 8);
}

//
//
// Helper code
//
//

fn compile_and_execute(function: &'static str, code: &str) -> isize {
    let _ = env_logger::builder().is_test(true).filter(None, log::LevelFilter::max()).try_init();
    let tree = parse_string_to_tree(code).unwrap();

    let mut compiler = Compiler::new();
    compiler.compile_trees(&[tree]);

    let program = compiler.finish();
    println!("{program}");


    let (byte_code, offset) = compile_ir_to_arm_and_link(&program, function);
    let region = allocate_executable_region(&byte_code);

    #[cfg(unix)]
    {
        use signal_hook::{consts::SIGBUS, iterator::Signals};
        let mut signals = Signals::new(&[SIGBUS]).unwrap();

        std::thread::spawn(move || {
            for sig in signals.forever() {
                println!("Received signal {:?}", sig);
            }
        });
    }

    execute_code(&region, offset)
}

fn compile_ir_to_arm_and_link(program: &Program, target: &str) -> (Vec<u8>, usize) {
    let mut offsets = HashMap::new();
    let mut data = Vec::new();
    let mut links = Vec::new();

    for function in program.functions() {
        let function = Amd64CodeGenerator::compile(function, Platform::host_platform());
        let offset = data.len();
        offsets.insert(function.name().clone(), offset);

        for link in function.relocations() {
            links.push((offset, link.clone()));
        }

        data.extend_from_slice(function.byte_code());
    }

    for (base_offset, link) in links {
        match link.ty() {
            RelocationType::Data { .. } => {
                unimplemented!()
            }

            RelocationType::Function { name } => {
                let subroutine_offset = *offsets.get(name).unwrap();

                let base_offset = base_offset + link.offset();
                let offset = (subroutine_offset as i32 - base_offset as i32) / 4;
                let instruction = 0x94000000 + (offset as u32 & 0x3FFFFFF);
                data[base_offset..base_offset+4].copy_from_slice(&instruction.to_ne_bytes());
            }
        }
    }

    let target = *offsets.get(&BabString::from(target.to_string())).unwrap();
    (data, target)
}

fn allocate_executable_region(code: &[u8]) -> region::Allocation {
    let size = ceil_to_page_size(code.len());

    let mut region = region::alloc(size, region::Protection::READ_WRITE).unwrap();
    copy_to_region(&mut region, code);

    unsafe {
        region::protect::<u8>(region.as_ptr(), size, region::Protection::READ_EXECUTE)
    }.unwrap();

    region
}

fn ceil_to_page_size(size: usize) -> usize {
    let page_size = region::page::size();

    let num_pages = size / page_size;
    let rest = size % page_size;

    if rest != 0 {
        (num_pages + 1) * page_size
    } else {
        size
    }
}

fn copy_to_region(region: &mut region::Allocation, src: &[u8]) {
    let ptr = region.as_mut_ptr();
    let len = region.len();

    let slice = unsafe {
        slice::from_raw_parts_mut(ptr, len)
    };

    (&mut slice[0..src.len()]).copy_from_slice(src);
}

fn execute_code(region: &region::Allocation, offset: usize) -> isize {
    type MainFn = extern "C" fn() -> isize;

    let ptr = region.as_ptr::<()>();
    let ptr = unsafe { (ptr as *const u8).offset(offset as _) };
    let func: MainFn = unsafe { std::mem::transmute(ptr) };

    (func)()
}
