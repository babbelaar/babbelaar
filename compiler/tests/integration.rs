// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use babbelaar::{parse_string_to_tree, BabString, ParseDiagnostic};
use babbelaar_compiler::{Compiler, Immediate, Interpreter};

fn compile_and_interpret(code: &str, function: &'static str) -> Option<Immediate> {
    let tree = match parse_string_to_tree(code) {
        Ok(tree) => tree,
        Err(e) => {
            eprintln!("Fout:");
            eprintln!("    {e}");

            if let Ok(err) = e.downcast::<ParseDiagnostic>() {
                let location = err.range().start();
                eprintln!("Op regel {}, kolom {}", location.line(), location.column());
            }
            panic!("Gegeven code gaf een fout terug")
        }
    };

    let mut compiler = Compiler::new();
    compiler.compile_trees(&[tree]);

    let program = compiler.finish();

    println!("{program}");

    let mut interpreter = Interpreter::new(program);
    interpreter.execute_function(&BabString::new_static(function), Vec::new())
}

#[test]
fn function_that_returns_two() {
    let value = compile_and_interpret("
    werkwijze twee() -> g32 {
        bekeer 2;
    }
    ", "twee");

    assert_eq!(value.map(|x| x.as_i64()), Some(2));
}

#[test]
fn function_that_adds_two_and_three() {
    let value = compile_and_interpret("
    werkwijze twee_plus_drie() -> g32 {
        bekeer 2 + 3;
    }
    ", "twee_plus_drie");

    assert_eq!(value.map(|x| x.as_i64()), Some(5));
}

#[test]
fn function_with_jump_and_returning_zero() {
    let value = compile_and_interpret("
    werkwijze als_1_is_2_dan_1_anders_0() -> g32 {
        als 1 == 2 {
            bekeer 1;
        }
        bekeer 0;
    }
    ", "als_1_is_2_dan_1_anders_0");

    assert_eq!(value.map(|x| x.as_i64()), Some(0));
}

#[test]
fn function_with_for_statement() {
    let value = compile_and_interpret("
    werkwijze volg_i_in_reeks_1_tot_10() -> g32 {
        volg i in reeks(0, 10) {
            als i == 10 {
                bekeer 50;
            }
        }
        bekeer 100;
    }
    ", "volg_i_in_reeks_1_tot_10");

    assert_eq!(value.map(|x| x.as_i64()), Some(100));
}

#[test]
fn structure_and_using_function() {
    let value = compile_and_interpret("
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
    ", "gebruik_nummertjes");

    assert_eq!(value.map(|x| x.as_i64()), Some(44));
}

#[test]
fn two_functions() {
    let value = compile_and_interpret("
    werkwijze krijgGetal() -> g32 {
        bekeer 8;
    }

    werkwijze stuurGetalDoor() -> g32 {
        bekeer krijgGetal();
    }
    ", "stuurGetalDoor");

    assert_eq!(value.map(|x| x.as_i64()), Some(8));
}

#[test]
fn method_call() {
    let value = compile_and_interpret("
    structuur MijnGeavanceerdeStructuur {
        werkwijze krijgGetal() -> g32 {
            bekeer 3;
        }
    }

    werkwijze gebruikStructuur() -> g32 {
        stel a = nieuw MijnGeavanceerdeStructuur {};
        bekeer a.krijgGetal();
    }
    ", "gebruikStructuur");

    assert_eq!(value.map(|x| x.as_i64()), Some(3));
}
