//! Ensures instructions are optimized correctly.

use babbelaar::{parse_string_to_tree, ParseDiagnostic};
use babbelaar_compiler::{optimize_program, Compiler, Instruction, MathOperation, Program};
use rstest::rstest;

#[rstest]
#[case("
    werkwijze test(a: g32) -> g32 {
        bekeer a * 2;
    }
")]
#[case("
    werkwijze test(a: g32) -> g32 {
        bekeer a * 8;
    }
")]
fn test_multiply_power_of_two_optimizes_as_left_shift(#[case] input: &str) {
    let mut program = compile(input);
    optimize_program(&mut program);

    let mut shifts = 0;

    for instruction in program.functions()[0].instructions() {
        if let Instruction::MathOperation { operation, .. } = instruction {
            assert_ne!(*operation, MathOperation::Multiply, "We mochten geen 'Vermenigvuldig' gebruiken");

            if *operation == MathOperation::LeftShift {
                shifts += 1;
            }
        }
    }

    assert_ne!(shifts, 0);
}

fn compile(code: &str) -> Program {
    let _ = env_logger::builder().is_test(true).filter(None, log::LevelFilter::max()).try_init();
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

    compiler.finish()
}