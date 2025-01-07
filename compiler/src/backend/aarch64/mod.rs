// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

// Optimization possibilities:
// - use the instructions vec as a tool to remove / simplify instructions
// - use BC.cond as a [[unlikely]] hint
// - convert B.AL to B for fewer instructions (B.cond can only take imm19, B can take imm26 and is clearer)

mod addressing_mode;
mod code_generator;
mod condition_code;
mod fixup;
mod function_characteristics;
mod instruction;
mod instruction_selector;
mod optimizer;
mod register;

const POINTER_SIZE: usize = 8;

const SPACE_NEEDED_FOR_FP_AND_LR: usize = 2 * POINTER_SIZE;

pub use self::{
    addressing_mode::{
        ArmSignedAddressingMode,
        ArmUnsignedAddressingMode,
    },
    code_generator::AArch64CodeGenerator,
    condition_code::ArmConditionCode,
    function_characteristics::{
        AArch64FunctionCharacteristics,
        AArch64VarArgsConvention,
    },
    instruction::{
        ArmInstruction,
        ArmShift2,
    },
    instruction_selector::AArch64InstructionSelector,
    optimizer::AArch64Optimizer,
    register::ArmRegister,
};
