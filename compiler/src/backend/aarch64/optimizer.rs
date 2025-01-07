// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use crate::Label;

use super::{ArmInstruction, ArmShift2};

pub struct AArch64Optimizer<'i, Reg> {
    instructions: &'i mut Vec<ArmInstruction<Reg>>,
    label_offsets: &'i mut HashMap<Label, usize>,
}

impl<'i, Reg: Clone + Copy + PartialEq> AArch64Optimizer<'i, Reg> {
    pub fn optimize(instructions: &'i mut Vec<ArmInstruction<Reg>>, label_offsets: &'i mut HashMap<Label, usize>) {
        let mut this = Self {
            instructions,
            label_offsets,
        };

        this.optimize_instructions();
    }

    fn optimize_instructions(&mut self) {
        for idx in 0..self.instructions.len() {
            if idx >= self.instructions.len() {
                break;
            }

            match self.instructions[idx] {
                ArmInstruction::AddRegister { is_64_bit: add_64bit, dst, lhs, rhs, imm, shift } => {
                    self.handle_add(idx, add_64bit, dst, lhs, rhs, imm, shift);
                }

                ArmInstruction::Mul { is_64_bit, dst, lhs, rhs } => {
                    self.handle_mul(idx, is_64_bit, dst, lhs, rhs);
                }

                _ => (),
            }
        }
    }

    fn handle_add(&mut self, idx: usize, add_64bit: bool, add_dst: Reg, add_lhs: Reg, add_rhs: Reg, add_imm: u8, add_shift: ArmShift2) {
        if add_imm != 0 {
            return;
        }

        let Some(next_instruction) = self.instructions.get(idx + 1).cloned() else { return };

        match next_instruction {
            ArmInstruction::Mul { is_64_bit: mul_64bit, dst: mul_dst, lhs: mul_lhs, rhs: mul_rhs } => {
                if let Some(instr) = try_madd(add_64bit, add_dst, add_lhs, add_rhs, add_imm, add_shift, mul_64bit, mul_dst, mul_lhs, mul_rhs) {
                    self.instructions[idx] = instr;
                    self.instructions.remove(idx + 1);
                    self.update_label_offsets_after(idx);
                }
            }

            _ => (),
        }
    }

    fn handle_mul(&mut self, idx: usize, mul_64bit: bool, mul_dst: Reg, mul_lhs: Reg, mul_rhs: Reg) {
        let Some(next_instruction) = self.instructions.get(idx + 1).cloned() else { return };

        match next_instruction {
            ArmInstruction::AddRegister { is_64_bit: add_64bit, dst: add_dst, lhs: add_lhs, rhs: add_rhs, imm: add_imm, shift: add_shift } => {
                if let Some(instr) = try_madd(add_64bit, add_dst, add_lhs, add_rhs, add_imm, add_shift, mul_64bit, mul_dst, mul_lhs, mul_rhs) {
                    self.instructions[idx] = instr;
                    self.instructions.remove(idx + 1);
                    self.update_label_offsets_after(idx);
                }
            }

            _ => {
                return;
            }
        }
    }

    fn update_label_offsets_after(&mut self, idx: usize) {
        for offset in self.label_offsets.values_mut() {
            if *offset > idx {
                *offset -= 1;
            }
        }
    }
}

#[inline]
fn try_madd<Reg: PartialEq>(
    add_64bit: bool, add_dst: Reg, add_lhs: Reg, add_rhs: Reg, add_imm: u8, add_shift: ArmShift2,
    mul_64bit: bool, mul_dst: Reg, mul_lhs: Reg, mul_rhs: Reg,
) -> Option<ArmInstruction<Reg>> {
    // mul	w0, w0, w1
    // add	w0, w0, w2

    if add_64bit != mul_64bit {
        return None;
    }

    if mul_dst != add_dst {
        return None;
    }

    if add_dst != add_lhs {
        return None;
    }

    if add_imm != 0 {
        return None;
    }
    debug_assert_eq!(add_shift, ArmShift2::default());

    // mul x1, x1, x2
    // add x0, x0 + x1 + #0 (LSL)

    Some(ArmInstruction::MAdd {
        is_64_bit: mul_64bit,
        dst: mul_dst,
        mul_lhs,
        mul_rhs,
        addend: add_rhs,
    })
}
