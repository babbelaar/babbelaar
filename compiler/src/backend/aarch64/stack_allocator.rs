// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use log::debug;

use crate::Register;

use super::{
    POINTER_SIZE,
    STACK_ALIGNMENT,
};

#[derive(Debug)]
pub struct AArch64StackAllocator {
    allocations: Vec<StackAllocation>,
    total_size: usize,
    is_finalized: bool,
}

impl AArch64StackAllocator {
    #[must_use]
    pub fn new() -> Self {
        Self {
            allocations: Vec::new(),
            total_size: 0,
            is_finalized: false,
        }
    }

    #[must_use]
    pub fn total_size(&self) -> usize {
        debug_assert!(self.is_finalized, "Kan grootte van stapel is ongeldig voordat het gefinaliseerd is!");
        self.total_size
    }

    pub fn reserve_save_frame_pointer(&mut self, size: usize) {
        debug_assert!(!self.is_finalized, "Kan de stapel niet aanpassen na finalisering!");

        self.allocations.push(StackAllocation::FramePointer {
            size,
            offset: 0,
        });
    }

    pub fn reserve_stack_allocation(&mut self, instruction_id: usize, register: Register, size: usize) {
        debug_assert!(!self.is_finalized, "Kan de stapel niet aanpassen na finalisering!");

        self.allocations.push(StackAllocation::Register {
            instruction_id,
            register,
            size,
            offset: 0,
        });
    }

    pub fn reserve_variadic_function_call_arguments(&mut self, instruction_id: usize, size: usize) {
        debug_assert!(!self.is_finalized, "Kan de stapel niet aanpassen na finalisering!");

        let size = size.next_multiple_of(8);

        self.allocations.push(StackAllocation::VariadicFunctionArguments {
            instruction_id,
            size,
            offset: 0,
        });
    }

    pub fn reserve_callee_saved_registers(&mut self, registers_to_save: usize) {
        debug_assert!(!self.is_finalized, "Kan de stapel niet aanpassen na finalisering!");

        let size = (registers_to_save * POINTER_SIZE).next_multiple_of(STACK_ALIGNMENT);

        self.allocations.push(StackAllocation::CalleeRegisterSave {
            size,
            offset: 0,
        });
    }

    pub fn finalize(&mut self) {
        debug_assert!(!self.is_finalized, "Kan de stapel niet twee keer finaliseren!");
        self.is_finalized = true;

        self.total_size = 0;
        self.lay_out_var_args();
        self.lay_out_normal_registers();
        self.lay_out_register_saves();
        self.lay_out_frame_pointer();

        self.total_size = self.total_size.next_multiple_of(16);

        assert!(self.total_size < (1 << 12));
    }

    #[must_use]
    pub fn has_only_frame_pointer_reservation(&self) -> bool {
        self.allocations.len() == 1 && matches!(self.allocations[0], StackAllocation::FramePointer { .. })
    }

    #[must_use]
    pub fn offset_of_reg(&self, instruction_id: usize) -> usize {
        let needle = instruction_id;

        for alloc in &self.allocations {
            if let StackAllocation::Register { instruction_id, offset, .. } = alloc {
                if needle == *instruction_id {
                    return *offset;
                }
            }
        }

        panic!("Ongeldige stapelallocatieaanvraag, kon afstand van register op instructie {instruction_id} niet vinden!");
    }

    #[must_use]
    pub fn offset_of_frame_pointer(&self) -> usize {
        for alloc in &self.allocations {
            if let StackAllocation::FramePointer { offset, .. } = alloc {
                return *offset;
            }
        }

        panic!("Ongeldige stapelallocatieaanvraag, we hebben geen ruimte gereserveerd voor een vensterwijzerallocatie!");
    }

    #[must_use]
    pub fn offset_of_callee_register_saves(&self) -> usize {
        for alloc in &self.allocations {
            if let StackAllocation::CalleeRegisterSave { offset, .. } = alloc {
                return *offset;
            }
        }

        panic!("Ongeldige stapelallocatieaanvraag, we hebben geen ruimte gereserveerd voor aangeroepenregister-allocaties!");
    }

    #[must_use]
    pub fn offset_of_variadic_args(&self, instruction_id: usize) -> usize {
        let needle = instruction_id;

        for alloc in &self.allocations {
            if let StackAllocation::VariadicFunctionArguments { instruction_id, offset, .. } = alloc {
                if needle == *instruction_id {
                    return *offset;
                }
            }
        }

        panic!("Ongeldige stapelallocatieaanvraag, we hebben geen ruimte gereserveerd voor variabele argumenten!");
    }
}

impl AArch64StackAllocator {
    fn lay_out_var_args(&mut self) {
        let mut max_size = 0;

        for alloc in &mut self.allocations {
            match alloc {
                StackAllocation::VariadicFunctionArguments { offset, size, .. } => {
                    *offset = self.total_size;
                    max_size = max_size.max(*size);
                }

                _ => (),
            }
        }

        debug!("Stapelallocatie: variabele argumenten worden opgeslagen op afstand {} met grootte {max_size}", self.total_size);

        self.total_size += max_size;
    }

    fn lay_out_normal_registers(&mut self) {
        for alloc in &mut self.allocations {
            match alloc {
                StackAllocation::CalleeRegisterSave { .. } => (),
                StackAllocation::FramePointer { .. } => (),
                StackAllocation::VariadicFunctionArguments { .. } => (),

                StackAllocation::Register { register, offset, size, .. } => {
                    *offset = self.total_size;

                    debug!("Stapelallocatie: register {register} wordt opgeslagen op afstand {offset} met grootte {size}");

                    self.total_size += *size;
                }
            }
        }
    }

    fn lay_out_register_saves(&mut self) {
        self.total_size = self.total_size.next_multiple_of(STACK_ALIGNMENT);

        let mut max_size = 0;

        for alloc in &mut self.allocations {
            match alloc {
                StackAllocation::CalleeRegisterSave { offset, size, .. } => {
                    *offset = self.total_size;

                    debug_assert!(max_size == 0, "We hebben meerdere aangeroepenregister-allocaties?");
                    max_size = max_size.max(*size);
                }

                _ => (),
            }
        }

        let size = max_size.next_multiple_of(STACK_ALIGNMENT);
        debug!("Stapelallocatie: aangeroepenregisters worden opgeslagen op afstand {} met grootte {size}", self.total_size);
        self.total_size += size;
    }

    fn lay_out_frame_pointer(&mut self) {
        let mut max_size = 0;

        for alloc in &mut self.allocations {
            match alloc {
                StackAllocation::FramePointer { offset, size, .. } => {
                    *offset = self.total_size;

                    debug_assert!(max_size == 0, "We hebben al een vensterwijzerallocatie?");
                    max_size = max_size.max(*size);
                }

                _ => (),
            }
        }

        debug!("Stapelallocatie: vensterwijzer wordt opgeslagen op afstand {} met grootte {max_size}", self.total_size);
        self.total_size += max_size;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StackAllocation {
    CalleeRegisterSave {
        offset: usize,
        size: usize,
    },
    FramePointer {
        offset: usize,
        size: usize,
    },
    Register {
        instruction_id: usize,
        offset: usize,
        size: usize,

        register: Register,
    },
    VariadicFunctionArguments {
        instruction_id: usize,
        offset: usize,
        size: usize,
    },
}
