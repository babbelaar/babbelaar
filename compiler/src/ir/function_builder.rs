// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use babbelaar::BabString;

use crate::{DataSectionOffset, StructureLayout, TypeId, TypeInfo};

use super::{Function, FunctionArgument, FunctionAttributesExt, FunctionParameter, Immediate, Instruction, JumpCondition, Label, MathOperation, Operand, PrimitiveType, ProgramBuilder, Register, RegisterAllocator};

#[derive(Debug)]
pub struct FunctionBuilder<'program> {
    pub(super) program_builder: &'program mut ProgramBuilder,
    pub(super) name: BabString,
    pub(super) register_allocator: RegisterAllocator,
    pub(super) this: Option<(TypeInfo, Register, PrimitiveType)>,
    pub(super) return_ty: PrimitiveType,
    pub(super) arguments: Vec<FunctionParameter>,
    pub(super) instructions: Vec<Instruction>,
    pub(super) locals: HashMap<BabString, FunctionLocal>,
    pub(super) label_counter: usize,
    pub(crate) label_names: HashMap<Label, BabString>,
    pub(super) label_positions: HashMap<Label, usize>,
}

impl<'program> FunctionBuilder<'program> {
    pub fn call(&mut self, name: BabString, arguments: impl Into<Vec<FunctionArgument>>) -> Register {
        let var_args_after_n = self.program_builder.function_attributes_of(&name).var_args_after_n_normal_params().unwrap_or(usize::MAX);

        let ret_val_reg = self.register_allocator.next();

        let mut arguments: Vec<FunctionArgument> = arguments.into();
        let variable_arguments = if var_args_after_n > arguments.len() {
            Vec::new()
        } else {
            arguments.split_off(var_args_after_n)
        };

        let ret_ty = self.return_type_of(&name);
        let ret_ty = self.program_builder.type_manager.layout(ret_ty).primitive_type();

        self.instructions.push(Instruction::Call {
            name,
            arguments,
            variable_arguments,
            ret_ty: Some(ret_ty),
            ret_val_reg: Some(ret_val_reg),
        });

        ret_val_reg
    }

    pub fn compare(&mut self, lhs: Register, rhs: impl Into<Operand>, typ: PrimitiveType) {
        self.instructions.push(Instruction::Compare {
            typ,
            lhs,
            rhs: rhs.into()
        });
    }

    pub fn increment(&mut self, register: Register, typ: PrimitiveType) {
        self.instructions.push(Instruction::Increment { register, typ });
    }

    pub fn jump(&mut self, location: Label) {
        self.instructions.push(Instruction::Jump { location });
    }

    pub fn jump_if_equal(&mut self, location: Label) {
        let condition = JumpCondition::Equal;
        self.instructions.push(Instruction::JumpConditional { condition, location });
    }

    pub fn jump_if_less(&mut self, location: Label) {
        let condition = JumpCondition::Less;
        self.instructions.push(Instruction::JumpConditional { condition, location });
    }

    pub fn jump_if_less_or_equal(&mut self, location: Label) {
        let condition = JumpCondition::LessOrEqual;
        self.instructions.push(Instruction::JumpConditional { condition, location });
    }

    pub fn jump_if_greater(&mut self, location: Label) {
        let condition = JumpCondition::Greater;
        self.instructions.push(Instruction::JumpConditional { condition, location });
    }

    pub fn jump_if_greater_or_equal(&mut self, location: Label) {
        let condition = JumpCondition::GreaterOrEqual;
        self.instructions.push(Instruction::JumpConditional { condition, location });
    }

    pub fn jump_if_not_equal(&mut self, location: Label) {
        let condition = JumpCondition::NotEqual;
        self.instructions.push(Instruction::JumpConditional { condition, location });
    }

    pub fn jump_if(&mut self, location: Label, condition: JumpCondition) {
        self.instructions.push(Instruction::JumpConditional { condition, location });
    }

    #[must_use]
    pub fn load_immediate(&mut self, immediate: Immediate, typ: PrimitiveType) -> Register {
        let destination = self.register_allocator.next();

        self.instructions.push(Instruction::Move {
            typ,
            destination,
            source: Operand::Immediate(immediate),
        });

        destination
    }

    /// Loads `1` to the register if the condition is true.
    #[must_use]
    pub fn load_condition(&mut self, condition: JumpCondition) -> Register {
        let destination = self.register_allocator.next();

        self.instructions.push(Instruction::MoveCondition {
            destination,
            condition,
        });

        destination
    }

    pub fn associate_register_to_local(&mut self, register: Register, local_name: impl Into<BabString>, type_info: TypeInfo) {
        let local_name = local_name.into();

        debug_assert_eq!(self.locals.get(&local_name), None, "Lokale `{local_name}` had al een waarde!");
        let primitive = self.primitive_type_of(&type_info);

        self.locals.insert(local_name, FunctionLocal {
            register,
            type_info,
            primitive,
        });
    }

    #[must_use]
    pub fn load_string(&mut self, string: &str) -> (TypeInfo, Register, PrimitiveType) {
        let offset = self.program_builder.program.add_string(string);
        let reg = self.load_effective_address(offset);
        (TypeInfo::Plain(TypeId::SLINGER), reg, PrimitiveType::new(self.pointer_size(), false))
    }

    #[must_use]
    pub fn load_local(&mut self, name: &BabString) -> Option<(TypeInfo, Register, PrimitiveType)> {
        let src = self.locals.get(name).cloned()?;
        log::debug!("Loading {name} = {src:#?}");
        Some((src.type_info, src.register, src.primitive))
    }

    #[must_use]
    pub fn math(&mut self, operation: MathOperation, typ: PrimitiveType, lhs: impl Into<Operand>, rhs: impl Into<Operand>) -> Register {
        let destination = self.register_allocator.next();

        let lhs = lhs.into();
        let rhs = rhs.into();

        self.instructions.push(Instruction::MathOperation { operation, typ, destination, lhs, rhs });

        destination
    }

    pub fn ret(&mut self) {
        self.instructions.push(Instruction::Return {
            value_reg: None,
        });
    }

    pub fn ret_with(&mut self, register: Register) {
        self.instructions.push(Instruction::Return {
            value_reg: Some(register),
        });
    }

    #[must_use]
    pub fn create_label(&mut self, name: impl Into<BabString>) -> Label {
        self.label_counter += 1;
        let label = Label {
            id: self.label_counter
        };

        self.label_names.insert(label, name.into());

        label
    }

    fn link_label(&mut self, label: Label, offset: usize) {
        debug_assert_eq!(self.label_positions.get(&label), None, "Cannot link a label twice!");
        self.label_positions.insert(label, offset);
        self.instructions.push(Instruction::Label(label));
    }

    pub fn link_label_here(&mut self, label: Label) {
        self.link_label(label, self.instructions.len() + 1);
    }

    #[must_use]
    pub fn create_label_and_link_here(&mut self, name: impl Into<BabString>) -> Label {
        let label = self.create_label(name);
        self.link_label_here(label.clone());
        label
    }

    #[must_use]
    pub fn build(self) -> Function {
        Function {
            name: self.name,
            return_ty: self.return_ty,
            arguments: self.arguments,
            instructions: self.instructions,
            label_names: self.label_names,
            ir_register_allocator: self.register_allocator,
        }
    }

    /// Allocate a structure on the stack, and returns a Register containing the pointer to
    /// the start of that block.
    #[must_use]
    pub fn allocate_structure(&mut self, name: &BabString) -> (&StructureLayout, Register) {
        let layout = self.program_builder.type_manager.layout_of(name);
        let size = layout.size();

        let dst = self.register_allocator.next();

        self.instructions.push(Instruction::StackAlloc {
            dst,
            size,
        });

        (layout, dst)
    }

    pub fn allocate_array(&mut self, item_structure_name: &BabString, size: usize) -> (&StructureLayout, Register) {
        let layout = self.program_builder.type_manager.layout_of(item_structure_name);
        let size = layout.size() * size;

        let dst = self.register_allocator.next();

        self.instructions.push(Instruction::StackAlloc {
            dst,
            size,
        });

        (layout, dst)
    }

    pub fn allocate_array_type_id(&mut self, item_ty_id: TypeId, size: usize) -> (&StructureLayout, Register) {
        let layout = self.program_builder.type_manager.layout(item_ty_id);
        let size = layout.size() * size;

        let dst = self.register_allocator.next();

        self.instructions.push(Instruction::StackAlloc {
            dst,
            size,
        });

        (layout, dst)
    }

    /// Stores a value at the `base_ptr` offset by `offset`.
    pub fn store_ptr(&mut self, base_ptr: Register, offset: Operand, value: impl Into<Register>, typ: PrimitiveType) {
        self.instructions.push(Instruction::StorePtr {
            base_ptr,
            offset,
            value: value.into(),
            typ,
        });
    }

    #[must_use]
    pub fn load_ptr(&mut self, base_ptr: Register, offset: impl Into<Operand>, typ: PrimitiveType) -> Register {
        let destination = self.register_allocator.next();

        self.instructions.push(Instruction::LoadPtr {
            destination,
            base_ptr,
            offset: offset.into(),
            typ,
        });

        destination
    }

    #[must_use]
    pub fn layout_of(&self, ty: TypeId) -> &StructureLayout {
        self.program_builder.type_manager.layout(ty)
    }

    #[must_use]
    pub fn load_this(&self) -> Option<(TypeInfo, Register, PrimitiveType)> {
        self.this.clone()
    }

    pub fn move_register(&mut self, destination: Register, source: Register, typ: PrimitiveType) {
        let source = Operand::Register(source);
        self.instructions.push(Instruction::Move { source, destination, typ });
    }

    #[must_use]
    pub fn unary_negate(&mut self, typ: PrimitiveType, src: Register) -> Register {
        let dst = self.register_allocator.next();

        self.instructions.push(Instruction::Negate {
            typ,
            dst,
            src,
        });

        dst
    }

    #[must_use]
    pub fn load_effective_address(&mut self, offset: DataSectionOffset) -> Register {
        let destination = self.register_allocator.next();

        self.instructions.push(Instruction::MoveAddress {
            destination,
            offset,
        });

        destination
    }

    #[must_use]
    pub const fn pointer_size(&self) -> usize {
        // TODO: depends on platform
        8
    }

    #[must_use]
    pub const fn pointer_primitive_type(&self) -> PrimitiveType {
        PrimitiveType::new(self.pointer_size(), false)
    }

    #[must_use]
    pub fn size_of_type_id(&self, ty: TypeId) -> usize {
        self.program_builder.type_manager.layout(ty).size()
    }

    #[must_use]
    pub fn name_of_type_id(&self, ty: TypeId) -> &BabString {
        self.program_builder.type_manager.layout(ty).name()
    }

    #[must_use]
    pub fn size_of_type_info(&self, ty: &TypeInfo) -> usize {
        match ty {
            TypeInfo::Array(..) => self.pointer_size(),
            TypeInfo::Pointer(..) => self.pointer_size(),
            TypeInfo::Plain(ty) => self.size_of_type_id(*ty),
        }
    }

    #[must_use]
    pub fn primitive_type_of(&self, ty: &TypeInfo) -> PrimitiveType {
        match ty {
            TypeInfo::Array(..) => PrimitiveType::new(self.pointer_size(), false),
            TypeInfo::Pointer(..) => PrimitiveType::new(self.pointer_size(), false),
            TypeInfo::Plain(ty) => self.program_builder.type_manager.layout(*ty).primitive_type(),
        }
    }

    #[must_use]
    pub fn malloc(&mut self, size: Operand) -> Register {
        let size_register = match size {
            Operand::Immediate(imm) => self.load_immediate(imm, self.pointer_primitive_type()),
            Operand::Register(reg) => reg,
        };

        let size = FunctionArgument::new(
            size_register,
            self.program_builder.type_manager.usize_type_info(),
            self.program_builder.type_manager.usize_primitive_type(),
        );

        self.call(BabString::new_static("malloc"), [
            size
        ].to_vec())
    }

    #[must_use]
    pub const fn return_type(&self) -> PrimitiveType {
        self.return_ty
    }

    #[must_use]
    pub fn return_type_of(&self, name: &BabString) -> TypeId {
        self.program_builder.function_return_types.get(name).copied().unwrap_or_else(|| {
            log::warn!("We don't know the return type of {name}!");
            TypeId::G32
        })
    }

    pub fn promote_to_stack(&mut self, variable: BabString) -> (Register, PrimitiveType) {
        let local = self.locals.get(&variable).unwrap();
        let original_reg = local.register;
        let ty = local.type_info.clone();

        let TypeInfo::Plain(type_id) = ty else {
            panic!("Ongeldige stackpromotie van `{variable}`!");
        };

        let primitive_typ = self.primitive_type_of(&TypeInfo::Plain(type_id));

        let dst = self.register_allocator.next();
        self.instructions.push(Instruction::StackAlloc { dst, size: primitive_typ.bytes() });
        self.instructions.push(Instruction::StorePtr { base_ptr: dst, offset: Operand::zero(), value: original_reg, typ: primitive_typ });

        self.locals.insert(variable, FunctionLocal {
            register: dst,
            type_info: TypeInfo::Pointer(Box::new(ty)),
            primitive: primitive_typ,
        });

        (dst, primitive_typ)
    }
}

#[cfg(test)]
mod tests {

    use babbelaar::BabString;

    use crate::{ArgumentList, ProgramBuilder};

    #[test]
    fn test_empty_function() {
        ProgramBuilder::new()
            .build_function(BabString::new("Hallo"), ArgumentList::new(), |builder| {
                _ = builder;
            });
    }

}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionLocal {
    pub(super) register: Register,
    pub(super) type_info: TypeInfo,
    pub(super) primitive: PrimitiveType,
}
