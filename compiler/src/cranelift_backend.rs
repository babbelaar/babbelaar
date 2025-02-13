// Copyright (C) 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::collections::HashMap;

use crate::{
    Architecture,
    CompiledFunction,
    DataSectionOffset,
    Environment,
    Function as IrFunction,
    FunctionArgument,
    Immediate as IrImmediate,
    Instruction as IrInstruction,
    JumpCondition,
    Label,
    MathOperation,
    Operand as IrOperand,
    OperatingSystem,
    Platform,
    PrimitiveType,
    Register,
    Relocation,
    RelocationMethod,
    RelocationType,
};

use babbelaar::BabString;
use cranelift_codegen::{
    binemit::Reloc, control::ControlPlane, ir::{
        types::*, Function as ClFunction, GlobalValue, UserExternalName, UserExternalNameRef, UserFuncName
    }, verify_function, Context, FinalizedRelocTarget
};

use cranelift::prelude::{
    isa::CallConv,
    *,
};
use target_lexicon::Triple;


pub struct CraneliftBackend;

impl CraneliftBackend {
    #[must_use]
    pub fn compile(idx: usize, function: &IrFunction, platform: &Platform) -> CompiledFunction {
        log::trace!("Werkwijze '{}' aan het compileren...", function.name());

        let call_conv = match (platform.environment(), platform.architecture()) {
            (Environment::Darwin, Architecture::AArch64) => CallConv::AppleAarch64,
            (Environment::Darwin, Architecture::X86_64) => CallConv::SystemV,
            (Environment::Gnu, _) => CallConv::SystemV,
            (Environment::MsVC, _) => CallConv::WindowsFastcall,
        };

        let name = UserFuncName::user(1, idx as u32);
        let mut sig = Signature::new(call_conv);

        if function.return_ty.bytes() != 0 {
            sig.returns.push(AbiParam::new(cl_type_for_primitive_type(function.return_ty)));
        }

        for param in function.arguments() {
            sig.params.push(AbiParam::new(cl_type_for_primitive_type(param.primitive_type())));
        }

        let mut func = ClFunction::with_name_signature(name, sig);

        let mut ctx = FunctionBuilderContext::new();
        let frontend_info = CraneliftFrontend::compile(function, platform, &mut func, &mut ctx, call_conv);

        let mut builder = settings::builder();
        builder.enable("is_pic").unwrap();
        builder.set("unwind_info", "false").unwrap();
        let flags = settings::Flags::new(builder);
        let res = verify_function(&func, &flags);
        log::trace!("Werkwijze '{}' gecompileerd door Cranelift: {func}", function.name());
        if let Err(errors) = res {
            panic!("Cranelift-fout: {}", errors);
        }

        let mut context = Context::for_function(func);
        let mut ctrl_plane = ControlPlane::default();
        let isa = isa::lookup(cl_triple_from_platform(platform))
            .unwrap()
            .finish(flags)
            .unwrap();

        context.set_disasm(true);
        let result = context.compile(isa.as_ref(), &mut ctrl_plane).unwrap();

        let mut relocations = Vec::new();

        for reloc in result.buffer.relocs() {
            let ty = match reloc.target {
                FinalizedRelocTarget::ExternalName(ExternalName::User(usr)) => {
                    frontend_info.relocation_map.get(&usr).unwrap().clone()
                }

                _ => {
                    todo!("Support target of reloc {reloc:#?}");
                }
            };

            match reloc.kind {
                Reloc::Abs8 => {
                    relocations.push(Relocation {
                        method: RelocationMethod::GenericAbsolute {
                            bits: 64,
                            addend: reloc.addend,
                        },
                        ty: ty.clone(),
                        offset: reloc.offset as _,
                    });
                }

                Reloc::Arm64Call => {
                    relocations.push(Relocation {
                        method: RelocationMethod::AArch64BranchLink,
                        ty,
                        offset: reloc.offset as _,
                    });
                }

                Reloc::Aarch64AdrGotPage21 => {
                    relocations.push(Relocation {
                        method: RelocationMethod::Aarch64GotLoadPage21 {
                            addend: reloc.addend,
                        },
                        ty,
                        offset: reloc.offset as _,
                    });
                }

                Reloc::Aarch64Ld64GotLo12Nc => {
                    relocations.push(Relocation {
                        method: RelocationMethod::Aarch64GotLoadPageOff12 {
                            addend: reloc.addend,
                        },
                        ty,
                        offset: reloc.offset as _,
                    });
                }

                Reloc::X86CallPCRel4 => {
                    relocations.push(Relocation {
                        method: RelocationMethod::Amd64CallNearRelative,
                        ty,
                        offset: reloc.offset as _,
                    });
                }

                Reloc::X86GOTPCRel4 => {
                    relocations.push(Relocation {
                        method: RelocationMethod::Amd64GotPcRelative4 {
                            addend: reloc.addend,
                        },
                        ty,
                        offset: reloc.offset as _,
                    });
                }

                _ => {
                    todo!("Ondersteun doelsrelocatie {reloc:#?}");
                }
            }
        }

        CompiledFunction {
            name: function.name().clone(),
            byte_code: result.code_buffer().to_vec(),
            relocations,
        }
    }
}

struct FrontendCompileInfo {
    relocation_map: HashMap<UserExternalNameRef, RelocationType>,
}

#[derive(Debug, Clone, Copy)]
struct Compare {
    ty: PrimitiveType,
    lhs: Register,
    rhs: IrOperand,
}

struct CraneliftFrontend<'a> {
    builder: FunctionBuilder<'a>,
    call_conv: CallConv,
    platform: &'a Platform,

    register_map: HashMap<Register, VariableInfo>,
    label_map: HashMap<Label, Block>,
    function_names: HashMap<BabString, UserExternalNameRef>,
    global_value_map: HashMap<DataSectionOffset, GlobalValue>,
    relocation_map: HashMap<UserExternalNameRef, RelocationType>,

    current_block: Block,

    last_was_jump: bool,
    last_compare: Option<Compare>,
}

impl<'a> CraneliftFrontend<'a> {
    #[must_use]
    fn compile(
        function: &IrFunction,
        platform: &'a Platform,
        func: &'a mut ClFunction,
        ctx: &'a mut FunctionBuilderContext,
        call_conv: CallConv,
    ) -> FrontendCompileInfo {
        let mut builder = FunctionBuilder::new(func, ctx);

        let root_block = builder.create_block();
        builder.append_block_params_for_function_params(root_block);
        builder.switch_to_block(root_block);

        let mut label_map = HashMap::new();
        for label in function.labels() {
            let block = builder.create_block();
            label_map.insert(label, block);
        }

        let mut this = Self {
            builder,
            call_conv,
            platform,

            label_map,
            register_map: Default::default(),
            function_names: Default::default(),
            global_value_map: Default::default(),
            relocation_map: Default::default(),

            current_block: root_block,

            last_compare: None,
            last_was_jump: false,
        };

        this.compile_func(function);
        this.builder.finalize();

        FrontendCompileInfo {
            relocation_map: this.relocation_map,
        }
    }

    fn compile_func(&mut self, function: &IrFunction) {
        for instruction in &function.instructions {
            log::trace!("Compiling instruction {instruction}");
            self.compile_instruction(function, instruction);
        }

        self.builder.seal_all_blocks();
    }

    fn compile_instruction(&mut self, function: &IrFunction, instruction: &IrInstruction) {
        let last_was_jump = self.last_was_jump;
        self.last_was_jump = false;

        match instruction {
            IrInstruction::Move { destination, source, typ } => {
                let val = self.operand(source).1;
                self.set_variable(destination, *typ, val);
            }

            IrInstruction::Return { value_reg: None } => {
                self.builder.ins().return_(&[]);
                self.last_was_jump = true;
            }

            IrInstruction::Return { value_reg: Some(register) } => {
                let value = self.builder.use_var(Variable::new(register.number()));
                let value = self.ensure_type(value, self.builder.func.signature.returns[0].value_type, function.return_ty);
                self.builder.ins().return_(&[value]);
                self.last_was_jump = true;
            }

            IrInstruction::Label(label) => {
                let block = *self.label_map.get(label).unwrap();
                if !last_was_jump {
                    self.builder.ins().jump(block, &[]);
                }

                // There can be multiple consecutive labels in Babbelaar, but empty blocks aren't allowed in Cranelift.
                self.builder.switch_to_block(block);
                self.builder.ins().nop();
            }

            IrInstruction::Call { name, arguments, variable_arguments, ret_val_reg, ret_ty } => {
                let name = self.user_func_name(name);
                let sig = self.signature_for(ret_ty, arguments, variable_arguments);
                let signature = self.builder.import_signature(sig);

                let func_ref = self.builder.import_function(ExtFuncData {
                    name: ExternalName::User(name),
                    signature,
                    colocated: true,
                });

                let mut args = Vec::new();

                for arg in arguments {
                    let val = self.operand(&IrOperand::Register(arg.register())).1;
                    let val = self.ensure_type(val, cl_type_for_primitive_type(arg.primitive_type()), arg.primitive_type());
                    args.push(val);
                }

                if self.platform.environment() == Environment::Darwin && !variable_arguments.is_empty() {
                    for _ in arguments.len()..8 {
                        args.push(self.builder.ins().iconst(I64, 0));
                    }
                }

                for arg in variable_arguments {
                    let val = self.operand(&IrOperand::Register(arg.register())).1;
                    let val = self.ensure_type(val, self.pointer_type(), arg.primitive_type());
                    args.push(val);
                }

                let inst = self.builder.ins().call(func_ref, &args);
                if let Some(ret_val_reg) = ret_val_reg {
                    let value = self.builder.inst_results(inst)[0];
                    self.set_variable(ret_val_reg, ret_ty.unwrap(), value);
                }
            }

            IrInstruction::Compare { typ, lhs, rhs } => {
                self.last_compare = Some(Compare {
                    ty: *typ,
                    lhs: *lhs,
                    rhs: *rhs,
                });
            }

            IrInstruction::Increment { typ, register } => {
                let var = self.variable(register, cl_type_for_primitive_type(*typ), *typ);
                let val = self.builder.use_var(var.variable);
                let val = self.builder.ins().iadd_imm(val, 1);
                self.set_variable(register, *typ, val);
            }

            IrInstruction::InitArg { destination, arg_idx } => {
                let value = self.builder.block_params(self.current_block)[*arg_idx];
                self.set_variable(destination, function.arguments[*arg_idx].primitive_type(), value);
            }

            IrInstruction::MoveAddress { destination, offset } => {
                let ty = self.pointer_type();
                let value = self.global_value(offset);
                let value = self.builder.ins().global_value(ty, value);

                self.set_variable(destination, PrimitiveType::U64, value);
            }

            IrInstruction::MoveCondition { destination, condition } => {
                let val = self.condition(condition);
                self.set_variable(destination, PrimitiveType::U64, val);
            }

            IrInstruction::Jump { location } => {
                let block = *self.label_map.get(location).unwrap();
                self.builder.ins().jump(block, &[]);
                self.last_was_jump = true;
            }

            IrInstruction::JumpConditional { condition, location } => {
                let then_block = *self.label_map.get(location).unwrap();
                let else_block = self.builder.create_block();
                let condition = self.condition(condition);

                self.builder.ins().brif(condition, then_block, &[], else_block, &[]);

                // We create our own fake else block, so we have to nop it to make sure the block can't be empty.
                self.builder.switch_to_block(else_block);
                self.builder.ins().nop();

                // last_was_jump is here not set because the we didn't just jump, but we already entered a block (`else_block`).
            }

            IrInstruction::MathOperation { typ, operation, destination, lhs, rhs } => {
                let x = self.operand(lhs).1;
                let y = self.operand(rhs).1;

                let ty = cl_type_for_primitive_type(*typ);
                let x = self.ensure_type(x, ty, *typ);
                let y = self.ensure_type(y, ty, *typ);

                let val = match (operation, typ.is_signed()) {
                    (MathOperation::Add, _) => self.builder.ins().iadd(x, y),
                    (MathOperation::Subtract, _) => self.builder.ins().isub(x, y),
                    (MathOperation::Multiply, _) => self.builder.ins().imul(x, y),
                    (MathOperation::Divide, false) => self.builder.ins().sdiv(x, y),
                    (MathOperation::Divide, true) => self.builder.ins().udiv(x, y),
                    (MathOperation::Modulo, false) => self.builder.ins().srem(x, y),
                    (MathOperation::Modulo, true) => self.builder.ins().urem(x, y),
                    (MathOperation::LeftShift, _) => self.builder.ins().ishl(x, y),
                    (MathOperation::RightShift, false) => self.builder.ins().sshr(x, y),
                    (MathOperation::RightShift, true) => self.builder.ins().ushr(x, y),
                    (MathOperation::Xor, _) => self.builder.ins().bxor(x, y),
                };

                self.set_variable(destination, *typ, val);
            }

            IrInstruction::Negate { typ, dst, src } => {
                let value = self.operand(&IrOperand::Register(*src)).1;
                let val = self.builder.ins().ineg(value);
                self.set_variable(dst, *typ, val);
            }

            IrInstruction::StackAlloc { dst, size } => {
                let slot = self.builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, *size as _, 1));
                let ty = self.pointer_type();
                let value = self.builder.ins().stack_addr(ty, slot, 0);
                self.set_variable(dst, PrimitiveType::U64, value);
            }

            IrInstruction::LoadPtr { destination, base_ptr, offset, typ } => {
                let ptr = self.operand(&IrOperand::Register(*base_ptr)).1;
                let ptr = self.ensure_pointer_type(ptr);

                let offset = self.operand(offset).1;
                let offset = self.ensure_pointer_type(offset);

                let p = self.builder.ins().iadd(ptr, offset);
                let ty = cl_type_for_primitive_type(*typ);

                let val = self.builder.ins().load(ty, MemFlags::new(), p, 0);
                self.set_variable(destination, *typ, val);
            }

            IrInstruction::StorePtr { base_ptr, offset, value, typ } => {
                let ptr = self.operand(&IrOperand::Register(*base_ptr)).1;
                let ptr = self.ensure_pointer_type(ptr);

                let offset = self.operand(offset).1;
                let offset = self.ensure_pointer_type(offset);

                let p = self.builder.ins().iadd(ptr, offset);
                let ty = cl_type_for_primitive_type(*typ);

                let val = self.operand(&IrOperand::Register(*value)).1;
                let val = self.ensure_type(val, ty, *typ);

                self.builder.ins().store(MemFlags::new(), val, p, 0);
            }
        }
    }

    fn operand(&mut self, operand: &IrOperand) -> (Type, Value) {
        match operand {
            IrOperand::Immediate(immediate) => {
                let ty = cl_type_for_immediate(immediate);
                let val = self.builder.ins().iconst(ty, immediate.as_i64());
                (ty, val)
            }

            IrOperand::Register(register) => {
                let var = Variable::new(register.number());
                let ty = self.register_map.get(register).unwrap().ty;
                let val = self.builder.use_var(var);
                (ty, val)
            }
        }
    }

    #[must_use]
    fn variable(&mut self, register: &Register, ty: Type, primitive_ty: PrimitiveType) -> VariableInfo {
        let variable = Variable::new(register.number());

        self.register_map.entry(*register)
            .or_insert_with(|| {
                self.builder.declare_var(variable, ty);

                VariableInfo {
                    variable,
                    ty,
                    primitive_ty,
                }
            })
            .clone()
    }

    #[must_use]
    fn user_func_name(&mut self, name: &BabString) -> UserExternalNameRef {
        let reloc_ty = RelocationType::Function { name: name.clone() };
        let count = self.function_names.len();
        self.function_names.entry(name.clone())
            .or_insert_with(|| {
                let name = UserExternalName::new(2, count as _);
                let name_ref = self.builder.func.declare_imported_user_function(name);
                self.relocation_map.insert(name_ref, reloc_ty);
                name_ref
            })
            .clone()
    }

    #[must_use]
    fn signature_for(&mut self, ret_ty: &Option<PrimitiveType>, arguments: &[FunctionArgument], var_args: &[FunctionArgument]) -> Signature {
        let mut sig = Signature::new(self.call_conv);

        for arg in arguments {
            let ty = cl_type_for_primitive_type(arg.primitive_type());
            sig.params.push(AbiParam::new(ty));
        }

        if self.platform.environment() == Environment::Darwin && !var_args.is_empty() {
            for _ in arguments.len()..8 {
                sig.params.push(AbiParam::new(I64));
            }
        }

        for _ in var_args {
            sig.params.push(AbiParam::new(self.pointer_type()));
        }

        if let Some(ret_ty) = ret_ty {
            sig.returns.push(AbiParam::new(cl_type_for_primitive_type(*ret_ty)));
        }

        sig
    }

    #[must_use]
    fn condition(&mut self, condition: &JumpCondition) -> Value {
        let compare = self.last_compare.clone().unwrap();

        let x = self.operand(&IrOperand::Register(compare.lhs)).1;
        let y = self.operand(&compare.rhs).1;

        let target_ty = cl_type_for_primitive_type(compare.ty);
        let x = self.ensure_type(x, target_ty, compare.ty);
        let y = self.ensure_type(y, target_ty, compare.ty);

        let cond = cl_cond_for_jump_condition(condition, compare.ty);
        let condition = self.builder.ins().icmp(cond, x, y);

        condition
    }

    #[must_use]
    fn ensure_type(&mut self, val: Value, target_ty: Type, target_primitive: PrimitiveType) -> Value {
        let current_ty = self.builder.func.dfg.value_type(val);

        if current_ty == target_ty {
            val
        } else if target_ty.wider_or_equal(current_ty) {
            if target_primitive.is_signed() {
                self.builder.ins().sextend(target_ty, val)
            } else {
                self.builder.ins().uextend(target_ty, val)
            }
        } else {
            self.builder.ins().ireduce(target_ty, val)
        }
    }

    #[must_use]
    fn ensure_pointer_type(&mut self, value: Value) -> Value {
        self.ensure_type(value, self.pointer_type(), PrimitiveType::new(8, true))
    }

    #[must_use]
    fn pointer_type(&self) -> Type {
        I64
    }

    #[must_use]
    fn global_value(&mut self, offset: &DataSectionOffset) -> GlobalValue {
        self.global_value_map.entry(*offset)
            .or_insert_with(|| {
                let name = UserExternalName::new(3, offset.section_kind() as u32 + offset.offset() as u32 * 10);
                let name = self.builder.func.declare_imported_user_function(name);

                self.relocation_map.insert(name, RelocationType::Data { section: offset.section_kind(), offset: offset.offset() });

                self.builder.create_global_value(GlobalValueData::Symbol {
                    name: ExternalName::user(name),
                    offset: Imm64::new(0),
                    colocated: true,
                    tls: false,
                })
            })
            .clone()
    }

    fn set_variable(&mut self, register: &Register, typ: PrimitiveType, val: Value) {
        let var = self.variable(register, cl_type_for_primitive_type(typ), typ);
        let val = self.ensure_type(val, var.ty, var.primitive_ty);
        self.builder.def_var(var.variable, val);
    }
}

#[derive(Debug, Clone, Copy)]
struct VariableInfo {
    variable: Variable,
    ty: Type,
    primitive_ty: PrimitiveType,
}

#[must_use]
const fn cl_type_for_immediate(immediate: &IrImmediate) -> Type {
    match immediate {
        IrImmediate::Integer8(..) => I8,
        IrImmediate::Integer16(..) => I16,
        IrImmediate::Integer32(..) => I32,
        IrImmediate::Integer64(..) => I64,
    }
}

#[must_use]
fn cl_type_for_primitive_type(ty: PrimitiveType) -> Type {
    match ty.bytes() {
        0 => I32,
        1 => I8,
        2 => I16,
        4 => I32,
        8 => I64,
        _ => todo!("Ondersteun type met grootte {ty:?}"),
    }
}

#[must_use]
fn cl_triple_from_platform(platform: &Platform) -> Triple {
    use target_lexicon::{
        Architecture as Arch,
        Aarch64Architecture,
        BinaryFormat,
        Environment as Env,
        Vendor,
        OperatingSystem as OS,
    };

    Triple {
        architecture: match platform.architecture() {
            Architecture::AArch64 => Arch::Aarch64(Aarch64Architecture::Aarch64),
            Architecture::X86_64 => Arch::X86_64,
        },
        vendor: match platform.environment() {
            Environment::Darwin => Vendor::Apple,
            _ => Vendor::Unknown,
        },
        operating_system: match platform.operating_system() {
            OperatingSystem::Linux => OS::Linux,
            OperatingSystem::MacOs => OS::Darwin(None),
            OperatingSystem::Windows => OS::Windows,
        },
        environment: match platform.environment() {
            Environment::Darwin => Env::Unknown,
            Environment::Gnu => Env::Gnu,
            Environment::MsVC => Env::Msvc,
        },
        binary_format: match platform.operating_system() {
            OperatingSystem::Linux => BinaryFormat::Elf,
            OperatingSystem::MacOs => BinaryFormat::Macho,
            OperatingSystem::Windows => BinaryFormat::Coff,
        },
    }
}

#[must_use]
fn cl_cond_for_jump_condition(condition: &JumpCondition, ty: PrimitiveType,) -> IntCC {
    match (condition, ty.is_signed()) {
        (JumpCondition::Equal, _) => IntCC::Equal,
        (JumpCondition::NotEqual, _) => IntCC::NotEqual,

        (JumpCondition::Greater, false) => IntCC::SignedGreaterThan,
        (JumpCondition::GreaterOrEqual, false) => IntCC::SignedGreaterThanOrEqual,
        (JumpCondition::Less, false) => IntCC::SignedLessThan,
        (JumpCondition::LessOrEqual, false) => IntCC::SignedLessThanOrEqual,

        (JumpCondition::Greater, true) => IntCC::UnsignedGreaterThan,
        (JumpCondition::GreaterOrEqual, true) => IntCC::UnsignedGreaterThanOrEqual,
        (JumpCondition::Less, true) => IntCC::UnsignedLessThan,
        (JumpCondition::LessOrEqual, true) => IntCC::UnsignedLessThanOrEqual,
    }
}
