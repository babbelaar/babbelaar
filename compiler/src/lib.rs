// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

mod backend;
mod compiler;
mod interpreter;
mod ir;
mod memory;
mod optimization;
mod os;
mod pipeline;
mod types;

pub use self::{
    backend::{
        AArch64CodeGenerator,
        AllocatableRegister,
        CodeGenerator,
        CompiledFunction,
        CompiledObject,
        DataSection,
        DataSectionKind,
        DataSectionOffset,
        RegisterAllocator,
        Relocation,
        RelocationMethod,
        RelocationType,
    },
    compiler::Compiler,
    interpreter::Interpreter,
    ir::{
        ArgumentList,
        Function,
        FunctionBuilder,
        Immediate,
        Instruction,
        JumpCondition,
        Label,
        MathOperation,
        Operand,
        PrimitiveType,
        Program,
        ProgramBuilder,
        Register,
    },
    memory::{
        FieldLayout,
        StructureLayout,
        TypeId,
        TypeManager,
    },
    optimization::{
        FunctionOptimizer,
        optimize_function,
        optimize_program,
    },
    os::Signal,
    pipeline::{
        LinkerError,
        Pipeline,
    },
    types::{
        Architecture,
        Environment,
        OperatingSystem,
        Platform,
        WindowsVersion,
    },
};
