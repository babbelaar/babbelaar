// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#![feature(iter_array_chunks)]

mod analysis;
mod backend;
mod compiler;
mod cranelift_backend;
mod interpreter;
mod ir;
mod memory;
mod optimization;
mod os;
mod pipeline;
mod types;

pub use self::{
    analysis::ControlFlowGraph,
    backend::{
        AArch64CodeGenerator,
        AbstractRegister,
        Amd64CodeGenerator,
        AllocatableRegister,
        Backend,
        CodeGenerator,
        CompiledFunction,
        CompiledObject,
        DataSection,
        DataSectionKind,
        DataSectionOffset,
        LifeAnalysis,
        RegisterAllocator,
        RegisterLifetime,
        Relocation,
        RelocationMethod,
        RelocationType,
        TargetBranchInfo,
        TargetInstruction,
        TargetInstructionInfo,
    },
    compiler::Compiler,
    interpreter::Interpreter,
    ir::{
        ArgumentList,
        Function,
        FunctionArgument,
        FunctionAttribute,
        FunctionBuilder,
        FunctionParameter,
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
        RefCount,
        StackAllocator,
        StructureLayout,
        TypeId,
        TypeInfo,
        TypeManager,
    },
    optimization::{
        FunctionOptimizer,
        OptimizationContext,
        optimize_program,
    },
    os::{
        LinkerPath,
        NtStatus,
        Signal,
    },
    pipeline::{
        LinkerError,
        Pipeline,
    },
    types::{
        Architecture,
        Environment,
        Graph,
        OperatingSystem,
        Platform,
        PlatformOptions,
        WindowsVersion,
    },
};
