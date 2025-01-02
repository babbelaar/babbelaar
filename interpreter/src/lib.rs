// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#![feature(thread_id_value)]

pub use babbelaar::*;

mod data;
mod debugger;
mod error;
mod ffi;
mod interpreter;
mod scope;

pub use self::{
    data::{
        InterpreterExtension,
        InterpreterFunction,
        InterpreterInterface,
        InterpreterStructure,
    },
    debugger::{
        Debugger,
        DebuggerFunction,
        DebuggerFunctionType,
    },
    error::{
        ErrorPrinter,
        RuntimeError,
    },
    ffi::FFIManager,
    interpreter::Interpreter,
    scope::Scope,
};
