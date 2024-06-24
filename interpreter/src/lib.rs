// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#![feature(thread_id_value)]

pub use babbelaar::*;

mod debug_adapter;
mod debugger;
mod interpreter;
mod scope;

pub use self::{
    debug_adapter::DebugAdapter,
    debugger::{
        Debugger,
        DebuggerFunction,
        DebuggerFunctionType,
    },
    interpreter::Interpreter,
    scope::Scope,
};
