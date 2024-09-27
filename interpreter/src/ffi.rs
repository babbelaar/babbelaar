// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use babbelaar::{Attribute, BabString, PrimaryExpression, Value};
use libloading::Library;
use log::error;

type Primitive = i64;

type F0 = unsafe extern fn() -> Primitive;
type F1 = unsafe extern fn(Primitive) -> Primitive;
type F2 = unsafe extern fn(Primitive, Primitive) -> Primitive;
type F3 = unsafe extern fn(Primitive, Primitive, Primitive) -> Primitive;
type F4 = unsafe extern fn(Primitive, Primitive, Primitive, Primitive) -> Primitive;

pub struct FFIManager {
    libc: Library,
}

impl FFIManager {
    #[must_use]
    pub fn new() -> Self {
        Self {
            libc: load_libc(),
        }
    }

    pub fn execute(&self, attrib: &Attribute, arguments: Vec<Value>) -> Value {
        let arguments = arguments.iter()
            .map(|x| match x {
                Value::Array { .. } => todo!(),
                Value::ArrayElementReference { .. } => todo!(),
                Value::Bool(b) => *b as Primitive,
                Value::Function { .. } => todo!(),
                Value::Null => 0,
                Value::Integer(int) => *int as Primitive,
                Value::String(s) => s.as_ptr() as Primitive,
                Value::MethodReference { .. } => todo!(),
                Value::MethodIdReference { .. } => todo!(),
                Value::Object { .. } => todo!(),
            })
            .collect();

        self.do_execute(attrib, arguments)
    }

    fn do_execute(&self, attrib: &Attribute, arguments: Vec<i64>) -> Value {
        for arg in &attrib.arguments {
            if arg.name.value() == "naam" {
                let PrimaryExpression::StringLiteral(literal) = arg.value.value() else {
                    error!("Invalid @uitheems `naam`!");
                    return Value::Null;
                };

                return self.execute_named_libc(literal, arguments)
            }
        }

        error!("@uitheems has no `naam` argument");
        Value::Null
    }

    fn execute_named_libc(&self, name: &BabString, arguments: Vec<i64>) -> Value {
        let symbol = name.as_bytes();
        let return_value = match arguments[..] {
            [] => {
                let func = unsafe { self.libc.get::<F0>(symbol) }.unwrap();
                unsafe { func() }
            }

            [v0] => {
                let func = unsafe { self.libc.get::<F1>(symbol) }.unwrap();
                unsafe { func(v0) }
            }

            [v0, v1] => {
                let func = unsafe { self.libc.get::<F2>(symbol) }.unwrap();
                unsafe { func(v0, v1) }
            }

            [v0, v1, v2] => {
                let func = unsafe { self.libc.get::<F3>(symbol) }.unwrap();
                unsafe { func(v0, v1, v2) }
            }

            [v0, v1, v2, v3] => {
                let func = unsafe { self.libc.get::<F4>(symbol) }.unwrap();
                unsafe { func(v0, v1, v2, v3) }
            }

            _ => {
                error!("Too many arguments to destructure");
                return Value::Null
            }
        };

        Value::Integer(return_value)
    }
}

fn load_libc() -> Library {
    if cfg!(target_os = "linux") {
        return unsafe { Library::new("/lib/x86_64-linux-gnu/libc.so.6") }.unwrap();
    }

    if cfg!(target_os = "windows") {
        // TODO: Check if this is safe.

        // Currently, the libs are specified relative to the DLL include path,
        // Since the `C:\Windows` path may be different on different systems, etc.
        if let Ok(dll) = unsafe { Library::new("ucrtbase.dll") } {
            return dll;
        }

        return unsafe { Library::new("msvcrt.dll") }.unwrap();
    }

    if cfg!(target_os = "macos") {
        return unsafe { Library::new("/usr/lib/libSystem.B.dylib") }.unwrap();
    }

    panic!("Dit besturingssysteem wordt niet ondersteund!")
}
