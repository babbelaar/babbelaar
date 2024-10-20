// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{borrow::Cow, env::var, process::exit};

use babbelaar::{Attribute, BabString, Constants, PrimaryExpression, Value};
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
                Value::Character(char) => *char as Primitive,
                Value::String(s) => s.as_ptr() as Primitive,
                Value::MethodReference { .. } => todo!(),
                Value::MethodIdReference { .. } => todo!(),
                Value::Object { .. } => todo!(),
                Value::Pointer { address, .. } => *address as Primitive,
            })
            .collect();

        self.do_execute(attrib, arguments)
    }

    fn do_execute(&self, attrib: &Attribute, arguments: Vec<i64>) -> Value {
        for arg in attrib.arguments.value() {
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
    for path in libc_names() {
        if let Ok(library) = unsafe { Library::new(path.as_ref()) } {
            return library;
        }
    }

    eprintln!("Fout: kon de C-bibliotheek op dit platform niet vinden.");
    eprintln!("Je kunt de omgevingsvariabele `{}` naar het juist pad zetten en het programma opnieuw uitvoeren.", Constants::ENV_LIBRARY_C);

    exit(1);
}

fn libc_names() -> Vec<Cow<'static, str>> {
    let mut name_or_paths: Vec<Cow<'static, str>> = Vec::new();

    if let Ok(path) = var(Constants::ENV_LIBRARY_C) {
        name_or_paths.push(path.into());
    }

    if cfg!(target_os = "macos") {
        name_or_paths.push("/usr/lib/libSystem.B.dylib".into());
    }

    if cfg!(target_os = "windows") {
        // TODO: Check if this is safe.

        // Currently, the libs are specified relative to the DLL include path,
        // Since the `C:\Windows` path may be different on different systems, etc.
        name_or_paths.push("ucrtbase.dll".into());
        name_or_paths.push("msvcrt.dll".into());
    }

    if cfg!(target_os = "linux") {
        name_or_paths.push("/lib/x86_64-linux-gnu/libc.so.6".into());
    }

    name_or_paths
}
