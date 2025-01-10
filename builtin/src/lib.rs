// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

#![feature(breakpoint)]

use std::{arch::breakpoint, borrow::Cow, slice};

#[no_mangle]
pub unsafe extern "C" fn schrijf(ptr: *const u8) {
    println!("{}", load_str(ptr));
}

#[no_mangle]
pub unsafe extern "C" fn Slinger(ptr: *const u8) -> *const u8 {
    let str = unsafe { load_str(ptr) }.to_string();
    str.leak().as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn __ingebouwd_stoppunt() {
    breakpoint();
}

#[no_mangle]
pub unsafe extern "C" fn Slinger__lengte(ptr: *const u8) -> u64 {
    let len = unsafe { strlen(ptr) };
    len as _
}

#[no_mangle]
pub unsafe extern "C" fn Slinger__voegSamen(lhs: *const u8, rhs: *const u8) -> *mut u8 {
    let lhs = unsafe { load_str(lhs) };
    let rhs = unsafe { load_str(rhs) };

    let result = format!("{lhs}{rhs}");
    result.leak().as_mut_ptr()
}

#[must_use]
unsafe fn load_str<'a>(data: *const u8) -> Cow<'a, str> {
    let len = strlen(data);
    let slice = slice::from_raw_parts(data, len);
    String::from_utf8_lossy(slice)
}

#[must_use]
pub unsafe fn strlen(mut ptr: *const u8) -> usize {
    let mut size = 0;

    while ptr.read() != 0 {
        size += 1;
        ptr = ptr.add(1);
    }

    size
}

// #[cfg(not(feature = "no-win-main"))]
#[cfg(windows)]
unsafe extern "C" {
    #[must_use]
    unsafe fn hoofd() -> i32;
}

// #[cfg(not(feature = "no-win-main"))]
#[cfg(windows)]
#[must_use]
#[no_mangle]
pub unsafe extern "C"  fn babbelaar_hoofd() {
    let code = hoofd();
    std::process::exit(code);
}
