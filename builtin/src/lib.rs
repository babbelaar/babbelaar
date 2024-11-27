// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{borrow::Cow, slice};

#[no_mangle]
pub unsafe extern "C" fn schrijf(ptr: *const u8) {
    println!("{}", load_str(ptr));
}

#[no_mangle]
pub unsafe extern "C" fn g32__lengte(ptr: *const u8) -> u64 {
    let len = unsafe { strlen(ptr) };
    len as _
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
