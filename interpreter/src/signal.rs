// Copyright (C) 2025 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.


#[must_use]
pub fn human_name(signal: i32) -> Option<&'static str> {
    if cfg!(target_os = "macos") {
        return human_name_macos(signal);
    }

    None
}

// https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man3/signal.3.html
fn human_name_macos(signal: i32) -> Option<&'static str> {
    Some(match signal {
        2 => "SIGINT: onderbroken",
        3 => "SIGQUIT: geforceerd afgebroken",
        4 => "SIGILL: ongeldige instructie",
        5 => "SIGTRAP: processorval",
        6 => "SIGABRT: voorttijdig afgebroken",
        8 => "SIGFPE: zwevendekommagetalsuitzondering",
        9 => "SIGKILL: geforceerd getermineerd",
        10 => "SIGBUS: geheugenuitlijningsfout",
        11 => "SIGSEGV: geheugentoegangsfout",
        12 => "SIGSYS: niet-bestaande systeemaanroep verzocht",
        15 => "SIGTERM: geforceerd getermineerd",
        _ => return None,
    })
}
