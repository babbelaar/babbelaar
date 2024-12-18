// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Error)]
pub enum Signal {
    #[error("unknown error")]
    Other(i32),

    #[error("terminal line hangup")]
    SIGHUP,

    #[error("interrupt program")]
    SIGINT,

    #[error("quit program")]
    SIGQUIT,

    #[error("illegal instruction")]
    SIGILL,

    #[error("trace trap")]
    SIGTRAP,

    #[error("abort program (formerly SIGIOT)")]
    SIGABRT,

    #[error("emulate instruction executed")]
    SIGEMT,

    #[error("floating-point exception")]
    SIGFPE,

    #[error("kill program")]
    SIGKILL,

    #[error("bus error")]
    SIGBUS,

    #[error("segmentation violation")]
    SIGSEGV,

    #[error("non-existent system call invoked")]
    SIGSYS,

    #[error("write on a pipe with no reader")]
    SIGPIPE,

    #[error("real-time timer expired")]
    SIGALRM,

    #[error("software termination signal")]
    SIGTERM,

    #[error("urgent condition present on socket")]
    SIGURG,

    #[error("stop (cannot be caught or ignored)")]
    SIGSTOP,

    #[error("stop signal generated from keyboard")]
    SIGTSTP,

    #[error("continue after stop")]
    SIGCONT,

    #[error("child status has changed")]
    SIGCHLD,

    #[error("background read attempted from control terminal")]
    SIGTTIN,

    #[error("background write attempted to control terminal")]
    SIGTTOU,

    #[error("I/O is possible on a descriptor (see fcntl(2))")]
    SIGIO,

    #[error("cpu time limit exceeded (see setrlimit(2))")]
    SIGXCPU,

    #[error("file size limit exceeded (see setrlimit(2))")]
    SIGXFSZ,

    #[error("virtual time alarm (see setitimer(2))")]
    SIGVTALRM,

    #[error("profiling timer alarm (see setitimer(2))")]
    SIGPROF,

    #[error("Window size change")]
    SIGWINCH,

    #[error("status request from keyboard")]
    SIGINFO,

    #[error("User defined signal 1")]
    SIGUSR1,

    #[error("User defined signal 2")]
    SIGUSR2,

    #[error("Stack fault on coprocessor (unused)")]
    SIGSTKFLT,

    #[error("Power failure (System V)")]
    SIGPWR,
}

#[cfg(target_os = "macos")]
impl Signal {
    #[must_use]
    pub const fn new(num: i32) -> Self {
        match num {
            1 => Self::SIGHUP,
            2 => Self::SIGINT,
            3 => Self::SIGQUIT,
            4 => Self::SIGILL,
            5 => Self::SIGTRAP,
            6 => Self::SIGABRT,
            7 => Self::SIGEMT,
            8 => Self::SIGFPE,
            9 => Self::SIGKILL,
            10 => Self::SIGBUS,
            11 => Self::SIGSEGV,
            12 => Self::SIGSYS,
            13 => Self::SIGPIPE,
            14 => Self::SIGALRM,
            15 => Self::SIGTERM,
            16 => Self::SIGURG,
            17 => Self::SIGSTOP,
            18 => Self::SIGTSTP,
            19 => Self::SIGCONT,
            20 => Self::SIGCHLD,
            21 => Self::SIGTTIN,
            22 => Self::SIGTTOU,
            23 => Self::SIGIO,
            24 => Self::SIGXCPU,
            25 => Self::SIGXFSZ,
            26 => Self::SIGVTALRM,
            27 => Self::SIGPROF,
            28 => Self::SIGWINCH,
            29 => Self::SIGINFO,
            30 => Self::SIGUSR1,
            31 => Self::SIGUSR2,

            _ => Self::Other(num),
        }
    }
}

#[cfg(target_os = "linux")]
impl Signal {
    pub const fn new(num: i32) -> Self {
        match num {
            1 => Self::SIGHUP,
            2 => Self::SIGINT,
            3 => Self::SIGQUIT,
            4 => Self::SIGILL,
            5 => Self::SIGTRAP,
            6 => Self::SIGABRT,
            7 => Self::SIGBUS,
            8 => Self::SIGFPE,
            9 => Self::SIGKILL,
            10 => Self::SIGUSR1,
            11 => Self::SIGSEGV,
            12 => Self::SIGUSR2,
            13 => Self::SIGPIPE,
            14 => Self::SIGALRM,
            15 => Self::SIGTERM,
            16 => Self::SIGSTKFLT,
            17 => Self::SIGCHLD,
            18 => Self::SIGCONT,
            19 => Self::SIGSTOP,
            20 => Self::SIGTSTP,
            21 => Self::SIGTTIN,
            22 => Self::SIGTTOU,
            23 => Self::SIGURG,
            24 => Self::SIGXCPU,
            25 => Self::SIGXFSZ,
            26 => Self::SIGVTALRM,
            27 => Self::SIGPROF,
            28 => Self::SIGWINCH,
            29 => Self::SIGIO,
            30 => Self::SIGPWR,
            31 => Self::SIGSYS,

            _ => Self::Other(num),
        }
    }
}

impl From<i32> for Signal {
    fn from(value: i32) -> Self {
        Self::new(value)
    }
}

#[cfg(not(any(target_os = "macos", target_os = "linux")))]
impl Signal {
    #[must_use]
    pub const fn new(num: i32) -> Self {
        Self::Other(num)
    }
}
