// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

use std::{
    io::{BufRead, BufReader, BufWriter, Write},
    process::{ChildStdin, ChildStdout, Command, Stdio},
};

#[test]
fn simple_debugger() {
    let mut path = std::env::current_exe().unwrap();
    path.pop();
    path.pop();


    let mut interpreter = path.clone();
    interpreter.push("babbelaar-interpreter");

    path.pop();
    path.pop();
    path.push("interpreter");
    path.push("tests");
    path.push("debug_adapter.bab");

    let process = Command::new(interpreter)
        .arg("debug")
        .arg(path.display().to_string())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    let mut reader = Reader {
        reader: BufReader::new(process.stdout.unwrap()),
    };

    let mut writer = Writer {
        writer: BufWriter::new(process.stdin.unwrap()),
    };

    writer.write(br#"{"command":"initialize","arguments":{"clientID":"vscode","clientName":"Visual Studio Code","adapterID":"babbelaar","pathFormat":"path","linesStartAt1":true,"columnsStartAt1":true,"supportsVariableType":true,"supportsVariablePaging":true,"supportsRunInTerminalRequest":true,"locale":"en","supportsProgressReporting":true,"supportsInvalidatedEvent":true,"supportsMemoryReferences":true,"supportsArgsCanBeInterpretedByShell":true,"supportsMemoryEvent":true,"supportsStartDebuggingRequest":true},"type":"request","seq":1}
"#);

    reader.read_message();
    reader.read_message();

    writer.write(br#"{"command":"launch","arguments":{"type":"babbelaar","name":"Launch","request":"launch","program":"/Users/tager/Developer/Public/Babbelaar/example-workspace/test.bab","stopOnEntry":true,"__sessionId":"cd829ea4-30a6-47f4-919c-a98f2ccbd764"},"type":"request","seq":2}
"#);

    writer.write(br#"{"command":"setFunctionBreakpoints","arguments":{"breakpoints":[]},"type":"request","seq":3}
"#);

    reader.read_message();
    reader.read_message();

    writer.write(br#"{"command":"configurationDone","type":"request","seq":4}
"#);

    reader.read_message();

    writer.write(br#"{"command":"threads","type":"request","seq":5}
"#);

    reader.read_message();
}

struct Writer {
    writer: BufWriter<ChildStdin>,
}

impl Writer {
    pub fn write(&mut self, str: &[u8]) {
        self.writer.write_fmt(format_args!("Content-Length: {}\r\n\r\n", str.len())).unwrap();
        self.writer.write(str).unwrap();
        self.writer.flush().unwrap();
    }
}

struct Reader {
    reader: BufReader<ChildStdout>,
}

impl Reader {
    pub fn read_message(&mut self) {
        self.reader.read_line(&mut String::new()).unwrap();
        self.reader.read_line(&mut String::new()).unwrap();
    }
}
