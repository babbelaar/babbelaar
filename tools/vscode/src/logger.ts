// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

import {
    OutputChannel,
    window,
 } from "vscode";

let extensionOutputChannel: OutputChannel|null = null;

function log(level: string, message: string) {
    if (extensionOutputChannel == null) {
        extensionOutputChannel = window.createOutputChannel("Babbelaar");
	    extensionOutputChannel.appendLine("Babbelaar-uitbreiding is geactiveerd.");
    }

    const dateTime = Intl.DateTimeFormat("nl", {
        dateStyle: "short",
        timeStyle: "long"
    }).format(new Date());
    extensionOutputChannel.appendLine(`[${dateTime}] [${level}] ${message}`);
}

const BabbelaarLog = {
    info: (message: string) => log("Informatie", message),

    warn: (message: string) => log("Waarschuwing", message),

    error: (message: string) => log("Fout", message),
};

export { BabbelaarLog }
