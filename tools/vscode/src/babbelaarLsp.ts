// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

import { Command, Executable, ExecuteCommandRequest, LanguageClient, LanguageClientOptions, ServerOptions } from "vscode-languageclient/node";
import { BabbelaarContext } from "./babbelaarContext";
import { ensureLspServer } from "./downloadBabbelaar";
import { window, workspace } from "vscode";

let client: LanguageClient;

const BabbelaarLsp = {
	startClient,
	stopClient,
	sendBabbelaarNotification,
};

export { BabbelaarLsp };

async function startClient(context: BabbelaarContext) {
	const command = await ensureLspServer(context);
	if (!command)
		return;

	stopClient();

	const traceOutputChannel = window.createOutputChannel("Babbelaar Taaldienaar-trace");
	const run: Executable = {
		command,
		options: {
			env: {
				...process.env,
				// eslint-disable-next-line @typescript-eslint/naming-convention
				RUST_LOG: "debug",
			},
		},
	};
	const serverOptions: ServerOptions = {
		run,
		debug: run,
	};

	let clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: "file", language: "babbelaar" }],
		synchronize: {
			fileEvents: workspace.createFileSystemWatcher("**/.bab"),
		},
		traceOutputChannel,
	};

	// Create the language client and start the client.
	client = new LanguageClient("babbelaar-lsp", "Babbelaar Taalondersteuning", serverOptions, clientOptions);
	client.start();
	console.log("Client is gestart");
}

async function sendBabbelaarNotification(method: string, body: any) {
	await client.sendNotification("experimental/babbelaar/" + method, body);
}

export async function stopClient() {
    if (!client)
        return;
    client.stop();
}
