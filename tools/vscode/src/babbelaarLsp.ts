// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

import { Executable, LanguageClient, LanguageClientOptions, ServerOptions, TextDocumentFeature } from "vscode-languageclient/node";
import { BabbelaarContext } from "./babbelaarContext";
import { ensureLspServer } from "./downloadBabbelaar";
import { InlayHint, window, workspace } from "vscode";

let client: LanguageClient;

const BabbelaarLsp = {
	startClient,
	stopClient,
};

export { BabbelaarLsp };

async function startClient(context: BabbelaarContext) {
	const command = await ensureLspServer(context);
	if (!command)
		return;

	stopClient();

	const traceOutputChannel = window.createOutputChannel("Babbelaar Taalserveerder trace");
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
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
		},
		traceOutputChannel,
	};

	// Create the language client and start the client.
	client = new LanguageClient("babbelaar-lsp", "Babbelaar Taalondersteuning", serverOptions, clientOptions);
	client.registerProposedFeatures();
	// activateInlayHints(context);
	client.start();
}

export async function stopClient() {
    if (!client)
        return;
    client.stop();
}
