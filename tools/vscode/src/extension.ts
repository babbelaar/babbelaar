// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

import {
	ExtensionContext,
	window,
} from "vscode";

import { BabbelaarContext } from "./babbelaarContext";
import { BabbelaarLsp } from "./babbelaarLsp";
import { BabbelaarDebugAdapter } from "./debugAdapter";
import { BabbelaarCommands } from "./commands";
import { BabbelaarLog } from "./logger";
import { registerTaskProvider } from "./tasks";

export async function activate(context: ExtensionContext) {
	const babbelaarContext: BabbelaarContext = {
		ext: context,
		version: context.extension.packageJSON.version?.trim(),
		taskProvider: null,
	};

	BabbelaarLog.info(`Babbelaar-extensie is geactiveerd met gedetecteerde versie ${babbelaarContext.version}`);

	if (!babbelaarContext.version || `${babbelaarContext.version}`.length === 0) {
		window.showErrorMessage("Ongeldige Babbelaar-extensie: versie is niet ingesteld.");
	}

	await BabbelaarDebugAdapter.register(babbelaarContext);
	await registerTaskProvider(babbelaarContext);
	await BabbelaarCommands.register(babbelaarContext);
	await BabbelaarLsp.startClient(babbelaarContext);

	BabbelaarLog.info(`Babbelaar-extensie is volledig geactiveerd.`);
}

export function deactivate(): Thenable<void> {
	BabbelaarLog.info(`Babbelaar-extensie wordt gedeactiveerd...`);
	return BabbelaarLsp.stopClient();
}
