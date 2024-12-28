// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

import { commands, tasks, workspace } from "vscode";
import { BabbelaarContext } from "./babbelaarContext";
import { BabbelaarLsp } from "./babbelaarLsp";
import { BabbelaarLog } from "./logger";
import { dirname } from "path";

const BabbelaarCommands = {
    register: async (context: BabbelaarContext) => {
        BabbelaarLog.info(`Commando's worden geregistreerd...`);

        context.ext.subscriptions.push(
            commands.registerCommand("babbelaar.herstarten", async () => {
                await BabbelaarLsp.startClient(context);
            })
        );

        context.ext.subscriptions.push(
            commands.registerCommand("babbelaar.uitvoeren", async uri => {
                let path = workspace.textDocuments[0].uri.fsPath;
                path = dirname(path);
                tasks.executeTask(await context.taskProvider!.createRun(path));
            })
        );
    }
};

export { BabbelaarCommands };
