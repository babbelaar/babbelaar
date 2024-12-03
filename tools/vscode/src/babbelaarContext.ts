import { ExtensionContext } from "vscode";
import { BabbelaarTaskProvider } from "./tasks";

export interface BabbelaarContext {
    ext: ExtensionContext,
    version: string,

    taskProvider: BabbelaarTaskProvider|null,
};
