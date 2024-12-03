// Copyright (C) 2024 Tristan Gerritsen <tristan@thewoosh.org>
// All Rights Reserved.

import { File } from "buffer";
import { copyFile, copyFileSync, existsSync, realpathSync } from "fs";
import { chmod, mkdir, writeFile } from "fs/promises";
import { window } from "vscode";
import { unzip } from "zlib";
import { BabbelaarContext } from "./babbelaarContext";
import { BabbelaarLog } from "./logger";

export async function ensureCli(context: BabbelaarContext): Promise<string|null> {
    BabbelaarLog.info(`Babbelaar-hoofdprogramma wordt gezocht...`);
    const path = await ensureCliPath(context);

    if (!path)
        return path;

    if (process.platform === 'win32')
        return copyTempPath(path);

    return path;
}

export async function ensureLspServer(context: BabbelaarContext): Promise<string|null> {
    BabbelaarLog.info(`Babbelaar-taaldienaar wordt gezocht...`);
    const path = await ensureLspServerPath(context);

    if (!path)
        return path;

    if (process.platform === 'win32')
        return copyTempPath(path);

    return path;
}

async function ensureLspServerPath(context: BabbelaarContext): Promise<string|null> {
    const envPath = process.env.BABBELAAR_LSP_SERVER_PATH;
    if (envPath && existsSync(envPath)) {
        const path = realpathSync(envPath);
        BabbelaarLog.info(`Babbelaar-taaldienaar gebruikt omgevingspad: '${path}'`);
        return path;
    }

    const installPath = ensureLspServerUsingDownload(context);
    if (!installPath) {
        showError(context, "Kon Babbelaar niet vinden op het systeem, maar kon het ook niet downloaden.");
    }

    return installPath;
}

async function ensureCliPath(context: BabbelaarContext): Promise<string|null> {
    const envPath = process.env.BABBELAAR_CLI_PATH;
    if (envPath && existsSync(envPath)) {
        const path = realpathSync(envPath);
        BabbelaarLog.info(`Babbelaar-hoofdprogramma gebruikt omgevingspad: '${path}'`);
        return path;
    }

    const installPath = ensureCliUsingDownload(context);
    if (!installPath) {
        showError(context, "Kon Babbelaar niet vinden op het systeem, maar kon het ook niet downloaden.");
    }

    return installPath;
}

async function ensureLspServerUsingDownload(context: BabbelaarContext): Promise<string|null> {
    const dir = context.ext.globalStorageUri.fsPath + "/" + context.version;
    if (!existsSync(dir)) {
        await mkdir(dir, { recursive: true });
    }

    const executableName = detectBabbelaarFileName(context, "babbelaar-lsp");
	if (!executableName)
        return null;

    let path = dir + "/" + executableName.file;
    if (existsSync(path)) {
        BabbelaarLog.info(`Babbelaar-taaldienaar is gevonden op '${path}'`);
        return await ensureExecutable(path);
    }

    window.showInformationMessage("We downloaden Babbelaar...");

    const babbelaar = await downloadProgram(context, executableName);
    if (!babbelaar)
        return null;

    await writeFile(path, Buffer.from(babbelaar.arrayBuffer));
    path = await ensureExecutable(path);

    BabbelaarLog.info("Babbelaar-taaldienaar is gedownload in: " + path);
    window.showInformationMessage("Babbelaar is gedownload");
    return path;
}

async function ensureCliUsingDownload(context: BabbelaarContext): Promise<string|null> {
    const dir = context.ext.globalStorageUri.fsPath + "/" + context.version;
    if (!existsSync(dir)) {
        await mkdir(dir, { recursive: true });
    }

    const executableName = detectBabbelaarFileName(context, "babbelaar-interpreter");
	if (!executableName)
        return null;

    let path = dir + "/" + executableName.file;
    if (existsSync(path)) {
        BabbelaarLog.info(`Babbelaar-hoofdprogramma is gevonden op '${path}'`);
        return await ensureExecutable(path);
    }

    window.showInformationMessage("We downloaden Babbelaar...");

    const babbelaar = await downloadProgram(context, executableName);
    if (!babbelaar)
        return null;

    await writeFile(path, Buffer.from(babbelaar.arrayBuffer));
    path = await ensureExecutable(path);

    BabbelaarLog.info("Babbelaar-hoofdprogramma is gedownload in: " + path);
    window.showInformationMessage("Babbelaar-hoofdprogramma is gedownload");
    return path;
}

async function downloadProgram(ctx: BabbelaarContext, executableName: ExecutableName): Promise<BabbelaarDownload|undefined> {
	const url: string = "https://babbelaar.dev/blob/" + ctx.version + "/" + executableName.dir + "/" + executableName.file;

    BabbelaarLog.info(`${executableName.file} wordt gedownload vanaf ${url}`);
	const response = await fetch(url);

    if (!response.ok) {
        showError(ctx, `"${response.status} ${response.statusText}"`);
        return;
    }

    const contentType = response.headers.get("Content-Type");
    if (contentType != null && contentType !== "application/octet-stream") {
        showError(ctx, `onverwacht inhoudstype: ${contentType}`)
        return;
    }

    const blob = await response.blob();
    const arrayBuffer = await blob.arrayBuffer();

    return {
        arrayBuffer,
    };
}

function detectBabbelaarFileName(ctx: BabbelaarContext, program: string): ExecutableName|null {
    const messageOptions = {
        noRetry: true,
    };

	if (process.platform === "darwin") {
		if (process.arch !== "arm64") {
			showError(ctx, "Alleen macOS met M1+ processors zijn ondersteund. Intel/PowerPC wordt op het moment niet ondersteund.", messageOptions);
			return null;
		}

		return {
            dir: "macos",
            file: program,
        };
	}

	if (process.platform == "linux") {
		if (process.arch !== "x64") {
			showError(ctx, "Alleen Linux met x86-64 processors zijn ondersteund. ARM64 of anders wordt niet ondersteund.", messageOptions);
			return null;
		}

		return {
            dir: "linux",
            file: program,
        };
	}

	if (process.platform == "win32") {
		if (process.arch !== "x64") {
			showError(ctx, "Alleen Windows met x86-64 processors zijn ondersteund. ARM64 of anders wordt niet ondersteund.", messageOptions);
			return null;
		}

		return {
            dir: "windows",
            file: program + ".exe",
        };
	}

	showError(ctx, "Dit besturingssysteem wordt op het moment niet ondersteund door Babbelaar.", messageOptions);
	return null;
}

interface ExecutableName {
    dir: string,
    file: string,
}

interface BabbelaarDownload {
    arrayBuffer: ArrayBuffer,
}

interface MessageOptions {
    noRetry?: boolean,
}

function showError(context: BabbelaarContext, message: string, messageOptions: MessageOptions | null = null) {
    const msg = `Kon Babbelaar niet downloaden: ${message}`;

    if (messageOptions?.noRetry) {
        window.showErrorMessage(msg, "Oké");
        return;
    }

    window.showErrorMessage(msg, "Opnieuw proberen", "Stoppen")
        .then(async (s) => {
            if (s === "Opnieuw proberen") {
                await ensureLspServer(context);
            }
        });
}

async function ensureExecutable(path: string): Promise<string> {
    await chmod(path, "700");
    return path;
}

/// In order to be able to debug on Windows, we must copy the executable to a
/// temporary path, since we otherwise won't be able to both be executing the
/// LSP server, and overwriting it using "cargo watch -x 'build --all --all-targets'"
async function copyTempPath(path: string): Promise<string | null>{
    const tempPath = path.replace(".exe", ".temp.exe");
    copyFileSync(path, tempPath);
    return tempPath;
}
