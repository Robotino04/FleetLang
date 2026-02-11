/* --------------------------------------------------------------------------------------------
 * Copyright (c) 2024 TypeFox and others.
 * Licensed under the MIT License. See LICENSE in the package root for license information.
 * ------------------------------------------------------------------------------------------ */

import { RegisteredFileSystemProvider, RegisteredMemoryFile, registerFileSystemOverlay } from '@codingame/monaco-vscode-files-service-override';
import * as vscode from 'vscode';
import '@codingame/monaco-vscode-cpp-default-extension'; // syntax highlighting
import { LanguageClientWrapper } from 'monaco-languageclient/lcwrapper';
import { MonacoVscodeApiWrapper } from 'monaco-languageclient/vscodeApiWrapper';
import { createDefaultWorkspaceContent, disableElement } from '../../common/client/utils.js';
import { HOME_DIR, WORKSPACE_PATH } from '../definitions.js';
import { createFleetAppConfig } from './config.js';
import { FleetWorkerHandler } from './workerHandler.js';

export const runFleetWrapper = async () => {
    // 1. Create the LSP message channel
    const channelLs = new MessageChannel();

    // 2. Register a minimal in-memory Monaco workspace
    const fileSystemProvider = new RegisteredFileSystemProvider(false);
    const workspaceFileUri = vscode.Uri.file(`${HOME_DIR}/workspace.code-workspace`);
    const defaultContent = createDefaultWorkspaceContent(WORKSPACE_PATH);
    fileSystemProvider.registerFile(new RegisteredMemoryFile(workspaceFileUri, defaultContent));

    
    fileSystemProvider.registerFile(
        new RegisteredMemoryFile(vscode.Uri.file(`${WORKSPACE_PATH}/main.fl`),
        "let main = () -> i32 {\n    return 0;\n}\n")
    );

    registerFileSystemOverlay(1, fileSystemProvider);

    // 3. Prepare Fleet worker
    const fleetWorkerHandler = new FleetWorkerHandler();

    // 4. Create Fleet app config
    const appConfig = await createFleetAppConfig({
        htmlContainer: document.getElementById("monaco-container")!,
        workspaceUri: vscode.Uri.file(WORKSPACE_PATH),
        workspaceFileUri,
        fleetWorkerHandler,
        lsMessageLocalPort: channelLs.port1
    });

    // 5. Initialize Monaco / VSC API wrapper
    const apiWrapper = new MonacoVscodeApiWrapper(appConfig.vscodeApiConfig);
    await apiWrapper.start();

    // 6. Initialize the language client wrapper
    const lcWrapper = new LanguageClientWrapper(appConfig.languageClientConfig);

    // 7. Define the Fleet worker init + launch
    const startWrapper = async () => {
        // Only pass the LSP port; Fleet ignores FS channels
        await fleetWorkerHandler.init({ lsMessagePort: channelLs.port2 });
        await fleetWorkerHandler.launch();
        await lcWrapper.start();
    };

    // 8. Attach UI buttons
    try {
        document.querySelector('#button-start')?.addEventListener('click', async () => {
            disableElement('button-start', true);
            disableElement('button-start-fresh', true);
            await startWrapper();
        });

        document.querySelector('#button-start-fresh')?.addEventListener('click', async () => {
            disableElement('button-start', true);
            disableElement('button-start-fresh', true);
            await startWrapper();
        });
    } catch (e) {
        console.error(e);
    }
};
