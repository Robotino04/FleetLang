/* --------------------------------------------------------------------------------------------
 * Copyright (c) 2024 TypeFox and others.
 * Licensed under the MIT License. See LICENSE in the package root for license information.
 * ------------------------------------------------------------------------------------------ */

import { LogLevel } from '@codingame/monaco-vscode-api';
import getEnvironmentServiceOverride from '@codingame/monaco-vscode-environment-service-override';
import getExplorerServiceOverride from '@codingame/monaco-vscode-explorer-service-override';
import getKeybindingsServiceOverride from '@codingame/monaco-vscode-keybindings-service-override';
import getLifecycleServiceOverride from '@codingame/monaco-vscode-lifecycle-service-override';
import getRemoteAgentServiceOverride from '@codingame/monaco-vscode-remote-agent-service-override';
import getSecretStorageServiceOverride from '@codingame/monaco-vscode-secret-storage-service-override';
import getBannerServiceOverride from '@codingame/monaco-vscode-view-banner-service-override';
import getStatusBarServiceOverride from '@codingame/monaco-vscode-view-status-bar-service-override';
import getTitleBarServiceOverride from '@codingame/monaco-vscode-view-title-bar-service-override';
import getViewsServiceOverride from "@codingame/monaco-vscode-views-service-override";
import type { EditorAppConfig } from 'monaco-languageclient/editorApp';
import type { LanguageClientConfig } from 'monaco-languageclient/lcwrapper';
import { defaultHtmlAugmentationInstructions, defaultViewsInit, type MonacoVscodeApiConfig } from 'monaco-languageclient/vscodeApiWrapper';
import { useWorkerFactory, Worker, WorkerLoader } from 'monaco-languageclient/workerFactory';
import { Uri } from 'vscode';
import { FleetWorkerHandler } from './workerHandler.js';
import statemachineLanguageConfig from "./language-configuration.json?raw"

import EditorWorkerUrl from '@codingame/monaco-vscode-editor-api/esm/vs/editor/editor.worker?worker&url';
import ExtensionHostWorkerUrl from '@codingame/monaco-vscode-api/workers/extensionHost.worker?worker&url';
import TextMateWorkerUrl from '@codingame/monaco-vscode-textmate-service-override/worker?worker&url';
import { HoverMiddleware } from 'vscode-languageclient';


export type FleetAppConfig = {
    languageClientConfig: LanguageClientConfig;
    vscodeApiConfig: MonacoVscodeApiConfig;
    editorAppConfig: EditorAppConfig;
}

export const createFleetAppConfig = async (config: {
    htmlContainer: HTMLElement,
    workspaceUri: Uri,
    workspaceFileUri: Uri,
    fleetWorkerHandler: FleetWorkerHandler,
    lsMessageLocalPort: MessagePort
}): Promise<FleetAppConfig> => {
    const extensionFilesOrContents = new Map<string, string | URL>();
    extensionFilesOrContents.set(`/language-statemachine-configuration.json`, statemachineLanguageConfig);
    //extensionFilesOrContents.set(`/language-statemachine-grammar.json`, responseStatemachineTm);

    const vscodeApiConfig: MonacoVscodeApiConfig = {
        $type: 'extended',
        logLevel: LogLevel.Debug,
        serviceOverrides: {
            ...getKeybindingsServiceOverride(),
            ...getLifecycleServiceOverride(),
            ...getBannerServiceOverride(),
            ...getStatusBarServiceOverride(),
            ...getTitleBarServiceOverride(),
            ...getExplorerServiceOverride(),
            ...getRemoteAgentServiceOverride(),
            ...getEnvironmentServiceOverride(),
            ...getSecretStorageServiceOverride(),
            ...getViewsServiceOverride(),
        },
        viewsConfig: {
            $type: 'ViewsService',
            htmlContainer: config.htmlContainer,
            htmlAugmentationInstructions: defaultHtmlAugmentationInstructions,
            viewsInitFunc: defaultViewsInit
        },
        workspaceConfig: {
            enableWorkspaceTrust: true,
            windowIndicator: {
                label: 'mlc-fleet-example',
                tooltip: '',
                command: ''
            },
            workspaceProvider: {
                trusted: true,
                async open() {
                    window.open(window.location.href);
                    return true;
                },
                workspace: {
                    workspaceUri: config.workspaceFileUri
                },
            },
            configurationDefaults: {
                'window.title': 'mlc-fleet-example${separator}${dirty}${activeEditorShort}'
            },
            productConfiguration: {
                nameShort: 'mlc-fleet-example',
                nameLong: 'mlc-fleet-example'
            }
        },
        userConfiguration: {
            json: JSON.stringify({
                'workbench.colorTheme': 'Default Dark Modern',
                'editor.wordBasedSuggestions': 'off',
                'editor.guides.bracketPairsHorizontal': true,
                'editor.inlayHints.enabled': 'offUnlessPressed',
                'editor.quickSuggestionsDelay': 200,
                'editor.experimental.asyncTokenization': false
            })
        },
        extensions: [{
            config: {
                name: 'mlc-fleet-example',
                publisher: 'TypeFox',
                version: '1.0.0',
                engines: { vscode: '*' },
                contributes: {
                    languages: [{
                        id: 'fleet',
                        extensions: ['.fl', ".fleet"],
                        aliases: ['Fleet'],
                        configuration: `./language-statemachine-configuration.json`
                    }],
                    /*
                    grammars: [{
                        language: 'fleet',
                        scopeName: 'source.statemachine',
                        path: `./language-statemachine-grammar.json`
                    }]
                    */
                },
            },
            filesOrContents: extensionFilesOrContents
        }],
        monacoWorkerFactory: () => useWorkerFactory({
            workerLoaders: {
                editorWorkerService: () => new Worker(EditorWorkerUrl, { type: 'module' }),
                extensionHostWorkerMain: () => new Worker(ExtensionHostWorkerUrl, { type: 'module' }),
                TextMateWorker: () => new Worker(TextMateWorkerUrl, { type: 'module' }),
                OutputLinkDetectionWorker: undefined,
                LanguageDetectionWorker: undefined,
                NotebookEditorWorker: undefined,
                LocalFileSearchWorker: undefined
            }
        })
    };

    const languageClientConfig: LanguageClientConfig = {
        languageId: 'fleet',
        connection: {
            options: {
                $type: 'WorkerDirect',
                worker: await config.fleetWorkerHandler.createWorker(),
                messagePort: config.lsMessageLocalPort
            }
        },
        restartOptions: {
            retries: 5,
            timeout: 1000,
            keepWorker: true
        },
        clientOptions: {
            documentSelector: [{
                language: "fleet",
            }],
            markdown: {
                isTrusted: true,
                supportHtml: true,
            },
            workspaceFolder: {
                index: 0,
                name: 'workspace',
                uri: config.workspaceUri
            }
        }
    };

    const editorAppConfig: EditorAppConfig = {};

    return {
        vscodeApiConfig,
        languageClientConfig,
        editorAppConfig
    };
};
