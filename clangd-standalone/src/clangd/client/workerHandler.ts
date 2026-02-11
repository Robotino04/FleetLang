/* --------------------------------------------------------------------------------------------
 * Copyright (c) 2024 TypeFox and others.
 * Licensed under the MIT License. See LICENSE in the package root for license information.
 * ------------------------------------------------------------------------------------------ */

import { ComChannelEndpoint, type ComRouter, RawPayload, WorkerMessage } from 'wtd-core';
import fleetWorkerUrl from '../worker/clangd-server?worker&url';

/**
 * Minimal interaction main thread, just forwards LSP messages.
 */
class FleetInteractionMain implements ComRouter {
    setComChannelEndpoint(_comChannelEndpoint: ComChannelEndpoint): void {
        // No-op for now
    }

    // Optional hooks for progress / errors
    clangd_progress(_message: WorkerMessage) {}
    clangd_error(_message: WorkerMessage) {}
}

/**
 * Fleet worker handler (drop-in replacement for ClangdWorkerHandler)
 */
export class FleetWorkerHandler {
    private interactionMain: FleetInteractionMain = new FleetInteractionMain();
    private endpointMain?: ComChannelEndpoint;
    private worker?: Worker;

    /**
     * Creates and returns a Fleet worker
     */
    async createWorker() {
        if (!this.worker) {
            this.worker = new Worker(fleetWorkerUrl, {
                type: 'module',
                name: 'Fleet Server Worker',
            });

            this.endpointMain = new ComChannelEndpoint({
                endpointId: 1,
                endpointConfig: {
                    $type: 'DirectImplConfig',
                    impl: this.worker
                },
                verbose: true,
                endpointName: 'main_worker'
            });

            this.endpointMain.connect(this.interactionMain);
        }

        return this.worker;
    }

    /**
     * Initialize the Fleet worker
     */
    async init(config: { lsMessagePort: MessagePort }) {
        if (!this.endpointMain) throw new Error("Worker endpoint not created");

        await this.endpointMain.sentMessage({
            message: WorkerMessage.fromPayload(
                new RawPayload({
                    lsMessagePort: config.lsMessagePort
                }),
                'clangd_init' // keep old command name for drop-in compatibility
            ),
            transferables: [config.lsMessagePort],
            awaitAnswer: true,
            expectedAnswer: 'clangd_init_complete'
        });
    }

    /**
     * Launch FleetLS server
     */
    async launch() {
        if (!this.endpointMain) throw new Error("Worker endpoint not created");

        await this.endpointMain.sentMessage({
            message: WorkerMessage.fromPayload(
                new RawPayload({}),
                'clangd_launch' // still matches the old contract
            ),
            awaitAnswer: true,
            expectedAnswer: 'clangd_launch_complete'
        });
    }
}

