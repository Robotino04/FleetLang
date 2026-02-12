/* --------------------------------------------------------------------------------------------
 * Copyright (c) 2024 TypeFox and others.
 * Licensed under the MIT License. See LICENSE in the package root for license information.
 * ------------------------------------------------------------------------------------------ */

import { ComChannelEndpoint, type ComRouter, RawPayload, WorkerMessage } from 'wtd-core';
import fleetWorkerUrl from '../worker/fleet-server?worker&url';

/**
 * Minimal interaction main thread, just forwards LSP messages.
 */
class FleetInteractionMain implements ComRouter {
    setComChannelEndpoint(_comChannelEndpoint: ComChannelEndpoint): void {
        // No-op for now
    }

    // Optional hooks for progress / errors
    fleetls_progress(_message: WorkerMessage) {}
    fleetls_error(_message: WorkerMessage) {}
}

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
                'fleetls_init' // keep old command name for drop-in compatibility
            ),
            transferables: [config.lsMessagePort],
            awaitAnswer: true,
            expectedAnswer: 'fleetls_init_complete'
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
                'fleetls_launch' // still matches the old contract
            ),
            awaitAnswer: true,
            expectedAnswer: 'fleetls_launch_complete'
        });
    }
}

