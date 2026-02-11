/* --------------------------------------------------------------------------------------------
 * Copyright (c) 2024 TypeFox and others.
 * Licensed under the MIT License. See LICENSE in the package root for license information.
 * ------------------------------------------------------------------------------------------ */

/// <reference lib="WebWorker" />

import {
  BrowserMessageReader,
  BrowserMessageWriter,
} from "vscode-languageserver/browser.js";
import {
  ComChannelEndpoint,
  type ComRouter,
  RawPayload,
  WorkerMessage,
} from "wtd-core";

import initFleet, { serve, ServerConfig, InitOutput } from "../../../resources/fleet/fleetls_wasm";
import wasmUrl from "../../../resources/fleet/fleetls_wasm_bg.wasm?url";
import { JsonStream } from "./json_stream";

declare const self: DedicatedWorkerGlobalScope;


// -------------------------
// Minimal async iterator for LSP -> Fleet
// -------------------------
class IntoServer implements AsyncIterator<Uint8Array> {
  private queue: Uint8Array[] = [];
  private resolvers: ((value: IteratorResult<Uint8Array>) => void)[] = [];

  send(msg: Uint8Array) {
    if (this.resolvers.length) {
      const resolve = this.resolvers.shift()!;
      resolve({ value: msg, done: false });
    } else {
      this.queue.push(msg);
    }
  }

  next(): Promise<IteratorResult<Uint8Array>> {
    if (this.queue.length) {
      return Promise.resolve({ value: this.queue.shift()!, done: false });
    }
    return new Promise((resolve) => this.resolvers.push(resolve));
  }
}

// -------------------------
// WritableStream for Fleet -> LSP
// -------------------------
class FromServer extends WritableStream<Uint8Array> {
  private jsonStream = new JsonStream();

  constructor(private writer: BrowserMessageWriter) {
    super({
      write: (chunk) => {
        for (const byte of chunk) {
          const jsonStr = this.jsonStream.insert(byte);
          if (jsonStr) this.writer.write(JSON.parse(jsonStr));
        }
      },
    });
  }
}

// -------------------------
// Worker implementing ComRouter
// -------------------------
export class FleetInteractionWorker implements ComRouter {
  private endpointWorker?: ComChannelEndpoint;

  private reader?: BrowserMessageReader;
  private writer?: BrowserMessageWriter;
  private lsMessagePort?: MessagePort;

  private fleetInit?: InitOutput;
  private intoServer?: IntoServer;
  private fromServer?: FromServer;

  setComChannelEndpoint(endpoint: ComChannelEndpoint): void {
    this.endpointWorker = endpoint;
  }

  // -------------------------
  // INIT
  // -------------------------
  async clangd_init(message: WorkerMessage) {
    const rawPayload = (message.payloads![0] as RawPayload).message.raw;
    this.lsMessagePort = rawPayload.lsMessagePort as MessagePort;

    this.reader = new BrowserMessageReader(this.lsMessagePort);
    this.writer = new BrowserMessageWriter(this.lsMessagePort);

    this.endpointWorker?.sentAnswer({
      message: WorkerMessage.createFromExisting(message, {
        overrideCmd: "clangd_init_complete",
      }),
    });
  }

  // -------------------------
  // LAUNCH
  // -------------------------
  async clangd_launch(message: WorkerMessage) {
    await this.startFleet();

    this.endpointWorker?.sentAnswer({
      message: WorkerMessage.createFromExisting(message, {
        overrideCmd: "clangd_launch_complete",
      }),
    });
  }

  // -------------------------
  // START FLEET SERVER
  // -------------------------
  private async startFleet() {
    if (!this.reader || !this.writer) throw new Error("Message ports not initialized");

    // 1. Transport objects
    this.intoServer = new IntoServer();
    this.fromServer = new FromServer(this.writer);

    // 2. Forward LSP client → Fleet
    this.reader.listen((msg) => {
      const json = JSON.stringify(msg);
      const payload = `Content-Length: ${json.length}\r\n\r\n${json}`;
      this.intoServer!.send(new TextEncoder().encode(payload));
    });

    // 3. Load Fleet WASM
    const wasmBytes = await (await fetch(wasmUrl)).arrayBuffer();
    this.fleetInit = await initFleet({ module_or_path: wasmBytes });

    // 4. Start Fleet server
    const config = new ServerConfig(this.intoServer, this.fromServer);
    serve(config).catch((err) => console.error("Fleet server stopped:", err));
  }
}

// -------------------------
// Worker bootstrap
// -------------------------
new ComChannelEndpoint({
  endpointId: 2000,
  endpointConfig: { $type: "DirectImplConfig", impl: self },
  verbose: true,
  endpointName: "fleet_main",
}).connect(new FleetInteractionWorker());
