import init, {
  InitOutput,
  serve,
  ServerConfig,
} from "@assets/wasm/fleetls_wasm";
import wasmUrl from "@assets/wasm/fleetls_wasm_bg.wasm?url";

import { FromServer, IntoServer } from "./codec";

let server: null | Server;

async function fetchWasmWithProgress(
  url: string,
  progressCallback: (
    percentage: number,
    bytes_done: number,
    bytes_total: number,
  ) => void,
): Promise<Uint8Array> {
  const response = await fetch(url);
  const contentLength = +response.headers.get("Content-Length")!;

  if (!response.body) {
    throw new Error("ReadableStream not supported in this browser.");
  }

  const reader = response.body.getReader();
  const chunks: Uint8Array[] = [];
  let receivedLength = 0;

  while (true) {
    const { done, value } = await reader.read();
    if (done) break;
    if (value) {
      chunks.push(value);
      receivedLength += value.length;

      const percent = receivedLength / contentLength;
      progressCallback(percent, receivedLength, contentLength);
    }
  }

  // Merge chunks into a single Uint8Array
  const wasmBytes = new Uint8Array(receivedLength);
  let position = 0;
  for (const chunk of chunks) {
    wasmBytes.set(chunk, position);
    position += chunk.length;
  }

  return wasmBytes;
}

export default class Server {
  readonly initOutput: InitOutput;
  readonly #intoServer: IntoServer;
  readonly #fromServer: FromServer;

  private constructor(
    initOutput: InitOutput,
    intoServer: IntoServer,
    fromServer: FromServer,
  ) {
    this.initOutput = initOutput;
    this.#intoServer = intoServer;
    this.#fromServer = fromServer;
  }

  static async initialize(
    intoServer: IntoServer,
    fromServer: FromServer,
    progressCallback: (
      percentage: number,
      bytes_done: number,
      bytes_total: number,
    ) => void,
  ): Promise<Server> {
    if (null == server) {
      console.log("Fetching WASM with progress...");
      const wasmBytes = await fetchWasmWithProgress(wasmUrl, progressCallback);
      const initOutput = await init({ module_or_path: wasmBytes });
      server = new Server(initOutput, intoServer, fromServer);
    } else {
      console.warn("Server already initialized; ignoring");
    }
    return server;
  }

  async start(): Promise<void> {
    const config = new ServerConfig(this.#intoServer, this.#fromServer);
    await serve(config);
    console.error("FleetLS has stopped");
  }
}
