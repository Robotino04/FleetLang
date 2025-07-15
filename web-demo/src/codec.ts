import * as jsrpc from "vscode-jsonrpc";
import * as vsrpc from "vscode-languageserver-protocol";
import Bytes from "./codec/bytes";
import StreamDemuxer from "./codec/demuxer";
import Queue from "./codec/queue";

export const encoder = new TextEncoder();
export const decoder = new TextDecoder();

export class Codec {
  static encode(
    json: jsrpc.RequestMessage | jsrpc.ResponseMessage,
  ): Uint8Array {
    const message = JSON.stringify(json);
    const delimited = `Content-Length: ${message.length}\r\n\r\n${message}`;
    return Bytes.encode(delimited);
  }
}

export class IntoServer
  extends Queue<Uint8Array>
  implements AsyncGenerator<Uint8Array, never, void>
{
  send(item: jsrpc.RequestMessage | jsrpc.ResponseMessage): void {
    console.debug("[TO LSP]", item);
    super.enqueue(Codec.encode(item));
  }
}

export interface FromServer extends WritableStream<Uint8Array> {
  readonly responses: {
    get(key: number | string): null | Promise<vsrpc.ResponseMessage>;
  };
  readonly notifications: AsyncGenerator<
    vsrpc.NotificationMessage,
    never,
    void
  >;
  readonly requests: AsyncGenerator<vsrpc.RequestMessage, never, void>;
}

export namespace FromServer {
  export function create(): FromServer {
    return new StreamDemuxer();
  }
}
