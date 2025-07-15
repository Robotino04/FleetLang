import { encoder, decoder } from "../codec";

export default class Bytes {
  static encode(input: string): Uint8Array {
    return encoder.encode(input);
  }

  static decode(input: Uint8Array): string {
    return decoder.decode(input);
  }

  static append(...arrays: Uint8Array[]): Uint8Array<ArrayBuffer> {
    let totalLength = 0;
    for (const arr of arrays) {
      totalLength += arr.length;
    }
    const result = new Uint8Array(totalLength);
    let offset = 0;
    for (const arr of arrays) {
      result.set(arr, offset);
      offset += arr.length;
    }
    return result;
  }
}
