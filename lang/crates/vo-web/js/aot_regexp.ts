import type { AotExternCall, AotExternDescriptor } from './index.js';

const MAX_RESULTS = 1_000_000;
const RESPONSE_HEADER_BYTES = 36;
const OPERATIONS = new Map<string, number>([
  ['matchString', 0],
  ['matchBytes', 1],
  ['findString', 2],
  ['findStringIndex', 3],
  ['findAllString', 4],
  ['replaceAllString', 5],
  ['replaceAllLiteralString', 6],
  ['splitString', 7],
  ['findStringSubmatch', 8],
  ['findAllStringIndexFlat', 9],
  ['findAllStringSubmatchFlat', 10],
  ['findAllStringSubmatchIndexFlat', 11],
  ['subexpNames', 12],
  ['findBytesSubmatchIndex', 13],
  ['findAllBytesIndexFlat', 14],
  ['replaceAllBytes', 15],
  ['replaceAllLiteralBytes', 16],
  ['quoteMeta', 17],
]);

interface RegexpSupportModule {
  default(input?: unknown): Promise<unknown>;
  initSync(input: unknown): unknown;
  voAotRegexp(
    operation: number,
    pattern: Uint8Array,
    input: Uint8Array,
    replacement: Uint8Array,
    n: bigint,
  ): Uint8Array;
}

interface RegexpResponse {
  readonly valid: boolean;
  readonly scalar0: bigint;
  readonly scalar1: bigint;
  readonly bytes: readonly Uint8Array[];
  readonly integers: readonly bigint[];
}

let loadedSupport: RegexpSupportModule | undefined;
let loadingSupport: Promise<RegexpSupportModule> | undefined;

function canonicalExternName(functionName: string): string {
  return `vo1:6:regexp:${new TextEncoder().encode(functionName).byteLength}:${functionName}`;
}

function operation(descriptor: AotExternDescriptor): number | undefined {
  if (descriptor.source !== 1) return undefined;
  for (const [name, value] of OPERATIONS) {
    if (descriptor.name === canonicalExternName(name)) return value;
  }
  return undefined;
}

async function loadSupport(
  source?: BufferSource | WebAssembly.Module,
): Promise<RegexpSupportModule> {
  if (loadedSupport !== undefined) return loadedSupport;
  if (loadingSupport !== undefined) return loadingSupport;
  loadingSupport = (async () => {
    const support = (await import('../aot-support/vo_aot_support_wasm.js')) as unknown as RegexpSupportModule;
    if (source === undefined) {
      try {
        await support.default();
      } catch (error) {
        const wrapped = new Error(
          'Volang AOT regexp support could not load automatically; pass supportModule in non-browser hosts',
        );
        (wrapped as Error & { cause?: unknown }).cause = error;
        throw wrapped;
      }
    } else {
      support.initSync({ module: source });
    }
    loadedSupport = support;
    return support;
  })();
  try {
    return await loadingSupport;
  } finally {
    loadingSupport = undefined;
  }
}

function decodeResponse(encoded: Uint8Array): RegexpResponse {
  if (encoded.byteLength < RESPONSE_HEADER_BYTES) {
    throw new Error('truncated Volang AOT regexp response');
  }
  const magic = new TextDecoder('ascii', { fatal: true }).decode(encoded.subarray(0, 8));
  if (magic !== 'VOREG001') throw new Error('invalid Volang AOT regexp response magic');
  const view = new DataView(encoded.buffer, encoded.byteOffset, encoded.byteLength);
  const flags = view.getUint32(8, true);
  const byteCount = view.getUint32(28, true);
  const integerCount = view.getUint32(32, true);
  if ((flags & ~1) !== 0 || byteCount > MAX_RESULTS || integerCount > MAX_RESULTS) {
    throw new Error('invalid Volang AOT regexp response header');
  }
  let offset = 36;
  const bytes: Uint8Array[] = [];
  for (let index = 0; index < byteCount; index += 1) {
    if (offset + 4 > encoded.byteLength) throw new Error('truncated regexp byte result');
    const length = view.getUint32(offset, true);
    offset += 4;
    if (offset + length > encoded.byteLength) throw new Error('truncated regexp byte result');
    bytes.push(encoded.slice(offset, offset + length));
    offset += length;
  }
  if (offset + integerCount * 8 !== encoded.byteLength) {
    throw new Error('invalid Volang AOT regexp response length');
  }
  const integers: bigint[] = [];
  for (let index = 0; index < integerCount; index += 1) {
    integers.push(view.getBigInt64(offset, true));
    offset += 8;
  }
  return {
    valid: (flags & 1) !== 0,
    scalar0: view.getBigInt64(12, true),
    scalar1: view.getBigInt64(20, true),
    bytes,
    integers,
  };
}

export class AotRegexpHost {
  private support?: RegexpSupportModule;

  static supportsDescriptor(descriptor: AotExternDescriptor): boolean {
    return operation(descriptor) !== undefined;
  }

  async initialize(
    descriptors: readonly AotExternDescriptor[],
    source?: BufferSource | WebAssembly.Module,
  ): Promise<void> {
    if (descriptors.some(AotRegexpHost.supportsDescriptor)) {
      this.support = await loadSupport(source);
    }
  }

  supports(descriptor: AotExternDescriptor): boolean {
    return AotRegexpHost.supportsDescriptor(descriptor);
  }

  handle(call: AotExternCall): void {
    const opcode = operation(call.descriptor);
    if (opcode === undefined || this.support === undefined) {
      throw new Error(`unsupported AOT regexp extern ${call.name}`);
    }
    const arg = (index: number) => call.readSlot(call.argumentsStart + index);
    let pattern: Uint8Array = new Uint8Array();
    let input: Uint8Array = new Uint8Array();
    let replacement: Uint8Array = new Uint8Array();
    let n = -1n;
    if (opcode === 17) input = call.readStringBytes(arg(0));
    else {
      pattern = call.readStringBytes(arg(0));
      const byteInput = [1, 13, 14, 15, 16].includes(opcode);
      input = byteInput ? call.readByteSlice(arg(1)) : call.readStringBytes(arg(1));
      if ([5, 6].includes(opcode)) replacement = call.readStringBytes(arg(2));
      else if ([15, 16].includes(opcode)) replacement = call.readByteSlice(arg(2));
      if ([4, 7, 9, 10, 11, 14].includes(opcode)) n = BigInt.asIntN(64, arg(2));
    }
    const response = decodeResponse(this.support.voAotRegexp(
      opcode,
      pattern,
      input,
      replacement,
      n,
    ));
    const write = (offset: number, value: bigint) => call.writeSlot(call.destination + offset, value);
    const optionalStrings = (): bigint => response.bytes.length === 0
      ? 0n : call.allocateStringBytesSlice(response.bytes);
    if (opcode === 0 || opcode === 1) {
      write(0, response.scalar0 === 0n ? 0n : 1n);
      write(1, response.valid ? 1n : 0n);
    } else if (opcode === 2 || opcode === 5 || opcode === 6 || opcode === 17) {
      write(0, call.allocateStringBytes(response.bytes[0] ?? new Uint8Array()));
    } else if (opcode === 3) {
      write(0, response.scalar0);
      write(1, response.scalar1);
    } else if ([4, 8, 12].includes(opcode)) {
      write(0, optionalStrings());
    } else if (opcode === 7) {
      write(0, n === 0n ? 0n : call.allocateStringBytesSlice(response.bytes));
    } else if (opcode === 9 || opcode === 13 || opcode === 14) {
      write(0, call.allocateIntSlice(response.integers));
    } else if (opcode === 10) {
      write(0, optionalStrings());
      write(1, response.scalar0);
    } else if (opcode === 11) {
      write(0, call.allocateIntSlice(response.integers));
      write(1, response.scalar0);
    } else if (opcode === 15 || opcode === 16) {
      const value = response.bytes[0] ?? new Uint8Array();
      write(0, value.byteLength === 0 ? 0n : call.allocateByteSlice(value));
    }
  }
}
