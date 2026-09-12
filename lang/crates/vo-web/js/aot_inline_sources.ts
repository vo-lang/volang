/** Optional immutable source ancestry, sharing the common VOB DAG contract. */
import { AOT_INLINE_SOURCE_SECTION, MAX_EXTERN_COUNT } from './aot_abi.js';
import type { AotDebugMetadata } from './aot_metadata.js';
import { AotSourceIndex } from './aot_source_index.js';

const MAX_RECORDS = 131_072;
const MAX_DEPTH = 32;
const NO_PARENT = 0xffff_ffff;

export interface AotSourceSpan {
  readonly file: string;
  readonly line: number;
  readonly col: number;
  readonly length: number;
}

export interface AotLogicalSourceFrame {
  readonly functionId: number;
  readonly location: AotSourceSpan | undefined;
}

interface InlineFrame extends AotLogicalSourceFrame {
  readonly parent: number;
}

interface InlineFrames {
  readonly length: number;
  get(index: number): InlineFrame | undefined;
}

/** One validated wire section backs every frame and sparse PC index. */
class InlineSourceFrames {
  constructor(private readonly view: DataView, readonly length: number, private readonly files: readonly string[]) {}
  get(index: number): InlineFrame | undefined {
    if (!Number.isInteger(index) || index < 0 || index >= this.length) return undefined;
    const offset = 20 + index * 24;
    const view = this.view;
    const fileId = view.getUint32(offset + 8, true);
    return {
      parent: view.getUint32(offset, true),
      functionId: view.getUint32(offset + 4, true),
      location: fileId === NO_PARENT ? undefined : {
        file: this.files[fileId], line: view.getUint32(offset + 12, true),
        col: view.getUint32(offset + 16, true), length: view.getUint32(offset + 20, true),
      },
    };
  }
}

class InlinePcSources extends AotSourceIndex<number> {
  constructor(private readonly view: DataView, private readonly offset: number, readonly size: number) { super(); }
  protected keyAt(index: number): number { return this.view.getUint32(this.offset + index * 8, true); }
  protected valueAt(index: number): number { return this.view.getUint32(this.offset + index * 8 + 4, true); }
}

const EMPTY_FRAMES = Object.freeze(new InlineSourceFrames(new DataView(new ArrayBuffer(0)), 0, []));

export interface AotInlineSources {
  readonly frames: InlineFrames;
  readonly functions: ReadonlyMap<number, ReadonlyMap<number, number>>;
}

export function parseAotInlineSources(module: WebAssembly.Module, files: readonly string[], expectedFunctionCount: number): AotInlineSources {
  const sections = WebAssembly.Module.customSections(module, AOT_INLINE_SOURCE_SECTION);
  if (sections.length === 0) return { frames: EMPTY_FRAMES, functions: new Map() };
  if (sections.length !== 1) throw new Error('duplicate Volang inline source section');
  const bytes = new Uint8Array(sections[0]);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  let offset = 0;
  const word = (): number => {
    if (offset + 4 > bytes.byteLength) throw new Error('truncated Volang inline sources');
    const value = view.getUint32(offset, true);
    offset += 4;
    return value;
  };
  if (bytes.byteLength < 20
    || new TextDecoder('ascii', { fatal: true }).decode(bytes.subarray(0, 8)) !== 'VOINS001') {
    throw new Error('invalid Volang inline source header');
  }
  offset = 8;
  const functionCount = word();
  const frameCount = word();
  const tableCount = word();
  if (functionCount !== expectedFunctionCount) {
    throw new Error('Volang inline source function count differs from physical debug metadata');
  }
  if (functionCount > MAX_EXTERN_COUNT || frameCount > MAX_RECORDS
    || tableCount > Math.min(functionCount, MAX_RECORDS)
    || frameCount * 24 + tableCount * 12 > bytes.byteLength - offset) {
    throw new Error('Volang inline source count exceeds limits or input');
  }
  const frames = new InlineSourceFrames(view, frameCount, files);
  const roots = new Uint32Array(frameCount);
  const depths = new Uint8Array(frameCount);
  for (let index = 0; index < frameCount; index += 1) {
    const parent = word();
    const functionId = word();
    const fileId = word();
    const line = word();
    const col = word();
    const length = word();
    if (functionId >= functionCount || (parent !== NO_PARENT && parent >= index)) {
      throw new Error('invalid Volang inline source ancestry');
    }
    depths[index] = parent === NO_PARENT ? 1 : depths[parent] + 1;
    roots[index] = parent === NO_PARENT ? functionId : roots[parent];
    if (depths[index] > MAX_DEPTH) throw new Error('Volang inline source depth exceeds limit');
    if (fileId === NO_PARENT) {
      if (line !== 0 || col !== 0 || length !== 0) throw new Error('invalid absent inline source span');
    } else {
      if (fileId >= files.length || line === 0 || col === 0 || length === 0) {
        throw new Error('invalid Volang inline source span');
      }
    }
  }
  const functions = new Map<number, ReadonlyMap<number, number>>();
  let previousFunction = -1;
  let remaining = MAX_RECORDS;
  for (let index = 0; index < tableCount; index += 1) {
    const functionId = word();
    const codeLength = word();
    const entryCount = word();
    if (functionId <= previousFunction || functionId >= functionCount
      || entryCount > remaining || entryCount * 8 > bytes.byteLength - offset) {
      throw new Error('invalid Volang inline source function table');
    }
    previousFunction = functionId;
    remaining -= entryCount;
    const entriesOffset = offset;
    let previousPc = -1;
    for (let entry = 0; entry < entryCount; entry += 1) {
      const pc = word();
      const frame = word();
      if (pc <= previousPc || pc >= codeLength || frame >= frameCount || roots[frame] !== functionId) {
        throw new Error('invalid Volang inline source instruction');
      }
      previousPc = pc;
    }
    functions.set(functionId, new InlinePcSources(view, entriesOffset, entryCount));
  }
  if (offset !== bytes.byteLength) throw new Error('Volang inline source section has trailing bytes');
  return { frames, functions };
}

/** Resolve one exact instruction in leaf-to-caller order for diagnostics. */
export function lookupAotLogicalSources(debug: AotDebugMetadata, functionId: number, pc: number): readonly AotLogicalSourceFrame[] {
  const sources = debug.inlineSources;
  let next = sources.functions.get(functionId)?.get(pc);
  if (next === undefined) {
    const location = debug.functions[functionId]?.get(pc);
    return [{ functionId, location }];
  }
  const result: AotLogicalSourceFrame[] = [];
  for (let depth = 0; depth < MAX_DEPTH && next !== NO_PARENT; depth += 1) {
    const frame = sources.frames.get(next);
    if (frame === undefined) break;
    result.push({ functionId: frame.functionId, location: frame.location });
    next = frame.parent;
  }
  return result;
}
