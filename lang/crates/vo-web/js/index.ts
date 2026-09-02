// =============================================================================
// vo-web JavaScript API
// =============================================================================

import { vfs, VirtualFS, registerVFSBindings } from './vfs.js';
import {
  GENERAL_CATEGORY,
  SIMPLE_FOLD,
  SIMPLE_LOWER,
  SIMPLE_TITLE,
  SIMPLE_UPPER,
  WHITE_SPACE,
  type UnicodeRange,
} from './unicode16.js';
import {
  AotJsonError,
  AotStructuredJsonHost,
  type AotStructuredJsonOperations,
  type AotRuntimeMetadata,
  type AotRuntimeType,
  type AotStructField,
  type AotStructType,
} from './aot_json.js';
import { AotPlatformHost } from './aot_platform.js';
import { AotFmtScanHost } from './aot_fmt_scan.js';
import { AotRegexpHost } from './aot_regexp.js';

// Re-export
export { vfs, VirtualFS, registerVFSBindings };
export * from './ui_protocol.js';
export * from './ui_dom.js';
export * from './ui_system.js';
export * from './ui_system_aot.js';
export * from './ui_aot.js';

// WASM module reference
let wasmModule: typeof import('../pkg/vo_web.js') | null = null;

const AOT_MANIFEST_SECTION = 'volang.aot.v5';
const AOT_EXTERN_SECTION = 'volang.externs.v3';
const AOT_RUNTIME_METADATA_SECTION = 'volang.runtime.v1';
const AOT_DEBUG_METADATA_SECTION = 'volang.debug.v2';
const AOT_RUNTIME_MODULE = 'volang:runtime/v3';
const AOT_RUNTIME_FUNCTION = 'call-extern';
const AOT_MEMORY_EXPORT = 'memory';
const AOT_ENTRY_EXPORT = 'vo_start';
const AOT_ALLOC_EXPORT = 'vo_alloc';
const AOT_SEQUENCE_ALLOC_EXPORT = 'vo_alloc_sequence';
const AOT_TYPED_ALLOC_EXPORT = 'vo_alloc_typed';
const AOT_MAP_LOOKUP_EXPORT = 'vo_map_lookup';
const AOT_PANIC_MESSAGE_EXPORT = 'vo_panic_message';
const AOT_PANIC_TYPE_EXPORT = 'vo_panic_type';
const AOT_PANIC_DATA_EXPORT = 'vo_panic_data';
const AOT_RAISE_HOST_PANIC_EXPORT = 'vo_raise_host_panic';
const AOT_FUEL_EXPORT = 'vo_fuel';
const AOT_ABI_VERSION = 5;
const AOT_CORE_MODULE_KIND = 1;
const MAX_AOT_IMAGE_BYTES = 128 * 1024 * 1024;
const MAX_AOT_ARGUMENT_BYTES = 16 * 1024 * 1024;
const MAX_AOT_STDIN_BYTES = 64 * 1024 * 1024;
const DEFAULT_AOT_MEMORY_LIMIT_PAGES = 4096;
const MAX_EXTERN_COUNT = 1_000_000;

export interface AotManifest {
  readonly abiVersion: number;
  readonly target: string;
  readonly semanticModuleLength: number;
  readonly memoryPages: number;
  readonly moduleSha256: Uint8Array;
}

export interface AotExecutionResult {
  readonly status: 'ok' | 'error';
  readonly stdout: string;
  readonly stderr: string;
  readonly exitCode?: number;
}

export interface AotRunResult {
  readonly instance: WebAssembly.Instance;
  readonly manifest: AotManifest;
  readonly result: AotExecutionResult;
  readonly exitCode: number;
}

export interface AotExternCall {
  readonly descriptor: AotExternDescriptor;
  readonly name: string;
  readonly externId: number;
  readonly memory: WebAssembly.Memory;
  readonly frame: number;
  readonly destination: number;
  readonly argumentsStart: number;
  readonly argumentSlots: number;
  readonly args: readonly string[];
  readSlot(slot: number): bigint;
  writeSlot(slot: number, value: bigint): void;
  readFloat64(slot: number): number;
  writeFloat64(slot: number, value: number): void;
  readString(reference: bigint): string;
  readStringBytes(reference: bigint): Uint8Array;
  readStringSlice(reference: bigint): readonly string[];
  readByteSlice(reference: bigint): Uint8Array;
  writeByteSlice(reference: bigint, bytes: Uint8Array): number;
  allocate(bytes: number): number;
  allocateSequence(bytes: number, elementMeta: number): number;
  allocateString(value: string): bigint;
  allocateStringBytes(value: Uint8Array): bigint;
  allocateStringSlice(values: readonly string[]): bigint;
  allocateStringBytesSlice(values: readonly Uint8Array[]): bigint;
  allocateByteSlice(value: Uint8Array): bigint;
  allocateIntSlice(values: readonly bigint[]): bigint;
  allocateInterfaceSlice(values: readonly (readonly [bigint, bigint])[]): bigint;
  allocateNamedStructSlice(
    typeName: string,
    values: readonly Readonly<Record<string, bigint>>[],
  ): bigint;
  writeError(slot: number, message: string, cause?: readonly [bigint, bigint]): void;
  clearError(slot: number): void;
  writeOutput(fd: number, bytes: Uint8Array): void;
  exit(code: number): number;
  panic(message: string): number;
}

export type AotExternHandler = (
  call: AotExternCall,
) => number | void | Promise<number | void>;

export interface AotExternDescriptor {
  readonly id: number;
  readonly name: string;
  readonly required: boolean;
  readonly paramSlots?: number;
  readonly returnSlots: number;
  readonly allowedEffects: bigint;
  readonly effectiveEffects: bigint;
  readonly abiFingerprint: bigint;
  readonly providerIdentity: bigint;
  readonly source: number;
  readonly returnSlotTypes: Uint8Array;
}

export interface AotExternProvider {
  readonly handler: AotExternHandler;
  readonly abiFingerprint?: bigint;
  readonly supportedEffects?: bigint;
}

export interface AotRunOptions {
  readonly args?: readonly string[];
  /** Complete process-standard-input byte stream. Reads consume one line at a time. */
  readonly stdin?: string | Uint8Array;
  readonly externs?: Readonly<Record<string, AotExternHandler | AotExternProvider>>;
  readonly memoryLimitPages?: number;
  /** Guest basic-block budget. Omit for unlimited execution. */
  readonly fuel?: number | bigint;
  /** Optional precompiled vo-aot-support-wasm module for non-browser hosts. */
  readonly supportModule?: BufferSource | WebAssembly.Module;
}

interface AotDebugLocation {
  readonly pc: number;
  readonly file: string;
  readonly line: number;
}

interface AotDebugMetadata {
  readonly functions: readonly ReadonlyMap<number, AotDebugLocation>[];
  readonly frameStateBytes: number;
  readonly frameFunctionIdOffset: number;
  readonly frameParentOffset: number;
  readonly frameDebugPcOffset: number;
}

function readU16(view: DataView, offset: number): number {
  if (offset + 2 > view.byteLength) throw new Error('truncated Volang AOT manifest');
  return view.getUint16(offset, true);
}

function readU32(view: DataView, offset: number): number {
  if (offset + 4 > view.byteLength) throw new Error('truncated Volang AOT manifest');
  return view.getUint32(offset, true);
}

function readU64(view: DataView, offset: number): bigint {
  if (offset + 8 > view.byteLength) throw new Error('truncated Volang AOT manifest');
  return view.getBigUint64(offset, true);
}

function parseAotManifest(module: WebAssembly.Module): AotManifest {
  const sections = WebAssembly.Module.customSections(module, AOT_MANIFEST_SECTION);
  if (sections.length !== 1) {
    throw new Error(`expected one ${AOT_MANIFEST_SECTION} section`);
  }
  const bytes = new Uint8Array(sections[0]);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  const magic = new TextDecoder('ascii', { fatal: true }).decode(bytes.subarray(0, 8));
  if (magic !== 'VOAOTW05') throw new Error('invalid Volang AOT manifest magic');
  const abiVersion = readU16(view, 8);
  if (abiVersion !== AOT_ABI_VERSION) {
    throw new Error(`unsupported Volang AOT ABI ${abiVersion}`);
  }
  if (bytes[10] !== AOT_CORE_MODULE_KIND) throw new Error('artifact is not a Core Wasm AOT module');
  if (bytes[11] !== 0) throw new Error('Volang AOT manifest has unknown flags');
  const semanticModuleLength = readU32(view, 12);
  const memoryPages = readU32(view, 16);
  if (semanticModuleLength < 1) throw new Error('Volang AOT semantic module is empty');
  if (memoryPages < 1 || memoryPages > 65_536) {
    throw new Error(`Volang AOT memory size ${memoryPages} exceeds the wasm32 contract`);
  }
  if (bytes.byteLength < 56) throw new Error('truncated Volang AOT manifest');
  const moduleSha256 = bytes.slice(20, 52);
  const targetLength = readU16(view, 52);
  if (targetLength < 1 || targetLength > 255 || readU16(view, 54) !== 0) {
    throw new Error('invalid Volang AOT target encoding');
  }
  if (56 + targetLength !== bytes.byteLength) {
    throw new Error('Volang AOT manifest length is inconsistent');
  }
  const target = new TextDecoder('utf-8', { fatal: true }).decode(bytes.subarray(56));
  if (target !== 'wasm32-unknown-unknown') {
    throw new Error(`unsupported Core Wasm AOT target ${target}`);
  }
  return { abiVersion, target, semanticModuleLength, memoryPages, moduleSha256 };
}

function parseAotExterns(module: WebAssembly.Module): readonly AotExternDescriptor[] {
  const sections = WebAssembly.Module.customSections(module, AOT_EXTERN_SECTION);
  if (sections.length !== 1) throw new Error(`expected one ${AOT_EXTERN_SECTION} section`);
  const bytes = new Uint8Array(sections[0]);
  if (bytes.byteLength < 12) throw new Error('truncated Volang extern manifest');
  const magic = new TextDecoder('ascii', { fatal: true }).decode(bytes.subarray(0, 8));
  if (magic !== 'VOEXT003') throw new Error('invalid Volang extern manifest magic');
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  const count = readU32(view, 8);
  if (count > MAX_EXTERN_COUNT) throw new Error('Volang extern count exceeds host limits');
  const externs: AotExternDescriptor[] = [];
  let offset = 12;
  for (let index = 0; index < count; index += 1) {
    const length = readU16(view, offset);
    offset += 2;
    if (length < 1 || offset + length > bytes.byteLength) {
      throw new Error('truncated Volang extern name');
    }
    const name = new TextDecoder('utf-8', { fatal: true })
      .decode(bytes.subarray(offset, offset + length));
    offset += length;
    if (offset + 39 > bytes.byteLength) throw new Error('truncated Volang extern ABI');
    const flags = readU16(view, offset);
    const paramShape = bytes[offset + 2];
    const paramSlotsValue = readU16(view, offset + 3);
    const returnSlots = readU16(view, offset + 5);
    const allowedEffects = readU64(view, offset + 7);
    const effectiveEffects = readU64(view, offset + 15);
    const abiFingerprint = readU64(view, offset + 23);
    const providerIdentity = readU64(view, offset + 31);
    offset += 39;
    const source = bytes[offset];
    const reserved = bytes[offset + 1];
    const returnTypeCount = readU16(view, offset + 2);
    offset += 4;
    if ((flags & ~1) !== 0 || (paramShape !== 0 && paramShape !== 1) || reserved !== 0) {
      throw new Error(`invalid Volang extern ABI flags for ${name}`);
    }
    if (paramShape === 1 && paramSlotsValue !== 0) {
      throw new Error(`variadic Volang extern ${name} has a fixed slot count`);
    }
    if (returnTypeCount !== 0 && returnTypeCount !== returnSlots) {
      throw new Error(`Volang extern ${name} has an inconsistent return layout`);
    }
    if (offset + returnTypeCount > bytes.byteLength) {
      throw new Error('truncated Volang extern return layout');
    }
    const returnSlotTypes = bytes.slice(offset, offset + returnTypeCount);
    if (returnSlotTypes.some((slot) => slot > 5)) {
      throw new Error(`Volang extern ${name} has an unknown return slot type`);
    }
    offset += returnTypeCount;
    externs.push({
      id: index,
      name,
      required: (flags & 1) !== 0,
      ...(paramShape === 0 ? { paramSlots: paramSlotsValue } : {}),
      returnSlots,
      allowedEffects,
      effectiveEffects,
      abiFingerprint,
      providerIdentity,
      source,
      returnSlotTypes,
    });
  }
  if (offset !== bytes.byteLength) throw new Error('Volang extern manifest has trailing bytes');
  return externs;
}

function parseAotRuntimeMetadata(module: WebAssembly.Module): AotRuntimeMetadata {
  const sections = WebAssembly.Module.customSections(module, AOT_RUNTIME_METADATA_SECTION);
  if (sections.length !== 1) {
    throw new Error(`expected one ${AOT_RUNTIME_METADATA_SECTION} section`);
  }
  const bytes = new Uint8Array(sections[0]);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  if (bytes.byteLength < 36) throw new Error('truncated Volang runtime metadata');
  const magic = new TextDecoder('ascii', { fatal: true }).decode(bytes.subarray(0, 8));
  if (magic !== 'VORT0001') throw new Error('invalid Volang runtime metadata magic');
  const descriptorCount = readU32(view, 8);
  const typeCount = readU32(view, 12);
  const structCount = readU32(view, 16);
  if (descriptorCount === 0 || descriptorCount > MAX_EXTERN_COUNT
    || typeCount > MAX_EXTERN_COUNT || structCount > MAX_EXTERN_COUNT) {
    throw new Error('Volang runtime metadata count exceeds host limits');
  }
  const absent = 0xffff_ffff;
  const rawErrorValue = readU32(view, 20);
  const rawErrorDescriptor = readU32(view, 24);
  const errorSlots = readU16(view, 28);
  const errorMessageOffset = readU16(view, 30);
  const errorCauseOffset = readU16(view, 32);
  if (readU16(view, 34) !== 0) throw new Error('Volang runtime metadata has unknown flags');
  const decodeDescriptor = (descriptor: number): number | undefined => {
    if (descriptor === absent) return undefined;
    if (descriptor >= descriptorCount) {
      throw new Error('Volang runtime metadata references an invalid allocation descriptor');
    }
    return descriptor;
  };
  const errorDescriptor = decodeDescriptor(rawErrorDescriptor);
  const errorValueRaw = rawErrorValue === absent ? undefined : rawErrorValue;
  if ((errorValueRaw === undefined) !== (errorDescriptor === undefined)
    || (errorValueRaw !== undefined
      && (errorSlots === 0 || errorMessageOffset >= errorSlots || errorCauseOffset + 1 >= errorSlots))) {
    throw new Error('Volang runtime error metadata is incomplete');
  }

  let offset = 36;
  const decoder = new TextDecoder('utf-8', { fatal: true });
  const types = new Map<number, AotRuntimeType>();
  for (let index = 0; index < typeCount; index += 1) {
    if (offset + 52 > bytes.byteLength) throw new Error('truncated Volang runtime type metadata');
    const raw = readU32(view, offset);
    const canonicalMeta = readU32(view, offset + 4);
    const kind = bytes[offset + 8];
    const tag = bytes[offset + 9];
    const typeNameLength = readU16(view, offset + 10);
    if (kind !== (raw & 0xff) || tag > 10) {
      throw new Error('invalid Volang runtime type record');
    }
    const slotCount = readU32(view, offset + 12);
    const storageBytes = readU32(view, offset + 16);
    const fixedDescriptor = decodeDescriptor(readU32(view, offset + 20));
    const sequenceDescriptor = decodeDescriptor(readU32(view, offset + 24));
    const mapDescriptor = decodeDescriptor(readU32(view, offset + 28));
    const mapEntriesDescriptor = decodeDescriptor(readU32(view, offset + 32));
    const first = readU32(view, offset + 36);
    const second = readU32(view, offset + 40);
    const length = readU64(view, offset + 44);
    if (slotCount > 0xffff || storageBytes > 0xffff_ffff
      || types.has(raw) || (canonicalMeta & 0xff) !== kind
      || fixedDescriptor === undefined || sequenceDescriptor === undefined
      || (tag === 2 && length > BigInt(Number.MAX_SAFE_INTEGER))) {
      throw new Error('invalid Volang runtime type layout');
    }
    const expectedStorageBytes = kind === 0 ? 0
      : ([1, 3, 8].includes(kind) ? 1
        : ([4, 9].includes(kind) ? 2
          : ([5, 10, 12].includes(kind) ? 4
            : (kind === 16 ? 16
              : ([14, 15].includes(kind) ? slotCount * 8 : 8)))));
    if (storageBytes !== expectedStorageBytes) {
      throw new Error('Volang runtime type storage width is inconsistent');
    }
    offset += 52;
    if (offset + typeNameLength > bytes.byteLength) {
      throw new Error('truncated Volang runtime type name');
    }
    let typeName: string | undefined;
    if (typeNameLength !== 0) {
      try {
        typeName = decoder.decode(bytes.subarray(offset, offset + typeNameLength));
      } catch {
        throw new Error('invalid UTF-8 in Volang runtime type name');
      }
      if (typeName.length === 0) throw new Error('empty Volang runtime type name');
    }
    types.set(raw, {
      raw,
      canonicalMeta,
      kind,
      tag,
      slotCount,
      storageBytes,
      fixedDescriptor,
      sequenceDescriptor,
      mapDescriptor,
      mapEntriesDescriptor,
      first,
      second,
      length,
      ...(typeName === undefined ? {} : { typeName }),
    });
    offset += typeNameLength;
  }

  const structs: AotStructType[] = [];
  for (let structIndex = 0; structIndex < structCount; structIndex += 1) {
    if (offset + 4 > bytes.byteLength) throw new Error('truncated Volang struct metadata');
    const slotCount = readU16(view, offset);
    const fieldCount = readU16(view, offset + 2);
    offset += 4;
    const fields: AotStructField[] = [];
    for (let fieldIndex = 0; fieldIndex < fieldCount; fieldIndex += 1) {
      if (offset + 20 > bytes.byteLength) throw new Error('truncated Volang struct field metadata');
      const nameLength = readU32(view, offset);
      const tagLength = readU32(view, offset + 4);
      const fieldOffset = readU16(view, offset + 8);
      const fieldSlots = readU16(view, offset + 10);
      const typeRaw = readU32(view, offset + 12);
      const flags = bytes[offset + 16];
      if (flags > 3 || bytes[offset + 17] !== 0 || bytes[offset + 18] !== 0
        || bytes[offset + 19] !== 0 || fieldOffset + fieldSlots > slotCount) {
        throw new Error('invalid Volang struct field layout');
      }
      offset += 20;
      const textLength = nameLength + tagLength;
      if (!Number.isSafeInteger(textLength) || offset + textLength > bytes.byteLength) {
        throw new Error('truncated Volang struct field text');
      }
      let name: string;
      let tag: string;
      try {
        name = decoder.decode(bytes.subarray(offset, offset + nameLength));
        tag = decoder.decode(bytes.subarray(offset + nameLength, offset + textLength));
      } catch {
        throw new Error('invalid UTF-8 in Volang struct metadata');
      }
      fields.push({
        name,
        tag,
        offset: fieldOffset,
        slotCount: fieldSlots,
        typeRaw,
        embedded: (flags & 1) !== 0,
        exported: (flags & 2) !== 0,
      });
      offset += textLength;
    }
    structs.push({ slotCount, fields });
  }
  if (offset !== bytes.byteLength) throw new Error('Volang runtime metadata has trailing bytes');

  for (const type of types.values()) {
    const referenced = type.tag === 1 || type.tag === 2 || type.tag === 3
      ? [type.first]
      : (type.tag === 4 ? [type.first, type.second] : []);
    if (referenced.some((raw) => !types.has(raw))) {
      throw new Error('Volang runtime type references missing child metadata');
    }
    if (type.tag === 5 && (type.first >= structs.length
      || structs[type.first].slotCount !== type.slotCount)) {
      throw new Error('Volang runtime type references missing struct metadata');
    }
    if (type.tag === 4 && (type.mapDescriptor === undefined
      || type.mapEntriesDescriptor === undefined)) {
      throw new Error('Volang map runtime type lacks allocation metadata');
    }
  }
  for (const struct of structs) {
    if (struct.fields.some((field) => {
      const type = types.get(field.typeRaw);
      return type === undefined || field.slotCount !== type.slotCount;
    })) {
      throw new Error('Volang struct field references missing runtime type metadata');
    }
  }
  if (errorValueRaw !== undefined) {
    const errorPointer = types.get(errorValueRaw);
    const errorStruct = errorPointer?.tag === 1 ? types.get(errorPointer.first) : undefined;
    if (errorPointer?.kind !== 22 || errorStruct?.tag !== 5
      || errorStruct.slotCount !== errorSlots
      || errorStruct.fixedDescriptor !== errorDescriptor) {
      throw new Error('Volang runtime error type metadata is inconsistent');
    }
  }
  return {
    descriptorCount,
    types,
    structs,
    errorValueRaw,
    errorDescriptor,
    errorSlots,
    errorMessageOffset,
    errorCauseOffset,
  };
}

function parseAotDebugMetadata(module: WebAssembly.Module): AotDebugMetadata {
  const sections = WebAssembly.Module.customSections(module, AOT_DEBUG_METADATA_SECTION);
  if (sections.length !== 1) {
    throw new Error(`expected one ${AOT_DEBUG_METADATA_SECTION} section`);
  }
  const bytes = new Uint8Array(sections[0]);
  if (bytes.byteLength < 32) throw new Error('truncated Volang debug metadata');
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  const magic = new TextDecoder('ascii', { fatal: true }).decode(bytes.subarray(0, 8));
  if (magic !== 'VODBG002') throw new Error('invalid Volang debug metadata magic');
  const fileCount = readU32(view, 8);
  const functionCount = readU32(view, 12);
  const frameStateBytes = readU32(view, 16);
  const frameFunctionIdOffset = readU32(view, 20);
  const frameParentOffset = readU32(view, 24);
  const frameDebugPcOffset = readU32(view, 28);
  if (fileCount > MAX_EXTERN_COUNT || functionCount > MAX_EXTERN_COUNT) {
    throw new Error('Volang debug metadata count exceeds host limits');
  }
  if (frameStateBytes === 0 || frameStateBytes > 64 * 1024
    || frameFunctionIdOffset + 4 > frameStateBytes
    || frameParentOffset + 4 > frameStateBytes
    || frameDebugPcOffset + 4 > frameStateBytes
    || [frameStateBytes, frameFunctionIdOffset, frameParentOffset, frameDebugPcOffset]
      .some((value) => value % 4 !== 0)
    || new Set([frameFunctionIdOffset, frameParentOffset, frameDebugPcOffset]).size !== 3) {
    throw new Error('invalid Volang debug frame layout');
  }
  const decoder = new TextDecoder('utf-8', { fatal: true });
  const files: string[] = [];
  let offset = 32;
  for (let index = 0; index < fileCount; index += 1) {
    const length = readU32(view, offset);
    offset += 4;
    if (offset + length > bytes.byteLength) throw new Error('truncated Volang debug file path');
    try {
      files.push(decoder.decode(bytes.subarray(offset, offset + length)));
    } catch {
      throw new Error('invalid UTF-8 in Volang debug file path');
    }
    offset += length;
  }
  const functions: Array<ReadonlyMap<number, AotDebugLocation>> = [];
  for (let functionId = 0; functionId < functionCount; functionId += 1) {
    const entryCount = readU32(view, offset);
    offset += 4;
    if (entryCount > MAX_EXTERN_COUNT || offset + entryCount * 20 > bytes.byteLength) {
      throw new Error('truncated Volang debug locations');
    }
    const entries = new Map<number, AotDebugLocation>();
    let previousPc = -1;
    for (let index = 0; index < entryCount; index += 1) {
      const pc = readU32(view, offset);
      const fileId = readU32(view, offset + 4);
      const line = readU32(view, offset + 8);
      const col = readU32(view, offset + 12);
      const length = readU32(view, offset + 16);
      offset += 20;
      if (fileId >= files.length || pc <= previousPc || line === 0 || col === 0 || length === 0) {
        throw new Error('invalid Volang debug location');
      }
      previousPc = pc;
      entries.set(pc, { pc, file: files[fileId], line });
    }
    functions.push(entries);
  }
  if (offset !== bytes.byteLength) throw new Error('Volang debug metadata has trailing bytes');
  return {
    functions,
    frameStateBytes,
    frameFunctionIdOffset,
    frameParentOffset,
    frameDebugPcOffset,
  };
}

function validateAotShape(module: WebAssembly.Module): void {
  const imports = WebAssembly.Module.imports(module);
  const expectedImports = new Map([
    [`${AOT_RUNTIME_MODULE}\0${AOT_RUNTIME_FUNCTION}`, 'function'],
    [`${AOT_RUNTIME_MODULE}\0${AOT_MEMORY_EXPORT}`, 'memory'],
  ]);
  if (
    imports.length !== expectedImports.size ||
    imports.some(
      (entry) => expectedImports.get(`${entry.module}\0${entry.name}`) !== entry.kind,
    )
  ) {
    throw new Error('Volang AOT module imports do not match AOT ABI v5');
  }
  const exports = new Map(WebAssembly.Module.exports(module).map((entry) => [entry.name, entry.kind]));
  const expectedExports = new Map([
    [AOT_ENTRY_EXPORT, 'function'],
    [AOT_ALLOC_EXPORT, 'function'],
    [AOT_SEQUENCE_ALLOC_EXPORT, 'function'],
    [AOT_TYPED_ALLOC_EXPORT, 'function'],
    [AOT_MAP_LOOKUP_EXPORT, 'function'],
    [AOT_PANIC_MESSAGE_EXPORT, 'function'],
    [AOT_PANIC_TYPE_EXPORT, 'function'],
    [AOT_PANIC_DATA_EXPORT, 'function'],
    [AOT_RAISE_HOST_PANIC_EXPORT, 'function'],
    [AOT_FUEL_EXPORT, 'global'],
    [AOT_MEMORY_EXPORT, 'memory'],
  ]);
  if (
    exports.size !== expectedExports.size ||
    [...expectedExports].some(([name, kind]) => exports.get(name) !== kind)
  ) {
    throw new Error('Volang AOT module exports do not match AOT ABI v5');
  }
}

function statusMessage(status: number): string {
  const messages: Record<number, string> = {
    1: 'division by zero',
    2: 'negative shift',
    3: 'index or slice bounds violation',
    4: 'WebAssembly memory allocation failed',
    5: 'scheduler suspension escaped vo_start',
    6: 'send or close on a closed channel',
    7: 'all goroutines are asleep',
    8: 'hash of unhashable map key type',
    9: 'Core-Wasm call stack overflow',
    10: 'comparison of an uncomparable dynamic value',
    11: 'interface type assertion failed',
    12: 'unhandled panic',
    14: 'panic unwind escaped the scheduler',
    15: 'Core-Wasm execution fuel exhausted',
    126: 'invalid generated control flow',
  };
  return messages[status] ?? `runtime status ${status}`;
}

function canonicalExternName(packageName: string, functionName: string): string {
  const encoder = new TextEncoder();
  return `vo1:${encoder.encode(packageName).byteLength}:${packageName}`
    + `:${encoder.encode(functionName).byteLength}:${functionName}`;
}

function isStdlibExtern(
  descriptor: AotExternDescriptor,
  packageName: string,
  functionName: string,
): boolean {
  return descriptor.source === 1
    && descriptor.name === canonicalExternName(packageName, functionName);
}

function isMathExtern(descriptor: AotExternDescriptor, operation: string): boolean {
  // Math intrinsics are authenticated as compiler/runtime built-ins (source
  // zero), while the remaining portable math providers use the stdlib source.
  // Both routes retain the same canonical public extern identity.
  return (descriptor.source === 0 || descriptor.source === 1)
    && descriptor.name === canonicalExternName('math', operation);
}

const MATH_BITS_OPERATIONS = [
  'nativeUintSize',
  'LeadingZeros', 'LeadingZeros8', 'LeadingZeros16', 'LeadingZeros32', 'LeadingZeros64',
  'TrailingZeros', 'TrailingZeros8', 'TrailingZeros16', 'TrailingZeros32', 'TrailingZeros64',
  'OnesCount', 'OnesCount8', 'OnesCount16', 'OnesCount32', 'OnesCount64',
  'Add', 'Add32', 'Add64', 'Sub', 'Sub32', 'Sub64',
  'Mul', 'Mul32', 'Mul64', 'Div', 'Div32', 'Div64',
] as const;

function mathBitsOperation(descriptor: AotExternDescriptor): string | undefined {
  return MATH_BITS_OPERATIONS.find((operation) => (
    isStdlibExtern(descriptor, 'math/bits', operation)
  ));
}

const UNICODE_OPERATIONS = [
  'IsLetter', 'IsDigit', 'IsSpace', 'IsUpper', 'IsLower', 'IsTitle', 'IsControl',
  'IsPrint', 'IsPunct', 'IsGraphic', 'IsNumber', 'IsMark', 'IsSymbol',
  'ToLower', 'ToUpper', 'ToTitle', 'SimpleFold',
] as const;

function unicodeOperation(descriptor: AotExternDescriptor): string | undefined {
  return UNICODE_OPERATIONS.find((operation) => (
    isStdlibExtern(descriptor, 'unicode', operation)
  ));
}

const STRING_OPERATIONS = [
  'Index', 'LastIndex', 'Count', 'ToLower', 'ToUpper', 'ToTitle',
  'Split', 'SplitN', 'SplitAfter', 'SplitAfterN', 'Fields', 'Replace', 'EqualFold',
] as const;

function stringOperation(descriptor: AotExternDescriptor): string | undefined {
  return STRING_OPERATIONS.find((operation) => (
    isStdlibExtern(descriptor, 'strings', operation)
  ));
}

const BYTES_OPERATIONS = [
  'Index', 'LastIndex', 'Count', 'ToLower', 'ToUpper', 'ToTitle', 'Replace', 'EqualFold',
] as const;

function bytesOperation(descriptor: AotExternDescriptor): string | undefined {
  return BYTES_OPERATIONS.find((operation) => (
    isStdlibExtern(descriptor, 'bytes', operation)
  ));
}

function strconvOperation(descriptor: AotExternDescriptor): string | undefined {
  if (isStdlibExtern(descriptor, 'strconv', 'parseFloat')) return 'parseFloat';
  if (isStdlibExtern(descriptor, 'strconv', 'formatFloat')) return 'formatFloat';
  return undefined;
}

const RAND_OPERATIONS = [
  'Intn', 'Int63n', 'Int', 'Uint64', 'Uint32', 'Float64', 'Float32', 'Read',
] as const;

function randOperation(descriptor: AotExternDescriptor): string | undefined {
  return RAND_OPERATIONS.find((operation) => (
    isStdlibExtern(descriptor, 'math/rand', operation)
  ));
}

const TIME_OPERATIONS = [
  'nowUnixNano', 'nowMonoNano', 'localOffsetAt', 'localAbbrevAt',
  'ianaOffsetAt', 'ianaAbbrevAt', 'loadLocation',
] as const;

function timeOperation(descriptor: AotExternDescriptor): string | undefined {
  return TIME_OPERATIONS.find((operation) => (
    isStdlibExtern(descriptor, 'time', operation)
  ));
}

function dateAtUnixSeconds(seconds: bigint): Date | undefined {
  const numeric = Number(seconds);
  if (!Number.isSafeInteger(numeric)) return undefined;
  const date = new Date(numeric * 1000);
  return Number.isFinite(date.getTime()) ? date : undefined;
}

function canonicalTimeZone(name: string): string | undefined {
  try {
    return new Intl.DateTimeFormat('en-US', { timeZone: name }).resolvedOptions().timeZone;
  } catch {
    return undefined;
  }
}

function timeZoneOffsetAt(timeZone: string, seconds: bigint): number {
  const date = dateAtUnixSeconds(seconds);
  if (date === undefined) return 0;
  try {
    const parts = new Intl.DateTimeFormat('en-US-u-ca-iso8601', {
      timeZone,
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit',
      hourCycle: 'h23',
    }).formatToParts(date);
    const component = (kind: Intl.DateTimeFormatPartTypes): number => {
      const text = parts.find((part) => part.type === kind)?.value;
      if (text === undefined) throw new Error(`missing ${kind} time-zone component`);
      return Number(text);
    };
    const wallClock = new Date(0);
    wallClock.setUTCFullYear(
      component('year'),
      component('month') - 1,
      component('day'),
    );
    wallClock.setUTCHours(
      component('hour'),
      component('minute'),
      component('second'),
      0,
    );
    return Math.trunc(wallClock.getTime() / 1000) - Number(seconds);
  } catch {
    return 0;
  }
}

function timeZoneAbbreviationAt(timeZone: string | undefined, seconds: bigint): string {
  const date = dateAtUnixSeconds(seconds);
  if (date === undefined) return timeZone === undefined ? 'Local' : 'UTC';
  try {
    const formatter = new Intl.DateTimeFormat('en-US', {
      ...(timeZone === undefined ? {} : { timeZone }),
      timeZoneName: 'short',
    });
    return formatter.formatToParts(date).find((part) => part.type === 'timeZoneName')?.value
      ?? (timeZone === undefined ? 'Local' : 'UTC');
  } catch {
    return timeZone === undefined ? 'Local' : 'UTC';
  }
}

function validUnicodeScalar(rune: number): boolean {
  return Number.isInteger(rune)
    && rune >= 0
    && rune <= 0x10ffff
    && (rune < 0xd800 || rune > 0xdfff);
}

function unicodeRangeValue(
  ranges: readonly UnicodeRange[],
  codePoint: number,
  fallback: number,
): number {
  let lower = 0;
  let upper = ranges.length - 1;
  while (lower <= upper) {
    const middle = lower + ((upper - lower) >>> 1);
    const [start, end, value] = ranges[middle];
    if (codePoint < start) upper = middle - 1;
    else if (codePoint > end) lower = middle + 1;
    else return value;
  }
  return fallback;
}

function unicodeCategory(rune: number): number {
  return validUnicodeScalar(rune) ? unicodeRangeValue(GENERAL_CATEGORY, rune, 0) : 0;
}

function unicodeIsPrint(rune: number): boolean {
  const category = unicodeCategory(rune);
  return rune === 0x20 || (category >= 1 && category <= 11)
    || (category >= 19 && category <= 29);
}

function unicodeMap(rune: number, ranges: readonly UnicodeRange[]): number {
  return validUnicodeScalar(rune) ? rune + unicodeRangeValue(ranges, rune, 0) : rune;
}

function roundRationalToEven(numerator: bigint, denominator: bigint): bigint {
  const quotient = numerator / denominator;
  const remainder = numerator % denominator;
  const doubled = remainder * 2n;
  return doubled > denominator || (doubled === denominator && (quotient & 1n) !== 0n)
    ? quotient + 1n : quotient;
}

function rationalBinaryExponent(numerator: bigint, denominator: bigint): number {
  let exponent = bitLength(numerator) - bitLength(denominator);
  if (exponent >= 0) {
    if (numerator < (denominator << BigInt(exponent))) exponent -= 1;
  } else if ((numerator << BigInt(-exponent)) < denominator) exponent -= 1;
  return exponent;
}

function rationalToFloat(
  numerator: bigint,
  denominator: bigint,
  negative: boolean,
  bitSize: number,
): { value: number; overflow: boolean } {
  const precision = bitSize === 32 ? 24 : 53;
  const minimumNormal = bitSize === 32 ? -126 : -1022;
  const maximum = bitSize === 32 ? 127 : 1023;
  const bias = bitSize === 32 ? 127 : 1023;
  const fractionBits = precision - 1;
  const sign = negative ? 1n << BigInt(bitSize - 1) : 0n;
  if (numerator === 0n) {
    return {
      value: bitSize === 32
        ? float32FromBits(Number(sign)) : float64FromBits(sign),
      overflow: false,
    };
  }
  let exponent = rationalBinaryExponent(numerator, denominator);
  if (exponent > maximum) {
    return { value: negative ? -Infinity : Infinity, overflow: true };
  }
  let encoded: bigint;
  if (exponent >= minimumNormal) {
    const shift = fractionBits - exponent;
    let significand = shift >= 0
      ? roundRationalToEven(numerator << BigInt(shift), denominator)
      : roundRationalToEven(numerator, denominator << BigInt(-shift));
    if (significand === 1n << BigInt(precision)) {
      significand >>= 1n;
      exponent += 1;
      if (exponent > maximum) {
        return { value: negative ? -Infinity : Infinity, overflow: true };
      }
    }
    encoded = BigInt(exponent + bias) << BigInt(fractionBits);
    encoded |= significand & ((1n << BigInt(fractionBits)) - 1n);
  } else {
    const quantum = minimumNormal - fractionBits;
    encoded = quantum < 0
      ? roundRationalToEven(numerator << BigInt(-quantum), denominator)
      : roundRationalToEven(numerator, denominator << BigInt(quantum));
  }
  encoded |= sign;
  return {
    value: bitSize === 32 ? float32FromBits(Number(encoded)) : float64FromBits(encoded),
    overflow: false,
  };
}

function validFloatUnderscores(text: string, hexadecimal: boolean): boolean {
  const start = /^[+\-]/.test(text) ? 1 : 0;
  const prefixed = /^0[xX]/.test(text.slice(start));
  hexadecimal ||= prefixed;
  const isDigit = (character: string | undefined) => character !== undefined
    && (/[0-9]/.test(character) || (hexadecimal && /[a-fA-F]/.test(character)));
  for (let index = start; index < text.length; index += 1) {
    if (text[index] !== '_') continue;
    const followsPrefix = prefixed && index === start + 2;
    if ((!followsPrefix && !isDigit(text[index - 1])) || !isDigit(text[index + 1])) return false;
  }
  return true;
}

function parseVolangFloat(text: string, bitSizeValue: bigint): {
  value: number;
  status: number;
} {
  const bitSize = bitSizeValue === 32n ? 32 : 64;
  if (/^nan$/i.test(text)) {
    return {
      value: bitSize === 32 ? float32FromBits(0x7fc0_0000) : float64FromBits(0x7ff8_0000_0000_0001n),
      status: 0,
    };
  }
  const infinity = /^([+\-]?)(inf(?:inity)?)$/i.exec(text);
  if (infinity) return { value: infinity[1] === '-' ? -Infinity : Infinity, status: 0 };
  const negative = text.startsWith('-');
  const unsigned = /^[+\-]/.test(text) ? text.slice(1) : text;
  const hexadecimal = /^0[xX]/.test(unsigned);
  if (text.includes('_') && !validFloatUnderscores(text, hexadecimal)) {
    return { value: 0, status: 1 };
  }
  const clean = text.replace(/_/g, '');
  if (hexadecimal) {
    const match = /^[+\-]?0[xX]([0-9a-fA-F]*)(?:\.([0-9a-fA-F]*))?[pP]([+\-]?\d+)$/.exec(clean);
    if (!match || (match[1].length === 0 && (match[2]?.length ?? 0) === 0)) {
      return { value: 0, status: 1 };
    }
    const fraction = match[2] ?? '';
    const coefficientText = (match[1] + fraction).replace(/^0+/, '') || '0';
    const coefficient = BigInt(`0x${coefficientText}`);
    const exponent = Number(match[3]) - fraction.length * 4;
    if (!Number.isSafeInteger(exponent)) {
      return exponent > 0
        ? { value: negative ? -Infinity : Infinity, status: 2 }
        : rationalToFloat(0n, 1n, negative, bitSize).value === 0
          ? { value: negative ? -0 : 0, status: 0 } : { value: 0, status: 1 };
    }
    if (exponent > 5000) return { value: negative ? -Infinity : Infinity, status: 2 };
    if (exponent < -5000) return { value: negative ? -0 : 0, status: 0 };
    const converted = exponent >= 0
      ? rationalToFloat(coefficient << BigInt(exponent), 1n, negative, bitSize)
      : rationalToFloat(coefficient, 1n << BigInt(-exponent), negative, bitSize);
    return { value: converted.value, status: converted.overflow ? 2 : 0 };
  }
  const match = /^[+\-]?(?:(\d+)(?:\.(\d*))?|\.(\d+))(?:[eE]([+\-]?\d+))?$/.exec(clean);
  if (!match) return { value: 0, status: 1 };
  const integer = match[1] ?? '';
  const fraction = match[2] ?? match[3] ?? '';
  const digits = (integer + fraction).replace(/^0+/, '') || '0';
  const explicitExponent = Number(match[4] ?? '0');
  if (!Number.isSafeInteger(explicitExponent)) {
    return explicitExponent > 0
      ? { value: negative ? -Infinity : Infinity, status: 2 }
      : { value: negative ? -0 : 0, status: 0 };
  }
  const decimalExponent = explicitExponent - fraction.length;
  const adjustedExponent = decimalExponent + digits.length - 1;
  const overflowBoundary = bitSize === 32 ? 50 : 320;
  const underflowBoundary = bitSize === 32 ? -60 : -340;
  if (adjustedExponent > overflowBoundary) {
    return { value: negative ? -Infinity : Infinity, status: 2 };
  }
  if (adjustedExponent < underflowBoundary) return { value: negative ? -0 : 0, status: 0 };
  const coefficient = BigInt(digits);
  const converted = decimalExponent >= 0
    ? rationalToFloat(coefficient * (10n ** BigInt(decimalExponent)), 1n, negative, bitSize)
    : rationalToFloat(coefficient, 10n ** BigInt(-decimalExponent), negative, bitSize);
  return { value: converted.value, status: converted.overflow ? 2 : 0 };
}

interface FiniteFloatParts {
  readonly negative: boolean;
  readonly coefficient: bigint;
  readonly exponent: number;
  readonly formatExponent: number;
  readonly fractionBits: number;
}

function finiteFloatParts(value: number, bitSize: number): FiniteFloatParts {
  if (bitSize === 32) {
    const bits = float32Bits(value);
    const encodedExponent = (bits >>> 23) & 0xff;
    const fraction = bits & 0x007f_ffff;
    return {
      negative: (bits >>> 31) !== 0,
      coefficient: BigInt(encodedExponent === 0 ? fraction : fraction | 0x0080_0000),
      exponent: encodedExponent === 0 ? -149 : encodedExponent - 150,
      formatExponent: encodedExponent === 0 ? -126 : encodedExponent - 127,
      fractionBits: 23,
    };
  }
  const bits = float64Bits(value);
  const encodedExponent = Number((bits >> 52n) & 0x7ffn);
  const fraction = bits & 0x000f_ffff_ffff_ffffn;
  return {
    negative: (bits >> 63n) !== 0n,
    coefficient: encodedExponent === 0 ? fraction : fraction | (1n << 52n),
    exponent: encodedExponent === 0 ? -1074 : encodedExponent - 1075,
    formatExponent: encodedExponent === 0 ? -1022 : encodedExponent - 1023,
    fractionBits: 52,
  };
}

function roundFloatAtDecimalScale(value: number, scale: number, bitSize: number): bigint {
  const parts = finiteFloatParts(value, bitSize);
  let numerator = parts.coefficient;
  let denominator = 1n;
  if (parts.exponent >= 0) numerator <<= BigInt(parts.exponent);
  else denominator <<= BigInt(-parts.exponent);
  if (scale >= 0) numerator *= 10n ** BigInt(scale);
  else denominator *= 10n ** BigInt(-scale);
  return roundRationalToEven(numerator, denominator);
}

function compareFloatToPower10(value: number, exponent: number, bitSize: number): number {
  const parts = finiteFloatParts(value, bitSize);
  let numerator = parts.coefficient;
  let denominator = 1n;
  if (parts.exponent >= 0) numerator <<= BigInt(parts.exponent);
  else denominator <<= BigInt(-parts.exponent);
  if (exponent >= 0) denominator *= 10n ** BigInt(exponent);
  else numerator *= 10n ** BigInt(-exponent);
  return numerator < denominator ? -1 : (numerator > denominator ? 1 : 0);
}

function exactDecimalExponent(value: number, bitSize: number): number {
  let exponent = Math.floor(Math.log10(Math.abs(value)));
  while (compareFloatToPower10(value, exponent, bitSize) < 0) exponent -= 1;
  while (compareFloatToPower10(value, exponent + 1, bitSize) >= 0) exponent += 1;
  return exponent;
}

interface DecimalParts {
  readonly negative: boolean;
  readonly digits: string;
  readonly decimalPoint: number;
}

function parseDecimalParts(text: string, negative: boolean): DecimalParts {
  const match = /^[+\-]?(\d+)(?:\.(\d*))?(?:[eE]([+\-]?\d+))?$/.exec(text);
  if (!match) throw new Error(`invalid internal decimal float ${text}`);
  const before = match[1];
  let digits = before + (match[2] ?? '');
  let decimalPoint = before.length + Number(match[3] ?? '0');
  while (digits.length > 1 && digits.startsWith('0')) {
    digits = digits.slice(1);
    decimalPoint -= 1;
  }
  while (digits.length > 1 && digits.endsWith('0')) digits = digits.slice(0, -1);
  return { negative, digits, decimalPoint };
}

function decimalPartsToFixed(parts: DecimalParts): string {
  const sign = parts.negative ? '-' : '';
  if (parts.decimalPoint <= 0) {
    return `${sign}0.${'0'.repeat(-parts.decimalPoint)}${parts.digits}`;
  }
  if (parts.decimalPoint >= parts.digits.length) {
    return sign + parts.digits + '0'.repeat(parts.decimalPoint - parts.digits.length);
  }
  return `${sign}${parts.digits.slice(0, parts.decimalPoint)}.${parts.digits.slice(parts.decimalPoint)}`;
}

function decimalExponentSuffix(exponent: number): string {
  return `${exponent < 0 ? '-' : '+'}${String(Math.abs(exponent)).padStart(2, '0')}`;
}

function decimalPartsToExponent(parts: DecimalParts, upper: boolean): string {
  const fraction = parts.digits.length > 1 ? `.${parts.digits.slice(1)}` : '';
  return `${parts.negative ? '-' : ''}${parts.digits[0]}${fraction}`
    + `${upper ? 'E' : 'e'}${decimalExponentSuffix(parts.decimalPoint - 1)}`;
}

function shortestDecimalParts(value: number, bitSize: number): DecimalParts {
  const negative = Object.is(value, -0) || value < 0;
  const magnitude = Math.abs(bitSize === 32 ? Math.fround(value) : value);
  if (magnitude === 0) return { negative, digits: '0', decimalPoint: 1 };
  if (bitSize === 64) return parseDecimalParts(String(magnitude), negative);
  const expected = float32Bits(magnitude);
  for (let significant = 1; significant <= 9; significant += 1) {
    const candidate = magnitude.toExponential(significant - 1);
    if (float32Bits(Number(candidate)) === expected) {
      return parseDecimalParts(candidate, negative);
    }
  }
  return parseDecimalParts(magnitude.toExponential(8), negative);
}

function formatFixedFloat(value: number, precision: number, bitSize: number): string {
  const negative = finiteFloatParts(value, bitSize).negative;
  const rounded = roundFloatAtDecimalScale(Math.abs(value), precision, bitSize).toString();
  if (precision === 0) return `${negative ? '-' : ''}${rounded}`;
  const padded = rounded.padStart(precision + 1, '0');
  return `${negative ? '-' : ''}${padded.slice(0, -precision)}.${padded.slice(-precision)}`;
}

function formatExponentFloat(
  value: number,
  precision: number,
  bitSize: number,
  upper: boolean,
): string {
  const negative = finiteFloatParts(value, bitSize).negative;
  if (value === 0) {
    return `${negative ? '-' : ''}0${precision > 0 ? `.${'0'.repeat(precision)}` : ''}`
      + `${upper ? 'E' : 'e'}+00`;
  }
  let exponent = exactDecimalExponent(value, bitSize);
  let rounded = roundFloatAtDecimalScale(Math.abs(value), precision - exponent, bitSize);
  if (rounded.toString().length > precision + 1) {
    exponent += 1;
    rounded = roundFloatAtDecimalScale(Math.abs(value), precision - exponent, bitSize);
  }
  const digits = rounded.toString().padStart(precision + 1, '0');
  return `${negative ? '-' : ''}${digits[0]}${precision > 0 ? `.${digits.slice(1)}` : ''}`
    + `${upper ? 'E' : 'e'}${decimalExponentSuffix(exponent)}`;
}

function formatHexFloat(value: number, precision: number, bitSize: number, upper: boolean): string {
  const parts = finiteFloatParts(value, bitSize);
  const mask64 = (1n << 64n) - 1n;
  let mantissa = parts.coefficient << BigInt(60 - parts.fractionBits);
  let exponent = parts.formatExponent;
  if (mantissa === 0n) exponent = 0;
  while (mantissa !== 0n && (mantissa & (1n << 60n)) === 0n) {
    mantissa <<= 1n;
    exponent -= 1;
  }
  if (precision >= 0 && precision < 15) {
    const shift = precision * 4;
    const extra = ((mantissa << BigInt(shift)) & mask64) & ((1n << 60n) - 1n);
    mantissa >>= BigInt(60 - shift);
    if ((extra | (mantissa & 1n)) > 1n << 59n) mantissa += 1n;
    mantissa <<= BigInt(60 - shift);
    if ((mantissa & (1n << 61n)) !== 0n) {
      mantissa >>= 1n;
      exponent += 1;
    }
  }
  const alphabet = upper ? '0123456789ABCDEF' : '0123456789abcdef';
  let result = `${parts.negative ? '-' : ''}0${upper ? 'X' : 'x'}${Number((mantissa >> 60n) & 1n)}`;
  mantissa = (mantissa << 4n) & mask64;
  if (precision < 0 && mantissa !== 0n) {
    result += '.';
    while (mantissa !== 0n) {
      result += alphabet[Number((mantissa >> 60n) & 15n)];
      mantissa = (mantissa << 4n) & mask64;
    }
  } else if (precision > 0) {
    result += '.';
    for (let index = 0; index < precision; index += 1) {
      result += alphabet[Number((mantissa >> 60n) & 15n)];
      mantissa = (mantissa << 4n) & mask64;
    }
  }
  return `${result}${upper ? 'P' : 'p'}${decimalExponentSuffix(exponent)}`;
}

function formatVolangFloat(
  input: number,
  formatByte: number,
  precisionValue: bigint,
  bitSizeValue: bigint,
): Uint8Array {
  const bitSize = bitSizeValue === 32n ? 32 : 64;
  const value = bitSize === 32 ? Math.fround(input) : input;
  if (Number.isNaN(value)) return new TextEncoder().encode('NaN');
  if (!Number.isFinite(value)) return new TextEncoder().encode(value < 0 ? '-Inf' : '+Inf');
  if (precisionValue > 1_000_000n) throw new Error('strconv precision exceeds AOT host limits');
  const precision = precisionValue < -1n ? -1 : Number(precisionValue);
  const format = String.fromCharCode(formatByte);
  let result: string;
  if (format === 'b') {
    const parts = finiteFloatParts(value, bitSize);
    result = `${parts.negative ? '-' : ''}${parts.coefficient}`
      + `p${parts.exponent >= 0 ? '+' : ''}${parts.exponent}`;
  } else if (format === 'x' || format === 'X') {
    result = formatHexFloat(value, precision, bitSize, format === 'X');
  } else if (format === 'f') {
    result = precision < 0
      ? decimalPartsToFixed(shortestDecimalParts(value, bitSize))
      : formatFixedFloat(value, precision, bitSize);
  } else if (format === 'e' || format === 'E') {
    result = precision < 0
      ? decimalPartsToExponent(shortestDecimalParts(value, bitSize), format === 'E')
      : formatExponentFloat(value, precision, bitSize, format === 'E');
  } else if (format === 'g' || format === 'G') {
    if (value === 0) result = finiteFloatParts(value, bitSize).negative ? '-0' : '0';
    else if (precision < 0) {
      const parts = shortestDecimalParts(value, bitSize);
      const exponent = parts.decimalPoint - 1;
      result = exponent < -4 || exponent >= 6
        ? decimalPartsToExponent(parts, format === 'G') : decimalPartsToFixed(parts);
    } else {
      const significant = precision === 0 ? 1 : precision;
      const rounded = parseDecimalParts(
        formatExponentFloat(value, significant - 1, bitSize, false).replace('e', 'e'),
        value < 0,
      );
      const exponent = rounded.decimalPoint - 1;
      result = exponent < -4 || exponent >= significant
        ? decimalPartsToExponent(rounded, format === 'G') : decimalPartsToFixed(rounded);
    }
  } else return Uint8Array.of(0x25, formatByte);
  return new TextEncoder().encode(result);
}

function roundShiftToEven(value: bigint, shift: number): bigint {
  if (shift <= 0) return value << BigInt(-shift);
  const distance = BigInt(shift);
  const quotient = value >> distance;
  const remainder = value - (quotient << distance);
  const halfway = 1n << (distance - 1n);
  return remainder > halfway || (remainder === halfway && (quotient & 1n) !== 0n)
    ? quotient + 1n
    : quotient;
}

function bitLength(value: bigint): number {
  return value === 0n ? 0 : value.toString(2).length;
}

function float64FromBits(bits: bigint): number {
  const storage = new DataView(new ArrayBuffer(8));
  storage.setBigUint64(0, BigInt.asUintN(64, bits), true);
  return storage.getFloat64(0, true);
}

function float32FromBits(bits: number): number {
  const storage = new DataView(new ArrayBuffer(4));
  storage.setUint32(0, bits >>> 0, true);
  return storage.getFloat32(0, true);
}

function float32Bits(value: number): number {
  const storage = new DataView(new ArrayBuffer(4));
  storage.setFloat32(0, value, true);
  return storage.getUint32(0, true);
}

function float64Bits(value: number): bigint {
  const storage = new DataView(new ArrayBuffer(8));
  storage.setFloat64(0, value, true);
  return storage.getBigUint64(0, true);
}

function copyFloat64Sign(value: number, signSource: number): number {
  return float64FromBits(
    (float64Bits(value) & 0x7fff_ffff_ffff_ffffn)
      | (float64Bits(signSource) & 0x8000_0000_0000_0000n),
  );
}

function frexpFloat64(value: number): readonly [number, bigint] {
  if (value === 0 || !Number.isFinite(value)) return [value, 0n];
  const bits = float64Bits(value);
  const sign = bits & 0x8000_0000_0000_0000n;
  const encodedExponent = (bits >> 52n) & 0x7ffn;
  const mantissa = bits & 0x000f_ffff_ffff_ffffn;
  if (encodedExponent === 0n) {
    const [fraction, exponent] = frexpFloat64(value * 18_014_398_509_481_984);
    return [fraction, exponent - 54n];
  }
  return [
    float64FromBits(sign | 0x3fe0_0000_0000_0000n | mantissa),
    encodedExponent - 1022n,
  ];
}

function ldexpFloat64(fraction: number, requestedExponent: bigint): number {
  if (fraction === 0 || !Number.isFinite(fraction)) return fraction;
  let normalized = fraction;
  let exponent = requestedExponent;
  if (Math.abs(normalized) < 2.2250738585072014e-308) {
    normalized *= 4_503_599_627_370_496;
    exponent -= 52n;
  }
  let bits = float64Bits(normalized);
  const encodedExponent = (bits >> 52n) & 0x7ffn;
  exponent += encodedExponent - 1023n;
  if (exponent < -1075n) return copyFloat64Sign(0, fraction);
  if (exponent > 1023n) return copyFloat64Sign(Number.POSITIVE_INFINITY, fraction);
  let multiplier = 1;
  if (exponent < -1022n) {
    exponent += 53n;
    multiplier = 1 / 9_007_199_254_740_992;
  }
  bits &= 0x800f_ffff_ffff_ffffn;
  bits |= (exponent + 1023n) << 52n;
  return multiplier * float64FromBits(bits);
}

function decomposeFiniteFloat64(value: number): { coefficient: bigint; exponent: number } {
  const storage = new DataView(new ArrayBuffer(8));
  storage.setFloat64(0, value, true);
  const bits = storage.getBigUint64(0, true);
  const negative = (bits >> 63n) !== 0n;
  const encodedExponent = Number((bits >> 52n) & 0x7ffn);
  const fraction = bits & 0x000f_ffff_ffff_ffffn;
  const significand = encodedExponent === 0 ? fraction : (1n << 52n) | fraction;
  return {
    coefficient: negative ? -significand : significand,
    exponent: encodedExponent === 0 ? -1074 : encodedExponent - 1075,
  };
}

/** IEEE-754 binary64 fused multiply-add with one round-to-nearest-even step. */
function fusedMultiplyAdd(x: number, y: number, z: number): number {
  if (Number.isNaN(x) || Number.isNaN(y) || Number.isNaN(z)) return Number.NaN;
  if (!Number.isFinite(x) || !Number.isFinite(y)) {
    if (x === 0 || y === 0) return Number.NaN;
    const product = x * y;
    return !Number.isFinite(z) && Object.is(product, -z) ? Number.NaN : product;
  }
  if (!Number.isFinite(z)) return z;

  const left = decomposeFiniteFloat64(x);
  const right = decomposeFiniteFloat64(y);
  const addend = decomposeFiniteFloat64(z);
  const productCoefficient = left.coefficient * right.coefficient;
  const productExponent = left.exponent + right.exponent;
  const commonExponent = Math.min(productExponent, addend.exponent);
  const exact = (productCoefficient << BigInt(productExponent - commonExponent))
    + (addend.coefficient << BigInt(addend.exponent - commonExponent));
  if (exact === 0n) return x * y + z;

  const negative = exact < 0n;
  const magnitude = negative ? -exact : exact;
  const bits = bitLength(magnitude);
  let topExponent = commonExponent + bits - 1;
  const sign = negative ? 1n << 63n : 0n;
  if (topExponent > 1023) return negative ? Number.NEGATIVE_INFINITY : Number.POSITIVE_INFINITY;

  if (topExponent >= -1022) {
    let significand = roundShiftToEven(magnitude, bits - 53);
    if (significand === 1n << 53n) {
      significand >>= 1n;
      topExponent += 1;
      if (topExponent > 1023) {
        return negative ? Number.NEGATIVE_INFINITY : Number.POSITIVE_INFINITY;
      }
    }
    const encodedExponent = BigInt(topExponent + 1023) << 52n;
    return float64FromBits(sign | encodedExponent | (significand - (1n << 52n)));
  }

  const subnormal = roundShiftToEven(magnitude, -(commonExponent + 1074));
  if (subnormal === 0n) return float64FromBits(sign);
  if (subnormal >= 1n << 52n) return float64FromBits(sign | (1n << 52n));
  return float64FromBits(sign | subnormal);
}

function builtInAotExtern(descriptor: AotExternDescriptor): boolean {
  const { name } = descriptor;
  if ([
    'vo_print', 'vo_println', 'vo_assert', 'vo_conv_int_str', 'vo_conv_str_bytes',
    'vo_conv_bytes_str', 'vo_conv_str_runes', 'vo_conv_runes_str', 'vo_copy',
    'vo_copy_string', 'vo_slice_append_slice', 'vo_slice_append_string',
  ].includes(name)) return descriptor.source === 0;
  if (isStdlibExtern(descriptor, 'os', 'nativeGetArgs')
    || isStdlibExtern(descriptor, 'fmt', 'nativeWrite')
    || isStdlibExtern(descriptor, 'fmt', 'nativeReadLine')
    || isStdlibExtern(descriptor, 'fmt', 'nativeSprint')
    || isStdlibExtern(descriptor, 'fmt', 'nativeSprintln')
    || isStdlibExtern(descriptor, 'fmt', 'nativeSprintf')
    || isStdlibExtern(descriptor, 'time', 'blocking_sleepNano')) return true;
  if ((descriptor.source === 0 || descriptor.source === 1)
    && descriptor.name === canonicalExternName('runtime', 'Caller')) return true;
  if (mathBitsOperation(descriptor) !== undefined) return true;
  if (unicodeOperation(descriptor) !== undefined) return true;
  if (stringOperation(descriptor) !== undefined) return true;
  if (bytesOperation(descriptor) !== undefined) return true;
  if (strconvOperation(descriptor) !== undefined) return true;
  if (randOperation(descriptor) !== undefined) return true;
  if (timeOperation(descriptor) !== undefined) return true;
  if (isStdlibExtern(descriptor, 'encoding/json', 'marshalAny')
    || isStdlibExtern(descriptor, 'encoding/json', 'unmarshalAny')
    || isStdlibExtern(descriptor, 'encoding/toml', 'marshalAny')
    || isStdlibExtern(descriptor, 'encoding/toml', 'unmarshalAny')) return true;
  if (AotPlatformHost.supportsDescriptor(descriptor)) return true;
  if (AotFmtScanHost.supportsDescriptor(descriptor)) return true;
  if (AotRegexpHost.supportsDescriptor(descriptor)) return true;
  return [
    'Floor', 'Ceil', 'Round', 'Trunc', 'Sqrt', 'Cbrt', 'Pow', 'Hypot', 'Exp', 'Exp2',
    'Expm1', 'Log', 'Log2', 'Log10', 'Log1p', 'Sin', 'Cos', 'Tan', 'Asin', 'Acos',
    'Atan', 'Atan2', 'Sinh', 'Cosh', 'Tanh', 'Asinh', 'Acosh', 'Atanh', 'Mod', 'Modf',
    'Frexp', 'Ldexp', 'FMA', 'Inf', 'NaN', 'Float64bits', 'Float64frombits',
    'Float32bits', 'Float32frombits',
  ]
    .some((operation) => isMathExtern(descriptor, operation));
}

function builtInAotExternEffects(descriptor: AotExternDescriptor): bigint | undefined {
  if (!builtInAotExtern(descriptor)) return undefined;
  const platformEffects = AotPlatformHost.supportedEffects(descriptor);
  if (platformEffects !== undefined) return platformEffects;
  if (isStdlibExtern(descriptor, 'time', 'blocking_sleepNano')) return 1n << 2n;
  return 0n;
}

function externProvider(
  value: AotExternHandler | AotExternProvider | undefined,
): AotExternProvider | undefined {
  if (typeof value === 'function') return { handler: value };
  return value;
}

function preflightAotExterns(
  externs: readonly AotExternDescriptor[],
  providers: AotRunOptions['externs'],
): void {
  const missing: string[] = [];
  for (const descriptor of externs) {
    if (!descriptor.required) continue;
    const provider = externProvider(providers?.[descriptor.name]);
    if (!provider) {
      const supportedEffects = builtInAotExternEffects(descriptor);
      if (supportedEffects === undefined) missing.push(descriptor.name);
      else if ((descriptor.effectiveEffects & ~supportedEffects) !== 0n) {
        throw new Error(`Volang built-in extern ${descriptor.name} requires unsupported control effects`);
      }
      continue;
    }
    if (provider.abiFingerprint !== undefined
      && provider.abiFingerprint !== descriptor.abiFingerprint) {
      throw new Error(`Volang extern ${descriptor.name} ABI fingerprint does not match the image`);
    }
    const supportedEffects = provider.supportedEffects ?? 0n;
    if ((descriptor.effectiveEffects & ~supportedEffects) !== 0n) {
      throw new Error(`Volang extern ${descriptor.name} requires unsupported control effects`);
    }
  }
  if (missing.length !== 0) {
    throw new Error(`Volang AOT host is missing required extern providers: ${missing.join(', ')}`);
  }
}

function formatVoFloat(value: number): string {
  if (Number.isNaN(value)) return 'NaN';
  if (value === Infinity) return '+Inf';
  if (value === -Infinity) return '-Inf';
  const encoded = String(value);
  const match = /^(-?)(\d+)(?:\.(\d+))?[eE]([+-]?\d+)$/.exec(encoded);
  if (!match) return encoded;
  const [, sign, integer, fraction = '', exponentText] = match;
  const digits = integer + fraction;
  const decimal = integer.length + Number(exponentText);
  if (decimal <= 0) return `${sign}0.${'0'.repeat(-decimal)}${digits}`;
  if (decimal >= digits.length) return sign + digits + '0'.repeat(decimal - digits.length);
  return `${sign}${digits.slice(0, decimal)}.${digits.slice(decimal)}`;
}

/**
 * Instantiate and run a Core Wasm AOT image produced by `vo build --kind=wasm`.
 * Volang functions execute directly as Core Wasm. Only declared extern calls
 * cross into this host adapter.
 */
export async function runAot(
  image: BufferSource | WebAssembly.Module,
  argsOrOptions: readonly string[] | AotRunOptions = [],
): Promise<AotRunResult> {
  const options: AotRunOptions = Array.isArray(argsOrOptions)
    ? { args: argsOrOptions as readonly string[] }
    : argsOrOptions as AotRunOptions;
  const args = options.args ?? [];
  if (args.length > 1024 || args.some((value) => typeof value !== 'string')) {
    throw new Error('Volang AOT arguments exceed the host contract');
  }
  const argumentEncoder = new TextEncoder();
  let argumentBytes = 0;
  for (const argument of args) {
    if (argument.length > Math.floor(MAX_AOT_ARGUMENT_BYTES / 3)) {
      throw new Error('Volang AOT arguments exceed the host byte limit');
    }
    argumentBytes += argumentEncoder.encode(argument).byteLength;
    if (argumentBytes > MAX_AOT_ARGUMENT_BYTES) {
      throw new Error('Volang AOT arguments exceed the host byte limit');
    }
  }
  if (options.stdin instanceof Uint8Array
    && options.stdin.byteLength > MAX_AOT_STDIN_BYTES) {
    throw new Error('Volang AOT standard input exceeds 64 MiB');
  }
  if (typeof options.stdin === 'string'
    && options.stdin.length > Math.floor(MAX_AOT_STDIN_BYTES / 3)) {
    throw new Error('Volang AOT standard-input string exceeds its safe UTF-8 admission bound');
  }
  const stdin = typeof options.stdin === 'string'
    ? new TextEncoder().encode(options.stdin)
    : (options.stdin?.slice() ?? new Uint8Array());
  if (stdin.byteLength > MAX_AOT_STDIN_BYTES) {
    throw new Error('Volang AOT standard input exceeds 64 MiB');
  }
  let stdinOffset = 0;
  let module: WebAssembly.Module;
  if (image instanceof WebAssembly.Module) {
    module = image;
  } else {
    const byteLength = ArrayBuffer.isView(image) ? image.byteLength : image.byteLength;
    if (byteLength > MAX_AOT_IMAGE_BYTES) throw new Error('Volang AOT image exceeds size limit');
    module = await WebAssembly.compile(image);
  }
  validateAotShape(module);
  const manifest = parseAotManifest(module);
  const externDescriptors = parseAotExterns(module);
  const runtimeMetadata = parseAotRuntimeMetadata(module);
  const debugMetadata = parseAotDebugMetadata(module);
  const platformHost = new AotPlatformHost(vfs);
  const fmtScanHost = new AotFmtScanHost();
  const regexpHost = new AotRegexpHost();
  await regexpHost.initialize(externDescriptors, options.supportModule);
  await platformHost.initialize(externDescriptors);
  preflightAotExterns(externDescriptors, options.externs);
  const memoryLimitPages = options.memoryLimitPages ?? DEFAULT_AOT_MEMORY_LIMIT_PAGES;
  if (!Number.isInteger(memoryLimitPages)
    || memoryLimitPages < manifest.memoryPages
    || memoryLimitPages > 65_536) {
    throw new Error(`Volang AOT memory limit must be within ${manifest.memoryPages}..65536 pages`);
  }
  const memory = new WebAssembly.Memory({
    initial: manifest.memoryPages,
    maximum: memoryLimitPages,
  });
  let instance: WebAssembly.Instance | undefined;
  let stdout = '';
  let stderr = '';
  let requestedExitCode: number | undefined;
  interface AsyncExternState {
    settled: boolean;
    status: number;
    error?: unknown;
    readonly wake: Promise<void>;
  }
  const asyncExterns = new Map<string, AsyncExternState>();
  const wordMask = 0xffff_ffff_ffff_ffffn;
  const rotateLeft64 = (value: bigint, shift: bigint) => (
    ((value << shift) | (value >> (64n - shift))) & wordMask
  );
  const splitMix64 = (seed: bigint): bigint => {
    let value = seed & wordMask;
    value = ((value ^ (value >> 30n)) * 0xbf58_476d_1ce4_e5b9n) & wordMask;
    value = ((value ^ (value >> 27n)) * 0x94d0_49bb_1331_11ebn) & wordMask;
    return (value ^ (value >> 31n)) & wordMask;
  };
  const randomSeed = 0x6a09_e667_f3bc_c909n;
  let randomState0 = splitMix64(randomSeed);
  let randomState1 = splitMix64(randomSeed + 0x9e37_79b9_7f4a_7c15n);
  if (randomState0 === 0n && randomState1 === 0n) {
    randomState0 = 1n;
    randomState1 = 1n;
  }
  let randomReadValue = 0n;
  let randomReadPosition = 0;
  const nextRandomU64 = (): bigint => {
    const state0 = randomState0;
    let state1 = randomState1;
    const result = (rotateLeft64((state0 + state1) & wordMask, 17n) + state0) & wordMask;
    state1 ^= state0;
    randomState0 = (rotateLeft64(state0, 49n) ^ state1 ^ ((state1 << 21n) & wordMask))
      & wordMask;
    randomState1 = rotateLeft64(state1, 28n);
    return result;
  };
  const boundedRandom = (limit: bigint): bigint => {
    if ((limit & (limit - 1n)) === 0n) return nextRandomU64() & (limit - 1n);
    for (;;) {
      const product = nextRandomU64() * limit;
      const low = product & wordMask;
      const threshold = ((-limit) & wordMask) % limit;
      if (low >= threshold) return (product >> 64n) & wordMask;
    }
  };

  const view = () => new DataView(memory.buffer);
  const slotAddress = (frame: number, slot: number) => frame + slot * 8;
  const readSlot = (frame: number, slot: number) => view().getBigUint64(slotAddress(frame, slot), true);
  const writeSlot = (frame: number, slot: number, value: bigint) => {
    view().setBigUint64(slotAddress(frame, slot), BigInt.asUintN(64, value), true);
  };
  const readFloat64 = (frame: number, slot: number) => view().getFloat64(slotAddress(frame, slot), true);
  const writeFloat64 = (frame: number, slot: number, value: number) => {
    view().setFloat64(slotAddress(frame, slot), value, true);
  };
  const readString = (reference: bigint): string => {
    if (reference === 0n) return '';
    const header = Number(reference);
    const length = Number(view().getBigUint64(header, true));
    const pointer = Number(view().getBigUint64(header + 8, true));
    return new TextDecoder().decode(new Uint8Array(memory.buffer, pointer, length));
  };
  const readStringBytes = (reference: bigint): Uint8Array => {
    if (reference === 0n) return new Uint8Array();
    const header = Number(reference);
    const length = Number(view().getBigUint64(header, true));
    const pointer = Number(view().getBigUint64(header + 8, true));
    return new Uint8Array(memory.buffer, pointer, length);
  };
  const allocate = (bytes: number): number => {
    if (!instance) throw new Error('Volang allocator called before instantiation');
    if (!Number.isSafeInteger(bytes) || bytes < 1 || bytes > 0xffff_ffff) {
      throw new Error(`invalid Volang allocation size ${bytes}`);
    }
    const allocator = instance.exports[AOT_ALLOC_EXPORT];
    if (typeof allocator !== 'function') throw new Error('Volang AOT allocator export is missing');
    const pointer = allocator(bytes) as number;
    if (pointer === 0) throw new Error(`Volang allocation of ${bytes} bytes failed`);
    return pointer;
  };
  const allocateSequence = (bytes: number, elementMeta: number): number => {
    if (!instance) throw new Error('Volang sequence allocator called before instantiation');
    if (!Number.isSafeInteger(bytes) || bytes < 1 || bytes > 0xffff_ffff) {
      throw new Error(`invalid Volang sequence allocation size ${bytes}`);
    }
    if (!Number.isSafeInteger(elementMeta) || elementMeta < 0 || elementMeta > 0xffff_ffff) {
      throw new Error(`invalid Volang sequence element metadata ${elementMeta}`);
    }
    const allocator = instance.exports[AOT_SEQUENCE_ALLOC_EXPORT];
    if (typeof allocator !== 'function') {
      throw new Error('Volang AOT sequence allocator export is missing');
    }
    const pointer = allocator(bytes, elementMeta) as number;
    if (pointer === 0) {
      throw new Error(`Volang typed sequence allocation of ${bytes} bytes failed`);
    }
    return pointer;
  };
  const allocateTyped = (bytes: number, descriptor: number): number => {
    if (!instance) throw new Error('Volang typed allocator called before instantiation');
    if (!Number.isSafeInteger(bytes) || bytes < 1 || bytes > 0xffff_ffff
      || !Number.isInteger(descriptor) || descriptor < 0
      || descriptor >= runtimeMetadata.descriptorCount) {
      throw new Error('invalid Volang typed allocation request');
    }
    const allocator = instance.exports[AOT_TYPED_ALLOC_EXPORT];
    if (typeof allocator !== 'function') {
      throw new Error('Volang AOT typed allocator export is missing');
    }
    const pointer = allocator(bytes, descriptor) as number;
    if (pointer === 0) throw new Error(`Volang typed allocation of ${bytes} bytes failed`);
    return pointer;
  };
  const allocateStringBytes = (encoded: Uint8Array): bigint => {
    if (encoded.byteLength === 0) return 0n;
    // Guest-backed views are detached when allocation grows WebAssembly
    // memory. Stabilize them before calling into the allocator so conversions
    // remain correct exactly at a memory-growth boundary.
    const stable = encoded.buffer === memory.buffer ? encoded.slice() : encoded;
    const header = allocate(16 + stable.byteLength);
    const data = header + 16;
    view().setBigUint64(header, BigInt(stable.byteLength), true);
    view().setBigUint64(header + 8, BigInt(data), true);
    new Uint8Array(memory.buffer, data, stable.byteLength).set(stable);
    return BigInt(header);
  };
  const allocateString = (value: string): bigint => (
    allocateStringBytes(new TextEncoder().encode(value))
  );
  const decodeUtf8Rune = (bytes: Uint8Array, offset: number): readonly [number, number] => {
    const first = bytes[offset];
    if (first < 0x80) return [first, 1];
    const continuation = (index: number) => bytes[index] >= 0x80 && bytes[index] <= 0xbf;
    if (first >= 0xc2 && first <= 0xdf && offset + 1 < bytes.length
      && continuation(offset + 1)) {
      return [((first & 0x1f) << 6) | (bytes[offset + 1] & 0x3f), 2];
    }
    if (first >= 0xe0 && first <= 0xef && offset + 2 < bytes.length
      && continuation(offset + 1) && continuation(offset + 2)
      && (first !== 0xe0 || bytes[offset + 1] >= 0xa0)
      && (first !== 0xed || bytes[offset + 1] <= 0x9f)) {
      return [((first & 0x0f) << 12) | ((bytes[offset + 1] & 0x3f) << 6)
        | (bytes[offset + 2] & 0x3f), 3];
    }
    if (first >= 0xf0 && first <= 0xf4 && offset + 3 < bytes.length
      && continuation(offset + 1) && continuation(offset + 2) && continuation(offset + 3)
      && (first !== 0xf0 || bytes[offset + 1] >= 0x90)
      && (first !== 0xf4 || bytes[offset + 1] <= 0x8f)) {
      return [((first & 0x07) << 18) | ((bytes[offset + 1] & 0x3f) << 12)
        | ((bytes[offset + 2] & 0x3f) << 6) | (bytes[offset + 3] & 0x3f), 4];
    }
    return [0xfffd, 1];
  };
  const allocateSlice = (
    length: number,
    capacity: number,
    stride: number,
    elementMeta: number,
  ) => {
    const bytes = 32 + capacity * stride;
    if (!Number.isSafeInteger(bytes) || bytes < 32 || bytes > 0xffff_ffff) {
      throw new Error('Volang slice allocation size exceeds the wasm32 contract');
    }
    const header = allocateSequence(bytes, elementMeta);
    const data = header + 32;
    view().setBigUint64(header, BigInt(data), true);
    view().setBigUint64(header + 8, BigInt(length), true);
    view().setBigUint64(header + 16, BigInt(capacity), true);
    view().setBigUint64(header + 24, BigInt(stride), true);
    return { header, data };
  };
  const allocateStringBytesSlice = (encoded: readonly Uint8Array[]): bigint => {
    const values = encoded;
    const referencesBytes = values.length * 8;
    const stringBytes = encoded.reduce(
      (total, value) => total + ((16 + value.byteLength + 7) & ~7),
      0,
    );
    const totalBytes = 32 + referencesBytes + stringBytes;
    if (!Number.isSafeInteger(totalBytes) || totalBytes > 0xffff_ffff) {
      throw new Error('Volang argument string slice exceeds the wasm32 allocation contract');
    }
    // Keep the slice header, reference array, string headers, and UTF-8 bytes
    // in one allocation. The AOT GC can retain the entire immutable graph by
    // any interior reference without trusting host-authored type descriptors.
    const header = allocateSequence(totalBytes, 17);
    const data = header + 32;
    view().setBigUint64(header, BigInt(data), true);
    view().setBigUint64(header + 8, BigInt(values.length), true);
    view().setBigUint64(header + 16, BigInt(values.length), true);
    view().setBigUint64(header + 24, 8n, true);
    let cursor = data + referencesBytes;
    encoded.forEach((value, index) => {
      view().setBigUint64(data + index * 8, BigInt(cursor), true);
      view().setBigUint64(cursor, BigInt(value.byteLength), true);
      view().setBigUint64(cursor + 8, BigInt(cursor + 16), true);
      new Uint8Array(memory.buffer, cursor + 16, value.byteLength).set(value);
      cursor += (16 + value.byteLength + 7) & ~7;
    });
    return BigInt(header);
  };
  const allocateStringSlice = (values: readonly string[]): bigint => (
    allocateStringBytesSlice(values.map((value) => new TextEncoder().encode(value)))
  );
  const readStringSlice = (reference: bigint): readonly string[] => {
    if (reference === 0n) return [];
    const header = Number(reference);
    const data = Number(view().getBigUint64(header, true));
    const length = Number(view().getBigUint64(header + 8, true));
    const stride = Number(view().getBigUint64(header + 24, true));
    if (stride !== 8) throw new Error(`string slice has invalid stride ${stride}`);
    const decoder = new TextDecoder('utf-8', { fatal: true });
    const values: string[] = [];
    for (let index = 0; index < length; index += 1) {
      values.push(decoder.decode(readStringBytes(view().getBigUint64(data + index * 8, true))));
    }
    return values;
  };
  const allocateInterfaceSlice = (
    values: readonly (readonly [bigint, bigint])[],
  ): bigint => {
    const allocation = allocateSlice(values.length, values.length, 16, 16);
    values.forEach(([slot0, slot1], index) => {
      view().setBigUint64(allocation.data + index * 16, BigInt.asUintN(64, slot0), true);
      view().setBigUint64(
        allocation.data + index * 16 + 8,
        BigInt.asUintN(64, slot1),
        true,
      );
    });
    return BigInt(allocation.header);
  };
  const allocateIntSlice = (values: readonly bigint[]): bigint => {
    if (values.length === 0) return 0n;
    const allocation = allocateSlice(values.length, values.length, 8, 2);
    values.forEach((value, index) => {
      view().setBigUint64(allocation.data + index * 8, BigInt.asUintN(64, value), true);
    });
    return BigInt(allocation.header);
  };
  const allocateNamedStructSlice = (
    typeName: string,
    values: readonly Readonly<Record<string, bigint>>[],
  ): bigint => {
    const type = [...runtimeMetadata.types.values()].find((candidate) => (
      candidate.typeName === typeName && candidate.tag === 5
    ));
    if (type === undefined) {
      throw new Error(`Volang runtime metadata is missing named struct ${typeName}`);
    }
    const struct = runtimeMetadata.structs[type.first];
    if (struct === undefined || struct.slotCount !== type.slotCount) {
      throw new Error(`Volang named struct ${typeName} has invalid layout metadata`);
    }
    const allocation = allocateSlice(
      values.length,
      values.length,
      type.storageBytes,
      type.canonicalMeta,
    );
    new Uint8Array(memory.buffer, allocation.data, values.length * type.storageBytes).fill(0);
    values.forEach((record, index) => {
      const base = allocation.data + index * type.storageBytes;
      for (const [name, value] of Object.entries(record)) {
        const field = struct.fields.find((candidate) => candidate.name === name);
        if (field === undefined || field.slotCount !== 1) {
          throw new Error(`Volang named struct ${typeName} has no scalar field ${name}`);
        }
        view().setBigUint64(base + field.offset * 8, BigInt.asUintN(64, value), true);
      }
    });
    return BigInt(allocation.header);
  };
  const findBytes = (source: Uint8Array, pattern: Uint8Array, from = 0): number => {
    if (pattern.byteLength === 0) return Math.min(from, source.byteLength);
    const last = source.byteLength - pattern.byteLength;
    outer: for (let offset = from; offset <= last; offset += 1) {
      for (let index = 0; index < pattern.byteLength; index += 1) {
        if (source[offset + index] !== pattern[index]) continue outer;
      }
      return offset;
    }
    return -1;
  };
  const lastIndexBytes = (source: Uint8Array, pattern: Uint8Array): number => {
    if (pattern.byteLength === 0) return source.byteLength;
    outer: for (let offset = source.byteLength - pattern.byteLength; offset >= 0; offset -= 1) {
      for (let index = 0; index < pattern.byteLength; index += 1) {
        if (source[offset + index] !== pattern[index]) continue outer;
      }
      return offset;
    }
    return -1;
  };
  const runeCount = (source: Uint8Array): number => {
    let count = 0;
    for (let offset = 0; offset < source.byteLength; count += 1) {
      offset += decodeUtf8Rune(source, offset)[1];
    }
    return count;
  };
  const concatBytes = (parts: readonly Uint8Array[]): Uint8Array => {
    const length = parts.reduce((total, part) => total + part.byteLength, 0);
    if (!Number.isSafeInteger(length)) throw new Error('Volang string result exceeds host limits');
    const result = new Uint8Array(length);
    let offset = 0;
    for (const part of parts) {
      result.set(part, offset);
      offset += part.byteLength;
    }
    return result;
  };
  const mappedStringBytes = (
    source: Uint8Array,
    mapping: readonly UnicodeRange[],
  ): Uint8Array => {
    const encoder = new TextEncoder();
    const parts: Uint8Array[] = [];
    for (let offset = 0; offset < source.byteLength;) {
      const [rune, width] = decodeUtf8Rune(source, offset);
      parts.push(encoder.encode(String.fromCodePoint(unicodeMap(rune, mapping))));
      offset += width;
    }
    return concatBytes(parts);
  };
  const splitStringBytes = (
    source: Uint8Array,
    separator: Uint8Array,
    keepSeparator: boolean,
    limit: bigint,
  ): Uint8Array[] => {
    if (limit === 0n) return [];
    if (separator.byteLength === 0) {
      const count = runeCount(source);
      const partLimit = limit < 0n ? count : Math.min(count, Number(
        limit > BigInt(Number.MAX_SAFE_INTEGER) ? BigInt(Number.MAX_SAFE_INTEGER) : limit,
      ));
      if (partLimit === 0) return [];
      const parts: Uint8Array[] = [];
      let offset = 0;
      for (let index = 0; index + 1 < partLimit; index += 1) {
        const width = decodeUtf8Rune(source, offset)[1];
        parts.push(source.slice(offset, offset + width));
        offset += width;
      }
      parts.push(source.slice(offset));
      return parts;
    }
    const parts: Uint8Array[] = [];
    let start = 0;
    while (limit < 0n || BigInt(parts.length + 1) < limit) {
      const found = findBytes(source, separator, start);
      if (found < 0) break;
      const next = found + separator.byteLength;
      parts.push(source.slice(start, keepSeparator ? next : found));
      start = next;
    }
    parts.push(source.slice(start));
    return parts;
  };
  const replaceStringBytes = (
    source: Uint8Array,
    old: Uint8Array,
    replacement: Uint8Array,
    limit: bigint,
  ): Uint8Array => {
    if (limit === 0n) return source.slice();
    const parts: Uint8Array[] = [];
    if (old.byteLength === 0) {
      parts.push(replacement);
      let replacements = 1n;
      let offset = 0;
      while (offset < source.byteLength && (limit < 0n || replacements < limit)) {
        const width = decodeUtf8Rune(source, offset)[1];
        parts.push(source.slice(offset, offset + width), replacement);
        offset += width;
        replacements += 1n;
      }
      parts.push(source.slice(offset));
      return concatBytes(parts);
    }
    let start = 0;
    let replacements = 0n;
    while (limit < 0n || replacements < limit) {
      const found = findBytes(source, old, start);
      if (found < 0) break;
      parts.push(source.slice(start, found), replacement);
      start = found + old.byteLength;
      replacements += 1n;
    }
    parts.push(source.slice(start));
    return concatBytes(parts);
  };
  const equalFoldBytes = (left: Uint8Array, right: Uint8Array): boolean => {
    let leftOffset = 0;
    let rightOffset = 0;
    while (leftOffset < left.byteLength && rightOffset < right.byteLength) {
      const [leftRune, leftWidth] = decodeUtf8Rune(left, leftOffset);
      const [rightRune, rightWidth] = decodeUtf8Rune(right, rightOffset);
      if (leftRune !== rightRune) {
        let folded = unicodeMap(leftRune, SIMPLE_FOLD);
        let equivalent = false;
        while (folded !== leftRune) {
          if (folded === rightRune) {
            equivalent = true;
            break;
          }
          folded = unicodeMap(folded, SIMPLE_FOLD);
        }
        if (!equivalent) return false;
      }
      leftOffset += leftWidth;
      rightOffset += rightWidth;
    }
    return leftOffset === left.byteLength && rightOffset === right.byteLength;
  };
  const formatError = (reference: bigint): string => {
    if (reference === 0n) return '<nil>';
    const parts: string[] = [];
    const seen = new Set<number>();
    let pointer = Number(reference);
    for (let depth = 0; pointer !== 0 && depth < 10_000; depth += 1) {
      if (seen.has(pointer)) {
        parts.push('<cycle>');
        break;
      }
      seen.add(pointer);
      const message = view().getBigUint64(
        pointer + runtimeMetadata.errorMessageOffset * 8,
        true,
      );
      parts.push(readString(message));
      const causeAddress = pointer + runtimeMetadata.errorCauseOffset * 8;
      const cause0 = view().getBigUint64(causeAddress, true);
      const cause1 = view().getBigUint64(causeAddress + 8, true);
      if (cause0 === 0n) break;
      if (runtimeMetadata.errorValueRaw !== undefined
        && Number(cause0 & 0xffff_ffffn) === runtimeMetadata.errorValueRaw
        && cause1 !== 0n) {
        pointer = Number(cause1);
        continue;
      }
      parts.push(formatInterface(cause0, cause1));
      break;
    }
    return parts.join(': ');
  };
  const formatInterface = (slot0: bigint, slot1: bigint): string => {
    const kind = Number(slot0 & 0xffn);
    switch (kind) {
      case 0: return '<nil>';
      case 1: return slot1 === 0n ? 'false' : 'true';
      case 2:
      case 6: return BigInt.asIntN(64, slot1).toString();
      case 3: return BigInt.asIntN(8, slot1).toString();
      case 4: return BigInt.asIntN(16, slot1).toString();
      case 5: return BigInt.asIntN(32, slot1).toString();
      case 7:
      case 11: return slot1.toString();
      case 8: return BigInt.asUintN(8, slot1).toString();
      case 9: return BigInt.asUintN(16, slot1).toString();
      case 10: return BigInt.asUintN(32, slot1).toString();
      case 12: {
        const scratch = new DataView(new ArrayBuffer(4));
        scratch.setUint32(0, Number(slot1 & 0xffff_ffffn), true);
        return formatVoFloat(scratch.getFloat32(0, true));
      }
      case 13: {
        const scratch = new DataView(new ArrayBuffer(8));
        scratch.setBigUint64(0, slot1, true);
        return formatVoFloat(scratch.getFloat64(0, true));
      }
      case 17: return readString(slot1);
      case 18: return '[...]';
      case 19: return 'map[...]';
      case 20:
      case 21:
      case 22:
        if (runtimeMetadata.errorValueRaw !== undefined
          && Number(slot0 & 0xffff_ffffn) === runtimeMetadata.errorValueRaw) {
          return formatError(slot1);
        }
        return `0x${slot1.toString(16)}`;
      case 23:
      case 24: return `0x${slot1.toString(16)}`;
      default: return `<value-kind:${kind}>`;
    }
  };
  const integerKinds = new Set([2, 3, 4, 5, 6, 7, 8, 9, 10, 11]);
  const signedIntegerKinds = new Set([2, 3, 4, 5, 6]);
  const valueKindName = (kind: number): string => [
    '<nil>', 'bool', 'int', 'int8', 'int16', 'int32', 'int64', 'uint', 'uint8',
    'uint16', 'uint32', 'uint64', 'float32', 'float64', '[...]...', 'struct{...}',
    'interface{}', 'string', '[]...', 'map[...]...', 'chan ...', 'func(...)', '*...',
    'port ...', 'island',
  ][kind] ?? `<value-kind:${kind}>`;
  const normalizedInteger = (kind: number, value: bigint): bigint => {
    switch (kind) {
      case 3: return BigInt.asIntN(8, value);
      case 4: return BigInt.asIntN(16, value);
      case 5: return BigInt.asIntN(32, value);
      case 2:
      case 6: return BigInt.asIntN(64, value);
      case 8: return BigInt.asUintN(8, value);
      case 9: return BigInt.asUintN(16, value);
      case 10: return BigInt.asUintN(32, value);
      default: return BigInt.asUintN(64, value);
    }
  };
  const integerBits = (kind: number, value: bigint): bigint => {
    switch (kind) {
      case 3: return BigInt.asUintN(64, BigInt.asIntN(8, value));
      case 4: return BigInt.asUintN(64, BigInt.asIntN(16, value));
      case 5: return BigInt.asUintN(64, BigInt.asIntN(32, value));
      case 8: return BigInt.asUintN(8, value);
      case 9: return BigInt.asUintN(16, value);
      case 10: return BigInt.asUintN(32, value);
      default: return BigInt.asUintN(64, value);
    }
  };
  const padText = (
    value: string,
    width: number | undefined,
    left: boolean,
    zero: boolean,
    numeric: boolean,
  ): string => {
    if (width === undefined || [...value].length >= width) return value;
    const count = width - [...value].length;
    if (left) return value + ' '.repeat(count);
    if (zero && numeric && /^[+\- ]/.test(value)) {
      return value[0] + '0'.repeat(count) + value.slice(1);
    }
    return (zero ? '0' : ' ').repeat(count) + value;
  };
  const quoteString = (value: string): string => JSON.stringify(value)
    .replace(/\\u0008/g, '\\b')
    .replace(/\\u000c/g, '\\f');
  const formatSprintf = (formatReference: bigint, argsReference: bigint): bigint => {
    const format = readString(formatReference);
    const argsHeader = Number(argsReference);
    const argsLength = argsHeader === 0 ? 0 : Number(view().getBigUint64(argsHeader + 8, true));
    const argsData = argsHeader === 0 ? 0 : Number(view().getBigUint64(argsHeader, true));
    const argsStride = argsHeader === 0 ? 16 : Number(view().getBigUint64(argsHeader + 24, true));
    if (argsHeader !== 0 && argsStride < 16) throw new Error('invalid fmt []interface{} stride');
    let output = '';
    let offset = 0;
    let argumentIndex = 0;
    while (offset < format.length) {
      const character = format[offset++];
      if (character !== '%') {
        output += character;
        continue;
      }
      if (format[offset] === '%') {
        output += '%';
        offset += 1;
        continue;
      }
      const flags = { left: false, plus: false, zero: false, hash: false, space: false };
      while (offset < format.length && '-+0# '.includes(format[offset])) {
        const flag = format[offset++];
        if (flag === '-') flags.left = true;
        else if (flag === '+') flags.plus = true;
        else if (flag === '0') flags.zero = true;
        else if (flag === '#') flags.hash = true;
        else flags.space = true;
      }
      const widthStart = offset;
      while (offset < format.length && /[0-9]/.test(format[offset])) offset += 1;
      const widthText = format.slice(widthStart, offset);
      const widthValue = widthText === '' ? undefined : Number(widthText);
      const badWidth = widthValue !== undefined && widthValue > 1_000_000;
      const width = badWidth ? undefined : widthValue;
      let precision: number | undefined;
      let badPrecision = false;
      if (format[offset] === '.') {
        offset += 1;
        const precisionStart = offset;
        while (offset < format.length && /[0-9]/.test(format[offset])) offset += 1;
        precision = precisionStart === offset ? 0 : Number(format.slice(precisionStart, offset));
        if (precision > 1_000_000) {
          precision = undefined;
          badPrecision = true;
        }
      }
      if (offset >= format.length) {
        output += '%!(NOVERB)';
        break;
      }
      const verb = format[offset++];
      if (badWidth) output += '%!(BADWIDTH)';
      if (badPrecision) output += '%!(BADPREC)';
      if (argumentIndex >= argsLength) {
        output += `%!${verb}(MISSING)`;
        continue;
      }
      const element = argsData + argumentIndex * argsStride;
      const slot0 = view().getBigUint64(element, true);
      const slot1 = view().getBigUint64(element + 8, true);
      const kind = Number(slot0 & 0xffn);
      const diagnostic = () => formatInterface(slot0, slot1);
      let value: string;
      if ('dbBoOxX'.includes(verb) && integerKinds.has(kind)) {
        const integer = normalizedInteger(kind, slot1);
        const negative = signedIntegerKinds.has(kind) && integer < 0n;
        const magnitude = negative ? -integer : integer;
        const radix = verb === 'b' ? 2 : (verb === 'o' || verb === 'O' ? 8 : (verb === 'd' ? 10 : 16));
        let digits = magnitude.toString(radix);
        if (verb === 'X') digits = digits.toUpperCase();
        if (precision === 0 && magnitude === 0n) digits = '';
        else if (precision !== undefined) digits = digits.padStart(precision, '0');
        let sign = negative ? '-' : (flags.plus ? '+' : (flags.space ? ' ' : ''));
        if (digits === '') sign = '';
        let prefix = '';
        if (digits !== '' && verb === 'O') prefix = '0o';
        else if (digits !== '' && flags.hash && verb === 'b') prefix = '0b';
        else if (digits !== '' && flags.hash && verb === 'o' && !digits.startsWith('0')) prefix = '0';
        else if (digits !== '' && flags.hash && (verb === 'x' || verb === 'X')) {
          prefix = verb === 'X' ? '0X' : '0x';
        }
        value = sign + prefix + digits;
        const padding = Math.max(0, (width ?? 0) - value.length);
        if (flags.left) value += ' '.repeat(padding);
        else if (flags.zero && precision === undefined) value = sign + prefix + '0'.repeat(padding) + digits;
        else value = ' '.repeat(padding) + value;
      } else if (verb === 't' && kind === 1) {
        value = padText(slot1 === 0n ? 'false' : 'true', width, flags.left, flags.zero, false);
      } else if ((verb === 's' || verb === 'q') && kind === 17) {
        let stringValue = readString(slot1);
        if (precision !== undefined) stringValue = [...stringValue].slice(0, precision).join('');
        value = verb === 'q' ? quoteString(stringValue) : stringValue;
        value = padText(value, width, flags.left, flags.zero, false);
      } else if ((verb === 'x' || verb === 'X') && kind === 17) {
        let bytes = readStringBytes(slot1);
        if (precision !== undefined) bytes = bytes.subarray(0, precision);
        const separator = flags.space ? ' ' : '';
        const bytePrefix = flags.hash && flags.space ? (verb === 'X' ? '0X' : '0x') : '';
        value = [...bytes].map((byte) => bytePrefix + byte.toString(16).padStart(2, '0'))
          .join(separator);
        if (verb === 'X') value = value.toUpperCase();
        if (flags.hash && !flags.space && bytes.length !== 0) value = (verb === 'X' ? '0X' : '0x') + value;
        value = padText(value, width, flags.left, flags.zero, false);
      } else if ('fFeEgG'.includes(verb) && (kind === 12 || kind === 13)) {
        const number = kind === 12
          ? (() => {
            const scratch = new DataView(new ArrayBuffer(4));
            scratch.setUint32(0, Number(slot1 & 0xffff_ffffn), true);
            return scratch.getFloat32(0, true);
          })()
          : float64FromBits(slot1);
        const digits = precision ?? 6;
        if (verb === 'f' || verb === 'F') value = number.toFixed(Math.min(digits, 100));
        else if (verb === 'e' || verb === 'E') value = number.toExponential(Math.min(digits, 100));
        else value = number.toPrecision(Math.min(Math.max(1, digits), 100)).replace(/(?:\.0+|(?:(\.\d*?)0+))(?=e|$)/, '$1');
        if (verb === verb.toUpperCase()) value = value.toUpperCase();
        value = value.replace(/([eE])([+\-]?)(\d+)$/, (_match, e, sign, exponent) => (
          `${e}${sign || '+'}${String(exponent).padStart(2, '0')}`
        ));
        if (!value.startsWith('-') && flags.plus) value = `+${value}`;
        else if (!value.startsWith('-') && flags.space) value = ` ${value}`;
        value = padText(value, width, flags.left, flags.zero, true);
      } else if (verb === 'c' && integerKinds.has(kind)) {
        const integer = normalizedInteger(kind, slot1);
        const codePoint = integer >= 0n && integer <= 0x10ffffn ? Number(integer) : 0xfffd;
        value = padText(String.fromCodePoint(
          codePoint >= 0xd800 && codePoint <= 0xdfff ? 0xfffd : codePoint,
        ), width, flags.left, flags.zero, false);
      } else if (verb === 'U' && integerKinds.has(kind)) {
        const integer = integerBits(kind, slot1);
        const minimumDigits = Math.max(4, precision ?? 4);
        let representation = `U+${integer.toString(16).toUpperCase().padStart(minimumDigits, '0')}`;
        if (flags.hash && integer <= 0x10ffffn && unicodeIsPrint(Number(integer))) {
          representation += ` '${String.fromCodePoint(Number(integer))}'`;
        }
        value = padText(representation, width, flags.left, false, false);
      } else if (verb === 'p') {
        value = padText(`0x${slot1.toString(16)}`, width, flags.left, flags.zero, false);
      } else if (verb === 'T') {
        value = padText(valueKindName(kind), width, flags.left, flags.zero, false);
      } else if (verb === 'v' || (verb === 's' && kind !== 17)) {
        value = diagnostic();
        if (precision !== undefined) value = [...value].slice(0, precision).join('');
        value = padText(value, width, flags.left, flags.zero, false);
      } else {
        value = `%!${verb}(${diagnostic()})`;
      }
      output += value;
      argumentIndex += 1;
    }
    if (argumentIndex < argsLength) {
      output += '%!(EXTRA ';
      for (let index = argumentIndex; index < argsLength; index += 1) {
        if (index > argumentIndex) output += ', ';
        const element = argsData + index * argsStride;
        const slot0 = view().getBigUint64(element, true);
        const slot1 = view().getBigUint64(element + 8, true);
        const kind = Number(slot0 & 0xffn);
        output += kind === 0 ? '<nil>' : `${valueKindName(kind)}=${formatInterface(slot0, slot1)}`;
      }
      output += ')';
    }
    return allocateString(output);
  };
  let activeJsonRoot: { frame: number; destination: number } | undefined;
  const structuredOperations: AotStructuredJsonOperations = {
    metadata: runtimeMetadata,
    memory: () => memory,
    view,
    allocateTyped,
    allocateSequence,
    allocateStringBytes,
    lowerSimple: (codePoint) => unicodeMap(codePoint, SIMPLE_LOWER),
    formatFloat: (value, bitSize) => new TextDecoder('ascii', { fatal: true }).decode(
      formatVolangFloat(value, 0x67, -1n, BigInt(bitSize)),
    ),
    setReturnRoot: (slot0, slot1) => {
      if (activeJsonRoot === undefined) throw new Error('Volang JSON return root is inactive');
      writeSlot(activeJsonRoot.frame, activeJsonRoot.destination, slot0);
      writeSlot(activeJsonRoot.frame, activeJsonRoot.destination + 1, slot1);
    },
    clearReturnRoot: () => {
      if (activeJsonRoot === undefined) throw new Error('Volang JSON return root is inactive');
      writeSlot(activeJsonRoot.frame, activeJsonRoot.destination, 0n);
      writeSlot(activeJsonRoot.frame, activeJsonRoot.destination + 1, 0n);
    },
  };
  const structuredJson = new AotStructuredJsonHost(structuredOperations);
  const structuredToml = new AotStructuredJsonHost(structuredOperations, 'toml');
  const writeGuestError = (
    frame: number,
    destination: number,
    message: string,
    cause?: readonly [bigint, bigint],
  ): void => {
    const {
      errorValueRaw,
      errorDescriptor,
      errorSlots,
      errorMessageOffset,
      errorCauseOffset,
    } = runtimeMetadata;
    if (errorValueRaw === undefined || errorDescriptor === undefined || errorSlots === 0) {
      throw new Error('Volang runtime error allocation metadata is unavailable');
    }
    const reference = allocateTyped(errorSlots * 8, errorDescriptor);
    new Uint8Array(memory.buffer, reference, errorSlots * 8).fill(0);
    writeSlot(frame, destination, BigInt(errorValueRaw));
    writeSlot(frame, destination + 1, BigInt(reference));
    const messageReference = allocateString(message);
    view().setBigUint64(reference + errorMessageOffset * 8, messageReference, true);
    if (cause !== undefined) {
      view().setBigUint64(reference + errorCauseOffset * 8, cause[0], true);
      view().setBigUint64(reference + (errorCauseOffset + 1) * 8, cause[1], true);
    }
  };
  const readByteSlice = (reference: bigint): Uint8Array => {
    if (reference === 0n) return new Uint8Array();
    const header = Number(reference);
    const data = Number(view().getBigUint64(header, true));
    const length = Number(view().getBigUint64(header + 8, true));
    const stride = Number(view().getBigUint64(header + 24, true));
    if (stride !== 1) throw new Error(`byte slice has invalid stride ${stride}`);
    return new Uint8Array(memory.buffer, data, length);
  };
  const allocateByteSlice = (bytes: Uint8Array): bigint => {
    const allocation = allocateSlice(
      bytes.byteLength,
      bytes.byteLength,
      1,
      structuredJson.byteElementMeta(),
    );
    new Uint8Array(memory.buffer, allocation.data, bytes.byteLength).set(bytes);
    return BigInt(allocation.header);
  };
  const sleepNanoseconds = async (duration: bigint): Promise<void> => {
    if (duration <= 0n) return;
    let remainingMilliseconds = (duration + 999_999n) / 1_000_000n;
    const maxTimerMilliseconds = 2_147_483_647n;
    while (remainingMilliseconds > 0n) {
      const chunk = remainingMilliseconds > maxTimerMilliseconds
        ? maxTimerMilliseconds : remainingMilliseconds;
      await new Promise<void>((resolvePromise) => {
        setTimeout(resolvePromise, Number(chunk));
      });
      remainingMilliseconds -= chunk;
    }
  };

  const imports = {
    [AOT_RUNTIME_MODULE]: {
      [AOT_MEMORY_EXPORT]: memory,
      [AOT_RUNTIME_FUNCTION]: (
        externId: number,
        frame: number,
        destination: number,
        argumentsStart: number,
        argumentSlots: number,
      ): number => {
        const descriptor = externDescriptors[externId];
        if (descriptor === undefined) throw new Error(`missing Volang extern ${externId}`);
        const { name } = descriptor;
        if (descriptor.paramSlots !== undefined && descriptor.paramSlots !== argumentSlots) {
          throw new Error(
            `Volang extern ${name} received ${argumentSlots} slots; expected ${descriptor.paramSlots}`,
          );
        }
        const call: AotExternCall = {
          descriptor,
          name,
          externId,
          memory,
          frame,
          destination,
          argumentsStart,
          argumentSlots,
          args,
          readSlot: (slot) => readSlot(frame, slot),
          writeSlot: (slot, value) => writeSlot(frame, slot, value),
          readFloat64: (slot) => readFloat64(frame, slot),
          writeFloat64: (slot, value) => writeFloat64(frame, slot, value),
          readString,
          readStringBytes: (reference) => readStringBytes(reference).slice(),
          readStringSlice,
          readByteSlice: (reference) => readByteSlice(reference).slice(),
          writeByteSlice: (reference, bytes) => {
            const destinationBytes = readByteSlice(reference);
            const count = Math.min(destinationBytes.byteLength, bytes.byteLength);
            destinationBytes.set(bytes.subarray(0, count));
            return count;
          },
          allocate,
          allocateSequence,
          allocateString,
          allocateStringBytes,
          allocateStringSlice,
          allocateStringBytesSlice,
          allocateByteSlice,
          allocateIntSlice,
          allocateInterfaceSlice,
          allocateNamedStructSlice,
          writeError: (slot, message, cause) => writeGuestError(frame, slot, message, cause),
          clearError: (slot) => {
            writeSlot(frame, slot, 0n);
            writeSlot(frame, slot + 1, 0n);
          },
          writeOutput: (fd, bytes) => {
            const output = new TextDecoder().decode(bytes);
            if (fd === 2) stderr += output;
            else stdout += output;
          },
          exit: (code) => {
            if (!Number.isInteger(code) || code < -0x8000_0000 || code > 0x7fff_ffff) {
              throw new Error(`Volang exit code ${code} is outside the signed 32-bit domain`);
            }
            requestedExitCode = code;
            return 5;
          },
          panic: (message) => {
            if (!instance) throw new Error('Volang panic raised before instantiation');
            const raise = instance.exports[AOT_RAISE_HOST_PANIC_EXPORT];
            if (typeof raise !== 'function') {
              throw new Error('Volang AOT host-panic export is missing');
            }
            const messageReference = allocateString(message);
            return raise(frame, Number(messageReference)) as number;
          },
        };
        const asyncKey = `${externId}:${frame}:${destination}:${argumentsStart}:${argumentSlots}`;
        const executeHandler = (handler: AotExternHandler): number => {
          const replay = asyncExterns.get(asyncKey);
          if (replay) {
            if (!replay.settled) return 5;
            asyncExterns.delete(asyncKey);
            if (replay.error !== undefined) throw replay.error;
            return replay.status;
          }
          const outcome = handler(call);
          if (!(outcome instanceof Promise)) return outcome ?? 0;
          const replayEffects = (1n << 2n) | (1n << 3n) | (1n << 4n);
          if ((descriptor.effectiveEffects & replayEffects) === 0n) {
            throw new Error(`Volang extern ${name} returned a Promise without a replay effect`);
          }
          let resolveWake: (() => void) | undefined;
          const state: AsyncExternState = {
            settled: false,
            status: 0,
            wake: new Promise<void>((resolvePromise) => { resolveWake = resolvePromise; }),
          };
          asyncExterns.set(asyncKey, state);
          void outcome.then(
            (status) => {
              state.status = status ?? 0;
              state.settled = true;
              resolveWake?.();
            },
            (error) => {
              state.error = error;
              state.settled = true;
              resolveWake?.();
            },
          );
          return 5;
        };
        const custom = externProvider(options.externs?.[name]);
        if (custom) return executeHandler(custom.handler);
        if (platformHost.supports(descriptor)) {
          return executeHandler((platformCall) => platformHost.handle(platformCall));
        }
        if (fmtScanHost.supports(descriptor)) return fmtScanHost.handle(call) ?? 0;
        if (regexpHost.supports(descriptor)) return regexpHost.handle(call) ?? 0;
        if ((descriptor.source === 0 || descriptor.source === 1)
          && name === canonicalExternName('runtime', 'Caller')) {
          const skip = BigInt.asIntN(64, readSlot(frame, argumentsStart));
          let callerFrame = frame;
          let remaining = skip;
          let location: AotDebugLocation | undefined;
          let functionId = 0;
          let pc = 0;
          while (remaining >= 0n && callerFrame !== 0) {
            const rawFrame = callerFrame - debugMetadata.frameStateBytes;
            if (rawFrame < 0
              || rawFrame + debugMetadata.frameStateBytes > memory.buffer.byteLength) {
              callerFrame = 0;
              break;
            }
            functionId = view().getUint32(
              rawFrame + debugMetadata.frameFunctionIdOffset,
              true,
            );
            pc = view().getUint32(rawFrame + debugMetadata.frameDebugPcOffset, true);
            if (remaining === 0n) {
              location = debugMetadata.functions[functionId]?.get(pc);
              break;
            }
            callerFrame = view().getUint32(rawFrame + debugMetadata.frameParentOffset, true);
            remaining -= 1n;
          }
          if (skip < 0n || location === undefined) {
            writeSlot(frame, destination, 0n);
            writeSlot(frame, destination + 1, 0n);
            writeSlot(frame, destination + 2, 0n);
            writeSlot(frame, destination + 3, 0n);
          } else {
            const logicalPc = (BigInt(functionId) + 1n) << 32n | BigInt(pc);
            writeSlot(frame, destination, logicalPc);
            writeSlot(frame, destination + 1, allocateString(location.file));
            writeSlot(frame, destination + 2, BigInt(location.line));
            writeSlot(frame, destination + 3, 1n);
          }
          return 0;
        }
        if (isStdlibExtern(descriptor, 'encoding/json', 'marshalAny')) {
          try {
            const encoded = structuredJson.marshal(
              readSlot(frame, argumentsStart),
              readSlot(frame, argumentsStart + 1),
            );
            writeSlot(frame, destination, allocateByteSlice(encoded));
            writeSlot(frame, destination + 1, 0n);
            writeSlot(frame, destination + 2, 0n);
          } catch (error) {
            if (!(error instanceof AotJsonError)) throw error;
            writeSlot(frame, destination, 0n);
            writeGuestError(frame, destination + 1, error.message);
          }
          return 0;
        }
        if (isStdlibExtern(descriptor, 'encoding/json', 'unmarshalAny')) {
          const encoded = readByteSlice(readSlot(frame, argumentsStart)).slice();
          activeJsonRoot = { frame, destination };
          try {
            structuredJson.unmarshal(
              encoded,
              readSlot(frame, argumentsStart + 1),
              readSlot(frame, argumentsStart + 2),
            );
          } catch (error) {
            if (!(error instanceof AotJsonError)) throw error;
            writeGuestError(frame, destination, error.message);
          } finally {
            activeJsonRoot = undefined;
          }
          return 0;
        }
        if (isStdlibExtern(descriptor, 'encoding/toml', 'marshalAny')) {
          try {
            const encoded = structuredToml.marshal(
              readSlot(frame, argumentsStart),
              readSlot(frame, argumentsStart + 1),
            );
            writeSlot(frame, destination, allocateByteSlice(encoded));
            writeSlot(frame, destination + 1, 0n);
            writeSlot(frame, destination + 2, 0n);
          } catch (error) {
            if (!(error instanceof AotJsonError)) throw error;
            writeSlot(frame, destination, 0n);
            writeGuestError(frame, destination + 1, error.message);
          }
          return 0;
        }
        if (isStdlibExtern(descriptor, 'encoding/toml', 'unmarshalAny')) {
          const encoded = readByteSlice(readSlot(frame, argumentsStart)).slice();
          activeJsonRoot = { frame, destination };
          try {
            structuredToml.unmarshal(
              encoded,
              readSlot(frame, argumentsStart + 1),
              readSlot(frame, argumentsStart + 2),
            );
          } catch (error) {
            if (!(error instanceof AotJsonError)) throw error;
            writeGuestError(frame, destination, error.message);
          } finally {
            activeJsonRoot = undefined;
          }
          return 0;
        }
        if (name === 'vo_print' || name === 'vo_println') {
          const fields: string[] = [];
          for (let slot = 0; slot + 1 < argumentSlots; slot += 2) {
            fields.push(formatInterface(readSlot(frame, argumentsStart + slot), readSlot(frame, argumentsStart + slot + 1)));
          }
          stdout += fields.join(' ');
          if (name === 'vo_println') stdout += '\n';
          return 0;
        }
        if (isStdlibExtern(descriptor, 'fmt', 'nativeReadLine')) {
          if (stdinOffset >= stdin.byteLength) {
            writeSlot(frame, destination, allocateStringBytes(new Uint8Array()));
            platformHost.writeIoError(call, destination + 1, 'EOF');
            return 0;
          }
          const relativeNewline = stdin.subarray(stdinOffset).indexOf(0x0a);
          const end = relativeNewline < 0 ? stdin.byteLength : stdinOffset + relativeNewline;
          let contentEnd = end;
          if (contentEnd > stdinOffset && stdin[contentEnd - 1] === 0x0d) contentEnd -= 1;
          writeSlot(
            frame,
            destination,
            allocateStringBytes(stdin.subarray(stdinOffset, contentEnd)),
          );
          stdinOffset = relativeNewline < 0 ? stdin.byteLength : end + 1;
          writeSlot(frame, destination + 1, 0n);
          writeSlot(frame, destination + 2, 0n);
          return 0;
        }
        const formatSlice = (reference: bigint, newline: boolean): string => {
          if (reference === 0n) return newline ? '\n' : '';
          const header = Number(reference);
          const length = Number(view().getBigUint64(header + 8, true));
          const data = Number(view().getBigUint64(header, true));
          const stride = Number(view().getBigUint64(header + 24, true));
          if (stride < 16) throw new Error(`invalid []interface{} stride ${stride}`);
          let formatted = '';
          let previousKind: number | undefined;
          for (let index = 0; index < length; index += 1) {
            const element = data + index * stride;
            const slot0 = view().getBigUint64(element, true);
            const kind = Number(slot0 & 0xffn);
            if (index > 0 && (newline || (previousKind !== 17 && kind !== 17))) {
              formatted += ' ';
            }
            formatted += formatInterface(slot0, view().getBigUint64(element + 8, true));
            previousKind = kind;
          }
          return formatted + (newline ? '\n' : '');
        };
        if (isStdlibExtern(descriptor, 'fmt', 'nativeWrite')) {
          const text = readString(readSlot(frame, argumentsStart));
          stdout += text;
          return 0;
        }
        if (isStdlibExtern(descriptor, 'fmt', 'nativeSprintln')) {
          writeSlot(frame, destination, allocateString(formatSlice(
            readSlot(frame, argumentsStart),
            true,
          )));
          return 0;
        }
        if (isStdlibExtern(descriptor, 'fmt', 'nativeSprint')) {
          writeSlot(frame, destination, allocateString(formatSlice(
            readSlot(frame, argumentsStart),
            false,
          )));
          return 0;
        }
        if (isStdlibExtern(descriptor, 'fmt', 'nativeSprintf')) {
          writeSlot(frame, destination, formatSprintf(
            readSlot(frame, argumentsStart),
            readSlot(frame, argumentsStart + 1),
          ));
          return 0;
        }
        if (name === 'vo_conv_int_str') {
          const raw = readSlot(frame, argumentsStart);
          const codePoint = raw <= 0xffff_ffffn ? Number(raw) : 0xfffd;
          const valid = codePoint <= 0x10ffff && !(codePoint >= 0xd800 && codePoint <= 0xdfff);
          writeSlot(frame, destination, allocateString(String.fromCodePoint(valid ? codePoint : 0xfffd)));
          return 0;
        }
        if (name === 'vo_conv_str_bytes') {
          // allocateSlice may grow WebAssembly memory and detach every guest
          // view. Copy the source before allocating the destination so a
          // conversion remains valid at the exact growth boundary.
          const encoded = readStringBytes(readSlot(frame, argumentsStart)).slice();
          const { header, data } = allocateSlice(encoded.byteLength, encoded.byteLength, 1, 8);
          new Uint8Array(memory.buffer, data, encoded.byteLength).set(encoded);
          writeSlot(frame, destination, BigInt(header));
          return 0;
        }
        if (name === 'vo_conv_bytes_str') {
          const source = Number(readSlot(frame, argumentsStart));
          if (source === 0) {
            writeSlot(frame, destination, 0n);
            return 0;
          }
          const data = Number(view().getBigUint64(source, true));
          const length = Number(view().getBigUint64(source + 8, true));
          const stride = Number(view().getBigUint64(source + 24, true));
          if (stride !== 1) throw new Error(`byte slice has invalid stride ${stride}`);
          writeSlot(frame, destination, allocateStringBytes(
            new Uint8Array(memory.buffer, data, length),
          ));
          return 0;
        }
        if (name === 'vo_conv_str_runes') {
          const encoded = readStringBytes(readSlot(frame, argumentsStart));
          const runes: number[] = [];
          for (let offset = 0; offset < encoded.byteLength;) {
            const [rune, width] = decodeUtf8Rune(encoded, offset);
            runes.push(rune);
            offset += width;
          }
          const { header, data } = allocateSlice(runes.length, runes.length, 4, 5);
          runes.forEach((rune, index) => view().setUint32(data + index * 4, rune, true));
          writeSlot(frame, destination, BigInt(header));
          return 0;
        }
        if (name === 'vo_conv_runes_str') {
          const source = Number(readSlot(frame, argumentsStart));
          if (source === 0) {
            writeSlot(frame, destination, 0n);
            return 0;
          }
          const data = Number(view().getBigUint64(source, true));
          const length = Number(view().getBigUint64(source + 8, true));
          const stride = Number(view().getBigUint64(source + 24, true));
          if (stride !== 4) throw new Error(`rune slice has invalid stride ${stride}`);
          let value = '';
          for (let index = 0; index < length; index += 1) {
            const rune = view().getUint32(data + index * stride, true);
            const valid = rune <= 0x10ffff && !(rune >= 0xd800 && rune <= 0xdfff);
            value += String.fromCodePoint(valid ? rune : 0xfffd);
          }
          writeSlot(frame, destination, allocateString(value));
          return 0;
        }
        if (name === 'vo_copy' || name === 'vo_copy_string') {
          const destinationRef = Number(readSlot(frame, argumentsStart));
          const sourceRef = Number(readSlot(frame, argumentsStart + 1));
          if (destinationRef === 0 || sourceRef === 0) {
            writeSlot(frame, destination, 0n);
            return 0;
          }
          const destinationLength = Number(view().getBigUint64(destinationRef + 8, true));
          const destinationStride = Number(view().getBigUint64(destinationRef + 24, true));
          const sourceStride = Number(view().getBigUint64(sourceRef + 24, true));
          const sourceIsString = name === 'vo_copy_string';
          const sourceLength = Number(view().getBigUint64(
            sourceRef + (sourceIsString ? 0 : 8),
            true,
          ));
          const count = Math.min(destinationLength, sourceLength);
          const destinationData = Number(view().getBigUint64(destinationRef, true));
          const sourceData = Number(view().getBigUint64(
            sourceRef + (sourceIsString ? 8 : 0),
            true,
          ));
          const sourceElementBytes = sourceIsString ? 1 : sourceStride;
          if (sourceIsString && destinationStride !== 1) {
            throw new Error('vo_copy_string destination element layout is not byte-sized');
          }
          if (!sourceIsString && destinationStride !== sourceStride) {
            const compactStride = Math.min(destinationStride, sourceStride);
            const flatStride = Math.max(destinationStride, sourceStride);
            if (flatStride !== 8 || ![1, 2, 4].includes(compactStride)) {
              throw new Error('vo_copy element layouts differ');
            }
            // Slices of compact primitive arrays may expose either packed
            // backing storage or one 64-bit VM slot per element. Stage each
            // logical value so overlap still follows memmove semantics.
            const staged = new Uint8Array(count * compactStride);
            for (let index = 0; index < count; index += 1) {
              staged.set(new Uint8Array(
                memory.buffer,
                sourceData + index * sourceStride,
                compactStride,
              ), index * compactStride);
            }
            for (let index = 0; index < count; index += 1) {
              const destinationElement = new Uint8Array(
                memory.buffer,
                destinationData + index * destinationStride,
                compactStride,
              );
              destinationElement.set(staged.subarray(
                index * compactStride,
                (index + 1) * compactStride,
              ));
            }
            writeSlot(frame, destination, BigInt(count));
            return 0;
          }
          new Uint8Array(memory.buffer, destinationData, count * destinationStride)
            .set(new Uint8Array(memory.buffer, sourceData, count * sourceElementBytes));
          writeSlot(frame, destination, BigInt(count));
          return 0;
        }
        if (name === 'vo_slice_append_slice' || name === 'vo_slice_append_string') {
          const destinationRef = Number(readSlot(frame, argumentsStart));
          const sourceRef = Number(readSlot(frame, argumentsStart + 1));
          const elementMeta = Number(readSlot(frame, argumentsStart + 2) & 0xffff_ffffn);
          if (sourceRef === 0) {
            writeSlot(frame, destination, BigInt(destinationRef));
            return 0;
          }
          const sourceIsString = name === 'vo_slice_append_string';
          const sourceData = Number(view().getBigUint64(
            sourceRef + (sourceIsString ? 8 : 0), true,
          ));
          const sourceLength = Number(view().getBigUint64(
            sourceRef + (sourceIsString ? 0 : 8), true,
          ));
          if (sourceLength === 0) {
            writeSlot(frame, destination, BigInt(destinationRef));
            return 0;
          }
          const sourceStride = sourceIsString
            ? 1 : Number(view().getBigUint64(sourceRef + 24, true));
          const oldData = destinationRef === 0
            ? 0 : Number(view().getBigUint64(destinationRef, true));
          const oldLength = destinationRef === 0
            ? 0 : Number(view().getBigUint64(destinationRef + 8, true));
          const oldCapacity = destinationRef === 0
            ? 0 : Number(view().getBigUint64(destinationRef + 16, true));
          const stride = destinationRef === 0
            ? sourceStride : Number(view().getBigUint64(destinationRef + 24, true));
          if (stride !== sourceStride) throw new Error('append source element layouts differ');
          const newLength = oldLength + sourceLength;
          if (!Number.isSafeInteger(newLength)) throw new Error('Volang append length overflow');
          if (newLength <= oldCapacity) {
            const header = allocateSequence(32, elementMeta);
            view().setBigUint64(header, BigInt(oldData), true);
            view().setBigUint64(header + 8, BigInt(newLength), true);
            view().setBigUint64(header + 16, BigInt(oldCapacity), true);
            view().setBigUint64(header + 24, BigInt(stride), true);
            new Uint8Array(memory.buffer, oldData + oldLength * stride, sourceLength * stride)
              .set(new Uint8Array(memory.buffer, sourceData, sourceLength * stride));
            writeSlot(frame, destination, BigInt(header));
            return 0;
          }
          const newCapacity = Math.max(4, newLength, oldCapacity * 2);
          const allocation = allocateSlice(newLength, newCapacity, stride, elementMeta);
          if (oldLength !== 0) {
            new Uint8Array(memory.buffer, allocation.data, oldLength * stride)
              .set(new Uint8Array(memory.buffer, oldData, oldLength * stride));
          }
          new Uint8Array(
            memory.buffer,
            allocation.data + oldLength * stride,
            sourceLength * stride,
          ).set(new Uint8Array(memory.buffer, sourceData, sourceLength * stride));
          writeSlot(frame, destination, BigInt(allocation.header));
          return 0;
        }
        if (name === 'vo_assert') {
          if (readSlot(frame, argumentsStart + 1) === 0n) {
            const fields: string[] = [];
            for (let slot = 2; slot + 1 < argumentSlots; slot += 2) {
              fields.push(formatInterface(
                readSlot(frame, argumentsStart + slot),
                readSlot(frame, argumentsStart + slot + 1),
              ));
            }
            return call.panic(fields.length === 0
              ? 'assertion failed'
              : `assertion failed: ${fields.join(' ')}`);
          }
          return 0;
        }
        if (isStdlibExtern(descriptor, 'os', 'nativeGetArgs')) {
          writeSlot(frame, destination, allocateStringSlice(args));
          return 0;
        }
        const time = timeOperation(descriptor);
        if (time !== undefined) {
          if (time === 'nowUnixNano') {
            writeSlot(frame, destination, BigInt(Date.now()) * 1_000_000n);
            return 0;
          }
          if (time === 'nowMonoNano') {
            const milliseconds = typeof performance === 'undefined'
              ? Date.now() : performance.now();
            writeSlot(frame, destination, BigInt(Math.trunc(milliseconds * 1_000_000)));
            return 0;
          }
          const unixSecondsSlot = time.startsWith('iana')
            ? argumentsStart + 1 : argumentsStart;
          const unixSeconds = BigInt.asIntN(64, readSlot(frame, unixSecondsSlot));
          if (time === 'localOffsetAt') {
            const date = dateAtUnixSeconds(unixSeconds);
            writeSlot(frame, destination, BigInt(date === undefined
              ? 0 : -date.getTimezoneOffset() * 60));
            return 0;
          }
          if (time === 'localAbbrevAt') {
            writeSlot(
              frame,
              destination,
              allocateString(timeZoneAbbreviationAt(undefined, unixSeconds)),
            );
            return 0;
          }
          const requestedZone = readString(readSlot(frame, argumentsStart));
          const timeZone = canonicalTimeZone(requestedZone);
          if (time === 'ianaOffsetAt') {
            writeSlot(
              frame,
              destination,
              BigInt(timeZone === undefined ? 0 : timeZoneOffsetAt(timeZone, unixSeconds)),
            );
            return 0;
          }
          if (time === 'ianaAbbrevAt') {
            writeSlot(
              frame,
              destination,
              allocateString(timeZoneAbbreviationAt(timeZone ?? 'UTC', unixSeconds)),
            );
            return 0;
          }
          if (timeZone === undefined) {
            writeSlot(frame, destination, 0n);
            writeGuestError(frame, destination + 1, `time: unknown time zone ${requestedZone}`);
          } else {
            writeSlot(frame, destination, allocateString(timeZone));
            writeSlot(frame, destination + 1, 0n);
            writeSlot(frame, destination + 2, 0n);
          }
          return 0;
        }
        if (isStdlibExtern(descriptor, 'time', 'blocking_sleepNano')) {
          const duration = BigInt.asIntN(64, readSlot(frame, argumentsStart));
          if (duration <= 0n) return 0;
          return executeHandler(() => sleepNanoseconds(duration));
        }
        const rand = randOperation(descriptor);
        if (rand !== undefined) {
          if (rand === 'Read') {
            const reference = Number(readSlot(frame, argumentsStart));
            const length = reference === 0
              ? 0 : Number(view().getBigUint64(reference + 8, true));
            const stride = reference === 0
              ? 1 : Number(view().getBigUint64(reference + 24, true));
            if (stride !== 1) throw new Error(`math/rand.Read byte slice has stride ${stride}`);
            const data = reference === 0
              ? 0 : Number(view().getBigUint64(reference, true));
            const destinationBytes = new Uint8Array(memory.buffer, data, length);
            for (let index = 0; index < length; index += 1) {
              if (randomReadPosition === 0) {
                randomReadValue = nextRandomU64() >> 1n;
                randomReadPosition = 7;
              }
              destinationBytes[index] = Number(randomReadValue & 0xffn);
              randomReadValue >>= 8n;
              randomReadPosition -= 1;
            }
            writeSlot(frame, destination, BigInt(length));
            writeSlot(frame, destination + 1, 0n);
            writeSlot(frame, destination + 2, 0n);
            return 0;
          }
          if (rand === 'Intn' || rand === 'Int63n') {
            const limit = BigInt.asIntN(64, readSlot(frame, argumentsStart));
            if (limit <= 0n) return call.panic(`rand.${rand}: invalid argument ${limit}`);
            writeSlot(frame, destination, boundedRandom(limit));
          } else if (rand === 'Int') {
            writeSlot(frame, destination, nextRandomU64() >> 1n);
          } else if (rand === 'Uint64') {
            writeSlot(frame, destination, nextRandomU64());
          } else if (rand === 'Uint32') {
            writeSlot(frame, destination, nextRandomU64() & 0xffff_ffffn);
          } else if (rand === 'Float64') {
            writeFloat64(frame, destination, Number(nextRandomU64() >> 11n) / (2 ** 53));
          } else {
            const value = Math.fround(Number(nextRandomU64() >> 40n) / (2 ** 24));
            writeSlot(frame, destination, BigInt(float32Bits(value)));
          }
          return 0;
        }
        const strconv = strconvOperation(descriptor);
        if (strconv === 'parseFloat') {
          let parsed: { value: number; status: number };
          try {
            const source = readStringBytes(readSlot(frame, argumentsStart));
            const text = new TextDecoder('utf-8', { fatal: true }).decode(source);
            parsed = parseVolangFloat(
              text,
              BigInt.asIntN(64, readSlot(frame, argumentsStart + 1)),
            );
          } catch {
            parsed = { value: 0, status: 1 };
          }
          writeFloat64(frame, destination, parsed.value);
          writeSlot(frame, destination + 1, BigInt(parsed.status));
          return 0;
        }
        if (strconv === 'formatFloat') {
          const encoded = formatVolangFloat(
            readFloat64(frame, argumentsStart),
            Number(readSlot(frame, argumentsStart + 1) & 0xffn),
            BigInt.asIntN(64, readSlot(frame, argumentsStart + 2)),
            BigInt.asIntN(64, readSlot(frame, argumentsStart + 3)),
          );
          writeSlot(frame, destination, allocateStringBytes(encoded));
          return 0;
        }
        const string = stringOperation(descriptor);
        if (string !== undefined) {
          const source = readStringBytes(readSlot(frame, argumentsStart)).slice();
          if (string === 'Index' || string === 'LastIndex' || string === 'Count') {
            const pattern = readStringBytes(readSlot(frame, argumentsStart + 1)).slice();
            let result: number;
            if (string === 'Index') result = findBytes(source, pattern);
            else if (string === 'LastIndex') result = lastIndexBytes(source, pattern);
            else if (pattern.byteLength === 0) result = runeCount(source) + 1;
            else {
              result = 0;
              for (let offset = 0; offset <= source.byteLength - pattern.byteLength;) {
                const found = findBytes(source, pattern, offset);
                if (found < 0) break;
                result += 1;
                offset = found + pattern.byteLength;
              }
            }
            writeSlot(frame, destination, BigInt(result));
            return 0;
          }
          if (string === 'ToLower' || string === 'ToUpper' || string === 'ToTitle') {
            const mapping = string === 'ToLower'
              ? SIMPLE_LOWER : (string === 'ToUpper' ? SIMPLE_UPPER : SIMPLE_TITLE);
            writeSlot(frame, destination, allocateStringBytes(mappedStringBytes(source, mapping)));
            return 0;
          }
          if (string === 'EqualFold') {
            const right = readStringBytes(readSlot(frame, argumentsStart + 1)).slice();
            writeSlot(frame, destination, equalFoldBytes(source, right) ? 1n : 0n);
            return 0;
          }
          if (string === 'Fields') {
            const fields: Uint8Array[] = [];
            let fieldStart = -1;
            for (let offset = 0; offset < source.byteLength;) {
              const [rune, width] = decodeUtf8Rune(source, offset);
              const space = unicodeRangeValue(WHITE_SPACE, rune, 0) !== 0;
              if (space && fieldStart >= 0) {
                fields.push(source.slice(fieldStart, offset));
                fieldStart = -1;
              } else if (!space && fieldStart < 0) fieldStart = offset;
              offset += width;
            }
            if (fieldStart >= 0) fields.push(source.slice(fieldStart));
            writeSlot(frame, destination, allocateStringBytesSlice(fields));
            return 0;
          }
          if (string === 'Replace') {
            const old = readStringBytes(readSlot(frame, argumentsStart + 1)).slice();
            const replacement = readStringBytes(readSlot(frame, argumentsStart + 2)).slice();
            const limit = BigInt.asIntN(64, readSlot(frame, argumentsStart + 3));
            writeSlot(
              frame,
              destination,
              allocateStringBytes(replaceStringBytes(source, old, replacement, limit)),
            );
            return 0;
          }
          const separator = readStringBytes(readSlot(frame, argumentsStart + 1)).slice();
          const keepSeparator = string === 'SplitAfter' || string === 'SplitAfterN';
          const limit = string === 'SplitN' || string === 'SplitAfterN'
            ? BigInt.asIntN(64, readSlot(frame, argumentsStart + 2)) : -1n;
          const parts = splitStringBytes(source, separator, keepSeparator, limit);
          writeSlot(
            frame,
            destination,
            limit === 0n ? 0n : allocateStringBytesSlice(parts),
          );
          return 0;
        }
        const bytes = bytesOperation(descriptor);
        if (bytes !== undefined) {
          const source = readByteSlice(readSlot(frame, argumentsStart)).slice();
          if (bytes === 'Index' || bytes === 'LastIndex' || bytes === 'Count') {
            const pattern = readByteSlice(readSlot(frame, argumentsStart + 1)).slice();
            let result: number;
            if (bytes === 'Index') result = findBytes(source, pattern);
            else if (bytes === 'LastIndex') result = lastIndexBytes(source, pattern);
            else if (pattern.byteLength === 0) result = runeCount(source) + 1;
            else {
              result = 0;
              for (let offset = 0; offset <= source.byteLength - pattern.byteLength;) {
                const found = findBytes(source, pattern, offset);
                if (found < 0) break;
                result += 1;
                offset = found + pattern.byteLength;
              }
            }
            writeSlot(frame, destination, BigInt(result));
            return 0;
          }
          if (bytes === 'ToLower' || bytes === 'ToUpper' || bytes === 'ToTitle') {
            const mapping = bytes === 'ToLower'
              ? SIMPLE_LOWER : (bytes === 'ToUpper' ? SIMPLE_UPPER : SIMPLE_TITLE);
            writeSlot(frame, destination, allocateByteSlice(mappedStringBytes(source, mapping)));
            return 0;
          }
          if (bytes === 'EqualFold') {
            const right = readByteSlice(readSlot(frame, argumentsStart + 1)).slice();
            writeSlot(frame, destination, equalFoldBytes(source, right) ? 1n : 0n);
            return 0;
          }
          const old = readByteSlice(readSlot(frame, argumentsStart + 1)).slice();
          const replacement = readByteSlice(readSlot(frame, argumentsStart + 2)).slice();
          const limit = BigInt.asIntN(64, readSlot(frame, argumentsStart + 3));
          if (source.byteLength === 0 && old.byteLength !== 0) {
            writeSlot(frame, destination, 0n);
            return 0;
          }
          writeSlot(
            frame,
            destination,
            allocateByteSlice(replaceStringBytes(source, old, replacement, limit)),
          );
          return 0;
        }
        const unicode = unicodeOperation(descriptor);
        if (unicode !== undefined) {
          const rune = Number(BigInt.asIntN(32, readSlot(frame, argumentsStart)));
          const category = unicodeCategory(rune);
          let result: number;
          switch (unicode) {
            case 'IsLetter': result = category >= 1 && category <= 5 ? 1 : 0; break;
            case 'IsDigit': result = category === 9 ? 1 : 0; break;
            case 'IsSpace':
              result = validUnicodeScalar(rune)
                && unicodeRangeValue(WHITE_SPACE, rune, 0) !== 0 ? 1 : 0;
              break;
            case 'IsUpper': result = category === 1 ? 1 : 0; break;
            case 'IsLower': result = category === 2 ? 1 : 0; break;
            case 'IsTitle': result = category === 3 ? 1 : 0; break;
            case 'IsControl': result = category === 15 ? 1 : 0; break;
            case 'IsPrint':
              result = unicodeIsPrint(rune) ? 1 : 0;
              break;
            case 'IsPunct':
              result = (category >= 19 && category <= 23) || category === 28 || category === 29
                ? 1 : 0;
              break;
            case 'IsGraphic':
              result = (category >= 1 && category <= 12)
                || (category >= 19 && category <= 29) ? 1 : 0;
              break;
            case 'IsNumber': result = category >= 9 && category <= 11 ? 1 : 0; break;
            case 'IsMark': result = category >= 6 && category <= 8 ? 1 : 0; break;
            case 'IsSymbol': result = category >= 24 && category <= 27 ? 1 : 0; break;
            case 'ToLower': result = unicodeMap(rune, SIMPLE_LOWER); break;
            case 'ToUpper': result = unicodeMap(rune, SIMPLE_UPPER); break;
            case 'ToTitle': result = unicodeMap(rune, SIMPLE_TITLE); break;
            case 'SimpleFold': result = unicodeMap(rune, SIMPLE_FOLD); break;
            default: throw new Error(`unknown Unicode operation ${unicode}`);
          }
          writeSlot(frame, destination, BigInt(result));
          return 0;
        }
        const bitsOperation = mathBitsOperation(descriptor);
        if (bitsOperation !== undefined) {
          const width = bitsOperation === 'nativeUintSize' || !/\d+$/.test(bitsOperation)
            ? 32
            : Number(/(8|16|32|64)$/.exec(bitsOperation)?.[1]);
          if (bitsOperation === 'nativeUintSize') {
            writeSlot(frame, destination, 32n);
            return 0;
          }
          const mask = (1n << BigInt(width)) - 1n;
          const operand = (slot: number) => readSlot(frame, argumentsStart + slot) & mask;
          if (bitsOperation.startsWith('LeadingZeros')) {
            const value = operand(0);
            writeSlot(frame, destination, BigInt(value === 0n ? width : width - bitLength(value)));
          } else if (bitsOperation.startsWith('TrailingZeros')) {
            let value = operand(0);
            let count = 0;
            if (value === 0n) count = width;
            else while ((value & 1n) === 0n) { value >>= 1n; count += 1; }
            writeSlot(frame, destination, BigInt(count));
          } else if (bitsOperation.startsWith('OnesCount')) {
            const count = operand(0).toString(2).replace(/0/g, '').length;
            writeSlot(frame, destination, BigInt(count));
          } else if (bitsOperation.startsWith('Add')) {
            const sum = operand(0) + operand(1) + (operand(2) & 1n);
            writeSlot(frame, destination, sum & mask);
            writeSlot(frame, destination + 1, sum >> BigInt(width));
          } else if (bitsOperation.startsWith('Sub')) {
            const difference = operand(0) - operand(1) - (operand(2) & 1n);
            writeSlot(frame, destination, difference & mask);
            writeSlot(frame, destination + 1, difference < 0n ? 1n : 0n);
          } else if (bitsOperation.startsWith('Mul')) {
            const product = operand(0) * operand(1);
            writeSlot(frame, destination, (product >> BigInt(width)) & mask);
            writeSlot(frame, destination + 1, product & mask);
          } else {
            const divisor = operand(2);
            if (divisor === 0n) return call.panic('division by zero');
            const dividend = (operand(0) << BigInt(width)) | operand(1);
            writeSlot(frame, destination, (dividend / divisor) & mask);
            writeSlot(frame, destination + 1, dividend % divisor);
          }
          return 0;
        }
        const mathUnary = (operation: (value: number) => number) => {
          writeFloat64(frame, destination, operation(readFloat64(frame, argumentsStart)));
        };
        const mathBinary = (operation: (left: number, right: number) => number) => {
          writeFloat64(frame, destination, operation(
            readFloat64(frame, argumentsStart),
            readFloat64(frame, argumentsStart + 1),
          ));
        };
        if (isMathExtern(descriptor, 'Sqrt')) mathUnary(Math.sqrt);
        else if (isMathExtern(descriptor, 'Floor')) mathUnary(Math.floor);
        else if (isMathExtern(descriptor, 'Ceil')) mathUnary(Math.ceil);
        else if (isMathExtern(descriptor, 'Round')) mathUnary((value) => {
          if (value === 0 || !Number.isFinite(value)) return value;
          const rounded = value < 0 ? -Math.floor(-value + 0.5) : Math.floor(value + 0.5);
          return rounded === 0 ? copyFloat64Sign(0, value) : rounded;
        });
        else if (isMathExtern(descriptor, 'Trunc')) mathUnary(Math.trunc);
        else if (isMathExtern(descriptor, 'Cbrt')) mathUnary(Math.cbrt);
        else if (isMathExtern(descriptor, 'Pow')) mathBinary(Math.pow);
        else if (isMathExtern(descriptor, 'Hypot')) mathBinary(Math.hypot);
        else if (isMathExtern(descriptor, 'Exp')) mathUnary(Math.exp);
        else if (isMathExtern(descriptor, 'Exp2')) mathUnary((value) => 2 ** value);
        else if (isMathExtern(descriptor, 'Expm1')) mathUnary(Math.expm1);
        else if (isMathExtern(descriptor, 'Log')) mathUnary(Math.log);
        else if (isMathExtern(descriptor, 'Log2')) mathUnary(Math.log2);
        else if (isMathExtern(descriptor, 'Log10')) mathUnary(Math.log10);
        else if (isMathExtern(descriptor, 'Log1p')) mathUnary(Math.log1p);
        else if (isMathExtern(descriptor, 'Sin')) mathUnary(Math.sin);
        else if (isMathExtern(descriptor, 'Cos')) mathUnary(Math.cos);
        else if (isMathExtern(descriptor, 'Tan')) mathUnary(Math.tan);
        else if (isMathExtern(descriptor, 'Asin')) mathUnary(Math.asin);
        else if (isMathExtern(descriptor, 'Acos')) mathUnary(Math.acos);
        else if (isMathExtern(descriptor, 'Atan')) mathUnary(Math.atan);
        else if (isMathExtern(descriptor, 'Atan2')) mathBinary(Math.atan2);
        else if (isMathExtern(descriptor, 'Sinh')) mathUnary(Math.sinh);
        else if (isMathExtern(descriptor, 'Cosh')) mathUnary(Math.cosh);
        else if (isMathExtern(descriptor, 'Tanh')) mathUnary(Math.tanh);
        else if (isMathExtern(descriptor, 'Asinh')) mathUnary(Math.asinh);
        else if (isMathExtern(descriptor, 'Acosh')) mathUnary(Math.acosh);
        else if (isMathExtern(descriptor, 'Atanh')) mathUnary(Math.atanh);
        else if (isMathExtern(descriptor, 'Mod')) mathBinary((left, right) => left % right);
        else if (isMathExtern(descriptor, 'Modf')) {
          const value = readFloat64(frame, argumentsStart);
          const integer = Math.trunc(value);
          writeFloat64(frame, destination, integer);
          writeFloat64(frame, destination + 1, copyFloat64Sign(value - integer, value));
        } else if (isMathExtern(descriptor, 'Frexp')) {
          const [fraction, exponent] = frexpFloat64(readFloat64(frame, argumentsStart));
          writeFloat64(frame, destination, fraction);
          writeSlot(frame, destination + 1, BigInt.asUintN(64, exponent));
        } else if (isMathExtern(descriptor, 'Ldexp')) {
          writeFloat64(frame, destination, ldexpFloat64(
            readFloat64(frame, argumentsStart),
            BigInt.asIntN(64, readSlot(frame, argumentsStart + 1)),
          ));
        } else if (isMathExtern(descriptor, 'FMA')) {
          writeFloat64(frame, destination, fusedMultiplyAdd(
            readFloat64(frame, argumentsStart),
            readFloat64(frame, argumentsStart + 1),
            readFloat64(frame, argumentsStart + 2),
          ));
        } else if (isMathExtern(descriptor, 'Inf')) {
          writeFloat64(frame, destination, BigInt.asIntN(64, readSlot(frame, argumentsStart)) >= 0n
            ? Number.POSITIVE_INFINITY : Number.NEGATIVE_INFINITY);
        } else if (isMathExtern(descriptor, 'NaN')) {
          writeSlot(frame, destination, 0x7ff8_0000_0000_0001n);
        } else if (isMathExtern(descriptor, 'Float64bits')
          || isMathExtern(descriptor, 'Float64frombits')) {
          writeSlot(frame, destination, readSlot(frame, argumentsStart));
        } else if (isMathExtern(descriptor, 'Float32bits')
          || isMathExtern(descriptor, 'Float32frombits')) {
          writeSlot(frame, destination, readSlot(frame, argumentsStart) & 0xffff_ffffn);
        } else {
          throw new Error(`vo-web AOT host does not provide Volang extern ${name}`);
        }
        return 0;
      },
    },
  };
  instance = await WebAssembly.instantiate(module, imports);
  if (instance.exports[AOT_MEMORY_EXPORT] !== memory) {
    throw new Error('Volang AOT module did not re-export its admitted memory');
  }
  const fuelExport = instance.exports[AOT_FUEL_EXPORT];
  if (!(fuelExport instanceof WebAssembly.Global)) {
    throw new Error('Volang AOT fuel export is missing');
  }
  if (options.fuel !== undefined) {
    const fuel = typeof options.fuel === 'bigint'
      ? options.fuel
      : (Number.isSafeInteger(options.fuel) ? BigInt(options.fuel) : -1n);
    if (fuel < 0n || fuel > 0x7fff_ffff_ffff_ffffn) {
      throw new Error('Volang AOT fuel must be a non-negative safe integer or bigint');
    }
    fuelExport.value = fuel;
  }
  const entry = instance.exports[AOT_ENTRY_EXPORT] as (() => number) | undefined;
  if (typeof entry !== 'function') throw new Error('Volang AOT entry export is missing');
  let exitCode = entry();
  while (exitCode === 5 && asyncExterns.size !== 0) {
    await Promise.race([...asyncExterns.values()].map((state) => state.wake));
    exitCode = entry();
  }
  const exitedByGuest = requestedExitCode !== undefined;
  if (requestedExitCode !== undefined) exitCode = requestedExitCode;
  const panicMessage = !exitedByGuest && exitCode === 12
    ? (instance.exports[AOT_PANIC_MESSAGE_EXPORT] as () => number)()
    : 0;
  let panicDetail = '';
  if (!exitedByGuest && exitCode === 12) {
    if (panicMessage !== 0) {
      panicDetail = readString(BigInt(panicMessage));
    } else {
      const panicType = (instance.exports[AOT_PANIC_TYPE_EXPORT] as () => bigint)();
      const panicData = (instance.exports[AOT_PANIC_DATA_EXPORT] as () => bigint)();
      panicDetail = formatInterface(panicType, panicData);
    }
  }
  const runtimeFailure = panicDetail.length === 0
    ? statusMessage(exitCode)
    : `${statusMessage(exitCode)}: ${panicDetail}`;
  const failure = stderr.length === 0
    ? runtimeFailure
    : `${stderr}${stderr.endsWith('\n') ? '' : '\n'}${runtimeFailure}`;
  const result: AotExecutionResult = exitCode === 0
    ? { status: 'ok', stdout, stderr }
    : (exitedByGuest
      ? { status: 'error', stdout, stderr, exitCode }
      : { status: 'error', stdout, stderr: failure, exitCode });
  return { instance, manifest, result, exitCode };
}

/**
 * Initialize only the VFS layer (MemoryFS + OPFS + JavaScript-global bindings).
 * Use this when you load your own WASM (e.g. a custom build with extra externs).
 * Must be called before running any Vo program.
 */
export async function initVFS(): Promise<void> {
  await vfs.init();
  registerVFSBindings();
}

/**
 * Initialize vo-web runtime (VFS + built-in vo-web WASM).
 * Must be called before using any other vo-web functions.
 */
export async function init(runtime?: import('../pkg/vo_web.js').InitInput): Promise<void> {
  await initVFS();

  const wasm = await import('../pkg/vo_web.js');
  await wasm.default(runtime === undefined ? undefined : { module_or_path: runtime });
  wasmModule = wasm;
}

/**
 * Compile Vo source code to bytecode.
 */
export function compile(source: string, filename?: string) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.compile(source, filename);
}

/** Compile a complete project stored in the browser VFS. */
export function compileProject(
  entry: string,
  projectRoot: string,
  modRoot = '',
  overlayPath?: string,
  overlayText?: string,
) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.compileProject(entry, projectRoot, modRoot, overlayPath, overlayText);
}

/** Analyze one project package without requiring an executable entry point. */
export function analyzeProject(
  entry: string,
  projectRoot: string,
  modRoot = '',
  overlayPath?: string,
  overlayText?: string,
) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.analyzeProject(entry, projectRoot, modRoot, overlayPath, overlayText);
}

/** Install locked registry modules and compile a complete browser-VFS project. */
export function compileProjectAutoInstall(
  entry: string,
  projectRoot: string,
  overlayPath?: string,
  overlayText?: string,
) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.compileProjectAutoInstall(entry, projectRoot, overlayPath, overlayText);
}

/** Install locked registry modules and analyze one browser-VFS package. */
export function analyzeProjectAutoInstall(
  entry: string,
  projectRoot: string,
  overlayPath?: string,
  overlayText?: string,
) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.analyzeProjectAutoInstall(entry, projectRoot, overlayPath, overlayText);
}

/** Create a frozen workspace-only lock for packaged browser modules. */
export function prepareWorkspaceLock(rootMod: string, workspaceModules: readonly string[]): string {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.prepareWorkspaceLock(rootMod, Array.from(workspaceModules));
}

/**
 * Run bytecode.
 */
export function run(bytecode: Uint8Array) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.run(bytecode);
}

/** Run bytecode with explicit process arguments. */
export function runWithArgs(bytecode: Uint8Array, args: readonly string[] = []) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.runWithArgs(bytecode, Array.from(args));
}

/** Create a persistent VM/JIT development Island for an already compiled app. */
export function createVmIsland(bytecode: Uint8Array) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return new wasmModule.VoVmIsland(bytecode);
}

/**
 * Compile and run in one step.
 */
export function compileAndRun(source: string, filename?: string) {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.compileAndRun(source, filename);
}

/**
 * Get version string.
 */
export function version(): string {
  if (!wasmModule) throw new Error('vo-web not initialized. Call init() first.');
  return wasmModule.version();
}

/**
 * Await a durable VFS checkpoint in OPFS. Rejects when browser persistence
 * fails; resolves immediately on memory-only hosts. Vo `File.Sync` covers
 * immediate visibility inside the synchronous in-memory VFS.
 */
export async function flushVFS(): Promise<void> {
  await vfs.forceFlush();
}
