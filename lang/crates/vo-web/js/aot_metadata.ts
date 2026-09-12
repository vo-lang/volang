/** Validate and decode compiler-authenticated image metadata before execution. */
import { AOT_MANIFEST_SECTION, AOT_EXTERN_SECTION, AOT_RUNTIME_METADATA_SECTION, AOT_DEBUG_METADATA_SECTION, AOT_RUNTIME_MODULE, AOT_RUNTIME_FUNCTION, AOT_MEMORY_EXPORT, AOT_ENTRY_EXPORT, AOT_ALLOC_EXPORT, AOT_SEQUENCE_ALLOC_EXPORT, AOT_TYPED_ALLOC_EXPORT, AOT_MAP_LOOKUP_EXPORT, AOT_PANIC_MESSAGE_EXPORT, AOT_PANIC_TYPE_EXPORT, AOT_PANIC_DATA_EXPORT, AOT_RAISE_HOST_PANIC_EXPORT, AOT_FUEL_EXPORT, AOT_ABI_VERSION, AOT_CORE_MODULE_KIND, MAX_EXTERN_COUNT } from './aot_abi.js';
import type { AotManifest, AotExternDescriptor } from './aot_types.js';
import type { AotRuntimeMetadata, AotRuntimeType, AotStructField, AotStructType } from './aot_json.js';
import { parseAotInlineSources, type AotInlineSources } from './aot_inline_sources.js';
import { AotSourceLocations } from './aot_source_locations.js';

export interface AotDebugLocation {
  readonly pc: number;
  readonly file: string;
  readonly line: number;
  readonly col: number;
  readonly length: number;
}

export interface AotDebugMetadata {
  readonly functions: readonly ReadonlyMap<number, AotDebugLocation>[];
  readonly inlineSources: AotInlineSources;
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

export function parseAotManifest(module: WebAssembly.Module): AotManifest {
  const sections = WebAssembly.Module.customSections(module, AOT_MANIFEST_SECTION);
  if (sections.length !== 1) {
    throw new Error(`expected one ${AOT_MANIFEST_SECTION} section`);
  }
  const bytes = new Uint8Array(sections[0]);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  const magic = new TextDecoder('ascii', { fatal: true }).decode(bytes.subarray(0, 8));
  if (magic !== 'VOAOTW09') throw new Error('invalid Volang AOT manifest magic');
  const abiVersion = readU16(view, 8);
  if (abiVersion !== AOT_ABI_VERSION) {
    throw new Error(`unsupported Volang AOT ABI ${abiVersion}`);
  }
  if (bytes[10] !== AOT_CORE_MODULE_KIND) throw new Error('artifact is not a Core Wasm AOT module');
  if (bytes[11] !== 1) throw new Error('unsupported Core Wasm memory contract');
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
  return { abiVersion, target, semanticModuleLength, memoryPages, moduleSha256, memoryContract: 'island-span-heap' };
}

export function parseAotExterns(module: WebAssembly.Module): readonly AotExternDescriptor[] {
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

export function parseAotRuntimeMetadata(module: WebAssembly.Module): AotRuntimeMetadata {
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
    const canonicalArrayOnly = tag === 2 && kind === 14 && slotCount > 0xffff;
    const validDescriptors = canonicalArrayOnly
      ? fixedDescriptor === undefined && sequenceDescriptor === undefined
      : fixedDescriptor !== undefined && sequenceDescriptor !== undefined;
    if ((slotCount > 0xffff && !canonicalArrayOnly) || storageBytes > 0xffff_ffff
      || types.has(raw) || (canonicalMeta & 0xff) !== kind
      || !validDescriptors
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
    if (type.tag === 2 && (type.kind !== 14
      || BigInt(type.slotCount) !== type.length * BigInt(types.get(type.first)!.slotCount))) {
      throw new Error('Volang array runtime type has an inconsistent logical layout');
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

export function parseAotDebugMetadata(module: WebAssembly.Module): AotDebugMetadata {
  const sections = WebAssembly.Module.customSections(module, AOT_DEBUG_METADATA_SECTION);
  if (sections.length !== 1) {
    throw new Error(`expected one ${AOT_DEBUG_METADATA_SECTION} section`);
  }
  const bytes = new Uint8Array(sections[0]);
  if (bytes.byteLength < 32) throw new Error('truncated Volang debug metadata');
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  const magic = new TextDecoder('ascii', { fatal: true }).decode(bytes.subarray(0, 8));
  if (magic !== 'VODBG003') throw new Error('invalid Volang debug metadata magic');
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
  const sourceWord = (): number => {
    let value = 0;
    for (let index = 0; index < 5; index++) {
      if (offset >= bytes.byteLength) throw new Error('truncated Volang debug coordinate');
      const byte = bytes[offset++];
      if (index === 4 && byte > 15) throw new Error('invalid Volang debug coordinate');
      value += (byte & 127) * 2 ** (index * 7);
      if ((byte & 128) === 0) {
        if (index !== 0 && byte === 0) throw new Error('noncanonical Volang debug coordinate');
        return value;
      }
    }
    throw new Error('invalid Volang debug coordinate');
  };
  for (let functionId = 0; functionId < functionCount; functionId += 1) {
    const entryCount = readU32(view, offset);
    offset += 4;
    if (entryCount > MAX_EXTERN_COUNT || offset + entryCount * 5 > bytes.byteLength) {
      throw new Error('truncated Volang debug locations');
    }
    const entries = new Uint32Array(entryCount * 5);
    let previousPc = -1;
    for (let index = 0; index < entryCount; index += 1) {
      const pc = sourceWord();
      const fileId = sourceWord();
      const line = sourceWord();
      const col = sourceWord();
      const length = sourceWord();
      if (fileId >= files.length || pc <= previousPc || line === 0 || col === 0 || length === 0) {
        throw new Error('invalid Volang debug location');
      }
      previousPc = pc;
      const record = index * 5;
      entries[record] = pc;
      entries[record + 1] = fileId;
      entries[record + 2] = line;
      entries[record + 3] = col;
      entries[record + 4] = length;
    }
    functions.push(new AotSourceLocations(entries, files));
  }
  if (offset !== bytes.byteLength) throw new Error('Volang debug metadata has trailing bytes');
  return {
    functions,
    inlineSources: parseAotInlineSources(module, files, functionCount),
    frameStateBytes,
    frameFunctionIdOffset,
    frameParentOffset,
    frameDebugPcOffset,
  };
}

export function validateAotShape(module: WebAssembly.Module): void {
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
    throw new Error('Volang AOT module imports do not match AOT ABI v9');
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
    ['vo_current_fiber', 'global'],
    ['vo_fiber_head', 'global'],
    ['vo_gc_debt', 'global'],
    ['vo_gc_barrier', 'global'],
    ['vo_memory_failed', 'global'],
    ['vo_execution_quantum', 'global'],
    [AOT_MEMORY_EXPORT, 'memory'],
  ]);
  if (
    exports.size !== expectedExports.size ||
    [...expectedExports].some(([name, kind]) => exports.get(name) !== kind)
  ) {
    throw new Error('Volang AOT module exports do not match AOT ABI v9');
  }
}
