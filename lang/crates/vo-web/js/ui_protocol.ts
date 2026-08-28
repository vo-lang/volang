export interface UiProtocolLimits {
  readonly maxBatchBytes: number;
  readonly maxEventBytes: number;
  readonly maxNodes: number;
  readonly maxMutationsPerBatch: number;
  readonly maxChildrenPerNode: number;
  readonly maxPropertiesPerNode: number;
  readonly maxTextBytes: number;
  readonly maxValueBytes: number;
}

export const DEFAULT_UI_PROTOCOL_LIMITS: UiProtocolLimits = Object.freeze({
  maxBatchBytes: 16 * 1024 * 1024,
  maxEventBytes: 4 * 1024 * 1024,
  maxNodes: 100_000,
  maxMutationsPerBatch: 100_000,
  maxChildrenPerNode: 50_000,
  maxPropertiesPerNode: 512,
  maxTextBytes: 4 * 1024 * 1024,
  maxValueBytes: 4 * 1024 * 1024,
});

export interface UiIdentity {
  readonly index: number;
  readonly generation: number;
}

export interface UiListener {
  readonly event: number;
  readonly handler: UiIdentity;
  readonly capture: boolean;
  readonly passive: boolean;
  readonly once: boolean;
}

export interface UiEventModifiers {
  readonly shift: boolean;
  readonly control: boolean;
  readonly alt: boolean;
  readonly meta: boolean;
}

export type UiLength =
  | { readonly unit: 'auto' }
  | { readonly unit: 'px' | 'percent' | 'vw' | 'vh'; readonly value: number };

export type UiValue =
  | { readonly type: 'bool'; readonly value: boolean }
  | { readonly type: 'i64'; readonly value: bigint }
  | { readonly type: 'f64'; readonly value: number }
  | { readonly type: 'text'; readonly value: string }
  | { readonly type: 'color'; readonly value: number }
  | { readonly type: 'length'; readonly value: UiLength }
  | { readonly type: 'bytes'; readonly value: Uint8Array };

export type UiMutation =
  | { readonly type: 'create-element'; readonly id: UiIdentity; readonly primitive: number }
  | { readonly type: 'create-text'; readonly id: UiIdentity }
  | { readonly type: 'set-text'; readonly id: UiIdentity; readonly text: string }
  | { readonly type: 'set-property'; readonly id: UiIdentity; readonly property: number; readonly value: UiValue }
  | { readonly type: 'remove-property'; readonly id: UiIdentity; readonly property: number }
  | { readonly type: 'listen'; readonly id: UiIdentity; readonly listener: UiListener }
  | { readonly type: 'unlisten'; readonly id: UiIdentity; readonly event: number; readonly handler: UiIdentity }
  | { readonly type: 'insert-before'; readonly parent: UiIdentity; readonly child: UiIdentity; readonly before?: UiIdentity }
  | { readonly type: 'remove'; readonly parent: UiIdentity; readonly child: UiIdentity }
  | { readonly type: 'delete'; readonly id: UiIdentity };

export interface UiMutationBatch {
  readonly sessionEpoch: bigint;
  readonly revision: bigint;
  readonly mutations: readonly UiMutation[];
}

export type UiEventPayload =
  | { readonly type: 'none' }
  | { readonly type: 'text'; readonly value: string }
  | {
    readonly type: 'text-input';
    readonly value: string;
    readonly selectionStartUtf16: number;
    readonly selectionLengthUtf16: number;
  }
  | { readonly type: 'toggle'; readonly value: boolean }
  | { readonly type: 'scalar'; readonly value: bigint }
  | { readonly type: 'bytes'; readonly value: Uint8Array }
  | {
    readonly type: 'key';
    readonly key: string;
    readonly code: string;
    readonly modifiers: UiEventModifiers;
    readonly repeat: boolean;
    readonly composing: boolean;
  }
  | {
    readonly type: 'pointer';
    readonly x: number;
    readonly y: number;
    readonly button: number;
    readonly buttons: number;
    readonly pointerId: bigint;
    readonly kind: 'unknown' | 'mouse' | 'pen' | 'touch';
    readonly modifiers: UiEventModifiers;
  }
  | {
    readonly type: 'scroll';
    readonly x: number;
    readonly y: number;
    readonly deltaX: number;
    readonly deltaY: number;
    readonly unit: 'pixel' | 'line' | 'page';
    readonly modifiers: UiEventModifiers;
  }
  | {
    readonly type: 'composition';
    readonly value: string;
    readonly selectionStartUtf16: number;
    readonly selectionLengthUtf16: number;
  };

export interface UiEventEnvelope {
  readonly sessionEpoch: bigint;
  readonly handler: UiIdentity;
  readonly event: number;
  readonly target: UiIdentity;
  readonly sequence: bigint;
  readonly payload: UiEventPayload;
}

const textDecoder = new TextDecoder('utf-8', { fatal: true });
const textEncoder = new TextEncoder();

class Reader {
  private readonly view: DataView;
  private cursor = 0;

  constructor(private readonly bytes: Uint8Array) {
    this.view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  }

  get remaining(): number {
    return this.bytes.byteLength - this.cursor;
  }

  take(length: number): Uint8Array {
    if (!Number.isSafeInteger(length) || length < 0 || length > this.remaining) {
      throw new Error('truncated Volang UI frame');
    }
    const value = this.bytes.subarray(this.cursor, this.cursor + length);
    this.cursor += length;
    return value;
  }

  u8(): number {
    return this.take(1)[0];
  }

  u16(): number {
    const offset = this.cursor;
    this.take(2);
    return this.view.getUint16(offset, true);
  }

  i16(): number {
    const offset = this.cursor;
    this.take(2);
    return this.view.getInt16(offset, true);
  }

  u32(): number {
    const offset = this.cursor;
    this.take(4);
    return this.view.getUint32(offset, true);
  }

  u64(): bigint {
    const offset = this.cursor;
    this.take(8);
    return this.view.getBigUint64(offset, true);
  }

  i64(): bigint {
    const offset = this.cursor;
    this.take(8);
    return this.view.getBigInt64(offset, true);
  }

  f32(): number {
    const offset = this.cursor;
    this.take(4);
    return this.view.getFloat32(offset, true);
  }

  f64(): number {
    const offset = this.cursor;
    this.take(8);
    return this.view.getFloat64(offset, true);
  }

  identity(): UiIdentity {
    return { index: this.u32(), generation: this.u32() };
  }

  byteString(limit: number): Uint8Array {
    const length = this.u32();
    if (length > limit) throw new Error('Volang UI value exceeds its byte limit');
    return this.take(length);
  }

  string(limit: number): string {
    return textDecoder.decode(this.byteString(limit));
  }

  finish(): void {
    if (this.remaining !== 0) throw new Error('Volang UI frame has trailing bytes');
  }
}

function expectMagic(reader: Reader, expected: string): void {
  const magic = new TextDecoder('ascii', { fatal: true }).decode(reader.take(4));
  if (magic !== expected) throw new Error(`invalid Volang UI frame magic ${JSON.stringify(magic)}`);
}

function decodeListener(reader: Reader): UiListener {
  const event = reader.u16();
  const handler = reader.identity();
  const options = reader.u8();
  if ((options & ~0b111) !== 0) throw new Error('invalid Volang UI listener options');
  return {
    event,
    handler,
    capture: (options & 1) !== 0,
    passive: (options & 2) !== 0,
    once: (options & 4) !== 0,
  };
}

function decodeValue(reader: Reader, limits: UiProtocolLimits): UiValue {
  switch (reader.u8()) {
    case 1: {
      const value = reader.u8();
      if (value > 1) throw new Error('invalid Volang UI boolean');
      return { type: 'bool', value: value === 1 };
    }
    case 2:
      return { type: 'i64', value: reader.i64() };
    case 3:
      return { type: 'f64', value: reader.f64() };
    case 4:
      return { type: 'text', value: reader.string(limits.maxValueBytes) };
    case 5:
      return { type: 'color', value: reader.u32() };
    case 6: {
      const unit = reader.u8();
      if (unit === 0) return { type: 'length', value: { unit: 'auto' } };
      const value = reader.f32();
      const units = ['px', 'percent', 'vw', 'vh'] as const;
      const decoded = units[unit - 1];
      if (decoded === undefined) throw new Error(`invalid Volang UI length unit ${unit}`);
      return { type: 'length', value: { unit: decoded, value } };
    }
    case 7:
      return { type: 'bytes', value: reader.byteString(limits.maxValueBytes).slice() };
    default:
      throw new Error('invalid Volang UI value tag');
  }
}

function decodeMutation(reader: Reader, limits: UiProtocolLimits): UiMutation {
  switch (reader.u8()) {
    case 1: {
      const id = reader.identity();
      const kind = reader.u8();
      if (kind === 1) {
        const primitive = reader.u16();
        if (primitive > 16) throw new Error(`invalid Volang UI primitive ${primitive}`);
        return { type: 'create-element', id, primitive };
      }
      if (kind === 2) return { type: 'create-text', id };
      throw new Error(`invalid Volang UI node kind ${kind}`);
    }
    case 2:
      return { type: 'set-text', id: reader.identity(), text: reader.string(limits.maxTextBytes) };
    case 3:
      return {
        type: 'set-property',
        id: reader.identity(),
        property: reader.u32(),
        value: decodeValue(reader, limits),
      };
    case 4:
      return { type: 'remove-property', id: reader.identity(), property: reader.u32() };
    case 5:
      return { type: 'listen', id: reader.identity(), listener: decodeListener(reader) };
    case 6:
      return {
        type: 'unlisten',
        id: reader.identity(),
        event: reader.u16(),
        handler: reader.identity(),
      };
    case 7: {
      const parent = reader.identity();
      const child = reader.identity();
      const present = reader.u8();
      if (present === 0) return { type: 'insert-before', parent, child };
      if (present === 1) return { type: 'insert-before', parent, child, before: reader.identity() };
      throw new Error('invalid Volang UI optional node tag');
    }
    case 8:
      return { type: 'remove', parent: reader.identity(), child: reader.identity() };
    case 9:
      return { type: 'delete', id: reader.identity() };
    default:
      throw new Error('invalid Volang UI mutation tag');
  }
}

export function decodeUiMutationBatch(
  frame: Uint8Array,
  limits: UiProtocolLimits = DEFAULT_UI_PROTOCOL_LIMITS,
): UiMutationBatch {
  if (frame.byteLength > limits.maxBatchBytes) throw new Error('Volang UI batch exceeds host limits');
  const reader = new Reader(frame);
  expectMagic(reader, 'VUI1');
  const sessionEpoch = reader.u64();
  const revision = reader.u64();
  const count = reader.u32();
  if (count > limits.maxMutationsPerBatch || count > reader.remaining) {
    throw new Error('Volang UI mutation count exceeds host limits');
  }
  const mutations: UiMutation[] = [];
  for (let index = 0; index < count; index += 1) {
    mutations.push(decodeMutation(reader, limits));
  }
  reader.finish();
  return { sessionEpoch, revision, mutations };
}

class Writer {
  private readonly bytes: number[] = [];

  u8(value: number): void {
    if (!Number.isInteger(value) || value < 0 || value > 0xff) {
      throw new Error('Volang UI integer exceeds u8');
    }
    this.bytes.push(value);
  }

  u16(value: number): void {
    if (!Number.isInteger(value) || value < 0 || value > 0xffff) {
      throw new Error('Volang UI integer exceeds u16');
    }
    this.bytes.push(value & 0xff, (value >>> 8) & 0xff);
  }

  i16(value: number): void {
    if (!Number.isInteger(value) || value < -0x8000 || value > 0x7fff) {
      throw new Error('Volang UI integer exceeds i16');
    }
    this.u16(value & 0xffff);
  }

  u32(value: number): void {
    if (!Number.isSafeInteger(value) || value < 0 || value > 0xffff_ffff) {
      throw new Error('Volang UI integer exceeds u32');
    }
    this.bytes.push(
      value & 0xff,
      (value >>> 8) & 0xff,
      (value >>> 16) & 0xff,
      (value >>> 24) & 0xff,
    );
  }

  u64(value: bigint): void {
    if (value < 0n || value > 0xffff_ffff_ffff_ffffn) {
      throw new Error('Volang UI integer exceeds u64');
    }
    let remaining = value;
    for (let index = 0; index < 8; index += 1) {
      this.u8(Number(remaining & 0xffn));
      remaining >>= 8n;
    }
  }

  i64(value: bigint): void {
    if (value < -0x8000_0000_0000_0000n || value > 0x7fff_ffff_ffff_ffffn) {
      throw new Error('Volang UI integer exceeds i64');
    }
    let remaining = BigInt.asUintN(64, value);
    for (let index = 0; index < 8; index += 1) {
      this.u8(Number(remaining & 0xffn));
      remaining >>= 8n;
    }
  }

  f64(value: number): void {
    if (!Number.isFinite(value)) throw new Error('Volang UI number must be finite');
    const bytes = new Uint8Array(8);
    new DataView(bytes.buffer).setFloat64(0, value, true);
    for (const byte of bytes) this.u8(byte);
  }

  identity(value: UiIdentity): void {
    this.u32(value.index);
    this.u32(value.generation);
  }

  byteString(value: Uint8Array): void {
    this.u32(value.byteLength);
    for (const byte of value) this.u8(byte);
  }

  finish(): Uint8Array {
    return Uint8Array.from(this.bytes);
  }
}

function modifierFlags(modifiers: UiEventModifiers): number {
  return Number(modifiers.shift)
    | (Number(modifiers.control) << 1)
    | (Number(modifiers.alt) << 2)
    | (Number(modifiers.meta) << 3);
}

export function encodeUiEvent(
  event: UiEventEnvelope,
  limits: UiProtocolLimits = DEFAULT_UI_PROTOCOL_LIMITS,
): Uint8Array {
  const writer = new Writer();
  for (const byte of new TextEncoder().encode('VUE1')) writer.u8(byte);
  writer.u64(event.sessionEpoch);
  writer.identity(event.handler);
  writer.u16(event.event);
  writer.identity(event.target);
  writer.u64(event.sequence);
  switch (event.payload.type) {
    case 'none':
      writer.u8(0);
      break;
    case 'text':
      writer.u8(1);
      {
        const value = textEncoder.encode(event.payload.value);
        if (value.byteLength > limits.maxValueBytes) {
          throw new Error('Volang UI event value exceeds host limits');
        }
        writer.byteString(value);
      }
      break;
    case 'toggle':
      writer.u8(2);
      writer.u8(event.payload.value ? 1 : 0);
      break;
    case 'scalar':
      writer.u8(3);
      writer.i64(event.payload.value);
      break;
    case 'bytes':
      if (event.payload.value.byteLength > limits.maxValueBytes) {
        throw new Error('Volang UI event value exceeds host limits');
      }
      writer.u8(4);
      writer.byteString(event.payload.value);
      break;
    case 'key': {
      writer.u8(5);
      const key = textEncoder.encode(event.payload.key);
      const code = textEncoder.encode(event.payload.code);
      if (key.byteLength > limits.maxValueBytes || code.byteLength > limits.maxValueBytes) {
        throw new Error('Volang UI event value exceeds host limits');
      }
      writer.byteString(key);
      writer.byteString(code);
      const flags = modifierFlags(event.payload.modifiers)
        | (Number(event.payload.repeat) << 4)
        | (Number(event.payload.composing) << 5);
      writer.u8(flags);
      break;
    }
    case 'pointer': {
      writer.u8(6);
      writer.f64(event.payload.x);
      writer.f64(event.payload.y);
      writer.i16(event.payload.button);
      writer.u16(event.payload.buttons);
      writer.i64(event.payload.pointerId);
      writer.u8({ unknown: 0, mouse: 1, pen: 2, touch: 3 }[event.payload.kind]);
      writer.u8(modifierFlags(event.payload.modifiers));
      break;
    }
    case 'scroll': {
      writer.u8(7);
      writer.f64(event.payload.x);
      writer.f64(event.payload.y);
      writer.f64(event.payload.deltaX);
      writer.f64(event.payload.deltaY);
      writer.u8({ pixel: 0, line: 1, page: 2 }[event.payload.unit]);
      writer.u8(modifierFlags(event.payload.modifiers));
      break;
    }
    case 'composition': {
      writer.u8(8);
      const value = textEncoder.encode(event.payload.value);
      if (value.byteLength > limits.maxValueBytes) {
        throw new Error('Volang UI event value exceeds host limits');
      }
      writer.byteString(value);
      writer.u32(event.payload.selectionStartUtf16);
      writer.u32(event.payload.selectionLengthUtf16);
      break;
    }
    case 'text-input': {
      writer.u8(9);
      const value = textEncoder.encode(event.payload.value);
      if (value.byteLength > limits.maxValueBytes) {
        throw new Error('Volang UI event value exceeds host limits');
      }
      writer.byteString(value);
      writer.u32(event.payload.selectionStartUtf16);
      writer.u32(event.payload.selectionLengthUtf16);
      break;
    }
  }
  const frame = writer.finish();
  if (frame.byteLength > limits.maxEventBytes) throw new Error('Volang UI event exceeds host limits');
  return frame;
}

function decodeEventModifiers(flags: number): UiEventModifiers {
  return {
    shift: (flags & 1) !== 0,
    control: (flags & 2) !== 0,
    alt: (flags & 4) !== 0,
    meta: (flags & 8) !== 0,
  };
}

/** Decode one renderer-to-runtime event envelope. */
export function decodeUiEvent(
  frame: Uint8Array,
  limits: UiProtocolLimits = DEFAULT_UI_PROTOCOL_LIMITS,
): UiEventEnvelope {
  if (frame.byteLength > limits.maxEventBytes) throw new Error('Volang UI event exceeds host limits');
  const reader = new Reader(frame);
  expectMagic(reader, 'VUE1');
  const sessionEpoch = reader.u64();
  const handler = reader.identity();
  const event = reader.u16();
  const target = reader.identity();
  const sequence = reader.u64();
  let payload: UiEventPayload;
  switch (reader.u8()) {
    case 0:
      payload = { type: 'none' };
      break;
    case 1:
      payload = { type: 'text', value: reader.string(limits.maxValueBytes) };
      break;
    case 2: {
      const value = reader.u8();
      if (value > 1) throw new Error('invalid Volang UI toggle event');
      payload = { type: 'toggle', value: value === 1 };
      break;
    }
    case 3:
      payload = { type: 'scalar', value: reader.i64() };
      break;
    case 4:
      payload = { type: 'bytes', value: reader.byteString(limits.maxValueBytes).slice() };
      break;
    case 5: {
      const key = reader.string(limits.maxValueBytes);
      const code = reader.string(limits.maxValueBytes);
      const flags = reader.u8();
      if ((flags & ~0x3f) !== 0) throw new Error('invalid Volang UI key event flags');
      payload = {
        type: 'key',
        key,
        code,
        modifiers: decodeEventModifiers(flags),
        repeat: (flags & 0x10) !== 0,
        composing: (flags & 0x20) !== 0,
      };
      break;
    }
    case 6: {
      const x = reader.f64();
      const y = reader.f64();
      const button = reader.i16();
      const buttons = reader.u16();
      const pointerId = reader.i64();
      const kind = ['unknown', 'mouse', 'pen', 'touch'] as const;
      const pointerKind = kind[reader.u8()];
      if (pointerKind === undefined) throw new Error('invalid Volang UI pointer kind');
      const flags = reader.u8();
      if ((flags & ~0x0f) !== 0) throw new Error('invalid Volang UI pointer event flags');
      payload = {
        type: 'pointer', x, y, button, buttons, pointerId,
        kind: pointerKind,
        modifiers: decodeEventModifiers(flags),
      };
      break;
    }
    case 7: {
      const x = reader.f64();
      const y = reader.f64();
      const deltaX = reader.f64();
      const deltaY = reader.f64();
      const units = ['pixel', 'line', 'page'] as const;
      const unit = units[reader.u8()];
      if (unit === undefined) throw new Error('invalid Volang UI scroll unit');
      const flags = reader.u8();
      if ((flags & ~0x0f) !== 0) throw new Error('invalid Volang UI scroll event flags');
      payload = {
        type: 'scroll', x, y, deltaX, deltaY, unit,
        modifiers: decodeEventModifiers(flags),
      };
      break;
    }
    case 8:
      payload = {
        type: 'composition',
        value: reader.string(limits.maxValueBytes),
        selectionStartUtf16: reader.u32(),
        selectionLengthUtf16: reader.u32(),
      };
      break;
    case 9:
      payload = {
        type: 'text-input',
        value: reader.string(limits.maxValueBytes),
        selectionStartUtf16: reader.u32(),
        selectionLengthUtf16: reader.u32(),
      };
      break;
    default:
      throw new Error('invalid Volang UI event payload tag');
  }
  reader.finish();
  return { sessionEpoch, handler, event, target, sequence, payload };
}

export function uiIdentityKey(identity: UiIdentity): string {
  return `${identity.index}:${identity.generation}`;
}
