const MAX_JSON_DEPTH = 64;
const SLOT_BYTES = 8;

const VALUE_KIND_VOID = 0;
const VALUE_KIND_BOOL = 1;
const VALUE_KIND_INT = 2;
const VALUE_KIND_INT8 = 3;
const VALUE_KIND_INT16 = 4;
const VALUE_KIND_INT32 = 5;
const VALUE_KIND_INT64 = 6;
const VALUE_KIND_UINT = 7;
const VALUE_KIND_UINT8 = 8;
const VALUE_KIND_UINT16 = 9;
const VALUE_KIND_UINT32 = 10;
const VALUE_KIND_UINT64 = 11;
const VALUE_KIND_FLOAT32 = 12;
const VALUE_KIND_FLOAT64 = 13;
const VALUE_KIND_ARRAY = 14;
const VALUE_KIND_STRUCT = 15;
const VALUE_KIND_INTERFACE = 16;
const VALUE_KIND_STRING = 17;
const VALUE_KIND_SLICE = 18;
const VALUE_KIND_MAP = 19;
const VALUE_KIND_POINTER = 22;

export interface AotRuntimeType {
  readonly raw: number;
  readonly canonicalMeta: number;
  readonly kind: number;
  readonly tag: number;
  readonly slotCount: number;
  readonly storageBytes: number;
  readonly fixedDescriptor?: number;
  readonly sequenceDescriptor?: number;
  readonly mapDescriptor?: number;
  readonly mapEntriesDescriptor?: number;
  readonly first: number;
  readonly second: number;
  readonly length: bigint;
  readonly typeName?: string;
}

export interface AotStructField {
  readonly name: string;
  readonly tag: string;
  readonly offset: number;
  readonly slotCount: number;
  readonly typeRaw: number;
  readonly embedded: boolean;
  readonly exported: boolean;
}

export interface AotStructType {
  readonly slotCount: number;
  readonly fields: readonly AotStructField[];
}

export interface AotRuntimeMetadata {
  readonly descriptorCount: number;
  readonly types: ReadonlyMap<number, AotRuntimeType>;
  readonly structs: readonly AotStructType[];
  readonly errorValueRaw?: number;
  readonly errorDescriptor?: number;
  readonly errorSlots: number;
  readonly errorMessageOffset: number;
  readonly errorCauseOffset: number;
}

export class AotJsonError extends Error {
  constructor(message: string) {
    super(message);
    this.name = 'AotJsonError';
  }
}

export interface AotStructuredJsonOperations {
  readonly metadata: AotRuntimeMetadata;
  memory(): WebAssembly.Memory;
  view(): DataView;
  allocateTyped(bytes: number, descriptor: number): number;
  allocateSequence(bytes: number, elementMeta: number): number;
  allocateStringBytes(bytes: Uint8Array): bigint;
  lowerSimple(codePoint: number): number;
  formatFloat(value: number, bitSize: 32 | 64): string;
  setReturnRoot(slot0: bigint, slot1: bigint): void;
  clearReturnRoot(): void;
}

interface JsonNumber {
  readonly kind: 'number';
  readonly text: string;
  readonly integer: boolean;
}

interface JsonArray {
  readonly kind: 'array';
  readonly values: readonly JsonValue[];
}

interface JsonObject {
  readonly kind: 'object';
  readonly entries: readonly (readonly [string, JsonValue])[];
}

interface TomlNamedString {
  readonly kind: 'named-string';
  readonly value: string;
  readonly typeName: string;
}

type JsonValue = null | boolean | string | JsonNumber | JsonArray | JsonObject
  | TomlNamedString;

type StructuredFormat = 'json' | 'toml';

interface EmbeddedFieldStep {
  readonly offset: number;
  readonly type: AotRuntimeType;
}

interface SerdeField {
  readonly name: string;
  readonly embeddedPath: readonly EmbeddedFieldStep[];
  readonly offset: number;
  readonly type: AotRuntimeType;
  readonly omitEmpty: boolean;
  readonly depth: number;
  readonly tagged: boolean;
  readonly order: number;
}

function jsonError(message: string): never {
  throw new AotJsonError(message);
}

class JsonParser {
  private position = 0;

  constructor(private readonly source: string) {}

  parse(): JsonValue {
    if (this.source.length === 0) jsonError('empty input');
    const value = this.parseValue(0);
    this.skipWhitespace();
    if (this.position !== this.source.length) {
      jsonError('trailing characters after JSON value');
    }
    return value;
  }

  private parseValue(depth: number): JsonValue {
    this.skipWhitespace();
    const character = this.source[this.position];
    if (character === undefined) jsonError('unexpected end');
    if (character === '{') return this.parseObject(depth);
    if (character === '[') return this.parseArray(depth);
    if (character === '"') return this.parseString();
    if (character === 't') return this.keyword('true', true);
    if (character === 'f') return this.keyword('false', false);
    if (character === 'n') return this.keyword('null', null);
    if (character === '-' || (character >= '0' && character <= '9')) {
      return this.parseNumber();
    }
    return jsonError('unexpected character');
  }

  private enterContainer(depth: number): number {
    if (depth >= MAX_JSON_DEPTH) jsonError('maximum JSON nesting depth exceeded');
    return depth + 1;
  }

  private parseObject(depth: number): JsonObject {
    const childDepth = this.enterContainer(depth);
    this.position += 1;
    this.skipWhitespace();
    const entries: Array<readonly [string, JsonValue]> = [];
    if (this.source[this.position] === '}') {
      this.position += 1;
      return { kind: 'object', entries };
    }
    for (;;) {
      if (this.source[this.position] !== '"') {
        jsonError('expected string key in JSON object');
      }
      const key = this.parseString();
      this.skipWhitespace();
      if (this.source[this.position] !== ':') {
        jsonError("expected ':' after JSON object key");
      }
      this.position += 1;
      entries.push([key, this.parseValue(childDepth)]);
      this.skipWhitespace();
      const separator = this.source[this.position];
      if (separator === '}') {
        this.position += 1;
        return { kind: 'object', entries };
      }
      if (separator !== ',') jsonError("expected ',' or '}' in JSON object");
      this.position += 1;
      this.skipWhitespace();
      if (this.source[this.position] === '}') jsonError('trailing comma in JSON object');
    }
  }

  private parseArray(depth: number): JsonArray {
    const childDepth = this.enterContainer(depth);
    this.position += 1;
    this.skipWhitespace();
    const values: JsonValue[] = [];
    if (this.source[this.position] === ']') {
      this.position += 1;
      return { kind: 'array', values };
    }
    for (;;) {
      values.push(this.parseValue(childDepth));
      this.skipWhitespace();
      const separator = this.source[this.position];
      if (separator === ']') {
        this.position += 1;
        return { kind: 'array', values };
      }
      if (separator !== ',') jsonError("expected ',' or ']' in JSON array");
      this.position += 1;
      this.skipWhitespace();
      if (this.source[this.position] === ']') jsonError('trailing comma in JSON array');
    }
  }

  private parseNumber(): JsonNumber {
    const start = this.position;
    if (this.source[this.position] === '-') {
      this.position += 1;
      if (this.position === this.source.length) jsonError('invalid JSON number');
    }
    if (this.source[this.position] === '0') {
      this.position += 1;
      const next = this.source[this.position];
      if (next !== undefined && next >= '0' && next <= '9') {
        jsonError('leading zero in JSON number');
      }
    } else if (this.source[this.position] >= '1' && this.source[this.position] <= '9') {
      this.consumeDigits();
    } else {
      jsonError('invalid JSON number');
    }
    let integer = true;
    if (this.source[this.position] === '.') {
      integer = false;
      this.position += 1;
      if (!this.isDigit(this.source[this.position])) {
        jsonError('missing fraction digits in JSON number');
      }
      this.consumeDigits();
    }
    if (this.source[this.position] === 'e' || this.source[this.position] === 'E') {
      integer = false;
      this.position += 1;
      if (this.source[this.position] === '+' || this.source[this.position] === '-') {
        this.position += 1;
      }
      if (!this.isDigit(this.source[this.position])) {
        jsonError('missing exponent digits in JSON number');
      }
      this.consumeDigits();
    }
    const text = this.source.slice(start, this.position);
    if (integer) {
      const value = BigInt(text);
      if (value < -0x8000_0000_0000_0000n || value > 0xffff_ffff_ffff_ffffn) {
        jsonError('JSON integer out of range');
      }
    } else if (!Number.isFinite(Number(text))) {
      jsonError('JSON number out of range');
    }
    return { kind: 'number', text, integer };
  }

  private parseString(): string {
    this.position += 1;
    let value = '';
    let segmentStart = this.position;
    for (;;) {
      const character = this.source[this.position];
      if (character === undefined) jsonError('unterminated JSON string');
      const code = character.charCodeAt(0);
      if (character === '"') {
        value += this.source.slice(segmentStart, this.position);
        this.position += 1;
        return value;
      }
      if (code < 0x20) jsonError('unescaped control character in JSON string');
      if (character !== '\\') {
        this.position += 1;
        continue;
      }
      value += this.source.slice(segmentStart, this.position);
      this.position += 1;
      const escape = this.source[this.position];
      if (escape === undefined) jsonError('unterminated JSON escape');
      this.position += 1;
      switch (escape) {
        case '"': value += '"'; break;
        case '\\': value += '\\'; break;
        case '/': value += '/'; break;
        case 'b': value += '\b'; break;
        case 'f': value += '\f'; break;
        case 'n': value += '\n'; break;
        case 'r': value += '\r'; break;
        case 't': value += '\t'; break;
        case 'u': {
          const first = this.parseHexQuad();
          let scalar = first;
          if (first >= 0xd800 && first <= 0xdbff) {
            if (this.source.slice(this.position, this.position + 2) !== '\\u') {
              jsonError('high surrogate without low surrogate in JSON string');
            }
            this.position += 2;
            const second = this.parseHexQuad();
            if (second < 0xdc00 || second > 0xdfff) {
              jsonError('invalid low surrogate in JSON string');
            }
            scalar = 0x1_0000 + ((first - 0xd800) << 10) + second - 0xdc00;
          } else if (first >= 0xdc00 && first <= 0xdfff) {
            jsonError('unpaired low surrogate in JSON string');
          }
          value += String.fromCodePoint(scalar);
          break;
        }
        default: jsonError('invalid escape in JSON string');
      }
      segmentStart = this.position;
    }
  }

  private parseHexQuad(): number {
    if (this.position + 4 > this.source.length) {
      jsonError('incomplete Unicode escape in JSON string');
    }
    let value = 0;
    for (let index = 0; index < 4; index += 1) {
      const code = this.source.charCodeAt(this.position + index);
      let digit: number;
      if (code >= 0x30 && code <= 0x39) digit = code - 0x30;
      else if (code >= 0x61 && code <= 0x66) digit = code - 0x61 + 10;
      else if (code >= 0x41 && code <= 0x46) digit = code - 0x41 + 10;
      else return jsonError('invalid Unicode escape in JSON string');
      value = (value << 4) | digit;
    }
    this.position += 4;
    return value;
  }

  private keyword<T extends boolean | null>(text: string, value: T): T {
    if (this.source.slice(this.position, this.position + text.length) !== text) {
      jsonError('invalid JSON literal');
    }
    this.position += text.length;
    return value;
  }

  private consumeDigits(): void {
    while (this.isDigit(this.source[this.position])) this.position += 1;
  }

  private isDigit(character: string | undefined): boolean {
    return character !== undefined && character >= '0' && character <= '9';
  }

  private skipWhitespace(): void {
    while (this.position < this.source.length) {
      const character = this.source[this.position];
      if (character !== ' ' && character !== '\t' && character !== '\n' && character !== '\r') {
        return;
      }
      this.position += 1;
    }
  }
}

const TOML_OFFSET_DATE_TIME = 'encoding/toml.OffsetDateTime';
const TOML_LOCAL_DATE_TIME = 'encoding/toml.LocalDateTime';
const TOML_LOCAL_DATE = 'encoding/toml.LocalDate';
const TOML_LOCAL_TIME = 'encoding/toml.LocalTime';

function isTomlDateTimeTypeName(name: string | undefined): boolean {
  return name !== undefined
    && [TOML_OFFSET_DATE_TIME, TOML_LOCAL_DATE_TIME, TOML_LOCAL_DATE, TOML_LOCAL_TIME]
      .includes(name);
}

function tomlDateTimeType(token: string): string | undefined {
  const validDate = (year: string, month: string, day: string): boolean => {
    const y = Number(year);
    const m = Number(month);
    const d = Number(day);
    if (m < 1 || m > 12 || d < 1) return false;
    const leap = y % 4 === 0 && (y % 100 !== 0 || y % 400 === 0);
    const days = [31, leap ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    return d <= days[m - 1];
  };
  const validTime = (hour: string, minute: string, second?: string): boolean => (
    Number(hour) <= 23 && Number(minute) <= 59
      && (second === undefined || Number(second) <= 60)
  );
  const date = '([0-9]{4})-([0-9]{2})-([0-9]{2})';
  const time = '([0-9]{2}):([0-9]{2})(?::([0-9]{2})(?:\\.[0-9]+)?)?';
  const dateTime = new RegExp(`^${date}([Tt ])${time}(?:(Z|z)|([+\\-])([0-9]{2}):([0-9]{2}))?$`)
    .exec(token);
  if (dateTime !== null && validDate(dateTime[1], dateTime[2], dateTime[3])
    && validTime(dateTime[5], dateTime[6], dateTime[7])) {
    const hasOffset = dateTime[8] !== undefined || dateTime[9] !== undefined;
    if (dateTime[9] !== undefined
      && (Number(dateTime[10]) > 23 || Number(dateTime[11]) > 59)) return undefined;
    return hasOffset ? TOML_OFFSET_DATE_TIME : TOML_LOCAL_DATE_TIME;
  }
  const localDate = new RegExp(`^${date}$`).exec(token);
  if (localDate !== null && validDate(localDate[1], localDate[2], localDate[3])) {
    return TOML_LOCAL_DATE;
  }
  const localTime = new RegExp(`^${time}$`).exec(token);
  if (localTime !== null && validTime(localTime[1], localTime[2], localTime[3])) {
    return TOML_LOCAL_TIME;
  }
  return undefined;
}

/** Parser for the canonical root/inline TOML transport emitted by the Vo package. */
class CanonicalTomlParser {
  private position = 0;

  constructor(private readonly source: string) {}

  parse(): JsonObject {
    const entries: Array<readonly [string, JsonValue]> = [];
    this.skipWhitespace(true);
    while (this.position < this.source.length) {
      const key = this.parseKey();
      this.skipWhitespace(false);
      this.expect('=');
      this.skipWhitespace(false);
      entries.push([key, this.parseValue(0)]);
      this.skipWhitespace(false);
      if (this.position === this.source.length) break;
      if (this.source[this.position] !== '\n') jsonError('expected newline after TOML field');
      this.skipWhitespace(true);
    }
    return { kind: 'object', entries };
  }

  private parseValue(depth: number): JsonValue {
    if (depth >= MAX_JSON_DEPTH) jsonError('maximum TOML nesting depth exceeded');
    const character = this.source[this.position];
    if (character === '"') return this.parseString();
    if (character === '[') return this.parseArray(depth + 1);
    if (character === '{') return this.parseInlineTable(depth + 1);
    const start = this.position;
    while (this.position < this.source.length
      && ![',', ']', '}', '\n'].includes(this.source[this.position])) {
      this.position += 1;
    }
    const token = this.source.slice(start, this.position).trimEnd();
    if (token === 'true') return true;
    if (token === 'false') return false;
    if (/^[+\-]?(?:inf|nan)$/.test(token)) {
      const text = token.endsWith('inf')
        ? (token.startsWith('-') ? '-Infinity' : 'Infinity')
        : 'NaN';
      return { kind: 'number', text, integer: false };
    }
    const namedType = tomlDateTimeType(token);
    if (namedType !== undefined) return { kind: 'named-string', value: token, typeName: namedType };
    if (/^[+\-]?(?:0|[1-9][0-9]*)$/.test(token)) {
      const integer = BigInt(token);
      if (integer < -0x8000_0000_0000_0000n || integer > 0x7fff_ffff_ffff_ffffn) {
        jsonError('TOML integer out of range');
      }
      return { kind: 'number', text: token.startsWith('+') ? token.slice(1) : token, integer: true };
    }
    if (/^[+\-]?(?:(?:[0-9]+(?:\.[0-9]+)?)(?:[eE][+\-]?[0-9]+)?|[0-9]+\.[0-9]+)$/.test(token)) {
      return { kind: 'number', text: token, integer: false };
    }
    return jsonError('invalid canonical TOML value');
  }

  private parseArray(depth: number): JsonArray {
    this.position += 1;
    this.skipWhitespace(false);
    const values: JsonValue[] = [];
    if (this.source[this.position] === ']') {
      this.position += 1;
      return { kind: 'array', values };
    }
    for (;;) {
      values.push(this.parseValue(depth));
      this.skipWhitespace(false);
      if (this.source[this.position] === ']') {
        this.position += 1;
        return { kind: 'array', values };
      }
      this.expect(',');
      this.skipWhitespace(false);
    }
  }

  private parseInlineTable(depth: number): JsonObject {
    this.position += 1;
    this.skipWhitespace(false);
    const entries: Array<readonly [string, JsonValue]> = [];
    if (this.source[this.position] === '}') {
      this.position += 1;
      return { kind: 'object', entries };
    }
    for (;;) {
      const key = this.parseKey();
      this.skipWhitespace(false);
      this.expect('=');
      this.skipWhitespace(false);
      entries.push([key, this.parseValue(depth)]);
      this.skipWhitespace(false);
      if (this.source[this.position] === '}') {
        this.position += 1;
        return { kind: 'object', entries };
      }
      this.expect(',');
      this.skipWhitespace(false);
    }
  }

  private parseKey(): string {
    if (this.source[this.position] === '"') return this.parseString();
    const start = this.position;
    while (/[A-Za-z0-9_-]/.test(this.source[this.position] ?? '')) this.position += 1;
    if (start === this.position) jsonError('invalid canonical TOML key');
    return this.source.slice(start, this.position);
  }

  private parseString(): string {
    this.position += 1;
    let result = '';
    let segment = this.position;
    while (this.position < this.source.length) {
      const character = this.source[this.position];
      if (character === '"') {
        result += this.source.slice(segment, this.position);
        this.position += 1;
        return result;
      }
      if (character.charCodeAt(0) < 0x20 || character.charCodeAt(0) === 0x7f) {
        jsonError('unescaped control character in TOML string');
      }
      if (character !== '\\') {
        this.position += 1;
        continue;
      }
      result += this.source.slice(segment, this.position);
      this.position += 1;
      const escape = this.source[this.position++];
      const simple: Record<string, string> = {
        '"': '"', '\\': '\\', b: '\b', t: '\t', n: '\n', f: '\f', r: '\r', e: '\x1b',
      };
      if (simple[escape] !== undefined) result += simple[escape];
      else if (escape === 'x' || escape === 'u' || escape === 'U') {
        const digits = escape === 'x' ? 2 : (escape === 'u' ? 4 : 8);
        const text = this.source.slice(this.position, this.position + digits);
        if (!new RegExp(`^[0-9A-Fa-f]{${digits}}$`).test(text)) {
          jsonError('invalid Unicode escape in TOML string');
        }
        const scalar = Number.parseInt(text, 16);
        if (scalar > 0x10ffff || (scalar >= 0xd800 && scalar <= 0xdfff)) {
          jsonError('invalid Unicode scalar in TOML string');
        }
        result += String.fromCodePoint(scalar);
        this.position += digits;
      } else jsonError('invalid escape in TOML string');
      segment = this.position;
    }
    return jsonError('unterminated TOML string');
  }

  private expect(character: string): void {
    if (this.source[this.position] !== character) {
      jsonError(`expected '${character}' in canonical TOML`);
    }
    this.position += 1;
  }

  private skipWhitespace(includeNewlines: boolean): void {
    while (this.position < this.source.length) {
      const character = this.source[this.position];
      if (character === ' ' || character === '\t' || character === '\r'
        || (includeNewlines && character === '\n')) this.position += 1;
      else break;
    }
  }
}

function compareBytes(left: Uint8Array, right: Uint8Array): number {
  const length = Math.min(left.byteLength, right.byteLength);
  for (let index = 0; index < length; index += 1) {
    if (left[index] !== right[index]) return left[index] - right[index];
  }
  return left.byteLength - right.byteLength;
}

function concatBytes(parts: readonly Uint8Array[]): Uint8Array {
  const length = parts.reduce((total, part) => total + part.byteLength, 0);
  if (!Number.isSafeInteger(length) || length > 0xffff_ffff) {
    jsonError('JSON result exceeds the wasm32 contract');
  }
  const output = new Uint8Array(length);
  let offset = 0;
  for (const part of parts) {
    output.set(part, offset);
    offset += part.byteLength;
  }
  return output;
}

function decodeUtf8Rune(bytes: Uint8Array, offset: number): readonly [number, number] {
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
}

function quoteJsonBytes(bytes: Uint8Array): Uint8Array {
  const encoder = new TextEncoder();
  const parts: Uint8Array[] = [Uint8Array.of(0x22)];
  for (let offset = 0; offset < bytes.byteLength;) {
    const [rune, width] = decodeUtf8Rune(bytes, offset);
    if (rune === 0xfffd && width === 1) {
      parts.push(encoder.encode('\\ufffd'));
      offset += 1;
      continue;
    }
    const escape = rune === 0x22 ? '\\"'
      : rune === 0x5c ? '\\\\'
        : rune === 0x08 ? '\\b'
          : rune === 0x0c ? '\\f'
            : rune === 0x0a ? '\\n'
              : rune === 0x0d ? '\\r'
                : rune === 0x09 ? '\\t'
                  : rune < 0x20 ? `\\u${rune.toString(16).padStart(4, '0')}`
                    : undefined;
    parts.push(escape === undefined ? bytes.slice(offset, offset + width) : encoder.encode(escape));
    offset += width;
  }
  parts.push(Uint8Array.of(0x22));
  return concatBytes(parts);
}

function quoteTomlBytes(bytes: Uint8Array): Uint8Array {
  let value: string;
  try {
    value = new TextDecoder('utf-8', { fatal: true }).decode(bytes);
  } catch {
    return jsonError('TOML strings and keys must contain valid UTF-8');
  }
  let output = '"';
  for (const character of value) {
    const code = character.codePointAt(0)!;
    if (character === '"') output += '\\"';
    else if (character === '\\') output += '\\\\';
    else if (code === 0x08) output += '\\b';
    else if (code === 0x09) output += '\\t';
    else if (code === 0x0a) output += '\\n';
    else if (code === 0x0c) output += '\\f';
    else if (code === 0x0d) output += '\\r';
    else if (code < 0x20 || code === 0x7f) output += `\\u${code.toString(16).padStart(4, '0')}`;
    else output += character;
  }
  return new TextEncoder().encode(`${output}"`);
}

function tomlKey(bytes: Uint8Array): Uint8Array {
  let value: string;
  try {
    value = new TextDecoder('utf-8', { fatal: true }).decode(bytes);
  } catch {
    return jsonError('TOML strings and keys must contain valid UTF-8');
  }
  return value.length > 0 && /^[A-Za-z0-9_-]+$/.test(value)
    ? new TextEncoder().encode(value)
    : quoteTomlBytes(bytes);
}

function lookupStructTag(tag: string, key: string): string | undefined {
  let offset = 0;
  for (;;) {
    while (tag[offset] === ' ') offset += 1;
    if (offset === tag.length) return undefined;
    const keyStart = offset;
    while (offset < tag.length) {
      const code = tag.charCodeAt(offset);
      if (code <= 0x20 || code === 0x3a || code === 0x22 || code === 0x7f) break;
      offset += 1;
    }
    if (offset === keyStart || tag[offset] !== ':' || tag[offset + 1] !== '"') {
      return undefined;
    }
    const currentKey = tag.slice(keyStart, offset);
    offset += 2;
    const valueStart = offset;
    while (offset < tag.length) {
      if (tag[offset] === '"') {
        const value = tag.slice(valueStart, offset);
        offset += 1;
        if (currentKey === key) return value;
        break;
      }
      if (tag[offset] === '\\') {
        if (offset + 1 >= tag.length) return undefined;
        offset += 2;
      } else {
        offset += 1;
      }
    }
    if (offset > tag.length || tag[offset - 1] !== '"') return undefined;
  }
}

function asciiEqualFold(left: string, right: string): boolean {
  if (left.length !== right.length) return false;
  for (let index = 0; index < left.length; index += 1) {
    const normalize = (code: number) => (code >= 0x41 && code <= 0x5a ? code + 0x20 : code);
    if (normalize(left.charCodeAt(index)) !== normalize(right.charCodeAt(index))) return false;
  }
  return true;
}

function compareStrings(left: string, right: string): number {
  if (left === right) return 0;
  return left < right ? -1 : 1;
}

function isIntegerKind(kind: number): boolean {
  return kind >= VALUE_KIND_INT && kind <= VALUE_KIND_UINT64;
}

function signedWidth(kind: number): number | undefined {
  if (kind === VALUE_KIND_INT || kind === VALUE_KIND_INT64) return 64;
  if (kind === VALUE_KIND_INT8) return 8;
  if (kind === VALUE_KIND_INT16) return 16;
  if (kind === VALUE_KIND_INT32) return 32;
  return undefined;
}

function unsignedWidth(kind: number): number | undefined {
  if (kind === VALUE_KIND_UINT || kind === VALUE_KIND_UINT64) return 64;
  if (kind === VALUE_KIND_UINT8) return 8;
  if (kind === VALUE_KIND_UINT16) return 16;
  if (kind === VALUE_KIND_UINT32) return 32;
  return undefined;
}

function checkedNumberInteger(value: JsonValue, kind: number): bigint {
  if (typeof value !== 'object' || value === null || value.kind !== 'number' || !value.integer) {
    return jsonError('expected integer');
  }
  const integer = BigInt(value.text);
  const signed = signedWidth(kind);
  if (signed !== undefined) {
    const lower = -(1n << BigInt(signed - 1));
    const upper = (1n << BigInt(signed - 1)) - 1n;
    if (integer < lower || integer > upper) jsonError('signed integer out of range');
    return integer;
  }
  const unsigned = unsignedWidth(kind);
  if (unsigned === undefined) return jsonError('target is not an integer');
  if (integer < 0n || integer > (1n << BigInt(unsigned)) - 1n) {
    jsonError('unsigned integer out of range');
  }
  return integer;
}

export class AotStructuredJsonHost {
  private readonly encoder = new TextEncoder();
  private readonly fields = new Map<number, readonly SerdeField[]>();
  private readonly basicTypes = new Map<number, AotRuntimeType>();
  private readonly namedTypes = new Map<string, AotRuntimeType>();
  private readonly dynamicSliceType?: AotRuntimeType;
  private readonly dynamicMapType?: AotRuntimeType;

  constructor(
    private readonly operations: AotStructuredJsonOperations,
    private readonly format: StructuredFormat = 'json',
  ) {
    for (const type of operations.metadata.types.values()) {
      if (type.tag === 0 && !this.basicTypes.has(type.kind)) this.basicTypes.set(type.kind, type);
      if (type.typeName !== undefined) this.namedTypes.set(type.typeName, type);
    }
    this.dynamicSliceType = [...operations.metadata.types.values()].find((type) => {
      const child = operations.metadata.types.get(type.first);
      return type.tag === 3 && child?.kind === VALUE_KIND_INTERFACE;
    });
    this.dynamicMapType = [...operations.metadata.types.values()].find((type) => {
      const key = operations.metadata.types.get(type.first);
      const value = operations.metadata.types.get(type.second);
      return type.tag === 4 && key?.kind === VALUE_KIND_STRING
        && value?.kind === VALUE_KIND_INTERFACE;
    });
  }

  byteElementMeta(): number {
    const byteType = this.basicTypes.get(VALUE_KIND_UINT8);
    if (byteType === undefined) return jsonError('uint8 runtime metadata is missing');
    return byteType.canonicalMeta;
  }

  private pushNull(output: Uint8Array[]): void {
    if (this.format === 'toml') jsonError('TOML does not support null values');
    output.push(this.encoder.encode('null'));
  }

  private pushFloat(output: Uint8Array[], value: number, bitSize: 32 | 64): void {
    if (this.format === 'json') {
      if (!Number.isFinite(value)) jsonError('NaN/Infinity not supported in JSON');
      output.push(this.encoder.encode(this.operations.formatFloat(value, bitSize)));
      return;
    }
    if (Number.isNaN(value)) output.push(this.encoder.encode('nan'));
    else if (value === Infinity) output.push(this.encoder.encode('inf'));
    else if (value === -Infinity) output.push(this.encoder.encode('-inf'));
    else {
      let formatted = this.operations.formatFloat(value, bitSize);
      if (!/[.eE]/.test(formatted)) formatted += '.0';
      output.push(this.encoder.encode(formatted));
    }
  }

  private pushString(output: Uint8Array[], bytes: Uint8Array, type: AotRuntimeType): void {
    const semanticTomlType = isTomlDateTimeTypeName(type.typeName);
    if (this.format === 'toml' && semanticTomlType) {
      let value: string;
      try {
        value = new TextDecoder('utf-8', { fatal: true }).decode(bytes);
      } catch {
        return jsonError('TOML date/time values must contain valid UTF-8');
      }
      if (tomlDateTimeType(value) !== type.typeName) {
        jsonError('invalid TOML date/time value or category');
      }
      output.push(this.encoder.encode(value));
      return;
    }
    output.push(this.format === 'json' ? quoteJsonBytes(bytes) : quoteTomlBytes(bytes));
  }

  private pushObjectStart(output: Uint8Array[], depth: number): void {
    if (this.format === 'json' || depth !== 0) output.push(Uint8Array.of(0x7b));
  }

  private pushObjectEnd(output: Uint8Array[], depth: number): void {
    if (this.format === 'json' || depth !== 0) output.push(Uint8Array.of(0x7d));
  }

  private pushFieldStart(
    output: Uint8Array[],
    key: Uint8Array,
    first: boolean,
    depth: number,
  ): void {
    if (!first) {
      if (this.format === 'json') output.push(Uint8Array.of(0x2c));
      else if (depth !== 0) output.push(this.encoder.encode(', '));
    }
    output.push(this.format === 'json' ? quoteJsonBytes(key) : tomlKey(key));
    output.push(this.encoder.encode(this.format === 'json' ? ':' : ' = '));
  }

  private pushFieldEnd(output: Uint8Array[], depth: number): void {
    if (this.format === 'toml' && depth === 0) output.push(Uint8Array.of(0x0a));
  }

  marshal(slot0: bigint, slot1: bigint): Uint8Array {
    const parts: Uint8Array[] = [];
    this.marshalInterface(slot0, slot1, parts, 0);
    return concatBytes(parts);
  }

  unmarshal(data: Uint8Array, targetSlot0: bigint, targetSlot1: bigint): void {
    let source: string;
    try {
      source = new TextDecoder('utf-8', { fatal: true }).decode(data);
    } catch {
      return jsonError('invalid UTF-8');
    }
    const parsed = this.format === 'json'
      ? new JsonParser(source).parse()
      : new CanonicalTomlParser(source).parse();
    const targetType = this.typeForRaw(Number(targetSlot0 & 0xffff_ffffn));
    if (targetType.kind !== VALUE_KIND_POINTER) jsonError('target must be pointer');
    if (targetSlot1 === 0n) jsonError('nil pointer');
    const pointed = this.typeForRaw(targetType.first);
    if (pointed.tag !== 5 || pointed.kind !== VALUE_KIND_STRUCT) {
      jsonError('target must point to a struct');
    }
    if (typeof parsed !== 'object' || parsed === null || parsed.kind !== 'object') {
      jsonError('expected object');
    }
    this.validateTyped(pointed, parsed, 0);
    if (pointed.fixedDescriptor === undefined || pointed.storageBytes < 1) {
      throw new Error('Volang struct runtime allocation metadata is incomplete');
    }
    const target = Number(targetSlot1);
    const staged = this.operations.allocateTyped(pointed.storageBytes, pointed.fixedDescriptor);
    new Uint8Array(this.operations.memory().buffer, staged, pointed.storageBytes)
      .set(new Uint8Array(this.operations.memory().buffer, target, pointed.storageBytes));
    this.operations.setReturnRoot(BigInt(pointed.raw), BigInt(staged));
    this.writeStruct(staged, pointed, parsed);
    new Uint8Array(this.operations.memory().buffer, target, pointed.storageBytes)
      .set(new Uint8Array(this.operations.memory().buffer, staged, pointed.storageBytes));
    this.operations.clearReturnRoot();
  }

  private typeForRaw(raw: number): AotRuntimeType {
    const type = this.operations.metadata.types.get(raw);
    if (type === undefined) jsonError(`runtime metadata is missing type ${raw}`);
    return type;
  }

  private basicType(kind: number): AotRuntimeType {
    const type = this.basicTypes.get(kind);
    if (type === undefined) jsonError(`runtime metadata is missing basic kind ${kind}`);
    return type;
  }

  private structFor(type: AotRuntimeType): AotStructType {
    if (type.tag !== 5) return jsonError('expected struct runtime metadata');
    const metadata = this.operations.metadata.structs[type.first];
    if (metadata === undefined) return jsonError('struct runtime metadata is missing');
    return metadata;
  }

  private childType(type: AotRuntimeType): AotRuntimeType {
    return this.typeForRaw(type.first);
  }

  private mapTypes(type: AotRuntimeType): readonly [AotRuntimeType, AotRuntimeType] {
    if (type.tag !== 4) return jsonError('expected map runtime metadata');
    return [this.typeForRaw(type.first), this.typeForRaw(type.second)];
  }

  private structFields(type: AotRuntimeType): readonly SerdeField[] {
    const cached = this.fields.get(type.first);
    if (cached !== undefined) return cached;
    const candidates: SerdeField[] = [];
    let nextOrder = 0;
    const collect = (
      current: AotRuntimeType,
      depth: number,
      embeddedPath: readonly EmbeddedFieldStep[],
      metaPath: readonly number[],
    ): void => {
      for (const field of this.structFor(current).fields) {
        const fieldType = this.typeForRaw(field.typeRaw);
        const rawTag = lookupStructTag(field.tag, this.format);
        const options = rawTag?.split(',') ?? [];
        const tagName = options[0] ?? '';
        const omitEmpty = options.slice(1).includes('omitempty');
        const explicitName = tagName.length !== 0;
        const defaultName = field.exported
          ? String.fromCodePoint(this.operations.lowerSimple(field.name.codePointAt(0)!))
            + field.name.slice(String.fromCodePoint(field.name.codePointAt(0)!).length)
          : field.name;
        const name = explicitName ? tagName : defaultName;
        if (name === '-') continue;
        let flatten: AotRuntimeType | undefined;
        if (field.embedded && !explicitName) {
          if (fieldType.kind === VALUE_KIND_STRUCT && fieldType.tag === 5) flatten = fieldType;
          else if (fieldType.kind === VALUE_KIND_POINTER) {
            const pointed = this.childType(fieldType);
            if (pointed.kind === VALUE_KIND_STRUCT && pointed.tag === 5) flatten = pointed;
          }
        }
        if (flatten !== undefined) {
          if (!metaPath.includes(flatten.first)) {
            collect(
              flatten,
              depth + 1,
              [...embeddedPath, { offset: field.offset, type: fieldType }],
              [...metaPath, flatten.first],
            );
          }
          continue;
        }
        if (!field.exported) continue;
        candidates.push({
          name,
          embeddedPath,
          offset: field.offset,
          type: fieldType,
          omitEmpty,
          depth,
          tagged: explicitName,
          order: nextOrder,
        });
        nextOrder += 1;
      }
    };
    collect(type, 0, [], [type.first]);
    candidates.sort((left, right) => compareStrings(left.name, right.name)
      || left.depth - right.depth
      || Number(right.tagged) - Number(left.tagged)
      || left.order - right.order);
    const selected: SerdeField[] = [];
    for (let start = 0; start < candidates.length;) {
      let end = start + 1;
      while (end < candidates.length && candidates[end].name === candidates[start].name) end += 1;
      const minimumDepth = candidates[start].depth;
      let minimumEnd = start;
      while (minimumEnd < end && candidates[minimumEnd].depth === minimumDepth) minimumEnd += 1;
      const tagged = candidates.slice(start, minimumEnd).filter((field) => field.tagged);
      if (tagged.length === 1) selected.push(tagged[0]);
      else if (tagged.length === 0 && minimumEnd === start + 1) selected.push(candidates[start]);
      start = end;
    }
    selected.sort((left, right) => left.order - right.order);
    this.fields.set(type.first, selected);
    return selected;
  }

  private fieldAddress(root: number, field: SerdeField, materialize: boolean): number | undefined {
    let pointer = root;
    for (const step of field.embeddedPath) {
      const address = pointer + step.offset * SLOT_BYTES;
      if (step.type.kind === VALUE_KIND_STRUCT) {
        pointer = address;
      } else if (step.type.kind === VALUE_KIND_POINTER) {
        let pointee = Number(this.operations.view().getBigUint64(address, true));
        if (pointee === 0) {
          if (!materialize) return undefined;
          const pointed = this.childType(step.type);
          if (pointed.fixedDescriptor === undefined || pointed.storageBytes < 1) {
            throw new Error('embedded pointer allocation metadata is incomplete');
          }
          pointee = this.operations.allocateTyped(pointed.storageBytes, pointed.fixedDescriptor);
          this.operations.view().setBigUint64(address, BigInt(pointee), true);
        }
        pointer = pointee;
      } else {
        return jsonError('embedded field path contains an incompatible type');
      }
    }
    return pointer + field.offset * SLOT_BYTES;
  }

  private findField(type: AotRuntimeType, key: string): SerdeField | undefined {
    const fields = this.structFields(type);
    return fields.find((field) => field.name === key)
      ?? fields.find((field) => asciiEqualFold(field.name, key));
  }

  private validateTyped(type: AotRuntimeType, value: JsonValue, depth: number): void {
    if (isIntegerKind(type.kind)) {
      checkedNumberInteger(value, type.kind);
      return;
    }
    if (type.kind === VALUE_KIND_FLOAT32 || type.kind === VALUE_KIND_FLOAT64) {
      if (typeof value !== 'object' || value === null || value.kind !== 'number') {
        jsonError(type.kind === VALUE_KIND_FLOAT32 ? 'expected float32' : 'expected float64');
      }
      const number = Number(value.text);
      if (this.format === 'json' && !Number.isFinite(number)) {
        jsonError('floating-point value out of range');
      }
      if (type.kind === VALUE_KIND_FLOAT32 && Number.isFinite(number)
        && !Number.isFinite(Math.fround(number))) jsonError('float32 out of range');
      return;
    }
    if (type.kind === VALUE_KIND_BOOL) {
      if (typeof value !== 'boolean') jsonError('expected bool');
      return;
    }
    if (type.kind === VALUE_KIND_STRING) {
      if (value === null) return;
      if (typeof value === 'object' && value.kind === 'named-string') {
        if (this.format !== 'toml' || type.typeName !== value.typeName) {
          jsonError('named string category does not match target type');
        }
        return;
      }
      if (typeof value !== 'string') jsonError('expected string');
      if (this.format === 'toml' && isTomlDateTimeTypeName(type.typeName)) {
        jsonError('ordinary string cannot decode into a TOML date/time type');
      }
      return;
    }
    if (type.kind === VALUE_KIND_STRUCT) {
      if (value === null) return;
      if (typeof value !== 'object' || value.kind !== 'object') jsonError('expected object');
      if (depth >= MAX_JSON_DEPTH) jsonError('max depth exceeded');
      for (const [key, fieldValue] of value.entries) {
        const field = this.findField(type, key);
        if (field !== undefined) this.validateTyped(field.type, fieldValue, depth + 1);
      }
      return;
    }
    if (type.kind === VALUE_KIND_POINTER) {
      if (value === null) return;
      const child = this.childType(type);
      if (child.kind !== VALUE_KIND_STRUCT) jsonError('pointer target must be a struct');
      this.validateTyped(child, value, depth);
      return;
    }
    if (type.kind === VALUE_KIND_SLICE) {
      if (value === null) return;
      if (typeof value !== 'object' || value.kind !== 'array') jsonError('expected array or null for slice');
      if (depth >= MAX_JSON_DEPTH) jsonError('max depth exceeded');
      const child = this.childType(type);
      value.values.forEach((element) => this.validateTyped(child, element, depth + 1));
      return;
    }
    if (type.kind === VALUE_KIND_ARRAY) {
      if (typeof value !== 'object' || value === null || value.kind !== 'array') {
        jsonError('expected array');
      }
      if (value.values.length !== Number(type.length)) {
        jsonError('array length does not match target type');
      }
      if (depth >= MAX_JSON_DEPTH) jsonError('max depth exceeded');
      const child = this.childType(type);
      value.values.forEach((element) => this.validateTyped(child, element, depth + 1));
      return;
    }
    if (type.kind === VALUE_KIND_MAP) {
      if (value === null) return;
      if (typeof value !== 'object' || value.kind !== 'object') jsonError('expected object or null for map');
      if (depth >= MAX_JSON_DEPTH) jsonError('max depth exceeded');
      const [key, child] = this.mapTypes(type);
      if (key.kind !== VALUE_KIND_STRING) jsonError('only map[string]T is supported');
      value.entries.forEach((entry) => this.validateTyped(child, entry[1], depth + 1));
      return;
    }
    if (type.kind === VALUE_KIND_INTERFACE) {
      this.validateInterface(value, depth);
      return;
    }
    jsonError('unsupported target type for unmarshal');
  }

  private validateInterface(value: JsonValue, depth: number): void {
    if (typeof value !== 'object' || value === null) return;
    if (value.kind === 'number' || value.kind === 'named-string') return;
    if (depth >= MAX_JSON_DEPTH) jsonError('max depth exceeded');
    if (value.kind === 'array') {
      if (this.dynamicSliceType === undefined) jsonError('dynamic JSON slice type is unavailable');
      value.values.forEach((element) => this.validateInterface(element, depth + 1));
    } else {
      if (this.dynamicMapType === undefined) jsonError('dynamic JSON map type is unavailable');
      value.entries.forEach((entry) => this.validateInterface(entry[1], depth + 1));
    }
  }

  private writeStruct(address: number, type: AotRuntimeType, value: JsonObject): void {
    for (const [key, fieldValue] of value.entries) {
      const field = this.findField(type, key);
      if (field === undefined) continue;
      const fieldAddress = this.fieldAddress(address, field, true)!;
      this.writeTyped(fieldAddress, field.type, fieldValue);
    }
  }

  private writeTyped(address: number, type: AotRuntimeType, value: JsonValue): void {
    const memory = this.operations.view();
    if (isIntegerKind(type.kind)) {
      const integer = checkedNumberInteger(value, type.kind);
      switch (type.kind) {
        case VALUE_KIND_INT8: memory.setInt8(address, Number(integer)); break;
        case VALUE_KIND_INT16: memory.setInt16(address, Number(integer), true); break;
        case VALUE_KIND_INT32: memory.setInt32(address, Number(integer), true); break;
        case VALUE_KIND_UINT8: memory.setUint8(address, Number(integer)); break;
        case VALUE_KIND_UINT16: memory.setUint16(address, Number(integer), true); break;
        case VALUE_KIND_UINT32: memory.setUint32(address, Number(integer), true); break;
        case VALUE_KIND_INT:
        case VALUE_KIND_INT64: memory.setBigInt64(address, integer, true); break;
        default: memory.setBigUint64(address, integer, true); break;
      }
      return;
    }
    if (type.kind === VALUE_KIND_FLOAT32 || type.kind === VALUE_KIND_FLOAT64) {
      const number = Number((value as JsonNumber).text);
      if (type.kind === VALUE_KIND_FLOAT32) memory.setFloat32(address, Math.fround(number), true);
      else memory.setFloat64(address, number, true);
      return;
    }
    if (type.kind === VALUE_KIND_BOOL) {
      memory.setUint8(address, value ? 1 : 0);
      return;
    }
    if (type.kind === VALUE_KIND_STRING) {
      const stringValue = typeof value === 'object' && value !== null
        && value.kind === 'named-string' ? value.value : value as string | null;
      const reference = stringValue === null ? 0n
        : this.operations.allocateStringBytes(this.encoder.encode(stringValue));
      this.operations.view().setBigUint64(address, reference, true);
      return;
    }
    if (type.kind === VALUE_KIND_STRUCT) {
      if (value !== null) this.writeStruct(address, type, value as JsonObject);
      return;
    }
    if (type.kind === VALUE_KIND_POINTER) {
      if (value === null) {
        memory.setBigUint64(address, 0n, true);
        return;
      }
      const child = this.childType(type);
      if (child.fixedDescriptor === undefined || child.storageBytes < 1) {
        throw new Error('pointer target allocation metadata is incomplete');
      }
      const pointer = this.operations.allocateTyped(child.storageBytes, child.fixedDescriptor);
      this.operations.view().setBigUint64(address, BigInt(pointer), true);
      this.writeStruct(pointer, child, value as JsonObject);
      return;
    }
    if (type.kind === VALUE_KIND_SLICE) {
      if (value === null) {
        memory.setBigUint64(address, 0n, true);
        return;
      }
      const elements = (value as JsonArray).values;
      const child = this.childType(type);
      const sequence = this.allocateSequence(elements.length, child);
      this.operations.view().setBigUint64(address, BigInt(sequence.header), true);
      elements.forEach((element, index) => {
        this.writeTyped(sequence.data + index * child.storageBytes, child, element);
      });
      return;
    }
    if (type.kind === VALUE_KIND_ARRAY) {
      const elements = (value as JsonArray).values;
      const child = this.childType(type);
      const stride = child.slotCount * SLOT_BYTES;
      elements.forEach((element, index) => this.writeTyped(address + index * stride, child, element));
      return;
    }
    if (type.kind === VALUE_KIND_MAP) {
      if (value === null) {
        memory.setBigUint64(address, 0n, true);
        return;
      }
      const map = this.allocateMap(type, (value as JsonObject).entries.length);
      this.operations.view().setBigUint64(address, BigInt(map), true);
      this.fillMap(map, type, (value as JsonObject).entries);
      return;
    }
    if (type.kind === VALUE_KIND_INTERFACE) {
      this.writeInterface(address, value);
      return;
    }
    jsonError('unsupported target type for unmarshal');
  }

  private writeInterface(address: number, value: JsonValue): void {
    if (value === null) {
      this.operations.view().setBigUint64(address, 0n, true);
      this.operations.view().setBigUint64(address + SLOT_BYTES, 0n, true);
      return;
    }
    if (typeof value === 'boolean') {
      const type = this.basicType(VALUE_KIND_BOOL);
      this.operations.view().setBigUint64(address, BigInt(type.raw), true);
      this.operations.view().setBigUint64(address + SLOT_BYTES, value ? 1n : 0n, true);
      return;
    }
    if (typeof value === 'string') {
      const type = this.basicType(VALUE_KIND_STRING);
      const reference = this.operations.allocateStringBytes(this.encoder.encode(value));
      this.operations.view().setBigUint64(address, BigInt(type.raw), true);
      this.operations.view().setBigUint64(address + SLOT_BYTES, reference, true);
      return;
    }
    if (value.kind === 'named-string') {
      const type = this.namedTypes.get(value.typeName);
      if (type === undefined || type.kind !== VALUE_KIND_STRING) {
        jsonError(`runtime metadata is missing named TOML type ${value.typeName}`);
      }
      const reference = this.operations.allocateStringBytes(this.encoder.encode(value.value));
      this.operations.view().setBigUint64(address, BigInt(type.raw), true);
      this.operations.view().setBigUint64(address + SLOT_BYTES, reference, true);
      return;
    }
    if (value.kind === 'number') {
      const type = this.basicType(this.format === 'toml' && value.integer
        ? VALUE_KIND_INT : VALUE_KIND_FLOAT64);
      this.operations.view().setBigUint64(address, BigInt(type.raw), true);
      if (this.format === 'toml' && value.integer) {
        this.operations.view().setBigInt64(address + SLOT_BYTES, BigInt(value.text), true);
      } else {
        this.operations.view().setFloat64(address + SLOT_BYTES, Number(value.text), true);
      }
      return;
    }
    if (value.kind === 'array') {
      const type = this.dynamicSliceType!;
      const child = this.childType(type);
      const sequence = this.allocateSequence(value.values.length, child);
      this.operations.view().setBigUint64(address, BigInt(VALUE_KIND_SLICE), true);
      this.operations.view().setBigUint64(address + SLOT_BYTES, BigInt(sequence.header), true);
      value.values.forEach((element, index) => {
        this.writeInterface(sequence.data + index * child.storageBytes, element);
      });
      return;
    }
    const type = this.dynamicMapType!;
    const map = this.allocateMap(type, value.entries.length);
    this.operations.view().setBigUint64(address, BigInt(VALUE_KIND_MAP), true);
    this.operations.view().setBigUint64(address + SLOT_BYTES, BigInt(map), true);
    this.fillMap(map, type, value.entries);
  }

  private allocateSequence(length: number, child: AotRuntimeType): { header: number; data: number } {
    const bytes = 32 + length * child.storageBytes;
    if (!Number.isSafeInteger(bytes) || bytes > 0xffff_ffff) {
      jsonError('JSON array allocation exceeds the wasm32 contract');
    }
    const header = this.operations.allocateSequence(bytes, child.canonicalMeta);
    const data = header + 32;
    const memory = this.operations.view();
    memory.setBigUint64(header, BigInt(data), true);
    memory.setBigUint64(header + 8, BigInt(length), true);
    memory.setBigUint64(header + 16, BigInt(length), true);
    memory.setBigUint64(header + 24, BigInt(child.storageBytes), true);
    return { header, data };
  }

  private allocateMap(type: AotRuntimeType, entryCount: number): number {
    const [key, value] = this.mapTypes(type);
    if (key.kind !== VALUE_KIND_STRING || type.mapDescriptor === undefined
      || type.mapEntriesDescriptor === undefined) {
      return jsonError('map runtime allocation metadata is incomplete');
    }
    const keyBytes = key.slotCount * SLOT_BYTES;
    const valueBytes = value.slotCount * SLOT_BYTES;
    const stride = SLOT_BYTES + keyBytes + valueBytes;
    let capacity = 8;
    while (entryCount * 4 > capacity * 3) capacity *= 2;
    const bytes = 64 + capacity * stride;
    if (!Number.isSafeInteger(bytes) || bytes > 0xffff_ffff) {
      jsonError('JSON map allocation exceeds the wasm32 contract');
    }
    const map = this.operations.allocateTyped(bytes, type.mapDescriptor);
    const memory = this.operations.view();
    memory.setBigUint64(map, 0n, true);
    memory.setBigUint64(map + 8, BigInt(capacity), true);
    memory.setBigUint64(map + 16, BigInt(keyBytes), true);
    memory.setBigUint64(map + 24, BigInt(valueBytes), true);
    memory.setBigUint64(map + 32, BigInt(map + 64), true);
    memory.setBigUint64(map + 40, BigInt(key.canonicalMeta), true);
    memory.setBigUint64(map + 48, BigInt(key.raw), true);
    memory.setBigUint64(map + 56, BigInt(type.mapEntriesDescriptor), true);
    return map;
  }

  private fillMap(
    map: number,
    type: AotRuntimeType,
    entries: readonly (readonly [string, JsonValue])[],
  ): void {
    const [keyType, valueType] = this.mapTypes(type);
    const keyBytes = keyType.slotCount * SLOT_BYTES;
    for (const [key, value] of entries) {
      const encoded = this.encoder.encode(key);
      const keyReference = this.operations.allocateStringBytes(encoded);
      const bucket = this.mapBucket(map, encoded);
      const occupied = this.operations.view().getBigUint64(bucket, true) === 1n;
      if (!occupied) {
        this.operations.view().setBigUint64(bucket, 1n, true);
        this.operations.view().setBigUint64(bucket + SLOT_BYTES, keyReference, true);
        const length = this.operations.view().getBigUint64(map, true);
        this.operations.view().setBigUint64(map, length + 1n, true);
      }
      this.writeTyped(bucket + SLOT_BYTES + keyBytes, valueType, value);
    }
  }

  private mapBucket(map: number, key: Uint8Array): number {
    const memory = this.operations.view();
    const capacity = Number(memory.getBigUint64(map + 8, true));
    const keyBytes = Number(memory.getBigUint64(map + 16, true));
    const valueBytes = Number(memory.getBigUint64(map + 24, true));
    const data = Number(memory.getBigUint64(map + 32, true));
    const stride = SLOT_BYTES + keyBytes + valueBytes;
    let hash = 0x811c9dc5;
    for (const byte of key) hash = Math.imul((hash ^ byte) >>> 0, 0x01000193) >>> 0;
    for (let probes = 0; probes < capacity; probes += 1) {
      const index = (hash + probes) & (capacity - 1);
      const bucket = data + index * stride;
      const state = memory.getBigUint64(bucket, true);
      if (state === 0n) return bucket;
      if (state === 1n) {
        const stored = this.readStringBytes(memory.getBigUint64(bucket + SLOT_BYTES, true));
        if (compareBytes(stored, key) === 0) return bucket;
      }
    }
    return jsonError('JSON map capacity invariant was violated');
  }

  private marshalInterface(
    slot0: bigint,
    slot1: bigint,
    output: Uint8Array[],
    depth: number,
  ): void {
    const kind = Number(slot0 & 0xffn);
    if (kind === VALUE_KIND_VOID) {
      this.pushNull(output);
      return;
    }
    if (kind === VALUE_KIND_MAP && Number(slot0 & 0xffff_ffffn) === VALUE_KIND_MAP) {
      if (this.dynamicMapType === undefined) jsonError('dynamic JSON map type is unavailable');
      this.marshalMap(Number(slot1), this.dynamicMapType, output, depth);
      return;
    }
    if (kind === VALUE_KIND_SLICE && Number(slot0 & 0xffff_ffffn) === VALUE_KIND_SLICE) {
      if (this.dynamicSliceType === undefined) jsonError('dynamic JSON slice type is unavailable');
      this.marshalSequence(Number(slot1), this.dynamicSliceType, output, depth);
      return;
    }
    const type = this.typeForRaw(Number(slot0 & 0xffff_ffffn));
    if (type.kind !== kind) jsonError('interface type metadata is inconsistent');
    if (kind === VALUE_KIND_STRUCT) {
      if (slot1 === 0n) this.pushNull(output);
      else this.marshalStruct(Number(slot1), type, output, depth);
    } else if (kind === VALUE_KIND_POINTER) {
      if (slot1 === 0n) this.pushNull(output);
      else {
        const child = this.childType(type);
        if (child.kind !== VALUE_KIND_STRUCT) jsonError('pointer target must be a struct');
        this.marshalStruct(Number(slot1), child, output, depth);
      }
    } else if (kind === VALUE_KIND_SLICE || kind === VALUE_KIND_ARRAY) {
      this.marshalSequence(Number(slot1), type, output, depth);
    } else if (kind === VALUE_KIND_MAP) {
      this.marshalMap(Number(slot1), type, output, depth);
    } else {
      this.marshalScalarSlot(slot1, type, output);
    }
  }

  private marshalScalarSlot(value: bigint, type: AotRuntimeType, output: Uint8Array[]): void {
    if (isIntegerKind(type.kind)) {
      const width = signedWidth(type.kind);
      const number = width === undefined ? BigInt.asUintN(unsignedWidth(type.kind)!, value)
        : BigInt.asIntN(width, value);
      if (this.format === 'toml' && signedWidth(type.kind) === undefined
        && number > 0x7fff_ffff_ffff_ffffn) {
        jsonError('unsigned TOML integer exceeds signed 64-bit range');
      }
      output.push(this.encoder.encode(number.toString()));
      return;
    }
    if (type.kind === VALUE_KIND_FLOAT32) {
      const storage = new DataView(new ArrayBuffer(4));
      storage.setUint32(0, Number(value & 0xffff_ffffn), true);
      const number = storage.getFloat32(0, true);
      this.pushFloat(output, number, 32);
      return;
    }
    if (type.kind === VALUE_KIND_FLOAT64) {
      const storage = new DataView(new ArrayBuffer(8));
      storage.setBigUint64(0, value, true);
      const number = storage.getFloat64(0, true);
      this.pushFloat(output, number, 64);
      return;
    }
    if (type.kind === VALUE_KIND_BOOL) {
      output.push(this.encoder.encode(value === 0n ? 'false' : 'true'));
      return;
    }
    if (type.kind === VALUE_KIND_STRING) {
      this.pushString(output, this.readStringBytes(value), type);
      return;
    }
    jsonError('unsupported interface value type for marshal');
  }

  private marshalTyped(address: number, type: AotRuntimeType, output: Uint8Array[], depth: number): void {
    const memory = this.operations.view();
    if (isIntegerKind(type.kind)) {
      let value: bigint;
      switch (type.kind) {
        case VALUE_KIND_INT8: value = BigInt(memory.getInt8(address)); break;
        case VALUE_KIND_INT16: value = BigInt(memory.getInt16(address, true)); break;
        case VALUE_KIND_INT32: value = BigInt(memory.getInt32(address, true)); break;
        case VALUE_KIND_UINT8: value = BigInt(memory.getUint8(address)); break;
        case VALUE_KIND_UINT16: value = BigInt(memory.getUint16(address, true)); break;
        case VALUE_KIND_UINT32: value = BigInt(memory.getUint32(address, true)); break;
        case VALUE_KIND_INT:
        case VALUE_KIND_INT64: value = memory.getBigInt64(address, true); break;
        default: value = memory.getBigUint64(address, true); break;
      }
      if (this.format === 'toml' && signedWidth(type.kind) === undefined
        && value > 0x7fff_ffff_ffff_ffffn) {
        jsonError('unsigned TOML integer exceeds signed 64-bit range');
      }
      output.push(this.encoder.encode(value.toString()));
      return;
    }
    if (type.kind === VALUE_KIND_FLOAT32 || type.kind === VALUE_KIND_FLOAT64) {
      const value = type.kind === VALUE_KIND_FLOAT32
        ? memory.getFloat32(address, true) : memory.getFloat64(address, true);
      this.pushFloat(output, value, type.kind === VALUE_KIND_FLOAT32 ? 32 : 64);
      return;
    }
    if (type.kind === VALUE_KIND_BOOL) {
      output.push(this.encoder.encode(memory.getUint8(address) === 0 ? 'false' : 'true'));
      return;
    }
    if (type.kind === VALUE_KIND_STRING) {
      this.pushString(output, this.readStringBytes(memory.getBigUint64(address, true)), type);
      return;
    }
    if (type.kind === VALUE_KIND_STRUCT) {
      this.marshalStruct(address, type, output, depth + 1);
      return;
    }
    if (type.kind === VALUE_KIND_POINTER) {
      const pointer = Number(memory.getBigUint64(address, true));
      if (pointer === 0) this.pushNull(output);
      else {
        const child = this.childType(type);
        if (child.kind !== VALUE_KIND_STRUCT) jsonError('pointer target must be a struct');
        this.marshalStruct(pointer, child, output, depth + 1);
      }
      return;
    }
    if (type.kind === VALUE_KIND_INTERFACE) {
      this.marshalInterface(
        memory.getBigUint64(address, true),
        memory.getBigUint64(address + SLOT_BYTES, true),
        output,
        depth + 1,
      );
      return;
    }
    if (type.kind === VALUE_KIND_SLICE) {
      this.marshalSequence(Number(memory.getBigUint64(address, true)), type, output, depth + 1);
      return;
    }
    if (type.kind === VALUE_KIND_ARRAY) {
      this.marshalInlineArray(address, type, output, depth + 1);
      return;
    }
    if (type.kind === VALUE_KIND_MAP) {
      this.marshalMap(Number(memory.getBigUint64(address, true)), type, output, depth + 1);
      return;
    }
    jsonError('unsupported value type for marshal');
  }

  private marshalStruct(
    address: number,
    type: AotRuntimeType,
    output: Uint8Array[],
    depth: number,
  ): void {
    if (depth >= MAX_JSON_DEPTH) jsonError('max depth exceeded (possible cycle)');
    this.pushObjectStart(output, depth);
    let first = true;
    for (const field of this.structFields(type)) {
      const fieldAddress = this.fieldAddress(address, field, false);
      if (fieldAddress === undefined) continue;
      if (field.omitEmpty && this.isEmpty(fieldAddress, field.type)) continue;
      this.pushFieldStart(output, this.encoder.encode(field.name), first, depth);
      first = false;
      this.marshalTyped(fieldAddress, field.type, output, depth);
      this.pushFieldEnd(output, depth);
    }
    this.pushObjectEnd(output, depth);
  }

  private marshalSequence(
    reference: number,
    type: AotRuntimeType,
    output: Uint8Array[],
    depth: number,
  ): void {
    if (reference === 0) {
      this.pushNull(output);
      return;
    }
    if (depth >= MAX_JSON_DEPTH) jsonError('max depth exceeded (possible cycle)');
    const child = this.childType(type);
    const memory = this.operations.view();
    const data = Number(memory.getBigUint64(reference, true));
    const length = Number(memory.getBigUint64(reference + 8, true));
    const stride = Number(memory.getBigUint64(reference + 24, true));
    if (stride !== child.storageBytes) jsonError('sequence runtime layout does not match type metadata');
    output.push(Uint8Array.of(0x5b));
    for (let index = 0; index < length; index += 1) {
      if (index !== 0) {
        output.push(this.encoder.encode(this.format === 'json' ? ',' : ', '));
      }
      this.marshalTyped(data + index * stride, child, output, depth);
    }
    output.push(Uint8Array.of(0x5d));
  }

  private marshalInlineArray(
    address: number,
    type: AotRuntimeType,
    output: Uint8Array[],
    depth: number,
  ): void {
    if (depth >= MAX_JSON_DEPTH) jsonError('max depth exceeded (possible cycle)');
    const child = this.childType(type);
    const length = Number(type.length);
    const stride = child.slotCount * SLOT_BYTES;
    output.push(Uint8Array.of(0x5b));
    for (let index = 0; index < length; index += 1) {
      if (index !== 0) {
        output.push(this.encoder.encode(this.format === 'json' ? ',' : ', '));
      }
      this.marshalTyped(address + index * stride, child, output, depth);
    }
    output.push(Uint8Array.of(0x5d));
  }

  private marshalMap(
    reference: number,
    type: AotRuntimeType,
    output: Uint8Array[],
    depth: number,
  ): void {
    if (reference === 0) {
      this.pushNull(output);
      return;
    }
    if (depth >= MAX_JSON_DEPTH) jsonError('max depth exceeded (possible cycle)');
    const [keyType, valueType] = this.mapTypes(type);
    if (keyType.kind !== VALUE_KIND_STRING) jsonError('unsupported map key type');
    const memory = this.operations.view();
    const capacity = Number(memory.getBigUint64(reference + 8, true));
    const keyBytes = Number(memory.getBigUint64(reference + 16, true));
    const valueBytes = Number(memory.getBigUint64(reference + 24, true));
    const data = Number(memory.getBigUint64(reference + 32, true));
    if (capacity < 8 || (capacity & (capacity - 1)) !== 0
      || keyBytes !== keyType.slotCount * SLOT_BYTES
      || valueBytes !== valueType.slotCount * SLOT_BYTES) {
      jsonError('map runtime layout does not match type metadata');
    }
    const stride = SLOT_BYTES + keyBytes + valueBytes;
    const entries: Array<{ key: Uint8Array; valueAddress: number }> = [];
    for (let index = 0; index < capacity; index += 1) {
      const bucket = data + index * stride;
      if (memory.getBigUint64(bucket, true) !== 1n) continue;
      entries.push({
        key: this.readStringBytes(memory.getBigUint64(bucket + SLOT_BYTES, true)).slice(),
        valueAddress: bucket + SLOT_BYTES + keyBytes,
      });
    }
    entries.sort((left, right) => compareBytes(left.key, right.key));
    this.pushObjectStart(output, depth);
    entries.forEach((entry, index) => {
      this.pushFieldStart(output, entry.key, index === 0, depth);
      this.marshalTyped(entry.valueAddress, valueType, output, depth);
      this.pushFieldEnd(output, depth);
    });
    this.pushObjectEnd(output, depth);
  }

  private readStringBytes(reference: bigint): Uint8Array {
    if (reference === 0n) return new Uint8Array();
    const header = Number(reference);
    const memory = this.operations.view();
    const length = Number(memory.getBigUint64(header, true));
    const data = Number(memory.getBigUint64(header + 8, true));
    return new Uint8Array(this.operations.memory().buffer, data, length);
  }

  private isEmpty(address: number, type: AotRuntimeType): boolean {
    const memory = this.operations.view();
    if (type.kind === VALUE_KIND_BOOL || type.kind === VALUE_KIND_INT8
      || type.kind === VALUE_KIND_UINT8) return memory.getUint8(address) === 0;
    if (type.kind === VALUE_KIND_INT16 || type.kind === VALUE_KIND_UINT16) {
      return memory.getUint16(address, true) === 0;
    }
    if (type.kind === VALUE_KIND_INT32 || type.kind === VALUE_KIND_UINT32) {
      return memory.getUint32(address, true) === 0;
    }
    if (type.kind === VALUE_KIND_FLOAT32) return memory.getFloat32(address, true) === 0;
    if (type.kind === VALUE_KIND_FLOAT64) return memory.getFloat64(address, true) === 0;
    if (isIntegerKind(type.kind)) return memory.getBigUint64(address, true) === 0n;
    if (type.kind === VALUE_KIND_STRING) {
      const reference = memory.getBigUint64(address, true);
      return reference === 0n || this.readStringBytes(reference).byteLength === 0;
    }
    if (type.kind === VALUE_KIND_POINTER) return memory.getBigUint64(address, true) === 0n;
    if (type.kind === VALUE_KIND_INTERFACE) {
      return Number(memory.getBigUint64(address, true) & 0xffn) === VALUE_KIND_VOID;
    }
    if (type.kind === VALUE_KIND_SLICE) {
      const reference = Number(memory.getBigUint64(address, true));
      return reference === 0 || memory.getBigUint64(reference + 8, true) === 0n;
    }
    if (type.kind === VALUE_KIND_MAP) {
      const reference = Number(memory.getBigUint64(address, true));
      return reference === 0 || memory.getBigUint64(reference, true) === 0n;
    }
    if (type.kind === VALUE_KIND_ARRAY) return type.length === 0n;
    return type.kind === VALUE_KIND_VOID;
  }
}
