import type { AotExternCall, AotExternDescriptor } from './index.js';

const VALUE_KIND_BOOL = 1;
const VALUE_KIND_INT = 2;
const VALUE_KIND_FLOAT64 = 13;
const VALUE_KIND_STRING = 17;
const OPERATIONS = new Set(['nativeSscan', 'nativeSscanf']);

function canonicalExternName(packageName: string, functionName: string): string {
  const encoder = new TextEncoder();
  return `vo1:${encoder.encode(packageName).byteLength}:${packageName}`
    + `:${encoder.encode(functionName).byteLength}:${functionName}`;
}

function operation(descriptor: AotExternDescriptor): string | undefined {
  if (descriptor.source !== 1) return undefined;
  for (const name of OPERATIONS) {
    if (descriptor.name === canonicalExternName('fmt', name)) return name;
  }
  return undefined;
}

function packBasicInterface(kind: number, value: bigint): readonly [bigint, bigint] {
  return [BigInt((kind << 8) | kind), BigInt.asUintN(64, value)];
}

function stringInterface(reference: bigint): readonly [bigint, bigint] {
  return packBasicInterface(VALUE_KIND_STRING, reference);
}

function decodeFirst(bytes: Uint8Array, offset: number): readonly [number, number] {
  const first = bytes[offset];
  const continuation = (index: number) => index < bytes.byteLength
    && bytes[index] >= 0x80 && bytes[index] <= 0xbf;
  if (first < 0x80) return [first, 1];
  if (first >= 0xc2 && first <= 0xdf && continuation(offset + 1)) {
    return [((first & 0x1f) << 6) | (bytes[offset + 1] & 0x3f), 2];
  }
  if (first >= 0xe0 && first <= 0xef && continuation(offset + 1)
    && continuation(offset + 2) && (first !== 0xe0 || bytes[offset + 1] >= 0xa0)
    && (first !== 0xed || bytes[offset + 1] <= 0x9f)) {
    return [((first & 0x0f) << 12) | ((bytes[offset + 1] & 0x3f) << 6)
      | (bytes[offset + 2] & 0x3f), 3];
  }
  if (first >= 0xf0 && first <= 0xf4 && continuation(offset + 1)
    && continuation(offset + 2) && continuation(offset + 3)
    && (first !== 0xf0 || bytes[offset + 1] >= 0x90)
    && (first !== 0xf4 || bytes[offset + 1] <= 0x8f)) {
    return [((first & 0x07) << 18) | ((bytes[offset + 1] & 0x3f) << 12)
      | ((bytes[offset + 2] & 0x3f) << 6) | (bytes[offset + 3] & 0x3f), 4];
  }
  return [0xfffd, 1];
}

function isSpace(codePoint: number): boolean {
  return (codePoint >= 0x09 && codePoint <= 0x0d) || codePoint === 0x20
    || codePoint === 0x85 || codePoint === 0xa0 || codePoint === 0x1680
    || (codePoint >= 0x2000 && codePoint <= 0x200a)
    || codePoint === 0x2028 || codePoint === 0x2029 || codePoint === 0x202f
    || codePoint === 0x205f || codePoint === 0x3000;
}

function skipWhitespace(input: Uint8Array, initial: number): number {
  let offset = initial;
  while (offset < input.byteLength) {
    const [codePoint, width] = decodeFirst(input, offset);
    if (!isSpace(codePoint)) break;
    offset += width;
  }
  return offset;
}

function takeUntilWhitespace(input: Uint8Array, initial: number): number {
  let offset = initial;
  while (offset < input.byteLength) {
    const [codePoint, width] = decodeFirst(input, offset);
    if (isSpace(codePoint)) break;
    offset += width;
  }
  return offset;
}

function splitWhitespace(input: Uint8Array): Uint8Array[] {
  const result: Uint8Array[] = [];
  let offset = 0;
  while (offset < input.byteLength) {
    offset = skipWhitespace(input, offset);
    if (offset === input.byteLength) break;
    const end = takeUntilWhitespace(input, offset);
    result.push(input.slice(offset, end));
    offset = end;
  }
  return result;
}

function ascii(bytes: Uint8Array, description: string): string {
  if (bytes.some((byte) => byte >= 0x80)) {
    throw new Error(`${description} requires an ASCII numeric token`);
  }
  return String.fromCharCode(...bytes);
}

function scanRadix(
  input: Uint8Array,
  initial: number,
  radix: number,
  prefixLetter: number,
  description: string,
): readonly [bigint, number] {
  let offset = initial;
  let negative = false;
  if (input[offset] === 45 || input[offset] === 43) {
    negative = input[offset] === 45;
    offset += 1;
  }
  if (input[offset] === 48 && offset + 1 < input.byteLength
    && (input[offset + 1] | 0x20) === prefixLetter) offset += 2;
  const start = offset;
  while (offset < input.byteLength) {
    const byte = input[offset];
    const valid = radix === 2 ? byte === 48 || byte === 49
      : (radix === 8 ? byte >= 48 && byte <= 55
        : (byte >= 48 && byte <= 57) || (byte | 0x20) >= 97 && (byte | 0x20) <= 102);
    if (!valid) break;
    offset += 1;
  }
  if (offset === start) throw new Error(`expected digits for ${description}`);
  const digits = ascii(input.slice(start, offset), description);
  let magnitude = 0n;
  for (const character of digits) {
    const digit = character >= '0' && character <= '9'
      ? character.charCodeAt(0) - 48 : (character.toLowerCase().charCodeAt(0) - 87);
    magnitude = magnitude * BigInt(radix) + BigInt(digit);
  }
  const maximum = negative ? 1n << 63n : (1n << 63n) - 1n;
  if (magnitude > maximum) throw new Error(`${description} overflows int`);
  return [negative ? -magnitude : magnitude, offset];
}

function floatBits(value: number): bigint {
  const storage = new DataView(new ArrayBuffer(8));
  storage.setFloat64(0, value, true);
  return storage.getBigUint64(0, true);
}

function scanFormatted(
  call: AotExternCall,
  input: Uint8Array,
  format: Uint8Array,
): readonly (readonly [bigint, bigint])[] {
  const result: Array<readonly [bigint, bigint]> = [];
  let formatOffset = 0;
  let inputOffset = 0;
  while (formatOffset < format.byteLength) {
    const [formatRune, formatWidth] = decodeFirst(format, formatOffset);
    if (format[formatOffset] === 37) {
      formatOffset += 1;
      if (format[formatOffset] === 37) {
        formatOffset += 1;
        if (input[inputOffset] !== 37) throw new Error(`expected '%%' at position ${inputOffset}`);
        inputOffset += 1;
        continue;
      }
      if (formatOffset >= format.byteLength) throw new Error('incomplete format verb');
      const verbByte = format[formatOffset];
      if (verbByte >= 0x80) throw new Error(`non-ASCII scan verb at byte ${formatOffset}`);
      const verb = String.fromCharCode(verbByte);
      formatOffset += 1;
      if (verb !== 'c') inputOffset = skipWhitespace(input, inputOffset);
      if (inputOffset >= input.byteLength) throw new Error(`unexpected end of input for %${verb}`);
      if (verb === 'd') {
        const start = inputOffset;
        if (input[inputOffset] === 45 || input[inputOffset] === 43) inputOffset += 1;
        while (inputOffset < input.byteLength && input[inputOffset] >= 48
          && input[inputOffset] <= 57) inputOffset += 1;
        const token = ascii(input.slice(start, inputOffset), '%d');
        if (!/^[+\-]?\d+$/.test(token)) throw new Error('expected integer for %d');
        const value = BigInt(token);
        if (value < -(1n << 63n) || value > (1n << 63n) - 1n) throw new Error('expected integer for %d: overflow');
        result.push(packBasicInterface(VALUE_KIND_INT, value));
      } else if (verb === 'f' || verb === 'e' || verb === 'g') {
        const start = inputOffset;
        if (input[inputOffset] === 45 || input[inputOffset] === 43) inputOffset += 1;
        while (inputOffset < input.byteLength) {
          const byte = input[inputOffset];
          if ((byte >= 48 && byte <= 57) || byte === 46 || byte === 101 || byte === 69
            || ((byte === 45 || byte === 43) && inputOffset > start)) inputOffset += 1;
          else break;
        }
        const token = ascii(input.slice(start, inputOffset), 'float');
        const value = Number(token);
        if (token.length === 0 || Number.isNaN(value)) throw new Error(`expected float for %${verb}`);
        result.push(packBasicInterface(VALUE_KIND_FLOAT64, floatBits(value)));
      } else if (verb === 's' || verb === 'v') {
        const end = takeUntilWhitespace(input, inputOffset);
        const reference = call.allocateStringBytes(input.slice(inputOffset, end));
        result.push(stringInterface(reference));
        inputOffset = end;
      } else if (verb === 't') {
        const end = takeUntilWhitespace(input, inputOffset);
        const token = ascii(input.slice(inputOffset, end), '%t');
        if (!['true', 'TRUE', '1', 'false', 'FALSE', '0'].includes(token)) {
          throw new Error('expected bool for %t');
        }
        result.push(packBasicInterface(
          VALUE_KIND_BOOL,
          ['true', 'TRUE', '1'].includes(token) ? 1n : 0n,
        ));
        inputOffset = end;
      } else if (verb === 'c') {
        const [codePoint, width] = decodeFirst(input, inputOffset);
        result.push(packBasicInterface(VALUE_KIND_INT, BigInt(codePoint)));
        inputOffset += width;
      } else if (verb === 'x' || verb === 'X' || verb === 'o' || verb === 'b') {
        const radix = verb === 'o' ? 8 : (verb === 'b' ? 2 : 16);
        const prefix = verb === 'o' ? 111 : (verb === 'b' ? 98 : 120);
        const [value, end] = scanRadix(input, inputOffset, radix, prefix, `%${verb}`);
        result.push(packBasicInterface(VALUE_KIND_INT, value));
        inputOffset = end;
      } else throw new Error(`unsupported scan verb '%${verb}'`);
    } else if (isSpace(formatRune)) {
      formatOffset += formatWidth;
      inputOffset = skipWhitespace(input, inputOffset);
    } else {
      const literal = format.slice(formatOffset, formatOffset + formatWidth);
      const actual = input.slice(inputOffset, inputOffset + formatWidth);
      if (literal.byteLength !== actual.byteLength
        || literal.some((byte, index) => byte !== actual[index])) {
        throw new Error(`format literal mismatch at byte ${inputOffset}`);
      }
      formatOffset += formatWidth;
      inputOffset += formatWidth;
    }
  }
  return result;
}

export class AotFmtScanHost {
  static supportsDescriptor(descriptor: AotExternDescriptor): boolean {
    return operation(descriptor) !== undefined;
  }

  supports(descriptor: AotExternDescriptor): boolean {
    return AotFmtScanHost.supportsDescriptor(descriptor);
  }

  handle(call: AotExternCall): void {
    const name = operation(call.descriptor);
    if (name === undefined) throw new Error(`unsupported AOT fmt extern ${call.name}`);
    const input = call.readStringBytes(call.readSlot(call.argumentsStart));
    if (name === 'nativeSscan') {
      const values = splitWhitespace(input).map((token) => (
        stringInterface(call.allocateStringBytes(token))
      ));
      call.writeSlot(call.destination, call.allocateInterfaceSlice(values));
      return;
    }
    try {
      const format = call.readStringBytes(call.readSlot(call.argumentsStart + 1));
      const values = scanFormatted(call, input, format);
      call.writeSlot(call.destination, call.allocateInterfaceSlice(values));
      call.clearError(call.destination + 1);
    } catch (error) {
      call.writeSlot(call.destination, 0n);
      call.writeError(call.destination + 1, (error as Error).message);
    }
  }
}
