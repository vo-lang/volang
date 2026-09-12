/** Pure IEEE-754 conversion and arithmetic semantics shared by AOT host providers. */
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

export function parseVolangFloat(text: string, bitSizeValue: bigint): {
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

export function formatVolangFloat(
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

export function bitLength(value: bigint): number {
  return value === 0n ? 0 : value.toString(2).length;
}

export function float64FromBits(bits: bigint): number {
  const storage = new DataView(new ArrayBuffer(8));
  storage.setBigUint64(0, BigInt.asUintN(64, bits), true);
  return storage.getFloat64(0, true);
}

function float32FromBits(bits: number): number {
  const storage = new DataView(new ArrayBuffer(4));
  storage.setUint32(0, bits >>> 0, true);
  return storage.getFloat32(0, true);
}

export function float32Bits(value: number): number {
  const storage = new DataView(new ArrayBuffer(4));
  storage.setFloat32(0, value, true);
  return storage.getUint32(0, true);
}

function float64Bits(value: number): bigint {
  const storage = new DataView(new ArrayBuffer(8));
  storage.setFloat64(0, value, true);
  return storage.getBigUint64(0, true);
}

export function copyFloat64Sign(value: number, signSource: number): number {
  return float64FromBits(
    (float64Bits(value) & 0x7fff_ffff_ffff_ffffn)
      | (float64Bits(signSource) & 0x8000_0000_0000_0000n),
  );
}

export function frexpFloat64(value: number): readonly [number, bigint] {
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

export function ldexpFloat64(fraction: number, requestedExponent: bigint): number {
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
export function fusedMultiplyAdd(x: number, y: number, z: number): number {
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
