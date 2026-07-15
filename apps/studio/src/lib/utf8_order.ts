const encoder = new TextEncoder();

/** Compare strings by the UTF-8 byte order used by Rust wire formats. */
export function compareUtf8(left: string, right: string): number {
  if (left === right) return 0;
  const leftBytes = encoder.encode(left);
  const rightBytes = encoder.encode(right);
  const length = Math.min(leftBytes.byteLength, rightBytes.byteLength);
  for (let index = 0; index < length; index += 1) {
    if (leftBytes[index] !== rightBytes[index]) {
      return leftBytes[index] - rightBytes[index];
    }
  }
  if (leftBytes.byteLength !== rightBytes.byteLength) {
    return leftBytes.byteLength - rightBytes.byteLength;
  }
  // TextEncoder replaces isolated surrogates. Keep ordering total even if a
  // non-protocol caller supplies malformed UTF-16.
  return left < right ? -1 : 1;
}
