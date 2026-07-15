import {
  UNICODE_CASE_FOLD_EXCEPTIONS,
  UNICODE_CASE_FOLD_RANGES,
} from './unicode_casefold_data.mjs';

const FULL_CASE_FOLD = new Map();
for (let index = 0; index < UNICODE_CASE_FOLD_RANGES.length; index += 4) {
  const start = UNICODE_CASE_FOLD_RANGES[index];
  const end = UNICODE_CASE_FOLD_RANGES[index + 1];
  const step = UNICODE_CASE_FOLD_RANGES[index + 2];
  const delta = UNICODE_CASE_FOLD_RANGES[index + 3];
  for (let source = start; source <= end; source += step) {
    FULL_CASE_FOLD.set(source, String.fromCodePoint(source + delta));
  }
}
for (let index = 0; index < UNICODE_CASE_FOLD_EXCEPTIONS.length; index += 2) {
  FULL_CASE_FOLD.set(
    UNICODE_CASE_FOLD_EXCEPTIONS[index],
    UNICODE_CASE_FOLD_EXCEPTIONS[index + 1],
  );
}

function assertScalarString(value, label) {
  if (typeof value !== 'string') throw new TypeError(`${label} input must be a string`);
  for (let index = 0; index < value.length; index += 1) {
    const unit = value.charCodeAt(index);
    if (unit >= 0xd800 && unit <= 0xdbff) {
      const next = value.charCodeAt(index + 1);
      if (!(next >= 0xdc00 && next <= 0xdfff)) {
        throw new TypeError(
          `${label} input contains an isolated surrogate at UTF-16 index ${index}`,
        );
      }
      index += 1;
    } else if (unit >= 0xdc00 && unit <= 0xdfff) {
      throw new TypeError(
        `${label} input contains an isolated surrogate at UTF-16 index ${index}`,
      );
    }
  }
}

// Keep this operation aligned with vo-module's portable_case_key: ICU4X
// default full case folding followed by canonical composition (NFC).
export function portableCaseKey(value) {
  assertScalarString(value, 'portable case key');
  let folded = '';
  for (const scalar of value) {
    folded += FULL_CASE_FOLD.get(scalar.codePointAt(0)) ?? scalar;
  }
  return folded.normalize('NFC');
}

export function portablePathCollisionKey(value) {
  assertScalarString(value, 'portable path key');
  return value.split('/').map(portableCaseKey).join('/');
}
