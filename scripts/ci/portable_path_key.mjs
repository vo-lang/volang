import {
  UNICODE_CASE_FOLD_EXCEPTIONS,
  UNICODE_CASE_FOLD_RANGES,
} from './unicode_casefold_data.mjs';

const FULL_CASE_FOLD = new Map();
const UTF8_ENCODER = new TextEncoder();
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

/**
 * Bounded portable path closure backed by a component trie.
 *
 * Each node retains one original component and is indexed by its folded
 * component. Keeping complete prefixes in several maps makes a deep path
 * closure quadratic in stored component bytes; this representation's actual
 * storage stays linear in the number and size of distinct components.
 * `pathKeyBytes` is the stricter cross-runtime protocol budget: every new node
 * is charged by the UTF-8 length of its complete prefix, including separators
 * and implicit directories. Rust, Quickplay, and Studio use that same rule.
 */
export class PortablePathTrie {
  #maxNodes;
  #maxPathKeyBytes;
  #nodeCount = 0;
  #pathKeyBytes = 0;
  #root = new Map();

  constructor(maxNodes, maxPathKeyBytes = Number.POSITIVE_INFINITY) {
    if (!Number.isSafeInteger(maxNodes) || maxNodes <= 0) {
      throw new RangeError('portable path trie requires a positive safe node limit');
    }
    if (
      maxPathKeyBytes !== Number.POSITIVE_INFINITY
      && (!Number.isSafeInteger(maxPathKeyBytes) || maxPathKeyBytes <= 0)
    ) {
      throw new RangeError('portable path trie requires a positive safe path-key byte limit');
    }
    this.#maxNodes = maxNodes;
    this.#maxPathKeyBytes = maxPathKeyBytes;
  }

  get nodeCount() {
    return this.#nodeCount;
  }

  get pathKeyBytes() {
    return this.#pathKeyBytes;
  }

  insert(relative, isDirectory = false, label = 'path closure') {
    assertScalarString(relative, 'portable path trie');
    if (typeof isDirectory !== 'boolean' || typeof label !== 'string') {
      throw new TypeError('portable path trie insertion has invalid options');
    }
    const components = relative.split('/');
    const foldedComponents = components.map(portableCaseKey);
    const componentBytes = components.map((component) => UTF8_ENCODER.encode(component).byteLength);
    const existingPrefix = [];
    let prefixBytes = 0;
    let children = this.#root;

    for (let index = 0; index < components.length; index += 1) {
      const component = components[index];
      const folded = foldedComponents[index];
      const last = index + 1 === components.length;
      const existing = children.get(folded);
      if (existing === undefined) {
        const additionalNodes = components.length - index;
        if (this.#nodeCount + additionalNodes > this.#maxNodes) {
          throw new Error(
            `${label} exceeds the ${this.#maxNodes}-node path-closure limit`,
          );
        }
        let additionalPathKeyBytes = 0;
        let nextPrefixBytes = prefixBytes;
        for (let remaining = index; remaining < components.length; remaining += 1) {
          nextPrefixBytes += (remaining === 0 ? 0 : 1) + componentBytes[remaining];
          additionalPathKeyBytes += nextPrefixBytes;
        }
        if (this.#pathKeyBytes + additionalPathKeyBytes > this.#maxPathKeyBytes) {
          throw new Error(
            `${label} exceeds the ${this.#maxPathKeyBytes}-byte path-key limit`,
          );
        }
        for (let remaining = index; remaining < components.length; remaining += 1) {
          const remainingLast = remaining + 1 === components.length;
          const file = remainingLast && !isDirectory;
          const node = {
            spelling: components[remaining],
            file,
            explicitDirectory: remainingLast && isDirectory,
            children: file ? null : new Map(),
          };
          children.set(foldedComponents[remaining], node);
          this.#nodeCount += 1;
          if (node.children !== null) children = node.children;
        }
        this.#pathKeyBytes += additionalPathKeyBytes;
        return;
      }

      if (existing.spelling !== component) {
        const spelling = [...components.slice(0, index), component].join('/');
        const conflict = [...existingPrefix, existing.spelling].join('/');
        throw new Error(
          `${label} contains a portable path alias at ${spelling}; conflicts with ${conflict}`,
        );
      }
      existingPrefix.push(existing.spelling);
      prefixBytes += (index === 0 ? 0 : 1) + componentBytes[index];

      if (!last) {
        if (existing.file) {
          throw new Error(
            `${label} contains a file/directory collision: ${relative} descends through file ${existingPrefix.join('/')}`,
          );
        }
        children = existing.children;
        continue;
      }
      if (isDirectory) {
        if (existing.file) {
          throw new Error(
            `${label} contains a file/directory collision: ${relative} is both a file and directory`,
          );
        }
        if (existing.explicitDirectory) {
          throw new Error(`${label} contains duplicate directory ${relative}`);
        }
        existing.explicitDirectory = true;
        return;
      }
      if (!existing.file) {
        throw new Error(
          `${label} contains a file/directory collision: ${relative} is both a file and directory`,
        );
      }
      throw new Error(`${label} contains duplicate file ${relative}`);
    }
  }
}
