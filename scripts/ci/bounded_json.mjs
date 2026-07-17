const DEFAULT_MAX_BYTES = 16 * 1024 * 1024;
const DEFAULT_MAX_DEPTH = 127;
const DEFAULT_MAX_TOKENS = 1_000_000;
const DEFAULT_MAX_OBJECT_KEYS = 200_000;
const UTF8 = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true });
const UTF8_ENCODER = new TextEncoder();

/**
 * Parse one bounded, duplicate-key-free JSON value from UTF-8 bytes.
 *
 * This module deliberately has no Node imports so the exact parser can be
 * shared by build tooling and browser protocol validation.
 */
export function parseBoundedJsonBytes(bytes, label = 'JSON', options = {}) {
  const maxBytes = options.maxBytes ?? DEFAULT_MAX_BYTES;
  const maxDepth = options.maxDepth ?? DEFAULT_MAX_DEPTH;
  const maxTokens = options.maxTokens ?? DEFAULT_MAX_TOKENS;
  const maxObjectKeys = options.maxObjectKeys ?? DEFAULT_MAX_OBJECT_KEYS;
  const maxObjectKeyBytes = options.maxObjectKeyBytes ?? maxBytes;
  const assertBounded = (condition, message) => {
    if (!condition) throw new Error(message);
  };
  for (const [name, value] of Object.entries({
    maxBytes,
    maxDepth,
    maxTokens,
    maxObjectKeys,
    maxObjectKeyBytes,
  })) {
    assertBounded(
      Number.isSafeInteger(value) && value > 0,
      `${label}: ${name} must be a positive safe integer`,
    );
  }
  assertBounded(
    bytes instanceof Uint8Array && bytes.byteLength <= maxBytes,
    `${label}: must be a Uint8Array containing at most ${maxBytes} bytes`,
  );
  let source;
  try {
    source = UTF8.decode(bytes);
  } catch (error) {
    throw new Error(`${label}: must be valid UTF-8: ${error.message}`);
  }

  const fail = (detail) => {
    throw new Error(`${label}: ${detail}`);
  };
  const hasUnpairedSurrogate = (value) => {
    for (let index = 0; index < value.length; index += 1) {
      const code = value.charCodeAt(index);
      if (code >= 0xd800 && code <= 0xdbff) {
        const next = value.charCodeAt(index + 1);
        if (!(next >= 0xdc00 && next <= 0xdfff)) return true;
        index += 1;
      } else if (code >= 0xdc00 && code <= 0xdfff) {
        return true;
      }
    }
    return false;
  };

  const stack = [];
  let rootState = 'value';
  let index = 0;
  let tokens = 0;
  let objectKeys = 0;
  let objectKeyBytes = 0;
  const charge = () => {
    tokens += 1;
    if (tokens > maxTokens) fail(`exceeds the ${maxTokens}-token JSON limit`);
  };
  const skipWhitespace = () => {
    while (index < source.length && /[\u0009\u000A\u000D\u0020]/.test(source[index])) {
      index += 1;
    }
  };
  const scanString = () => {
    const start = index;
    index += 1;
    while (index < source.length) {
      if (source[index] === '"') {
        index += 1;
        return source.slice(start, index);
      }
      if (source[index] === '\\') index += 2;
      else index += 1;
    }
    fail('contains an unterminated JSON string');
  };
  const decodeString = (raw, context) => {
    let value;
    try {
      value = JSON.parse(raw);
    } catch (error) {
      fail(`contains an invalid ${context}: ${error.message}`);
    }
    if (hasUnpairedSurrogate(value)) {
      fail(`${context} must contain only Unicode scalar values`);
    }
    return value;
  };
  const scanScalar = () => {
    const start = index;
    while (index < source.length && !/[\u0009\u000A\u000D\u0020,\]}]/.test(source[index])) {
      index += 1;
    }
    if (index === start) fail('contains a missing JSON value');
    const raw = source.slice(start, index);
    let value;
    try {
      value = JSON.parse(raw);
    } catch (error) {
      fail(`contains an invalid JSON scalar: ${error.message}`);
    }
    if (typeof value === 'number' && !Number.isFinite(value)) {
      fail('contains a JSON number outside the finite f64 range');
    }
  };
  const beginValue = () => {
    charge();
    if (source[index] === '{') {
      if (stack.length >= maxDepth) fail(`exceeds the ${maxDepth}-level JSON depth limit`);
      index += 1;
      stack.push({ kind: 'object', keys: new Set(), state: 'keyOrEnd' });
    } else if (source[index] === '[') {
      if (stack.length >= maxDepth) fail(`exceeds the ${maxDepth}-level JSON depth limit`);
      index += 1;
      stack.push({ kind: 'array', state: 'valueOrEnd' });
    } else if (source[index] === '"') {
      decodeString(scanString(), 'JSON string');
    } else {
      scanScalar();
    }
  };

  while (true) {
    skipWhitespace();
    if (stack.length === 0) {
      if (rootState === 'done') {
        if (index !== source.length) fail('contains trailing JSON data');
        break;
      }
      if (index === source.length) fail('contains no JSON value');
      rootState = 'done';
      beginValue();
      continue;
    }

    const context = stack.at(-1);
    if (context.kind === 'object') {
      if (context.state === 'keyOrEnd' || context.state === 'key') {
        if (source[index] === '}' && context.state === 'keyOrEnd') {
          charge();
          index += 1;
          stack.pop();
          continue;
        }
        if (source[index] !== '"') fail('object key must be a JSON string');
        charge();
        const key = decodeString(scanString(), 'object key');
        const keyBytes = UTF8_ENCODER.encode(key).byteLength;
        objectKeys += 1;
        objectKeyBytes += keyBytes;
        if (objectKeys > maxObjectKeys) fail(`exceeds the ${maxObjectKeys}-key JSON limit`);
        if (!Number.isSafeInteger(objectKeyBytes) || objectKeyBytes > maxObjectKeyBytes) {
          fail(`exceeds the ${maxObjectKeyBytes}-byte object-key limit`);
        }
        if (context.keys.has(key)) fail(`contains duplicate object key ${JSON.stringify(key)}`);
        context.keys.add(key);
        context.state = 'colon';
        continue;
      }
      if (context.state === 'colon') {
        if (source[index] !== ':') fail('object key is missing a colon');
        charge();
        index += 1;
        context.state = 'value';
        continue;
      }
      if (context.state === 'value') {
        if (index === source.length) fail('object is missing a value');
        context.state = 'commaOrEnd';
        beginValue();
        continue;
      }
      if (source[index] === ',') {
        charge();
        index += 1;
        context.state = 'key';
      } else if (source[index] === '}') {
        charge();
        index += 1;
        stack.pop();
      } else {
        fail('object is missing a comma or closing brace');
      }
      continue;
    }

    if (context.state === 'valueOrEnd' || context.state === 'value') {
      if (source[index] === ']' && context.state === 'valueOrEnd') {
        charge();
        index += 1;
        stack.pop();
      } else {
        if (index === source.length) fail('array is missing a value');
        context.state = 'commaOrEnd';
        beginValue();
      }
      continue;
    }
    if (source[index] === ',') {
      charge();
      index += 1;
      context.state = 'value';
    } else if (source[index] === ']') {
      charge();
      index += 1;
      stack.pop();
    } else {
      fail('array is missing a comma or closing bracket');
    }
  }

  try {
    return JSON.parse(source);
  } catch (error) {
    throw new Error(`${label}: is invalid JSON: ${error.message}`);
  }
}
