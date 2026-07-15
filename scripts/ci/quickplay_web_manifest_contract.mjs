import { createHash } from 'node:crypto';
import { portableCaseKey } from './portable_path_key.mjs';

// Keep these protocol ceilings synchronized with vo-module and the stricter
// Quickplay ingestion envelope.  The validator receives an already parsed
// JSON value, so every collection is checked before it is copied or sorted.
export const QUICKPLAY_WEB_MANIFEST_LIMITS = Object.freeze({
  modules: 10_000,
  edges: 10_000,
  files: 20_000,
  artifacts: 10_000,
  metadataEntries: 10_000,
  sourceBytes: 64 * 1024 * 1024,
  sourceFileBytes: 16 * 1024 * 1024,
  fileBytes: 256 * 1024 * 1024,
  metadataBytes: 16 * 1024 * 1024,
  pathBytes: 4 * 1024,
  pathComponentBytes: 255,
  pathComponents: 256,
  moduleBytes: 255,
});

const LIMITS = QUICKPLAY_WEB_MANIFEST_LIMITS;
const MAX_SAFE_U64 = Number.MAX_SAFE_INTEGER;
// serde_json 1.0.x starts at 128 remaining levels and rejects the container
// that would decrement the counter to zero, so 127 is the portable ceiling.
const MAX_STRICT_JSON_DEPTH = 127;
const MAX_STRICT_JSON_TOKENS = 1_000_000;
const MAX_STRICT_JSON_OBJECT_KEYS = 200_000;
const SHA256_PATTERN = /^sha256:[0-9a-f]{64}$/;
const COMMIT_PATTERN = /^[0-9a-f]{40}$/;
const WASM_TARGET = 'wasm32-unknown-unknown';
const RESERVED_SOURCE_ROOTS = new Set([
  'artifacts',
  '.vo-version',
  '.vo-source-digest',
  'vo.release.json',
]);
const WEB_MANIFEST_FIELDS = [
  'schema_version',
  'module',
  'version',
  'commit',
  'module_root',
  'vo',
  'require',
  'source_digest',
  'source',
  'artifacts',
  'web',
  'extension',
];

function fail(label, detail) {
  throw new Error(`${label}: ${detail}`);
}

function isPlainObject(value) {
  if (value === null || typeof value !== 'object' || Array.isArray(value)) return false;
  const prototype = Object.getPrototypeOf(value);
  return prototype === Object.prototype || prototype === null;
}

/** Parse bounded UTF-8 JSON while rejecting duplicate object keys at any depth. */
export function parseBoundedStrictJsonBytes(bytes, label = 'JSON', options = {}) {
  const maxBytes = options.maxBytes ?? LIMITS.metadataBytes;
  const maxDepth = options.maxDepth ?? MAX_STRICT_JSON_DEPTH;
  const maxTokens = options.maxTokens ?? MAX_STRICT_JSON_TOKENS;
  const maxObjectKeys = options.maxObjectKeys ?? MAX_STRICT_JSON_OBJECT_KEYS;
  const maxObjectKeyBytes = options.maxObjectKeyBytes ?? maxBytes;
  for (const [name, value] of Object.entries({
    maxBytes,
    maxDepth,
    maxTokens,
    maxObjectKeys,
    maxObjectKeyBytes,
  })) {
    if (!Number.isSafeInteger(value) || value <= 0) fail(label, `${name} must be a positive safe integer`);
  }
  if (!(bytes instanceof Uint8Array) || bytes.byteLength > maxBytes) {
    fail(label, `must be a Uint8Array containing at most ${maxBytes} bytes`);
  }
  let source;
  try {
    source = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true }).decode(bytes);
  } catch (error) {
    fail(label, `must be valid UTF-8: ${error.message}`);
  }

  const stack = [];
  let rootState = 'value';
  let index = 0;
  let tokens = 0;
  let objectKeys = 0;
  let objectKeyBytes = 0;
  const charge = () => {
    tokens += 1;
    if (tokens > maxTokens) fail(label, `exceeds the ${maxTokens}-token JSON limit`);
  };
  const skipWhitespace = () => {
    while (index < source.length && /[\u0009\u000A\u000D\u0020]/.test(source[index])) index += 1;
  };
  const scanString = () => {
    const start = index;
    index += 1;
    while (index < source.length) {
      if (source[index] === '"') {
        index += 1;
        return source.slice(start, index);
      }
      if (source[index] === '\\') {
        index += 2;
      } else {
        index += 1;
      }
    }
    fail(label, 'contains an unterminated JSON string');
  };
  const decodeString = (raw, context) => {
    let value;
    try {
      value = JSON.parse(raw);
    } catch (error) {
      fail(label, `contains an invalid ${context}: ${error.message}`);
    }
    if (hasUnpairedSurrogate(value)) {
      fail(label, `${context} must contain only Unicode scalar values`);
    }
    return value;
  };
  const scanScalar = () => {
    const start = index;
    while (index < source.length && !/[\u0009\u000A\u000D\u0020,\]}]/.test(source[index])) index += 1;
    if (index === start) fail(label, 'contains a missing JSON value');
    const raw = source.slice(start, index);
    let value;
    try {
      value = JSON.parse(raw);
    } catch (error) {
      fail(label, `contains an invalid JSON scalar: ${error.message}`);
    }
    if (typeof value === 'number' && !Number.isFinite(value)) {
      fail(label, 'contains a JSON number outside the finite f64 range');
    }
  };
  const beginValue = () => {
    charge();
    if (source[index] === '{') {
      if (stack.length >= maxDepth) fail(label, `exceeds the ${maxDepth}-level JSON depth limit`);
      index += 1;
      stack.push({ kind: 'object', keys: new Set(), state: 'keyOrEnd' });
    } else if (source[index] === '[') {
      if (stack.length >= maxDepth) fail(label, `exceeds the ${maxDepth}-level JSON depth limit`);
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
        if (index !== source.length) fail(label, 'contains trailing JSON data');
        break;
      }
      if (index === source.length) fail(label, 'contains no JSON value');
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
        if (source[index] !== '"') fail(label, 'object key must be a JSON string');
        charge();
        const rawKey = scanString();
        const key = decodeString(rawKey, 'object key');
        const keyBytes = Buffer.byteLength(key, 'utf8');
        objectKeys += 1;
        objectKeyBytes += keyBytes;
        if (objectKeys > maxObjectKeys) fail(label, `exceeds the ${maxObjectKeys}-key JSON limit`);
        if (!Number.isSafeInteger(objectKeyBytes) || objectKeyBytes > maxObjectKeyBytes) {
          fail(label, `exceeds the ${maxObjectKeyBytes}-byte object-key limit`);
        }
        if (context.keys.has(key)) fail(label, `contains duplicate object key ${JSON.stringify(key)}`);
        context.keys.add(key);
        context.state = 'colon';
        continue;
      }
      if (context.state === 'colon') {
        if (source[index] !== ':') fail(label, 'object key is missing a colon');
        charge();
        index += 1;
        context.state = 'value';
        continue;
      }
      if (context.state === 'value') {
        if (index === source.length) fail(label, 'object is missing a value');
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
        fail(label, 'object is missing a comma or closing brace');
      }
      continue;
    }

    if (context.state === 'valueOrEnd' || context.state === 'value') {
      if (source[index] === ']' && context.state === 'valueOrEnd') {
        charge();
        index += 1;
        stack.pop();
      } else {
        if (index === source.length) fail(label, 'array is missing a value');
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
      fail(label, 'array is missing a comma or closing bracket');
    }
  }

  try {
    return JSON.parse(source);
  } catch (error) {
    fail(label, `is invalid JSON: ${error.message}`);
  }
}

function objectContract(value, label, required, optional = []) {
  if (!isPlainObject(value)) fail(label, 'must be an object');
  const allowed = new Set([...required, ...optional]);
  for (const key in value) {
    if (!Object.hasOwn(value, key)) continue;
    if (!allowed.has(key)) fail(label, `contains unsupported field ${JSON.stringify(key)}`);
  }
  for (const key of required) {
    if (!Object.hasOwn(value, key)) fail(label, `is missing required field ${JSON.stringify(key)}`);
  }
  return value;
}

function boundedArray(value, label, limit) {
  if (!Array.isArray(value)) fail(label, 'must be an array');
  if (value.length > limit) fail(label, `contains more than ${limit} entries`);
  return value;
}

function utf8Length(value) {
  return Buffer.byteLength(value, 'utf8');
}

function hasUnpairedSurrogate(value) {
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
}

function boundedString(value, label, maxBytes, { allowEmpty = false } = {}) {
  if (typeof value !== 'string') fail(label, 'must be a string');
  if (hasUnpairedSurrogate(value)) fail(label, 'must contain valid Unicode scalar values');
  if ((!allowEmpty && value.length === 0) || utf8Length(value) > maxBytes) {
    fail(label, `must contain ${allowEmpty ? 'at most' : 'between 1 and'} ${maxBytes} UTF-8 bytes`);
  }
  return value;
}

function compareUtf8(left, right) {
  return Buffer.compare(Buffer.from(left, 'utf8'), Buffer.from(right, 'utf8'));
}

function compareArtifactIdentity(left, right) {
  return compareUtf8(left.kind, right.kind)
    || compareUtf8(left.target, right.target)
    || compareUtf8(left.name, right.name);
}

function canonicalDigest(value, label) {
  if (typeof value !== 'string' || !SHA256_PATTERN.test(value)) {
    fail(label, 'must be a canonical lowercase sha256 digest');
  }
  return value;
}

function nonNegativeSafeInteger(value, label, maximum = MAX_SAFE_U64) {
  if (!Number.isSafeInteger(value) || value < 0 || value > maximum) {
    fail(label, `must be a non-negative safe integer no greater than ${maximum}`);
  }
  return value;
}

function normalizedMetadataString(value, label, maxBytes = LIMITS.pathBytes) {
  boundedString(value, label, maxBytes);
  if (hasUnicodeWhiteSpaceBoundary(value) || /\p{Cc}/u.test(value)) {
    fail(label, 'must be a normalized metadata string without boundary whitespace or controls');
  }
  return value;
}

function hasUnicodeWhiteSpaceBoundary(value) {
  return /^(?:\p{White_Space})|(?:\p{White_Space})$/u.test(value);
}

function windowsDeviceStem(component) {
  return portableCaseKey(component.split('.', 1)[0]);
}

export function validatePortablePathComponent(value, label = 'path component') {
  boundedString(value, label, LIMITS.pathComponentBytes);
  const stem = windowsDeviceStem(value);
  if (
    value === '.'
    || value === '..'
    || hasUnicodeWhiteSpaceBoundary(value)
    || value.endsWith('.')
    || value.includes('/')
    || value.includes('\\')
    || value.normalize('NFC') !== value
    || /[\p{Cc}<>:"|?*]/u.test(value)
    || ['con', 'prn', 'aux', 'nul', 'conin$', 'conout$'].includes(stem)
    || /^(?:com|lpt)(?:[1-9]|[¹²³])$/u.test(stem)
  ) {
    fail(label, 'must be a normalized portable path component');
  }
  return value;
}

export function validatePortableRelativePath(value, label = 'relative path') {
  boundedString(value, label, LIMITS.pathBytes);
  if (hasUnicodeWhiteSpaceBoundary(value) || value.includes('\\') || /\p{Cc}/u.test(value)) {
    fail(label, 'must be a normalized portable module-relative path');
  }
  const components = value.split('/');
  if (components.length > LIMITS.pathComponents) {
    fail(label, `contains more than ${LIMITS.pathComponents} components`);
  }
  components.forEach((component, index) => (
    validatePortablePathComponent(component, `${label} component ${index}`)
  ));
  return value;
}

function pathPrefixes(value) {
  validatePortableRelativePath(value);
  const prefixes = [];
  let spelling = '';
  let key = '';
  for (const component of value.split('/')) {
    spelling = spelling ? `${spelling}/${component}` : component;
    const folded = portableCaseKey(component);
    key = key ? `${key}/${folded}` : folded;
    prefixes.push({ spelling, key });
  }
  return prefixes;
}

class PortablePathSet {
  constructor() {
    this.spellings = new Map();
    this.paths = new Set();
    this.files = new Set();
    this.directories = new Set();
  }

  checkSpellings(value, prefixes, label) {
    for (const prefix of prefixes) {
      const existing = this.spellings.get(prefix.key);
      if (existing !== undefined && existing !== prefix.spelling) {
        fail(label, `${JSON.stringify(value)} conflicts with portable spelling ${JSON.stringify(existing)}`);
      }
    }
  }

  remember(prefixes) {
    for (const prefix of prefixes) {
      if (!this.spellings.has(prefix.key)) this.spellings.set(prefix.key, prefix.spelling);
    }
  }

  insertPath(value, label) {
    const prefixes = pathPrefixes(value);
    this.checkSpellings(value, prefixes, label);
    this.remember(prefixes);
    return !this.paths.has(prefixes.at(-1).key) && this.paths.add(prefixes.at(-1).key);
  }

  insertFile(value, label) {
    const prefixes = pathPrefixes(value);
    this.checkSpellings(value, prefixes, label);
    for (let index = 0; index < prefixes.length; index += 1) {
      const prefix = prefixes[index];
      const last = index + 1 === prefixes.length;
      if (!last && this.files.has(prefix.key)) {
        fail(label, `${JSON.stringify(value)} descends through file ${JSON.stringify(prefix.spelling)}`);
      }
      if (last && this.directories.has(prefix.key)) {
        fail(label, `${JSON.stringify(value)} is declared as both a file and directory`);
      }
    }
    this.remember(prefixes);
    for (const prefix of prefixes.slice(0, -1)) this.directories.add(prefix.key);
    const key = prefixes.at(-1).key;
    if (this.files.has(key)) return false;
    this.files.add(key);
    return true;
  }
}

function parseSemver(value, label) {
  boundedString(value, label, LIMITS.pathComponentBytes - 1);
  if (value.includes('+')) fail(label, 'must not contain build metadata');
  const match = /^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(?:-([0-9A-Za-z-]+(?:\.[0-9A-Za-z-]+)*))?$/.exec(value);
  if (!match) fail(label, 'must be canonical MAJOR.MINOR.PATCH[-PRERELEASE]');
  for (const numeric of match.slice(1, 4)) {
    if (BigInt(numeric) > 18_446_744_073_709_551_615n) fail(label, 'numeric component overflows u64');
  }
  if (match[4]) {
    for (const identifier of match[4].split('.')) {
      if (/^[0-9]+$/.test(identifier) && (identifier.length > 1 && identifier.startsWith('0'))) {
        fail(label, 'numeric prerelease identifiers must not contain leading zeroes');
      }
      if (/^[0-9]+$/.test(identifier) && BigInt(identifier) > 18_446_744_073_709_551_615n) {
        fail(label, 'numeric prerelease identifier overflows u64');
      }
    }
  }
  return { major: BigInt(match[1]), canonical: value };
}

function validateExactVersion(value, label) {
  boundedString(value, label, LIMITS.pathComponentBytes);
  validatePortablePathComponent(value, label);
  if (!value.startsWith('v')) fail(label, 'must start with v');
  return parseSemver(value.slice(1), label);
}

function validateConstraint(value, label, { toolchain = false } = {}) {
  boundedString(value, label, LIMITS.pathComponentBytes);
  const prefix = value[0];
  let version;
  if (prefix === '^' || prefix === '~') {
    version = parseSemver(value.slice(1), label);
  } else if (!toolchain && prefix === 'v') {
    version = validateExactVersion(value, label);
  } else if (toolchain && prefix !== 'v') {
    version = parseSemver(value, label);
  } else {
    fail(label, toolchain
      ? 'must be an exact, compatible, or patch-compatible toolchain constraint without v'
      : 'must start with ^, ~, or v');
  }
  return { canonical: value, major: version.major };
}

function validateModulePath(value, label) {
  boundedString(value, label, LIMITS.moduleBytes);
  if (!value.startsWith('github.com/') || value.startsWith('/') || value.endsWith('/')) {
    fail(label, 'must begin with github.com/ and have no boundary slash');
  }
  const segments = value.split('/');
  if (segments.length < 3) fail(label, 'must contain github.com/<owner>/<repository>');
  segments.forEach((segment, index) => {
    validatePortablePathComponent(segment, `${label} segment ${index}`);
    if (!/^[a-z0-9][a-z0-9._-]*$/.test(segment)) {
      fail(`${label} segment ${index}`, 'must use lowercase ASCII module spelling');
    }
  });
  let suffixMajor = null;
  const suffix = /^v([0-9]+)$/.exec(segments.at(-1));
  if (suffix) {
    if (suffix[1].length > 1 && suffix[1].startsWith('0')) fail(label, 'major suffix must be unpadded');
    suffixMajor = BigInt(suffix[1]);
    if (suffixMajor < 2n || suffixMajor > 18_446_744_073_709_551_615n) {
      fail(label, 'major suffix must be vN with 2 <= N <= u64::MAX');
    }
  }
  return {
    value,
    suffixMajor,
    moduleRoot: segments.length === 3 ? '.' : segments.slice(3).join('/'),
  };
}

function validateModuleVersionCompatibility(module, version, label) {
  if (module.suffixMajor === null ? version.major > 1n : module.suffixMajor !== version.major) {
    fail(label, `version major ${version.major} is incompatible with module path ${module.value}`);
  }
}

function validateRequirements(raw, owner, label) {
  const requirements = boundedArray(raw, label, Math.min(LIMITS.modules, LIMITS.edges));
  const normalized = requirements.map((entry, index) => {
    const entryLabel = `${label}[${index}]`;
    objectContract(entry, entryLabel, ['module', 'constraint']);
    const module = validateModulePath(entry.module, `${entryLabel}.module`);
    const constraint = validateConstraint(entry.constraint, `${entryLabel}.constraint`);
    if (module.value === owner.value) fail(entryLabel, 'must not require the manifest module itself');
    validateModuleVersionCompatibility(module, constraint, entryLabel);
    return { module: module.value, constraint: constraint.canonical };
  });
  for (let index = 1; index < normalized.length; index += 1) {
    if (compareUtf8(normalized[index - 1].module, normalized[index].module) >= 0) {
      fail(label, 'must be unique and sorted by module path');
    }
  }
  return normalized;
}

function sourcePathParticipates(path) {
  const first = portableCaseKey(path.split('/', 1)[0]);
  if (RESERVED_SOURCE_ROOTS.has(first)) return false;
  return portableCaseKey(path) !== 'vo.web.json';
}

function defaultDigestBytes(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function normalizedDigestCallback(options, label) {
  const callback = options.digestBytes ?? defaultDigestBytes;
  if (typeof callback !== 'function') fail(label, 'digestBytes option must be a function');
  return (bytes, context) => {
    const digest = callback(bytes, context);
    return canonicalDigest(digest, `${context} computed digest`);
  };
}

function callbackBytes(callback, value, label, maximum = LIMITS.fileBytes) {
  const bytes = callback(value);
  if (!(bytes instanceof Uint8Array)) fail(label, 'byte callback must return Uint8Array');
  if (bytes.byteLength > maximum) fail(label, `exceeds the ${maximum}-byte file limit`);
  return bytes;
}

export function canonicalWebSourceDigest(entries, options = {}) {
  const label = options.label ?? 'vo.web.json source';
  if (!Array.isArray(entries) || entries.length > LIMITS.files) {
    fail(label, `must be an array with at most ${LIMITS.files} entries`);
  }
  const canonical = entries.map((entry, index) => {
    objectContract(entry, `${label}[${index}]`, ['path', 'size', 'digest']);
    return {
      path: boundedString(entry.path, `${label}[${index}].path`, LIMITS.pathBytes),
      size: nonNegativeSafeInteger(entry.size, `${label}[${index}].size`, LIMITS.sourceFileBytes),
      digest: canonicalDigest(entry.digest, `${label}[${index}].digest`),
    };
  }).sort((left, right) => compareUtf8(left.path, right.path));
  const bytes = Buffer.from(JSON.stringify(canonical), 'utf8');
  return normalizedDigestCallback(options, label)(bytes, `${label} canonical set`);
}

function validateSources(raw, expectedDigest, options, label) {
  const entries = boundedArray(raw, label, LIMITS.files);
  if (entries.length === 0) fail(label, 'must contain vo.mod');
  const paths = new PortablePathSet();
  let totalSize = 0;
  const digestBytes = normalizedDigestCallback(options, label);
  const normalized = entries.map((entry, index) => {
    const entryLabel = `${label}[${index}]`;
    objectContract(entry, entryLabel, ['path', 'size', 'digest']);
    const path = validatePortableRelativePath(entry.path, `${entryLabel}.path`);
    if (!sourcePathParticipates(path)) fail(entryLabel, `uses source-set-excluded path ${JSON.stringify(path)}`);
    if (!paths.insertFile(path, entryLabel)) fail(entryLabel, `duplicates source path ${JSON.stringify(path)}`);
    const size = nonNegativeSafeInteger(entry.size, `${entryLabel}.size`, LIMITS.sourceFileBytes);
    totalSize += size;
    if (!Number.isSafeInteger(totalSize) || totalSize > LIMITS.sourceBytes) {
      fail(label, `exceeds the ${LIMITS.sourceBytes}-byte source payload limit`);
    }
    const digest = canonicalDigest(entry.digest, `${entryLabel}.digest`);
    if (options.sourceBytes !== undefined) {
      if (typeof options.sourceBytes !== 'function') fail(label, 'sourceBytes option must be a function');
      const bytes = callbackBytes(
        () => options.sourceBytes(path, entry),
        entry,
        entryLabel,
        LIMITS.sourceFileBytes,
      );
      if (bytes.byteLength !== size) fail(entryLabel, 'declared size does not match source bytes');
      if (digestBytes(bytes, entryLabel) !== digest) fail(entryLabel, 'declared digest does not match source bytes');
    }
    return { path, size, digest };
  });
  for (let index = 1; index < normalized.length; index += 1) {
    if (compareUtf8(normalized[index - 1].path, normalized[index].path) >= 0) {
      fail(label, 'must be unique and sorted by path');
    }
  }
  if (!normalized.some((entry) => entry.path === 'vo.mod')) fail(label, 'is missing vo.mod');
  const actual = canonicalWebSourceDigest(normalized, { ...options, label });
  if (actual !== expectedDigest) fail(label, `canonical digest mismatch: expected ${expectedDigest}, found ${actual}`);
  return { entries: normalized, totalSize };
}

function validateArtifactIdentity(value, label, { allowNative = false } = {}) {
  const kind = boundedString(value.kind, `${label}.kind`, LIMITS.pathComponentBytes);
  const target = validatePortablePathComponent(value.target, `${label}.target`);
  const name = validatePortablePathComponent(value.name, `${label}.name`);
  if (!['extension-native', 'extension-wasm', 'extension-js-glue'].includes(kind)) {
    fail(`${label}.kind`, `unsupported artifact kind ${JSON.stringify(kind)}`);
  }
  if (!allowNative && kind === 'extension-native') fail(label, 'must not list native artifacts');
  if ((kind === 'extension-wasm' || kind === 'extension-js-glue') && target !== WASM_TARGET) {
    fail(label, `${kind} must target ${WASM_TARGET}`);
  }
  if (kind === 'extension-native') {
    const segments = target.split('-');
    if (segments.length < 3 || segments.some((segment) => !/^[a-z0-9_.]+$/.test(segment))) {
      fail(label, 'native artifact target must be a canonical Rust target triple');
    }
  }
  return { kind, target, name };
}

function artifactKey(artifact) {
  return `${artifact.kind}\0${artifact.target}\0${artifact.name}`;
}

function validateArtifacts(raw, options, label) {
  const artifacts = boundedArray(raw, label, LIMITS.artifacts);
  const sourcePaths = new PortablePathSet();
  const cachePaths = new PortablePathSet();
  const digestBytes = normalizedDigestCallback(options, label);
  const normalized = artifacts.map((artifact, index) => {
    const artifactLabel = `${label}[${index}]`;
    objectContract(artifact, artifactLabel, ['kind', 'target', 'name', 'path', 'size', 'digest']);
    const identity = validateArtifactIdentity(artifact, artifactLabel);
    const path = validatePortableRelativePath(artifact.path, `${artifactLabel}.path`);
    if (!sourcePaths.insertFile(path, artifactLabel)) fail(artifactLabel, `reuses path ${JSON.stringify(path)}`);
    const cachePath = `artifacts/${identity.kind}/${identity.target}/${identity.name}`;
    if (!cachePaths.insertFile(cachePath, artifactLabel)) fail(artifactLabel, `duplicates cache path ${JSON.stringify(cachePath)}`);
    const size = nonNegativeSafeInteger(artifact.size, `${artifactLabel}.size`, LIMITS.fileBytes);
    const digest = canonicalDigest(artifact.digest, `${artifactLabel}.digest`);
    const result = { ...identity, path, size, digest };
    if (options.artifactBytes !== undefined) {
      if (typeof options.artifactBytes !== 'function') fail(label, 'artifactBytes option must be a function');
      const bytes = callbackBytes(() => options.artifactBytes(result, index), result, artifactLabel);
      if (bytes.byteLength !== size) fail(artifactLabel, 'declared size does not match artifact bytes');
      if (digestBytes(bytes, artifactLabel) !== digest) fail(artifactLabel, 'declared digest does not match artifact bytes');
    }
    return result;
  });
  for (let index = 1; index < normalized.length; index += 1) {
    if (compareArtifactIdentity(normalized[index - 1], normalized[index]) >= 0) {
      fail(label, 'must be unique and sorted by (kind, target, name)');
    }
  }
  return normalized;
}

function metadataBudget() {
  return {
    bytes: 0,
    add(value, label) {
      this.bytes += utf8Length(value);
      if (!Number.isSafeInteger(this.bytes) || this.bytes > LIMITS.metadataBytes) {
        fail(label, `extension metadata strings exceed ${LIMITS.metadataBytes} bytes`);
      }
    },
  };
}

function normalizedInclude(raw, label, budget = null) {
  const include = boundedArray(raw, label, LIMITS.metadataEntries);
  const paths = new PortablePathSet();
  return include.map((value, index) => {
    const path = validatePortableRelativePath(value, `${label}[${index}]`);
    if (!paths.insertPath(path, label)) fail(label, `contains duplicate path ${JSON.stringify(path)}`);
    budget?.add(path, label);
    return path;
  });
}

function normalizeWebProject(raw, label) {
  if (raw === undefined || raw === null) return null;
  objectContract(raw, label, ['include'], ['entry']);
  return {
    entry: raw.entry === undefined || raw.entry === null
      ? null
      : normalizedMetadataString(raw.entry, `${label}.entry`),
    include: normalizedInclude(raw.include, `${label}.include`),
  };
}

function normalizeWasm(raw, label, budget, moduleFiles) {
  if (raw === undefined || raw === null) return null;
  objectContract(raw, label, ['kind', 'wasm'], ['js_glue', 'local_wasm', 'local_js_glue']);
  if (raw.kind !== 'Standalone' && raw.kind !== 'Bindgen') {
    fail(`${label}.kind`, 'must be Standalone or Bindgen');
  }
  const wasm = validatePortablePathComponent(raw.wasm, `${label}.wasm`);
  if (wasm.includes('/') || wasm.includes('\\')) fail(`${label}.wasm`, 'must be a file name');
  budget.add(wasm, `${label}.wasm`);
  const jsGlue = raw.js_glue === undefined || raw.js_glue === null
    ? null
    : validatePortablePathComponent(raw.js_glue, `${label}.js_glue`);
  if (jsGlue !== null) budget.add(jsGlue, `${label}.js_glue`);
  const localWasm = raw.local_wasm === undefined || raw.local_wasm === null
    ? null
    : validatePortableRelativePath(raw.local_wasm, `${label}.local_wasm`);
  const localJsGlue = raw.local_js_glue === undefined || raw.local_js_glue === null
    ? null
    : validatePortableRelativePath(raw.local_js_glue, `${label}.local_js_glue`);
  for (const [path, field] of [[localWasm, 'local_wasm'], [localJsGlue, 'local_js_glue']]) {
    if (path !== null) {
      if (!moduleFiles.insertFile(path, `${label}.${field}`)) fail(`${label}.${field}`, 'duplicates another extension module file');
      budget.add(path, `${label}.${field}`);
    }
  }
  if (raw.kind === 'Standalone' && (jsGlue !== null || localJsGlue !== null)) {
    fail(label, 'Standalone extensions must not declare js_glue or local_js_glue');
  }
  if (raw.kind === 'Bindgen' && jsGlue === null) fail(label, 'Bindgen extensions must declare js_glue');
  return { kind: raw.kind, wasm, js_glue: jsGlue, local_wasm: localWasm, local_js_glue: localJsGlue };
}

function runtimeModuleName(value, label) {
  boundedString(value, label, LIMITS.metadataBytes);
  if (!/^[A-Za-z0-9_-]+$/.test(value)) fail(label, 'must be a normalized runtime module identifier');
  return value;
}

function normalizeWebRuntime(raw, label, budget, moduleFiles) {
  if (raw === undefined || raw === null) return null;
  objectContract(raw, label, ['capabilities', 'js_modules'], ['entry']);
  const entry = raw.entry === undefined || raw.entry === null
    ? null
    : normalizedMetadataString(raw.entry, `${label}.entry`, LIMITS.metadataBytes);
  if (entry !== null) budget.add(entry, `${label}.entry`);
  const capabilities = boundedArray(raw.capabilities, `${label}.capabilities`, LIMITS.metadataEntries)
    .map((capability, index) => {
      const value = normalizedMetadataString(
        capability,
        `${label}.capabilities[${index}]`,
        LIMITS.metadataBytes,
      );
      budget.add(value, `${label}.capabilities`);
      return value;
    });
  if (new Set(capabilities).size !== capabilities.length) fail(`${label}.capabilities`, 'must not contain duplicates');
  if (!isPlainObject(raw.js_modules)) fail(`${label}.js_modules`, 'must be an object');
  const jsEntries = [];
  for (const name in raw.js_modules) {
    if (!Object.hasOwn(raw.js_modules, name)) continue;
    if (jsEntries.length >= LIMITS.metadataEntries) {
      fail(`${label}.js_modules`, `contains more than ${LIMITS.metadataEntries} entries`);
    }
    jsEntries.push([name, raw.js_modules[name]]);
  }
  jsEntries.sort(([left], [right]) => compareUtf8(left, right));
  const jsModules = Object.create(null);
  for (const [name, rawPath] of jsEntries) {
    runtimeModuleName(name, `${label}.js_modules key`);
    const path = validatePortableRelativePath(rawPath, `${label}.js_modules.${name}`);
    if (!moduleFiles.insertFile(path, `${label}.js_modules.${name}`)) {
      fail(`${label}.js_modules.${name}`, 'duplicates another extension module file');
    }
    budget.add(name, `${label}.js_modules`);
    budget.add(path, `${label}.js_modules.${name}`);
    jsModules[name] = path;
  }
  return { entry, capabilities, js_modules: jsModules };
}

function normalizeExtension(raw, label, sharedBudget = null, sharedModuleFiles = null) {
  if (raw === undefined || raw === null) return null;
  objectContract(raw, label, ['name', 'include'], ['wasm', 'web']);
  const budget = sharedBudget ?? metadataBudget();
  const moduleFiles = sharedModuleFiles ?? new PortablePathSet();
  const name = runtimeModuleName(raw.name, `${label}.name`);
  budget.add(name, `${label}.name`);
  const include = normalizedInclude(raw.include, `${label}.include`, budget);
  const wasm = normalizeWasm(raw.wasm, `${label}.wasm`, budget, moduleFiles);
  const web = normalizeWebRuntime(raw.web, `${label}.web`, budget, moduleFiles);
  return { name, include, wasm, web };
}

function bindExpected(value, expected, label) {
  if (expected !== undefined && value !== expected) {
    fail(label, `expected ${JSON.stringify(expected)}, found ${JSON.stringify(value)}`);
  }
}

/**
 * Validate and normalize one already-JSON-parsed `vo.web.json` value.
 *
 * Optional synchronous callbacks:
 * - `digestBytes(Uint8Array, label) -> "sha256:..."`
 * - `sourceBytes(path, entry) -> Uint8Array`
 * - `artifactBytes(artifact, index) -> Uint8Array`
 * Optional `expectedModule`, `expectedVersion`, `expectedCommit`, and
 * `expectedVo` values bind the manifest to an enclosing package contract.
 */
export function validateWebManifestContract(value, label = 'vo.web.json', options = {}) {
  objectContract(
    value,
    label,
    WEB_MANIFEST_FIELDS.slice(0, 10),
    WEB_MANIFEST_FIELDS.slice(10),
  );
  if (value.schema_version !== 1) fail(`${label}.schema_version`, 'must equal 1');
  const module = validateModulePath(value.module, `${label}.module`);
  const version = validateExactVersion(value.version, `${label}.version`);
  validateModuleVersionCompatibility(module, version, `${label}.version`);
  if (value.module_root !== module.moduleRoot) {
    fail(`${label}.module_root`, `must equal ${JSON.stringify(module.moduleRoot)}`);
  }
  if (typeof value.commit !== 'string' || !COMMIT_PATTERN.test(value.commit)) {
    fail(`${label}.commit`, 'must be exactly 40 lowercase hexadecimal characters');
  }
  const vo = validateConstraint(value.vo, `${label}.vo`, { toolchain: true }).canonical;
  const require = validateRequirements(value.require, module, `${label}.require`);
  const sourceDigest = canonicalDigest(value.source_digest, `${label}.source_digest`);
  const source = validateSources(value.source, sourceDigest, options, `${label}.source`);
  const artifacts = validateArtifacts(value.artifacts, options, `${label}.artifacts`);
  const web = normalizeWebProject(value.web, `${label}.web`);
  const extension = normalizeExtension(value.extension, `${label}.extension`);
  bindExpected(module.value, options.expectedModule, `${label}.module`);
  bindExpected(value.version, options.expectedVersion, `${label}.version`);
  bindExpected(value.commit, options.expectedCommit, `${label}.commit`);
  bindExpected(vo, options.expectedVo, `${label}.vo`);
  return {
    schema_version: 1,
    module: module.value,
    version: value.version,
    commit: value.commit,
    module_root: module.moduleRoot,
    vo,
    require,
    source_digest: sourceDigest,
    source: source.entries,
    artifacts,
    web,
    extension,
  };
}

const MAX_TOML_LINES = 100_000;
// The closed metadata schema can contain at most about 80k structural nodes
// when every bounded collection is full.  Leave headroom for dotted-key
// intermediates while keeping hostile inline nesting firmly bounded.
const MAX_TOML_NODES = 200_000;

// Non-container TOML scalars can never satisfy this metadata schema.  Keep an
// opaque value so syntactically scalar input reaches the normal semantic type
// checks without lossy JavaScript number/date coercion.
class TomlScalar {
  constructor(token) {
    this.token = token;
  }
}

class MetadataTomlParser {
  constructor(source, label) {
    if (typeof source !== 'string') fail(label, 'must be UTF-8 text decoded as a string');
    if (hasUnpairedSurrogate(source)) fail(label, 'must contain valid Unicode scalar values');
    if (source.length > LIMITS.metadataBytes || utf8Length(source) > LIMITS.metadataBytes) {
      fail(label, `exceeds the ${LIMITS.metadataBytes}-byte text limit`);
    }
    this.source = source;
    this.label = label;
    this.index = this.findMetadataOffset();
    this.nodes = 0;
    this.tableMetadata = new WeakMap();
    this.arrayTables = new WeakSet();
    this.root = this.createTable('root');
    this.current = null;
    this.validateLineBudget();
  }

  validateLineBudget() {
    let lines = 1;
    for (let index = 0; index < this.source.length; index += 1) {
      if (this.source.charCodeAt(index) === 10) {
        lines += 1;
        if (lines > MAX_TOML_LINES) fail(this.label, `contains more than ${MAX_TOML_LINES} lines`);
      }
    }
  }

  error(detail) {
    let line = 1;
    for (let index = 0; index < this.index; index += 1) {
      if (this.source.charCodeAt(index) === 10) line += 1;
      if (line > MAX_TOML_LINES) break;
    }
    fail(`${this.label}:${line}`, detail);
  }

  findMetadataOffset() {
    let start = 0;
    let lines = 0;
    while (start <= this.source.length) {
      lines += 1;
      if (lines > MAX_TOML_LINES) fail(this.label, `contains more than ${MAX_TOML_LINES} lines`);
      const newline = this.source.indexOf('\n', start);
      const end = newline === -1 ? this.source.length : newline;
      const raw = this.source.slice(start, end).replace(/\r$/, '');
      const asciiIndented = raw.replace(/^[ \t]*/, '');
      if (asciiIndented.startsWith('[')) return start;
      const unicodeIndented = raw.trimStart();
      if (
        unicodeIndented.startsWith('[')
        || (raw.startsWith('\ufeff') && raw.slice(1).replace(/^[ \t]*/, '').startsWith('['))
      ) {
        fail(this.label, 'metadata table headers may be indented only with ASCII space or tab');
      }
      if (newline === -1) return this.source.length;
      start = newline + 1;
    }
    return this.source.length;
  }

  peek(offset = 0) {
    return this.source[this.index + offset];
  }

  reserveNode(context) {
    this.nodes += 1;
    if (this.nodes > MAX_TOML_NODES) {
      this.error(`${context} exceeds the ${MAX_TOML_NODES}-node TOML resource limit`);
    }
  }

  createTable(origin, dottedOwner = null) {
    this.reserveNode('metadata');
    const table = Object.create(null);
    this.tableMetadata.set(table, { origin, dottedOwner });
    return table;
  }

  createArrayTable() {
    this.reserveNode('metadata');
    const array = [];
    this.arrayTables.add(array);
    return array;
  }

  isTable(value) {
    return isPlainObject(value) && this.tableMetadata.has(value);
  }

  skipHorizontal() {
    while (this.peek() === ' ' || this.peek() === '\t') this.index += 1;
  }

  consumeNewline(context) {
    if (this.peek() === '\n') {
      this.index += 1;
      return true;
    }
    if (this.peek() === '\r') {
      if (this.peek(1) !== '\n') this.error(`${context} contains a bare carriage return`);
      this.index += 2;
      return true;
    }
    return false;
  }

  skipComment() {
    if (this.peek() !== '#') return;
    while (this.index < this.source.length && this.peek() !== '\n' && this.peek() !== '\r') {
      const codepoint = this.source.codePointAt(this.index);
      if (codepoint <= 0x08 || (codepoint >= 0x0b && codepoint <= 0x1f) || codepoint === 0x7f) {
        this.error('comment contains an invalid control character');
      }
      this.index += codepoint > 0xffff ? 2 : 1;
    }
  }

  skipTrivia() {
    while (this.index < this.source.length) {
      this.skipHorizontal();
      if (this.peek() === '#') this.skipComment();
      if (!this.consumeNewline('metadata')) return;
    }
  }

  finishLine(context) {
    this.skipHorizontal();
    this.skipComment();
    if (this.index === this.source.length) return;
    if (!this.consumeNewline(context)) this.error(`${context} has trailing data`);
  }

  parseKey(context) {
    const quote = this.peek();
    if (quote === '"' || quote === "'") return this.parseString(context, false);
    const start = this.index;
    while (/[A-Za-z0-9_-]/.test(this.peek() ?? '')) this.index += 1;
    if (start === this.index) this.error(`${context} requires a bare or quoted key`);
    return this.source.slice(start, this.index);
  }

  parseDottedKey(context) {
    const components = [];
    while (true) {
      if (components.length >= LIMITS.pathComponents) {
        this.error(`${context} contains more than ${LIMITS.pathComponents} components`);
      }
      components.push(this.parseKey(context));
      this.skipHorizontal();
      if (this.peek() !== '.') break;
      this.index += 1;
      this.skipHorizontal();
    }
    return components;
  }

  rawNewline(context) {
    if (this.peek() !== '\n' && this.peek() !== '\r') return false;
    this.consumeNewline(context);
    return true;
  }

  consumeMultilineOpeningNewline(context) {
    if (this.peek() === '\n' || this.peek() === '\r') this.consumeNewline(context);
  }

  consumeLineContinuation(context) {
    let cursor = this.index;
    while (this.source[cursor] === ' ' || this.source[cursor] === '\t') cursor += 1;
    if (this.source[cursor] === '\r') {
      if (this.source[cursor + 1] !== '\n') return false;
      cursor += 2;
    } else if (this.source[cursor] === '\n') {
      cursor += 1;
    } else {
      return false;
    }
    this.index = cursor;
    while (this.index < this.source.length) {
      this.skipHorizontal();
      if (!this.rawNewline(context)) break;
    }
    return true;
  }

  parseBasicString(context, allowMultiline = true) {
    const multiline = this.source.slice(this.index, this.index + 3) === '"""';
    if (multiline && !allowMultiline) this.error(`${context} must use a single-line quoted key`);
    this.index += multiline ? 3 : 1;
    if (multiline) this.consumeMultilineOpeningNewline(context);
    let result = '';
    while (this.index < this.source.length) {
      const character = this.peek();
      if (character === '"') {
        if (!multiline) {
          this.index += 1;
          return result;
        }
        let quotes = 0;
        while (this.peek(quotes) === '"') quotes += 1;
        if (quotes >= 3) {
          if (quotes <= 5) {
            result += '"'.repeat(quotes - 3);
            this.index += quotes;
          } else {
            this.index += 3;
          }
          return result;
        }
        result += '"'.repeat(quotes);
        this.index += quotes;
        continue;
      }
      if (character === '\n' || character === '\r') {
        if (!multiline) this.error(`${context} contains an unterminated basic string`);
        this.consumeNewline(context);
        result += '\n';
        continue;
      }
      if (character !== '\\') {
        const codepoint = this.source.codePointAt(this.index);
        if (codepoint <= 0x08 || (codepoint >= 0x0a && codepoint <= 0x1f) || codepoint === 0x7f) {
          this.error(`${context} contains an unescaped control character`);
        }
        result += String.fromCodePoint(codepoint);
        this.index += codepoint > 0xffff ? 2 : 1;
        continue;
      }
      this.index += 1;
      if (multiline && this.consumeLineContinuation(context)) continue;
      const escaped = this.peek();
      const simple = { b: '\b', t: '\t', n: '\n', f: '\f', r: '\r', '"': '"', '\\': '\\' };
      if (Object.hasOwn(simple, escaped)) {
        result += simple[escaped];
        this.index += 1;
        continue;
      }
      if (escaped !== 'u' && escaped !== 'U') {
        this.error(`${context} contains unsupported escape \\${escaped ?? ''}`);
      }
      const digits = escaped === 'u' ? 4 : 8;
      const hex = this.source.slice(this.index + 1, this.index + 1 + digits);
      if (hex.length !== digits || !/^[0-9A-Fa-f]+$/.test(hex)) {
        this.error(`${context} contains an invalid Unicode escape`);
      }
      const codepoint = Number.parseInt(hex, 16);
      if (codepoint > 0x10ffff || (codepoint >= 0xd800 && codepoint <= 0xdfff)) {
        this.error(`${context} Unicode escape is not a scalar value`);
      }
      result += String.fromCodePoint(codepoint);
      this.index += digits + 1;
    }
    this.error(`${context} contains an unterminated basic string`);
  }

  parseLiteralString(context, allowMultiline = true) {
    const multiline = this.source.slice(this.index, this.index + 3) === "'''";
    if (multiline && !allowMultiline) this.error(`${context} must use a single-line quoted key`);
    this.index += multiline ? 3 : 1;
    if (multiline) this.consumeMultilineOpeningNewline(context);
    let result = '';
    while (this.index < this.source.length) {
      const character = this.peek();
      if (character === "'") {
        if (!multiline) {
          this.index += 1;
          return result;
        }
        let quotes = 0;
        while (this.peek(quotes) === "'") quotes += 1;
        if (quotes >= 3) {
          if (quotes <= 5) {
            result += "'".repeat(quotes - 3);
            this.index += quotes;
          } else {
            this.index += 3;
          }
          return result;
        }
        result += "'".repeat(quotes);
        this.index += quotes;
        continue;
      }
      if (character === '\n' || character === '\r') {
        if (!multiline) this.error(`${context} contains an unterminated literal string`);
        this.consumeNewline(context);
        result += '\n';
        continue;
      }
      const codepoint = this.source.codePointAt(this.index);
      if (codepoint <= 0x08 || (codepoint >= 0x0a && codepoint <= 0x1f) || codepoint === 0x7f) {
        this.error(`${context} contains an invalid literal string character`);
      }
      result += String.fromCodePoint(codepoint);
      this.index += codepoint > 0xffff ? 2 : 1;
    }
    this.error(`${context} contains an unterminated literal string`);
  }

  parseString(context, allowMultiline = true) {
    if (this.peek() === '"') return this.parseBasicString(context, allowMultiline);
    if (this.peek() === "'") return this.parseLiteralString(context, allowMultiline);
    this.error(`${context} must be a TOML string`);
  }

  parseArray(context, depth) {
    this.index += 1;
    const values = [];
    while (true) {
      this.skipTrivia();
      if (this.peek() === ']') {
        this.index += 1;
        return values;
      }
      if (values.length >= LIMITS.metadataEntries) {
        this.error(`${context} contains more than ${LIMITS.metadataEntries} entries`);
      }
      this.reserveNode(context);
      values.push(this.parseValue(`${context}[${values.length}]`, depth + 1));
      this.skipTrivia();
      if (this.peek() === ']') {
        this.index += 1;
        return values;
      }
      if (this.peek() !== ',') this.error(`${context} entries must be separated by commas`);
      this.index += 1;
    }
  }

  assignDotted(table, components, value, context) {
    let current = table;
    for (let index = 0; index < components.length - 1; index += 1) {
      const component = components[index];
      if (!Object.hasOwn(current, component)) {
        current[component] = this.createTable('dotted', table);
      } else {
        const existing = current[component];
        const metadata = this.isTable(existing) ? this.tableMetadata.get(existing) : null;
        if (metadata?.origin !== 'dotted' || metadata.dottedOwner !== table) {
          this.error(`${context} conflicts with existing key ${JSON.stringify(component)}`);
        }
      }
      current = current[component];
    }
    const final = components.at(-1);
    if (Object.hasOwn(current, final)) {
      this.error(`${context} duplicates key ${JSON.stringify(final)}`);
    }
    this.reserveNode(context);
    current[final] = value;
  }

  parseInlineTable(context, depth) {
    this.index += 1;
    const table = this.createTable('inline');
    this.skipHorizontal();
    while (true) {
      if (this.peek() === '}') {
        this.index += 1;
        return table;
      }
      if (this.peek() === '\n' || this.peek() === '\r' || this.peek() === '#') {
        this.error(`${context} must remain on one logical line`);
      }
      const components = this.parseDottedKey(`${context} key`);
      this.skipHorizontal();
      if (this.peek() !== '=') this.error(`${context} key is missing =`);
      this.index += 1;
      this.skipHorizontal();
      const value = this.parseValue(`${context}.${components.join('.')}`, depth + 1);
      this.assignDotted(table, components, value, context);
      this.skipHorizontal();
      if (this.peek() === '}') {
        this.index += 1;
        return table;
      }
      if (this.peek() !== ',') this.error(`${context} entries must be separated by commas`);
      this.index += 1;
      this.skipHorizontal();
      if (this.peek() === '}') this.error(`${context} must not contain a trailing comma`);
    }
  }

  parseBareScalar(context) {
    const start = this.index;
    while (this.index < this.source.length && !/[ \t\r\n,#\]}]/.test(this.peek())) {
      this.index += 1;
    }
    if (this.index === start) this.error(`${context} is missing a TOML value`);
    return new TomlScalar(this.source.slice(start, this.index));
  }

  parseValue(context, depth = 0) {
    if (depth > LIMITS.pathComponents) {
      this.error(`${context} exceeds the ${LIMITS.pathComponents}-level TOML nesting limit`);
    }
    if (this.peek() === '[') return this.parseArray(context, depth);
    if (this.peek() === '{') return this.parseInlineTable(context, depth);
    if (this.peek() === '"' || this.peek() === "'") return this.parseString(context);
    return this.parseBareScalar(context);
  }

  headerParent(components, context) {
    let current = this.root;
    for (const component of components) {
      if (!Object.hasOwn(current, component)) {
        current[component] = this.createTable('implicit');
      }
      let next = current[component];
      if (this.arrayTables.has(next)) next = next.at(-1);
      if (!this.isTable(next)) this.error(`${context} conflicts with existing key ${JSON.stringify(component)}`);
      if (this.tableMetadata.get(next).origin === 'inline') {
        this.error(`${context} cannot extend inline table ${JSON.stringify(component)}`);
      }
      current = next;
    }
    return current;
  }

  parseHeader() {
    const arrayTable = this.peek(1) === '[';
    this.index += arrayTable ? 2 : 1;
    this.skipHorizontal();
    const components = this.parseDottedKey('table header');
    this.skipHorizontal();
    const closing = arrayTable ? ']]' : ']';
    if (this.source.slice(this.index, this.index + closing.length) !== closing) {
      this.error('malformed metadata table header');
    }
    this.index += closing.length;
    this.finishLine('table header');
    const parent = this.headerParent(components.slice(0, -1), 'table header');
    const final = components.at(-1);
    if (arrayTable) {
      if (!Object.hasOwn(parent, final)) parent[final] = this.createArrayTable();
      const targets = parent[final];
      if (!this.arrayTables.has(targets)) {
        this.error(`array table conflicts with existing key ${JSON.stringify(final)}`);
      }
      if (targets.length >= LIMITS.metadataEntries) {
        this.error(`array table ${JSON.stringify(final)} contains more than ${LIMITS.metadataEntries} entries`);
      }
      const table = this.createTable('explicit');
      targets.push(table);
      this.current = table;
      return;
    }
    if (!Object.hasOwn(parent, final)) {
      parent[final] = this.createTable('explicit');
      this.current = parent[final];
      return;
    }
    const table = parent[final];
    if (!this.isTable(table) || this.tableMetadata.get(table).origin !== 'implicit') {
      this.error(`duplicate or conflicting table ${JSON.stringify(components.join('.'))}`);
    }
    this.tableMetadata.get(table).origin = 'explicit';
    this.current = table;
  }

  parseAssignment() {
    if (this.current === null) this.error('metadata assignment appears before a table header');
    const components = this.parseDottedKey('metadata assignment');
    this.skipHorizontal();
    if (this.peek() !== '=') {
      this.error(`metadata field ${JSON.stringify(components.join('.'))} is missing =`);
    }
    this.index += 1;
    this.skipHorizontal();
    const value = this.parseValue(`metadata field ${JSON.stringify(components.join('.'))}`);
    this.assignDotted(this.current, components, value, 'metadata assignment');
    this.finishLine(`metadata field ${JSON.stringify(components.join('.'))}`);
  }

  parse() {
    while (this.index < this.source.length) {
      this.skipTrivia();
      if (this.index >= this.source.length) break;
      if (this.peek() === '[') this.parseHeader();
      else this.parseAssignment();
    }
    return this.root;
  }
}

function tomlTable(value, label) {
  if (!isPlainObject(value)) fail(label, 'must be a table');
  return value;
}

function optionalTomlTable(table, key, label) {
  const value = table?.[key];
  if (value === undefined) return undefined;
  return tomlTable(value, label);
}

function rejectUnknownTomlFields(table, allowed, label) {
  const keys = Object.keys(table);
  if (keys.length > LIMITS.metadataEntries) {
    fail(label, `contains more than ${LIMITS.metadataEntries} keys`);
  }
  const allowedSet = new Set(allowed);
  const unknown = keys.filter((key) => !allowedSet.has(key)).sort(compareUtf8);
  if (unknown.length > 0) {
    fail(label, `contains unsupported field(s): ${unknown.map((key) => JSON.stringify(key)).join(', ')}`);
  }
}

function optionalTomlString(table, key, label) {
  const value = table?.[key];
  if (value === undefined) return null;
  if (typeof value !== 'string') fail(label, 'must be a string');
  return value;
}

function tomlStringArray(table, key, label) {
  const value = table?.[key];
  if (value === undefined) return [];
  if (!Array.isArray(value)) fail(label, 'must be an array of strings');
  if (value.length > LIMITS.metadataEntries) {
    fail(label, `contains more than ${LIMITS.metadataEntries} entries`);
  }
  value.forEach((entry, index) => {
    if (typeof entry !== 'string') fail(`${label}[${index}]`, 'must be a string');
  });
  return value;
}

function requiredTomlString(table, key, label) {
  const value = optionalTomlString(table, key, label);
  if (value === null || value.length === 0 || /^\p{White_Space}+$/u.test(value)) {
    fail(label, 'is required and must not be empty');
  }
  return value;
}

function normalizeNativeMetadata(table, label, budget) {
  if (table === undefined) return null;
  rejectUnknownTomlFields(table, ['path', 'targets'], label);
  const path = optionalTomlString(table, 'path', `${label}.path`);
  if (path !== null) {
    validatePortableRelativePath(path, `${label}.path`);
    budget.add(path, `${label}.path`);
  }
  const rawTargets = table.targets ?? [];
  if (!Array.isArray(rawTargets)) fail(`${label}.targets`, 'must be an array of tables');
  if (rawTargets.length > LIMITS.artifacts) {
    fail(`${label}.targets`, `contains more than ${LIMITS.artifacts} entries`);
  }
  const seen = new Set();
  const normalizedTargets = rawTargets.map((rawTarget, index) => {
    const targetLabel = `${label}.targets[${index}]`;
    const target = tomlTable(rawTarget, targetLabel);
    rejectUnknownTomlFields(target, ['target', 'library'], targetLabel);
    const triple = requiredTomlString(target, 'target', `${targetLabel}.target`);
    validatePortablePathComponent(triple, `${targetLabel}.target`);
    const segments = triple.split('-');
    if (segments.length < 3 || segments.some((segment) => !/^[a-z0-9_.]+$/.test(segment))) {
      fail(`${targetLabel}.target`, 'must be a canonical Rust target triple');
    }
    if (seen.has(triple)) fail(label, `contains duplicate target ${JSON.stringify(triple)}`);
    seen.add(triple);
    const library = requiredTomlString(target, 'library', `${targetLabel}.library`);
    validatePortablePathComponent(library, `${targetLabel}.library`);
    budget.add(triple, `${targetLabel}.target`);
    budget.add(library, `${targetLabel}.library`);
    return { target: triple, library };
  });
  return { path, targets: normalizedTargets };
}

/** Parse and normalize the strict module-metadata TOML suffix of `vo.mod`. */
export function parseVoModWebMetadata(source, label = 'vo.mod') {
  const root = new MetadataTomlParser(source, label).parse();
  rejectUnknownTomlFields(root, ['web', 'extension'], `${label} metadata`);
  const webTable = optionalTomlTable(root, 'web', `${label} [web]`);
  if (webTable !== undefined) rejectUnknownTomlFields(webTable, ['entry', 'include'], `${label} [web]`);
  const web = webTable === undefined
    ? null
    : normalizeWebProject({
        entry: optionalTomlString(webTable, 'entry', `${label} [web].entry`),
        include: tomlStringArray(webTable, 'include', `${label} [web].include`),
      }, `${label} [web]`);

  const extensionTable = optionalTomlTable(root, 'extension', `${label} [extension]`);
  if (extensionTable === undefined) {
    return { web, extension: null, webManifestExtension: null, declaredArtifacts: [] };
  }
  rejectUnknownTomlFields(
    extensionTable,
    ['name', 'include', 'native', 'wasm', 'web'],
    `${label} [extension]`,
  );
  const name = requiredTomlString(extensionTable, 'name', `${label} [extension].name`);
  const include = tomlStringArray(extensionTable, 'include', `${label} [extension].include`);
  const wasmTable = optionalTomlTable(extensionTable, 'wasm', `${label} [extension.wasm]`);
  let rawWasm = null;
  if (wasmTable !== undefined) {
    rejectUnknownTomlFields(
      wasmTable,
      ['type', 'wasm', 'js_glue', 'local_wasm', 'local_js_glue'],
      `${label} [extension.wasm]`,
    );
    const kind = requiredTomlString(wasmTable, 'type', `${label} [extension.wasm].type`);
    if (kind !== 'standalone' && kind !== 'bindgen') {
      fail(`${label} [extension.wasm].type`, `unsupported value ${JSON.stringify(kind)}`);
    }
    rawWasm = {
      kind: kind === 'standalone' ? 'Standalone' : 'Bindgen',
      wasm: requiredTomlString(wasmTable, 'wasm', `${label} [extension.wasm].wasm`),
      js_glue: optionalTomlString(wasmTable, 'js_glue', `${label} [extension.wasm].js_glue`),
      local_wasm: optionalTomlString(wasmTable, 'local_wasm', `${label} [extension.wasm].local_wasm`),
      local_js_glue: optionalTomlString(wasmTable, 'local_js_glue', `${label} [extension.wasm].local_js_glue`),
    };
  }
  const webRuntimeTable = optionalTomlTable(extensionTable, 'web', `${label} [extension.web]`);
  if (webRuntimeTable !== undefined) {
    rejectUnknownTomlFields(
      webRuntimeTable,
      ['entry', 'capabilities', 'js'],
      `${label} [extension.web]`,
    );
  }
  const jsTable = optionalTomlTable(webRuntimeTable, 'js', `${label} [extension.web.js]`);
  if (jsTable !== undefined && Object.keys(jsTable).length > LIMITS.metadataEntries) {
    fail(`${label} [extension.web.js]`, `contains more than ${LIMITS.metadataEntries} keys`);
  }
  let rawWebRuntime = null;
  if (webRuntimeTable !== undefined || jsTable !== undefined) {
    rawWebRuntime = {
      entry: optionalTomlString(webRuntimeTable, 'entry', `${label} [extension.web].entry`),
      capabilities: tomlStringArray(webRuntimeTable, 'capabilities', `${label} [extension.web].capabilities`),
      js_modules: jsTable ?? Object.create(null),
    };
  }
  const budget = metadataBudget();
  const moduleFiles = new PortablePathSet();
  const webManifestExtension = normalizeExtension(
    { name, include, wasm: rawWasm, web: rawWebRuntime },
    `${label} [extension]`,
    budget,
    moduleFiles,
  );
  const native = normalizeNativeMetadata(
    optionalTomlTable(extensionTable, 'native', `${label} [extension.native]`),
    `${label} [extension.native]`,
    budget,
  );
  const declaredArtifacts = [];
  for (const target of native?.targets ?? []) {
    declaredArtifacts.push(validateArtifactIdentity({
      kind: 'extension-native',
      target: target.target,
      name: target.library,
    }, `${label} native artifact`, { allowNative: true }));
  }
  if (webManifestExtension.wasm !== null) {
    declaredArtifacts.push({
      kind: 'extension-wasm',
      target: WASM_TARGET,
      name: webManifestExtension.wasm.wasm,
    });
    if (webManifestExtension.wasm.js_glue !== null) {
      declaredArtifacts.push({
        kind: 'extension-js-glue',
        target: WASM_TARGET,
        name: webManifestExtension.wasm.js_glue,
      });
    }
  }
  if (declaredArtifacts.length > LIMITS.artifacts) {
    fail(`${label} [extension]`, `declares more than ${LIMITS.artifacts} target artifacts`);
  }
  declaredArtifacts.sort(compareArtifactIdentity);
  return {
    web,
    extension: { ...webManifestExtension, native },
    webManifestExtension,
    declaredArtifacts,
  };
}

function jsonContractEqual(left, right) {
  return JSON.stringify(left) === JSON.stringify(right);
}

/**
 * Validate `vo.web.json` and bind every represented `[web]`/`[extension]`
 * field plus the exact browser artifact identity set back to packaged vo.mod.
 */
export function validateWebManifestVoModContract(
  value,
  voModSource,
  label = 'vo.web.json vs vo.mod',
  options = {},
) {
  const manifest = validateWebManifestContract(value, `${label} vo.web.json`, options);
  const metadata = parseVoModWebMetadata(voModSource, `${label} vo.mod`);
  if (!jsonContractEqual(manifest.web, metadata.web)) {
    fail(label, 'web metadata does not exactly match packaged vo.mod');
  }
  if (!jsonContractEqual(manifest.extension, metadata.webManifestExtension)) {
    fail(label, 'extension metadata does not exactly match packaged vo.mod');
  }
  const declaredBrowser = metadata.declaredArtifacts
    .filter((artifact) => artifact.kind !== 'extension-native')
    .map(artifactKey);
  const manifestArtifacts = manifest.artifacts.map(artifactKey);
  if (!jsonContractEqual(manifestArtifacts, declaredBrowser)) {
    fail(label, 'browser artifact identities do not exactly match packaged vo.mod');
  }
  return { manifest, metadata };
}
