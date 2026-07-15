import { artifactKey, validateArtifactIdentity } from './quickplay_artifact_paths.mjs';

const LOCK_VERSION = 2;
const MAX_LOCK_BYTES = 128 * 1024 * 1024;
const MAX_MODULE_TEXT_BYTES = 16 * 1024 * 1024;
const MAX_RESOLVED = 10_000;
const MAX_EDGES = 10_000;
const MAX_ARTIFACTS = 10_000;
const MAX_GRAPH_EDGES = 100_000;
const MAX_GRAPH_ARTIFACTS = 100_000;
const MAX_ARTIFACT_BYTES = 512 * 1024 * 1024;
const MAX_MODULE_PATH_BYTES = 255;
const MAX_LOCAL_NAME_BYTES = 255;
const MAX_VERSION_BYTES = 254;
const MAX_INLINE_DEPENDENCY_BYTES = MAX_MODULE_PATH_BYTES + MAX_VERSION_BYTES + 128;
const MAX_MODULE_DIRECTIVE_BYTES = MAX_INLINE_DEPENDENCY_BYTES;
const MAX_U64 = 18_446_744_073_709_551_615n;
// A canonical lock needs at most ten non-artifact lines per module, one line
// per graph edge, seven lines per artifact, and seven top-level/final lines.
// Keeping the scanner budget tied to the protocol budgets rejects newline
// bombs without excluding any canonical v2 lock that the renderer can emit.
const MAX_LOCK_LINES = 7
  + (MAX_RESOLVED * 10)
  + MAX_GRAPH_EDGES
  + (MAX_GRAPH_ARTIFACTS * 7);
const MAX_MODULE_LINES = 100_000;
const MAX_LEGACY_DEPS_TEXT_BYTES = MAX_MODULE_TEXT_BYTES;
const LOCK_SECTION_FIELDS = Object.freeze({
  top: new Set(['version', 'created_by']),
  root: new Set(['module', 'vo']),
  resolved: new Set(['path', 'version', 'vo', 'commit', 'release_manifest', 'source', 'deps']),
  artifact: new Set(['kind', 'target', 'name', 'size', 'digest']),
});

function fail(label, detail) {
  throw new Error(`${label}: ${detail}`);
}

export function decodeModuleTextUtf8(bytes, label = 'module text') {
  if (!(bytes instanceof Uint8Array)) fail(label, 'expected raw bytes');
  if (bytes.byteLength > MAX_LOCK_BYTES) {
    fail(label, `exceeds the ${MAX_LOCK_BYTES}-byte text boundary`);
  }
  try {
    return new TextDecoder('utf-8', { fatal: true, ignoreBOM: true }).decode(bytes);
  } catch (error) {
    fail(label, `invalid UTF-8: ${error.message}`);
  }
}

function validateTextBoundary(source, label, maxBytes) {
  if (typeof source !== 'string') fail(label, 'expected text');
  // Every UTF-16 code unit contributes at least one UTF-8 byte. This cheap
  // check can reject an oversized input without first walking all of it.
  if (source.length > maxBytes || Buffer.byteLength(source, 'utf8') > maxBytes) {
    fail(label, `exceeds the ${maxBytes}-byte text limit`);
  }
}

function* scanBoundedLines(source, label, maxBytes, maxLines) {
  validateTextBoundary(source, label, maxBytes);
  let start = 0;
  let lineNumber = 0;
  while (true) {
    lineNumber += 1;
    if (lineNumber > maxLines) {
      fail(label, `line count exceeds the ${maxLines}-line limit`);
    }
    const newline = source.indexOf('\n', start);
    if (newline === -1) {
      yield { lineNumber, rawLine: source.slice(start) };
      return;
    }
    yield { lineNumber, rawLine: source.slice(start, newline) };
    start = newline + 1;
  }
}

function parseString(value, label, maxCanonicalBytes = MAX_LOCK_BYTES) {
  if (!value.startsWith('"')) fail(label, 'expected a TOML basic string');
  const maxWireBytes = maxCanonicalBytes + 2;
  if (
    value.length > maxWireBytes
    || Buffer.byteLength(value, 'utf8') > maxWireBytes
  ) {
    fail(label, `canonical string exceeds the ${maxCanonicalBytes}-byte value limit`);
  }
  let parsed = '';
  for (let index = 1; index < value.length; index += 1) {
    const char = value[index];
    if (char === '"') {
      if (index + 1 !== value.length) {
        fail(label, 'string has trailing data');
      }
      return parsed;
    }
    if (char !== '\\') {
      const codepoint = value.codePointAt(index);
      if (
        (codepoint >= 0xd800 && codepoint <= 0xdfff)
        || codepoint <= 0x08
        || (codepoint >= 0x0a && codepoint <= 0x1f)
        || codepoint === 0x7f
      ) {
        fail(label, `unescaped or invalid Unicode scalar U+${codepoint.toString(16).toUpperCase()}`);
      }
      parsed += String.fromCodePoint(codepoint);
      if (codepoint > 0xffff) index += 1;
      continue;
    }

    index += 1;
    if (index >= value.length) fail(label, 'unterminated escape sequence');
    const escaped = value[index];
    const simple = {
      b: '\b',
      f: '\f',
      n: '\n',
      r: '\r',
      t: '\t',
      '"': '"',
      '\\': '\\',
    };
    if (Object.hasOwn(simple, escaped)) {
      parsed += simple[escaped];
      continue;
    }
    if (escaped !== 'u' && escaped !== 'U') {
      fail(label, `unsupported TOML escape \\${escaped}`);
    }
    const digits = escaped === 'u' ? 4 : 8;
    const hex = value.slice(index + 1, index + 1 + digits);
    if (hex.length !== digits || !/^[0-9A-Fa-f]+$/.test(hex)) {
      fail(label, `invalid \\${escaped} Unicode escape`);
    }
    const codepoint = Number.parseInt(hex, 16);
    if (codepoint > 0x10ffff || (codepoint >= 0xd800 && codepoint <= 0xdfff)) {
      fail(label, `Unicode escape is not a scalar value: U+${hex.toUpperCase()}`);
    }
    parsed += String.fromCodePoint(codepoint);
    index += digits;
  }
  fail(label, 'string has no closing quote');
}

function parseInteger(value, label) {
  if (value.length > String(Number.MAX_SAFE_INTEGER).length) {
    fail(label, 'integer exceeds JavaScript safe range');
  }
  if (!/^(0|[1-9][0-9]*)$/.test(value)) fail(label, 'expected a non-negative integer');
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed)) fail(label, 'integer exceeds JavaScript safe range');
  return parsed;
}

function parseInlineDependency(line, label) {
  if (
    line.length > MAX_INLINE_DEPENDENCY_BYTES
    || Buffer.byteLength(line, 'utf8') > MAX_INLINE_DEPENDENCY_BYTES
  ) {
    fail(label, `dependency entry exceeds the ${MAX_INLINE_DEPENDENCY_BYTES}-byte canonical limit`);
  }
  const match = line.match(
    /^\{\s*module\s*=\s*((?:"(?:[^"\\]|\\.)*"))\s*,\s*constraint\s*=\s*((?:"(?:[^"\\]|\\.)*"))\s*\},?$/,
  );
  if (!match) {
    fail(label, 'expected `{ module = "...", constraint = "..." },`');
  }
  return {
    module: parseString(match[1], `${label}.module`, MAX_MODULE_PATH_BYTES),
    constraint: parseString(match[2], `${label}.constraint`, MAX_VERSION_BYTES + 1),
  };
}

function lockStringFieldLimit(section, key) {
  if (section === 'top') return MAX_LOCK_BYTES;
  if (section === 'root' && key === 'module') {
    return Math.max(MAX_MODULE_PATH_BYTES, 'local/'.length + MAX_LOCAL_NAME_BYTES);
  }
  if (key === 'path') return MAX_MODULE_PATH_BYTES;
  if (key === 'version' || key === 'vo') return MAX_VERSION_BYTES + 1;
  if (key === 'commit') return 40;
  if (key === 'release_manifest' || key === 'source' || key === 'digest') return 71;
  return 255;
}

function setUnique(target, key, value, label) {
  if (Object.hasOwn(target, key)) fail(label, `duplicate field ${key}`);
  target[key] = value;
}

function requireFields(value, fields, label) {
  for (const field of fields) {
    if (!Object.hasOwn(value, field)) fail(label, `missing field ${field}`);
  }
  for (const field of Object.keys(value)) {
    if (!fields.includes(field)) fail(label, `unknown field ${field}`);
  }
}

function validateUnique(items, key, label) {
  const seen = new Set();
  for (const item of items) {
    const identity = key(item);
    if (seen.has(identity)) fail(label, `duplicate entry ${identity}`);
    seen.add(identity);
  }
}

function parseLockStructure(source, label) {
  const lock = { resolved: [] };
  let section = 'top';
  let current = null;
  let artifact = null;
  let dependencyIndex = 0;
  let graphEdges = 0;
  let graphArtifacts = 0;

  for (const { lineNumber, rawLine } of scanBoundedLines(
    source,
    label,
    MAX_LOCK_BYTES,
    MAX_LOCK_LINES,
  )) {
    const line = rawLine.trim();
    if (line === '' || line.startsWith('#')) continue;
    const lineLabel = `${label}:${lineNumber}`;

    if (section === 'deps') {
      if (line === ']') {
        section = 'resolved';
        continue;
      }
      if (current.deps.length >= MAX_EDGES) fail(lineLabel, 'too many dependency edges');
      graphEdges += 1;
      if (graphEdges > MAX_GRAPH_EDGES) {
        fail(lineLabel, `locked graph exceeds the ${MAX_GRAPH_EDGES}-edge aggregate limit`);
      }
      current.deps.push(parseInlineDependency(line, `${lineLabel}.deps[${dependencyIndex}]`));
      dependencyIndex += 1;
      continue;
    }

    if (line === '[root]') {
      if (lock.root) fail(lineLabel, 'duplicate [root] section');
      lock.root = {};
      section = 'root';
      current = null;
      artifact = null;
      continue;
    }
    if (line === '[[resolved]]') {
      if (lock.resolved.length >= MAX_RESOLVED) fail(lineLabel, 'too many resolved modules');
      current = { artifacts: [] };
      lock.resolved.push(current);
      artifact = null;
      section = 'resolved';
      continue;
    }
    if (line === '[[resolved.artifact]]') {
      if (!current) fail(lineLabel, 'artifact appears before a resolved module');
      if (current.artifacts.length >= MAX_ARTIFACTS) fail(lineLabel, 'too many artifacts');
      graphArtifacts += 1;
      if (graphArtifacts > MAX_GRAPH_ARTIFACTS) {
        fail(lineLabel, `locked graph exceeds the ${MAX_GRAPH_ARTIFACTS}-artifact aggregate limit`);
      }
      artifact = {};
      current.artifacts.push(artifact);
      section = 'artifact';
      continue;
    }

    const pair = line.match(/^([A-Za-z0-9_]+)\s*=\s*(.*)$/);
    if (!pair) fail(lineLabel, 'unsupported TOML syntax');
    const [, key, rawValue] = pair;
    if (section === 'resolved' && key === 'deps') {
      if (Object.hasOwn(current, 'deps')) fail(lineLabel, 'duplicate field deps');
      current.deps = [];
      if (rawValue === '[]') continue;
      if (rawValue !== '[') fail(lineLabel, 'deps must be [] or a canonical multiline array');
      section = 'deps';
      dependencyIndex = 0;
      continue;
    }

    const numeric = (key === 'version' && section === 'top')
      || (key === 'size' && section === 'artifact');
    const target = section === 'top'
      ? lock
      : section === 'root'
        ? lock.root
        : section === 'artifact'
          ? artifact
          : current;
    if (!target) fail(lineLabel, `field ${key} appears outside its section`);
    if (!LOCK_SECTION_FIELDS[section]?.has(key)) {
      fail(lineLabel, `unknown field ${key}`);
    }
    const value = numeric
      ? parseInteger(rawValue, lineLabel)
      : parseString(rawValue, lineLabel, lockStringFieldLimit(section, key));
    setUnique(target, key, value, lineLabel);
  }
  if (section === 'deps') fail(label, 'unterminated deps array');

  requireFields(lock, ['version', 'created_by', 'root', 'resolved'], label);
  if (lock.version !== LOCK_VERSION) fail(label, `unsupported lock version ${lock.version}`);
  requireFields(lock.root, ['module', 'vo'], `${label} [root]`);
  for (const [index, module] of lock.resolved.entries()) {
    const moduleLabel = `${label} resolved[${index}]`;
    requireFields(
      module,
      ['path', 'version', 'vo', 'commit', 'release_manifest', 'source', 'deps', 'artifacts'],
      moduleLabel,
    );
    validateUnique(module.deps, (edge) => edge.module, `${moduleLabel}.deps`);
    for (const [edgeIndex, edge] of module.deps.entries()) {
      requireFields(edge, ['module', 'constraint'], `${moduleLabel}.deps[${edgeIndex}]`);
    }
    validateUnique(
      module.artifacts,
      (entry) => `${entry.kind}\u0000${entry.target}\u0000${entry.name}`,
      `${moduleLabel}.artifacts`,
    );
    for (const [artifactIndex, entry] of module.artifacts.entries()) {
      requireFields(
        entry,
        ['kind', 'target', 'name', 'size', 'digest'],
        `${moduleLabel}.artifacts[${artifactIndex}]`,
      );
    }
  }
  validateUnique(lock.resolved, (module) => module.path, `${label}.resolved`);
  return lock;
}

function compareUtf8(left, right) {
  return Buffer.compare(Buffer.from(left, 'utf8'), Buffer.from(right, 'utf8'));
}

function parseSemVer(value, label) {
  if (
    typeof value !== 'string'
    || value.length > MAX_VERSION_BYTES
    || Buffer.byteLength(value, 'utf8') > MAX_VERSION_BYTES
  ) {
    fail(label, `version exceeds the ${MAX_VERSION_BYTES}-byte canonical limit`);
  }
  if (value.includes('+')) fail(label, 'build metadata is not allowed');
  const match = /^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(?:-([0-9A-Za-z-]+(?:\.[0-9A-Za-z-]+)*))?$/.exec(value);
  if (!match) fail(label, `invalid semantic version ${JSON.stringify(value)}`);
  const numeric = match.slice(1, 4).map((part) => BigInt(part));
  if (numeric.some((part) => part > MAX_U64)) fail(label, 'numeric version component exceeds u64');
  const pre = (match[4]?.split('.') ?? []).map((part) => {
    if (!/^[0-9]+$/.test(part)) return { alpha: part };
    if ((part.length > 1 && part.startsWith('0')) || BigInt(part) > MAX_U64) {
      fail(label, `invalid numeric prerelease component ${part}`);
    }
    return { numeric: BigInt(part) };
  });
  return {
    major: numeric[0],
    minor: numeric[1],
    patch: numeric[2],
    pre,
    text: value,
  };
}

function compareSemVer(left, right) {
  for (const field of ['major', 'minor', 'patch']) {
    if (left[field] < right[field]) return -1;
    if (left[field] > right[field]) return 1;
  }
  if (left.pre.length === 0 || right.pre.length === 0) {
    if (left.pre.length === right.pre.length) return 0;
    return left.pre.length === 0 ? 1 : -1;
  }
  const length = Math.min(left.pre.length, right.pre.length);
  for (let index = 0; index < length; index += 1) {
    const leftPart = left.pre[index];
    const rightPart = right.pre[index];
    const leftNumeric = Object.hasOwn(leftPart, 'numeric');
    const rightNumeric = Object.hasOwn(rightPart, 'numeric');
    if (leftNumeric && rightNumeric) {
      if (leftPart.numeric < rightPart.numeric) return -1;
      if (leftPart.numeric > rightPart.numeric) return 1;
    } else if (leftNumeric !== rightNumeric) {
      return leftNumeric ? -1 : 1;
    } else if (leftPart.alpha !== rightPart.alpha) {
      return leftPart.alpha < rightPart.alpha ? -1 : 1;
    }
  }
  return Math.sign(left.pre.length - right.pre.length);
}

function parseExactVersion(value, label) {
  if (typeof value !== 'string' || !value.startsWith('v')) {
    fail(label, 'exact dependency version must start with v');
  }
  return parseSemVer(value.slice(1), label);
}

function parseConstraint(value, kind, label) {
  if (typeof value !== 'string') fail(label, 'constraint must be a string');
  if (value.length > MAX_VERSION_BYTES + 1) {
    fail(label, `constraint exceeds the ${MAX_VERSION_BYTES + 1}-byte canonical limit`);
  }
  if (value.startsWith('^') || value.startsWith('~')) {
    return {
      op: value[0] === '^' ? 'compatible' : 'patch',
      version: parseSemVer(value.slice(1), label),
      text: value,
    };
  }
  if (kind === 'dependency') {
    if (!value.startsWith('v')) {
      fail(label, "dependency constraint must start with '^', '~', or 'v'");
    }
    return { op: 'exact', version: parseExactVersion(value, label), text: value };
  }
  if (value.startsWith('v')) fail(label, 'toolchain constraint must not start with v');
  return { op: 'exact', version: parseSemVer(value, label), text: value };
}

function prereleaseAllowed(candidate, lower) {
  if (candidate.pre.length === 0) return true;
  return lower.pre.length > 0
    && candidate.major === lower.major
    && candidate.minor === lower.minor
    && candidate.patch === lower.patch;
}

function constraintSatisfies(constraint, version) {
  if (constraint.op === 'exact') return compareSemVer(version, constraint.version) === 0;
  if (!prereleaseAllowed(version, constraint.version)) return false;
  if (compareSemVer(version, constraint.version) < 0) return false;
  if (constraint.op === 'patch') {
    return version.major === constraint.version.major
      && version.minor === constraint.version.minor;
  }
  if (constraint.version.major !== 0n) return version.major === constraint.version.major;
  if (constraint.version.minor !== 0n) {
    return version.major === 0n && version.minor === constraint.version.minor;
  }
  return version.major === 0n && version.minor === 0n
    && version.patch === constraint.version.patch;
}

function stableVersion(major, minor, patch) {
  return { major, minor, patch, pre: [], text: `${major}.${minor}.${patch}` };
}

function constraintRange(constraint) {
  const lower = constraint.version;
  let upper;
  if (constraint.op === 'compatible') {
    if (lower.major !== 0n) {
      upper = lower.major === MAX_U64 ? null : stableVersion(lower.major + 1n, 0n, 0n);
    } else if (lower.minor !== 0n) {
      upper = lower.minor === MAX_U64
        ? stableVersion(1n, 0n, 0n)
        : stableVersion(0n, lower.minor + 1n, 0n);
    } else {
      upper = lower.patch === MAX_U64
        ? stableVersion(0n, 1n, 0n)
        : stableVersion(0n, 0n, lower.patch + 1n);
    }
  } else if (lower.minor !== MAX_U64) {
    upper = stableVersion(lower.major, lower.minor + 1n, 0n);
  } else {
    upper = lower.major === MAX_U64 ? null : stableVersion(lower.major + 1n, 0n, 0n);
  }
  return { lower, upper };
}

function constraintIsSubset(left, right) {
  if (left.op === 'exact') return constraintSatisfies(right, left.version);
  if (right.op === 'exact') {
    return compareSemVer(left.version, right.version) === 0
      && rangeAcceptsOnlyLowerVersion(left);
  }
  if (
    left.version.pre.length > 0
    && (
      right.version.pre.length === 0
      || left.version.major !== right.version.major
      || left.version.minor !== right.version.minor
      || left.version.patch !== right.version.patch
    )
  ) {
    return false;
  }
  const leftRange = constraintRange(left);
  const rightRange = constraintRange(right);
  const upperContained = rightRange.upper === null
    || (leftRange.upper !== null && compareSemVer(leftRange.upper, rightRange.upper) <= 0);
  return compareSemVer(rightRange.lower, leftRange.lower) <= 0 && upperContained;
}

function rangeAcceptsOnlyLowerVersion(constraint) {
  if (constraint.version.pre.length > 0) return false;
  if (constraint.op === 'compatible' && constraint.version.major === 0n) {
    return constraint.version.minor === 0n || constraint.version.patch === MAX_U64;
  }
  if (constraint.op === 'compatible') {
    return constraint.version.minor === MAX_U64 && constraint.version.patch === MAX_U64;
  }
  return constraint.op === 'patch' && constraint.version.patch === MAX_U64;
}

function validateModulePath(value, versionMajor, label) {
  if (
    typeof value !== 'string'
    || value.length > MAX_MODULE_PATH_BYTES
    || Buffer.byteLength(value, 'utf8') > MAX_MODULE_PATH_BYTES
    || !value.startsWith('github.com/')
    || value.endsWith('/')
  ) {
    fail(label, `invalid canonical module path ${JSON.stringify(value)}`);
  }
  const segments = value.split('/');
  if (segments.length < 3) fail(label, 'module path requires github.com/<owner>/<repository>');
  for (const [index, segment] of segments.entries()) {
    if (
      !/^[a-z0-9][a-z0-9._-]*$/.test(segment)
      || segment.endsWith('.')
      || ['con', 'prn', 'aux', 'nul', 'conin$', 'conout$'].includes(segment.split('.')[0])
      || /^(?:com|lpt)[1-9]$/.test(segment.split('.')[0])
    ) {
      fail(label, `invalid canonical module path segment ${index}: ${JSON.stringify(segment)}`);
    }
  }
  const suffix = /^v([0-9]+)$/.exec(segments.at(-1));
  if (suffix) {
    if ((suffix[1].length > 1 && suffix[1].startsWith('0')) || BigInt(suffix[1]) > MAX_U64) {
      fail(label, 'invalid module major-version suffix');
    }
    const suffixMajor = BigInt(suffix[1]);
    if (suffixMajor < 2n || (versionMajor != null && suffixMajor !== versionMajor)) {
      fail(label, 'module major-version suffix is incompatible with selected version');
    }
  } else if (versionMajor != null && versionMajor > 1n) {
    fail(label, 'unsuffixed module path cannot select a version above v1');
  }
  return value;
}

function validateRootIdentity(value, label) {
  if (typeof value === 'string' && value.startsWith('local/')) {
    const name = value.slice('local/'.length);
    if (
      name.length > MAX_LOCAL_NAME_BYTES
      || Buffer.byteLength(name, 'utf8') > MAX_LOCAL_NAME_BYTES
    ) {
      fail(label, `invalid local root identity ${JSON.stringify(value)}`);
    }
    const dot = name.indexOf('.');
    const stem = dot === -1 ? name : name.slice(0, dot);
    if (
      !/^[a-z0-9][a-z0-9._-]*$/.test(name)
      || name.endsWith('.')
      || ['con', 'prn', 'aux', 'nul', 'conin$', 'conout$'].includes(stem)
      || /^(?:com|lpt)[1-9]$/.test(stem)
    ) {
      fail(label, `invalid local root identity ${JSON.stringify(value)}`);
    }
    return { kind: 'local', value };
  }
  validateModulePath(value, null, label);
  return { kind: 'github', value };
}

function validateDigest(value, label) {
  if (!/^sha256:[0-9a-f]{64}$/.test(value)) {
    fail(label, 'digest must use sha256:<64 lowercase hex digits>');
  }
}

function validateLockSemantics(lock, label) {
  const rootIdentity = validateRootIdentity(lock.root.module, `${label}.root.module`);
  const rootVo = parseConstraint(lock.root.vo, 'toolchain', `${label}.root.vo`);
  const selected = new Map();
  const moduleFacts = new Map();
  let graphEdges = 0;

  for (const [index, module] of lock.resolved.entries()) {
    const moduleLabel = `${label}.resolved[${index}]`;
    const version = parseExactVersion(module.version, `${moduleLabel}.version`);
    validateModulePath(module.path, version.major, `${moduleLabel}.path`);
    if (index > 0 && compareUtf8(lock.resolved[index - 1].path, module.path) >= 0) {
      fail(label, 'resolved modules must be unique and sorted by module path');
    }
    const vo = parseConstraint(module.vo, 'toolchain', `${moduleLabel}.vo`);
    if (!/^[0-9a-f]{40}$/.test(module.commit)) {
      fail(`${moduleLabel}.commit`, 'commit must be exactly 40 lowercase hex digits');
    }
    validateDigest(module.release_manifest, `${moduleLabel}.release_manifest`);
    validateDigest(module.source, `${moduleLabel}.source`);

    graphEdges += module.deps.length;
    if (!Number.isSafeInteger(graphEdges) || graphEdges > MAX_GRAPH_EDGES) {
      fail(label, `locked graph exceeds the ${MAX_GRAPH_EDGES}-edge aggregate limit`);
    }
    const edgeFacts = [];
    for (const [edgeIndex, edge] of module.deps.entries()) {
      const edgeLabel = `${moduleLabel}.deps[${edgeIndex}]`;
      const constraint = parseConstraint(edge.constraint, 'dependency', `${edgeLabel}.constraint`);
      validateModulePath(edge.module, constraint.version.major, `${edgeLabel}.module`);
      if (edge.module === module.path) fail(edgeLabel, 'a module must not require itself');
      if (edgeIndex > 0 && compareUtf8(module.deps[edgeIndex - 1].module, edge.module) >= 0) {
        fail(`${moduleLabel}.deps`, 'dependency edges must be unique and sorted by module path');
      }
      edgeFacts.push({ edge, constraint });
    }

    for (const [artifactIndex, artifact] of module.artifacts.entries()) {
      const artifactLabel = `${moduleLabel}.artifacts[${artifactIndex}]`;
      try {
        validateArtifactIdentity(artifact);
      } catch (error) {
        fail(artifactLabel, error.message);
      }
      if (
        !Number.isSafeInteger(artifact.size)
        || artifact.size < 0
        || artifact.size > MAX_ARTIFACT_BYTES
      ) {
        fail(`${artifactLabel}.size`, `must not exceed ${MAX_ARTIFACT_BYTES} bytes`);
      }
      validateDigest(artifact.digest, `${artifactLabel}.digest`);
      if (
        artifactIndex > 0
        && compareUtf8(artifactKey(module.artifacts[artifactIndex - 1]), artifactKey(artifact)) >= 0
      ) {
        fail(`${moduleLabel}.artifacts`, 'artifacts must be unique and sorted by (kind, target, name)');
      }
    }

    selected.set(module.path, module);
    moduleFacts.set(module.path, { version, vo, edgeFacts });
    if (!constraintIsSubset(rootVo, vo)) {
      fail(moduleLabel, `toolchain constraint ${lock.root.vo} is not covered by ${module.vo}`);
    }
  }

  if (rootIdentity.kind === 'github' && selected.has(rootIdentity.value)) {
    fail(label, `root module ${rootIdentity.value} must not appear in resolved`);
  }
  for (const module of lock.resolved) {
    for (const { edge, constraint } of moduleFacts.get(module.path).edgeFacts) {
      const dependency = moduleFacts.get(edge.module);
      if (!dependency) {
        fail(label, `${module.path} requires ${edge.module}, which is absent from vo.lock`);
      }
      if (!constraintSatisfies(constraint, dependency.version)) {
        fail(label, `${module.path} requires ${edge.module} ${edge.constraint}, but vo.lock selects ${selected.get(edge.module).version}`);
      }
    }
  }
}

function firstDifference(left, right) {
  const limit = Math.min(left.length, right.length);
  for (let index = 0; index < limit; index += 1) {
    if (left[index] !== right[index]) return index;
  }
  return limit;
}

function trimModuleLine(rawLine) {
  let end = rawLine.length;
  if (end > 0 && rawLine.charCodeAt(end - 1) === 0x0d) end -= 1;
  let start = 0;
  while (
    start < end
    && (rawLine.charCodeAt(start) === 0x20 || rawLine.charCodeAt(start) === 0x09)
  ) {
    start += 1;
  }
  while (
    end > start
    && (rawLine.charCodeAt(end - 1) === 0x20 || rawLine.charCodeAt(end - 1) === 0x09)
  ) {
    end -= 1;
  }
  return rawLine.slice(start, end);
}

function scanModuleDirectiveTokens(line, limit) {
  const tokens = [];
  let index = 0;
  while (index < line.length && tokens.length < limit) {
    while (
      index < line.length
      && (line.charCodeAt(index) === 0x20 || line.charCodeAt(index) === 0x09)
    ) {
      index += 1;
    }
    if (index === line.length) break;
    const start = index;
    while (
      index < line.length
      && line.charCodeAt(index) !== 0x20
      && line.charCodeAt(index) !== 0x09
    ) {
      index += 1;
    }
    tokens.push(line.slice(start, index));
  }
  return tokens;
}

export function parseVoLockV2(source, label = 'vo.lock') {
  const lock = parseLockStructure(source, label);
  validateLockSemantics(lock, label);
  const canonical = renderVoLockV2Unchecked(lock);
  if (source !== canonical) {
    fail(label, `lock is not canonical v2 (first differing UTF-16 offset ${firstDifference(source, canonical)})`);
  }
  return lock;
}

export function parseVoModRootContract(source, label = 'vo.mod') {
  let module = null;
  let vo = null;
  const require = [];
  const required = new Set();
  for (const { lineNumber, rawLine } of scanBoundedLines(
    source,
    label,
    MAX_MODULE_TEXT_BYTES,
    MAX_MODULE_LINES,
  )) {
    const line = trimModuleLine(rawLine);
    if (line === '' || line.startsWith('//')) continue;
    if (line.startsWith('[')) break;
    const lineLabel = `${label}:${lineNumber}`;
    if (
      line.length > MAX_MODULE_DIRECTIVE_BYTES
      || Buffer.byteLength(line, 'utf8') > MAX_MODULE_DIRECTIVE_BYTES
    ) {
      fail(lineLabel, `directive exceeds the ${MAX_MODULE_DIRECTIVE_BYTES}-byte canonical limit`);
    }
    const parts = scanModuleDirectiveTokens(line, 4);
    if (parts[0] === 'module') {
      if (module !== null) fail(lineLabel, "duplicate 'module' directive");
      if (parts.length !== 2) fail(lineLabel, "'module' requires exactly one path");
      validateRootIdentity(parts[1], `${lineLabel}.module`);
      module = parts[1];
    } else if (parts[0] === 'vo') {
      if (vo !== null) fail(lineLabel, "duplicate 'vo' directive");
      if (parts.length !== 2) fail(lineLabel, "'vo' requires exactly one constraint");
      parseConstraint(parts[1], 'toolchain', `${lineLabel}.vo`);
      vo = parts[1];
    } else if (parts[0] === 'require') {
      if (parts.length !== 3) fail(lineLabel, "'require' needs <module-path> <constraint>");
      if (require.length >= MAX_EDGES) fail(lineLabel, 'too many direct dependencies');
      if (required.has(parts[1])) fail(lineLabel, `duplicate require for ${parts[1]}`);
      const constraint = parseConstraint(parts[2], 'dependency', `${lineLabel}.constraint`);
      validateModulePath(parts[1], constraint.version.major, `${lineLabel}.module`);
      required.add(parts[1]);
      require.push({ module: parts[1], constraint: parts[2] });
    } else if (parts[0] === 'replace') {
      fail(lineLabel, 'replace directives are unsupported; use vo.work');
    } else {
      fail(lineLabel, `unknown directive ${JSON.stringify(parts[0])}`);
    }
  }
  if (module === null) fail(label, "missing 'module' directive");
  if (vo === null) fail(label, "missing 'vo' directive");
  if (module.startsWith('github.com/') && required.has(module)) {
    fail(label, `root module ${module} must not require itself`);
  }
  return { module, vo, require };
}

export function validateVoLockV2RootGraph(lock, modContract, label = 'vo.lock') {
  if (lock.root.module !== modContract.module) {
    fail(label, `root.module ${lock.root.module} does not match vo.mod module ${modContract.module}`);
  }
  if (lock.root.vo !== modContract.vo) {
    fail(label, `root.vo ${lock.root.vo} does not match vo.mod vo ${modContract.vo}`);
  }
  let edgeCount = modContract.require.length;
  for (const module of lock.resolved) {
    edgeCount += module.deps.length;
    if (!Number.isSafeInteger(edgeCount) || edgeCount > MAX_GRAPH_EDGES) {
      fail(label, `root and transitive graph exceeds the ${MAX_GRAPH_EDGES}-edge aggregate limit`);
    }
  }
  const selected = new Map(lock.resolved.map((module) => [module.path, module]));
  const reachable = new Set();
  const pending = [];
  for (const requirement of modContract.require) {
    const dependency = selected.get(requirement.module);
    if (!dependency) fail(label, `root requires ${requirement.module}, which is absent from vo.lock`);
    const constraint = parseConstraint(requirement.constraint, 'dependency', `${label}.root requirement`);
    const version = parseExactVersion(dependency.version, `${label}.${requirement.module}.version`);
    if (!constraintSatisfies(constraint, version)) {
      fail(label, `root requires ${requirement.module} ${requirement.constraint}, but vo.lock selects ${dependency.version}`);
    }
    pending.push(requirement.module);
  }
  while (pending.length > 0) {
    const modulePath = pending.pop();
    if (reachable.has(modulePath)) continue;
    reachable.add(modulePath);
    for (const dependency of selected.get(modulePath).deps) pending.push(dependency.module);
  }
  const orphaned = lock.resolved
    .map((module) => module.path)
    .filter((modulePath) => !reachable.has(modulePath));
  if (orphaned.length > 0) {
    fail(label, `resolved contains modules unreachable from root: ${orphaned.join(', ')}`);
  }
  return lock;
}

export function validatePackagedModuleSet(lock, modules, label = 'deps.modules') {
  if (!Array.isArray(modules)) fail(label, 'must be an array');
  if (modules.length > MAX_RESOLVED) {
    fail(label, `contains more than ${MAX_RESOLVED} modules`);
  }
  const packaged = new Map();
  for (const [index, module] of modules.entries()) {
    if (!module || typeof module !== 'object' || Array.isArray(module)) {
      fail(`${label}[${index}]`, 'must be an object');
    }
    if (packaged.has(module.module)) fail(label, `duplicate packaged module ${module.module}`);
    packaged.set(module.module, module);
  }
  const selected = new Map(lock.resolved.map((module) => [module.path, module]));
  const missing = [...selected.keys()].filter((module) => !packaged.has(module));
  const extra = [...packaged.keys()].filter((module) => !selected.has(module));
  if (missing.length > 0 || extra.length > 0) {
    fail(label, `module set differs from vo.lock; missing [${missing.join(', ')}], extra [${extra.join(', ')}]`);
  }
  for (const [modulePath, locked] of selected) {
    if (packaged.get(modulePath).version !== locked.version) {
      fail(label, `${modulePath} embeds ${packaged.get(modulePath).version}, vo.lock selects ${locked.version}`);
    }
  }
  return modules;
}

/// Read the former canonical v1 lock solely so the quickplay packager can
/// replace every entry from authenticated v2 release requirements in one
/// migration. General consumers must use `parseVoLockV2`.
export function parseVoLockForV2Migration(source, label = 'vo.lock') {
  let version;
  for (const { rawLine } of scanBoundedLines(
    source,
    label,
    MAX_LOCK_BYTES,
    MAX_LOCK_LINES,
  )) {
    const match = /^version\s*=\s*(\d+)\s*$/.exec(rawLine);
    if (match) {
      version = match[1];
      break;
    }
  }
  if (version === String(LOCK_VERSION)) return parseVoLockV2(source, label);
  if (version !== '1') fail(label, `unsupported lock version ${version ?? '<missing>'}`);
  const migrated = [];
  let migratedBytes = 0;
  let migratedLines = 0;
  let replacedVersion = false;
  const pushMigratedLine = (line, lineLabel) => {
    const separatorBytes = migrated.length === 0 ? 0 : 1;
    const lineBytes = Buffer.byteLength(line, 'utf8');
    if (lineBytes > MAX_LOCK_BYTES - separatorBytes - migratedBytes) {
      fail(lineLabel, `migrated lock exceeds the ${MAX_LOCK_BYTES}-byte limit`);
    }
    migratedLines += 1;
    if (migratedLines > MAX_LOCK_LINES) {
      fail(lineLabel, `migrated lock exceeds the ${MAX_LOCK_LINES}-line limit`);
    }
    migratedBytes += separatorBytes + lineBytes;
    migrated.push(line);
  };
  for (const { lineNumber, rawLine } of scanBoundedLines(
    source,
    label,
    MAX_LOCK_BYTES,
    MAX_LOCK_LINES,
  )) {
    const line = rawLine.trim();
    const lineLabel = `${label}:${lineNumber}`;
    if (line === '' || line.startsWith('#')) continue;
    if (!replacedVersion && /^version\s*=/.test(line)) {
      pushMigratedLine(`version = ${LOCK_VERSION}`, lineLabel);
      replacedVersion = true;
      continue;
    }
    if (!/^deps\s*=/.test(line)) {
      pushMigratedLine(rawLine, lineLabel);
      continue;
    }
    if (
      line.length > MAX_LEGACY_DEPS_TEXT_BYTES
      || Buffer.byteLength(line, 'utf8') > MAX_LEGACY_DEPS_TEXT_BYTES
    ) {
      fail(lineLabel, `legacy deps exceeds the ${MAX_LEGACY_DEPS_TEXT_BYTES}-byte text limit`);
    }
    const match = line.match(/^deps\s*=\s*(\[.*\])$/);
    if (!match) fail(lineLabel, 'legacy deps must be one canonical string array');
    let modules;
    try {
      modules = JSON.parse(match[1]);
    } catch (error) {
      fail(lineLabel, `invalid legacy deps array: ${error.message}`);
    }
    if (!Array.isArray(modules) || modules.some((module) => typeof module !== 'string')) {
      fail(lineLabel, 'legacy deps must contain only module strings');
    }
    if (modules.length > MAX_EDGES) fail(lineLabel, 'too many legacy dependency edges');
    if (new Set(modules).size !== modules.length) fail(lineLabel, 'duplicate legacy dependency edge');
    for (const [moduleIndex, module] of modules.entries()) {
      validateModulePath(module, null, `${lineLabel}.deps[${moduleIndex}]`);
    }
    if (modules.length === 0) {
      pushMigratedLine('deps = []', lineLabel);
      continue;
    }
    pushMigratedLine('deps = [', lineLabel);
    for (const module of modules) {
      pushMigratedLine(
        `  { module = ${quote(module)}, constraint = "__legacy_requires_replacement__" },`,
        lineLabel,
      );
    }
    pushMigratedLine(']', lineLabel);
  }
  const lock = parseLockStructure(migrated.join('\n'), `${label} (v1 migration)`);
  Object.defineProperty(lock, 'migratedFromVersion', { value: 1, enumerable: false });
  return lock;
}

/**
 * Return exactly the lock data needed to materialize one Quickplay cache
 * module. Dependency edges are parsed and validated as part of the lock
 * protocol, then deliberately omitted from this cache-facing projection.
 */
export function selectLockedModuleSnapshotForCache(source, module, label = 'vo.lock') {
  const lock = parseVoLockV2(source, label);
  const selected = lock.resolved.find((candidate) => candidate.path === module);
  if (!selected) fail(label, `does not resolve ${module}`);
  return {
    module: selected.path,
    version: selected.version,
    vo: selected.vo,
    commit: selected.commit,
    release_manifest: selected.release_manifest,
    source: selected.source,
    artifacts: selected.artifacts.map((artifact) => ({ ...artifact })),
  };
}

function quote(value) {
  const source = String(value);
  let rendered = '"';
  for (let index = 0; index < source.length; index += 1) {
    const codepoint = source.codePointAt(index);
    if (codepoint >= 0xd800 && codepoint <= 0xdfff) {
      fail('canonical TOML string', 'value contains an isolated UTF-16 surrogate');
    }
    if (codepoint > 0xffff) index += 1;
    if (codepoint === 0x22) rendered += '\\"';
    else if (codepoint === 0x5c) rendered += '\\\\';
    else if (codepoint === 0x08) rendered += '\\b';
    else if (codepoint === 0x09) rendered += '\\t';
    else if (codepoint === 0x0a) rendered += '\\n';
    else if (codepoint === 0x0c) rendered += '\\f';
    else if (codepoint === 0x0d) rendered += '\\r';
    else if (codepoint <= 0x1f || (codepoint >= 0x7f && codepoint <= 0x9f)) {
      const width = codepoint <= 0xffff ? 4 : 8;
      const prefix = width === 4 ? '\\u' : '\\U';
      rendered += `${prefix}${codepoint.toString(16).toUpperCase().padStart(width, '0')}`;
    } else rendered += String.fromCodePoint(codepoint);
  }
  return `${rendered}"`;
}

export function renderLockedModuleV2(module) {
  const lines = [
    '[[resolved]]',
    `path = ${quote(module.path)}`,
    `version = ${quote(module.version)}`,
    `vo = ${quote(module.vo)}`,
    `commit = ${quote(module.commit)}`,
    `release_manifest = ${quote(module.release_manifest)}`,
    `source = ${quote(module.source)}`,
  ];
  const dependencies = [...(module.deps ?? [])].sort((left, right) => compareUtf8(left.module, right.module));
  if (dependencies.length === 0) {
    lines.push('deps = []');
  } else {
    lines.push('deps = [');
    for (const dependency of dependencies) {
      lines.push(`  { module = ${quote(dependency.module)}, constraint = ${quote(dependency.constraint)} },`);
    }
    lines.push(']');
  }
  const artifacts = [...(module.artifacts ?? [])].sort((left, right) =>
    compareUtf8(
      `${left.kind}\u0000${left.target}\u0000${left.name}`,
      `${right.kind}\u0000${right.target}\u0000${right.name}`,
    ));
  for (const artifact of artifacts) {
    lines.push(
      '',
      '[[resolved.artifact]]',
      `kind = ${quote(artifact.kind)}`,
      `target = ${quote(artifact.target)}`,
      `name = ${quote(artifact.name)}`,
      `size = ${artifact.size}`,
      `digest = ${quote(artifact.digest)}`,
    );
  }
  return lines.join('\n');
}

function renderVoLockV2Unchecked(lock) {
  const modules = [...lock.resolved].sort((left, right) => compareUtf8(left.path, right.path));
  const sections = [
    `version = ${LOCK_VERSION}`,
    `created_by = ${quote(lock.created_by)}`,
    '',
    '[root]',
    `module = ${quote(lock.root.module)}`,
    `vo = ${quote(lock.root.vo)}`,
  ];
  for (const module of modules) {
    sections.push('', renderLockedModuleV2(module));
  }
  return `${sections.join('\n')}\n`;
}

export function renderVoLockV2(lock) {
  const rendered = renderVoLockV2Unchecked(lock);
  parseVoLockV2(rendered, 'rendered vo.lock');
  return rendered;
}
