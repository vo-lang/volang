import { createHash, randomUUID } from 'node:crypto';
import {
  closeSync,
  constants as fsConstants,
  fstatSync,
  fsyncSync,
  lstatSync,
  mkdirSync,
  openSync,
  readSync,
  renameSync,
  rmSync,
  writeSync,
} from 'node:fs';
import path from 'node:path';
import { isDeepStrictEqual } from 'node:util';
import {
  QUICKPLAY_GENERATOR_SOURCE_INPUTS,
} from './quickplay_generator_contract.mjs';
import {
  VO_CLI_GUEST_ENVIRONMENT,
  verifyVoCliBuildInputs,
  verifyVoCliExecutionIdentity,
} from './quickplay_cli_producer_contract.mjs';
import { artifactSetSnapshot } from './source_bound_evidence.mjs';
import { compareUtf8 } from './utf8_order.mjs';

export const QUICKPLAY_REGENERATE_LIMITS = Object.freeze({
  maxJsonBytes: 16 * 1024 * 1024,
  maxJsonDepth: 256,
  maxJsonTokens: 2_000_000,
  maxJsonObjectKeys: 50_000,
  maxJsonCollectionEntries: 50_000,
  maxJsonTotalCollectionEntries: 200_000,
  maxObjectKeyBytes: 4 * 1024,
  maxJsonTotalObjectKeyBytes: 32 * 1024 * 1024,
  maxReportBytes: 16 * 1024 * 1024,
});

export const QUICKPLAY_REGENERATE_GATE_FILES = Object.freeze([...new Set([
  'scripts/ci/quickplay_regenerate_check.mjs',
  'scripts/ci/quickplay_regenerate_contract.mjs',
  'scripts/ci/quickplay_regenerate_contract_selftest.mjs',
  ...QUICKPLAY_GENERATOR_SOURCE_INPUTS,
])]);

const UTF8_DECODER = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true });

function sameStat(left, right) {
  return left.dev === right.dev
    && left.ino === right.ino
    && left.mode === right.mode
    && left.size === right.size
    && left.mtimeNs === right.mtimeNs
    && left.ctimeNs === right.ctimeNs;
}

export function ensureRealDirectory(directory, { create = false } = {}) {
  const absolute = path.resolve(directory);
  const parsed = path.parse(absolute);
  let current = parsed.root;
  const components = absolute.slice(parsed.root.length).split(path.sep).filter(Boolean);
  for (const component of components) {
    current = path.join(current, component);
    let metadata;
    try {
      metadata = lstatSync(current, { bigint: true });
    } catch (error) {
      if (error?.code !== 'ENOENT' || !create) throw error;
      mkdirSync(current, { mode: 0o755 });
      metadata = lstatSync(current, { bigint: true });
    }
    if (!metadata.isDirectory() || metadata.isSymbolicLink()) {
      throw new Error(`directory path must contain only real directories: ${current}`);
    }
  }
  return absolute;
}

export function snapshotQuickplayDirectory(directory) {
  const root = ensureRealDirectory(directory);
  return artifactSetSnapshot(root, ['.']);
}

export function compareQuickplaySnapshots(actual, expected) {
  const actualByPath = new Map(actual.map((entry) => [entry.path, entry]));
  const expectedByPath = new Map(expected.map((entry) => [entry.path, entry]));
  const actualPaths = [...actualByPath.keys()].sort(compareUtf8);
  const expectedPaths = [...expectedByPath.keys()].sort(compareUtf8);
  const missing = expectedPaths.filter((file) => !actualByPath.has(file));
  const extra = actualPaths.filter((file) => !expectedByPath.has(file));
  const changed = [];
  for (const file of expectedPaths) {
    if (!actualByPath.has(file)) continue;
    const actualEntry = actualByPath.get(file);
    const expectedEntry = expectedByPath.get(file);
    if (!isDeepStrictEqual(actualEntry, expectedEntry)) {
      changed.push({ path: file, actual: actualEntry, expected: expectedEntry });
    }
  }
  return { missing, extra, changed };
}

export function assertQuickplaySnapshotUnchanged(expected, found, label) {
  if (!isDeepStrictEqual(found, expected)) {
    throw new Error(`${label} changed while the regeneration gate was running`);
  }
}

function readStableRegularFile(file, label, maxBytes) {
  const before = lstatSync(file, { bigint: true });
  if (!before.isFile() || before.isSymbolicLink()) {
    throw new Error(`${label} must be a regular file without symbolic links`);
  }
  if (before.size > BigInt(maxBytes) || before.size > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error(`${label} exceeds the ${maxBytes}-byte limit`);
  }
  const size = Number(before.size);
  const noFollow = typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0;
  const descriptor = openSync(file, fsConstants.O_RDONLY | noFollow);
  try {
    const opened = fstatSync(descriptor, { bigint: true });
    if (!opened.isFile() || !sameStat(before, opened)) {
      throw new Error(`${label} changed before it was read`);
    }
    const bytes = Buffer.allocUnsafe(size);
    let offset = 0;
    while (offset < size) {
      const count = readSync(descriptor, bytes, offset, size - offset, offset);
      if (count === 0) throw new Error(`${label} was truncated while it was read`);
      offset += count;
    }
    const extra = Buffer.allocUnsafe(1);
    if (readSync(descriptor, extra, 0, 1, size) !== 0) {
      throw new Error(`${label} grew while it was read`);
    }
    const after = fstatSync(descriptor, { bigint: true });
    const pathAfter = lstatSync(file, { bigint: true });
    if (!sameStat(opened, after) || !sameStat(opened, pathAfter)) {
      throw new Error(`${label} changed while it was read`);
    }
    return bytes;
  } finally {
    closeSync(descriptor);
  }
}

function assertStrictJsonShape(source, label, limits) {
  const stack = [];
  let rootState = 'value';
  let index = 0;
  let tokens = 0;
  let totalCollectionEntries = 0;
  let totalObjectKeyBytes = 0;
  const chargeToken = () => {
    tokens += 1;
    if (tokens > limits.maxJsonTokens) {
      throw new Error(`${label} exceeds the ${limits.maxJsonTokens}-token JSON limit`);
    }
  };
  const chargeCollectionEntry = (context) => {
    context.entries += 1;
    totalCollectionEntries += 1;
    if (context.entries > limits.maxJsonCollectionEntries) {
      throw new Error(`${label} exceeds the ${limits.maxJsonCollectionEntries}-entry collection limit`);
    }
    if (totalCollectionEntries > limits.maxJsonTotalCollectionEntries) {
      throw new Error(`${label} exceeds the ${limits.maxJsonTotalCollectionEntries}-entry aggregate collection limit`);
    }
  };
  const skipWhitespace = () => {
    while (index < source.length && /[\u0009\u000A\u000D\u0020]/u.test(source[index])) index += 1;
  };
  const decodeScalarString = (token) => {
    let value;
    try {
      value = JSON.parse(token);
    } catch (error) {
      throw new Error(`${label} contains an invalid JSON string: ${error.message}`);
    }
    for (let offset = 0; offset < value.length; offset += 1) {
      const unit = value.charCodeAt(offset);
      if (unit >= 0xd800 && unit <= 0xdbff) {
        const next = value.charCodeAt(offset + 1);
        if (!(next >= 0xdc00 && next <= 0xdfff)) {
          throw new Error(`${label} contains an isolated Unicode surrogate in a JSON string`);
        }
        offset += 1;
      } else if (unit >= 0xdc00 && unit <= 0xdfff) {
        throw new Error(`${label} contains an isolated Unicode surrogate in a JSON string`);
      }
    }
    return value;
  };
  const scanString = () => {
    const start = index;
    index += 1;
    while (index < source.length) {
      const character = source[index];
      if (character === '"') {
        index += 1;
        return decodeScalarString(source.slice(start, index));
      }
      index += character === '\\' ? 2 : 1;
    }
    throw new Error(`${label} contains an unterminated JSON string`);
  };
  const scanScalar = () => {
    const start = index;
    while (index < source.length && !/[\u0009\u000A\u000D\u0020,\]}]/u.test(source[index])) index += 1;
    if (index === start) throw new Error(`${label} contains a missing JSON value`);
    const token = source.slice(start, index);
    let value;
    try {
      value = JSON.parse(token);
    } catch (error) {
      throw new Error(`${label} contains an invalid JSON scalar: ${error.message}`);
    }
    if (typeof value === 'number' && !Number.isFinite(value)) {
      throw new Error(`${label} contains a JSON number outside the finite f64 range`);
    }
  };
  const beginValue = () => {
    chargeToken();
    if (source[index] === '{' || source[index] === '[') {
      if (stack.length >= limits.maxJsonDepth) {
        throw new Error(`${label} exceeds the ${limits.maxJsonDepth}-level JSON depth limit`);
      }
      const kind = source[index] === '{' ? 'object' : 'array';
      index += 1;
      stack.push(kind === 'object'
        ? { entries: 0, kind, keys: new Set(), state: 'keyOrEnd' }
        : { entries: 0, kind, state: 'valueOrEnd' });
    } else if (source[index] === '"') {
      scanString();
    } else {
      scanScalar();
    }
  };

  while (true) {
    skipWhitespace();
    if (stack.length === 0) {
      if (rootState === 'done') {
        if (index !== source.length) throw new Error(`${label} has trailing JSON data`);
        return;
      }
      if (index === source.length) throw new Error(`${label} contains no JSON value`);
      rootState = 'done';
      beginValue();
      continue;
    }
    const context = stack.at(-1);
    if (context.kind === 'object') {
      if (context.state === 'keyOrEnd') {
        if (source[index] === '}') {
          chargeToken();
          index += 1;
          stack.pop();
          continue;
        }
        if (source[index] !== '"') throw new Error(`${label} contains a non-string object key`);
        chargeToken();
        const key = scanString();
        const keyBytes = Buffer.byteLength(key, 'utf8');
        if (keyBytes > limits.maxObjectKeyBytes) {
          throw new Error(`${label} contains an object key exceeding ${limits.maxObjectKeyBytes} bytes`);
        }
        if (totalObjectKeyBytes > limits.maxJsonTotalObjectKeyBytes - keyBytes) {
          throw new Error(`${label} exceeds the aggregate JSON object-key byte limit`);
        }
        totalObjectKeyBytes += keyBytes;
        if (context.keys.size >= limits.maxJsonObjectKeys) {
          throw new Error(`${label} exceeds the ${limits.maxJsonObjectKeys}-key object limit`);
        }
        if (context.keys.has(key)) {
          throw new Error(`${label} contains duplicate object key ${JSON.stringify(key)}`);
        }
        context.keys.add(key);
        chargeCollectionEntry(context);
        context.state = 'colon';
        continue;
      }
      if (context.state === 'colon') {
        if (source[index] !== ':') throw new Error(`${label} object key is missing a colon`);
        chargeToken();
        index += 1;
        context.state = 'value';
        continue;
      }
      if (context.state === 'value') {
        if (index === source.length) throw new Error(`${label} object is missing a value`);
        context.state = 'commaOrEnd';
        beginValue();
        continue;
      }
      if (source[index] === ',') {
        chargeToken();
        index += 1;
        context.state = 'keyOrEnd';
      } else if (source[index] === '}') {
        chargeToken();
        index += 1;
        stack.pop();
      } else {
        throw new Error(`${label} object is missing a comma or closing brace`);
      }
      continue;
    }
    if (context.state === 'valueOrEnd') {
      if (source[index] === ']') {
        chargeToken();
        index += 1;
        stack.pop();
      } else {
        if (index === source.length) throw new Error(`${label} array is missing a value`);
        chargeCollectionEntry(context);
        context.state = 'commaOrEnd';
        beginValue();
      }
      continue;
    }
    if (source[index] === ',') {
      chargeToken();
      index += 1;
      context.state = 'valueOrEnd';
    } else if (source[index] === ']') {
      chargeToken();
      index += 1;
      stack.pop();
    } else {
      throw new Error(`${label} array is missing a comma or closing bracket`);
    }
  }
}

export function parseStrictJsonBytes(bytes, label, overrides = {}) {
  const limits = { ...QUICKPLAY_REGENERATE_LIMITS, ...overrides };
  if (!Buffer.isBuffer(bytes) || bytes.byteLength > limits.maxJsonBytes) {
    throw new Error(`${label} exceeds the ${limits.maxJsonBytes}-byte JSON limit`);
  }
  let source;
  try {
    source = UTF8_DECODER.decode(bytes);
  } catch (error) {
    throw new Error(`${label} is not valid UTF-8: ${error.message}`);
  }
  assertStrictJsonShape(source, label, limits);
  try {
    return JSON.parse(source);
  } catch (error) {
    throw new Error(`${label} is invalid JSON: ${error.message}`);
  }
}

export function readStrictJsonFile(file, label) {
  return readStrictJsonFileRecord(file, label).value;
}

function readStrictJsonFileRecord(file, label) {
  const bytes = readStableRegularFile(file, label, QUICKPLAY_REGENERATE_LIMITS.maxJsonBytes);
  return {
    value: parseStrictJsonBytes(bytes, label),
    digest: `sha256:${createHash('sha256').update(bytes).digest('hex')}`,
    size: bytes.byteLength,
  };
}

function exactBuildPlatform(value) {
  return value
    && typeof value === 'object'
    && !Array.isArray(value)
    && Object.keys(value).sort().join('\0') === 'arch\0os'
    && typeof value.os === 'string'
    && value.os.length > 0
    && Buffer.byteLength(value.os, 'utf8') <= 128
    && typeof value.arch === 'string'
    && value.arch.length > 0
    && Buffer.byteLength(value.arch, 'utf8') <= 128;
}

function voplayProducer(provenance, label) {
  if (provenance?.schemaVersion !== 3 || !Array.isArray(provenance.producers)) {
    throw new Error(`${label} must be a schema-v3 Quickplay provenance document`);
  }
  const matches = provenance.producers.filter((entry) => entry?.id === 'voplay-current-source-wasm');
  if (matches.length !== 1 || !exactBuildPlatform(matches[0].buildPlatform)) {
    throw new Error(`${label} must contain exactly one voplay producer with an exact buildPlatform`);
  }
  return matches[0];
}

export function assertQuickplayVoCliProducerInputs(
  directory,
  expected,
  label = 'Quickplay provenance',
  expectedToolchain = null,
) {
  const provenance = readStrictJsonFile(
    path.join(ensureRealDirectory(directory), 'provenance.json'),
    `${label} provenance`,
  );
  if (provenance?.schemaVersion !== 3 || !Array.isArray(provenance.producers)) {
    throw new Error(`${label} must be a schema-v3 Quickplay provenance document`);
  }
  const matches = provenance.producers.filter((entry) => (
    entry?.id === 'blockkart-runtime-vpak'
    && entry?.output === 'assets/blockkart.vpak'
  ));
  if (matches.length !== 1) {
    throw new Error(`${label} must contain exactly one BlockKart VPAK producer`);
  }
  const issues = verifyVoCliBuildInputs(matches[0].voCliBuildInputs, { expected });
  if (issues.length > 0) {
    throw new Error(`${label} Vo CLI producer inputs are invalid: ${issues.join('; ')}`);
  }
  const executionIssues = verifyVoCliExecutionIdentity(
    matches[0].toolchain,
    matches[0].voBinary,
    {
      buildInputs: matches[0].voCliBuildInputs,
      executionDigest: matches[0].voCliExecutionDigest,
      expectedToolchain,
    },
  );
  if (executionIssues.length > 0) {
    throw new Error(`${label} Vo CLI execution identity is invalid: ${executionIssues.join('; ')}`);
  }
  return matches[0].voCliBuildInputs;
}

function blockKartVpakProducer(provenance, label) {
  const matches = provenance.producers.filter((entry) => (
    entry?.id === 'blockkart-runtime-vpak'
    && entry?.output === 'assets/blockkart.vpak'
  ));
  if (matches.length !== 1) throw new Error(`${label} must contain exactly one BlockKart VPAK producer`);
  const producer = matches[0];
  if (!isDeepStrictEqual(producer.command, VO_CLI_GUEST_ENVIRONMENT.command)) {
    throw new Error(`${label} contains a non-canonical BlockKart VPAK guest command`);
  }
  const inputIssues = verifyVoCliBuildInputs(producer.voCliBuildInputs);
  const executionIssues = verifyVoCliExecutionIdentity(producer.toolchain, producer.voBinary, {
    buildInputs: producer.voCliBuildInputs,
    executionDigest: producer.voCliExecutionDigest,
  });
  if (inputIssues.length > 0 || executionIssues.length > 0) {
    throw new Error(
      `${label} contains invalid BlockKart VPAK CLI lineage: `
      + [...inputIssues, ...executionIssues].join('; '),
    );
  }
  return producer;
}

export function acceptedCrossPlatformVoplayVariant(diff, actualDir, expectedDir) {
  if (
    diff.missing.length !== 0
    || diff.extra.length !== 0
    || diff.changed.length !== 1
    || diff.changed[0].path !== 'provenance.json'
  ) return null;
  const actualRecord = readStrictJsonFileRecord(
    path.join(actualDir, 'provenance.json'),
    'generated provenance',
  );
  const expectedRecord = readStrictJsonFileRecord(
    path.join(expectedDir, 'provenance.json'),
    'checked provenance',
  );
  const changed = diff.changed[0];
  if (
    actualRecord.digest !== changed.actual.digest
    || actualRecord.size !== changed.actual.size
    || expectedRecord.digest !== changed.expected.digest
    || expectedRecord.size !== changed.expected.size
  ) {
    throw new Error('provenance changed between the directory snapshot and semantic comparison');
  }
  const actual = actualRecord.value;
  const expected = expectedRecord.value;
  const actualProducer = voplayProducer(actual, 'generated provenance');
  const expectedProducer = voplayProducer(expected, 'checked provenance');
  const actualVpakProducer = blockKartVpakProducer(actual, 'generated provenance');
  const expectedVpakProducer = blockKartVpakProducer(expected, 'checked provenance');
  const voplayPlatformVariant = !isDeepStrictEqual(
    actualProducer.buildPlatform,
    expectedProducer.buildPlatform,
  );
  const cliHostVariant = actualVpakProducer.toolchain.host !== expectedVpakProducer.toolchain.host;
  const cliBinaryVariant = !isDeepStrictEqual(
    actualVpakProducer.voBinary,
    expectedVpakProducer.voBinary,
  );
  if (
    (!voplayPlatformVariant && !cliHostVariant)
    || (cliBinaryVariant && !cliHostVariant)
    || actualVpakProducer.toolchain.cargo !== expectedVpakProducer.toolchain.cargo
    || actualVpakProducer.toolchain.rustc !== expectedVpakProducer.toolchain.rustc
    || !isDeepStrictEqual(
      actualVpakProducer.voCliBuildInputs,
      expectedVpakProducer.voCliBuildInputs,
    )
  ) return null;
  const generatedPlatform = structuredClone(actualProducer.buildPlatform);
  const checkedPlatform = structuredClone(expectedProducer.buildPlatform);
  actualProducer.buildPlatform = { os: '<platform>', arch: '<platform>' };
  expectedProducer.buildPlatform = { os: '<platform>', arch: '<platform>' };
  if (cliHostVariant) {
    actualVpakProducer.toolchain.host = '<platform>';
    actualVpakProducer.toolchain.target = '<platform>';
    expectedVpakProducer.toolchain.host = '<platform>';
    expectedVpakProducer.toolchain.target = '<platform>';
  }
  if (cliHostVariant) {
    actualVpakProducer.voBinary = { path: '<platform>', digest: '<platform>', size: 0 };
    expectedVpakProducer.voBinary = { path: '<platform>', digest: '<platform>', size: 0 };
    actualVpakProducer.voCliExecutionDigest = '<platform>';
    expectedVpakProducer.voCliExecutionDigest = '<platform>';
  }
  if (!isDeepStrictEqual(actual, expected)) return null;
  return Object.freeze({
    generatedPlatform,
    checkedPlatform,
    voCliBinaryVariant: cliBinaryVariant,
    voCliHostVariant: cliHostVariant,
    changedFiles: ['provenance.json'],
  });
}

export function writeFileAtomically(directory, name, bytes) {
  const outputDir = ensureRealDirectory(directory, { create: true });
  if (
    typeof name !== 'string'
    || name.length === 0
    || path.basename(name) !== name
    || name === '.'
    || name === '..'
  ) throw new Error('atomic output name must be one plain file name');
  const payload = Buffer.isBuffer(bytes) ? bytes : Buffer.from(bytes);
  if (payload.byteLength > QUICKPLAY_REGENERATE_LIMITS.maxReportBytes) {
    throw new Error(`atomic output exceeds the ${QUICKPLAY_REGENERATE_LIMITS.maxReportBytes}-byte report limit`);
  }
  const temporary = path.join(outputDir, `.${name}.${process.pid}.${randomUUID()}.tmp`);
  const destination = path.join(outputDir, name);
  let descriptor;
  try {
    descriptor = openSync(
      temporary,
      fsConstants.O_WRONLY | fsConstants.O_CREAT | fsConstants.O_EXCL,
      0o644,
    );
    let offset = 0;
    while (offset < payload.byteLength) {
      const written = writeSync(descriptor, payload, offset, payload.byteLength - offset, offset);
      if (written <= 0) throw new Error(`atomic output write made no progress: ${destination}`);
      offset += written;
    }
    fsyncSync(descriptor);
    closeSync(descriptor);
    descriptor = undefined;
    renameSync(temporary, destination);
  } finally {
    if (descriptor !== undefined) closeSync(descriptor);
    rmSync(temporary, { force: true });
  }
  const published = lstatSync(destination, { bigint: true });
  if (!published.isFile() || published.isSymbolicLink() || published.size !== BigInt(payload.byteLength)) {
    throw new Error(`atomic output publication failed: ${destination}`);
  }
  return destination;
}
