import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import {
  closeSync,
  constants as fsConstants,
  fstatSync,
  lstatSync,
  openSync,
  opendirSync,
  readSync,
  realpathSync,
} from 'node:fs';
import path from 'node:path';
import { isDeepStrictEqual } from 'node:util';
import { validatePortableRelativePath } from './quickplay_artifact_paths.mjs';
import { portablePathCollisionKey } from './portable_path_key.mjs';
import { compareUtf8 } from './utf8_order.mjs';

export const VO_CLI_BUILD_COMMAND = Object.freeze([
  'cargo',
  'build',
  '-q',
  '--offline',
  '--locked',
  '-p',
  'vo',
  '--bin',
  'vo',
  '--profile',
  'release-native',
  '--target',
  '<HOST>',
]);

export const VO_CLI_METADATA_COMMAND = Object.freeze([
  'cargo',
  'metadata',
  '--offline',
  '--locked',
  '--format-version',
  '1',
  '--manifest-path',
  'cmd/vo/Cargo.toml',
]);

const VO_CLI_BUILD_INHERITED_ENVIRONMENT = Object.freeze([
  'ComSpec',
  'HOME',
  'PATH',
  'PATHEXT',
  'SystemDrive',
  'SystemRoot',
  'TEMP',
  'TMP',
  'TMPDIR',
  'USERPROFILE',
  'WINDIR',
]);

const VO_CLI_GUEST_INHERITED_ENVIRONMENT = Object.freeze([
  'ComSpec',
  'PATHEXT',
  'SystemDrive',
  'SystemRoot',
  'WINDIR',
]);

export const VO_CLI_BUILD_ENVIRONMENT = Object.freeze({
  inherited: VO_CLI_BUILD_INHERITED_ENVIRONMENT,
  fixed: Object.freeze({
    CARGO_INCREMENTAL: '0',
    CARGO_TERM_COLOR: 'never',
    LANG: 'C',
    LC_ALL: 'C',
    SOURCE_DATE_EPOCH: '1',
    TZ: 'UTC',
    VOWORK: 'off',
    ZERO_AR_DATE: '1',
  }),
  paths: Object.freeze({
    CARGO_HOME: 'task-owned:target/.blockkart-vpak-cargo-home-*',
    CARGO_TARGET_DIR: 'task-owned:target/.blockkart-vpak-cargo-*',
  }),
  metadataPaths: Object.freeze({
    CARGO_HOME: '<HOME>/.cargo',
    CARGO_TARGET_DIR: 'target',
  }),
  rustFlags: Object.freeze([
    '--remap-path-prefix=<VOLANG_ROOT>=/workspace/volang',
    '--remap-path-prefix=<CARGO_HOME>=/workspace/cargo-home',
    '--remap-path-prefix=<CARGO_TARGET_DIR>=/workspace/cargo-target',
  ]),
  cargoConfigPolicy: Object.freeze({
    allowed: Object.freeze(['.cargo/config.toml']),
    rejected: Object.freeze([
      '.cargo/config',
      '<ANCESTOR>/.cargo/config',
      '<ANCESTOR>/.cargo/config.toml',
      '<CARGO_HOME>/config',
      '<CARGO_HOME>/config.toml',
    ]),
  }),
});

export const VO_CLI_GUEST_ENVIRONMENT = Object.freeze({
  command: Object.freeze(['vo', 'run', 'tools/pack_primitive_assets.vo']),
  cwd: 'external:BlockKart',
  inherited: VO_CLI_GUEST_INHERITED_ENVIRONMENT,
  fixed: Object.freeze({
    LANG: 'C',
    LC_ALL: 'C',
    TZ: 'UTC',
  }),
  paths: Object.freeze({
    HOME: 'target/blockkart-vpak-build/guest/home',
    TEMP: 'target/blockkart-vpak-build/guest/tmp',
    TMP: 'target/blockkart-vpak-build/guest/tmp',
    TMPDIR: 'target/blockkart-vpak-build/guest/tmp',
    USERPROFILE: 'target/blockkart-vpak-build/guest/home',
    VO_BIN: 'target/blockkart-vpak-build/vo[.exe]',
    VO_MOD_CACHE: 'target/blockkart-vpak-build/guest/mod-cache',
    XDG_CACHE_HOME: 'target/blockkart-vpak-build/guest/home/.cache',
  }),
  unset: Object.freeze(['VOWORK']),
  workspaceDiscovery: Object.freeze({
    environment: 'unset',
    mode: 'nearest-ancestor',
    selected: 'external:BlockKart/vo.work',
    start: 'project-root',
  }),
});

// These task/artifact paths deliberately over-approximate the per-file record
// below. The record is derived from locked Cargo metadata and the build-script
// identity roots; the globs make the engineering task graph invalidate before
// that derivation runs.
export const VO_CLI_PRODUCER_TASK_INPUTS = Object.freeze([
  '.gitattributes',
  '.cargo/config.toml',
  'Cargo.toml',
  'Cargo.lock',
  'rust-toolchain.toml',
  'cmd/vo/**',
  'lang/crates/**',
  'lang/stdlib/**',
]);

const VO_CLI_ROOT_FILES = Object.freeze([
  '.gitattributes',
  '.cargo/config.toml',
  'Cargo.lock',
  'Cargo.toml',
  'rust-toolchain.toml',
]);
const VO_CLI_IDENTITY_ROOTS = Object.freeze([
  'cmd/vo',
  'lang/crates',
  'lang/stdlib',
]);
const MAX_METADATA_BYTES = 64 * 1024 * 1024;
const MAX_METADATA_MILLIS = 120_000;
const MAX_TOOL_VERSION_BYTES = 1024 * 1024;
const MAX_LOCAL_PACKAGES = 1024;
const MAX_TARGETS_PER_PACKAGE = 128;
const MAX_KINDS_PER_TARGET = 16;
const MAX_INPUTS = 10_000;
const MAX_WALK_ENTRIES = 100_000;
const MAX_TOTAL_PATH_BYTES = 8 * 1024 * 1024;
const MAX_INPUT_BYTES = 64 * 1024 * 1024;
const MAX_VO_BINARY_BYTES = 256 * 1024 * 1024;
const MAX_TOTAL_INPUT_BYTES = 512 * 1024 * 1024;
const MAX_DEPTH = 256;
const MAX_PATH_BYTES = 4 * 1024;
const UTF8_DECODER = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true });

function sha256(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function canonicalJson(value) {
  if (value === null) return 'null';
  if (typeof value === 'string' || typeof value === 'boolean') return JSON.stringify(value);
  if (typeof value === 'number' && Number.isFinite(value)) return JSON.stringify(value);
  if (Array.isArray(value)) return `[${value.map(canonicalJson).join(',')}]`;
  if (typeof value === 'object') {
    return `{${Object.keys(value)
      .sort(compareUtf8)
      .map((key) => `${JSON.stringify(key)}:${canonicalJson(value[key])}`)
      .join(',')}}`;
  }
  throw new TypeError(`Vo CLI canonical JSON contains unsupported ${typeof value} value`);
}

function exactKeys(value, expected) {
  return value
    && typeof value === 'object'
    && !Array.isArray(value)
    && JSON.stringify(Object.keys(value).sort(compareUtf8))
      === JSON.stringify([...expected].sort(compareUtf8));
}

function validDigest(value) {
  return typeof value === 'string' && /^sha256:[0-9a-f]{64}$/.test(value);
}

function pathWithin(root, candidate) {
  const relative = path.relative(root, candidate);
  return relative === ''
    || (!relative.startsWith(`..${path.sep}`) && relative !== '..' && !path.isAbsolute(relative));
}

function portableRelative(root, candidate, label) {
  if (!pathWithin(root, candidate)) throw new Error(`${label} escapes the Volang root`);
  const relative = path.relative(root, candidate).split(path.sep).join('/');
  validatePortableRelativePath(relative, label);
  return relative;
}

function isPortableRelativePath(value) {
  if (typeof value !== 'string') return false;
  try {
    validatePortableRelativePath(value, 'Vo CLI contract path');
    return true;
  } catch {
    return false;
  }
}

function assertNoPortablePathCollisions(paths, label) {
  const spellings = new Map();
  for (const relative of paths) {
    validatePortableRelativePath(relative, `${label} ${relative}`);
    const key = portablePathCollisionKey(relative);
    const existing = spellings.get(key);
    if (existing !== undefined && existing !== relative) {
      throw new Error(`${label} contains a portable path collision: ${existing} and ${relative}`);
    }
    spellings.set(key, relative);
  }
}

function canonicalRoot(root) {
  const canonical = realpathSync.native(path.resolve(root));
  const metadata = lstatSync(canonical, { bigint: true });
  if (!metadata.isDirectory() || metadata.isSymbolicLink()) {
    throw new Error(`Volang root must be a real directory: ${canonical}`);
  }
  return canonical;
}

function sameNativePath(left, right) {
  const normalizedLeft = path.normalize(path.resolve(left));
  const normalizedRight = path.normalize(path.resolve(right));
  return process.platform === 'win32'
    ? normalizedLeft.toUpperCase() === normalizedRight.toUpperCase()
    : normalizedLeft === normalizedRight;
}

function sameStat(left, right) {
  return left.dev === right.dev
    && left.ino === right.ino
    && left.mode === right.mode
    && left.size === right.size
    && left.mtimeNs === right.mtimeNs
    && left.ctimeNs === right.ctimeNs;
}

function readStableRegularFile(file, label) {
  if (!sameNativePath(realpathSync.native(file), file)) {
    throw new Error(`${label} must not traverse symbolic-link path components`);
  }
  const before = lstatSync(file, { bigint: true });
  if (!before.isFile() || before.isSymbolicLink()) {
    throw new Error(`${label} must be a regular file without symbolic links`);
  }
  if (before.size > BigInt(MAX_INPUT_BYTES) || before.size > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error(`${label} exceeds the ${MAX_INPUT_BYTES}-byte input limit`);
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
    return { bytes, size };
  } finally {
    closeSync(descriptor);
  }
}

function identityInputFile(file) {
  const name = path.basename(file);
  if (['Cargo.lock', 'Cargo.toml', 'build.rs', 'stdlib.toml'].includes(name)) return true;
  return ['.rs', '.toml', '.vo'].includes(path.extname(name));
}

function addInputPath(paths, relative) {
  if (paths.has(relative)) return;
  if (paths.size >= MAX_INPUTS) {
    throw new Error(`Vo CLI source closure exceeds the ${MAX_INPUTS}-input limit`);
  }
  paths.add(relative);
}

function observeWalkPath(relative, walkState) {
  validatePortableRelativePath(relative, 'Vo CLI source closure path');
  const pathBytes = Buffer.byteLength(relative, 'utf8');
  if (pathBytes > MAX_PATH_BYTES) {
    throw new Error(`Vo CLI source closure path exceeds the ${MAX_PATH_BYTES}-byte limit: ${relative}`);
  }
  walkState.pathBytes += pathBytes;
  if (
    !Number.isSafeInteger(walkState.pathBytes)
    || walkState.pathBytes > MAX_TOTAL_PATH_BYTES
  ) {
    throw new Error(`Vo CLI source closure exceeds the ${MAX_TOTAL_PATH_BYTES}-byte aggregate path limit`);
  }
}

// Only vo-web's declared generated output roots are excluded. A source
// directory named target, pkg, or node_modules anywhere else remains part of
// the closure, including at another Cargo package root. New generated roots
// must be reviewed and added by their exact repository-relative path.
export function isVoCliGeneratedIdentityDirectory(relative) {
  return relative === 'lang/crates/vo-web/node_modules'
    || relative === 'lang/crates/vo-web/pkg'
    || relative === 'lang/crates/vo-web/pkg-island'
    || relative === 'lang/crates/vo-web/target';
}

function collectIdentityPaths(root, relativeRoot, paths, walkState, depth = 0) {
  if (depth > MAX_DEPTH) {
    throw new Error(`Vo CLI source closure exceeds the ${MAX_DEPTH}-level depth limit`);
  }
  observeWalkPath(relativeRoot, walkState);
  const absolute = path.join(root, ...relativeRoot.split('/'));
  const before = lstatSync(absolute, { bigint: true });
  if (before.isSymbolicLink()) {
    throw new Error(`Vo CLI source closure contains a symbolic link: ${relativeRoot}`);
  }
  if (before.isFile()) {
    if (identityInputFile(absolute)) addInputPath(paths, relativeRoot);
    return;
  }
  if (!before.isDirectory()) {
    throw new Error(`Vo CLI source closure contains a special filesystem entry: ${relativeRoot}`);
  }
  const directory = opendirSync(absolute);
  try {
    while (true) {
      const entry = directory.readSync();
      if (entry === null) break;
      walkState.entries += 1;
      if (!Number.isSafeInteger(walkState.entries) || walkState.entries > MAX_WALK_ENTRIES) {
        throw new Error(`Vo CLI source closure exceeds the ${MAX_WALK_ENTRIES}-entry traversal limit`);
      }
      const child = `${relativeRoot}/${entry.name}`;
      if (entry.isDirectory() && isVoCliGeneratedIdentityDirectory(child)) {
        observeWalkPath(child, walkState);
        continue;
      }
      collectIdentityPaths(root, child, paths, walkState, depth + 1);
    }
  } finally {
    directory.closeSync();
  }
  const after = lstatSync(absolute, { bigint: true });
  if (!sameStat(before, after)) {
    throw new Error(`Vo CLI source directory changed while it was enumerated: ${relativeRoot}`);
  }
}

function cargoBuildDependencyIds(node) {
  if (!Array.isArray(node?.deps)) {
    throw new Error(`cargo metadata resolve node has invalid dependency records: ${node?.id ?? '<unknown>'}`);
  }
  const dependencies = [];
  for (const dependency of node.deps) {
    if (
      !dependency
      || typeof dependency.pkg !== 'string'
      || !Array.isArray(dependency.dep_kinds)
      || dependency.dep_kinds.length === 0
      || dependency.dep_kinds.some((entry) => (
        !entry
        || ![null, 'build', 'dev'].includes(entry.kind)
        || (entry.target !== null && typeof entry.target !== 'string')
      ))
    ) {
      throw new Error(`cargo metadata resolve node has invalid dependency kinds: ${node?.id ?? '<unknown>'}`);
    }
    if (dependency.dep_kinds.some((entry) => entry.kind !== 'dev')) {
      dependencies.push(dependency.pkg);
    }
  }
  return dependencies;
}

function reachablePackageIds(metadata) {
  if (!metadata?.resolve || !Array.isArray(metadata.resolve.nodes)) {
    throw new Error('cargo metadata did not contain a resolved package graph');
  }
  const rootId = metadata.resolve.root;
  if (typeof rootId !== 'string' || rootId.length === 0) {
    throw new Error('cargo metadata did not identify the cmd/vo root package');
  }
  const nodes = new Map();
  for (const node of metadata.resolve.nodes) {
    if (!node || typeof node.id !== 'string' || nodes.has(node.id)) {
      throw new Error('cargo metadata contains an invalid or duplicate resolve node');
    }
    nodes.set(node.id, node);
  }
  const reachable = new Set();
  const pending = [rootId];
  while (pending.length > 0) {
    const id = pending.pop();
    if (reachable.has(id)) continue;
    const node = nodes.get(id);
    if (!node) throw new Error(`cargo metadata resolve edge references a missing package: ${id}`);
    reachable.add(id);
    pending.push(...cargoBuildDependencyIds(node));
  }
  return { reachable, rootId };
}

function productionTargets(cargoPackage, isRoot, root) {
  if (!Array.isArray(cargoPackage.targets)) {
    throw new Error(`cargo metadata package has no target list: ${cargoPackage.name}`);
  }
  const targets = [];
  for (const target of cargoPackage.targets) {
    if (
      !target
      || typeof target.name !== 'string'
      || !Array.isArray(target.kind)
      || target.kind.some((kind) => typeof kind !== 'string')
      || typeof target.src_path !== 'string'
    ) {
      throw new Error(`cargo metadata package has an invalid target: ${cargoPackage.name}`);
    }
    const kinds = [...target.kind].sort(compareUtf8);
    const buildTarget = kinds.some((kind) => (
      kind === 'custom-build'
      || kind === 'lib'
      || kind === 'rlib'
      || kind === 'dylib'
      || kind === 'cdylib'
      || kind === 'staticlib'
      || kind === 'proc-macro'
      || (isRoot && kind === 'bin' && target.name === 'vo')
    ));
    if (!buildTarget) continue;
    const source = realpathSync.native(path.resolve(target.src_path));
    targets.push({
      kind: kinds,
      name: target.name,
      source: portableRelative(root, source, `Cargo target ${cargoPackage.name}/${target.name}`),
    });
  }
  targets.sort((left, right) => compareUtf8(left.source, right.source)
    || compareUtf8(left.name, right.name));
  if (targets.length === 0) {
    throw new Error(`reachable local package has no production target: ${cargoPackage.name}`);
  }
  return targets;
}

function deriveLocalPackages(metadata, root) {
  if (!Array.isArray(metadata?.packages)) throw new Error('cargo metadata did not contain packages');
  const { reachable, rootId } = reachablePackageIds(metadata);
  const byId = new Map();
  for (const cargoPackage of metadata.packages) {
    if (!cargoPackage || typeof cargoPackage.id !== 'string' || byId.has(cargoPackage.id)) {
      throw new Error('cargo metadata contains an invalid or duplicate package');
    }
    byId.set(cargoPackage.id, cargoPackage);
  }
  const rootPackage = byId.get(rootId);
  const expectedRootManifest = realpathSync.native(path.join(root, 'cmd/vo/Cargo.toml'));
  if (
    rootPackage?.name !== 'vo'
    || !sameNativePath(
      realpathSync.native(path.resolve(rootPackage?.manifest_path ?? '')),
      expectedRootManifest,
    )
  ) {
    throw new Error('locked cargo metadata root must be cmd/vo/Cargo.toml');
  }
  const packages = [];
  for (const id of [...reachable].sort(compareUtf8)) {
    const cargoPackage = byId.get(id);
    if (!cargoPackage) throw new Error(`cargo metadata is missing reachable package ${id}`);
    if (cargoPackage.source !== null) continue;
    if (
      typeof cargoPackage.name !== 'string'
      || typeof cargoPackage.version !== 'string'
      || typeof cargoPackage.manifest_path !== 'string'
    ) {
      throw new Error(`reachable local package has incomplete identity: ${id}`);
    }
    const manifest = realpathSync.native(path.resolve(cargoPackage.manifest_path));
    const manifestRelative = portableRelative(root, manifest, `Cargo manifest ${cargoPackage.name}`);
    const packagePath = path.posix.dirname(manifestRelative);
    const allowed = packagePath === 'cmd/vo'
      || packagePath === 'lang/stdlib'
      || /^lang\/crates\/[^/]+$/.test(packagePath);
    if (!allowed) {
      throw new Error(`reachable local package is outside the declared Vo CLI source roots: ${packagePath}`);
    }
    packages.push({
      manifest: manifestRelative,
      name: cargoPackage.name,
      path: packagePath,
      targets: productionTargets(cargoPackage, id === rootId, root),
      version: cargoPackage.version,
    });
  }
  packages.sort((left, right) => compareUtf8(left.path, right.path)
    || compareUtf8(left.name, right.name));
  if (packages.length === 0 || packages[0]?.path !== 'cmd/vo') {
    throw new Error('Vo CLI source closure is missing the cmd/vo package');
  }
  return packages;
}

function lockedMetadata(root, environment) {
  const args = VO_CLI_METADATA_COMMAND.slice(1);
  const manifestIndex = args.indexOf('cmd/vo/Cargo.toml');
  args[manifestIndex] = path.join(root, 'cmd/vo/Cargo.toml');
  const result = spawnSync(VO_CLI_METADATA_COMMAND[0], args, {
    cwd: root,
    encoding: 'utf8',
    env: environment,
    maxBuffer: MAX_METADATA_BYTES,
    timeout: MAX_METADATA_MILLIS,
  });
  if (result.error || result.status !== 0) {
    const detail = String(result.stderr || result.error?.message || `status ${result.status}`).trim();
    throw new Error(`locked offline cargo metadata for cmd/vo failed: ${detail}`);
  }
  if (Buffer.byteLength(result.stdout, 'utf8') > MAX_METADATA_BYTES) {
    throw new Error(`cargo metadata exceeds the ${MAX_METADATA_BYTES}-byte output limit`);
  }
  try {
    return JSON.parse(result.stdout);
  } catch (error) {
    throw new Error(`cargo metadata returned invalid JSON: ${error.message}`);
  }
}

function canonicalContractDigest(value) {
  const authenticated = {
    schemaVersion: value.schemaVersion,
    kind: value.kind,
    buildCommand: value.buildCommand,
    buildEnvironment: value.buildEnvironment,
    guestEnvironment: value.guestEnvironment,
    metadataCommand: value.metadataCommand,
    rootFiles: value.rootFiles,
    identityRoots: value.identityRoots,
    packages: value.packages,
    inputs: value.inputs,
  };
  return sha256(Buffer.from(canonicalJson(authenticated), 'utf8'));
}

export function currentVoCliBuildInputs(volangRoot, cargoHomeDirectory = null) {
  const root = canonicalRoot(volangRoot);
  const metadata = lockedMetadata(
    root,
    voCliBuildEnvironment(process.env, root, null, cargoHomeDirectory),
  );
  const packages = deriveLocalPackages(metadata, root);
  const inputPaths = new Set();
  for (const rootFile of VO_CLI_ROOT_FILES) addInputPath(inputPaths, rootFile);
  const walkState = { entries: 0, pathBytes: 0 };
  for (const identityRoot of VO_CLI_IDENTITY_ROOTS) {
    collectIdentityPaths(root, identityRoot, inputPaths, walkState);
  }
  for (const cargoPackage of packages) {
    addInputPath(inputPaths, cargoPackage.manifest);
    for (const target of cargoPackage.targets) addInputPath(inputPaths, target.source);
  }
  const sortedPaths = [...inputPaths].sort(compareUtf8);
  assertNoPortablePathCollisions(sortedPaths, 'Vo CLI source closure');
  if (sortedPaths.length > MAX_INPUTS) {
    throw new Error(`Vo CLI source closure exceeds the ${MAX_INPUTS}-input limit`);
  }
  const inputs = [];
  let totalBytes = 0;
  for (const relative of sortedPaths) {
    const absolute = path.join(root, ...relative.split('/'));
    const { bytes, size } = readStableRegularFile(absolute, `Vo CLI input ${relative}`);
    totalBytes += size;
    if (!Number.isSafeInteger(totalBytes) || totalBytes > MAX_TOTAL_INPUT_BYTES) {
      throw new Error(`Vo CLI source closure exceeds the ${MAX_TOTAL_INPUT_BYTES}-byte aggregate limit`);
    }
    inputs.push({ digest: sha256(bytes), path: relative, size });
  }
  const contract = {
    schemaVersion: 4,
    kind: 'volang.voCliBuildInputs',
    buildCommand: [...VO_CLI_BUILD_COMMAND],
    buildEnvironment: { ...VO_CLI_BUILD_ENVIRONMENT },
    guestEnvironment: { ...VO_CLI_GUEST_ENVIRONMENT },
    metadataCommand: [...VO_CLI_METADATA_COMMAND],
    rootFiles: [...VO_CLI_ROOT_FILES],
    identityRoots: [...VO_CLI_IDENTITY_ROOTS],
    packages,
    inputs,
  };
  return { ...contract, digest: canonicalContractDigest(contract) };
}

function validateCanonicalTarget(target, label, issues) {
  if (!exactKeys(target, ['kind', 'name', 'source'])) {
    issues.push(`${label} must contain exact target fields`);
    return;
  }
  const kindsCanonical = Array.isArray(target.kind)
    && target.kind.length > 0
    && target.kind.length <= MAX_KINDS_PER_TARGET
    && Array.from({ length: target.kind.length }, (_, index) => target.kind[index])
      .every((kind) => typeof kind === 'string' && kind.length > 0);
  if (
    !kindsCanonical
    || JSON.stringify(target.kind) !== JSON.stringify([...new Set(target.kind)].sort(compareUtf8))
    || typeof target.name !== 'string'
    || target.name.length === 0
    || !isPortableRelativePath(target.source)
    || !target.source.endsWith('.rs')
  ) {
    issues.push(`${label} is invalid`);
  }
}

export function verifyVoCliBuildInputs(value, { expected = null } = {}) {
  const issues = [];
  if (!exactKeys(value, [
    'schemaVersion',
    'kind',
    'buildCommand',
    'buildEnvironment',
    'guestEnvironment',
    'metadataCommand',
    'rootFiles',
    'identityRoots',
    'packages',
    'inputs',
    'digest',
  ])) {
    return ['voCliBuildInputs must contain the exact contract fields'];
  }
  if (value.schemaVersion !== 4) issues.push('voCliBuildInputs schemaVersion must be 4');
  if (value.kind !== 'volang.voCliBuildInputs') issues.push('voCliBuildInputs kind is invalid');
  if (!isDeepStrictEqual(value.buildCommand, VO_CLI_BUILD_COMMAND)) {
    issues.push('voCliBuildInputs buildCommand is not canonical');
  }
  if (!isDeepStrictEqual(value.buildEnvironment, VO_CLI_BUILD_ENVIRONMENT)) {
    issues.push('voCliBuildInputs buildEnvironment is not canonical');
  }
  if (!isDeepStrictEqual(value.guestEnvironment, VO_CLI_GUEST_ENVIRONMENT)) {
    issues.push('voCliBuildInputs guestEnvironment is not canonical');
  }
  if (!isDeepStrictEqual(value.metadataCommand, VO_CLI_METADATA_COMMAND)) {
    issues.push('voCliBuildInputs metadataCommand is not canonical');
  }
  if (!isDeepStrictEqual(value.rootFiles, VO_CLI_ROOT_FILES)) {
    issues.push('voCliBuildInputs rootFiles are not canonical');
  }
  if (!isDeepStrictEqual(value.identityRoots, VO_CLI_IDENTITY_ROOTS)) {
    issues.push('voCliBuildInputs identityRoots are not canonical');
  }
  if (
    !Array.isArray(value.packages)
    || value.packages.length === 0
    || value.packages.length > MAX_LOCAL_PACKAGES
  ) {
    issues.push('voCliBuildInputs packages must be a non-empty bounded array');
  } else {
    const packagePaths = new Set();
    let packagesCanonical = true;
    for (const [index, cargoPackage] of value.packages.entries()) {
      const label = `voCliBuildInputs packages[${index}]`;
      if (!exactKeys(cargoPackage, ['manifest', 'name', 'path', 'targets', 'version'])) {
        issues.push(`${label} must contain exact package fields`);
        packagesCanonical = false;
        continue;
      }
      const allowedPath = typeof cargoPackage.path === 'string' && (
        cargoPackage.path === 'cmd/vo'
        || cargoPackage.path === 'lang/stdlib'
        || /^lang\/crates\/[^/]+$/.test(cargoPackage.path)
      );
      if (
        !allowedPath
        || !isPortableRelativePath(cargoPackage.path)
        || !isPortableRelativePath(cargoPackage.manifest)
        || cargoPackage.manifest !== `${cargoPackage.path}/Cargo.toml`
        || typeof cargoPackage.name !== 'string'
        || cargoPackage.name.length === 0
        || typeof cargoPackage.version !== 'string'
        || cargoPackage.version.length === 0
        || packagePaths.has(cargoPackage.path)
      ) {
        issues.push(`${label} has invalid identity`);
        packagesCanonical = false;
      }
      packagePaths.add(cargoPackage.path);
      if (
        !Array.isArray(cargoPackage.targets)
        || cargoPackage.targets.length === 0
        || cargoPackage.targets.length > MAX_TARGETS_PER_PACKAGE
      ) {
        issues.push(`${label} targets must be a non-empty bounded array`);
        packagesCanonical = false;
      } else {
        let targetsCanonical = true;
        for (const [targetIndex, target] of cargoPackage.targets.entries()) {
          const issueCount = issues.length;
          validateCanonicalTarget(target, `${label} targets[${targetIndex}]`, issues);
          if (issues.length !== issueCount) targetsCanonical = false;
        }
        if (targetsCanonical) {
          const sortedTargets = [...cargoPackage.targets].sort((left, right) => (
            compareUtf8(left.source, right.source) || compareUtf8(left.name, right.name)
          ));
          if (!isDeepStrictEqual(cargoPackage.targets, sortedTargets)) {
            issues.push(`${label} targets are not canonically ordered`);
            packagesCanonical = false;
          }
        } else {
          packagesCanonical = false;
        }
      }
    }
    if (packagesCanonical) {
      const sorted = [...value.packages].sort((left, right) => compareUtf8(left.path, right.path)
        || compareUtf8(left.name, right.name));
      if (!isDeepStrictEqual(value.packages, sorted)) {
        issues.push('voCliBuildInputs packages are not canonically ordered');
      }
    }
    if (!packagePaths.has('cmd/vo')) issues.push('voCliBuildInputs packages omit cmd/vo');
  }
  if (!Array.isArray(value.inputs) || value.inputs.length === 0 || value.inputs.length > MAX_INPUTS) {
    issues.push('voCliBuildInputs inputs must be a non-empty bounded array');
  } else {
    const paths = new Set();
    const portablePaths = new Map();
    let totalBytes = 0;
    let totalPathBytes = 0;
    let inputsCanonical = true;
    for (const [index, input] of value.inputs.entries()) {
      if (
        !exactKeys(input, ['digest', 'path', 'size'])
        || !validDigest(input.digest)
        || !isPortableRelativePath(input.path)
        || !Number.isSafeInteger(input.size)
        || input.size < 0
        || input.size > MAX_INPUT_BYTES
        || paths.has(input.path)
      ) {
        issues.push(`voCliBuildInputs inputs[${index}] is invalid`);
        inputsCanonical = false;
        continue;
      }
      const portableKey = portablePathCollisionKey(input.path);
      const collidingPath = portablePaths.get(portableKey);
      if (collidingPath !== undefined && collidingPath !== input.path) {
        issues.push(
          `voCliBuildInputs inputs[${index}] has a portable path collision with ${collidingPath}`,
        );
        inputsCanonical = false;
      }
      portablePaths.set(portableKey, input.path);
      paths.add(input.path);
      totalBytes += input.size;
      totalPathBytes += Buffer.byteLength(input.path, 'utf8');
    }
    if (!Number.isSafeInteger(totalBytes) || totalBytes > MAX_TOTAL_INPUT_BYTES) {
      issues.push('voCliBuildInputs inputs exceed the aggregate byte limit');
    }
    if (!Number.isSafeInteger(totalPathBytes) || totalPathBytes > MAX_TOTAL_PATH_BYTES) {
      issues.push('voCliBuildInputs input paths exceed the aggregate byte limit');
    }
    if (inputsCanonical) {
      const sorted = [...value.inputs].sort((left, right) => compareUtf8(left.path, right.path));
      if (!isDeepStrictEqual(value.inputs, sorted)) {
        issues.push('voCliBuildInputs inputs are not canonically ordered');
      }
    }
    for (const required of VO_CLI_ROOT_FILES) {
      if (!paths.has(required)) issues.push(`voCliBuildInputs inputs omit ${required}`);
    }
    const cargoPackages = Array.isArray(value.packages) ? value.packages : [];
    for (const cargoPackage of cargoPackages) {
      if (typeof cargoPackage?.manifest === 'string' && !paths.has(cargoPackage.manifest)) {
        issues.push(`voCliBuildInputs inputs omit ${cargoPackage.manifest}`);
      }
      const cargoTargets = Array.isArray(cargoPackage?.targets) ? cargoPackage.targets : [];
      for (const target of cargoTargets) {
        if (typeof target?.source === 'string' && !paths.has(target.source)) {
          issues.push(`voCliBuildInputs inputs omit ${target.source}`);
        }
      }
    }
  }
  if (!validDigest(value.digest)) {
    issues.push('voCliBuildInputs digest must be sha256');
  } else if (issues.length === 0 && value.digest !== canonicalContractDigest(value)) {
    issues.push('voCliBuildInputs digest does not match its canonical records');
  }
  if (expected !== null && issues.length === 0 && !isDeepStrictEqual(value, expected)) {
    issues.push('voCliBuildInputs do not match the current locked local source closure');
  }
  return issues;
}

export function assertVoCliBuildInputs(value, { expected = null, label = 'voCliBuildInputs' } = {}) {
  const issues = verifyVoCliBuildInputs(value, { expected });
  if (issues.length > 0) throw new Error(`${label} is invalid: ${issues.join('; ')}`);
  return value;
}

function inheritEnvironment(baseEnvironment, allowedKeys) {
  if (!baseEnvironment || typeof baseEnvironment !== 'object' || Array.isArray(baseEnvironment)) {
    throw new Error('base process environment must be an object');
  }
  const environment = {};
  for (const canonicalKey of allowedKeys) {
    const matchingKeys = Object.keys(baseEnvironment).filter((key) => (
      process.platform === 'win32'
        ? key.toUpperCase() === canonicalKey.toUpperCase()
        : key === canonicalKey
    ));
    if (matchingKeys.length > 1) {
      throw new Error(`base process environment has ambiguous ${canonicalKey} keys`);
    }
    const value = matchingKeys.length === 1 ? baseEnvironment[matchingKeys[0]] : undefined;
    if (typeof value === 'string' && value.length > 0) environment[canonicalKey] = value;
  }
  return environment;
}

function existingEntry(file) {
  try {
    return lstatSync(file, { bigint: true });
  } catch (error) {
    if (error?.code === 'ENOENT') return null;
    throw error;
  }
}

export function assertVoCliCargoConfigBoundary(volangRoot, environment) {
  const root = canonicalRoot(volangRoot);
  if (!environment || typeof environment !== 'object' || Array.isArray(environment)) {
    throw new Error('Vo CLI Cargo config boundary requires an environment object');
  }
  const cargoHome = environment.CARGO_HOME;
  if (typeof cargoHome !== 'string' || !path.isAbsolute(cargoHome)) {
    throw new Error('Vo CLI Cargo config boundary requires an absolute CARGO_HOME');
  }
  const allowed = path.join(root, '.cargo', 'config.toml');
  const candidates = new Map();
  let current = root;
  while (true) {
    for (const name of ['config', 'config.toml']) {
      const candidate = path.join(current, '.cargo', name);
      candidates.set(path.normalize(candidate), candidate);
    }
    const parent = path.dirname(current);
    if (parent === current) break;
    current = parent;
  }
  for (const name of ['config', 'config.toml']) {
    const candidate = path.join(cargoHome, name);
    candidates.set(path.normalize(candidate), candidate);
  }
  for (const candidate of candidates.values()) {
    const metadata = existingEntry(candidate);
    if (metadata === null) continue;
    if (!sameNativePath(candidate, allowed)) {
      throw new Error(`undeclared Cargo config may influence the Vo CLI producer: ${candidate}`);
    }
    if (
      !metadata.isFile()
      || metadata.isSymbolicLink()
      || !sameNativePath(realpathSync.native(candidate), allowed)
    ) {
      throw new Error(`declared Cargo config must be a real regular file: ${candidate}`);
    }
    readStableRegularFile(candidate, 'declared Vo CLI Cargo config');
  }
}

function resolveCargoTarget(root, candidate) {
  const targetRoot = path.join(root, 'target');
  if (candidate === undefined || candidate === null) return targetRoot;
  if (typeof candidate !== 'string' || !path.isAbsolute(candidate)) {
    throw new Error('Vo CLI producer Cargo target must be an absolute path');
  }
  const resolved = path.resolve(candidate);
  if (
    !pathWithin(targetRoot, resolved)
    || sameNativePath(targetRoot, resolved)
    || !path.basename(resolved).startsWith('.blockkart-vpak-cargo-')
  ) {
    throw new Error('Vo CLI producer Cargo target must be a task-owned .blockkart-vpak-cargo-* directory');
  }
  const metadata = lstatSync(resolved, { bigint: true });
  if (
    !metadata.isDirectory()
    || metadata.isSymbolicLink()
    || !sameNativePath(realpathSync.native(resolved), resolved)
  ) {
    throw new Error('Vo CLI producer Cargo target must be a real directory without symbolic links');
  }
  return resolved;
}

function resolveCargoHome(root, environment, candidate) {
  if (candidate === undefined || candidate === null) {
    const home = environment.HOME ?? environment.USERPROFILE;
    if (!path.isAbsolute(home)) {
      throw new Error('Vo CLI build environment requires an absolute HOME or USERPROFILE');
    }
    return path.resolve(home, '.cargo');
  }
  if (typeof candidate !== 'string' || !path.isAbsolute(candidate)) {
    throw new Error('Vo CLI producer Cargo home must be an absolute path');
  }
  const resolved = path.resolve(candidate);
  const targetRoot = path.join(root, 'target');
  if (
    !pathWithin(targetRoot, resolved)
    || sameNativePath(targetRoot, resolved)
    || !path.basename(resolved).startsWith('.blockkart-vpak-cargo-home-')
  ) {
    throw new Error('Vo CLI producer Cargo home must be a task-owned .blockkart-vpak-cargo-home-* directory');
  }
  const metadata = lstatSync(resolved, { bigint: true });
  if (
    !metadata.isDirectory()
    || metadata.isSymbolicLink()
    || !sameNativePath(realpathSync.native(resolved), resolved)
  ) {
    throw new Error('Vo CLI producer Cargo home must be a real directory without symbolic links');
  }
  return resolved;
}

function encodedRustFlags(root, cargoHome, cargoTarget) {
  const values = [
    `--remap-path-prefix=${root}=/workspace/volang`,
    `--remap-path-prefix=${cargoHome}=/workspace/cargo-home`,
    `--remap-path-prefix=${cargoTarget}=/workspace/cargo-target`,
  ];
  if (values.some((value) => value.includes('\0') || value.includes('\u001f'))) {
    throw new Error('Vo CLI producer remap paths contain an unsupported control character');
  }
  return values.join('\u001f');
}

export function voCliBuildCommand(host) {
  if (typeof host !== 'string' || !/^[A-Za-z0-9_.-]+$/u.test(host)) {
    throw new Error(`Vo CLI producer host target is invalid: ${host}`);
  }
  return VO_CLI_BUILD_COMMAND.map((argument) => argument === '<HOST>' ? host : argument);
}

export function voCliBuildEnvironment(
  baseEnvironment,
  volangRoot,
  cargoTargetDirectory = null,
  cargoHomeDirectory = null,
) {
  const root = canonicalRoot(volangRoot);
  const environment = inheritEnvironment(
    baseEnvironment,
    VO_CLI_BUILD_ENVIRONMENT.inherited,
  );
  if (typeof environment.PATH !== 'string' || environment.PATH.length === 0) {
    throw new Error('Vo CLI build environment requires an inherited PATH');
  }
  if (
    (typeof environment.HOME !== 'string' || environment.HOME.length === 0)
    && (typeof environment.USERPROFILE !== 'string' || environment.USERPROFILE.length === 0)
  ) {
    throw new Error('Vo CLI build environment requires HOME or USERPROFILE');
  }
  const cargoHome = resolveCargoHome(root, environment, cargoHomeDirectory);
  const cargoTarget = resolveCargoTarget(root, cargoTargetDirectory);
  Object.assign(environment, VO_CLI_BUILD_ENVIRONMENT.fixed, {
    CARGO_ENCODED_RUSTFLAGS: encodedRustFlags(root, cargoHome, cargoTarget),
    CARGO_HOME: cargoHome,
    CARGO_TARGET_DIR: cargoTarget,
  });
  assertVoCliCargoConfigBoundary(root, environment);
  return environment;
}

export function voCliGuestEnvironment(baseEnvironment, volangRoot, voBinary) {
  const root = canonicalRoot(volangRoot);
  if (typeof voBinary !== 'string' || !path.isAbsolute(voBinary)) {
    throw new Error('Vo CLI guest environment requires an absolute VO_BIN path');
  }
  const expectedBinary = path.join(
    root,
    'target',
    'blockkart-vpak-build',
    process.platform === 'win32' ? 'vo.exe' : 'vo',
  );
  if (!sameNativePath(voBinary, expectedBinary)) {
    throw new Error(`Vo CLI guest environment requires VO_BIN=${expectedBinary}`);
  }
  const environment = inheritEnvironment(
    baseEnvironment,
    VO_CLI_GUEST_ENVIRONMENT.inherited,
  );
  for (const [key, relative] of Object.entries(VO_CLI_GUEST_ENVIRONMENT.paths)) {
    if (key === 'VO_BIN') continue;
    environment[key] = path.join(root, ...relative.split('/'));
  }
  environment.VO_BIN = expectedBinary;
  Object.assign(environment, VO_CLI_GUEST_ENVIRONMENT.fixed);
  for (const unsetKey of VO_CLI_GUEST_ENVIRONMENT.unset) {
    for (const key of Object.keys(environment)) {
      if (key.toUpperCase() === unsetKey.toUpperCase()) delete environment[key];
    }
  }
  return environment;
}

function toolVersion(command, args, environment, root, label) {
  const result = spawnSync(command, args, {
    cwd: canonicalRoot(root),
    encoding: 'utf8',
    env: environment,
    maxBuffer: MAX_TOOL_VERSION_BYTES,
    timeout: 30_000,
  });
  if (result.error || result.status !== 0) {
    const detail = String(result.stderr || result.error?.message || `status ${result.status}`).trim();
    throw new Error(`${label} version query failed: ${detail}`);
  }
  return String(result.stdout).trim();
}

export function currentVoCliToolchain(volangRoot, baseEnvironment = process.env) {
  const environment = voCliBuildEnvironment(baseEnvironment, volangRoot);
  const cargoVerbose = toolVersion('cargo', ['--version', '--verbose'], environment, volangRoot, 'cargo');
  const rustcVerbose = toolVersion('rustc', ['-vV'], environment, volangRoot, 'rustc');
  const cargo = cargoVerbose.split(/\r?\n/u)[0] ?? '';
  const rustc = rustcVerbose.split(/\r?\n/u)[0] ?? '';
  const host = rustcVerbose.match(/^host:\s*(\S+)\s*$/mu)?.[1] ?? '';
  if (!/^cargo\s+\d+\.\d+\.\d+/u.test(cargo)) {
    throw new Error(`cargo returned an invalid version identity: ${cargo}`);
  }
  if (!/^rustc\s+\d+\.\d+\.\d+/u.test(rustc) || !/^[A-Za-z0-9_.-]+$/u.test(host)) {
    throw new Error(`rustc returned an invalid version identity: ${rustc}; host=${host}`);
  }
  return Object.freeze({ cargo, host, rustc, target: host });
}

export function verifyVoCliExecutionIdentity(
  toolchain,
  binary,
  {
    buildInputs = null,
    executionDigest = null,
    expectedToolchain = null,
  } = {},
) {
  const issues = [];
  if (!exactKeys(toolchain, ['cargo', 'host', 'rustc', 'target'])) {
    issues.push('Vo CLI toolchain must contain exact cargo/rustc/host/target fields');
  } else {
    if (!/^cargo\s+\d+\.\d+\.\d+/u.test(toolchain.cargo ?? '')) {
      issues.push('Vo CLI toolchain cargo identity is invalid');
    }
    if (!/^rustc\s+\d+\.\d+\.\d+/u.test(toolchain.rustc ?? '')) {
      issues.push('Vo CLI toolchain rustc identity is invalid');
    }
    if (
      typeof toolchain.host !== 'string'
      || !/^[A-Za-z0-9_.-]+$/u.test(toolchain.host)
      || toolchain.target !== toolchain.host
    ) {
      issues.push('Vo CLI toolchain host/target identity is invalid');
    }
    if (
      expectedToolchain !== null
      && (
        toolchain.cargo !== expectedToolchain.cargo
        || toolchain.rustc !== expectedToolchain.rustc
      )
    ) {
      issues.push('Vo CLI toolchain versions do not match the pinned current toolchain');
    }
  }
  if (!exactKeys(binary, ['digest', 'path', 'size'])) {
    issues.push('Vo CLI binary must contain exact digest/path/size fields');
  } else {
    const windowsTarget = typeof toolchain?.target === 'string' && toolchain.target.includes('windows');
    const expectedPath = windowsTarget
      ? 'target/blockkart-vpak-build/vo.exe'
      : 'target/blockkart-vpak-build/vo';
    if (binary.path !== expectedPath) issues.push('Vo CLI binary path does not match its target');
    if (!validDigest(binary.digest)) issues.push('Vo CLI binary digest must be sha256');
    if (
      !Number.isSafeInteger(binary.size)
      || binary.size <= 0
      || binary.size > MAX_VO_BINARY_BYTES
    ) {
      issues.push(`Vo CLI binary size must be within 1..${MAX_VO_BINARY_BYTES} bytes`);
    }
  }
  if (buildInputs !== null || executionDigest !== null) {
    if (!validDigest(buildInputs?.digest)) {
      issues.push('Vo CLI execution identity requires a valid build-input digest');
    }
    if (!validDigest(executionDigest)) {
      issues.push('Vo CLI execution digest must be sha256');
    } else if (
      issues.length === 0
      && executionDigest !== voCliExecutionDigest(buildInputs, toolchain, binary)
    ) {
      issues.push('Vo CLI execution digest does not match build inputs, toolchain, and binary');
    }
  }
  return issues;
}

export function voCliExecutionDigest(buildInputs, toolchain, binary) {
  if (!validDigest(buildInputs?.digest)) {
    throw new Error('Vo CLI execution digest requires a valid build-input digest');
  }
  if (!exactKeys(toolchain, ['cargo', 'host', 'rustc', 'target'])) {
    throw new Error('Vo CLI execution digest requires exact toolchain fields');
  }
  if (!exactKeys(binary, ['digest', 'path', 'size'])) {
    throw new Error('Vo CLI execution digest requires exact binary fields');
  }
  return sha256(Buffer.from(JSON.stringify({
    buildInputsDigest: buildInputs.digest,
    toolchain: {
      cargo: toolchain.cargo,
      host: toolchain.host,
      rustc: toolchain.rustc,
      target: toolchain.target,
    },
    binary: {
      digest: binary.digest,
      path: binary.path,
      size: binary.size,
    },
  }), 'utf8'));
}

export function decodeVoCliBuildInputsJson(bytes, label = 'voCliBuildInputs JSON') {
  if (!Buffer.isBuffer(bytes) || bytes.byteLength > MAX_METADATA_BYTES) {
    throw new Error(`${label} exceeds the ${MAX_METADATA_BYTES}-byte JSON limit`);
  }
  let source;
  try {
    source = UTF8_DECODER.decode(bytes);
  } catch (error) {
    throw new Error(`${label} is invalid UTF-8: ${error.message}`);
  }
  try {
    return JSON.parse(source);
  } catch (error) {
    throw new Error(`${label} is invalid JSON: ${error.message}`);
  }
}
