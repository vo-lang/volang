import { PortablePathTrie, portableCaseKey } from './portable_path_key';
import { parseBoundedJsonBytes } from '../../../../scripts/ci/bounded_json.mjs';

// Pure, side-effect-free validation for the checked-in Quickplay wire format.
// Keep these renderers and invariants aligned with vo-module's ProjectSnapshot
// v2, vo.lock v3, WorkFile v1, ReleaseManifest v2, and PackageManifest v1.
const BLOCKKART_MODULE = 'github.com/vo-lang/blockkart';
const BLOCKKART_VPAK_PATH = 'assets/blockkart.vpak';
const BLOCKKART_VPAK_PRODUCER_PATH = 'assets/blockkart.vpak.provenance.json';
const MAX_U64 = 18_446_744_073_709_551_615n;
const MAX_MODULES = 10_000;
const MAX_GRAPH_EDGES = 100_000;
const MAX_PACKAGE_ENTRIES = 20_000;
const MAX_PACKAGE_PATH_NODES = 100_000;
const MAX_PACKAGE_PATH_KEY_BYTES = 16 * 1024 * 1024;
const MAX_VFS_PATH_BYTES = 4 * 1024;
// Relative spellings are shared with the generator and become absolute only
// at the VFS commit boundary.
export const QUICKPLAY_PROJECT_VFS_ROOT = 'workspace/.volang/apps/studio/sessions/quickplay/BlockKart/current';
export const QUICKPLAY_WORKSPACE_MODULE_VFS_ROOT = 'workspace/.volang/apps/studio/sessions/quickplay/BlockKart/modules';
const MAX_MODULE_ARTIFACTS = 997;
const MAX_METADATA_BYTES = 16 * 1024 * 1024;
const MAX_FILE_BYTES = 256 * 1024 * 1024;
const MAX_MODULE_ARTIFACT_BYTES = 256 * 1024 * 1024;
const MAX_TOTAL_BYTES = 512 * 1024 * 1024;
const MAX_RELEASE_SOURCE_ARCHIVE_BYTES = 64 * 1024 * 1024;
const MAX_PACKAGE_SOURCE_FILE_BYTES = 64 * 1024 * 1024;
const MAX_PACKAGE_EXTRACTED_BYTES = 128 * 1024 * 1024;
const MAX_QUICKPLAY_SOURCE_BYTES = 64 * 1024 * 1024;
const MAX_CANONICAL_JSON_DEPTH = 127;
const DIGEST_PATTERN = /^sha256:[0-9a-f]{64}$/;
const COMMIT_PATTERN = /^[0-9a-f]{40}$/;
const VERSION_PATTERN = /^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(?:-([0-9a-z-]+(?:\.[0-9a-z-]+)*))?$/;
const WORKSPACE_RESERVED_PROTOCOL_FILES = new Set([
  '.vo-project.lock',
  '.vo-project.transaction',
  '.vo-source-digest',
  '.vo-version',
  'vo.lock',
  'vo.package.json',
  'vo.release.json',
  'source.tar.gz',
  'vo.sum',
  'vo.web.json',
  'vo.work',
]);
const PROJECT_ROOT_CONTROL_FILES = new Set(['vo.lock', 'vo.mod', 'vo.work']);
const PROJECT_REMOVED_PROTOCOL_FILES = new Set([
  '.vo-project.lock',
  '.vo-project.transaction',
  '.vo-source-digest',
  '.vo-version',
  'vo.package.json',
  'vo.release.json',
  'source.tar.gz',
  'vo.sum',
  'vo.web.json',
]);
const textEncoder = new TextEncoder();
const textDecoder = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true });

function isReservedWorkspacePath(relative: string): boolean {
  const components = relative.split('/');
  const first = portableCaseKey(components[0] ?? '');
  const basename = portableCaseKey(components[components.length - 1] ?? '');
  return first === 'artifacts'
    || (components.length === 1 && WORKSPACE_RESERVED_PROTOCOL_FILES.has(first))
    || (basename === 'vo.mod' && relative !== 'vo.mod');
}

type JsonRecord = Record<string, unknown>;

export interface QuickplaySnapshotDependency {
  module: string;
  constraint: string;
}

export interface QuickplaySnapshotRoot {
  module: string;
  vo: string;
  dependencies: QuickplaySnapshotDependency[];
}

export interface QuickplayRegistrySource {
  kind: 'registry';
  directory: string;
}

export interface QuickplayWorkspaceSource {
  kind: 'workspace';
  directory: string;
}

interface QuickplaySnapshotModuleBase {
  module: string;
  vo: string;
  dependencies: QuickplaySnapshotDependency[];
}

export interface QuickplayRegistrySnapshotModule extends QuickplaySnapshotModuleBase {
  version: string;
  release: string;
  source: QuickplayRegistrySource;
}

export interface QuickplayLockedWorkspaceSnapshotModule extends QuickplaySnapshotModuleBase {
  version: string;
  release: string;
  source: QuickplayWorkspaceSource;
}

export interface QuickplayWorkspaceSnapshotModule extends QuickplaySnapshotModuleBase {
  source: QuickplayWorkspaceSource;
}

export type QuickplayLockedSnapshotModule =
  | QuickplayRegistrySnapshotModule
  | QuickplayLockedWorkspaceSnapshotModule;

export type QuickplaySnapshotModule =
  | QuickplayLockedSnapshotModule
  | QuickplayWorkspaceSnapshotModule;

interface QuickplayProjectSnapshotBase {
  schema_version: 2;
  mode: 'effective';
  root: QuickplaySnapshotRoot;
}

export interface QuickplayLockProjectSnapshot extends QuickplayProjectSnapshotBase {
  authority: 'lock';
  workspace?: { file: 'vo.work' };
  modules: QuickplayLockedSnapshotModule[];
}

export interface QuickplayWorkspaceProjectSnapshot extends QuickplayProjectSnapshotBase {
  authority: 'workspace';
  workspace: { file: 'vo.work' };
  modules: QuickplayWorkspaceSnapshotModule[];
}

export type QuickplayProjectSnapshot =
  | QuickplayLockProjectSnapshot
  | QuickplayWorkspaceProjectSnapshot;

function isRegistrySnapshotModule(
  module: QuickplaySnapshotModule,
): module is QuickplayRegistrySnapshotModule {
  return module.source.kind === 'registry';
}

export interface QuickplayStaticFile {
  path: string;
  mode: number;
  size: number;
  digest: string;
  content?: string;
  contentBase64?: string;
}

export interface QuickplayStaticArtifact {
  kind: 'extension-wasm' | 'extension-js-glue';
  target: 'wasm32-unknown-unknown';
  name: string;
  size: number;
  digest: string;
  path: string;
  url: string;
}

export interface BlockKartProjectPackage {
  schemaVersion: 2;
  name: 'BlockKart';
  module: typeof BLOCKKART_MODULE;
  baseCommit: string;
  snapshot: QuickplayProjectSnapshot;
  files: QuickplayStaticFile[];
}

export interface BlockKartDependencyModulePackage {
  source: 'registry' | 'workspace';
  module: string;
  version?: string;
  cacheDir: string;
  files: QuickplayStaticFile[];
  artifacts: QuickplayStaticArtifact[];
}

export interface BlockKartDepsPackage {
  schemaVersion: 2;
  name: 'BlockKart dependencies';
  snapshotDigest: string;
  modules: BlockKartDependencyModulePackage[];
}

interface ParsedVersion {
  major: bigint;
  minor: bigint;
  patch: bigint;
  pre: Array<{ kind: 'numeric'; value: bigint } | { kind: 'alpha'; value: string }>;
}

interface ParsedConstraint {
  op: 'exact' | 'compatible' | 'patch';
  version: ParsedVersion;
}

interface ReleaseArtifact {
  kind: 'extension-native' | 'extension-wasm' | 'extension-js-glue';
  target: string;
  name: string;
  size: number;
  digest: string;
}

function fail(message: string): never {
  throw new Error(message);
}

function isRecord(value: unknown): value is JsonRecord {
  return value !== null && typeof value === 'object' && !Array.isArray(value);
}

function exactKeys(value: unknown, expected: readonly string[], label: string): asserts value is JsonRecord {
  if (!isRecord(value)) fail(`${label} must be an object`);
  const actual = Object.keys(value).sort(compareUtf8);
  const wanted = [...expected].sort(compareUtf8);
  if (actual.length !== wanted.length || actual.some((key, index) => key !== wanted[index])) {
    fail(`${label} has unexpected fields`);
  }
}

function compareUtf8(left: string, right: string): number {
  const leftBytes = textEncoder.encode(left);
  const rightBytes = textEncoder.encode(right);
  const common = Math.min(leftBytes.length, rightBytes.length);
  for (let index = 0; index < common; index += 1) {
    if (leftBytes[index] !== rightBytes[index]) return leftBytes[index] - rightBytes[index];
  }
  return leftBytes.length - rightBytes.length;
}

function canonicalProtocolJsonString(value: string): string {
  let output = '"';
  for (const character of value) {
    const codepoint = character.codePointAt(0) as number;
    if (codepoint >= 0xd800 && codepoint <= 0xdfff) {
      fail('Canonical protocol JSON strings must contain Unicode scalar values');
    }
    switch (character) {
      case '"': output += '\\"'; break;
      case '\\': output += '\\\\'; break;
      case '\b': output += '\\b'; break;
      case '\t': output += '\\t'; break;
      case '\n': output += '\\n'; break;
      case '\f': output += '\\f'; break;
      case '\r': output += '\\r'; break;
      default:
        output += codepoint <= 0x1f
          ? `\\u00${codepoint.toString(16).padStart(2, '0')}`
          : character;
    }
  }
  return `${output}"`;
}

export function canonicalProtocolJsonText(value: unknown, depth = 0): string {
  if (!Number.isSafeInteger(depth) || depth < 0 || depth > MAX_CANONICAL_JSON_DEPTH) {
    fail(`Canonical JSON depth exceeds ${MAX_CANONICAL_JSON_DEPTH}`);
  }
  if (value === null) return 'null';
  if (typeof value === 'string') return canonicalProtocolJsonString(value);
  if (typeof value === 'number') {
    if (!Number.isSafeInteger(value) || value < 0) {
      fail('Canonical protocol JSON numbers must be non-negative safe integers');
    }
    return String(value);
  }
  if (typeof value === 'boolean') return value ? 'true' : 'false';
  const indentation = '  '.repeat(depth);
  const childIndentation = '  '.repeat(depth + 1);
  if (Array.isArray(value)) {
    if (value.length === 0) return '[]';
    return `[\n${value
      .map((entry) => `${childIndentation}${canonicalProtocolJsonText(entry, depth + 1)}`)
      .join(',\n')}\n${indentation}]`;
  }
  if (!isRecord(value) || Object.getPrototypeOf(value) !== Object.prototype) {
    fail('Canonical protocol JSON values must use plain objects');
  }
  const entries = Object.entries(value);
  if (entries.length === 0) return '{}';
  return `{\n${entries
    .map(([key, entry]) => (
      `${childIndentation}${canonicalProtocolJsonString(key)}: ${canonicalProtocolJsonText(entry, depth + 1)}`
    ))
    .join(',\n')}\n${indentation}}`;
}

function assertString(value: unknown, label: string): asserts value is string {
  if (typeof value !== 'string' || value.length === 0) fail(`${label} must be a non-empty string`);
}

function hasUnpairedSurrogate(value: string): boolean {
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

function assertDigest(value: unknown, label: string): asserts value is string {
  if (typeof value !== 'string' || !DIGEST_PATTERN.test(value)) fail(`${label} must be a canonical sha256 digest`);
}

function assertPositiveSize(value: unknown, label: string, max = MAX_FILE_BYTES): asserts value is number {
  if (!Number.isSafeInteger(value) || (value as number) <= 0 || (value as number) > max) {
    fail(`${label} must be a positive bounded size`);
  }
}

function assertBoundSize(value: unknown, label: string, max = MAX_FILE_BYTES): asserts value is number {
  if (!Number.isSafeInteger(value) || (value as number) < 0 || (value as number) > max) {
    fail(`${label} must be a non-negative bounded size`);
  }
}

function parseVersion(value: unknown, label: string): ParsedVersion {
  assertString(value, label);
  if (textEncoder.encode(value).byteLength >= 255) fail(`${label} exceeds the canonical version length`);
  if (value.toLowerCase().endsWith('.lock')) fail(`${label} uses the Git-reserved .lock suffix`);
  const match = VERSION_PATTERN.exec(value);
  if (!match) fail(`${label} must be canonical SemVer`);
  const components = match.slice(1, 4).map((component) => BigInt(component));
  if (components.some((component) => component > MAX_U64)) fail(`${label} overflows u64`);
  const pre = match[4]?.split('.').map((component) => {
    if (!/^[0-9]+$/.test(component)) return { kind: 'alpha' as const, value: component };
    if ((component.length > 1 && component.startsWith('0')) || BigInt(component) > MAX_U64) {
      fail(`${label} has a non-canonical numeric prerelease identifier`);
    }
    return { kind: 'numeric' as const, value: BigInt(component) };
  }) ?? [];
  return { major: components[0], minor: components[1], patch: components[2], pre };
}

function parseConstraint(value: unknown, label: string): ParsedConstraint {
  assertString(value, label);
  const op = value[0] === '^' ? 'compatible' : value[0] === '~' ? 'patch' : 'exact';
  return { op, version: parseVersion(op === 'exact' ? value : value.slice(1), label) };
}

function compareVersions(left: ParsedVersion, right: ParsedVersion): number {
  for (const component of ['major', 'minor', 'patch'] as const) {
    if (left[component] < right[component]) return -1;
    if (left[component] > right[component]) return 1;
  }
  if (left.pre.length === 0 || right.pre.length === 0) {
    if (left.pre.length === right.pre.length) return 0;
    return left.pre.length === 0 ? 1 : -1;
  }
  const common = Math.min(left.pre.length, right.pre.length);
  for (let index = 0; index < common; index += 1) {
    const leftPart = left.pre[index];
    const rightPart = right.pre[index];
    if (leftPart.kind !== rightPart.kind) return leftPart.kind === 'numeric' ? -1 : 1;
    if (leftPart.value < rightPart.value) return -1;
    if (leftPart.value > rightPart.value) return 1;
  }
  return Math.sign(left.pre.length - right.pre.length);
}

function sameVersionCore(left: ParsedVersion, right: ParsedVersion): boolean {
  return left.major === right.major && left.minor === right.minor && left.patch === right.patch;
}

function constraintSatisfies(constraint: ParsedConstraint, selected: ParsedVersion): boolean {
  if (constraint.op === 'exact') return compareVersions(selected, constraint.version) === 0;
  if (
    selected.pre.length > 0
    && (constraint.version.pre.length === 0 || !sameVersionCore(selected, constraint.version))
  ) return false;
  if (compareVersions(selected, constraint.version) < 0) return false;
  if (constraint.op === 'patch') {
    return selected.major === constraint.version.major && selected.minor === constraint.version.minor;
  }
  if (constraint.version.major !== 0n) return selected.major === constraint.version.major;
  if (constraint.version.minor !== 0n) {
    return selected.major === 0n && selected.minor === constraint.version.minor;
  }
  return selected.major === 0n
    && selected.minor === 0n
    && selected.patch === constraint.version.patch;
}

function incrementVersion(version: ParsedVersion, component: 'major' | 'minor' | 'patch'): ParsedVersion | null {
  if (version[component] === MAX_U64) return null;
  if (component === 'major') return { major: version.major + 1n, minor: 0n, patch: 0n, pre: [] };
  if (component === 'minor') return { major: version.major, minor: version.minor + 1n, patch: 0n, pre: [] };
  return { major: version.major, minor: version.minor, patch: version.patch + 1n, pre: [] };
}

function constraintUpperBound(constraint: ParsedConstraint): ParsedVersion | null {
  const version = constraint.version;
  if (constraint.op === 'patch') return incrementVersion(version, 'minor') ?? incrementVersion(version, 'major');
  if (version.major !== 0n) return incrementVersion(version, 'major');
  if (version.minor !== 0n) {
    return incrementVersion(version, 'minor') ?? { major: 1n, minor: 0n, patch: 0n, pre: [] };
  }
  return incrementVersion(version, 'patch') ?? { major: 0n, minor: 1n, patch: 0n, pre: [] };
}

function rangeAcceptsOnlyLower(constraint: ParsedConstraint): boolean {
  if (constraint.version.pre.length > 0) return false;
  if (constraint.op === 'exact') return true;
  if (constraint.op === 'patch') return constraint.version.patch === MAX_U64;
  if (constraint.version.major === 0n) {
    return constraint.version.minor === 0n || constraint.version.patch === MAX_U64;
  }
  return constraint.version.minor === MAX_U64 && constraint.version.patch === MAX_U64;
}

function constraintIsSubset(left: ParsedConstraint, right: ParsedConstraint): boolean {
  if (left.op === 'exact') return constraintSatisfies(right, left.version);
  if (right.op === 'exact') {
    return compareVersions(left.version, right.version) === 0 && rangeAcceptsOnlyLower(left);
  }
  if (
    left.version.pre.length > 0
    && (right.version.pre.length === 0 || !sameVersionCore(left.version, right.version))
  ) return false;
  if (compareVersions(right.version, left.version) > 0) return false;
  const leftUpper = constraintUpperBound(left);
  const rightUpper = constraintUpperBound(right);
  if (rightUpper === null) return true;
  return leftUpper !== null && compareVersions(leftUpper, rightUpper) <= 0;
}

function validateModulePath(value: unknown, label: string, versionMajor?: bigint): string {
  assertString(value, label);
  if (textEncoder.encode(value).byteLength > 255 || !value.startsWith('github.com/')) {
    fail(`${label} must be a bounded canonical github.com module path`);
  }
  const components = value.split('/');
  if (
    components.length < 3
    || components.some((component, index) => (
      !/^[a-z0-9][a-z0-9._-]*$/.test(component)
      || component.endsWith('.')
      || reservedPortableStem(component)
      || (index >= 3 && (component.includes('..') || component.endsWith('.lock')))
    ))
  ) fail(`${label} must be a canonical github.com module path`);
  // Repository names such as v1 and v2 are unsuffixed roots. A major suffix
  // exists only when the terminal vN segment follows the repository.
  const suffix = components.length > 3
    ? /^v([0-9]+)$/.exec(components[components.length - 1])
    : null;
  let suffixMajor: bigint | null = null;
  if (suffix) {
    suffixMajor = BigInt(suffix[1]);
    if (
      (suffix[1].length > 1 && suffix[1].startsWith('0'))
      || suffixMajor < 2n
      || suffixMajor > MAX_U64
    ) fail(`${label} has an invalid major-version suffix`);
  }
  if (versionMajor !== undefined) {
    if (suffixMajor === null ? versionMajor > 1n : suffixMajor !== versionMajor) {
      fail(`${label} is incompatible with its selected major version`);
    }
  }
  return value.split('/').join('@');
}

function reservedPortableStem(component: string): boolean {
  const stem = portableCaseKey(component.split('.', 1)[0]);
  return ['con', 'prn', 'aux', 'nul', 'conin$', 'conout$'].includes(stem)
    || /^(?:com|lpt)(?:[1-9]|[¹²³])$/.test(stem);
}

function validatePortableComponent(value: unknown, label: string): asserts value is string {
  assertString(value, label);
  if (
    textEncoder.encode(value).byteLength > 255
    || hasUnpairedSurrogate(value)
    || value === '.'
    || value === '..'
    || /^(?:\p{White_Space})|(?:\p{White_Space})$/u.test(value)
    || value.endsWith('.')
    || value.normalize('NFC') !== value
    || value.includes('/')
    || value.includes('\\')
    || /[\p{Cc}<>:"|?*]/u.test(value)
    || reservedPortableStem(value)
  ) fail(`${label} must be a normalized portable path component`);
}

function validatePortableRelativePath(value: unknown, label: string): asserts value is string {
  assertString(value, label);
  if (
    textEncoder.encode(value).byteLength > 4 * 1024
    || hasUnpairedSurrogate(value)
    || /^(?:\p{White_Space})|(?:\p{White_Space})$/u.test(value)
    || value.includes('\\')
    || /\p{Cc}/u.test(value)
  ) fail(`${label} must be a normalized portable relative path`);
  const components = value.split('/');
  if (components.length > 256) fail(`${label} contains too many path components`);
  for (const component of components) validatePortableComponent(component, `${label} component`);
}

function assertPortableFileClosure(
  paths: readonly string[],
  label: string,
  absolute = false,
): number {
  const portablePaths = new PortablePathTrie(
    MAX_PACKAGE_PATH_NODES,
    MAX_PACKAGE_PATH_KEY_BYTES,
  );
  for (const relative of paths) {
    validatePortableRelativePath(relative, `${label} path`);
    if (absolute && textEncoder.encode(relative).byteLength + 1 > MAX_VFS_PATH_BYTES) {
      fail(`${label} path exceeds the ${MAX_VFS_PATH_BYTES}-byte VFS path limit`);
    }
    portablePaths.insert(relative, false, label);
  }
  return portablePaths.nodeCount;
}

function artifactCachePath(artifact: { kind: string; target: string; name: string }): string {
  return `artifacts/${artifact.kind}/${artifact.target}/${artifact.name}`;
}

function encodeQuickplayUrlComponent(value: string): string {
  return encodeURIComponent(value)
    .replace(/%40/g, '@')
    .replace(/[!'()*]/g, (character: string) => (
      `%${character.charCodeAt(0).toString(16).toUpperCase()}`
    ));
}

function quickplayArtifactUrl(
  cacheDir: string,
  artifact: { kind: string; target: string; name: string },
): string {
  const relative = `${cacheDir}/${artifact.kind}/${artifact.target}/${artifact.name}`;
  return `/quickplay/blockkart/artifacts/${relative.split('/').map(encodeQuickplayUrlComponent).join('/')}`;
}

function validateDependencies(value: unknown, label: string): QuickplaySnapshotDependency[] {
  if (!Array.isArray(value) || value.length > MAX_MODULES) fail(`${label} must be a bounded array`);
  const dependencies = value.map((raw, index) => {
    exactKeys(raw, ['module', 'constraint'], `${label}[${index}]`);
    const constraint = parseConstraint(raw.constraint, `${label}[${index}].constraint`);
    validateModulePath(raw.module, `${label}[${index}].module`, constraint.version.major);
    return { module: raw.module as string, constraint: raw.constraint as string };
  });
  for (let index = 1; index < dependencies.length; index += 1) {
    if (compareUtf8(dependencies[index - 1].module, dependencies[index].module) >= 0) {
      fail(`${label} must be strictly sorted and unique`);
    }
  }
  return dependencies;
}

function validateSnapshot(value: unknown): QuickplayProjectSnapshot {
  const record = value as JsonRecord;
  exactKeys(
    record,
    isRecord(record) && record.workspace === undefined
      ? ['schema_version', 'mode', 'authority', 'root', 'modules']
      : ['schema_version', 'mode', 'authority', 'root', 'workspace', 'modules'],
    'BlockKart project snapshot',
  );
  if (
    record.schema_version !== 2
    || record.mode !== 'effective'
    || (record.authority !== 'lock' && record.authority !== 'workspace')
  ) {
    fail('BlockKart requires effective ProjectSnapshot v2 with lock or workspace authority');
  }
  exactKeys(record.root, ['module', 'vo', 'dependencies'], 'BlockKart snapshot root');
  if (record.root.module !== BLOCKKART_MODULE) fail('BlockKart snapshot root module is invalid');
  validateModulePath(record.root.module, 'BlockKart snapshot root module');
  const rootVo = parseConstraint(record.root.vo, 'BlockKart snapshot root toolchain');
  const rootDependencies = validateDependencies(record.root.dependencies, 'BlockKart snapshot root dependencies');
  if (rootDependencies.length === 0) fail('BlockKart snapshot has no root dependencies');

  if (!Array.isArray(record.modules) || record.modules.length === 0 || record.modules.length > MAX_MODULES) {
    fail('BlockKart snapshot modules must be a non-empty bounded array');
  }
  const modules: QuickplaySnapshotModule[] = [];
  let previousModule = '';
  let edgeCount = rootDependencies.length;
  for (const [index, raw] of record.modules.entries()) {
    exactKeys(
      raw,
      record.authority === 'lock'
        ? ['module', 'version', 'vo', 'release', 'source', 'dependencies']
        : ['module', 'vo', 'source', 'dependencies'],
      `BlockKart snapshot modules[${index}]`,
    );
    const version = record.authority === 'lock'
      ? parseVersion(raw.version, `${String(raw.module)} selected version`)
      : null;
    validateModulePath(
      raw.module,
      `BlockKart snapshot modules[${index}].module`,
      version?.major,
    );
    if (raw.module === BLOCKKART_MODULE || compareUtf8(previousModule, raw.module as string) >= 0) {
      fail('BlockKart snapshot modules must be sorted, unique, and exclude the root');
    }
    previousModule = raw.module as string;
    const moduleVo = parseConstraint(raw.vo, `${String(raw.module)} toolchain constraint`);
    if (!constraintIsSubset(rootVo, moduleVo)) fail(`${String(raw.module)} does not cover the root toolchain constraint`);
    if (record.authority === 'lock') assertDigest(raw.release, `${String(raw.module)} release digest`);
    if (!isRecord(raw.source) || (raw.source.kind !== 'registry' && raw.source.kind !== 'workspace')) {
      fail(`${String(raw.module)} snapshot source kind is invalid`);
    }
    if (record.authority === 'workspace' && raw.source.kind !== 'workspace') {
      fail(`${String(raw.module)} must use a workspace source under workspace authority`);
    }
    const cacheKey = (raw.module as string).split('/').join('@');
    let source: QuickplayRegistrySource | QuickplayWorkspaceSource;
    if (raw.source.kind === 'registry') {
      exactKeys(raw.source, ['kind', 'directory'], `${String(raw.module)} registry source`);
      if (raw.source.directory !== `${cacheKey}/${String(raw.version)}`) {
        fail(`${String(raw.module)} registry source directory is not canonical`);
      }
      source = { kind: 'registry', directory: raw.source.directory as string };
    } else {
      exactKeys(raw.source, ['kind', 'directory'], `${String(raw.module)} workspace source`);
      if (raw.source.directory !== `../modules/${cacheKey}`) {
        fail(`${String(raw.module)} workspace source directory is not canonical`);
      }
      source = {
        kind: 'workspace',
        directory: raw.source.directory as string,
      };
    }
    const dependencies = validateDependencies(raw.dependencies, `${String(raw.module)} dependencies`);
    edgeCount += dependencies.length;
    if (edgeCount > MAX_GRAPH_EDGES) fail(`BlockKart snapshot exceeds the ${MAX_GRAPH_EDGES}-edge limit`);
    if (record.authority === 'lock') {
      const locked = {
        module: raw.module as string,
        version: raw.version as string,
        vo: raw.vo as string,
        release: raw.release as string,
        dependencies,
      };
      if (source.kind === 'registry') {
        modules.push({ ...locked, source });
      } else {
        modules.push({ ...locked, source });
      }
    } else {
      if (source.kind !== 'workspace') {
        fail(`${String(raw.module)} must use a workspace source under workspace authority`);
      }
      modules.push({
        module: raw.module as string,
        vo: raw.vo as string,
        source,
        dependencies,
      });
    }
  }

  const workspaceCount = modules.filter((module) => module.source.kind === 'workspace').length;
  if (workspaceCount === 0) {
    if (record.workspace !== undefined) fail('Registry-only BlockKart snapshot must omit workspace state');
  } else {
    exactKeys(record.workspace, ['file'], 'BlockKart snapshot workspace');
    if (record.workspace.file !== 'vo.work') fail('BlockKart snapshot workspace file must be vo.work');
  }

  validateSnapshotGraph({ authority: record.authority, rootDependencies, modules });
  const root = {
    module: BLOCKKART_MODULE,
    vo: record.root.vo as string,
    dependencies: rootDependencies,
  };
  if (record.authority === 'workspace') {
    if (workspaceCount !== modules.length || record.workspace === undefined) {
      fail('Workspace-authoritative BlockKart snapshot must contain only workspace modules');
    }
    return boundedSnapshot({
      schema_version: 2,
      mode: 'effective',
      authority: 'workspace',
      root,
      workspace: { file: 'vo.work' },
      modules: modules as QuickplayWorkspaceSnapshotModule[],
    });
  }
  return boundedSnapshot({
    schema_version: 2,
    mode: 'effective',
    authority: 'lock',
    root,
    ...(workspaceCount === 0 ? {} : { workspace: { file: 'vo.work' as const } }),
    modules: modules as QuickplayLockedSnapshotModule[],
  });
}

function boundedSnapshot<T extends QuickplayProjectSnapshot>(snapshot: T): T {
  const bytes = textEncoder.encode(`${canonicalProtocolJsonText(snapshot)}\n`).byteLength;
  if (bytes > MAX_METADATA_BYTES) {
    fail(`BlockKart ProjectSnapshot exceeds the ${MAX_METADATA_BYTES}-byte canonical JSON limit`);
  }
  return snapshot;
}

function validateSnapshotGraph(input: {
  authority: 'lock' | 'workspace';
  rootDependencies: QuickplaySnapshotDependency[];
  modules: QuickplaySnapshotModule[];
}): void {
  const modules = new Map(input.modules.map((module) => [module.module, module]));
  const incomingConstraints = new Map<string, ParsedConstraint[]>();
  let edgeCount = 0;
  for (const owner of [{ module: BLOCKKART_MODULE, dependencies: input.rootDependencies }, ...input.modules]) {
    for (const dependency of owner.dependencies) {
      edgeCount += 1;
      if (edgeCount > MAX_GRAPH_EDGES) fail(`BlockKart snapshot exceeds the ${MAX_GRAPH_EDGES}-edge limit`);
      if (dependency.module === owner.module) fail(`${owner.module} must not depend on itself`);
      if (dependency.module === BLOCKKART_MODULE) fail(`${owner.module} must not depend on the root module`);
      const selected = modules.get(dependency.module);
      if (!selected) fail(`${owner.module} has an open dependency on ${dependency.module}`);
      const constraint = parseConstraint(dependency.constraint, `${owner.module} dependency constraint`);
      if (input.authority === 'lock') {
        const locked = selected as QuickplayLockedSnapshotModule;
        if (!constraintSatisfies(
          constraint,
          parseVersion(locked.version, `${locked.module} selected version`),
        )) fail(`${locked.module}@${locked.version} does not satisfy ${dependency.constraint}`);
      } else {
        const incoming = incomingConstraints.get(dependency.module) ?? [];
        incoming.push(constraint);
        incomingConstraints.set(dependency.module, incoming);
      }
    }
  }
  if (input.authority === 'workspace') {
    for (const [module, constraints] of incomingConstraints) {
      const greatestLowerBound = constraints
        .map((constraint) => constraint.version)
        .reduce((left, right) => (compareVersions(left, right) >= 0 ? left : right));
      const stableCandidate: ParsedVersion = {
        major: greatestLowerBound.major,
        minor: greatestLowerBound.minor,
        patch: greatestLowerBound.patch,
        pre: [],
      };
      if (
        !constraints.every((constraint) => constraintSatisfies(constraint, greatestLowerBound))
        && (
          greatestLowerBound.pre.length === 0
          || !constraints.every((constraint) => constraintSatisfies(constraint, stableCandidate))
        )
      ) fail(`BlockKart snapshot has disjoint incoming constraints for ${module}`);
    }
  }
  const reachable = new Set<string>();
  const queue = input.rootDependencies.map((dependency) => dependency.module);
  for (let cursor = 0; cursor < queue.length; cursor += 1) {
    const module = queue[cursor];
    if (reachable.has(module)) continue;
    reachable.add(module);
    queue.push(...(modules.get(module)?.dependencies.map((dependency) => dependency.module) ?? []));
  }
  if (reachable.size !== modules.size) fail('BlockKart snapshot contains modules unreachable from the root');
}

export function renderQuickplayVoLock(snapshot: QuickplayProjectSnapshot): string | null {
  if (snapshot.authority !== 'lock') return null;
  let rendered = `version = 3\n\n[root]\nmodule = ${JSON.stringify(snapshot.root.module)}\nvo = ${JSON.stringify(snapshot.root.vo)}\n`;
  for (const module of snapshot.modules) {
    rendered += `\n[[module]]\npath = ${JSON.stringify(module.module)}\n`;
    rendered += `version = ${JSON.stringify(module.version)}\n`;
    rendered += `vo = ${JSON.stringify(module.vo)}\n`;
    rendered += `release = ${JSON.stringify(module.release)}\n`;
    if (module.dependencies.length === 0) {
      rendered += 'dependencies = []\n';
    } else {
      rendered += 'dependencies = [\n';
      for (const dependency of module.dependencies) {
        rendered += `  { module = ${JSON.stringify(dependency.module)}, constraint = ${JSON.stringify(dependency.constraint)} },\n`;
      }
      rendered += ']\n';
    }
  }
  return rendered;
}

export function renderQuickplayVoWork(snapshot: QuickplayProjectSnapshot): string | null {
  const members = snapshot.modules
    .filter((module) => module.source.kind === 'workspace')
    .map((module) => `../modules/${module.module.split('/').join('@')}`)
    .sort(compareUtf8);
  return members.length === 0
    ? null
    : `version = 1\nmembers = [${members.map((member) => JSON.stringify(member)).join(', ')}]\n`;
}

export function renderQuickplayRootVoMod(snapshot: QuickplayProjectSnapshot): string {
  return renderModuleIntent(snapshot.root);
}

function renderModuleIntent(owner: QuickplaySnapshotRoot | QuickplaySnapshotModule): string {
  let rendered = `module = ${JSON.stringify(owner.module)}\nvo = ${JSON.stringify(owner.vo)}\n`;
  if (owner.dependencies.length > 0) {
    rendered += '\n[dependencies]\n';
    for (const dependency of owner.dependencies) {
      rendered += `${JSON.stringify(dependency.module)} = ${JSON.stringify(dependency.constraint)}\n`;
    }
  }
  return rendered;
}

function assertCanonicalModuleIntent(
  text: string,
  owner: QuickplaySnapshotRoot | QuickplaySnapshotModule,
  label: string,
): void {
  const intent = renderModuleIntent(owner);
  if (!text.startsWith(intent)) fail(`${label} does not canonically match the ProjectSnapshot module intent`);
  const suffix = text.slice(intent.length);
  if (suffix !== '' && !suffix.startsWith('\n[')) {
    fail(`${label} has non-canonical root fields after the ProjectSnapshot module intent`);
  }
  if (/^\[dependencies\][\t ]*$/mu.test(suffix)) {
    fail(`${label} declares dependencies outside the ProjectSnapshot module intent`);
  }
}

function validateStaticFileDescriptors(value: unknown, label: string): QuickplayStaticFile[] {
  if (!Array.isArray(value) || value.length > MAX_PACKAGE_ENTRIES) fail(`${label} must be a bounded file array`);
  let totalBytes = 0;
  const files = value.map((raw, index) => {
    if (!isRecord(raw)) fail(`${label}[${index}] must be an object`);
    const hasText = typeof raw.content === 'string';
    const hasBase64 = typeof raw.contentBase64 === 'string';
    exactKeys(
      raw,
      hasText
        ? ['path', 'mode', 'size', 'digest', 'content']
        : ['path', 'mode', 'size', 'digest', 'contentBase64'],
      `${label}[${index}]`,
    );
    if (hasText === hasBase64) fail(`${label}[${index}] must carry exactly one payload`);
    validatePortableRelativePath(raw.path, `${label}[${index}].path`);
    if (raw.mode !== 0o644 && raw.mode !== 0o755) {
      fail(`${label}[${index}].mode must be 0644 or 0755`);
    }
    assertBoundSize(raw.size, `${label}[${index}].size`, MAX_QUICKPLAY_SOURCE_BYTES);
    assertDigest(raw.digest, `${label}[${index}].digest`);
    totalBytes += raw.size;
    if (!Number.isSafeInteger(totalBytes) || totalBytes > MAX_QUICKPLAY_SOURCE_BYTES) {
      fail(`${label} exceeds the 64 MiB Quickplay source byte limit`);
    }
    return raw as unknown as QuickplayStaticFile;
  });
  const portablePaths = new PortablePathTrie(
    MAX_PACKAGE_PATH_NODES,
    MAX_PACKAGE_PATH_KEY_BYTES,
  );
  for (const file of files) {
    portablePaths.insert(file.path, false, label);
  }
  for (let index = 1; index < files.length; index += 1) {
    if (compareUtf8(files[index - 1].path, files[index].path) >= 0) {
      fail(`${label} must be strictly sorted and unique`);
    }
  }
  return files;
}

function decodedFileText(bytes: ReadonlyMap<string, Uint8Array>, path: string, label: string): string {
  const value = bytes.get(path);
  if (!value) fail(`${label} is missing ${path}`);
  try {
    return textDecoder.decode(value);
  } catch {
    return fail(`${label} ${path} is not valid UTF-8`);
  }
}

export interface BlockKartVpakProducerBinding {
  producerDigest: string;
  producerIntent: unknown;
}

export async function validateBlockKartVpakProducerDigest(
  binding: BlockKartVpakProducerBinding,
): Promise<void> {
  const subtle = globalThis.crypto?.subtle;
  if (!subtle) fail('Web Crypto SHA-256 is required to verify the BlockKart VPAK producer');
  const intentBytes = textEncoder.encode(JSON.stringify(binding.producerIntent));
  const digest = new Uint8Array(await subtle.digest('SHA-256', intentBytes.slice().buffer));
  const actual = [...digest]
    .map((byte) => byte.toString(16).padStart(2, '0'))
    .join('');
  if (actual !== binding.producerDigest) {
    fail('BlockKart VPAK producerDigest does not bind the canonical producer intent');
  }
}

export function validateBlockKartVpakProducerBinding(
  files: readonly QuickplayStaticFile[],
  preparedBytes: ReadonlyMap<string, Uint8Array>,
): BlockKartVpakProducerBinding {
  const packFile = files.find((file) => file.path === BLOCKKART_VPAK_PATH);
  const producerFile = files.find((file) => file.path === BLOCKKART_VPAK_PRODUCER_PATH);
  if (!packFile || !producerFile) fail('BlockKart quickplay package is missing its VPAK producer binding');
  if (producerFile.size > MAX_METADATA_BYTES) {
    fail('BlockKart VPAK producer manifest exceeds the metadata byte limit');
  }
  const producerBytes = preparedBytes.get(BLOCKKART_VPAK_PRODUCER_PATH);
  if (!producerBytes) fail(`BlockKart project is missing ${BLOCKKART_VPAK_PRODUCER_PATH}`);
  const producer = parseBoundedJsonBytes(producerBytes, 'BlockKart VPAK producer manifest', {
    maxBytes: MAX_METADATA_BYTES,
    maxObjectKeyBytes: MAX_METADATA_BYTES,
  });
  const producerText = decodedFileText(
    preparedBytes,
    BLOCKKART_VPAK_PRODUCER_PATH,
    'BlockKart project',
  );
  const producerKeys = [
    'schemaVersion',
    'kind',
    'owner',
    'command',
    'pack',
    'inputs',
    'workspaceSourceInputCount',
    'payloadInputCount',
    'archiveEntryCount',
    'archiveEntries',
    'internalManifest',
    'upstream',
    'producerDigest',
  ] as const;
  exactKeys(producer, producerKeys, 'BlockKart VPAK producer manifest');
  if (
    Object.keys(producer).some((key, index) => key !== producerKeys[index])
    || producerText !== `${JSON.stringify(producer, null, 2)}\n`
  ) fail('BlockKart VPAK producer manifest is not canonical JSON');
  if (
    producer.schemaVersion !== 1
    || producer.kind !== 'blockkart.vpakProducerManifest'
    || producer.owner !== 'BlockKart'
    || JSON.stringify(producer.command) !== JSON.stringify(['vo', 'run', 'tools/pack_primitive_assets.vo'])
  ) fail('BlockKart VPAK producer identity is invalid');
  if (
    !Array.isArray(producer.inputs)
    || !Array.isArray(producer.archiveEntries)
    || !Array.isArray(producer.upstream)
    || !isRecord(producer.internalManifest)
    || !Number.isSafeInteger(producer.workspaceSourceInputCount)
    || (producer.workspaceSourceInputCount as number) <= 0
    || (producer.workspaceSourceInputCount as number) > producer.inputs.length
    || producer.payloadInputCount !== 37
    || producer.archiveEntryCount !== 37
    || producer.archiveEntries.length !== 37
  ) fail('BlockKart VPAK producer closure counts are invalid');
  exactKeys(producer.pack, ['path', 'sha256', 'size'], 'BlockKart VPAK producer pack');
  if (
    JSON.stringify(Object.keys(producer.pack)) !== JSON.stringify(['path', 'sha256', 'size'])
    || producer.pack.path !== BLOCKKART_VPAK_PATH
    || typeof producer.pack.sha256 !== 'string'
    || !/^[0-9a-f]{64}$/.test(producer.pack.sha256)
    || !Number.isSafeInteger(producer.pack.size)
    || (producer.pack.size as number) <= 0
    || producer.pack.size !== packFile.size
    || `sha256:${producer.pack.sha256}` !== packFile.digest
  ) fail('BlockKart VPAK producer does not bind the authenticated pack descriptor');
  if (
    typeof producer.producerDigest !== 'string'
    || !/^[0-9a-f]{64}$/.test(producer.producerDigest)
  ) fail('BlockKart VPAK producer digest is invalid');
  const { producerDigest, ...producerIntent } = producer;
  return { producerDigest, producerIntent };
}

export function validateBlockKartProjectProtocol(
  value: unknown,
  preparedBytes: ReadonlyMap<string, Uint8Array>,
): BlockKartProjectPackage {
  exactKeys(value, ['schemaVersion', 'name', 'module', 'baseCommit', 'snapshot', 'files'], 'BlockKart quickplay package');
  if (value.schemaVersion !== 2 || value.name !== 'BlockKart' || value.module !== BLOCKKART_MODULE) {
    fail('Invalid BlockKart quickplay package identity');
  }
  if (typeof value.baseCommit !== 'string' || !COMMIT_PATTERN.test(value.baseCommit)) {
    fail('Invalid BlockKart quickplay base commit');
  }
  const snapshot = validateSnapshot(value.snapshot);
  const files = validateStaticFileDescriptors(value.files, 'BlockKart project files');
  assertPortableFileClosure(
    files.map((file) => `${QUICKPLAY_PROJECT_VFS_ROOT}/${file.path}`),
    'BlockKart project final VFS closure',
    true,
  );
  for (const file of files) {
    const components = file.path.split('/');
    const rootKey = portableCaseKey(components[0] ?? '');
    const basename = portableCaseKey(components[components.length - 1] ?? '');
    if (
      (components.length === 1 && PROJECT_REMOVED_PROTOCOL_FILES.has(rootKey))
      || (basename === 'vo.mod' && file.path !== 'vo.mod')
      || (components.length === 1 && PROJECT_ROOT_CONTROL_FILES.has(rootKey) && file.path !== rootKey)
    ) fail(`BlockKart project contains reserved protocol state: ${file.path}`);
  }
  if (
    preparedBytes.size !== files.length
    || files.some((file) => !preparedBytes.has(file.path))
  ) fail('Prepared BlockKart project files differ from the authenticated descriptor closure');
  for (const required of [
    'main.vo',
    'vo.mod',
    BLOCKKART_VPAK_PATH,
    BLOCKKART_VPAK_PRODUCER_PATH,
  ]) {
    if (!preparedBytes.has(required)) fail(`BlockKart quickplay package is missing ${required}`);
  }
  validateBlockKartVpakProducerBinding(files, preparedBytes);
  if (decodedFileText(preparedBytes, 'vo.mod', 'BlockKart project') !== renderQuickplayRootVoMod(snapshot)) {
    fail('BlockKart vo.mod does not match the snapshot root intent');
  }
  const expectedLock = renderQuickplayVoLock(snapshot);
  if (expectedLock === null) {
    if (preparedBytes.has('vo.lock')) fail('Workspace-authoritative BlockKart snapshot must omit vo.lock');
  } else if (decodedFileText(preparedBytes, 'vo.lock', 'BlockKart project') !== expectedLock) {
    fail('BlockKart vo.lock is not canonical v3 for its ProjectSnapshot');
  }
  const expectedWork = renderQuickplayVoWork(snapshot);
  if (expectedWork === null) {
    if (preparedBytes.has('vo.work')) fail('Registry-only BlockKart snapshot must omit vo.work');
  } else if (decodedFileText(preparedBytes, 'vo.work', 'BlockKart project') !== expectedWork) {
    fail('BlockKart vo.work does not match the workspace module closure');
  }
  return {
    schemaVersion: 2,
    name: 'BlockKart',
    module: BLOCKKART_MODULE,
    baseCommit: value.baseCommit,
    snapshot,
    files,
  };
}

function artifactIdentity(artifact: { kind: string; target: string; name: string }): string {
  return `${artifact.kind}\0${artifact.target}\0${artifact.name}`;
}

function compareArtifacts(
  left: { kind: string; target: string; name: string },
  right: { kind: string; target: string; name: string },
): number {
  return compareUtf8(left.kind, right.kind)
    || compareUtf8(left.target, right.target)
    || compareUtf8(left.name, right.name);
}

function validateBrowserArtifacts(value: unknown, label: string): QuickplayStaticArtifact[] {
  if (!Array.isArray(value) || value.length > MAX_MODULE_ARTIFACTS) fail(`${label} must be a bounded array`);
  const artifacts = value.map((raw, index) => {
    exactKeys(raw, ['kind', 'target', 'name', 'size', 'digest', 'path', 'url'], `${label}[${index}]`);
    if (raw.kind !== 'extension-wasm' && raw.kind !== 'extension-js-glue') {
      fail(`${label}[${index}] uses an artifact kind unavailable in browsers`);
    }
    if (raw.target !== 'wasm32-unknown-unknown') fail(`${label}[${index}] has an invalid browser target`);
    validatePortableComponent(raw.name, `${label}[${index}].name`);
    assertPositiveSize(raw.size, `${label}[${index}].size`, MAX_MODULE_ARTIFACT_BYTES);
    assertDigest(raw.digest, `${label}[${index}].digest`);
    validatePortableRelativePath(raw.path, `${label}[${index}].path`);
    assertString(raw.url, `${label}[${index}].url`);
    return raw as unknown as QuickplayStaticArtifact;
  });
  for (let index = 1; index < artifacts.length; index += 1) {
    if (compareArtifacts(artifacts[index - 1], artifacts[index]) >= 0) {
      fail(`${label} must be sorted and unique`);
    }
  }
  return artifacts;
}

function fileByPath(files: QuickplayStaticFile[], path: string, label: string): QuickplayStaticFile {
  const file = files.find((entry) => entry.path === path);
  if (!file) fail(`${label} is missing ${path}`);
  return file;
}

function fileText(file: QuickplayStaticFile, label: string): string {
  if (typeof file.content !== 'string' || file.contentBase64 !== undefined) {
    fail(`${label} must use its canonical UTF-8 text payload`);
  }
  if (textEncoder.encode(file.content).byteLength !== file.size) fail(`${label} size does not match its payload`);
  return file.content;
}

function parseJsonObject(file: QuickplayStaticFile, label: string): JsonRecord {
  const text = fileText(file, label);
  let value: unknown;
  try {
    value = JSON.parse(text);
  } catch {
    return fail(`${label} is invalid JSON`);
  }
  if (!isRecord(value)) fail(`${label} must contain a JSON object`);
  return value;
}

function assertCanonicalJson(file: QuickplayStaticFile, value: unknown, label: string): void {
  if (fileText(file, label) !== `${canonicalProtocolJsonText(value)}\n`) {
    fail(`${label} must use canonical pretty JSON with canonical field order`);
  }
}

function validateReleaseArtifact(value: unknown, label: string): ReleaseArtifact {
  exactKeys(value, ['kind', 'target', 'name', 'size', 'digest'], label);
  if (!['extension-native', 'extension-wasm', 'extension-js-glue'].includes(value.kind as string)) {
    fail(`${label} has an invalid artifact kind`);
  }
  validatePortableComponent(value.target, `${label}.target`);
  validatePortableComponent(value.name, `${label}.name`);
  if ((value.kind === 'extension-wasm' || value.kind === 'extension-js-glue') && value.target !== 'wasm32-unknown-unknown') {
    fail(`${label} has an invalid browser artifact target`);
  }
  if (
    value.kind === 'extension-native'
    && ((value.target as string).split('-').length < 3
      || (value.target as string).split('-').some((part) => !/^[a-z0-9_.]+$/.test(part))
      || (value.target as string).startsWith('wasm32-')
      || (value.target as string).startsWith('wasm64-'))
  ) fail(`${label} has an invalid native artifact target`);
  assertPositiveSize(value.size, `${label}.size`, MAX_MODULE_ARTIFACT_BYTES);
  assertDigest(value.digest, `${label}.digest`);
  return value as unknown as ReleaseArtifact;
}

function validateRegistryModule(
  modulePack: BlockKartDependencyModulePackage,
  snapshotModule: QuickplayRegistrySnapshotModule,
): void {
  const label = snapshotModule.module;
  const releaseFile = fileByPath(modulePack.files, 'vo.release.json', label);
  if (releaseFile.mode !== 0o644) fail(`${label} release metadata must have mode 0644`);
  assertPositiveSize(releaseFile.size, `${label} release metadata size`, MAX_METADATA_BYTES);
  if (releaseFile.digest !== snapshotModule.release) fail(`${label} release digest differs from the ProjectSnapshot`);
  const release = parseJsonObject(releaseFile, `${label} vo.release.json`);
  exactKeys(
    release,
    ['schema_version', 'module', 'version', 'commit', 'vo', 'dependencies', 'source', 'package', 'artifacts'],
    `${label} release`,
  );
  if (
    release.schema_version !== 2
    || release.module !== snapshotModule.module
    || release.version !== snapshotModule.version
    || release.vo !== snapshotModule.vo
    || typeof release.commit !== 'string'
    || !COMMIT_PATTERN.test(release.commit)
  ) fail(`${label} release identity differs from the ProjectSnapshot`);
  const dependencies = validateDependencies(release.dependencies, `${label} release dependencies`);
  if (JSON.stringify(dependencies) !== JSON.stringify(snapshotModule.dependencies)) {
    fail(`${label} release dependency graph differs from the ProjectSnapshot`);
  }
  exactKeys(release.source, ['name', 'size', 'digest'], `${label} release source`);
  if (release.source.name !== 'source.tar.gz') fail(`${label} release source must be source.tar.gz`);
  assertPositiveSize(
    release.source.size,
    `${label} release source size`,
    MAX_RELEASE_SOURCE_ARCHIVE_BYTES,
  );
  assertDigest(release.source.digest, `${label} release source digest`);
  exactKeys(release.package, ['size', 'digest'], `${label} release package`);
  assertPositiveSize(release.package.size, `${label} release package size`, MAX_METADATA_BYTES);
  assertDigest(release.package.digest, `${label} release package digest`);
  if (!Array.isArray(release.artifacts) || release.artifacts.length > MAX_MODULE_ARTIFACTS) {
    fail(`${label} release artifacts must be a bounded array`);
  }
  const releaseArtifacts = release.artifacts.map((artifact, index) => (
    validateReleaseArtifact(artifact, `${label} release artifacts[${index}]`)
  ));
  for (let index = 1; index < releaseArtifacts.length; index += 1) {
    if (compareArtifacts(releaseArtifacts[index - 1], releaseArtifacts[index]) >= 0) {
      fail(`${label} release artifacts must be sorted and unique`);
    }
  }
  assertCanonicalJson(releaseFile, {
    schema_version: 2,
    module: release.module,
    version: release.version,
    commit: release.commit,
    vo: release.vo,
    dependencies,
    source: {
      name: release.source.name,
      size: release.source.size,
      digest: release.source.digest,
    },
    package: {
      size: release.package.size,
      digest: release.package.digest,
    },
    artifacts: releaseArtifacts.map((artifact) => ({
      kind: artifact.kind,
      target: artifact.target,
      name: artifact.name,
      size: artifact.size,
      digest: artifact.digest,
    })),
  }, `${label} vo.release.json`);

  const packageFile = fileByPath(modulePack.files, 'vo.package.json', label);
  if (packageFile.mode !== 0o644) fail(`${label} package metadata must have mode 0644`);
  if (packageFile.size !== release.package.size || packageFile.digest !== release.package.digest) {
    fail(`${label} package metadata differs from its release binding`);
  }
  const packageManifest = parseJsonObject(packageFile, `${label} vo.package.json`);
  exactKeys(packageManifest, ['schema_version', 'files'], `${label} package manifest`);
  if (packageManifest.schema_version !== 1) fail(`${label} package manifest schema must be 1`);
  if (
    !Array.isArray(packageManifest.files)
    || packageManifest.files.length === 0
    || packageManifest.files.length > MAX_PACKAGE_ENTRIES
  ) {
    fail(`${label} package manifest files must be a non-empty bounded array`);
  }
  let previousPath = '';
  let extractedBytes = packageFile.size;
  const canonicalPackageFiles: Array<{
    path: string;
    mode: 'regular' | 'executable';
    size: number;
    digest: string;
  }> = [];
  const expectedPaths = new Set(['.vo-source-digest', '.vo-version', 'vo.package.json', 'vo.release.json']);
  for (const [index, raw] of packageManifest.files.entries()) {
    exactKeys(raw, ['path', 'mode', 'size', 'digest'], `${label} package files[${index}]`);
    validatePortableRelativePath(raw.path, `${label} package files[${index}].path`);
    if (raw.mode !== 'regular' && raw.mode !== 'executable') {
      fail(`${label} package files[${index}].mode is invalid`);
    }
    assertBoundSize(
      raw.size,
      `${label} package files[${index}].size`,
      MAX_PACKAGE_SOURCE_FILE_BYTES,
    );
    if (
      ((raw.path as string) === 'vo.mod' || (raw.path as string).endsWith('.vo'))
      && (raw.size as number) > MAX_METADATA_BYTES
    ) fail(`${label} package files[${index}].size exceeds the Vo text-file limit`);
    assertDigest(raw.digest, `${label} package files[${index}].digest`);
    const components = (raw.path as string).split('/');
    const first = portableCaseKey(components[0]);
    const basename = portableCaseKey(components[components.length - 1]);
    if (
      ['artifacts', '.vo-version', '.vo-source-digest', 'vo.release.json', 'vo.package.json', 'source.tar.gz'].includes(first)
      || (components.length === 1 && ['vo.lock', 'vo.work', 'vo.sum', 'vo.web.json', '.vo-project.lock', '.vo-project.transaction'].includes(basename))
      || (basename === 'vo.mod' && raw.path !== 'vo.mod')
    ) fail(`${label} package files[${index}].path is reserved by the module protocol`);
    if (compareUtf8(previousPath, raw.path) >= 0) fail(`${label} package files must be sorted and unique`);
    previousPath = raw.path;
    extractedBytes += raw.size as number;
    if (!Number.isSafeInteger(extractedBytes) || extractedBytes > MAX_PACKAGE_EXTRACTED_BYTES) {
      fail(`${label} package exceeds the 128 MiB protocol extracted-source limit`);
    }
    const installed = fileByPath(modulePack.files, raw.path, label);
    const installedMode = raw.mode === 'executable' ? 0o755 : 0o644;
    if (
      installed.mode !== installedMode
      || installed.size !== raw.size
      || installed.digest !== raw.digest
    ) {
      fail(`${label} ${raw.path} differs from vo.package.json`);
    }
    expectedPaths.add(raw.path);
    canonicalPackageFiles.push({
      path: raw.path as string,
      mode: raw.mode,
      size: raw.size as number,
      digest: raw.digest as string,
    });
  }
  assertCanonicalJson(packageFile, {
    schema_version: 1,
    files: canonicalPackageFiles,
  }, `${label} vo.package.json`);
  const voModEntry = packageManifest.files.find((file) => isRecord(file) && file.path === 'vo.mod');
  if (!voModEntry || voModEntry.mode !== 'regular' || voModEntry.size === 0) {
    fail(`${label} package manifest is missing a non-empty regular vo.mod`);
  }
  if (
    modulePack.files.length !== expectedPaths.size
    || modulePack.files.some((file) => !expectedPaths.has(file.path))
  ) fail(`${label} files differ from the authenticated release/package closure`);
  const moduleVoMod = fileText(fileByPath(modulePack.files, 'vo.mod', label), `${label} vo.mod`);
  assertCanonicalModuleIntent(moduleVoMod, snapshotModule, `${label} registry vo.mod`);
  const versionFile = fileByPath(modulePack.files, '.vo-version', label);
  if (versionFile.mode !== 0o644) fail(`${label} version marker must have mode 0644`);
  if (fileText(versionFile, `${label} version marker`) !== `${snapshotModule.version}\n`) {
    fail(`${label} version marker differs from the ProjectSnapshot`);
  }
  const sourceDigestFile = fileByPath(modulePack.files, '.vo-source-digest', label);
  if (sourceDigestFile.mode !== 0o644) fail(`${label} source marker must have mode 0644`);
  if (fileText(sourceDigestFile, `${label} source marker`) !== `${release.source.digest as string}\n`) {
    fail(`${label} source marker differs from its release`);
  }

  const browserReleaseArtifacts = releaseArtifacts.filter((artifact) => artifact.kind !== 'extension-native');
  if (browserReleaseArtifacts.length !== modulePack.artifacts.length) {
    fail(`${label} browser artifacts differ from its authenticated release`);
  }
  for (let index = 0; index < browserReleaseArtifacts.length; index += 1) {
    const expected = browserReleaseArtifacts[index];
    const actual = modulePack.artifacts[index];
    if (
      artifactIdentity(actual) !== artifactIdentity(expected)
      || actual.size !== expected.size
      || actual.digest !== expected.digest
    ) fail(`${label} browser artifact differs from its authenticated release`);
  }
}

function validateWorkspaceModule(
  modulePack: BlockKartDependencyModulePackage,
  snapshotModule: QuickplayWorkspaceSnapshotModule | QuickplayLockedWorkspaceSnapshotModule,
): void {
  const voMod = fileText(fileByPath(modulePack.files, 'vo.mod', snapshotModule.module), `${snapshotModule.module} vo.mod`);
  assertCanonicalModuleIntent(voMod, snapshotModule, `${snapshotModule.module} workspace vo.mod`);
  for (const file of modulePack.files) {
    if (isReservedWorkspacePath(file.path)) {
      fail(`${snapshotModule.module} workspace contains reserved protocol state: ${file.path}`);
    }
  }
}

export function validateBlockKartDependenciesProtocol(
  value: unknown,
  snapshot: QuickplayProjectSnapshot,
  snapshotDigest: string,
): BlockKartDepsPackage {
  const projectSnapshot = validateSnapshot(snapshot);
  exactKeys(value, ['schemaVersion', 'name', 'snapshotDigest', 'modules'], 'BlockKart dependency package');
  if (value.schemaVersion !== 2 || value.name !== 'BlockKart dependencies') {
    fail('Invalid BlockKart dependency package identity');
  }
  assertDigest(value.snapshotDigest, 'BlockKart dependency snapshot digest');
  if (value.snapshotDigest !== snapshotDigest) fail('BlockKart project and dependency snapshot digests differ');
  if (
    !Array.isArray(value.modules)
    || value.modules.length !== projectSnapshot.modules.length
    || value.modules.length > MAX_MODULES
  ) fail('BlockKart dependency modules differ from the ProjectSnapshot');

  const modules: BlockKartDependencyModulePackage[] = [];
  let totalEntries = 0;
  let totalArtifactBytes = 0;
  const aggregateInstallClosure: string[] = [];
  for (const [index, raw] of value.modules.entries()) {
    if (!isRecord(raw) || (raw.source !== 'registry' && raw.source !== 'workspace')) {
      fail(`BlockKart dependency modules[${index}] has an invalid source kind`);
    }
    exactKeys(
      raw,
      raw.source === 'registry'
        ? ['source', 'module', 'version', 'cacheDir', 'files', 'artifacts']
        : ['source', 'module', 'cacheDir', 'files', 'artifacts'],
      `BlockKart dependency modules[${index}]`,
    );
    const snapshotModule = projectSnapshot.modules[index];
    if (raw.module !== snapshotModule.module) {
      fail('BlockKart dependency modules must exactly match ProjectSnapshot order and source selection');
    }
    if (raw.source !== snapshotModule.source.kind) {
      fail('BlockKart dependency modules must exactly match ProjectSnapshot order and source selection');
    }
    const cacheKey = snapshotModule.module.split('/').join('@');
    let expectedCacheDir: string;
    let registrySnapshot: QuickplayRegistrySnapshotModule | null = null;
    let workspaceSnapshot:
      | QuickplayWorkspaceSnapshotModule
      | QuickplayLockedWorkspaceSnapshotModule
      | null = null;
    if (raw.source === 'registry') {
      if (!isRegistrySnapshotModule(snapshotModule)) {
        fail(`${snapshotModule.module} registry package has no registry snapshot source`);
      }
      registrySnapshot = snapshotModule;
      expectedCacheDir = `${cacheKey}/${registrySnapshot.version}`;
    } else {
      if (isRegistrySnapshotModule(snapshotModule)) {
        fail(`${snapshotModule.module} workspace package has a registry snapshot source`);
      }
      workspaceSnapshot = snapshotModule;
      expectedCacheDir = `workspace/${cacheKey}`;
    }
    if (raw.cacheDir !== expectedCacheDir) fail(`${snapshotModule.module} cacheDir differs from the ProjectSnapshot`);
    if (registrySnapshot !== null && raw.version !== registrySnapshot.version) {
      fail(`${snapshotModule.module} version differs from the ProjectSnapshot`);
    }
    const files = validateStaticFileDescriptors(raw.files, `${snapshotModule.module} files`);
    const artifacts = validateBrowserArtifacts(raw.artifacts, `${snapshotModule.module} artifacts`);
    totalEntries += files.length + artifacts.length * (raw.source === 'workspace' ? 2 : 1);
    totalArtifactBytes += artifacts.reduce((total, artifact) => (
      total + artifact.size * (raw.source === 'workspace' ? 2 : 1)
    ), 0);
    if (!Number.isSafeInteger(totalEntries) || totalEntries > MAX_PACKAGE_ENTRIES) {
      fail(`BlockKart dependency package exceeds the ${MAX_PACKAGE_ENTRIES}-entry limit`);
    }
    if (!Number.isSafeInteger(totalArtifactBytes) || totalArtifactBytes > MAX_TOTAL_BYTES) {
      fail('BlockKart dependency artifacts exceed the aggregate byte limit');
    }
    const installClosure = files.map((file) => file.path);
    for (const artifact of artifacts) {
      const canonicalArtifactPath = artifactCachePath(artifact);
      if (raw.source === 'registry') {
        if (artifact.path !== canonicalArtifactPath) {
          fail(`${snapshotModule.module} registry artifact path differs from the module cache protocol`);
        }
      } else {
        if (isReservedWorkspacePath(artifact.path)) {
          fail(`${snapshotModule.module} workspace artifact install path is reserved`);
        }
      }
      if (artifact.url !== quickplayArtifactUrl(raw.cacheDir as string, artifact)) {
        fail(`${snapshotModule.module} artifact URL differs from its canonical Quickplay path`);
      }
      installClosure.push(artifact.path);
      if (raw.source === 'workspace') installClosure.push(canonicalArtifactPath);
    }
    assertPortableFileClosure(installClosure, `${snapshotModule.module} install closure`);
    const moduleVfsRoot = raw.source === 'registry'
      ? String(raw.cacheDir)
      : `${QUICKPLAY_WORKSPACE_MODULE_VFS_ROOT}/${cacheKey}`;
    aggregateInstallClosure.push(
      ...installClosure.map((relative) => `${moduleVfsRoot}/${relative}`),
    );
    const modulePack: BlockKartDependencyModulePackage = {
      source: raw.source,
      module: raw.module as string,
      ...(raw.source === 'registry' ? { version: raw.version as string } : {}),
      cacheDir: raw.cacheDir as string,
      files,
      artifacts,
    };
    if (registrySnapshot !== null) {
      validateRegistryModule(modulePack, registrySnapshot);
    } else if (workspaceSnapshot !== null) {
      validateWorkspaceModule(modulePack, workspaceSnapshot);
    } else {
      fail(`${snapshotModule.module} dependency package has no validated snapshot source`);
    }
    modules.push(modulePack);
  }
  assertPortableFileClosure(
    aggregateInstallClosure,
    'BlockKart dependency aggregate install closure',
    true,
  );
  return {
    schemaVersion: 2,
    name: 'BlockKart dependencies',
    snapshotDigest,
    modules,
  };
}
