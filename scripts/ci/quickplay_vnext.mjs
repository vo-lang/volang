import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import {
  closeSync,
  constants as fsConstants,
  existsSync,
  fstatSync,
  lstatSync,
  mkdirSync,
  mkdtempSync,
  openSync,
  opendirSync,
  readSync,
  readdirSync,
  realpathSync,
  rmSync,
  writeFileSync,
} from 'node:fs';
import path from 'node:path';
import { parseBoundedJsonBytes } from './bounded_json.mjs';
import {
  artifactCachePath,
  artifactOutputRelativePath,
  modulePathCacheKey,
  moduleCacheDir,
  quickplayArtifactUrl,
  validateArtifactIdentity,
  validateExactVersion,
  validateModulePath,
  validatePortableRelativePath,
} from './quickplay_artifact_paths.mjs';
import {
  VO_CLI_GUEST_ENVIRONMENT,
  assertVoCliCargoConfigBoundary,
} from './quickplay_cli_producer_contract.mjs';
import { PortablePathTrie, portableCaseKey } from './portable_path_key.mjs';
import { cleanGitEnvironment } from './repo_roots.mjs';
import {
  assertBindgenWasmExtensionV3,
  assertStandaloneWasmExtensionV3,
  extensionExportCatalogFromVoFiles,
  VOGUI_STANDALONE_HOST_IMPORTS_V3,
} from './wasm_protocol_v3.mjs';

export { parseBoundedJsonBytes } from './bounded_json.mjs';

export const QUICKPLAY_PROJECT_SCHEMA = 2;
export const QUICKPLAY_DEPS_SCHEMA = 2;
export const QUICKPLAY_GENERATOR_VERSION = 7;
export const SNAPSHOT_SCHEMA = 2;
export const RELEASE_SCHEMA = 2;
export const PACKAGE_SCHEMA = 1;
export const SOURCE_ARCHIVE_ASSET_NAME = 'source.tar.gz';
export const MAX_SOURCE_ARCHIVE_BYTES = 64 * 1024 * 1024;
export const MAX_SOURCE_ARCHIVE_ENTRY_BYTES = 64 * 1024 * 1024;
export const MAX_EXTRACTED_SOURCE_BYTES = 128 * 1024 * 1024;
export const MAX_MODULE_ARTIFACT_BYTES = 256 * 1024 * 1024;
export const MAX_PROTOCOL_METADATA_BYTES = 16 * 1024 * 1024;
export const MAX_BLOCKKART_VPAK_BYTES = 64 * 1024 * 1024;
export const BLOCKKART_VPAK_PATH = 'assets/blockkart.vpak';
export const BLOCKKART_VPAK_PRODUCER_PATH = 'assets/blockkart.vpak.provenance.json';

export const QUICKPLAY_ARTIFACT_INPUTS = Object.freeze([
  '.cargo/config.toml',
  '.gitattributes',
  'Cargo.lock',
  'Cargo.toml',
  'apps/studio/scripts/package_blockkart_quickplay.mjs',
  'apps/studio/package.json',
  'scripts/ci/bounded_json.mjs',
  'scripts/ci/bounded_json.d.mts',
  'scripts/ci/quickplay_vnext.mjs',
  'scripts/ci/wasm_protocol_v3.mjs',
  'scripts/ci/quickplay_validate.mjs',
  'scripts/ci/quickplay_artifact_paths.mjs',
  'scripts/ci/portable_path_key.mjs',
  'scripts/ci/unicode_casefold_data.mjs',
  'scripts/ci/repo_roots.mjs',
  'scripts/ci/source_bound_evidence.mjs',
  'scripts/ci/blockkart_vpak_build.mjs',
  'scripts/ci/quickplay_cli_producer_contract.mjs',
  'scripts/ci/utf8_order.mjs',
  'scripts/ci/vogui_current_wasm.mjs',
  'scripts/ci/voplay_current_wasm.mjs',
  'apps/studio/src/lib/backend/web_backend.ts',
  'apps/studio/src/lib/quickplay_package_validation.ts',
  'apps/studio/src/lib/quickplay.ts',
  'eng/artifacts.toml',
  'eng/project.toml',
  'eng/tasks.toml',
  'external:BlockKart',
  'first-party:vogui',
  'first-party:vopack',
  'first-party:voplay',
  'cmd/vo/**',
  'cmd/vo-dev/**',
  'lang/crates/**',
  'lang/stdlib/**',
  'rust-toolchain.toml',
]);

const QUICKPLAY_SOURCE_GIT_PATHSPECS = Object.freeze(
  QUICKPLAY_ARTIFACT_INPUTS
    .filter((input) => !input.startsWith('external:') && !input.startsWith('first-party:'))
    .map((input) => (input.endsWith('/**') ? `:(glob)${input}` : `:(literal)${input}`)),
);

const MAX_JSON_BYTES = MAX_PROTOCOL_METADATA_BYTES;
const MAX_PACKAGE_JSON_BYTES = 128 * 1024 * 1024;
const MAX_JSON_DEPTH = 127;
const MAX_JSON_TOKENS = 1_000_000;
const MAX_JSON_OBJECT_KEYS = 200_000;
const MAX_FILE_BYTES = 256 * 1024 * 1024;
const MAX_VO_BUILD_OUTPUT_BYTES = 64 * 1024 * 1024;
const MAX_VO_BINARY_BYTES = 256 * 1024 * 1024;
export const CURRENT_VO_CLI_BUILD_TIMEOUT_MS = 60 * 60 * 1000;
const VO_INVOCATION_TIMEOUT_MS = 10 * 60 * 1000;
const MAX_TOTAL_BYTES = 512 * 1024 * 1024;
const MAX_QUICKPLAY_SOURCE_BYTES = 64 * 1024 * 1024;
const MAX_STATIC_ENTRIES = 20_000;
const MAX_STATIC_PATH_NODES = 100_000;
const MAX_STATIC_PATH_KEY_BYTES = 16 * 1024 * 1024;
const MAX_STATIC_MODULES = 10_000;
const MAX_MODULE_ARTIFACTS = 997;
const MAX_PROJECT_GRAPH_EDGES = 100_000;
const MAX_WALK_ENTRIES = 100_000;
const MAX_PATH_DEPTH = 256;
const MAX_PATH_BYTES = 4 * 1024;
// The validator selftest keeps these final VFS roots identical to Studio.
export const QUICKPLAY_PROJECT_VFS_ROOT = 'workspace/.volang/apps/studio/sessions/quickplay/BlockKart/current';
export const QUICKPLAY_WORKSPACE_MODULE_VFS_ROOT = 'workspace/.volang/apps/studio/sessions/quickplay/BlockKart/modules';
const MAX_U64 = 18_446_744_073_709_551_615n;
const DIGEST = /^sha256:[0-9a-f]{64}$/;
const UTF8 = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true });
const REMOVED_PROTOCOL_FILES = new Set([
  '.vo-source-digest',
  '.vo-version',
  'vo.package.json',
  'vo.release.json',
  'source.tar.gz',
  'vo.sum',
  'vo.web.json',
]);
const INTERNAL_PROJECT_STATE_FILES = new Set([
  '.vo-project.lock',
  '.vo-project.transaction',
]);
const SNAPSHOT_CONTROL_FILES = new Set([
  ...REMOVED_PROTOCOL_FILES,
  ...INTERNAL_PROJECT_STATE_FILES,
]);
const WORKSPACE_RESERVED_FILES = new Set([
  ...SNAPSHOT_CONTROL_FILES,
  'vo.lock',
  'vo.work',
]);
const PROJECT_RESERVED_FILES = new Set([
  '.vo-source-digest',
  '.vo-version',
  'vo.lock',
  'vo.mod',
  'vo.package.json',
  'vo.release.json',
  'source.tar.gz',
  'vo.sum',
  'vo.work',
  'vo.web.json',
  ...INTERNAL_PROJECT_STATE_FILES,
]);
const PROJECT_ROOT_CONTROL_FILES = new Set(['vo.lock', 'vo.mod', 'vo.work']);
const ROOT_MODULE_PROTOCOL_FILES = new Set(['vo.lock', 'vo.mod', 'vo.work']);
const DEFAULT_CURRENT_SOURCE_IGNORED_PATHSPECS = Object.freeze([
  ':(glob,icase)**/vo.mod',
  ':(glob)**/*.vo',
  ':(literal)vo.lock',
  ':(literal)vo.work',
]);

function hasReservedRootPath(relative, reserved = WORKSPACE_RESERVED_FILES) {
  const components = relative.split('/');
  return components.length === 1 && reserved.has(portableCaseKey(components[0]));
}

function assertCanonicalRootProtocolFiles(root, label) {
  const canonicalRoot = realpathSync.native(path.resolve(root));
  const before = lstatSync(canonicalRoot, { bigint: true });
  assertQuickplay(
    before.isDirectory() && !before.isSymbolicLink(),
    `${label} root must be a real directory`,
  );
  const directory = opendirSync(canonicalRoot);
  let entries = 0;
  try {
    for (let entry = directory.readSync(); entry !== null; entry = directory.readSync()) {
      entries += 1;
      assertQuickplay(
        entries <= MAX_WALK_ENTRIES,
        `${label} root contains more than ${MAX_WALK_ENTRIES} entries`,
      );
      const key = portableCaseKey(entry.name);
      if (!ROOT_MODULE_PROTOCOL_FILES.has(key)) continue;
      assertQuickplay(
        entry.name === key,
        `${label} root protocol file must use the exact portable spelling ${key}: ${entry.name}`,
      );
      assertQuickplay(
        entry.isFile() && !entry.isSymbolicLink(),
        `${label} root protocol path must be a regular file: ${entry.name}`,
      );
    }
  } finally {
    directory.closeSync();
  }
  const after = lstatSync(canonicalRoot, { bigint: true });
  assertQuickplay(
    sameStableFile(before, after) && realpathSync.native(canonicalRoot) === canonicalRoot,
    `${label} root changed while protocol files were enumerated`,
  );
}

function hasNestedModuleMarker(relative) {
  return path.posix.dirname(relative) !== '.' && isPortableModuleManifestPath(relative);
}

function hasReservedWorkspacePath(relative) {
  const first = portableCaseKey(relative.split('/')[0]);
  return first === 'artifacts'
    || hasReservedRootPath(relative)
    || hasNestedModuleMarker(relative);
}

export function assertQuickplay(condition, message) {
  if (!condition) throw new Error(message);
}

export function selectModuleCacheRoot(defaultRoot, configured = process.env.VO_MOD_CACHE) {
  assertQuickplay(
    typeof defaultRoot === 'string' && defaultRoot.length > 0,
    'default module-cache root must be a non-empty path',
  );
  const fallback = path.resolve(defaultRoot);
  if (configured === undefined) return fallback;
  assertQuickplay(typeof configured === 'string', 'VO_MOD_CACHE must be a path string');
  assertQuickplay(
    configured.length > 0,
    'VO_MOD_CACHE must not be empty; unset it to use the task-local default',
  );
  assertQuickplay(
    path.isAbsolute(configured),
    `VO_MOD_CACHE must be an absolute path, found ${configured}`,
  );
  return configured;
}

export function sha256Digest(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

export function quickplayWorkspaceFile(modules) {
  assertQuickplay(Array.isArray(modules), 'Quickplay workspace modules must be an array');
  const members = modules
    .filter((module) => module?.source?.kind === 'workspace')
    .map((module) => {
      const cacheComponent = modulePathCacheKey(module.module).cacheComponent;
      return `../modules/${cacheComponent}`;
    })
    .sort(compareUtf8);
  if (members.length === 0) return null;
  return `version = 1\nmembers = [${members.map((member) => JSON.stringify(member)).join(', ')}]\n`;
}

function compareUtf8(left, right) {
  return Buffer.compare(Buffer.from(left, 'utf8'), Buffer.from(right, 'utf8'));
}

function compareArtifactIdentity(left, right) {
  return compareUtf8(left.kind, right.kind)
    || compareUtf8(left.target, right.target)
    || compareUtf8(left.name, right.name);
}

function canonicalModIntent(owner) {
  let rendered = `module = ${JSON.stringify(owner.module)}\nvo = ${JSON.stringify(owner.vo)}\n`;
  if (owner.dependencies.length > 0) {
    rendered += '\n[dependencies]\n';
    for (const dependency of owner.dependencies) {
      rendered += `${JSON.stringify(dependency.module)} = ${JSON.stringify(dependency.constraint)}\n`;
    }
  }
  return rendered;
}

function assertCanonicalModIntent(text, owner, label) {
  const intent = canonicalModIntent(owner);
  assertQuickplay(text.startsWith(intent), `${label} does not canonically match the ProjectSnapshot module intent`);
  const suffix = text.slice(intent.length);
  assertQuickplay(
    suffix === '' || suffix.startsWith('\n['),
    `${label} has non-canonical root fields after the ProjectSnapshot module intent`,
  );
  assertQuickplay(
    !/^\[dependencies\][\t ]*$/mu.test(suffix),
    `${label} declares dependencies outside the ProjectSnapshot module intent`,
  );
}

const MODULE_MANIFEST_PORTABLE_BASENAME = portableCaseKey('vo.mod');

export function isPortableModuleManifestPath(relative) {
  return typeof relative === 'string'
    && portableCaseKey(path.posix.basename(relative)) === MODULE_MANIFEST_PORTABLE_BASENAME;
}

export function filesOutsideNestedModules(sourceFiles, label = 'source file closure') {
  assertQuickplay(Array.isArray(sourceFiles), `${label} must be an array`);
  assertQuickplay(
    sourceFiles.every((relative) => typeof relative === 'string'),
    `${label} entries must be strings`,
  );
  assertPortableFileClosure(sourceFiles, label);
  for (const relative of sourceFiles) {
    if (
      isPortableModuleManifestPath(relative)
      && path.posix.basename(relative) !== 'vo.mod'
    ) {
      throw new Error(`${label} module boundaries must use the exact basename vo.mod: ${relative}`);
    }
  }
  const nestedModuleRootKeys = new Set(
    sourceFiles
      .filter((relative) => (
        path.posix.dirname(relative) !== '.'
        && path.posix.basename(relative) === 'vo.mod'
      ))
      .map((relative) => portableCaseKey(path.posix.dirname(relative))),
  );
  return sourceFiles.filter((relative) => {
    const components = relative.split('/');
    for (let length = 1; length < components.length; length += 1) {
      const ancestorKey = portableCaseKey(components.slice(0, length).join('/'));
      if (nestedModuleRootKeys.has(ancestorKey)) return false;
    }
    return true;
  });
}

function filesystemObservationFact(relative, metadata, kind) {
  return {
    path: relative,
    kind,
    dev: String(metadata.dev),
    ino: String(metadata.ino),
    mode: String(metadata.mode),
    nlink: String(metadata.nlink),
    size: String(metadata.size),
    mtimeNs: String(metadata.mtimeNs),
    ctimeNs: String(metadata.ctimeNs),
  };
}

function filesystemEntryKind(metadata) {
  if (metadata.isFile()) return 'file';
  if (metadata.isDirectory()) return 'directory';
  if (metadata.isSymbolicLink()) return 'symlink';
  return 'special';
}

function skipCurrentSourceDirectory(name) {
  return name.startsWith('.')
    || name === 'vendor'
    || name === 'testdata'
    || name === 'node_modules'
    || name === 'target'
    || name === 'dist';
}

/**
 * Observe the same bounded package tree used by the core workspace source
 * scan. Directory entries are authoritative for Vo package membership,
 * including Git-ignored files and unsupported kinds that Git cannot list.
 */
export function observeCurrentSourceFilesystemClosure(
  root,
  sourceFiles,
  label = 'current source filesystem closure',
) {
  assertQuickplay(Array.isArray(sourceFiles), `${label} paths must be an array`);
  assertPortableFileClosure(sourceFiles, `${label} selected paths`);
  const canonicalRoot = realpathSync.native(path.resolve(root));
  assertQuickplay(
    sameNativePath(canonicalRoot, path.resolve(root)),
    `${label} root must use its canonical filesystem path`,
  );
  const selected = new Set(sourceFiles);
  const pending = [{ relative: '', depth: 0 }];
  const facts = [];
  const moduleBoundaries = [];
  const voSources = [];
  let observedEntries = 0;
  let observedPathBytes = 0;
  let observedDirectories = 0;
  while (pending.length > 0) {
    const { relative: relativeDirectory, depth } = pending.pop();
    observedDirectories += 1;
    assertQuickplay(
      observedDirectories <= MAX_STATIC_PATH_NODES,
      `${label} contains more than ${MAX_STATIC_PATH_NODES} observed directories`,
    );
    assertQuickplay(
      depth <= MAX_PATH_DEPTH,
      `${label} exceeds the ${MAX_PATH_DEPTH}-directory depth limit at ${relativeDirectory || '.'}`,
    );
    const absoluteDirectory = relativeDirectory === ''
      ? canonicalRoot
      : path.join(canonicalRoot, ...relativeDirectory.split('/'));
    const before = lstatSync(absoluteDirectory, { bigint: true });
    assertQuickplay(
      before.isDirectory()
        && !before.isSymbolicLink()
        && realpathSync.native(absoluteDirectory) === absoluteDirectory,
      `${label} has a non-directory or filesystem-alias ancestor: ${relativeDirectory || '.'}`,
    );
    facts.push(filesystemObservationFact(relativeDirectory, before, 'directory'));
    const directory = opendirSync(absoluteDirectory);
    const entries = [];
    try {
      for (let entry = directory.readSync(); entry !== null; entry = directory.readSync()) {
        observedEntries += 1;
        assertQuickplay(
          observedEntries <= MAX_WALK_ENTRIES,
          `${label} observes more than ${MAX_WALK_ENTRIES} directory entries`,
        );
        const relative = relativeDirectory === ''
          ? entry.name
          : `${relativeDirectory}/${entry.name}`;
        observedPathBytes += Buffer.byteLength(relative, 'utf8');
        assertQuickplay(
          Number.isSafeInteger(observedPathBytes)
            && observedPathBytes <= MAX_STATIC_PATH_KEY_BYTES,
          `${label} directory-entry names exceed the ${MAX_STATIC_PATH_KEY_BYTES}-byte limit`,
        );
        entries.push(entry);
      }
    } finally {
      directory.closeSync();
    }
    entries.sort((left, right) => compareUtf8(left.name, right.name));
    let nestedBoundary = false;
    for (const entry of entries) {
      if (portableCaseKey(entry.name) !== MODULE_MANIFEST_PORTABLE_BASENAME) continue;
      const relative = relativeDirectory === ''
        ? entry.name
        : `${relativeDirectory}/${entry.name}`;
      validateRelativePath(relative, `${label} module boundary`);
      if (entry.name !== 'vo.mod') {
        throw new Error(`${label} module boundaries must use the exact basename vo.mod: ${relative}`);
      }
      const absolute = path.join(absoluteDirectory, entry.name);
      const metadata = lstatSync(absolute, { bigint: true });
      const kind = filesystemEntryKind(metadata);
      facts.push(filesystemObservationFact(relative, metadata, kind));
      assertQuickplay(
        metadata.isFile() && !metadata.isSymbolicLink() && metadata.nlink === 1n,
        `${label} module boundary must be a single-link regular file: ${relative} (${kind})`,
      );
      assertQuickplay(selected.has(relative), `${label} omitted module boundary ${relative}`);
      moduleBoundaries.push(relative);
      nestedBoundary = relativeDirectory !== '';
    }
    if (!nestedBoundary) {
      const childDirectories = [];
      for (const entry of entries) {
        const relative = relativeDirectory === ''
          ? entry.name
          : `${relativeDirectory}/${entry.name}`;
        const absolute = path.join(absoluteDirectory, entry.name);
        const metadata = lstatSync(absolute, { bigint: true });
        const kind = filesystemEntryKind(metadata);
        if (kind === 'directory') {
          if (skipCurrentSourceDirectory(entry.name)) {
            facts.push({ path: relative, kind });
          } else {
            childDirectories.push({ relative, depth: depth + 1 });
          }
          continue;
        }
        assertQuickplay(
          kind === 'file',
          `${label} entry must be a regular file or directory without links: ${relative} (${kind})`,
        );
        if (entry.name.endsWith('.vo')) {
          validateRelativePath(relative, `${label} Vo source`);
          facts.push(filesystemObservationFact(relative, metadata, kind));
          assertQuickplay(
            metadata.nlink === 1n,
            `${label} Vo source must be a single-link regular file: ${relative}`,
          );
          assertQuickplay(selected.has(relative), `${label} omitted Vo source file ${relative}`);
          voSources.push(relative);
        } else if (entry.name !== 'vo.mod') {
          facts.push({ path: relative, kind });
        }
      }
      for (const child of childDirectories.reverse()) {
        pending.push(child);
      }
    }
    const after = lstatSync(absoluteDirectory, { bigint: true });
    assertQuickplay(
      after.isDirectory()
        && !after.isSymbolicLink()
        && sameStableFile(before, after)
        && realpathSync.native(absoluteDirectory) === absoluteDirectory,
      `${label} directory changed while it was observed: ${relativeDirectory || '.'}`,
    );
  }
  moduleBoundaries.sort(compareUtf8);
  voSources.sort(compareUtf8);
  facts.sort((left, right) => compareUtf8(left.path, right.path) || compareUtf8(left.kind, right.kind));
  return { facts, moduleBoundaries, voSources };
}

function exactKeys(value, expected, label) {
  assertQuickplay(value && typeof value === 'object' && !Array.isArray(value), `${label} must be an object`);
  const found = Object.keys(value).sort(compareUtf8);
  const wanted = [...expected].sort(compareUtf8);
  assertQuickplay(JSON.stringify(found) === JSON.stringify(wanted), `${label} has unexpected fields`);
}

export function validateRelativePath(value, label = 'path') {
  try {
    validatePortableRelativePath(value, label);
  } catch (error) {
    throw new Error(error instanceof Error ? error.message : `${label} is invalid`);
  }
  assertQuickplay(value.split('/').length <= MAX_PATH_DEPTH, `${label} exceeds the ${MAX_PATH_DEPTH}-component depth limit`);
  return value;
}

function exactVersionMajor(value, label) {
  try {
    return validateExactVersion(value);
  } catch (error) {
    throw new Error(`${label}: ${error instanceof Error ? error.message : String(error)}`);
  }
}

function parseSemanticVersion(value, label) {
  exactVersionMajor(value, label);
  const match = /^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(?:-([0-9a-z-]+(?:\.[0-9a-z-]+)*))?$/.exec(value);
  assertQuickplay(match, `${label} is not canonical SemVer`);
  return {
    major: BigInt(match[1]),
    minor: BigInt(match[2]),
    patch: BigInt(match[3]),
    pre: match[4]?.split('.').map((part) => (
      /^[0-9]+$/.test(part)
        ? { kind: 'numeric', value: BigInt(part) }
        : { kind: 'alpha', value: part }
    )) ?? [],
  };
}

function parseConstraint(value, label) {
  assertQuickplay(typeof value === 'string' && value.length > 0, `${label} must be a non-empty string`);
  const op = value[0] === '^' ? 'compatible' : value[0] === '~' ? 'patch' : 'exact';
  const renderedVersion = op === 'exact' ? value : value.slice(1);
  return { op, version: parseSemanticVersion(renderedVersion, label) };
}

function constraintVersionMajor(value, label) {
  return parseConstraint(value, label).version.major;
}

function compareSemanticVersions(left, right) {
  for (const key of ['major', 'minor', 'patch']) {
    if (left[key] < right[key]) return -1;
    if (left[key] > right[key]) return 1;
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

function sameVersionCore(left, right) {
  return left.major === right.major && left.minor === right.minor && left.patch === right.patch;
}

function constraintSatisfies(constraint, selected) {
  if (constraint.op === 'exact') return compareSemanticVersions(selected, constraint.version) === 0;
  if (
    selected.pre.length > 0
    && (constraint.version.pre.length === 0 || !sameVersionCore(selected, constraint.version))
  ) {
    return false;
  }
  if (compareSemanticVersions(selected, constraint.version) < 0) return false;
  if (constraint.op === 'patch') {
    return selected.major === constraint.version.major && selected.minor === constraint.version.minor;
  }
  if (constraint.version.major !== 0n) return selected.major === constraint.version.major;
  if (constraint.version.minor !== 0n) {
    return selected.major === 0n && selected.minor === constraint.version.minor;
  }
  return selected.major === 0n && selected.minor === 0n && selected.patch === constraint.version.patch;
}

function incrementVersionComponent(version, component) {
  if (version[component] === MAX_U64) return null;
  if (component === 'major') return { major: version.major + 1n, minor: 0n, patch: 0n, pre: [] };
  if (component === 'minor') return { major: version.major, minor: version.minor + 1n, patch: 0n, pre: [] };
  return { major: version.major, minor: version.minor, patch: version.patch + 1n, pre: [] };
}

function constraintUpperBound(constraint) {
  const version = constraint.version;
  if (constraint.op === 'patch') {
    return incrementVersionComponent(version, 'minor')
      ?? incrementVersionComponent(version, 'major');
  }
  if (version.major !== 0n) return incrementVersionComponent(version, 'major');
  if (version.minor !== 0n) {
    return incrementVersionComponent(version, 'minor')
      ?? { major: 1n, minor: 0n, patch: 0n, pre: [] };
  }
  return incrementVersionComponent(version, 'patch')
    ?? { major: 0n, minor: 1n, patch: 0n, pre: [] };
}

function rangeAcceptsOnlyLower(constraint) {
  if (constraint.version.pre.length > 0) return false;
  if (constraint.op === 'exact') return true;
  if (constraint.op === 'patch') return constraint.version.patch === MAX_U64;
  if (constraint.version.major === 0n) {
    return constraint.version.minor === 0n || constraint.version.patch === MAX_U64;
  }
  return constraint.version.minor === MAX_U64 && constraint.version.patch === MAX_U64;
}

function constraintIsSubset(left, right) {
  if (left.op === 'exact') return constraintSatisfies(right, left.version);
  if (right.op === 'exact') {
    return compareSemanticVersions(left.version, right.version) === 0 && rangeAcceptsOnlyLower(left);
  }
  if (
    left.version.pre.length > 0
    && (right.version.pre.length === 0 || !sameVersionCore(left.version, right.version))
  ) {
    return false;
  }
  if (compareSemanticVersions(right.version, left.version) > 0) return false;
  const leftUpper = constraintUpperBound(left);
  const rightUpper = constraintUpperBound(right);
  if (rightUpper === null) return true;
  return leftUpper !== null && compareSemanticVersions(leftUpper, rightUpper) <= 0;
}

function validateModuleForMajor(module, major, label) {
  try {
    validateModulePath(module, major);
  } catch (error) {
    throw new Error(`${label}: ${error instanceof Error ? error.message : String(error)}`);
  }
}

function validateModuleIdentity(module, label) {
  try {
    modulePathCacheKey(module);
  } catch (error) {
    throw new Error(`${label}: ${error instanceof Error ? error.message : String(error)}`);
  }
}

export function readJsonFile(file, label = file, options = {}) {
  const bytes = readBoundedRegularFile(file, label, options.maxBytes ?? MAX_JSON_BYTES);
  return parseBoundedJsonBytes(bytes, label, options);
}

/**
 * Recognize the minimum complete identity accepted before replacing an older
 * checked-in Quickplay directory. Recovery deliberately preserves that exact
 * previous directory even when its schema predates the current generator.
 */
export function recoverableQuickplayDirectory(directory) {
  try {
    const root = path.resolve(directory);
    const rootMetadata = lstatSync(root);
    if (!rootMetadata.isDirectory() || rootMetadata.isSymbolicLink()) return false;
    for (const name of ['project.json', 'deps.json', 'provenance.json']) {
      const metadata = lstatSync(path.join(root, name));
      if (!metadata.isFile() || metadata.isSymbolicLink()) return false;
    }
    const provenance = readJsonFile(
      path.join(root, 'provenance.json'),
      'recoverable Quickplay provenance',
    );
    return provenance?.artifact === 'studio.quickplay.blockkart'
      && provenance?.path === 'apps/studio/public/quickplay/blockkart';
  } catch {
    return false;
  }
}

function sameStableFile(left, right) {
  return left.dev === right.dev
    && left.ino === right.ino
    && left.mode === right.mode
    && left.nlink === right.nlink
    && left.uid === right.uid
    && left.gid === right.gid
    && left.rdev === right.rdev
    && left.size === right.size
    && left.mtimeNs === right.mtimeNs
    && left.ctimeNs === right.ctimeNs;
}

function rootedDirectorySnapshot(root, relative, label) {
  const components = relative.split('/');
  const directories = [];
  let current = root;
  for (const component of [null, ...components.slice(0, -1)]) {
    if (component !== null) current = path.join(current, component);
    const metadata = lstatSync(current, { bigint: true });
    assertQuickplay(
      metadata.isDirectory() && !metadata.isSymbolicLink(),
      `${label} has a non-directory or symbolic-link ancestor`,
    );
    assertQuickplay(
      realpathSync.native(current) === current,
      `${label} has a filesystem-alias ancestor`,
    );
    directories.push({ path: current, metadata });
  }
  return directories;
}

function assertRootedDirectoriesStable(directories, label) {
  for (const directory of directories) {
    const current = lstatSync(directory.path, { bigint: true });
    assertQuickplay(
      current.isDirectory()
        && !current.isSymbolicLink()
        && sameStableFile(directory.metadata, current)
        && realpathSync.native(directory.path) === directory.path,
      `${label} ancestor changed while it was being read`,
    );
  }
}

export function readBoundedRegularFileSnapshot(file, label = file, maxBytes = MAX_FILE_BYTES) {
  assertQuickplay(
    Number.isSafeInteger(maxBytes) && maxBytes >= 0 && maxBytes <= MAX_FILE_BYTES,
    `${label}: invalid bounded read limit`,
  );
  const requested = path.resolve(file);
  const pathMetadata = lstatSync(requested, { bigint: true });
  assertQuickplay(pathMetadata.isFile() && !pathMetadata.isSymbolicLink(), `${label} must be a regular file`);
  assertQuickplay(pathMetadata.size <= BigInt(maxBytes), `${label} exceeds the ${maxBytes}-byte limit`);
  const absolute = path.join(realpathSync.native(path.dirname(requested)), path.basename(requested));
  assertQuickplay(realpathSync.native(requested) === absolute, `${label} must not end at a filesystem alias`);

  const descriptor = openSync(
    absolute,
    fsConstants.O_RDONLY | (typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0),
  );
  try {
    const before = fstatSync(descriptor, { bigint: true });
    assertQuickplay(before.isFile(), `${label} must be a regular file`);
    assertQuickplay(
      pathMetadata.dev === before.dev && pathMetadata.ino === before.ino,
      `${label} changed before it could be read`,
    );
    assertQuickplay(before.size <= BigInt(maxBytes), `${label} exceeds the ${maxBytes}-byte limit`);
    const size = Number(before.size);
    const bytes = Buffer.allocUnsafe(size);
    let offset = 0;
    while (offset < size) {
      const count = readSync(descriptor, bytes, offset, size - offset, offset);
      assertQuickplay(count > 0, `${label} ended before its declared size`);
      offset += count;
    }
    const after = fstatSync(descriptor, { bigint: true });
    assertQuickplay(sameStableFile(before, after), `${label} changed while it was being read`);
    return {
      bytes,
      mode: (before.mode & 0o111n) === 0n ? 0o644 : 0o755,
    };
  } finally {
    closeSync(descriptor);
  }
}

export function readRootedBoundedRegularFileSnapshot(
  root,
  relative,
  label = relative,
  maxBytes = MAX_FILE_BYTES,
) {
  validateRelativePath(relative, label);
  assertQuickplay(
    Number.isSafeInteger(maxBytes) && maxBytes >= 0 && maxBytes <= MAX_FILE_BYTES,
    `${label}: invalid bounded read limit`,
  );
  const rootReal = realpathSync.native(path.resolve(root));
  const directories = rootedDirectorySnapshot(rootReal, relative, label);
  const requested = path.join(rootReal, ...relative.split('/'));
  const pathMetadata = lstatSync(requested, { bigint: true });
  assertQuickplay(
    pathMetadata.isFile() && !pathMetadata.isSymbolicLink(),
    `${label} must be a regular file`,
  );
  assertQuickplay(pathMetadata.nlink === 1n, `${label} must have exactly one filesystem link`);
  assertQuickplay(pathMetadata.size <= BigInt(maxBytes), `${label} exceeds the ${maxBytes}-byte limit`);
  assertQuickplay(realpathSync.native(requested) === requested, `${label} must not use a filesystem alias`);
  assertRootedDirectoriesStable(directories, label);

  const descriptor = openSync(
    requested,
    fsConstants.O_RDONLY | (typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0),
  );
  try {
    const before = fstatSync(descriptor, { bigint: true });
    assertQuickplay(
      before.isFile()
        && before.nlink === 1n
        && sameStableFile(pathMetadata, before),
      `${label} changed before it could be read`,
    );
    assertQuickplay(before.size <= BigInt(maxBytes), `${label} exceeds the ${maxBytes}-byte limit`);
    const size = Number(before.size);
    const bytes = Buffer.allocUnsafe(size);
    let offset = 0;
    while (offset < size) {
      const count = readSync(descriptor, bytes, offset, size - offset, offset);
      assertQuickplay(count > 0, `${label} ended before its declared size`);
      offset += count;
    }
    const after = fstatSync(descriptor, { bigint: true });
    assertQuickplay(
      after.nlink === 1n && sameStableFile(before, after),
      `${label} changed while it was being read`,
    );
    return {
      bytes,
      mode: (before.mode & 0o111n) === 0n ? 0o644 : 0o755,
    };
  } finally {
    closeSync(descriptor);
    const pathAfter = lstatSync(requested, { bigint: true });
    assertQuickplay(
      pathAfter.isFile()
        && !pathAfter.isSymbolicLink()
        && pathAfter.nlink === 1n
        && sameStableFile(pathMetadata, pathAfter)
        && realpathSync.native(requested) === requested,
      `${label} path changed while it was being read`,
    );
    assertRootedDirectoriesStable(directories, label);
  }
}

export function readBoundedRegularFile(file, label = file, maxBytes = MAX_FILE_BYTES) {
  return readBoundedRegularFileSnapshot(file, label, maxBytes).bytes;
}

function isPlainJsonObject(value) {
  if (value === null || typeof value !== 'object' || Array.isArray(value)) return false;
  const prototype = Object.getPrototypeOf(value);
  return prototype === Object.prototype || prototype === null;
}

export function parseBlockKartVpakProducerManifestBytes(
  bytes,
  label = 'BlockKart VPAK producer manifest',
) {
  const producer = parseBoundedJsonBytes(bytes, label, {
    maxBytes: MAX_PROTOCOL_METADATA_BYTES,
    maxDepth: MAX_JSON_DEPTH,
    maxTokens: MAX_JSON_TOKENS,
    maxObjectKeys: MAX_JSON_OBJECT_KEYS,
    maxObjectKeyBytes: MAX_PROTOCOL_METADATA_BYTES,
  });
  assertQuickplay(isPlainJsonObject(producer), `${label} must contain a JSON object`);
  const canonicalBytes = Buffer.from(`${JSON.stringify(producer, null, 2)}\n`, 'utf8');
  const sourceBytes = Buffer.from(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  assertQuickplay(
    sourceBytes.equals(canonicalBytes),
    `${label} must use JSON.stringify(value, null, 2) with exactly one trailing LF`,
  );
  const expectedKeys = [
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
  ];
  assertQuickplay(
    JSON.stringify(Object.keys(producer)) === JSON.stringify(expectedKeys),
    `${label} must contain the canonical producer fields in canonical order`,
  );
  assertQuickplay(
    producer.schemaVersion === 1
      && producer.kind === 'blockkart.vpakProducerManifest'
      && producer.owner === 'BlockKart',
    `${label} identity is invalid`,
  );
  assertQuickplay(
    JSON.stringify(producer.command) === JSON.stringify(VO_CLI_GUEST_ENVIRONMENT.command),
    `${label} command does not match the authenticated Vo CLI guest command`,
  );
  assertQuickplay(
    Array.isArray(producer.inputs)
      && Array.isArray(producer.archiveEntries)
      && producer.archiveEntries.length === 37
      && Array.isArray(producer.upstream)
      && isPlainJsonObject(producer.internalManifest)
      && Number.isSafeInteger(producer.workspaceSourceInputCount)
      && producer.workspaceSourceInputCount > 0
      && producer.workspaceSourceInputCount <= producer.inputs.length
      && producer.payloadInputCount === 37
      && producer.archiveEntryCount === 37,
    `${label} source and archive closure counts are invalid`,
  );
  assertQuickplay(
    isPlainJsonObject(producer.pack)
      && JSON.stringify(Object.keys(producer.pack)) === JSON.stringify(['path', 'sha256', 'size'])
      && producer.pack.path === BLOCKKART_VPAK_PATH
      && /^[0-9a-f]{64}$/u.test(producer.pack.sha256 ?? '')
      && Number.isSafeInteger(producer.pack.size)
      && producer.pack.size > 0
      && producer.pack.size <= MAX_BLOCKKART_VPAK_BYTES
      && /^[0-9a-f]{64}$/u.test(producer.producerDigest ?? ''),
    `${label} pack or producer digest identity is invalid`,
  );
  const { producerDigest, ...producerIntent } = producer;
  assertQuickplay(
    sha256Digest(Buffer.from(JSON.stringify(producerIntent), 'utf8')) === `sha256:${producerDigest}`,
    `${label} producerDigest does not bind the canonical producer intent`,
  );
  return producer;
}

function blockKartVpakFileFact(root, relative, label, maxBytes) {
  const snapshot = readRootedBoundedRegularFileSnapshot(root, relative, label, maxBytes);
  return {
    bytes: snapshot.bytes,
    digest: sha256Digest(snapshot.bytes),
    mode: snapshot.mode,
    path: relative,
    size: snapshot.bytes.byteLength,
  };
}

function sameBlockKartVpakFileFact(left, right) {
  return Buffer.isBuffer(left?.bytes)
    && Buffer.isBuffer(right?.bytes)
    && left.path === right.path
    && left.digest === right.digest
    && left.mode === right.mode
    && left.size === right.size
    && left.bytes.equals(right.bytes);
}

export function observeBlockKartVpakProducer(
  blockKartRoot,
  label = 'BlockKart VPAK producer',
) {
  const manifest = blockKartVpakFileFact(
    blockKartRoot,
    BLOCKKART_VPAK_PRODUCER_PATH,
    `${label} manifest`,
    MAX_PROTOCOL_METADATA_BYTES,
  );
  const producer = parseBlockKartVpakProducerManifestBytes(manifest.bytes, `${label} manifest`);
  const pack = blockKartVpakFileFact(
    blockKartRoot,
    BLOCKKART_VPAK_PATH,
    `${label} VPAK`,
    MAX_BLOCKKART_VPAK_BYTES,
  );
  assertQuickplay(
    pack.digest === `sha256:${producer.pack.sha256}` && pack.size === producer.pack.size,
    `${label} VPAK bytes do not match the producer manifest`,
  );
  const manifestAfter = blockKartVpakFileFact(
    blockKartRoot,
    BLOCKKART_VPAK_PRODUCER_PATH,
    `${label} manifest post-pack verification`,
    MAX_PROTOCOL_METADATA_BYTES,
  );
  const packAfter = blockKartVpakFileFact(
    blockKartRoot,
    BLOCKKART_VPAK_PATH,
    `${label} VPAK post-manifest verification`,
    MAX_BLOCKKART_VPAK_BYTES,
  );
  assertQuickplay(
    sameBlockKartVpakFileFact(manifest, manifestAfter)
      && sameBlockKartVpakFileFact(pack, packAfter),
    `${label} changed while its producer binding was observed`,
  );
  return { manifest, pack, producer };
}

export function assertSameBlockKartVpakProducerState(left, right, label) {
  assertQuickplay(
    left?.producer?.producerDigest === right?.producer?.producerDigest
      && sameBlockKartVpakFileFact(left?.manifest ?? {}, right?.manifest ?? {})
      && sameBlockKartVpakFileFact(left?.pack ?? {}, right?.pack ?? {}),
    `${label} changed across its authenticated observation window`,
  );
}

function readRegularFile(root, relative, label, maxBytes = MAX_FILE_BYTES) {
  return readRootedBoundedRegularFileSnapshot(root, relative, label, maxBytes).bytes;
}

function readRegularFileSnapshot(root, relative, label, maxBytes = MAX_FILE_BYTES) {
  return readRootedBoundedRegularFileSnapshot(root, relative, label, maxBytes);
}

function digestBinding(value, label) {
  assertQuickplay(value && typeof value === 'object' && !Array.isArray(value), `${label} must be an object`);
  assertQuickplay(Number.isSafeInteger(value.size) && value.size >= 0, `${label}.size is invalid`);
  assertQuickplay(DIGEST.test(value.digest), `${label}.digest is invalid`);
}

function verifyBoundBytes(bytes, binding, label) {
  digestBinding(binding, label);
  assertQuickplay(bytes.byteLength === binding.size, `${label} size mismatch`);
  assertQuickplay(sha256Digest(bytes) === binding.digest, `${label} digest mismatch`);
}

function normalizedDependencies(value, label) {
  assertQuickplay(
    Array.isArray(value) && value.length <= MAX_STATIC_MODULES,
    `${label} must be an array with at most ${MAX_STATIC_MODULES} entries`,
  );
  const dependencies = value.map((entry, index) => {
    exactKeys(entry, ['module', 'constraint'], `${label}[${index}]`);
    const major = constraintVersionMajor(entry.constraint, `${label}[${index}].constraint`);
    validateModuleForMajor(entry.module, major, `${label}[${index}].module`);
    return { module: entry.module, constraint: entry.constraint };
  });
  const sorted = [...dependencies].sort((left, right) => compareUtf8(left.module, right.module));
  assertQuickplay(JSON.stringify(dependencies) === JSON.stringify(sorted), `${label} must be sorted`);
  assertQuickplay(new Set(dependencies.map((entry) => entry.module)).size === dependencies.length, `${label} has duplicates`);
  return dependencies;
}

function validateSnapshotGraph(snapshot, label) {
  const modules = new Map(snapshot.modules.map((module) => [module.module, module]));
  assertQuickplay(!modules.has(snapshot.root.module), `${label} must not repeat the root module`);
  let edgeCount = 0;
  const incomingConstraints = new Map();
  for (const owner of [snapshot.root, ...snapshot.modules]) {
    for (const dependency of owner.dependencies) {
      edgeCount += 1;
      assertQuickplay(
        edgeCount <= MAX_PROJECT_GRAPH_EDGES,
        `${label} contains more than ${MAX_PROJECT_GRAPH_EDGES} dependency edges`,
      );
      assertQuickplay(
        dependency.module !== owner.module,
        `${label} module ${owner.module} must not depend on itself`,
      );
      assertQuickplay(
        dependency.module !== snapshot.root.module,
        `${label} module ${owner.module} must not depend on the root module`,
      );
      const selected = modules.get(dependency.module);
      assertQuickplay(selected, `${label} dependency ${dependency.module} is absent from modules`);
      const constraint = parseConstraint(
        dependency.constraint,
        `${label} dependency ${dependency.module} constraint`,
      );
      if (snapshot.authority === 'lock') {
        const version = parseSemanticVersion(
          selected.version,
          `${label} selected version for ${dependency.module}`,
        );
        assertQuickplay(
          constraintSatisfies(constraint, version),
          `${label} selects ${dependency.module}@${selected.version}, which does not satisfy ${dependency.constraint}`,
        );
      } else if (snapshot.authority === 'workspace') {
        const incoming = incomingConstraints.get(dependency.module) ?? [];
        incoming.push(constraint);
        incomingConstraints.set(dependency.module, incoming);
      }
    }
  }
  if (snapshot.authority === 'workspace') {
    for (const [module, constraints] of incomingConstraints) {
      const greatestLowerBound = constraints
        .map((constraint) => constraint.version)
        .reduce((left, right) => (compareSemanticVersions(left, right) >= 0 ? left : right));
      const stableCandidate = {
        major: greatestLowerBound.major,
        minor: greatestLowerBound.minor,
        patch: greatestLowerBound.patch,
        pre: [],
      };
      assertQuickplay(
        constraints.every((constraint) => constraintSatisfies(constraint, greatestLowerBound))
          || (
            greatestLowerBound.pre.length > 0
            && constraints.every((constraint) => constraintSatisfies(constraint, stableCandidate))
          ),
        `${label} has disjoint incoming constraints for ${module}`,
      );
    }
  }
  const reachable = new Set();
  const queue = snapshot.root.dependencies.map((dependency) => dependency.module);
  for (let cursor = 0; cursor < queue.length; cursor += 1) {
    const module = queue[cursor];
    assertQuickplay(modules.has(module), `${label} dependency ${module} is absent from modules`);
    if (reachable.has(module)) continue;
    reachable.add(module);
    queue.push(...modules.get(module).dependencies.map((dependency) => dependency.module));
  }
  assertQuickplay(reachable.size === modules.size, `${label} contains unreachable modules`);
}

export function validateProjectSnapshot(snapshot, expectedRoot) {
  exactKeys(
    snapshot,
    snapshot.workspace === undefined
      ? ['schema_version', 'mode', 'authority', 'root', 'modules']
      : ['schema_version', 'mode', 'authority', 'root', 'workspace', 'modules'],
    'project snapshot',
  );
  assertQuickplay(snapshot.schema_version === SNAPSHOT_SCHEMA, `snapshot schema must be ${SNAPSHOT_SCHEMA}`);
  assertQuickplay(snapshot.mode === 'effective', 'Quickplay requires an effective project snapshot');
  assertQuickplay(
    snapshot.authority === 'empty'
      || snapshot.authority === 'lock'
      || snapshot.authority === 'workspace',
    'snapshot authority must be empty, lock, or workspace',
  );
  exactKeys(snapshot.root, ['module', 'vo', 'dependencies'], 'snapshot root');
  assertQuickplay(snapshot.root.module === expectedRoot, `snapshot root must be ${expectedRoot}`);
  validateModuleIdentity(snapshot.root.module, 'snapshot root module');
  const rootVo = parseConstraint(snapshot.root.vo, 'snapshot root toolchain constraint');
  const rootDependencies = normalizedDependencies(snapshot.root.dependencies, 'snapshot root dependencies');
  if (snapshot.workspace !== undefined) {
    exactKeys(snapshot.workspace, ['file'], 'snapshot workspace');
    assertQuickplay(
      typeof snapshot.workspace.file === 'string'
      && snapshot.workspace.file.length > 0
      && !/[\u0000-\u001f\u007f]/u.test(snapshot.workspace.file),
      'snapshot workspace file is invalid',
    );
  }
  assertQuickplay(Array.isArray(snapshot.modules), 'snapshot modules must be an array');
  assertQuickplay(snapshot.modules.length <= MAX_STATIC_MODULES, 'snapshot contains too many modules');
  if (snapshot.authority === 'empty') {
    assertQuickplay(
      snapshot.root.dependencies.length === 0 && snapshot.modules.length === 0,
      'empty snapshot authority requires an empty dependency graph',
    );
  } else if (snapshot.authority === 'lock') {
    assertQuickplay(
      snapshot.root.dependencies.length > 0 && snapshot.modules.length > 0,
      'lock snapshot authority requires a non-empty locked dependency graph',
    );
  } else {
    assertQuickplay(
      snapshot.workspace !== undefined
      && snapshot.root.dependencies.length > 0
      && snapshot.modules.length > 0,
      'workspace snapshot authority requires selected workspace provenance and a non-empty graph',
    );
  }
  let previous = '';
  for (const [index, module] of snapshot.modules.entries()) {
    exactKeys(
      module,
      snapshot.authority === 'workspace'
        ? ['module', 'vo', 'source', 'dependencies']
        : ['module', 'version', 'vo', 'release', 'source', 'dependencies'],
      `snapshot modules[${index}]`,
    );
    validateModuleIdentity(module.module, `snapshot module ${index}`);
    assertQuickplay(compareUtf8(previous, module.module) < 0, 'snapshot modules must be sorted and unique');
    previous = module.module;
    normalizedDependencies(module.dependencies, `${module.module} dependencies`);
    const moduleVo = parseConstraint(module.vo, `${module.module} toolchain constraint`);
    assertQuickplay(
      constraintIsSubset(rootVo, moduleVo),
      `${module.module} toolchain constraint does not cover the root constraint`,
    );
    if (snapshot.authority === 'lock') {
      const major = exactVersionMajor(module.version, `${module.module} exact version`);
      validateModuleForMajor(module.module, major, `${module.module} selected version`);
      assertQuickplay(DIGEST.test(module.release), `${module.module} release digest is invalid`);
      assertQuickplay(
        module.source.kind === 'registry' || module.source.kind === 'workspace',
        `${module.module} source kind is invalid`,
      );
    } else {
      assertQuickplay(
        module.source.kind === 'workspace',
        `${module.module} must use a workspace source under workspace authority`,
      );
    }
    exactKeys(module.source, ['kind', 'directory'], `${module.module} ${module.source.kind} source`);
    if (module.source.kind === 'workspace') {
      assertQuickplay(snapshot.workspace !== undefined, `${module.module} workspace source requires snapshot workspace state`);
    }
    assertQuickplay(
      typeof module.source.directory === 'string'
      && module.source.directory.length > 0
      && !/[\u0000-\u001f\u007f]/u.test(module.source.directory),
      `${module.module} source directory is invalid`,
    );
  }
  validateSnapshotGraph(snapshot, 'project snapshot');
  const canonical = {
    schema_version: SNAPSHOT_SCHEMA,
    mode: 'effective',
    authority: snapshot.authority,
    root: {
      module: snapshot.root.module,
      vo: snapshot.root.vo,
      dependencies: rootDependencies,
    },
    ...(snapshot.workspace === undefined
      ? {}
      : { workspace: { file: snapshot.workspace.file } }),
    modules: snapshot.modules.map((module) => (
      snapshot.authority === 'lock'
        ? {
            module: module.module,
            version: module.version,
            vo: module.vo,
            release: module.release,
            source: { kind: module.source.kind, directory: module.source.directory },
            dependencies: module.dependencies.map((dependency) => ({ ...dependency })),
          }
        : {
            module: module.module,
            vo: module.vo,
            source: { kind: 'workspace', directory: module.source.directory },
            dependencies: module.dependencies.map((dependency) => ({ ...dependency })),
          }
    )),
  };
  assertQuickplay(
    canonicalPrettyJsonBytes(canonical).byteLength <= MAX_JSON_BYTES,
    `project snapshot exceeds the ${MAX_JSON_BYTES}-byte canonical JSON limit`,
  );
  return canonical;
}

function renderCanonicalVoLock(snapshot) {
  if (snapshot.authority !== 'lock') return null;
  let rendered = `version = 3\n\n[root]\nmodule = ${JSON.stringify(snapshot.root.module)}\nvo = ${JSON.stringify(snapshot.root.vo)}\n`;
  for (const module of snapshot.modules) {
    rendered += `\n[[module]]\npath = ${JSON.stringify(module.module)}\n`;
    rendered += `version = ${JSON.stringify(module.version)}\n`;
    rendered += `vo = ${JSON.stringify(module.vo)}\n`;
    rendered += `release = ${JSON.stringify(module.release)}\n`;
    if (module.dependencies.length === 0) {
      rendered += 'dependencies = []\n';
      continue;
    }
    rendered += 'dependencies = [\n';
    for (const dependency of module.dependencies) {
      rendered += `  { module = ${JSON.stringify(dependency.module)}, constraint = ${JSON.stringify(dependency.constraint)} },\n`;
    }
    rendered += ']\n';
  }
  return rendered;
}

export function canonicalVoLock(snapshot) {
  return renderCanonicalVoLock(validateProjectSnapshot(snapshot, snapshot?.root?.module));
}

const VO_BINARY_AUTHORITY_STATE = new WeakMap();
const VO_BUILD_INHERITED_ENVIRONMENT = Object.freeze([
  'CARGO_HOME',
  'ComSpec',
  'HOME',
  'PATH',
  'PATHEXT',
  'RUSTUP_HOME',
  'SystemDrive',
  'SystemRoot',
  'TEMP',
  'TMP',
  'TMPDIR',
  'USERPROFILE',
  'WINDIR',
]);

function sameNativePath(left, right) {
  const normalizedLeft = path.normalize(path.resolve(left));
  const normalizedRight = path.normalize(path.resolve(right));
  return process.platform === 'win32'
    ? normalizedLeft.toUpperCase() === normalizedRight.toUpperCase()
    : normalizedLeft === normalizedRight;
}

function canonicalRealDirectory(directory, label) {
  const requested = path.resolve(directory);
  const canonical = realpathSync.native(requested);
  const metadata = lstatSync(canonical, { bigint: true });
  assertQuickplay(
    metadata.isDirectory() && !metadata.isSymbolicLink(),
    `${label} must be a real directory`,
  );
  return canonical;
}

function canonicalVoRoot(root) {
  assertQuickplay(typeof root === 'string' && path.isAbsolute(root), 'Volang root must be absolute');
  const requested = path.resolve(root);
  const canonical = canonicalRealDirectory(requested, 'Volang root');
  assertQuickplay(
    sameNativePath(requested, canonical),
    'Volang root must not traverse symbolic-link path components',
  );
  return canonical;
}

function canonicalCargoTargetDirectory(root, candidate) {
  const defaultTarget = candidate === null || candidate === undefined;
  const requested = defaultTarget ? path.join(root, 'target') : candidate;
  assertQuickplay(
    typeof requested === 'string' && path.isAbsolute(requested),
    'Vo CLI Cargo target directory must be absolute',
  );
  const resolved = path.resolve(requested);
  if (!existsSync(resolved)) {
    const parent = canonicalRealDirectory(path.dirname(resolved), 'Vo CLI Cargo target parent');
    const destination = path.join(parent, path.basename(resolved));
    mkdirSync(destination, { recursive: false, mode: 0o700 });
    const canonical = canonicalRealDirectory(destination, 'Vo CLI Cargo target directory');
    if (defaultTarget) {
      assertQuickplay(
        sameNativePath(destination, canonical),
        'default Vo CLI Cargo target directory must not traverse symbolic-link path components',
      );
    }
    return canonical;
  }
  const metadata = lstatSync(resolved, { bigint: true });
  assertQuickplay(
    metadata.isDirectory() && !metadata.isSymbolicLink(),
    'Vo CLI Cargo target directory must be a real directory',
  );
  const canonical = canonicalRealDirectory(resolved, 'Vo CLI Cargo target directory');
  if (defaultTarget) {
    assertQuickplay(
      sameNativePath(resolved, canonical),
      'default Vo CLI Cargo target directory must not traverse symbolic-link path components',
    );
  }
  return canonical;
}

function inheritVoBuildEnvironment(baseEnvironment, targetDirectory, root) {
  assertQuickplay(
    baseEnvironment && typeof baseEnvironment === 'object' && !Array.isArray(baseEnvironment),
    'Vo CLI build environment must be an object',
  );
  const environment = {};
  for (const canonicalKey of VO_BUILD_INHERITED_ENVIRONMENT) {
    const matches = Object.keys(baseEnvironment).filter((key) => (
      process.platform === 'win32'
        ? key.toUpperCase() === canonicalKey.toUpperCase()
        : key === canonicalKey
    ));
    assertQuickplay(matches.length <= 1, `Vo CLI build environment has ambiguous ${canonicalKey} keys`);
    const value = matches.length === 1 ? baseEnvironment[matches[0]] : undefined;
    if (typeof value === 'string' && value.length > 0) environment[canonicalKey] = value;
  }
  assertQuickplay(
    typeof environment.PATH === 'string' && environment.PATH.length > 0,
    'Vo CLI build environment requires PATH',
  );
  const home = environment.HOME ?? environment.USERPROFILE;
  assertQuickplay(
    typeof home === 'string' && path.isAbsolute(home),
    'Vo CLI build environment requires an absolute HOME or USERPROFILE',
  );
  const cargoHome = environment.CARGO_HOME ?? path.join(home, '.cargo');
  assertQuickplay(path.isAbsolute(cargoHome), 'Vo CLI build environment requires an absolute CARGO_HOME');
  Object.assign(environment, {
    CARGO_HOME: path.resolve(cargoHome),
    CARGO_TARGET_DIR: targetDirectory,
    CARGO_TERM_COLOR: 'never',
    LANG: 'C',
    LC_ALL: 'C',
    TZ: 'UTC',
    VOWORK: 'off',
  });
  assertVoCliCargoConfigBoundary(root, environment);
  return environment;
}

function commandFailureDetail(result) {
  return result?.error?.message
    || String(result?.stderr ?? '').trim()
    || `exit status ${result?.status ?? '(missing)'}`;
}

function runCheckedProcess(processRunner, command, args, options, label) {
  let result;
  try {
    result = processRunner(command, args, options);
  } catch (error) {
    throw new Error(`${label} failed: ${error instanceof Error ? error.message : String(error)}`);
  }
  assertQuickplay(result && typeof result === 'object', `${label} returned no process result`);
  if (result.error || result.status !== 0) {
    throw new Error(`${label} failed: ${commandFailureDetail(result)}`);
  }
  return result;
}

function currentHostTarget(root, environment, processRunner) {
  const result = runCheckedProcess(processRunner, 'rustc', ['-vV'], {
    cwd: root,
    env: environment,
    encoding: 'utf8',
    maxBuffer: 1024 * 1024,
    timeout: 30_000,
  }, 'rustc host query');
  const host = String(result.stdout ?? '').match(/^host:\s*([A-Za-z0-9_.-]+)\s*$/mu)?.[1];
  assertQuickplay(host, 'rustc host query returned no canonical host target');
  return host;
}

function observeVoBinary(file, label) {
  assertQuickplay(
    typeof file === 'string' && path.isAbsolute(file),
    `${label} path must be absolute`,
  );
  const resolved = path.resolve(file);
  const canonical = realpathSync.native(resolved);
  assertQuickplay(
    sameNativePath(resolved, canonical),
    `${label} must not traverse symbolic-link path components`,
  );
  const before = lstatSync(canonical, { bigint: true });
  assertQuickplay(
    before.isFile() && !before.isSymbolicLink(),
    `${label} must be a real regular file`,
  );
  assertQuickplay(before.nlink === 1n, `${label} must have exactly one filesystem link`);
  assertQuickplay(
    before.size > 0n && before.size <= BigInt(MAX_VO_BINARY_BYTES),
    `${label} size must be within 1..${MAX_VO_BINARY_BYTES} bytes`,
  );
  if (process.platform !== 'win32') {
    assertQuickplay((before.mode & 0o111n) !== 0n, `${label} must be executable`);
  }
  const descriptor = openSync(
    canonical,
    fsConstants.O_RDONLY
      | (typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0),
  );
  try {
    const opened = fstatSync(descriptor, { bigint: true });
    assertQuickplay(
      opened.isFile()
        && !opened.isSymbolicLink()
        && opened.nlink === 1n
        && sameStableFile(before, opened),
      `${label} changed before its identity could be observed`,
    );
    const hash = createHash('sha256');
    const chunk = Buffer.allocUnsafe(64 * 1024);
    const size = Number(opened.size);
    let offset = 0;
    while (offset < size) {
      const count = readSync(
        descriptor,
        chunk,
        0,
        Math.min(chunk.byteLength, size - offset),
        offset,
      );
      assertQuickplay(count > 0, `${label} ended before its declared size`);
      hash.update(chunk.subarray(0, count));
      offset += count;
    }
    assertQuickplay(
      readSync(descriptor, chunk, 0, 1, size) === 0,
      `${label} grew while its identity was observed`,
    );
    const after = fstatSync(descriptor, { bigint: true });
    const pathAfter = lstatSync(canonical, { bigint: true });
    assertQuickplay(
      after.nlink === 1n
        && pathAfter.nlink === 1n
        && sameStableFile(opened, after)
        && sameStableFile(opened, pathAfter),
      `${label} changed while its identity was observed`,
    );
    return Object.freeze({
      digest: `sha256:${hash.digest('hex')}`,
      path: canonical,
      size,
      stat: opened,
    });
  } finally {
    closeSync(descriptor);
  }
}

function assertSameVoBinary(expected, actual, label) {
  assertQuickplay(
    expected.path === actual.path
      && expected.size === actual.size
      && expected.digest === actual.digest
      && sameStableFile(expected.stat, actual.stat),
    `${label} changed after its binary authority was established`,
  );
}

function restrictedVoGuestEnvironment(baseEnvironment, guestEnvironment) {
  assertQuickplay(
    guestEnvironment && typeof guestEnvironment === 'object' && !Array.isArray(guestEnvironment),
    'Vo CLI guest environment must be an object',
  );
  const environment = { ...baseEnvironment };
  for (const key of Object.keys(environment)) {
    const upper = key.toUpperCase();
    if (upper === 'VOWORK' || upper.startsWith('VO_')) delete environment[key];
  }
  const explicitVoKeys = new Set();
  for (const [key, value] of Object.entries(guestEnvironment)) {
    assertQuickplay(
      key.length > 0 && !key.includes('\0') && !key.includes('='),
      'Vo CLI guest environment contains an invalid key',
    );
    assertQuickplay(
      typeof value === 'string' && !value.includes('\0'),
      `Vo CLI guest environment ${key} must be a string without NUL bytes`,
    );
    const upper = key.toUpperCase();
    const voKey = upper === 'VOWORK' || upper.startsWith('VO_');
    if (voKey) {
      assertQuickplay(upper !== 'VO_BIN', 'Vo CLI guest environment must not redefine VO_BIN');
      assertQuickplay(
        !explicitVoKeys.has(upper),
        `Vo CLI guest environment contains duplicate ${upper} spellings`,
      );
      explicitVoKeys.add(upper);
      environment[upper] = value;
    } else {
      environment[key] = value;
    }
  }
  return environment;
}

/**
 * Establish the exact Vo executable used by snapshot and fetch operations.
 * The default path builds the current root under a host-only environment;
 * callers opting into VO_BIN must bind its absolute path explicitly.
 */
export function createVoBinaryAuthority({
  root,
  allowVoBin = false,
  voBin = null,
  environment = process.env,
  cargoTargetDirectory = null,
  processRunner = spawnSync,
}) {
  const canonicalRoot = canonicalVoRoot(root);
  assertQuickplay(typeof processRunner === 'function', 'Vo CLI process runner must be a function');
  assertQuickplay(typeof allowVoBin === 'boolean', 'allowVoBin must be boolean');
  let observation;
  let kind;
  if (allowVoBin) {
    assertQuickplay(
      typeof voBin === 'string' && path.isAbsolute(voBin),
      'allowVoBin requires an explicitly bound absolute voBin path',
    );
    assertQuickplay(
      cargoTargetDirectory === null || cargoTargetDirectory === undefined,
      'explicit voBin authority cannot also select a Cargo target directory',
    );
    observation = observeVoBinary(voBin, 'explicit Vo CLI binary');
    kind = 'explicit';
  } else {
    assertQuickplay(voBin === null || voBin === undefined, 'voBin requires allowVoBin=true');
    const targetDirectory = canonicalCargoTargetDirectory(canonicalRoot, cargoTargetDirectory);
    const buildEnvironment = inheritVoBuildEnvironment(environment, targetDirectory, canonicalRoot);
    const host = currentHostTarget(canonicalRoot, buildEnvironment, processRunner);
    runCheckedProcess(processRunner, 'cargo', [
      'build',
      '-q',
      '--locked',
      '--target',
      host,
      '--target-dir',
      targetDirectory,
      '-p',
      'vo',
      '--bin',
      'vo',
    ], {
      cwd: canonicalRoot,
      env: buildEnvironment,
      encoding: 'utf8',
      maxBuffer: MAX_VO_BUILD_OUTPUT_BYTES,
      timeout: CURRENT_VO_CLI_BUILD_TIMEOUT_MS,
    }, 'current-root Vo CLI build');
    observation = observeVoBinary(
      path.join(targetDirectory, host, 'debug', process.platform === 'win32' ? 'vo.exe' : 'vo'),
      'current-root Vo CLI binary',
    );
    kind = 'current-root-build';
  }
  const authority = Object.freeze({
    binary: observation.path,
    kind,
    root: canonicalRoot,
  });
  VO_BINARY_AUTHORITY_STATE.set(authority, {
    baseEnvironment: { ...environment },
    observation,
    processRunner,
  });
  return authority;
}

function resolveVoBinaryAuthority({
  root,
  voAuthority,
  allowVoBin,
  voBin,
  environment,
  cargoTargetDirectory,
  processRunner,
}) {
  const canonicalRoot = canonicalVoRoot(root);
  if (voAuthority !== null && voAuthority !== undefined) {
    assertQuickplay(
      VO_BINARY_AUTHORITY_STATE.has(voAuthority),
      'voAuthority must come from createVoBinaryAuthority',
    );
    assertQuickplay(
      sameNativePath(voAuthority.root, canonicalRoot),
      'voAuthority belongs to a different Volang root',
    );
    assertQuickplay(!allowVoBin && (voBin === null || voBin === undefined), 'voAuthority cannot be combined with voBin');
    assertQuickplay(
      cargoTargetDirectory === null || cargoTargetDirectory === undefined,
      'voAuthority cannot be combined with a Cargo target directory',
    );
    return voAuthority;
  }
  return createVoBinaryAuthority({
    root: canonicalRoot,
    allowVoBin,
    voBin,
    environment,
    cargoTargetDirectory,
    processRunner,
  });
}

export function invokeVoBinary(authority, args, guestEnvironment) {
  const state = VO_BINARY_AUTHORITY_STATE.get(authority);
  assertQuickplay(state, 'Vo CLI invocation requires a valid binary authority');
  assertQuickplay(Array.isArray(args) && args.length > 0, 'Vo CLI invocation requires arguments');
  assertQuickplay(
    args.every((argument) => typeof argument === 'string' && !argument.includes('\0')),
    'Vo CLI invocation arguments must be strings without NUL bytes',
  );
  const before = observeVoBinary(authority.binary, 'Vo CLI binary before guest execution');
  assertSameVoBinary(state.observation, before, 'Vo CLI binary');
  const result = runCheckedProcess(state.processRunner, authority.binary, args, {
    cwd: authority.root,
    env: restrictedVoGuestEnvironment(state.baseEnvironment, guestEnvironment),
    encoding: 'utf8',
    maxBuffer: MAX_JSON_BYTES,
    timeout: VO_INVOCATION_TIMEOUT_MS,
  }, `vo ${args.join(' ')}`);
  const after = observeVoBinary(authority.binary, 'Vo CLI binary after guest execution');
  assertSameVoBinary(before, after, 'Vo CLI binary during guest execution');
  return String(result.stdout ?? '');
}

export function captureEffectiveSnapshot({
  root,
  projectRoot,
  cacheRoot,
  expectedRoot = 'github.com/vo-lang/blockkart',
  allowVoBin = false,
  voBin = null,
  voAuthority = null,
  environment = process.env,
  cargoTargetDirectory = null,
  processRunner = spawnSync,
}) {
  const authority = resolveVoBinaryAuthority({
    root,
    voAuthority,
    allowVoBin,
    voBin,
    environment,
    cargoTargetDirectory,
    processRunner,
  });
  const output = invokeVoBinary(
    authority,
    ['mod', 'snapshot', projectRoot],
    { VO_MOD_CACHE: cacheRoot, VOWORK: path.join(projectRoot, 'vo.work') },
  );
  const snapshot = parseBoundedJsonBytes(Buffer.from(output, 'utf8'), 'vo mod snapshot output');
  return validateProjectSnapshot(snapshot, expectedRoot);
}

function snapshotInputFileIncluded(relative, includeRootLock) {
  return relative === 'vo.mod'
    || relative === 'vo.work'
    || (includeRootLock && relative === 'vo.lock')
    || isPortableModuleManifestPath(relative)
    || relative.endsWith('.vo');
}

function copyCleanCurrentSourceTree(sourceRoot, destinationRoot, {
  includeRootLock = false,
  excludeNestedModules = false,
  fileIncluded = (relative) => snapshotInputFileIncluded(relative, includeRootLock),
  additionalIgnoredPathspecs = [],
} = {}) {
  assertQuickplay(typeof excludeNestedModules === 'boolean', 'excludeNestedModules must be boolean');
  assertQuickplay(
    Array.isArray(additionalIgnoredPathspecs),
    'additionalIgnoredPathspecs must be an array',
  );
  mkdirSync(destinationRoot, { recursive: true });
  const excluded = new Set(SNAPSHOT_CONTROL_FILES);
  if (!includeRootLock) excluded.add('vo.lock');
  const options = {
    allowDeleted: excluded,
    allowDeletedModuleLocks: true,
  };
  const selectedSnapshot = (label) => {
    const selection = gitCurrentSourceFiles(sourceRoot, {
      ...options,
      additionalFileIncluded: fileIncluded,
      additionalIgnoredPathspecs,
    }).filter((relative) => !excluded.has(relative) && fileIncluded(relative));
    const observation = observeCurrentSourceFilesystemClosure(sourceRoot, selection, label);
    const observedSources = new Set(observation.voSources);
    const boundaries = excludeNestedModules
      ? observation.moduleBoundaries.filter((relative) => path.posix.dirname(relative) !== '.')
      : [];
    const boundaryRootKeys = new Set(
      boundaries.map((relative) => portableCaseKey(path.posix.dirname(relative))),
    );
    const isInsideNestedBoundary = (relative) => {
      const components = relative.split('/');
      for (let length = 1; length < components.length; length += 1) {
        if (boundaryRootKeys.has(portableCaseKey(components.slice(0, length).join('/')))) {
          return true;
        }
      }
      return false;
    };
    const files = excludeNestedModules
      ? selection.filter((relative) => (
          !isInsideNestedBoundary(relative)
          && (!isPortableModuleManifestPath(relative) || relative === 'vo.mod')
          && (!relative.endsWith('.vo') || observedSources.has(relative))
        ))
      : selection;
    const effectivePaths = [...new Set([...files, ...boundaries])].sort(compareUtf8);
    assertQuickplay(
      effectivePaths.length <= MAX_STATIC_ENTRIES,
      `current-source snapshot contains more than ${MAX_STATIC_ENTRIES} files: ${sourceRoot}`,
    );
    return {
      boundaries,
      files,
      signature: JSON.stringify({ effectivePaths, observation }),
    };
  };
  const initial = selectedSnapshot(`${sourceRoot} initial current-source filesystem closure`);
  let totalBytes = 0;
  const sourceFacts = [];
  const captureSourceFact = (relative, suffix = '') => {
    const remaining = MAX_QUICKPLAY_SOURCE_BYTES - totalBytes;
    const source = readRegularFileSnapshot(
      sourceRoot,
      relative,
      `${sourceRoot} ${relative}${suffix}`,
      remaining,
    );
    totalBytes += source.bytes.byteLength;
    assertQuickplay(
      Number.isSafeInteger(totalBytes) && totalBytes <= MAX_QUICKPLAY_SOURCE_BYTES,
      `current-source snapshot exceeds the ${MAX_QUICKPLAY_SOURCE_BYTES}-byte limit: ${sourceRoot}`,
    );
    const fact = {
      path: relative,
      mode: source.mode,
      size: source.bytes.byteLength,
      digest: sha256Digest(source.bytes),
    };
    sourceFacts.push(fact);
    return { source, fact };
  };
  for (const relative of initial.boundaries) {
    captureSourceFact(relative, ' nested module boundary');
  }
  for (const relative of initial.files) {
    const { source } = captureSourceFact(relative);
    const destination = path.join(destinationRoot, ...relative.split('/'));
    mkdirSync(path.dirname(destination), { recursive: true });
    writeFileSync(destination, source.bytes, { flag: 'wx', mode: source.mode });
  }
  const middle = selectedSnapshot(`${sourceRoot} middle current-source filesystem closure`);
  assertQuickplay(
    initial.signature === middle.signature,
    `current-source file set changed while the snapshot was copied: ${sourceRoot}`,
  );
  let verifiedBytes = 0;
  for (const expected of sourceFacts) {
    const remaining = MAX_QUICKPLAY_SOURCE_BYTES - verifiedBytes;
    const current = readRegularFileSnapshot(
      sourceRoot,
      expected.path,
      `${sourceRoot} ${expected.path} post-copy verification`,
      remaining,
    );
    verifiedBytes += current.bytes.byteLength;
    assertQuickplay(
      current.mode === expected.mode
        && current.bytes.byteLength === expected.size
        && sha256Digest(current.bytes) === expected.digest,
      `current-source file changed while the snapshot was copied: ${expected.path} (${sourceRoot})`,
    );
  }
  const final = selectedSnapshot(`${sourceRoot} final current-source filesystem closure`);
  assertQuickplay(
    initial.signature === final.signature,
    `current-source file set changed while the copied bytes were verified: ${sourceRoot}`,
  );
  assertQuickplay(
    lstatSync(path.join(destinationRoot, 'vo.mod')).isFile(),
    `clean workspace snapshot is missing ${sourceRoot}/vo.mod`,
  );
}

export function withCleanEffectiveSnapshot({
  root,
  projectRoot,
  workspaceRoots,
  cacheRoot,
  stagingParent = path.join(root, 'target'),
  expectedRoot = 'github.com/vo-lang/blockkart',
  allowVoBin = false,
  voBin = null,
  voAuthority = null,
  environment = process.env,
  cargoTargetDirectory = null,
  processRunner = spawnSync,
  sourceFileIncluded = null,
  sourceIgnoredPathspecs = null,
  excludeNestedModules = false,
}, consume) {
  assertQuickplay(typeof consume === 'function', 'clean snapshot consumer must be a function');
  assertQuickplay(
    sourceFileIncluded === null || typeof sourceFileIncluded === 'function',
    'sourceFileIncluded must be a function or null',
  );
  assertQuickplay(
    sourceIgnoredPathspecs === null || typeof sourceIgnoredPathspecs === 'function',
    'sourceIgnoredPathspecs must be a function or null',
  );
  const authority = resolveVoBinaryAuthority({
    root,
    voAuthority,
    allowVoBin,
    voBin,
    environment,
    cargoTargetDirectory,
    processRunner,
  });
  mkdirSync(stagingParent, { recursive: true });
  const stagingRoot = mkdtempSync(path.join(stagingParent, '.quickplay-workspace-'));
  try {
    const sources = [projectRoot, ...workspaceRoots].map((sourceRoot) => {
      const name = path.basename(realpathSync.native(sourceRoot));
      assertQuickplay(name.length > 0 && name !== '.' && name !== '..', `workspace root name is invalid: ${sourceRoot}`);
      return {
        sourceRoot: realpathSync.native(sourceRoot),
        stagingRoot: path.join(stagingRoot, name),
      };
    });
    assertQuickplay(
      new Set(sources.map((source) => source.stagingRoot)).size === sources.length,
      'workspace roots must have distinct directory names',
    );
    for (const [index, source] of sources.entries()) {
      const includeRootLock = index === 0;
      copyCleanCurrentSourceTree(source.sourceRoot, source.stagingRoot, {
        includeRootLock,
        excludeNestedModules,
        fileIncluded: sourceFileIncluded === null
          ? (relative) => snapshotInputFileIncluded(relative, includeRootLock)
          : (relative) => sourceFileIncluded(index, relative),
        additionalIgnoredPathspecs: sourceIgnoredPathspecs === null
          ? []
          : sourceIgnoredPathspecs(index),
      });
    }
    const stagedProject = sources[0].stagingRoot;
    assertQuickplay(
      lstatSync(path.join(stagedProject, 'vo.work')).isFile(),
      'clean BlockKart snapshot is missing vo.work',
    );
    const snapshot = captureEffectiveSnapshot({
      root,
      projectRoot: stagedProject,
      cacheRoot,
      expectedRoot,
      voAuthority: authority,
    });
    const originalByStaged = new Map(sources.map((source) => [
      realpathSync.native(source.stagingRoot),
      source.sourceRoot,
    ]));
    for (const module of snapshot.modules) {
      if (module.source.kind !== 'workspace') continue;
      assertQuickplay(
        originalByStaged.has(realpathSync.native(module.source.directory)),
        `${module.module} resolved outside the clean workspace snapshot`,
      );
    }
    return consume({
      snapshot: validateProjectSnapshot(snapshot, expectedRoot),
      projectRoot: stagedProject,
      originalByStaged,
    });
  } finally {
    rmSync(stagingRoot, { recursive: true, force: true });
  }
}

export function captureCleanEffectiveSnapshot(options) {
  return withCleanEffectiveSnapshot(options, ({ snapshot, originalByStaged }) => {
    for (const module of snapshot.modules) {
      if (module.source.kind !== 'workspace') continue;
      module.source.directory = originalByStaged.get(realpathSync.native(module.source.directory));
    }
    return validateProjectSnapshot(snapshot, options.expectedRoot ?? 'github.com/vo-lang/blockkart');
  });
}

export function fetchAuthenticatedModules({
  root,
  projectRoot,
  cacheRoot,
  allowVoBin = false,
  voBin = null,
  voAuthority = null,
  environment = process.env,
  guestEnvironment = {},
  cargoTargetDirectory = null,
  processRunner = spawnSync,
}) {
  const authority = resolveVoBinaryAuthority({
    root,
    voAuthority,
    allowVoBin,
    voBin,
    environment,
    cargoTargetDirectory,
    processRunner,
  });
  const fetchEnvironment = {
    ...guestEnvironment,
    VO_MOD_CACHE: cacheRoot,
    VOWORK: 'off',
  };
  invokeVoBinary(authority, ['mod', 'fetch', projectRoot], fetchEnvironment);
  const output = invokeVoBinary(
    authority,
    ['mod', 'snapshot', projectRoot],
    fetchEnvironment,
  );
  return validateProjectSnapshot(
    parseBoundedJsonBytes(Buffer.from(output, 'utf8'), 'vo mod snapshot output'),
    'github.com/vo-lang/blockkart',
  );
}

function sourceFileEntry(entry, index, label) {
  exactKeys(entry, ['path', 'mode', 'size', 'digest'], `${label}[${index}]`);
  validateRelativePath(entry.path, `${label}[${index}].path`);
  assertQuickplay(
    entry.mode === 'regular' || entry.mode === 'executable',
    `${label}[${index}].mode is invalid`,
  );
  assertQuickplay(
    Number.isSafeInteger(entry.size)
      && entry.size >= 0
      && entry.size <= MAX_SOURCE_ARCHIVE_ENTRY_BYTES,
    `${label}[${index}].size is invalid`,
  );
  assertQuickplay(
    !(
      (entry.path === 'vo.mod' || entry.path.endsWith('.vo'))
      && entry.size > MAX_JSON_BYTES
    ),
    `${label}[${index}].size exceeds the Vo text-file limit`,
  );
  assertQuickplay(DIGEST.test(entry.digest), `${label}[${index}].digest is invalid`);
  return {
    path: entry.path,
    mode: entry.mode,
    size: entry.size,
    digest: entry.digest,
  };
}

function canonicalProtocolJsonString(value) {
  let output = '"';
  for (const character of value) {
    const codepoint = character.codePointAt(0);
    assertQuickplay(
      codepoint < 0xd800 || codepoint > 0xdfff,
      'canonical protocol JSON strings must contain Unicode scalar values',
    );
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

export function canonicalProtocolJsonText(value, depth = 0) {
  assertQuickplay(
    Number.isSafeInteger(depth) && depth >= 0 && depth <= MAX_JSON_DEPTH,
    `canonical JSON depth exceeds ${MAX_JSON_DEPTH}`,
  );
  if (value === null) return 'null';
  if (typeof value === 'string') return canonicalProtocolJsonString(value);
  if (typeof value === 'number') {
    assertQuickplay(
      Number.isSafeInteger(value) && value >= 0,
      'canonical protocol JSON numbers must be non-negative safe integers',
    );
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
  assertQuickplay(
    value && typeof value === 'object' && Object.getPrototypeOf(value) === Object.prototype,
    'canonical protocol JSON values must use plain objects',
  );
  const entries = Object.entries(value);
  if (entries.length === 0) return '{}';
  return `{\n${entries
    .map(([key, entry]) => (
      `${childIndentation}${canonicalProtocolJsonString(key)}: ${canonicalProtocolJsonText(entry, depth + 1)}`
    ))
    .join(',\n')}\n${indentation}}`;
}

function canonicalPrettyJsonBytes(value) {
  return Buffer.from(`${canonicalProtocolJsonText(value)}\n`, 'utf8');
}

function assertCanonicalPrettyJsonBytes(bytes, canonical, label) {
  assertQuickplay(
    Buffer.from(bytes).equals(canonicalPrettyJsonBytes(canonical)),
    `${label} must use canonical pretty JSON with exactly one trailing LF`,
  );
}

function staticFile(relative, bytes, mode = 0o644) {
  assertQuickplay(mode === 0o644 || mode === 0o755, `static file ${relative} mode is invalid`);
  const entry = {
    path: relative,
    mode,
    size: bytes.byteLength,
    digest: sha256Digest(bytes),
  };
  try {
    const text = UTF8.decode(bytes);
    if (Buffer.from(text, 'utf8').equals(bytes)) entry.content = text;
    else entry.contentBase64 = bytes.toString('base64');
  } catch {
    entry.contentBase64 = bytes.toString('base64');
  }
  return entry;
}

export function encodeStaticFile(relative, bytes, mode = 0o644) {
  validateRelativePath(relative, 'static file path');
  return staticFile(relative, Buffer.from(bytes), mode);
}

export function staticFileBytes(file, label = 'static file') {
  exactKeys(
    file,
    file.content !== undefined
      ? ['path', 'mode', 'size', 'digest', 'content']
      : ['path', 'mode', 'size', 'digest', 'contentBase64'],
    label,
  );
  validateRelativePath(file.path, `${label}.path`);
  assertQuickplay(file.mode === 0o644 || file.mode === 0o755, `${label}.mode is invalid`);
  if (file.content !== undefined) {
    assertQuickplay(typeof file.content === 'string', `${label}.content must be text`);
    const bytes = Buffer.from(file.content, 'utf8');
    verifyBoundBytes(bytes, file, label);
    return bytes;
  }
  assertQuickplay(typeof file.contentBase64 === 'string', `${label}.contentBase64 must be text`);
  const bytes = Buffer.from(file.contentBase64, 'base64');
  assertQuickplay(bytes.toString('base64') === file.contentBase64, `${label}.contentBase64 is not canonical`);
  verifyBoundBytes(bytes, file, label);
  return bytes;
}

function staticFilesMap(files, label) {
  assertQuickplay(Array.isArray(files) && files.length <= MAX_STATIC_ENTRIES, `${label} contains too many entries`);
  const result = new Map();
  const portablePaths = new PortablePathTrie(
    MAX_STATIC_PATH_NODES,
    MAX_STATIC_PATH_KEY_BYTES,
  );
  let previous = '';
  let totalBytes = 0;
  for (const [index, file] of files.entries()) {
    const bytes = staticFileBytes(file, `${label}[${index}]`);
    assertQuickplay(compareUtf8(previous, file.path) < 0, `${label} must be sorted and unique`);
    previous = file.path;
    portablePaths.insert(file.path, false, label);
    totalBytes += bytes.byteLength;
    assertQuickplay(
      Number.isSafeInteger(totalBytes) && totalBytes <= MAX_QUICKPLAY_SOURCE_BYTES,
      `${label} exceeds the Quickplay aggregate source-byte limit`,
    );
    result.set(file.path, bytes);
  }
  return result;
}

function assertPortableFileClosure(paths, label, absolute = false) {
  const portablePaths = new PortablePathTrie(
    MAX_STATIC_PATH_NODES,
    MAX_STATIC_PATH_KEY_BYTES,
  );
  for (const relative of paths) {
    validateRelativePath(relative, `${label} path`);
    assertQuickplay(
      !absolute || Buffer.byteLength(relative, 'utf8') + 1 <= MAX_PATH_BYTES,
      `${label} path exceeds the ${MAX_PATH_BYTES}-byte VFS path limit`,
    );
    portablePaths.insert(relative, false, label);
  }
  return portablePaths.nodeCount;
}

function artifactReleaseAssetName(artifact) {
  const identity = `vo-artifact-asset-v1\0${artifact.kind}\0${artifact.target}\0${artifact.name}`;
  return `vo-artifact-v1-${sha256Digest(Buffer.from(identity, 'utf8')).slice('sha256:'.length)}`;
}

export function validateRelease(release, snapshotModule) {
  exactKeys(
    release,
    ['schema_version', 'module', 'version', 'commit', 'vo', 'dependencies', 'source', 'package', 'artifacts'],
    `${snapshotModule.module} vo.release.json`,
  );
  assertQuickplay(release.schema_version === RELEASE_SCHEMA, `${snapshotModule.module} release schema must be ${RELEASE_SCHEMA}`);
  assertQuickplay(release.module === snapshotModule.module, `${snapshotModule.module} release identity mismatch`);
  assertQuickplay(release.version === snapshotModule.version, `${snapshotModule.module} release version mismatch`);
  const releaseMajor = exactVersionMajor(release.version, `${snapshotModule.module} release version`);
  validateModuleForMajor(release.module, releaseMajor, `${snapshotModule.module} release module`);
  assertQuickplay(/^[0-9a-f]{40}$/.test(release.commit), `${snapshotModule.module} release commit is invalid`);
  assertQuickplay(release.vo === snapshotModule.vo, `${snapshotModule.module} release toolchain mismatch`);
  const dependencies = normalizedDependencies(release.dependencies, `${snapshotModule.module} release dependencies`);
  assertQuickplay(
    dependencies.every((dependency) => dependency.module !== release.module),
    `${snapshotModule.module} release must not depend on itself`,
  );
  assertQuickplay(JSON.stringify(dependencies) === JSON.stringify(snapshotModule.dependencies), `${snapshotModule.module} release dependency graph mismatch`);
  exactKeys(release.source, ['name', 'size', 'digest'], `${snapshotModule.module} source binding`);
  assertQuickplay(
    release.source.name === SOURCE_ARCHIVE_ASSET_NAME,
    `${snapshotModule.module} source name must be ${SOURCE_ARCHIVE_ASSET_NAME}`,
  );
  assertQuickplay(
    Number.isSafeInteger(release.source.size)
    && release.source.size > 0
    && release.source.size <= MAX_SOURCE_ARCHIVE_BYTES,
    `${snapshotModule.module} source size is invalid`,
  );
  assertQuickplay(DIGEST.test(release.source.digest), `${snapshotModule.module} source digest is invalid`);
  exactKeys(release.package, ['size', 'digest'], `${snapshotModule.module} package binding`);
  digestBinding(release.package, `${snapshotModule.module} package binding`);
  assertQuickplay(
    release.package.size > 0 && release.package.size <= MAX_JSON_BYTES,
    `${snapshotModule.module} package size is invalid`,
  );
  assertQuickplay(
    Array.isArray(release.artifacts) && release.artifacts.length <= MAX_MODULE_ARTIFACTS,
    `${snapshotModule.module} artifacts must be a bounded array`,
  );
  let previousArtifact = null;
  const artifacts = [];
  const releaseAssetNames = new Set([
    portableCaseKey('vo.release.json'),
    portableCaseKey('vo.package.json'),
    portableCaseKey(release.source.name),
  ]);
  for (const [index, artifact] of release.artifacts.entries()) {
    exactKeys(artifact, ['kind', 'target', 'name', 'size', 'digest'], `${snapshotModule.module} artifacts[${index}]`);
    try {
      validateArtifactIdentity(artifact);
    } catch (error) {
      throw new Error(`${snapshotModule.module} artifacts[${index}]: ${error instanceof Error ? error.message : String(error)}`);
    }
    digestBinding(artifact, `${snapshotModule.module} artifacts[${index}]`);
    assertQuickplay(
      artifact.size > 0 && artifact.size <= MAX_MODULE_ARTIFACT_BYTES,
      `${snapshotModule.module} artifacts[${index}].size is invalid`,
    );
    assertQuickplay(
      !releaseAssetNames.has(portableCaseKey(artifactReleaseAssetName(artifact))),
      `${snapshotModule.module} artifacts[${index}] reuses a release asset name`,
    );
    releaseAssetNames.add(portableCaseKey(artifactReleaseAssetName(artifact)));
    assertQuickplay(
      previousArtifact === null || compareArtifactIdentity(previousArtifact, artifact) < 0,
      `${snapshotModule.module} artifacts must be sorted and unique`,
    );
    previousArtifact = artifact;
    artifacts.push({
      kind: artifact.kind,
      target: artifact.target,
      name: artifact.name,
      size: artifact.size,
      digest: artifact.digest,
    });
  }
  return {
    schema_version: release.schema_version,
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
    package: { size: release.package.size, digest: release.package.digest },
    artifacts,
  };
}

export function validatePackageManifest(packageManifest, label) {
  exactKeys(packageManifest, ['schema_version', 'files'], label);
  assertQuickplay(packageManifest.schema_version === PACKAGE_SCHEMA, `${label} schema must be ${PACKAGE_SCHEMA}`);
  assertQuickplay(
    Array.isArray(packageManifest.files)
    && packageManifest.files.length > 0
    && packageManifest.files.length <= MAX_STATIC_ENTRIES,
    `${label} files must be a bounded non-empty array`,
  );
  let previous = '';
  const files = packageManifest.files.map((entry, index) => {
    const canonical = sourceFileEntry(entry, index, `${label} files`);
    const components = canonical.path.split('/');
    const first = portableCaseKey(components[0]);
    const basename = portableCaseKey(components.at(-1));
    assertQuickplay(
      !['artifacts', '.vo-version', '.vo-source-digest', 'vo.release.json', 'vo.package.json', 'source.tar.gz'].includes(first)
      && !(components.length === 1 && ['vo.lock', 'vo.work', 'vo.sum', 'vo.web.json', '.vo-project.lock', '.vo-project.transaction'].includes(basename))
      && (basename !== 'vo.mod' || canonical.path === 'vo.mod'),
      `${label} files[${index}].path is reserved by the module protocol`,
    );
    assertQuickplay(compareUtf8(previous, canonical.path) < 0, `${label} files must be sorted and unique`);
    previous = canonical.path;
    return canonical;
  });
  const modFile = files.find((entry) => entry.path === 'vo.mod');
  assertQuickplay(
    modFile && modFile.mode === 'regular' && modFile.size > 0,
    `${label} files must contain a non-empty regular root vo.mod`,
  );
  return { schema_version: packageManifest.schema_version, files };
}

function packageRegistryModule(snapshotModule, outputRoot) {
  const moduleDir = realpathSync.native(snapshotModule.source.directory);
  const releaseBytes = readRegularFile(
    moduleDir,
    'vo.release.json',
    `${snapshotModule.module} vo.release.json`,
    MAX_JSON_BYTES,
  );
  assertQuickplay(sha256Digest(releaseBytes) === snapshotModule.release, `${snapshotModule.module} release bytes differ from vo.lock`);
  const release = validateRelease(
    parseBoundedJsonBytes(releaseBytes, `${snapshotModule.module} vo.release.json`),
    snapshotModule,
  );
  assertCanonicalPrettyJsonBytes(releaseBytes, release, `${snapshotModule.module} vo.release.json`);
  const packageBytes = readRegularFile(
    moduleDir,
    'vo.package.json',
    `${snapshotModule.module} vo.package.json`,
    MAX_PROTOCOL_METADATA_BYTES,
  );
  verifyBoundBytes(packageBytes, release.package, `${snapshotModule.module} package binding`);
  const packageManifest = validatePackageManifest(
    parseBoundedJsonBytes(packageBytes, `${snapshotModule.module} vo.package.json`),
    `${snapshotModule.module} vo.package.json`,
  );
  assertCanonicalPrettyJsonBytes(packageBytes, packageManifest, `${snapshotModule.module} vo.package.json`);

  const files = [];
  let totalBytes = 0;
  let extractedBytes = packageBytes.byteLength;
  let previous = '';
  for (const [index, rawEntry] of packageManifest.files.entries()) {
    const entry = sourceFileEntry(rawEntry, index, `${snapshotModule.module} package files`);
    assertQuickplay(compareUtf8(previous, entry.path) < 0, `${snapshotModule.module} package files must be sorted and unique`);
    previous = entry.path;
    const bytes = readRegularFile(moduleDir, entry.path, `${snapshotModule.module} ${entry.path}`);
    verifyBoundBytes(bytes, entry, `${snapshotModule.module} ${entry.path}`);
    totalBytes += bytes.byteLength;
    extractedBytes += bytes.byteLength;
    assertQuickplay(
      Number.isSafeInteger(extractedBytes) && extractedBytes <= MAX_EXTRACTED_SOURCE_BYTES,
      `${snapshotModule.module} package exceeds the protocol extracted-source limit`,
    );
    assertQuickplay(Number.isSafeInteger(totalBytes) && totalBytes <= MAX_QUICKPLAY_SOURCE_BYTES, `${snapshotModule.module} Quickplay source closure is too large`);
    files.push(staticFile(entry.path, bytes, entry.mode === 'executable' ? 0o755 : 0o644));
  }
  files.push(staticFile('vo.package.json', packageBytes));
  files.push(staticFile('vo.release.json', releaseBytes));
  files.push(staticFile('.vo-version', Buffer.from(`${snapshotModule.version}\n`, 'utf8')));
  files.push(staticFile('.vo-source-digest', Buffer.from(`${release.source.digest}\n`, 'utf8')));
  files.sort((left, right) => compareUtf8(left.path, right.path));

  const cacheDir = moduleCacheDir(snapshotModule.module, snapshotModule.version);
  const artifacts = [];
  for (const [index, artifact] of release.artifacts.entries()) {
    exactKeys(artifact, ['kind', 'target', 'name', 'size', 'digest'], `${snapshotModule.module} artifacts[${index}]`);
    if (artifact.kind === 'extension-native') continue;
    assertQuickplay(['extension-wasm', 'extension-js-glue'].includes(artifact.kind), `${snapshotModule.module} has an unsupported browser artifact kind`);
    assertQuickplay(artifact.target === 'wasm32-unknown-unknown', `${snapshotModule.module} browser artifact target is invalid`);
    assertQuickplay(typeof artifact.name === 'string' && artifact.name.length > 0, `${snapshotModule.module} artifact name is invalid`);
    assertQuickplay(!artifact.name.includes('/') && !artifact.name.includes('\\'), `${snapshotModule.module} artifact name is invalid`);
    const relative = artifactCachePath(artifact);
    const bytes = readRegularFile(moduleDir, relative, `${snapshotModule.module} ${relative}`);
    verifyBoundBytes(bytes, artifact, `${snapshotModule.module} ${relative}`);
    const publishedRelative = artifactOutputRelativePath(cacheDir, artifact);
    const published = path.join(outputRoot, ...publishedRelative.split('/'));
    artifacts.push({
      ...artifact,
      path: relative,
      url: quickplayArtifactUrl(cacheDir, artifact),
      bytes,
      published,
      publishedRelative,
    });
  }
  artifacts.sort(compareArtifactIdentity);
  return {
    source: 'registry',
    module: snapshotModule.module,
    version: snapshotModule.version,
    commit: release.commit,
    release: snapshotModule.release,
    cacheDir,
    files,
    artifacts,
  };
}

function packageWorkspaceModule(
  snapshotModule,
  outputRoot,
  workspaceArtifacts,
  workspaceFileIncluded,
) {
  const moduleDir = realpathSync.native(snapshotModule.source.directory);
  const sourceFiles = filesOutsideNestedModules(
    listRegularFiles(moduleDir),
    `${snapshotModule.module} workspace source closure`,
  );
  const files = [];
  let totalBytes = 0;
  for (const relative of sourceFiles) {
    validateRelativePath(relative, `${snapshotModule.module} workspace file`);
    if (hasReservedWorkspacePath(relative)) continue;
    if (!workspaceFileIncluded(snapshotModule, relative)) continue;
    const source = readRegularFileSnapshot(
      moduleDir,
      relative,
      `${snapshotModule.module} ${relative}`,
    );
    totalBytes += source.bytes.byteLength;
    assertQuickplay(Number.isSafeInteger(totalBytes) && totalBytes <= MAX_QUICKPLAY_SOURCE_BYTES, `${snapshotModule.module} Quickplay workspace snapshot is too large`);
    files.push(staticFile(relative, source.bytes, source.mode));
  }
  const voMod = files.find((file) => file.path === 'vo.mod');
  assertQuickplay(voMod, `${snapshotModule.module} workspace snapshot is missing vo.mod`);
  assertCanonicalModIntent(
    UTF8.decode(staticFileBytes(voMod, `${snapshotModule.module} vo.mod`)),
    snapshotModule,
    `${snapshotModule.module} workspace vo.mod`,
  );
  assertQuickplay(files.length <= MAX_STATIC_ENTRIES, `${snapshotModule.module} workspace snapshot contains too many files`);
  files.sort((left, right) => compareUtf8(left.path, right.path));

  const cacheDir = `workspace/${modulePathCacheKey(snapshotModule.module).cacheComponent}`;
  const artifacts = [];
  for (const descriptor of workspaceArtifacts(snapshotModule)) {
    exactKeys(
      descriptor,
      ['kind', 'target', 'name', 'bytes', 'size', 'digest', 'installPath'],
      `${snapshotModule.module} workspace artifact`,
    );
    assertQuickplay(['extension-wasm', 'extension-js-glue'].includes(descriptor.kind), `${snapshotModule.module} workspace artifact kind is invalid`);
    assertQuickplay(descriptor.target === 'wasm32-unknown-unknown', `${snapshotModule.module} workspace artifact target is invalid`);
    assertQuickplay(
      typeof descriptor.name === 'string'
      && descriptor.name.length > 0
      && !descriptor.name.includes('/')
      && !descriptor.name.includes('\\')
      && !/[\u0000-\u001f\u007f]/u.test(descriptor.name),
      `${snapshotModule.module} workspace artifact name is invalid`,
    );
    assertQuickplay(Buffer.isBuffer(descriptor.bytes), `${snapshotModule.module} workspace artifact bytes are invalid`);
    const bytes = Buffer.from(descriptor.bytes);
    assertQuickplay(
      bytes.byteLength > 0 && bytes.byteLength <= MAX_FILE_BYTES,
      `${snapshotModule.module} workspace artifact size is invalid`,
    );
    verifyBoundBytes(bytes, descriptor, `${snapshotModule.module} workspace artifact`);
    const identity = {
      kind: descriptor.kind,
      target: descriptor.target,
      name: descriptor.name,
    };
    validateRelativePath(descriptor.installPath, `${snapshotModule.module} workspace artifact install path`);
    assertQuickplay(
      !files.some((file) => portableCaseKey(file.path) === portableCaseKey(descriptor.installPath)),
      `${snapshotModule.module} workspace artifact collides with ${descriptor.installPath}`,
    );
    const publishedRelative = artifactOutputRelativePath(cacheDir, identity);
    artifacts.push({
      ...identity,
      size: bytes.byteLength,
      digest: sha256Digest(bytes),
      path: descriptor.installPath,
      url: quickplayArtifactUrl(cacheDir, identity),
      bytes,
      published: path.join(outputRoot, ...publishedRelative.split('/')),
      publishedRelative,
    });
    assertQuickplay(
      artifacts.length <= MAX_MODULE_ARTIFACTS,
      `${snapshotModule.module} contains too many workspace artifacts`,
    );
  }
  artifacts.sort(compareArtifactIdentity);
  assertPortableFileClosure(
    [
      ...files.map((file) => file.path),
      ...artifacts.flatMap((artifact) => [
        artifact.path,
        artifactCachePath(artifact),
      ]),
    ],
    `${snapshotModule.module} workspace install closure`,
  );
  return {
    source: 'workspace',
    module: snapshotModule.module,
    cacheDir,
    files,
    artifacts,
  };
}

export function packageSnapshotModule(snapshotModule, outputRoot, options = {}) {
  if (snapshotModule.source.kind === 'registry') {
    return packageRegistryModule(snapshotModule, outputRoot);
  }
  const workspaceArtifacts = options.workspaceArtifacts ?? (() => []);
  const workspaceFileIncluded = options.workspaceFileIncluded ?? (() => true);
  return packageWorkspaceModule(
    snapshotModule,
    outputRoot,
    workspaceArtifacts,
    workspaceFileIncluded,
  );
}

export function publicDependencyModule(module) {
  return {
    source: module.source,
    module: module.module,
    ...(module.version === undefined ? {} : { version: module.version }),
    cacheDir: module.cacheDir,
    files: module.files,
    artifacts: module.artifacts.map(({ bytes: _bytes, published: _published, publishedRelative: _relative, ...artifact }) => artifact),
  };
}

export function portableProjectSnapshot(snapshot) {
  snapshot = validateProjectSnapshot(snapshot, 'github.com/vo-lang/blockkart');
  const hasWorkspaceModules = snapshot.modules.some((module) => module.source.kind === 'workspace');
  return {
    schema_version: snapshot.schema_version,
    mode: snapshot.mode,
    authority: snapshot.authority,
    root: snapshot.root,
    ...(hasWorkspaceModules ? { workspace: { file: 'vo.work' } } : {}),
    modules: snapshot.modules.map((module) => {
      const source = module.source.kind === 'registry'
        ? { kind: 'registry', directory: moduleCacheDir(module.module, module.version) }
        : {
            kind: 'workspace',
            directory: `../modules/${modulePathCacheKey(module.module).cacheComponent}`,
          };
      if (snapshot.authority === 'lock') {
        return {
          module: module.module,
          version: module.version,
          vo: module.vo,
          release: module.release,
          source,
          dependencies: module.dependencies,
        };
      }
      return {
        module: module.module,
        vo: module.vo,
        source,
        dependencies: module.dependencies,
      };
    }),
  };
}

export function validatePortableProjectSnapshot(snapshot, expectedRoot) {
  snapshot = validateProjectSnapshot(snapshot, expectedRoot);
  assertQuickplay(
    snapshot.authority !== 'empty',
    'Quickplay requires a non-empty lock- or workspace-authoritative ProjectSnapshot',
  );
  let workspaceModules = 0;
  for (const module of snapshot.modules) {
    if (module.source.kind === 'registry') {
      assertQuickplay(
        module.source.directory === moduleCacheDir(module.module, module.version),
        `${module.module} portable registry directory is not canonical`,
      );
    } else {
      workspaceModules += 1;
      assertQuickplay(
        module.source.directory === `../modules/${modulePathCacheKey(module.module).cacheComponent}`,
        `${module.module} portable workspace directory is not canonical`,
      );
    }
  }
  if (workspaceModules === 0) {
    assertQuickplay(snapshot.workspace === undefined, 'portable registry graph must omit workspace state');
  } else {
    assertQuickplay(snapshot.workspace?.file === 'vo.work', 'portable workspace file must be vo.work');
  }
  return snapshot;
}

export function validateQuickplayPackage(packageRoot) {
  const project = readJsonFile(
    path.join(packageRoot, 'project.json'),
    'Quickplay project.json',
    { maxBytes: MAX_PACKAGE_JSON_BYTES },
  );
  exactKeys(project, ['schemaVersion', 'name', 'module', 'baseCommit', 'snapshot', 'files'], 'Quickplay project');
  assertQuickplay(project.schemaVersion === QUICKPLAY_PROJECT_SCHEMA, 'Quickplay project schema mismatch');
  assertQuickplay(project.name === 'BlockKart' && project.module === 'github.com/vo-lang/blockkart', 'Quickplay project identity mismatch');
  assertQuickplay(/^[0-9a-f]{40}$/.test(project.baseCommit), 'Quickplay project base commit is invalid');
  const snapshot = validatePortableProjectSnapshot(project.snapshot, project.module);
  assertQuickplay(
    JSON.stringify(project.snapshot) === JSON.stringify(snapshot),
    'Quickplay project snapshot must use canonical field order and value shape',
  );
  const projectFiles = staticFilesMap(project.files, 'project files');
  for (const relative of projectFiles.keys()) {
    const components = relative.split('/');
    const rootKey = portableCaseKey(components[0]);
    const basename = portableCaseKey(components.at(-1));
    assertQuickplay(
      !(components.length === 1 && PROJECT_RESERVED_FILES.has(rootKey) && !PROJECT_ROOT_CONTROL_FILES.has(relative))
      && !(basename === 'vo.mod' && relative !== 'vo.mod')
      && !(components.length === 1 && PROJECT_ROOT_CONTROL_FILES.has(rootKey) && relative !== rootKey),
      `Quickplay project contains reserved protocol or internal metadata: ${relative}`,
    );
  }
  assertQuickplay(projectFiles.has('main.vo'), 'Quickplay project is missing main.vo');
  assertQuickplay(projectFiles.has('vo.mod'), 'Quickplay project is missing vo.mod');
  const vpakBytes = projectFiles.get(BLOCKKART_VPAK_PATH);
  const vpakProducerBytes = projectFiles.get(BLOCKKART_VPAK_PRODUCER_PATH);
  assertQuickplay(vpakBytes !== undefined, `Quickplay project is missing ${BLOCKKART_VPAK_PATH}`);
  assertQuickplay(
    vpakProducerBytes !== undefined,
    `Quickplay project is missing ${BLOCKKART_VPAK_PRODUCER_PATH}`,
  );
  const vpakProducer = parseBlockKartVpakProducerManifestBytes(
    vpakProducerBytes,
    'Quickplay BlockKart VPAK producer manifest',
  );
  assertQuickplay(
    vpakBytes.byteLength === vpakProducer.pack.size
      && sha256Digest(vpakBytes) === `sha256:${vpakProducer.pack.sha256}`,
    'Quickplay BlockKart VPAK bytes do not match their producer manifest',
  );
  const modText = UTF8.decode(projectFiles.get('vo.mod') ?? Buffer.alloc(0));
  assertQuickplay(
    modText === canonicalModIntent(snapshot.root),
    'Quickplay project vo.mod must canonically match snapshot root intent',
  );
  const lockBytes = projectFiles.get('vo.lock');
  const expectedLock = renderCanonicalVoLock(snapshot);
  if (snapshot.authority === 'lock') {
    assertQuickplay(lockBytes !== undefined, 'Lock-authoritative Quickplay projects must carry vo.lock v3');
    assertQuickplay(
      UTF8.decode(lockBytes) === expectedLock,
      'Quickplay vo.lock must be canonical v3 and exactly match the project snapshot',
    );
  } else {
    assertQuickplay(
      lockBytes === undefined && expectedLock === null,
      'Quickplay projects without lock authority must omit vo.lock',
    );
  }
  const workspaceModules = snapshot.modules.filter((module) => module.source.kind === 'workspace');
  const expectedWorkText = quickplayWorkspaceFile(workspaceModules);
  const workBytes = projectFiles.get('vo.work');
  if (expectedWorkText === null) {
    assertQuickplay(workBytes === undefined, 'Registry-only Quickplay projects must omit vo.work');
  } else {
    assertQuickplay(
      workBytes !== undefined && UTF8.decode(workBytes) === expectedWorkText,
      'Workspace Quickplay project vo.work is not the canonical member closure',
    );
  }

  const deps = readJsonFile(
    path.join(packageRoot, 'deps.json'),
    'Quickplay deps.json',
    { maxBytes: MAX_PACKAGE_JSON_BYTES },
  );
  exactKeys(deps, ['schemaVersion', 'name', 'snapshotDigest', 'modules'], 'Quickplay dependencies');
  assertQuickplay(deps.schemaVersion === QUICKPLAY_DEPS_SCHEMA && deps.name === 'BlockKart dependencies', 'Quickplay dependency identity mismatch');
  assertQuickplay(deps.snapshotDigest === sha256Digest(Buffer.from(JSON.stringify(snapshot), 'utf8')), 'Quickplay snapshot binding mismatch');
  assertQuickplay(
    Array.isArray(deps.modules)
    && deps.modules.length === snapshot.modules.length
    && deps.modules.length <= MAX_STATIC_MODULES,
    'Quickplay dependency count mismatch',
  );
  assertQuickplay(
    JSON.stringify(deps.modules.map((module) => module?.module))
      === JSON.stringify(snapshot.modules.map((module) => module.module)),
    'Quickplay dependency modules must match core snapshot order',
  );
  const snapshotByModule = new Map(snapshot.modules.map((module) => [module.module, module]));
  const physicalFiles = new Set(['deps.json', 'project.json', 'provenance.json']);
  let totalEntries = projectFiles.size;
  const aggregateInstallClosurePaths = [...projectFiles.keys()]
    .map((relative) => `${QUICKPLAY_PROJECT_VFS_ROOT}/${relative}`);
  let totalArtifactBytes = 0;
  for (const module of deps.modules) {
    assertQuickplay(module && typeof module === 'object' && !Array.isArray(module), 'dependency package must be an object');
    exactKeys(
      module,
      module.source === 'registry'
        ? ['source', 'module', 'version', 'cacheDir', 'files', 'artifacts']
        : ['source', 'module', 'cacheDir', 'files', 'artifacts'],
      `${module?.module ?? 'dependency'} package`,
    );
    const snapshotModule = snapshotByModule.get(module.module);
    assertQuickplay(snapshotModule && snapshotModule.source.kind === module.source, `${module.module} is absent from the core snapshot`);
    const expectedCacheDir = module.source === 'registry'
      ? moduleCacheDir(module.module, snapshotModule.version)
      : `workspace/${modulePathCacheKey(module.module).cacheComponent}`;
    if (module.source === 'registry') {
      assertQuickplay(module.version === snapshotModule.version, `${module.module} version mismatch`);
    }
    assertQuickplay(module.cacheDir === expectedCacheDir, `${module.module} cacheDir mismatch`);
    const files = staticFilesMap(module.files, `${module.module} files`);
    const fileDescriptors = new Map(module.files.map((file) => [file.path, file]));
    assertQuickplay(
      Array.isArray(module.artifacts) && module.artifacts.length <= MAX_MODULE_ARTIFACTS,
      `${module.module} contains too many artifacts`,
    );
    totalEntries += module.files.length + module.artifacts.length * (module.source === 'workspace' ? 2 : 1);
    assertQuickplay(
      Number.isSafeInteger(totalEntries) && totalEntries <= MAX_STATIC_ENTRIES,
      'Quickplay dependency package contains too many aggregate entries',
    );
    let releaseArtifacts = null;
    let requiredReleaseArtifacts = null;
    if (module.source === 'registry') {
      const releaseBytes = files.get('vo.release.json');
      const packageBytes = files.get('vo.package.json');
      assertQuickplay(releaseBytes && packageBytes, `${module.module} is missing release/package metadata`);
      assertQuickplay(
        releaseBytes.byteLength <= MAX_JSON_BYTES,
        `${module.module} vo.release.json exceeds the metadata byte limit`,
      );
      assertQuickplay(
        fileDescriptors.get('vo.release.json')?.mode === 0o644
        && fileDescriptors.get('vo.package.json')?.mode === 0o644,
        `${module.module} release/package metadata mode must be 0644`,
      );
      const fullSnapshotModule = { ...snapshotModule, source: { ...snapshotModule.source, directory: '.' } };
      assertQuickplay(sha256Digest(releaseBytes) === snapshotModule.release, `${module.module} release differs from the core snapshot`);
      const release = validateRelease(
        parseBoundedJsonBytes(releaseBytes, `${module.module} vo.release.json`),
        fullSnapshotModule,
      );
      assertCanonicalPrettyJsonBytes(releaseBytes, release, `${module.module} vo.release.json`);
      requiredReleaseArtifacts = new Set(
        release.artifacts
          .filter((artifact) => artifact.kind !== 'extension-native')
          .map((artifact) => `${artifact.kind}/${artifact.target}/${artifact.name}`),
      );
      verifyBoundBytes(packageBytes, release.package, `${module.module} package binding`);
      const packageManifest = validatePackageManifest(
        parseBoundedJsonBytes(packageBytes, `${module.module} vo.package.json`),
        `${module.module} vo.package.json`,
      );
      assertCanonicalPrettyJsonBytes(packageBytes, packageManifest, `${module.module} vo.package.json`);
      let previousPackagePath = '';
      let extractedPackageBytes = packageBytes.byteLength;
      const expectedFiles = new Set([
        'vo.release.json',
        'vo.package.json',
        '.vo-version',
        '.vo-source-digest',
      ]);
      for (const [index, entry] of packageManifest.files.entries()) {
        sourceFileEntry(entry, index, `${module.module} package files`);
        assertQuickplay(
          compareUtf8(previousPackagePath, entry.path) < 0,
          `${module.module} package files must be sorted and unique`,
        );
        previousPackagePath = entry.path;
        extractedPackageBytes += entry.size;
        assertQuickplay(
          Number.isSafeInteger(extractedPackageBytes)
            && extractedPackageBytes <= MAX_EXTRACTED_SOURCE_BYTES,
          `${module.module} package exceeds the protocol extracted-source limit`,
        );
        expectedFiles.add(entry.path);
        const bytes = files.get(entry.path);
        assertQuickplay(bytes, `${module.module} is missing ${entry.path}`);
        assertQuickplay(
          fileDescriptors.get(entry.path)?.mode
            === (entry.mode === 'executable' ? 0o755 : 0o644),
          `${module.module} ${entry.path} mode differs from vo.package.json`,
        );
        verifyBoundBytes(bytes, entry, `${module.module} ${entry.path}`);
      }
      assertQuickplay(
        files.size === expectedFiles.size
        && [...files.keys()].every((relative) => expectedFiles.has(relative)),
        `${module.module} package contains files outside its authenticated closure`,
      );
      const voModBytes = files.get('vo.mod');
      assertQuickplay(voModBytes, `${module.module} package is missing vo.mod`);
      assertCanonicalModIntent(
        UTF8.decode(voModBytes),
        snapshotModule,
        `${module.module} registry vo.mod`,
      );
      assertQuickplay(
        fileDescriptors.get('.vo-version')?.mode === 0o644
        && fileDescriptors.get('.vo-source-digest')?.mode === 0o644,
        `${module.module} cache marker mode must be 0644`,
      );
      assertQuickplay(UTF8.decode(files.get('.vo-version') ?? Buffer.alloc(0)) === `${module.version}\n`, `${module.module} version marker mismatch`);
      assertQuickplay(UTF8.decode(files.get('.vo-source-digest') ?? Buffer.alloc(0)) === `${release.source.digest}\n`, `${module.module} source marker mismatch`);
      releaseArtifacts = new Map(release.artifacts.map((artifact) => [`${artifact.kind}/${artifact.target}/${artifact.name}`, artifact]));
    } else {
      assertQuickplay(files.has('vo.mod'), `${module.module} workspace snapshot is missing vo.mod`);
      const workspaceModText = UTF8.decode(files.get('vo.mod') ?? Buffer.alloc(0));
      assertCanonicalModIntent(
        workspaceModText,
        snapshotModule,
        `${module.module} workspace vo.mod`,
      );
      for (const relative of files.keys()) {
        assertQuickplay(
          !hasReservedWorkspacePath(relative),
          `${module.module} workspace snapshot must omit protocol control file ${relative}`,
        );
      }
    }
    assertQuickplay(Array.isArray(module.artifacts), `${module.module} artifacts must be an array`);
    let previousArtifact = null;
    const installClosurePaths = [...files.keys()];
    const foundReleaseArtifacts = new Set();
    const browserWasmArtifacts = [];
    const browserJsArtifacts = [];
    for (const artifact of module.artifacts) {
      exactKeys(artifact, ['kind', 'target', 'name', 'size', 'digest', 'path', 'url'], `${module.module} artifact`);
      try {
        validateArtifactIdentity(artifact);
      } catch (error) {
        throw new Error(`${module.module} artifact: ${error instanceof Error ? error.message : String(error)}`);
      }
      digestBinding(artifact, `${module.module} artifact`);
      assertQuickplay(
        artifact.kind === 'extension-wasm' || artifact.kind === 'extension-js-glue',
        `${module.module} artifact kind is unsupported in Quickplay`,
      );
      assertQuickplay(
        artifact.target === 'wasm32-unknown-unknown',
        `${module.module} Quickplay artifact target is invalid`,
      );
      assertQuickplay(
        artifact.size > 0 && artifact.size <= MAX_MODULE_ARTIFACT_BYTES,
        `${module.module} Quickplay artifact size is invalid`,
      );
      totalArtifactBytes += artifact.size * (module.source === 'workspace' ? 2 : 1);
      assertQuickplay(
        Number.isSafeInteger(totalArtifactBytes) && totalArtifactBytes <= MAX_TOTAL_BYTES,
        'Quickplay dependency artifacts exceed the aggregate byte limit',
      );
      assertQuickplay(
        previousArtifact === null || compareArtifactIdentity(previousArtifact, artifact) < 0,
        `${module.module} artifacts must be sorted and unique`,
      );
      previousArtifact = artifact;
      const key = `${artifact.kind}/${artifact.target}/${artifact.name}`;
      const released = releaseArtifacts?.get(key);
      if (releaseArtifacts !== null) {
        assertQuickplay(released && released.size === artifact.size && released.digest === artifact.digest, `${module.module} artifact differs from release`);
        foundReleaseArtifacts.add(key);
      }
      validateRelativePath(artifact.path, `${module.module} artifact install path`);
      if (module.source === 'registry') {
        assertQuickplay(artifact.path === artifactCachePath(artifact), `${module.module} artifact cache path mismatch`);
      } else {
        assertQuickplay(!hasReservedWorkspacePath(artifact.path), `${module.module} artifact install path is reserved`);
      }
      installClosurePaths.push(artifact.path);
      if (module.source === 'workspace') installClosurePaths.push(artifactCachePath(artifact));
      const publishedRelative = artifactOutputRelativePath(module.cacheDir, artifact);
      assertQuickplay(artifact.url === quickplayArtifactUrl(module.cacheDir, artifact), `${module.module} artifact URL mismatch`);
      const bytes = readRegularFile(packageRoot, publishedRelative, `${module.module} published artifact`);
      verifyBoundBytes(bytes, artifact, `${module.module} published artifact`);
      if (artifact.kind === 'extension-wasm') browserWasmArtifacts.push({ artifact, bytes });
      else browserJsArtifacts.push({ artifact, bytes });
      assertQuickplay(!physicalFiles.has(publishedRelative), `${module.module} published artifact path is duplicated`);
      physicalFiles.add(publishedRelative);
    }
    if (browserWasmArtifacts.length > 0 || browserJsArtifacts.length > 0) {
      assertQuickplay(
        browserWasmArtifacts.length === 1 && browserJsArtifacts.length <= 1,
        `${module.module} must publish exactly one browser WASM and at most one JS glue artifact`,
      );
      const catalog = extensionExportCatalogFromVoFiles(module.module, files);
      try {
        if (browserJsArtifacts.length === 1) {
          assertBindgenWasmExtensionV3(
            browserWasmArtifacts[0].bytes,
            browserJsArtifacts[0].bytes,
            {
              expectedExportKeys: catalog.exportKeys,
              label: `${module.module} packaged bindgen extension`,
              maxWasmBytes: MAX_MODULE_ARTIFACT_BYTES,
              maxJsBytes: MAX_PROTOCOL_METADATA_BYTES,
            },
          );
        } else {
          assertStandaloneWasmExtensionV3(browserWasmArtifacts[0].bytes, {
            expectedExportKeys: catalog.exportKeys,
            expectedImports: module.module === 'github.com/vo-lang/vogui'
              ? VOGUI_STANDALONE_HOST_IMPORTS_V3
              : [],
            label: `${module.module} packaged standalone extension`,
            maxBytes: MAX_MODULE_ARTIFACT_BYTES,
          });
        }
      } catch (error) {
        throw new Error(
          `${module.module} browser extension contract failed: ${error instanceof Error ? error.message : String(error)}`,
        );
      }
    }
    assertPortableFileClosure(installClosurePaths, `${module.module} install closure`);
    const moduleVfsRoot = module.source === 'registry'
      ? module.cacheDir
      : `${QUICKPLAY_WORKSPACE_MODULE_VFS_ROOT}/${module.cacheDir.slice('workspace/'.length)}`;
    aggregateInstallClosurePaths.push(
      ...installClosurePaths.map((relative) => `${moduleVfsRoot}/${relative}`),
    );
    if (requiredReleaseArtifacts !== null) {
      assertQuickplay(
        foundReleaseArtifacts.size === requiredReleaseArtifacts.size
        && [...requiredReleaseArtifacts].every((key) => foundReleaseArtifacts.has(key)),
        `${module.module} browser artifacts do not match its authenticated release`,
      );
    }
  }
  assertPortableFileClosure(
    aggregateInstallClosurePaths,
    'Quickplay aggregate install closure',
    true,
  );
  const foundPhysicalFiles = listRegularFiles(packageRoot);
  assertQuickplay(
    foundPhysicalFiles.length === physicalFiles.size
    && foundPhysicalFiles.every((relative) => physicalFiles.has(relative)),
    'Quickplay physical file closure contains missing or undeclared files',
  );
  return { project: { ...project, snapshot }, deps };
}

function gitRepositoryInvocation(repoRoot) {
  const root = realpathSync.native(path.resolve(repoRoot));
  const environment = cleanGitEnvironment();
  const prefix = [
    '-c', 'core.fsmonitor=false',
    '-c', 'core.untrackedCache=false',
    '-c', 'core.excludesFile=',
    '-C', root,
  ];
  const authority = spawnSync('git', [...prefix, 'rev-parse', '--show-toplevel'], {
    encoding: 'utf8',
    env: environment,
    timeout: 30_000,
    maxBuffer: MAX_JSON_BYTES,
  });
  if (authority.error || authority.status !== 0) {
    throw new Error(`failed to establish Git repository authority for ${repoRoot}`);
  }
  const reported = authority.stdout.replace(/\r?\n$/u, '');
  assertQuickplay(
    reported.length > 0 && realpathSync.native(path.resolve(reported)) === root,
    `Git repository authority differs from the requested root: ${repoRoot}`,
  );
  return { root, environment, prefix };
}

export function gitHead(repoRoot) {
  const git = gitRepositoryInvocation(repoRoot);
  const result = spawnSync('git', [...git.prefix, 'rev-parse', 'HEAD'], {
    encoding: 'utf8',
    env: git.environment,
    timeout: 30_000,
    maxBuffer: MAX_JSON_BYTES,
  });
  if (result.error || result.status !== 0 || !/^[0-9a-f]{40}\n$/.test(result.stdout)) {
    throw new Error(`failed to read Git HEAD for ${repoRoot}`);
  }
  return result.stdout.trim();
}

function gitFileList(repoRoot, args, label) {
  const git = gitRepositoryInvocation(repoRoot);
  const result = spawnSync(
    'git',
    [...git.prefix, 'ls-files', '-z', ...args],
    {
      encoding: 'buffer',
      env: git.environment,
      timeout: 30_000,
      maxBuffer: MAX_JSON_BYTES,
    },
  );
  if (result.error || result.status !== 0) {
    throw new Error(`failed to list ${label} for ${repoRoot}`);
  }
  const files = UTF8.decode(result.stdout)
    .split('\0')
    .filter(Boolean)
    .sort(compareUtf8);
  assertQuickplay(
    new Set(files).size === files.length,
    `Git returned duplicate ${label} for ${repoRoot}`,
  );
  return files;
}

function isTrackedModuleRootLock(relative, tracked) {
  if (path.posix.basename(relative) !== 'vo.lock') return false;
  const directory = path.posix.dirname(relative);
  const moduleManifest = directory === '.' ? 'vo.mod' : `${directory}/vo.mod`;
  return tracked.has(moduleManifest);
}

export function gitTrackedFiles(repoRoot, {
  allowDeleted = [],
  allowDeletedModuleLocks = false,
} = {}) {
  assertQuickplay(
    Array.isArray(allowDeleted) || allowDeleted instanceof Set,
    'allowDeleted must be an array or Set of tracked relative paths',
  );
  assertQuickplay(
    [...allowDeleted].every((relative) => typeof relative === 'string'),
    'allowDeleted entries must be strings',
  );
  assertQuickplay(
    typeof allowDeletedModuleLocks === 'boolean',
    'allowDeletedModuleLocks must be boolean',
  );
  const files = gitFileList(repoRoot, ['--cached'], 'tracked files');
  const tracked = new Set(files);
  const deleted = gitFileList(repoRoot, ['--deleted'], 'deleted tracked files');
  const allowed = new Set(allowDeleted);
  for (const relative of deleted) {
    assertQuickplay(
      tracked.has(relative),
      `Git reported an untracked deleted path for ${repoRoot}: ${relative}`,
    );
    if (
      allowed.has(relative)
      || (allowDeletedModuleLocks && isTrackedModuleRootLock(relative, tracked))
    ) {
      try {
        lstatSync(path.join(repoRoot, ...relative.split('/')));
        throw new Error(
          `tracked source deletion changed while the working tree was enumerated: ${relative} (${repoRoot})`,
        );
      } catch (error) {
        if (error?.code !== 'ENOENT') throw error;
      }
      tracked.delete(relative);
      continue;
    }
    throw new Error(
      `tracked source file is deleted from the working tree: ${relative} (${repoRoot})`,
    );
  }
  return files.filter((relative) => tracked.has(relative));
}

function defaultCurrentSourceFileIncluded(relative) {
  return relative.endsWith('.vo')
    || isPortableModuleManifestPath(relative)
    || ROOT_MODULE_PROTOCOL_FILES.has(relative);
}

/** Return the filesystem-relevant working source closure, independent of Git ignore state. */
export function gitCurrentSourceFiles(repoRoot, {
  allowDeleted = [],
  allowDeletedModuleLocks = false,
  additionalFileIncluded = () => false,
  additionalIgnoredPathspecs = [],
} = {}) {
  assertQuickplay(
    typeof additionalFileIncluded === 'function',
    'additionalFileIncluded must be a function',
  );
  assertQuickplay(
    Array.isArray(additionalIgnoredPathspecs)
      && additionalIgnoredPathspecs.every((value) => (
        typeof value === 'string' && value.length > 0 && !value.includes('\0')
      )),
    'additionalIgnoredPathspecs must contain non-empty pathspec strings',
  );
  const fileIncluded = (relative) => (
    defaultCurrentSourceFileIncluded(relative) || additionalFileIncluded(relative)
  );
  assertCanonicalRootProtocolFiles(repoRoot, `current source in ${repoRoot}`);
  const current = new Set(gitTrackedFiles(repoRoot, {
    allowDeleted,
    allowDeletedModuleLocks,
  }));
  const untracked = gitFileList(
    repoRoot,
    ['--others', '--exclude-standard'],
    'untracked current-source files',
  );
  for (const relative of untracked) {
    if (!fileIncluded(relative)) continue;
    validateRelativePath(relative, `untracked Vo source input in ${repoRoot}`);
    assertQuickplay(
      !current.has(relative),
      `Git reported an untracked file that is already tracked: ${relative} (${repoRoot})`,
    );
    current.add(relative);
  }
  const ignored = gitFileList(
    repoRoot,
    [
      '--others',
      '--ignored',
      '--exclude-standard',
      '--',
      ...new Set([
        ...DEFAULT_CURRENT_SOURCE_IGNORED_PATHSPECS,
        ...additionalIgnoredPathspecs,
      ]),
    ],
    'ignored current-source inputs',
  );
  for (const relative of ignored) {
    if (!fileIncluded(relative)) continue;
    validateRelativePath(relative, `ignored source input in ${repoRoot}`);
    assertQuickplay(
      !current.has(relative),
      `Git reported an ignored module boundary that is already current: ${relative} (${repoRoot})`,
    );
    current.add(relative);
  }
  const files = [...current].sort(compareUtf8);
  assertPortableFileClosure(
    files.filter(fileIncluded),
    `current Vo source closure in ${repoRoot}`,
  );
  assertCanonicalRootProtocolFiles(repoRoot, `current source in ${repoRoot}`);
  return files;
}

function gitSourceFiles(repoRoot) {
  const trackedFiles = gitFileList(
    repoRoot,
    ['--cached', '--', ...QUICKPLAY_SOURCE_GIT_PATHSPECS],
    'tracked Quickplay source input files',
  );
  const current = new Set(trackedFiles);
  const deleted = gitFileList(
    repoRoot,
    ['--deleted', '--', ...QUICKPLAY_SOURCE_GIT_PATHSPECS],
    'deleted Quickplay source input files',
  );
  for (const relative of deleted) {
    assertQuickplay(
      current.has(relative),
      `Git reported an untracked deleted Quickplay source input: ${relative} (${repoRoot})`,
    );
    try {
      lstatSync(path.join(repoRoot, ...relative.split('/')));
      throw new Error(
        `deleted Quickplay source input changed while Git was enumerated: ${relative} (${repoRoot})`,
      );
    } catch (error) {
      if (error?.code !== 'ENOENT') throw error;
    }
    current.delete(relative);
  }
  const untracked = gitFileList(
    repoRoot,
    ['--others', '--exclude-standard', '--', ...QUICKPLAY_SOURCE_GIT_PATHSPECS],
    'untracked Quickplay source input files',
  );
  for (const relative of untracked) {
    assertQuickplay(
      !current.has(relative),
      `Git reported a duplicate current Quickplay source input: ${relative} (${repoRoot})`,
    );
    current.add(relative);
  }
  const files = [...current].sort(compareUtf8);
  return files;
}

function hashFramedBytes(hash, bytes) {
  const size = Buffer.allocUnsafe(8);
  size.writeBigUInt64BE(BigInt(bytes.byteLength));
  hash.update(size);
  hash.update(bytes);
}

function selectedSourceInputs(repoRoot) {
  const files = gitSourceFiles(repoRoot);
  const selected = new Set();
  for (const input of QUICKPLAY_ARTIFACT_INPUTS) {
    if (input.startsWith('external:') || input.startsWith('first-party:')) continue;
    if (input.endsWith('/**')) {
      const prefix = input.slice(0, -2);
      for (const relative of files) {
        if (relative.startsWith(prefix)) selected.add(relative);
      }
      continue;
    }
    assertQuickplay(files.includes(input), `Quickplay source input is missing: ${input}`);
    selected.add(input);
  }
  const result = [...selected].sort(compareUtf8);
  assertPortableFileClosure(result, `Quickplay source input closure in ${repoRoot}`);
  return result;
}

function hashSelectedSourceInputs(repoRoot, selected) {
  assertQuickplay(
    selected.length <= MAX_STATIC_ENTRIES,
    `Quickplay source input closure contains more than ${MAX_STATIC_ENTRIES} files: ${repoRoot}`,
  );
  const hash = createHash('sha256');
  hashFramedBytes(hash, Buffer.from(JSON.stringify(QUICKPLAY_ARTIFACT_INPUTS), 'utf8'));
  let totalBytes = 0;
  for (const relative of selected) {
    const remaining = Math.min(MAX_FILE_BYTES, MAX_TOTAL_BYTES - totalBytes);
    const source = readRegularFileSnapshot(
      repoRoot,
      relative,
      `Quickplay source input ${relative}`,
      remaining,
    );
    totalBytes += source.bytes.byteLength;
    assertQuickplay(
      Number.isSafeInteger(totalBytes) && totalBytes <= MAX_TOTAL_BYTES,
      `Quickplay source input closure exceeds the ${MAX_TOTAL_BYTES}-byte limit: ${repoRoot}`,
    );
    hashFramedBytes(hash, Buffer.from(relative, 'utf8'));
    hashFramedBytes(hash, source.bytes);
  }
  return `sha256:${hash.digest('hex')}`;
}

export function sourceInputDigest(repoRoot) {
  const selected = selectedSourceInputs(repoRoot);
  const digest = hashSelectedSourceInputs(repoRoot, selected);
  assertQuickplay(
    JSON.stringify(selectedSourceInputs(repoRoot)) === JSON.stringify(selected),
    `Quickplay source input closure changed while it was hashed: ${repoRoot}`,
  );
  assertQuickplay(
    hashSelectedSourceInputs(repoRoot, selected) === digest,
    `Quickplay source input bytes changed while they were hashed: ${repoRoot}`,
  );
  assertQuickplay(
    JSON.stringify(selectedSourceInputs(repoRoot)) === JSON.stringify(selected),
    `Quickplay source input closure changed while its bytes were verified: ${repoRoot}`,
  );
  return digest;
}

export function quickplaySourceDigests(project, deps, repoRoot, { volangDigest = null } = {}) {
  if (volangDigest !== null) {
    assertQuickplay(DIGEST.test(volangDigest), 'captured Volang source digest is invalid');
  }
  const result = {
    volang: volangDigest ?? sourceInputDigest(repoRoot),
    [project.module]: sha256Digest(Buffer.from(JSON.stringify({
      module: project.module,
      snapshot: project.snapshot,
      files: project.files,
    }), 'utf8')),
  };
  for (const module of deps.modules) {
    result[module.module] = sha256Digest(Buffer.from(JSON.stringify(module), 'utf8'));
  }
  return result;
}

export function listRegularFiles(root) {
  const rootReal = realpathSync.native(root);
  const rootMetadata = lstatSync(rootReal);
  assertQuickplay(
    rootMetadata.isDirectory() && !rootMetadata.isSymbolicLink(),
    `source root must be a real directory: ${root}`,
  );
  const files = [];
  const directories = [''];
  let walkedEntries = 0;
  while (directories.length > 0) {
    const relative = directories.pop();
    const current = relative === '' ? rootReal : path.join(rootReal, ...relative.split('/'));
    assertQuickplay(
      realpathSync.native(current) === current,
      `source directory must not use a filesystem alias: ${relative || '.'}`,
    );
    const entries = readdirSync(current, { withFileTypes: true })
      .sort((left, right) => compareUtf8(left.name, right.name));
    for (const entry of entries) {
      walkedEntries += 1;
      assertQuickplay(
        walkedEntries <= MAX_WALK_ENTRIES,
        `source tree exceeds the ${MAX_WALK_ENTRIES}-entry traversal limit`,
      );
      const child = relative === '' ? entry.name : `${relative}/${entry.name}`;
      validateRelativePath(child, 'source file path');
      if (entry.isDirectory()) {
        directories.push(child);
      } else if (entry.isFile()) {
        const file = path.join(rootReal, ...child.split('/'));
        assertQuickplay(realpathSync.native(file) === file, `source file must not use a filesystem alias: ${child}`);
        files.push(child);
      } else {
        throw new Error(`source tree contains an unsupported entry: ${child}`);
      }
    }
  }
  return files.sort(compareUtf8);
}

export function outputFacts(root, relativePaths) {
  return [...relativePaths].sort(compareUtf8).map((relative) => {
    const bytes = readRegularFile(root, relative, `Quickplay output ${relative}`);
    return { path: relative, size: bytes.byteLength, digest: sha256Digest(bytes) };
  });
}
