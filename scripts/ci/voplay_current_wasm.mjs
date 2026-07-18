#!/usr/bin/env node
import { execFileSync } from 'node:child_process';
import { createHash, randomUUID } from 'node:crypto';
import {
  closeSync,
  constants as fsConstants,
  existsSync,
  fstatSync,
  fsyncSync,
  lstatSync,
  mkdirSync,
  mkdtempSync,
  openSync,
  readFileSync,
  readdirSync,
  realpathSync,
  renameSync,
  rmdirSync,
  rmSync,
  unlinkSync,
  writeFileSync,
} from 'node:fs';
import path from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import {
  canonicalGitRepositoryRoot,
  cleanGitEnvironment,
  requireRepoRoot,
} from './repo_roots.mjs';
import {
  captureCleanEffectiveSnapshot,
  createVoBinaryAuthority,
} from './quickplay_vnext.mjs';
import {
  artifactSetDigest,
  gitCommit,
  sourceTreeDigest,
  sourceTreeDeclaredOutputPaths,
  sourceTreeDigestExcludingDeclaredOutputs,
} from './source_bound_evidence.mjs';
import {
  assertBindgenWasmExtensionV3,
  extensionExportCatalogFromDirectory,
} from './wasm_protocol_v3.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const defaultOutDir = path.join(root, 'target', 'voplay-current-wasm');
const VOPLAY_WASM_OUTPUT_MAX_BYTES = 35_000_000;
const VOPLAY_PRODUCER_MANIFEST_MAX_BYTES = 8 * 1024 * 1024;
const VOPLAY_MODULE = 'github.com/vo-lang/voplay';
export const VOPLAY_WASM_PRODUCER_SCHEMA_VERSION = 4;
export const VOPLAY_SOURCE_CLOSURE_SCHEMA_VERSION = 2;
export const VOPLAY_FFI_SOURCE_FINGERPRINT_ENV = 'VO_FFI_SOURCE_FINGERPRINT';
export const VOPLAY_WORKSPACE_ENV = 'VOWORK';
export const VOPLAY_FFI_SOURCE_FINGERPRINT_NAMESPACE = 'voplay-ffi-source-v3';
export const VOPLAY_SOURCE_OUTPUT_DECLARATION = 'voplay.current-source-wasm';
export const VOPLAY_VOLANG_BUILD_ROOT_FILES = Object.freeze([
  '.cargo/config.toml',
  'Cargo.lock',
  'Cargo.toml',
  'rust-toolchain.toml',
]);
export const VOPLAY_VOLANG_STDLIB_INPUT = 'lang/stdlib';
export const VOPLAY_WASM_PRODUCER_COMMAND = [
  'wasm-pack',
  'build',
  '--target',
  'web',
  '--release',
  '--out-dir',
  '<OUT_DIR>',
  '--out-name',
  'voplay_island',
  '<VOPLAY_ROOT>/rust',
  '--no-default-features',
  '--features',
  'wasm-island',
  '--locked',
];
export const VOPLAY_WASM_REQUIRED_OUTPUTS = ['voplay_island.js', 'voplay_island_bg.wasm'];
const VOPLAY_DECLARED_SOURCE_OUTPUTS = sourceTreeDeclaredOutputPaths(
  VOPLAY_SOURCE_OUTPUT_DECLARATION,
);
if (
  JSON.stringify(VOPLAY_DECLARED_SOURCE_OUTPUTS.map((relative) => path.posix.basename(relative)))
  !== JSON.stringify(VOPLAY_WASM_REQUIRED_OUTPUTS)
) {
  throw new Error('voplay source-output declaration does not match its required WASM outputs');
}
export const VOPLAY_CARGO_METADATA_FEATURE_ARGS = Object.freeze([
  '--no-default-features',
  '--features',
  'wasm-island',
  '--filter-platform',
  'wasm32-unknown-unknown',
]);

export function currentVoplayWasmBuildPlatform() {
  return { os: process.platform, arch: process.arch };
}

function sha256(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

export function voplayFfiSourceFingerprint(sourceClosureContentDigest, volangBuildInputsDigest) {
  for (const [label, value] of [
    ['voplay source-closure content digest', sourceClosureContentDigest],
    ['Volang build-input digest', volangBuildInputsDigest],
  ]) {
    if (!/^sha256:[0-9a-f]{64}$/.test(value ?? '')) {
      throw new Error(`${label} must be a canonical sha256 digest`);
    }
  }
  return sha256(Buffer.from(
    `${VOPLAY_FFI_SOURCE_FINGERPRINT_NAMESPACE}\0${sourceClosureContentDigest}\0${volangBuildInputsDigest}\0`,
    'utf8',
  ));
}

export function voplayWasmBuildEnvironment(preflight, baseEnvironment = process.env) {
  const fingerprint = voplayFfiSourceFingerprint(
    preflight?.sourceClosure?.contentDigest,
    preflight?.volangBuildInputs?.digest,
  );
  if (preflight?.ffiSourceFingerprint !== fingerprint) {
    throw new Error('preflight FFI source fingerprint does not match its source digests');
  }
  const workspaceFile = preflight?.workspaceFile;
  if (
    typeof workspaceFile !== 'string'
    || !path.isAbsolute(workspaceFile)
    || realpathSync.native(workspaceFile) !== workspaceFile
  ) {
    throw new Error('preflight workspace file must be a canonical absolute path');
  }
  const workspace = preflight?.sourceClosure?.workspace;
  if (
    !exactObjectKeys(workspace, ['digest', 'modules', 'path', 'repository'])
    || workspace.path !== 'vo.work'
    || path.basename(workspaceFile) !== workspace.path
  ) {
    throw new Error('preflight workspace file does not match the source-closure workspace record');
  }
  const workspaceRoot = canonicalGitRepositoryRoot(
    path.dirname(workspaceFile),
    'preflight workspace repository',
    { requireVoMod: true },
  );
  if (
    workspaceRoot !== path.dirname(workspaceFile)
    || repoModuleName(workspaceRoot) !== workspace.repository
    || sha256(readBoundedUtf8File(workspaceFile, 'preflight workspace file').bytes) !== workspace.digest
  ) {
    throw new Error('preflight workspace file is not the recorded workspace generation');
  }
  return {
    ...baseEnvironment,
    [VOPLAY_FFI_SOURCE_FINGERPRINT_ENV]: fingerprint,
    [VOPLAY_WORKSPACE_ENV]: workspaceFile,
  };
}

function outputEntry(outDir, name) {
  const file = path.join(outDir, name);
  try {
    if (!existsSync(file)) return { name, missing: true };
    const metadata = lstatSync(file);
    if (!metadata.isFile() || metadata.isSymbolicLink()) {
      return { name, invalid: 'not-regular-file' };
    }
    if (metadata.size > VOPLAY_WASM_OUTPUT_MAX_BYTES) {
      return { name, invalid: 'oversized' };
    }
    const bytes = readFileSync(file);
    return { name, size: bytes.byteLength, digest: sha256(bytes) };
  } catch {
    return { name, invalid: 'unreadable' };
  }
}

function toolVersion(command, args) {
  return execFileSync(command, args, { encoding: 'utf8' }).trim();
}

function commandErrorDetail(error) {
  return String(error?.stderr || error?.message || error).trim();
}

function pathWithin(rootPath, candidate) {
  const relative = path.relative(rootPath, candidate);
  return relative === '' || (!relative.startsWith(`..${path.sep}`) && relative !== '..' && !path.isAbsolute(relative));
}

function portableRelativePath(rootPath, candidate) {
  return path.relative(rootPath, candidate).split(path.sep).join('/');
}

function compareUtf8Text(left, right) {
  return Buffer.compare(Buffer.from(left, 'utf8'), Buffer.from(right, 'utf8'));
}

function cargoBuildDependencyIds(node) {
  if (!Array.isArray(node.deps)) {
    if (
      !Array.isArray(node.dependencies)
      || node.dependencies.some((entry) => typeof entry !== 'string')
    ) {
      throw new Error(`cargo metadata resolve node has invalid dependencies: ${node.id}`);
    }
    return node.dependencies;
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
      throw new Error(`cargo metadata resolve node has invalid dependency kinds: ${node.id}`);
    }
    // wasm-pack builds the library target. Cargo dev-dependencies belong to
    // tests, examples, and benches, so they are outside this producer graph.
    if (dependency.dep_kinds.some((entry) => entry.kind !== 'dev')) {
      dependencies.push(dependency.pkg);
    }
  }
  return dependencies;
}

function reachableCargoPackageIds(metadata) {
  if (!metadata?.resolve || !Array.isArray(metadata.resolve.nodes)) {
    throw new Error('cargo metadata did not contain a resolved package graph');
  }
  const rootId = metadata.resolve.root
    ?? (Array.isArray(metadata.workspace_default_members)
      && metadata.workspace_default_members.length === 1
      ? metadata.workspace_default_members[0]
      : null);
  if (typeof rootId !== 'string' || rootId.length === 0) {
    throw new Error('cargo metadata did not identify exactly one root package');
  }
  const nodes = new Map();
  for (const node of metadata.resolve.nodes) {
    if (!node || typeof node.id !== 'string' || nodes.has(node.id)) {
      throw new Error('cargo metadata contains an invalid or duplicate resolve node');
    }
    nodes.set(node.id, node);
  }
  if (!nodes.has(rootId)) {
    throw new Error(`cargo metadata root package is absent from the resolve graph: ${rootId}`);
  }
  const reachable = new Set();
  const pending = [rootId];
  while (pending.length > 0) {
    const id = pending.pop();
    if (reachable.has(id)) continue;
    reachable.add(id);
    const node = nodes.get(id);
    if (!node) {
      throw new Error(`cargo metadata resolve edge references a missing package: ${id}`);
    }
    pending.push(...cargoBuildDependencyIds(node));
  }
  return reachable;
}

function compareBuildPackages(left, right) {
  return Buffer.compare(Buffer.from(left.path, 'utf8'), Buffer.from(right.path, 'utf8'))
    || Buffer.compare(Buffer.from(left.name, 'utf8'), Buffer.from(right.name, 'utf8'))
    || Buffer.compare(Buffer.from(left.version, 'utf8'), Buffer.from(right.version, 'utf8'));
}

function exactObjectKeys(value, expected) {
  return value
    && typeof value === 'object'
    && !Array.isArray(value)
    && JSON.stringify(Object.keys(value).sort()) === JSON.stringify([...expected].sort());
}

function validBuildPackage(value) {
  const canonicalPath = /^lang\/crates\/[^/]+$/.test(value?.path)
    || (value?.path === VOPLAY_VOLANG_STDLIB_INPUT && value?.name === 'vo-stdlib-source');
  return exactObjectKeys(value, ['name', 'path', 'version'])
    && typeof value.name === 'string'
    && value.name.length > 0
    && typeof value.version === 'string'
    && value.version.length > 0
    && typeof value.path === 'string'
    && canonicalPath;
}

function buildInputPaths(packages) {
  return [...new Set([
    ...VOPLAY_VOLANG_BUILD_ROOT_FILES,
    VOPLAY_VOLANG_STDLIB_INPUT,
    ...packages.map((entry) => entry.path),
  ])];
}

/**
 * Materialize a canonical, deliberately scoped digest of the Volang inputs
 * consumed through voplay's Cargo path dependencies. Keeping the producer and
 * checked Quickplay output outside this scope avoids a self-referential hash.
 */
export function createVoplayVolangBuildInputs(volangRoot, packages) {
  const canonicalRoot = realpathSync.native(path.resolve(volangRoot));
  if (!Array.isArray(packages) || packages.length === 0 || packages.some((entry) => !validBuildPackage(entry))) {
    throw new Error('Volang Cargo build packages must use canonical direct lang/crates paths or the vo-stdlib-source path');
  }
  const canonicalPackages = packages
    .map((entry) => ({ name: entry.name, version: entry.version, path: entry.path }))
    .sort(compareBuildPackages);
  if (new Set(canonicalPackages.map((entry) => entry.path)).size !== canonicalPackages.length) {
    throw new Error('Volang Cargo build package paths must be unique');
  }
  return {
    schemaVersion: 1,
    kind: 'volang.scopedCargoBuildInputs',
    rootFiles: [...VOPLAY_VOLANG_BUILD_ROOT_FILES],
    stdlib: VOPLAY_VOLANG_STDLIB_INPUT,
    packages: canonicalPackages,
    digest: artifactSetDigest(canonicalRoot, buildInputPaths(canonicalPackages)),
  };
}

/** Derive every resolved Volang-local Cargo path package from locked metadata. */
export function deriveVoplayVolangBuildInputs(metadata, volangRoot = root) {
  if (!metadata || typeof metadata !== 'object' || !Array.isArray(metadata.packages)) {
    throw new Error('cargo metadata did not contain a packages array');
  }
  const canonicalRoot = realpathSync.native(path.resolve(volangRoot));
  const resolvedIds = reachableCargoPackageIds(metadata);
  const packages = [];
  for (const cargoPackage of metadata.packages) {
    if (!resolvedIds.has(cargoPackage?.id) || cargoPackage?.source !== null) continue;
    if (typeof cargoPackage.manifest_path !== 'string') {
      throw new Error(`resolved path package ${cargoPackage?.id ?? '(unknown)'} has no manifest_path`);
    }
    let canonicalManifest;
    try {
      canonicalManifest = realpathSync.native(path.resolve(cargoPackage.manifest_path));
    } catch (error) {
      throw new Error(
        `resolved path package manifest cannot be read: ${cargoPackage.manifest_path}: ${commandErrorDetail(error)}`,
      );
    }
    if (!pathWithin(canonicalRoot, canonicalManifest)) continue;
    const packageRoot = path.dirname(canonicalManifest);
    const relative = portableRelativePath(canonicalRoot, packageRoot);
    const canonicalPath = /^lang\/crates\/[^/]+$/.test(relative)
      || (relative === VOPLAY_VOLANG_STDLIB_INPUT && cargoPackage.name === 'vo-stdlib-source');
    if (!canonicalPath) {
      throw new Error(
        `resolved Volang path package must live directly under lang/crates or be vo-stdlib-source: ${relative}`,
      );
    }
    if (typeof cargoPackage.name !== 'string' || typeof cargoPackage.version !== 'string') {
      throw new Error(`resolved Volang path package has incomplete identity: ${cargoPackage.id}`);
    }
    packages.push({
      name: cargoPackage.name,
      version: cargoPackage.version,
      path: relative,
    });
  }
  if (packages.length === 0) {
    throw new Error('locked voplay metadata did not resolve any Volang-local Cargo path packages');
  }
  return createVoplayVolangBuildInputs(canonicalRoot, packages);
}

/**
 * Verify scoped evidence without invoking Cargo. This keeps artifact validation
 * deterministic while still rejecting malformed, widened, missing, or stale
 * Volang input scopes.
 */
export function verifyVoplayVolangBuildInputs(
  value,
  volangRoot = root,
  { expected = null } = {},
) {
  const issues = [];
  if (!exactObjectKeys(value, ['digest', 'kind', 'packages', 'rootFiles', 'schemaVersion', 'stdlib'])) {
    return ['volangBuildInputs must contain the exact scoped-input fields'];
  }
  if (value.schemaVersion !== 1) issues.push('volangBuildInputs.schemaVersion must be 1');
  if (value.kind !== 'volang.scopedCargoBuildInputs') {
    issues.push('volangBuildInputs.kind must be volang.scopedCargoBuildInputs');
  }
  if (JSON.stringify(value.rootFiles) !== JSON.stringify(VOPLAY_VOLANG_BUILD_ROOT_FILES)) {
    issues.push('volangBuildInputs.rootFiles do not match the canonical root input scope');
  }
  if (value.stdlib !== VOPLAY_VOLANG_STDLIB_INPUT) {
    issues.push('volangBuildInputs.stdlib does not match the canonical stdlib input');
  }
  if (!Array.isArray(value.packages) || value.packages.length === 0) {
    issues.push('volangBuildInputs.packages must be a non-empty array');
    return issues;
  }
  if (value.packages.some((entry) => !validBuildPackage(entry))) {
    issues.push('volangBuildInputs.packages contains a non-canonical package record');
    return issues;
  }
  const sorted = [...value.packages].sort(compareBuildPackages);
  if (JSON.stringify(value.packages) !== JSON.stringify(sorted)) {
    issues.push('volangBuildInputs.packages must use canonical ordering');
  }
  if (new Set(value.packages.map((entry) => entry.path)).size !== value.packages.length) {
    issues.push('volangBuildInputs.packages contains duplicate package paths');
  }
  if (!/^sha256:[0-9a-f]{64}$/.test(value.digest ?? '')) {
    issues.push('volangBuildInputs.digest must be sha256');
    return issues;
  }
  if (issues.length > 0) return issues;
  try {
    const currentDigest = artifactSetDigest(
      realpathSync.native(path.resolve(volangRoot)),
      buildInputPaths(value.packages),
    );
    if (value.digest !== currentDigest) {
      issues.push('volangBuildInputs.digest does not match current scoped Volang inputs');
    }
  } catch (error) {
    issues.push(`volangBuildInputs scope cannot be verified: ${commandErrorDetail(error)}`);
  }
  if (expected && JSON.stringify(value) !== JSON.stringify(expected)) {
    issues.push('volangBuildInputs do not match locked Cargo metadata');
  }
  return issues;
}

function canonicalModuleName(value, label) {
  if (typeof value !== 'string' || Buffer.byteLength(value, 'utf8') > 255) {
    throw new Error(`${label} must be a bounded canonical GitHub module name`);
  }
  const parts = value.split('/');
  if (
    parts.length < 3
    || parts[0] !== 'github.com'
    || parts.slice(1).some((part) => (
      !/^[A-Za-z0-9][A-Za-z0-9._-]*$/.test(part)
      || part === '.'
      || part === '..'
    ))
  ) {
    throw new Error(`${label} must be a canonical github.com module name`);
  }
  return value;
}

function canonicalPortableRelative(value, label) {
  if (
    typeof value !== 'string'
    || value.length === 0
    || Buffer.byteLength(value, 'utf8') > 4096
    || value.includes('\\')
    || path.posix.isAbsolute(value)
  ) {
    throw new Error(`${label} must be a bounded portable relative path`);
  }
  const parts = value.split('/');
  if (parts.some((part) => part === '' || part === '.' || part === '..' || part.includes('\0'))) {
    throw new Error(`${label} must be normalized and must not contain traversal components`);
  }
  return value;
}

function canonicalPathWithoutLayoutAlias(value, label, kind) {
  const lexical = path.resolve(value);
  let canonical;
  let metadata;
  try {
    canonical = realpathSync.native(lexical);
    metadata = lstatSync(canonical);
  } catch (error) {
    throw new Error(`${label} cannot be resolved: ${commandErrorDetail(error)}`);
  }
  if (lexical !== canonical) {
    throw new Error(`${label} uses a symbolic-link, case, or filesystem layout alias: ${lexical} -> ${canonical}`);
  }
  if (metadata.isSymbolicLink() || (kind === 'file' ? !metadata.isFile() : !metadata.isDirectory())) {
    throw new Error(`${label} must be a real ${kind}`);
  }
  return canonical;
}

function readBoundedUtf8File(file, label, maxBytes = 16 * 1024 * 1024) {
  const metadata = lstatSync(file);
  if (!metadata.isFile() || metadata.isSymbolicLink()) {
    throw new Error(`${label} must be a regular file without symbolic links`);
  }
  if (metadata.size > maxBytes) {
    throw new Error(`${label} exceeds the ${maxBytes}-byte text limit`);
  }
  const bytes = readFileSync(file);
  try {
    return { bytes, text: new TextDecoder('utf-8', { fatal: true, ignoreBOM: true }).decode(bytes) };
  } catch (error) {
    throw new Error(`${label} is not valid UTF-8: ${commandErrorDetail(error)}`);
  }
}

function gitOutput(args, cwd) {
  return execFileSync('git', args, {
    cwd,
    encoding: 'utf8',
    env: cleanGitEnvironment(),
    maxBuffer: 8 * 1024 * 1024,
    stdio: ['ignore', 'pipe', 'pipe'],
  }).trim();
}

function gitStatusText(repoRoot, outputDeclaration = null) {
  const args = ['status', '--porcelain=v2', '--untracked-files=all'];
  if (outputDeclaration !== null) {
    args.push(
      '--',
      '.',
      ...sourceTreeDeclaredOutputPaths(outputDeclaration)
        .map((relative) => `:(top,exclude,literal)${relative}`),
    );
  }
  return gitOutput(args, repoRoot);
}

const repoModuleNames = new Map();

function repoModuleName(repoRoot) {
  const canonicalRoot = realpathSync.native(repoRoot);
  const cached = repoModuleNames.get(canonicalRoot);
  if (cached === undefined) {
    throw new Error(`${canonicalRoot} is absent from the core workspace snapshot`);
  }
  return cached;
}

function captureCoreWorkspaceModuleNames(projectRoot, workspaceRoots, voAuthority) {
  const snapshot = captureCleanEffectiveSnapshot({
    root,
    projectRoot,
    workspaceRoots,
    cacheRoot: path.join(root, 'target', 'voplay-current-wasm', 'module-cache'),
    expectedRoot: 'github.com/vo-lang/voplay',
    voAuthority,
  });
  repoModuleNames.set(
    realpathSync.native(projectRoot),
    canonicalModuleName(snapshot.root.module, `${projectRoot} module snapshot`),
  );
  for (const module of snapshot.modules) {
    if (module.source.kind !== 'workspace') {
      throw new Error(`voplay source closure selected registry module ${module.module}`);
    }
    repoModuleNames.set(
      realpathSync.native(module.source.directory),
      canonicalModuleName(module.module, `${module.source.directory} module snapshot`),
    );
  }
  return snapshot;
}

function selectedVoplayWorkspaceFile(voplayRoot, environment) {
  const configured = environment?.[VOPLAY_WORKSPACE_ENV];
  if (configured === 'off') {
    throw new Error('voplay current-source WASM requires its checked-in vo.work; VOWORK=off is unsupported');
  }
  let selected = null;
  if (typeof configured === 'string' && configured.length > 0) {
    selected = path.isAbsolute(configured) ? configured : path.resolve(voplayRoot, configured);
  } else {
    let current = voplayRoot;
    while (true) {
      const candidate = path.join(current, 'vo.work');
      if (existsSync(candidate)) {
        selected = candidate;
        break;
      }
      const parent = path.dirname(current);
      if (parent === current) break;
      current = parent;
    }
  }
  if (!selected) throw new Error('voplay root has no selected vo.work');
  const canonical = canonicalPathWithoutLayoutAlias(selected, 'selected voplay workspace file', 'file');
  const required = path.join(voplayRoot, 'vo.work');
  if (canonical !== required) {
    throw new Error(`selected voplay workspace file must be the root vo.work ${required}, found ${canonical}`);
  }
  return canonical;
}

function repositoryEnvironmentName(moduleName) {
  return `${moduleName.split('/').at(-1).replace(/[^A-Za-z0-9]/g, '_').toUpperCase()}_ROOT`;
}

function observeSourceRepository(state, requireClean) {
  const outputDeclaration = state.name === VOPLAY_MODULE
    ? VOPLAY_SOURCE_OUTPUT_DECLARATION
    : null;
  const statusBefore = gitStatusText(state.root, outputDeclaration);
  const commitBefore = gitCommit(state.root);
  const digest = outputDeclaration === null
    ? sourceTreeDigest(state.root)
    : sourceTreeDigestExcludingDeclaredOutputs(state.root, outputDeclaration);
  const statusAfter = gitStatusText(state.root, outputDeclaration);
  const commitAfter = gitCommit(state.root);
  if (statusBefore !== statusAfter || commitBefore !== commitAfter) {
    throw new Error(`source repository changed while provenance was collected: ${state.name}`);
  }
  if (!commitAfter) throw new Error(`source repository has no canonical Git commit: ${state.name}`);
  const dirty = statusAfter !== '';
  if (requireClean && dirty) {
    throw new Error(
      `source repository must be clean before producing checked-in Quickplay WASM: ${state.name}. `
      + `Commit or restore these entries: ${statusAfter.split(/\r?\n/).join(', ')}`,
    );
  }
  const cargoPackages = [...state.cargoPackages.values()].sort((left, right) => (
    compareUtf8Text(left.manifest, right.manifest)
    || compareUtf8Text(left.name, right.name)
    || compareUtf8Text(left.version, right.version)
  ));
  const workspaceModules = [...state.workspaceModules].sort(compareUtf8Text);
  const roles = [...state.roles].sort(compareUtf8Text);
  return {
    roles,
    name: state.name,
    commit: commitAfter,
    dirty,
    digest,
    cargoPackages,
    workspaceModules,
  };
}

function sourceClosureDigestRecord(value, includeProvenance) {
  return {
    schemaVersion: value.schemaVersion,
    kind: value.kind,
    ...(includeProvenance ? { contentDigest: value.contentDigest } : {}),
    workspace: {
      repository: value.workspace?.repository,
      path: value.workspace?.path,
      digest: value.workspace?.digest,
      modules: value.workspace?.modules,
    },
    repositories: value.repositories?.map((repository) => ({
      roles: repository?.roles,
      name: repository?.name,
      ...(includeProvenance ? {
        commit: repository?.commit,
        dirty: repository?.dirty,
      } : {}),
      digest: repository?.digest,
      cargoPackages: repository?.cargoPackages?.map((cargoPackage) => ({
        name: cargoPackage?.name,
        version: cargoPackage?.version,
        manifest: cargoPackage?.manifest,
      })),
      workspaceModules: repository?.workspaceModules,
    })),
  };
}

function sourceClosureContentDigest(value) {
  return sha256(Buffer.from(
    JSON.stringify(sourceClosureDigestRecord(value, false)),
    'utf8',
  ));
}

function sourceClosureDigest(value) {
  return sha256(Buffer.from(
    JSON.stringify(sourceClosureDigestRecord(value, true)),
    'utf8',
  ));
}

/**
 * Derive the host-independent Git source closure read by the voplay WASM build.
 * Absolute roots remain process-local and are never serialized into provenance.
 */
export function deriveVoplaySourceClosure(
  metadata,
  voplayRoot,
  {
    volangRoot = root,
    environment = process.env,
    requireClean = false,
    voAuthority = null,
  } = {},
) {
  if (!metadata || typeof metadata !== 'object' || !Array.isArray(metadata.packages)) {
    throw new Error('cargo metadata did not contain a packages array');
  }
  const canonicalVoplayRoot = canonicalGitRepositoryRoot(
    canonicalPathWithoutLayoutAlias(voplayRoot, 'voplay source root', 'directory'),
    'voplay source root',
    { requireVoMod: true },
  );
  const canonicalVolangRoot = canonicalGitRepositoryRoot(
    canonicalPathWithoutLayoutAlias(volangRoot, 'Volang source root', 'directory'),
    'Volang source root',
  );
  const workspaceFile = selectedVoplayWorkspaceFile(canonicalVoplayRoot, environment);
  const workspaceRead = readBoundedUtf8File(workspaceFile, 'selected voplay workspace file');
  const workspaceDigest = sha256(workspaceRead.bytes);
  const workspaceRoots = [
    ['VOGUI_ROOT', 'vogui'],
    ['VOPACK_ROOT', 'vopack'],
  ].map(([environmentName, sibling]) => canonicalGitRepositoryRoot(
    canonicalPathWithoutLayoutAlias(
      environment?.[environmentName] ?? path.resolve(canonicalVoplayRoot, '..', sibling),
      environmentName,
      'directory',
    ),
    environmentName,
    { requireVoMod: true },
  ));
  const moduleSnapshot = captureCoreWorkspaceModuleNames(
    canonicalVoplayRoot,
    workspaceRoots,
    voAuthority,
  );
  const rootModule = repoModuleName(canonicalVoplayRoot);
  const statesByRoot = new Map();
  const rootsByName = new Map();

  const stateFor = (repoRoot, name) => {
    const canonicalRoot = canonicalGitRepositoryRoot(repoRoot, `${name} source root`, { requireVoMod: true });
    const actualName = repoModuleName(canonicalRoot);
    if (actualName !== name) {
      throw new Error(`source repository identity mismatch at ${canonicalRoot}: expected ${name}, found ${actualName}`);
    }
    const knownRoot = rootsByName.get(name);
    if (knownRoot && knownRoot !== canonicalRoot) {
      throw new Error(`source repository identity ${name} resolves to multiple layouts`);
    }
    const knownState = statesByRoot.get(canonicalRoot);
    if (knownState && knownState.name !== name) {
      throw new Error(`source repository layout ${canonicalRoot} claims multiple identities`);
    }
    if (environment?.[repositoryEnvironmentName(name)]) {
      const mapped = canonicalGitRepositoryRoot(
        canonicalPathWithoutLayoutAlias(
          environment[repositoryEnvironmentName(name)],
          `${repositoryEnvironmentName(name)} for ${name}`,
          'directory',
        ),
        `${repositoryEnvironmentName(name)} for ${name}`,
        { requireVoMod: true },
      );
      if (mapped !== canonicalRoot) {
        throw new Error(`${name} source root does not match ${repositoryEnvironmentName(name)}`);
      }
    }
    rootsByName.set(name, canonicalRoot);
    if (knownState) return knownState;
    const state = {
      root: canonicalRoot,
      name,
      roles: new Set(),
      cargoPackages: new Map(),
      workspaceModules: new Set(),
    };
    statesByRoot.set(canonicalRoot, state);
    return state;
  };

  const rootState = stateFor(canonicalVoplayRoot, rootModule);
  rootState.roles.add('root');
  rootState.roles.add('workspace-owner');
  const workspaceModules = [];
  const seenWorkspaceModules = new Set();
  for (const module of moduleSnapshot.modules) {
    if (module.source.kind !== 'workspace') {
      throw new Error(`voplay source closure selected registry module ${module.module}`);
    }
    const repoRoot = realpathSync.native(module.source.directory);
    const moduleName = repoModuleName(repoRoot);
    if (moduleName === rootModule) throw new Error(`vo.work must not override its root module ${rootModule}`);
    if (seenWorkspaceModules.has(moduleName)) {
      throw new Error(`vo.work contains duplicate resolved module ${moduleName}`);
    }
    seenWorkspaceModules.add(moduleName);
    workspaceModules.push(moduleName);
    const state = stateFor(repoRoot, moduleName);
    state.roles.add('vo-workspace');
    state.workspaceModules.add(moduleName);
  }

  const reachableIds = reachableCargoPackageIds(metadata);
  const packagesById = new Map();
  for (const cargoPackage of metadata.packages) {
    if (!cargoPackage || typeof cargoPackage.id !== 'string' || packagesById.has(cargoPackage.id)) {
      throw new Error('cargo metadata contains an invalid or duplicate package record');
    }
    packagesById.set(cargoPackage.id, cargoPackage);
  }
  const cargoRootId = metadata.resolve.root
    ?? (metadata.workspace_default_members?.length === 1 ? metadata.workspace_default_members[0] : null);
  for (const id of reachableIds) {
    const cargoPackage = packagesById.get(id);
    if (!cargoPackage) throw new Error(`cargo metadata is missing package record ${id}`);
    if (cargoPackage.source !== null) continue;
    if (
      typeof cargoPackage.manifest_path !== 'string'
      || typeof cargoPackage.name !== 'string'
      || typeof cargoPackage.version !== 'string'
    ) {
      throw new Error(`resolved local Cargo package has incomplete identity: ${id}`);
    }
    const manifest = canonicalPathWithoutLayoutAlias(
      cargoPackage.manifest_path,
      `Cargo manifest for ${cargoPackage.name}`,
      'file',
    );
    if (pathWithin(canonicalVolangRoot, manifest)) continue;
    const packageRoot = path.dirname(manifest);
    const repoRoot = canonicalGitRepositoryRoot(
      gitOutput(['rev-parse', '--show-toplevel'], packageRoot),
      `Cargo repository for ${cargoPackage.name}`,
      { requireVoMod: true },
    );
    if (!pathWithin(repoRoot, manifest)) {
      throw new Error(`Cargo manifest escapes its Git repository: ${manifest}`);
    }
    const moduleName = repoModuleName(repoRoot);
    const state = stateFor(repoRoot, moduleName);
    state.roles.add('cargo-path');
    const manifestPath = canonicalPortableRelative(
      portableRelativePath(repoRoot, manifest),
      `Cargo manifest path for ${cargoPackage.name}`,
    );
    const packageKey = `${cargoPackage.name}\0${cargoPackage.version}\0${manifestPath}`;
    if (state.cargoPackages.has(packageKey)) {
      throw new Error(`Cargo metadata contains duplicate local package ${cargoPackage.name}`);
    }
    state.cargoPackages.set(packageKey, {
      name: cargoPackage.name,
      version: cargoPackage.version,
      manifest: manifestPath,
    });
    if (id === cargoRootId && repoRoot !== canonicalVoplayRoot) {
      throw new Error('Cargo metadata root package does not belong to the voplay repository');
    }
  }
  if (!rootState.roles.has('cargo-path')) {
    throw new Error('locked Cargo graph does not contain the local voplay root package');
  }

  workspaceModules.sort(compareUtf8Text);
  const repositories = [...statesByRoot.values()]
    .map((state) => observeSourceRepository(state, requireClean))
    .sort((left, right) => compareUtf8Text(left.name, right.name));
  const workspaceReadAfter = readBoundedUtf8File(workspaceFile, 'selected voplay workspace file');
  if (sha256(workspaceReadAfter.bytes) !== workspaceDigest) {
    throw new Error('selected voplay workspace file changed while provenance was collected');
  }
  const sourceClosure = {
    schemaVersion: VOPLAY_SOURCE_CLOSURE_SCHEMA_VERSION,
    kind: 'voplay.localSourceClosure',
    workspace: {
      repository: rootModule,
      path: 'vo.work',
      digest: workspaceDigest,
      modules: workspaceModules,
    },
    repositories,
  };
  sourceClosure.contentDigest = sourceClosureContentDigest(sourceClosure);
  sourceClosure.digest = sourceClosureDigest(sourceClosure);
  return { sourceClosure, workspaceFile };
}

function validSourceClosureCargoPackage(value) {
  if (!exactObjectKeys(value, ['manifest', 'name', 'version'])) return false;
  try {
    canonicalPortableRelative(value.manifest, 'sourceClosure Cargo manifest');
  } catch {
    return false;
  }
  return typeof value.name === 'string'
    && /^[A-Za-z0-9][A-Za-z0-9_-]{0,254}$/.test(value.name)
    && typeof value.version === 'string'
    && value.version.length > 0
    && Buffer.byteLength(value.version, 'utf8') <= 255
    && !/[\u0000-\u001f\u007f]/u.test(value.version);
}

/** Validate serialized source-closure evidence without persisting host paths. */
export function verifyVoplaySourceClosure(value, { expected = null } = {}) {
  const issues = [];
  if (!exactObjectKeys(value, [
    'contentDigest',
    'digest',
    'kind',
    'repositories',
    'schemaVersion',
    'workspace',
  ])) {
    return ['sourceClosure must contain the exact source-closure fields'];
  }
  if (value.schemaVersion !== VOPLAY_SOURCE_CLOSURE_SCHEMA_VERSION) {
    issues.push(`sourceClosure.schemaVersion must be ${VOPLAY_SOURCE_CLOSURE_SCHEMA_VERSION}`);
  }
  if (value.kind !== 'voplay.localSourceClosure') {
    issues.push('sourceClosure.kind must be voplay.localSourceClosure');
  }
  if (!exactObjectKeys(value.workspace, ['digest', 'modules', 'path', 'repository'])) {
    issues.push('sourceClosure.workspace must contain the exact workspace fields');
    return issues;
  }
  try {
    canonicalModuleName(value.workspace.repository, 'sourceClosure.workspace.repository');
    canonicalPortableRelative(value.workspace.path, 'sourceClosure.workspace.path');
  } catch (error) {
    issues.push(commandErrorDetail(error));
  }
  if (value.workspace.path !== 'vo.work') issues.push('sourceClosure.workspace.path must be vo.work');
  if (!/^sha256:[0-9a-f]{64}$/.test(value.workspace.digest ?? '')) {
    issues.push('sourceClosure.workspace.digest must be sha256');
  }
  if (
    !Array.isArray(value.workspace.modules)
    || value.workspace.modules.length > 10_000
    || value.workspace.modules.some((entry) => typeof entry !== 'string')
    || JSON.stringify(value.workspace.modules) !== JSON.stringify([...new Set(value.workspace.modules)].sort(compareUtf8Text))
  ) {
    issues.push('sourceClosure.workspace.modules must be a unique canonical list');
  } else {
    for (const [index, moduleName] of value.workspace.modules.entries()) {
      try {
        canonicalModuleName(moduleName, `sourceClosure.workspace.modules[${index}]`);
      } catch (error) {
        issues.push(commandErrorDetail(error));
      }
    }
  }
  if (
    !Array.isArray(value.repositories)
    || value.repositories.length === 0
    || value.repositories.length > 10_000
  ) {
    issues.push('sourceClosure.repositories must be a non-empty array');
    return issues;
  }
  const names = new Set();
  let rootCount = 0;
  let workspaceOwnerCount = 0;
  const workspaceModules = [];
  const allowedRoles = new Set(['cargo-path', 'root', 'vo-workspace', 'workspace-owner']);
  for (const [index, repository] of value.repositories.entries()) {
    const label = `sourceClosure.repositories[${index}]`;
    if (!exactObjectKeys(repository, [
      'cargoPackages',
      'commit',
      'digest',
      'dirty',
      'name',
      'roles',
      'workspaceModules',
    ])) {
      issues.push(`${label} must contain the exact repository fields`);
      continue;
    }
    try {
      canonicalModuleName(repository.name, `${label}.name`);
    } catch (error) {
      issues.push(commandErrorDetail(error));
    }
    if (names.has(repository.name)) issues.push(`${label}.name is duplicated`);
    names.add(repository.name);
    if (!/^[0-9a-f]{40}(?:[0-9a-f]{24})?$/.test(repository.commit ?? '')) {
      issues.push(`${label}.commit must be a canonical Git commit`);
    }
    if (typeof repository.dirty !== 'boolean') issues.push(`${label}.dirty must be boolean`);
    if (!/^sha256:[0-9a-f]{64}$/.test(repository.digest ?? '')) {
      issues.push(`${label}.digest must be sha256`);
    }
    if (
      !Array.isArray(repository.roles)
      || repository.roles.length === 0
      || repository.roles.length > allowedRoles.size
      || repository.roles.some((role) => !allowedRoles.has(role))
      || JSON.stringify(repository.roles) !== JSON.stringify([...new Set(repository.roles)].sort(compareUtf8Text))
    ) {
      issues.push(`${label}.roles must be a unique canonical role list`);
    } else {
      if (repository.roles.includes('root')) rootCount += 1;
      if (repository.roles.includes('workspace-owner')) workspaceOwnerCount += 1;
    }
    if (
      !Array.isArray(repository.cargoPackages)
      || repository.cargoPackages.length > 10_000
      || repository.cargoPackages.some((entry) => !validSourceClosureCargoPackage(entry))
      || new Set(repository.cargoPackages.map((entry) => (
        `${entry?.name}\0${entry?.version}\0${entry?.manifest}`
      ))).size !== repository.cargoPackages.length
      || JSON.stringify(repository.cargoPackages) !== JSON.stringify([...repository.cargoPackages].sort((left, right) => (
        compareUtf8Text(left.manifest, right.manifest)
        || compareUtf8Text(left.name, right.name)
        || compareUtf8Text(left.version, right.version)
      )))
    ) {
      issues.push(`${label}.cargoPackages must be a canonical local package list`);
    }
    if (
      !Array.isArray(repository.workspaceModules)
      || repository.workspaceModules.length > 10_000
      || repository.workspaceModules.some((entry) => typeof entry !== 'string')
      || JSON.stringify(repository.workspaceModules) !== JSON.stringify([...new Set(repository.workspaceModules)].sort(compareUtf8Text))
    ) {
      issues.push(`${label}.workspaceModules must be a unique canonical module list`);
    } else {
      workspaceModules.push(...repository.workspaceModules);
      for (const [moduleIndex, moduleName] of repository.workspaceModules.entries()) {
        try {
          canonicalModuleName(moduleName, `${label}.workspaceModules[${moduleIndex}]`);
        } catch (error) {
          issues.push(commandErrorDetail(error));
        }
      }
      if (
        repository.workspaceModules.length > 0
        && (
          repository.workspaceModules.length !== 1
          || repository.workspaceModules[0] !== repository.name
        )
      ) {
        issues.push(`${label}.workspaceModules must identify its repository module exactly`);
      }
    }
    if (
      Array.isArray(repository.roles)
      && Array.isArray(repository.cargoPackages)
      && Array.isArray(repository.workspaceModules)
    ) {
      const expectedRoles = [
        ...(repository.cargoPackages.length > 0 ? ['cargo-path'] : []),
        ...(repository.roles.includes('root') ? ['root'] : []),
        ...(repository.workspaceModules.length > 0 ? ['vo-workspace'] : []),
        ...(repository.roles.includes('workspace-owner') ? ['workspace-owner'] : []),
      ].sort(compareUtf8Text);
      if (JSON.stringify(repository.roles) !== JSON.stringify(expectedRoles)) {
        issues.push(`${label}.roles do not match its recorded source roles`);
      }
    }
  }
  const repositoryNamesAreStrings = value.repositories.every(
    (repository) => typeof repository?.name === 'string',
  );
  if (
    !repositoryNamesAreStrings
    || JSON.stringify(value.repositories) !== JSON.stringify(
      [...value.repositories].sort((left, right) => compareUtf8Text(left.name, right.name)),
    )
  ) {
    issues.push('sourceClosure.repositories must use canonical name ordering');
  }
  if (rootCount !== 1) issues.push('sourceClosure must contain exactly one root repository');
  if (workspaceOwnerCount !== 1) issues.push('sourceClosure must contain exactly one workspace owner');
  const owner = value.repositories.find((entry) => entry?.roles?.includes('workspace-owner'));
  const rootRepository = value.repositories.find((entry) => entry?.roles?.includes('root'));
  if (owner?.name !== value.workspace.repository) {
    issues.push('sourceClosure.workspace.repository must identify the workspace owner');
  }
  if (
    rootRepository?.name !== owner?.name
    || !rootRepository?.roles?.includes('cargo-path')
  ) {
    issues.push('sourceClosure root must be the Cargo-backed workspace owner');
  }
  const canonicalWorkspaceModules = [...new Set(workspaceModules)].sort(compareUtf8Text);
  if (JSON.stringify(value.workspace.modules) !== JSON.stringify(canonicalWorkspaceModules)) {
    issues.push('sourceClosure.workspace.modules do not match repository workspace roles');
  }
  if (!/^sha256:[0-9a-f]{64}$/.test(value.contentDigest ?? '')) {
    issues.push('sourceClosure.contentDigest must be sha256');
  } else if (issues.length === 0 && value.contentDigest !== sourceClosureContentDigest(value)) {
    issues.push('sourceClosure.contentDigest does not match its canonical content records');
  }
  if (!/^sha256:[0-9a-f]{64}$/.test(value.digest ?? '')) {
    issues.push('sourceClosure.digest must be sha256');
  } else if (issues.length === 0 && value.digest !== sourceClosureDigest(value)) {
    issues.push('sourceClosure.digest does not match its canonical closure records');
  }
  if (expected && JSON.stringify(value) !== JSON.stringify(expected)) {
    issues.push('sourceClosure does not match the current locked local source graph');
  }
  return issues;
}

function completeProducerDirectory(directory) {
  const isRegularFile = (file) => {
    try {
      return lstatSync(file).isFile();
    } catch {
      return false;
    }
  };
  return isRegularFile(path.join(directory, 'producer-manifest.json'))
    && VOPLAY_WASM_REQUIRED_OUTPUTS.every((name) => isRegularFile(path.join(directory, name)));
}

const ACTIVE_DIRECTORY_REPLACEMENTS = new Set();
const PUBLICATION_CLAIM_SCHEMA = 1;
const PUBLICATION_CLAIM_MAX_BYTES = 4096;
const PUBLICATION_CLAIM_NAME = /^claim-(\d+)-([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})\.json$/;
const PUBLICATION_CREATING_NAME = /^\.creating-(\d+)-([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})\.json$/;

function sameStableMetadata(left, right) {
  return left.dev === right.dev
    && left.ino === right.ino
    && left.mode === right.mode
    && left.nlink === right.nlink
    && left.size === right.size
    && left.mtimeNs === right.mtimeNs
    && left.ctimeNs === right.ctimeNs;
}

function sameDirectoryIdentity(left, right) {
  return left.dev === right.dev
    && left.ino === right.ino
    && left.mode === right.mode
    && left.nlink === right.nlink;
}

function stableRegularText(file, label, maxBytes = PUBLICATION_CLAIM_MAX_BYTES) {
  const before = lstatSync(file, { bigint: true });
  if (!before.isFile() || before.isSymbolicLink() || before.size > BigInt(maxBytes)) {
    throw new Error(`${label} must be a bounded regular file without symbolic links`);
  }
  const descriptor = openSync(
    file,
    fsConstants.O_RDONLY | (fsConstants.O_NOFOLLOW ?? 0),
  );
  try {
    const opened = fstatSync(descriptor, { bigint: true });
    if (!sameStableMetadata(before, opened)) {
      throw new Error(`${label} changed while it was being opened`);
    }
    const bytes = readFileSync(descriptor);
    const after = lstatSync(file, { bigint: true });
    if (!sameStableMetadata(opened, after)) {
      throw new Error(`${label} changed while it was being read`);
    }
    return {
      metadata: after,
      text: new TextDecoder('utf-8', { fatal: true, ignoreBOM: true }).decode(bytes),
    };
  } finally {
    closeSync(descriptor);
  }
}

function processStartIdentity(pid) {
  if (!Number.isSafeInteger(pid) || pid <= 0) return null;
  try {
    const value = execFileSync('ps', ['-p', String(pid), '-o', 'lstart='], {
      encoding: 'utf8',
      env: { ...process.env, LC_ALL: 'C' },
      maxBuffer: 4096,
      stdio: ['ignore', 'pipe', 'ignore'],
    }).trim();
    return value === '' ? null : value;
  } catch {
    return null;
  }
}

function processIsAlive(pid) {
  try {
    process.kill(pid, 0);
    return true;
  } catch (error) {
    return error?.code === 'EPERM';
  }
}

function publicationClaimOwner(file, expectedPid) {
  const { metadata, text } = stableRegularText(file, `publication claim ${file}`);
  let owner;
  try {
    owner = JSON.parse(text);
  } catch (error) {
    throw new Error(`publication claim ${file} is invalid JSON: ${commandErrorDetail(error)}`);
  }
  const keys = owner && typeof owner === 'object' ? Object.keys(owner).sort() : [];
  if (
    JSON.stringify(keys) !== JSON.stringify(['createdAt', 'nonce', 'pid', 'processIdentity', 'schemaVersion'])
    || owner.schemaVersion !== PUBLICATION_CLAIM_SCHEMA
    || owner.pid !== expectedPid
    || !Number.isSafeInteger(owner.pid)
    || owner.pid <= 0
    || typeof owner.nonce !== 'string'
    || !/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/.test(owner.nonce)
    || typeof owner.createdAt !== 'string'
    || Number.isNaN(Date.parse(owner.createdAt))
    || (owner.processIdentity !== null && typeof owner.processIdentity !== 'string')
  ) {
    throw new Error(`publication claim ${file} has an invalid owner record`);
  }
  return { metadata, owner };
}

function publicationClaimIsLive(owner) {
  if (!processIsAlive(owner.pid)) return false;
  if (owner.processIdentity !== null) {
    const currentIdentity = processStartIdentity(owner.pid);
    if (currentIdentity !== null && currentIdentity !== owner.processIdentity) return false;
  }
  return true;
}

function unlinkStableClaim(file, expected) {
  const current = lstatSync(file, { bigint: true });
  if (!sameStableMetadata(current, expected) || !current.isFile() || current.isSymbolicLink()) {
    throw new Error(`publication claim changed before cleanup: ${file}`);
  }
  unlinkSync(file);
}

function ensurePublicationClaimDirectory(directory) {
  try {
    mkdirSync(directory, { mode: 0o700 });
  } catch (error) {
    if (error?.code !== 'EEXIST') throw error;
  }
  const metadata = lstatSync(directory);
  const canonical = path.join(
    realpathSync.native(path.dirname(directory)),
    path.basename(directory),
  );
  if (
    !metadata.isDirectory()
    || metadata.isSymbolicLink()
    || realpathSync.native(directory) !== canonical
  ) {
    throw new Error(`publication claim root must be a real directory: ${directory}`);
  }
}

/**
 * Serialize one directory replacement with a set of unique live claims.
 * Every contender publishes its claim before scanning; if two arrive at once,
 * at least one observes the other and no contender may proceed while another
 * live claim exists. Dead claims are recoverable without stealing a fixed lock
 * path from a newly-started producer.
 */
export function withDirectoryReplacementLock(outDir, action, { warn = console.warn } = {}) {
  if (typeof action !== 'function') {
    throw new TypeError('directory replacement lock requires a synchronous action');
  }
  const destination = path.resolve(outDir);
  const parent = path.dirname(destination);
  mkdirSync(parent, { recursive: true });
  const claimRoot = `${destination}.publication-locks`;
  ensurePublicationClaimDirectory(claimRoot);

  const nonce = randomUUID();
  const owner = {
    schemaVersion: PUBLICATION_CLAIM_SCHEMA,
    pid: process.pid,
    nonce,
    createdAt: new Date().toISOString(),
    processIdentity: processStartIdentity(process.pid),
  };
  const temporary = path.join(claimRoot, `.creating-${process.pid}-${nonce}.json`);
  const claim = path.join(claimRoot, `claim-${process.pid}-${nonce}.json`);
  let descriptor;
  try {
    descriptor = openSync(
      temporary,
      fsConstants.O_WRONLY
        | fsConstants.O_CREAT
        | fsConstants.O_EXCL
        | (fsConstants.O_NOFOLLOW ?? 0),
      0o600,
    );
    writeFileSync(descriptor, `${JSON.stringify(owner)}\n`);
    fsyncSync(descriptor);
  } finally {
    if (descriptor !== undefined) closeSync(descriptor);
  }
  renameSync(temporary, claim);

  try {
    const contenders = [];
    for (const entry of readdirSync(claimRoot, { withFileTypes: true })) {
      if (entry.name === path.basename(claim)) continue;
      const claimMatch = PUBLICATION_CLAIM_NAME.exec(entry.name);
      const creatingMatch = PUBLICATION_CREATING_NAME.exec(entry.name);
      if (!claimMatch && !creatingMatch) {
        throw new Error(`publication claim root contains an unsupported entry: ${entry.name}`);
      }
      const candidate = path.join(claimRoot, entry.name);
      const expectedPid = Number((claimMatch ?? creatingMatch)[1]);
      let parsed;
      try {
        parsed = publicationClaimOwner(candidate, expectedPid);
      } catch (error) {
        if (!processIsAlive(expectedPid)) {
          const metadata = lstatSync(candidate, { bigint: true });
          if (metadata.isFile() && !metadata.isSymbolicLink()) unlinkStableClaim(candidate, metadata);
          continue;
        }
        if (creatingMatch) continue;
        throw error;
      }
      if (!publicationClaimIsLive(parsed.owner)) {
        unlinkStableClaim(candidate, parsed.metadata);
      } else if (claimMatch) {
        contenders.push(candidate);
      }
    }
    if (contenders.length > 0) {
      throw new Error(
        `another directory replacement is active for ${destination}: ${contenders.join(', ')}`,
      );
    }
    ACTIVE_DIRECTORY_REPLACEMENTS.add(destination);
    try {
      const result = action();
      if (result && typeof result.then === 'function') {
        throw new TypeError('directory replacement lock action must be synchronous');
      }
      return result;
    } finally {
      ACTIVE_DIRECTORY_REPLACEMENTS.delete(destination);
    }
  } finally {
    try {
      if (existsSync(claim)) {
        const parsed = publicationClaimOwner(claim, process.pid);
        if (parsed.owner.nonce !== nonce) {
          throw new Error(`publication claim owner changed before release: ${claim}`);
        }
        unlinkStableClaim(claim, parsed.metadata);
      }
      if (existsSync(temporary)) unlinkSync(temporary);
      try {
        rmdirSync(claimRoot);
      } catch (error) {
        if (error?.code !== 'ENOENT' && error?.code !== 'ENOTEMPTY') throw error;
      }
    } catch (error) {
      warn(`directory replacement claim cleanup failed: ${commandErrorDetail(error)}`);
    }
  }
}

function assertDirectoryReplacementLock(destination) {
  if (!ACTIVE_DIRECTORY_REPLACEMENTS.has(path.resolve(destination))) {
    throw new Error(`directory replacement requires an active publication lock: ${destination}`);
  }
}

function backupPathFor(destination) {
  const timestamp = String(Date.now()).padStart(13, '0');
  return `${destination}.backup-${timestamp}-${process.pid}-${randomUUID()}`;
}

function directoryReplacementBackupPaths(destination) {
  const parent = path.dirname(destination);
  if (!existsSync(parent)) return [];
  const prefix = `${path.basename(destination)}.backup-`;
  return readdirSync(parent, { withFileTypes: true })
    .filter((entry) => {
      if (!entry.name.startsWith(prefix)) return false;
      if (!entry.isDirectory() || entry.isSymbolicLink()) {
        throw new Error(`directory replacement backup is not a real directory: ${entry.name}`);
      }
      const suffix = entry.name.slice(prefix.length);
      return /^\d{13}-\d+-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/.test(suffix);
    })
    .map((entry) => path.join(parent, entry.name))
    .sort()
    .reverse();
}

function directoryReplacementCleanupPaths(destination) {
  const parent = path.dirname(destination);
  if (!existsSync(parent)) return [];
  const prefix = `${path.basename(destination)}.backup-`;
  return readdirSync(parent, { withFileTypes: true })
    .filter((entry) => {
      if (!entry.name.startsWith(prefix) || !entry.name.includes('.cleanup-')) return false;
      if (!entry.isDirectory() || entry.isSymbolicLink()) {
        throw new Error(`directory replacement cleanup entry is not a real directory: ${entry.name}`);
      }
      const suffix = entry.name.slice(prefix.length);
      return /^\d{13}-\d+-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\.cleanup-[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/.test(suffix);
    })
    .map((entry) => path.join(parent, entry.name));
}

function stableDirectoryMetadata(directory) {
  const before = lstatSync(directory, { bigint: true });
  const canonical = path.join(
    realpathSync.native(path.dirname(directory)),
    path.basename(directory),
  );
  if (
    !before.isDirectory()
    || before.isSymbolicLink()
    || realpathSync.native(directory) !== canonical
  ) {
    throw new Error(`directory replacement candidate must be a real directory: ${directory}`);
  }
  const after = lstatSync(directory, { bigint: true });
  if (!sameStableMetadata(before, after)) {
    throw new Error(`directory replacement candidate changed during inspection: ${directory}`);
  }
  return after;
}

function completeStableDirectory(directory, completeDirectory) {
  const before = stableDirectoryMetadata(directory);
  if (!completeDirectory(directory)) return null;
  const after = stableDirectoryMetadata(directory);
  if (!sameStableMetadata(before, after)) {
    throw new Error(`directory replacement candidate changed during validation: ${directory}`);
  }
  return after;
}

function restoreDirectoryReplacementCandidate(candidate, destination, expected) {
  const beforeRename = stableDirectoryMetadata(candidate);
  if (!sameStableMetadata(beforeRename, expected)) {
    throw new Error(`directory replacement backup changed before recovery: ${candidate}`);
  }
  renameSync(candidate, destination);
  try {
    const restored = stableDirectoryMetadata(destination);
    if (!sameDirectoryIdentity(restored, expected)) {
      throw new Error(`restored directory identity differs from the selected backup: ${destination}`);
    }
  } catch (error) {
    try {
      renameSync(destination, candidate);
    } catch (rollbackError) {
      throw new Error(
        `recovered an unverified directory at ${destination} and failed to move it back to ${candidate}: `
        + `${commandErrorDetail(error)}; rollback: ${commandErrorDetail(rollbackError)}`,
      );
    }
    throw error;
  }
}

function removeStableBackupDirectory(candidate, expected) {
  const beforeRename = stableDirectoryMetadata(candidate);
  if (!sameStableMetadata(beforeRename, expected)) {
    throw new Error(`directory replacement backup changed before cleanup: ${candidate}`);
  }
  const quarantine = `${candidate}.cleanup-${randomUUID()}`;
  renameSync(candidate, quarantine);
  const moved = stableDirectoryMetadata(quarantine);
  if (!sameDirectoryIdentity(moved, expected)) {
    try {
      renameSync(quarantine, candidate);
    } catch (rollbackError) {
      throw new Error(
        `backup identity changed during cleanup and rollback failed for ${candidate}: `
        + commandErrorDetail(rollbackError),
      );
    }
    throw new Error(`backup identity changed during cleanup: ${candidate}`);
  }
  rmSync(quarantine, { recursive: true, force: false });
}

export function cleanupDirectoryReplacementBackups(outDir, {
  completeDirectory = completeProducerDirectory,
  label = 'voplay current-source WASM',
  warn = console.warn,
} = {}) {
  if (typeof completeDirectory !== 'function') {
    throw new Error('directory replacement cleanup requires a completeness predicate');
  }
  const destination = path.resolve(outDir);
  assertDirectoryReplacementLock(destination);
  if (!existsSync(destination) || completeStableDirectory(destination, completeDirectory) === null) {
    throw new Error(`refusing to clean backups without a complete destination: ${destination}`);
  }
  for (const quarantine of directoryReplacementCleanupPaths(destination)) {
    try {
      const expected = stableDirectoryMetadata(quarantine);
      const current = stableDirectoryMetadata(quarantine);
      if (!sameStableMetadata(current, expected)) {
        throw new Error(`directory replacement cleanup entry changed: ${quarantine}`);
      }
      rmSync(quarantine, { recursive: true, force: false });
    } catch (error) {
      warn(`${label}: could not retry replacement cleanup ${quarantine}: ${commandErrorDetail(error)}`);
    }
  }
  for (const backup of directoryReplacementBackupPaths(destination)) {
    try {
      removeStableBackupDirectory(backup, stableDirectoryMetadata(backup));
    } catch (error) {
      warn(`${label}: could not clean replacement backup ${backup}: ${commandErrorDetail(error)}`);
    }
  }
}

/**
 * Restore the newest complete backup after a process died between the two
 * directory renames used by publishStagedDirectoryWithRollback. This closes
 * the restart-visible empty-destination state without relying on symlinks.
 */
export function recoverInterruptedDirectoryReplacement(outDir, {
  warn = console.warn,
  completeDirectory = completeProducerDirectory,
  label = 'voplay current-source WASM',
} = {}) {
  if (typeof completeDirectory !== 'function') {
    throw new Error('directory replacement recovery requires a completeness predicate');
  }
  const destination = path.resolve(outDir);
  assertDirectoryReplacementLock(destination);
  if (existsSync(destination)) return null;
  const backups = directoryReplacementBackupPaths(destination);
  if (backups.length === 0) return null;
  let selected = null;
  for (const backup of backups) {
    const metadata = completeStableDirectory(backup, completeDirectory);
    if (metadata !== null) {
      selected = { backup, metadata };
      break;
    }
  }
  if (selected === null) {
    throw new Error(
      `destination ${destination} is missing and no complete producer backup can be restored; `
      + `found: ${backups.join(', ')}`,
    );
  }
  restoreDirectoryReplacementCandidate(selected.backup, destination, selected.metadata);
  warn(`${label}: restored interrupted publication backup ${selected.backup}`);
  cleanupDirectoryReplacementBackups(destination, { completeDirectory, label, warn });
  return selected.backup;
}

/**
 * Publish a fully verified sibling staging directory with rollback for errors
 * observable by this producer. The two directory renames are not a filesystem
 * atomic exchange; startup recovery handles a crash between them. Injected
 * operations let the rollback path be proven without real filesystem faults.
 */
export function publishStagedDirectoryWithRollback(stagingDir, outDir, operations = {}) {
  const staging = path.resolve(stagingDir);
  const destination = path.resolve(outDir);
  if (staging === destination || path.dirname(staging) !== path.dirname(destination)) {
    throw new Error('staging and destination must be distinct sibling directories');
  }
  assertDirectoryReplacementLock(destination);

  const exists = operations.existsSync ?? existsSync;
  const rename = operations.renameSync ?? renameSync;
  const remove = operations.rmSync ?? rmSync;
  const label = operations.label ?? 'voplay current-source WASM';
  if (!exists(staging)) {
    throw new Error(`verified staging directory is missing: ${staging}`);
  }

  const backup = backupPathFor(destination);
  let previousMoved = false;
  try {
    if (exists(destination)) {
      rename(destination, backup);
      previousMoved = true;
    }
    rename(staging, destination);
  } catch (publishError) {
    if (previousMoved) {
      try {
        rename(backup, destination);
      } catch (rollbackError) {
        throw new Error(
          `failed to publish ${destination} and failed to restore its previous output; `
          + `the previous output remains at ${backup}. Publish error: ${commandErrorDetail(publishError)}. `
          + `Rollback error: ${commandErrorDetail(rollbackError)}`,
        );
      }
    }
    throw new Error(
      `failed to publish ${destination}; the previous output was ${previousMoved ? 'restored' : 'left unchanged'}. `
      + `Publish error: ${commandErrorDetail(publishError)}`,
    );
  }

  if (previousMoved) {
    try {
      remove(backup, { recursive: true, force: true });
    } catch (cleanupError) {
      console.warn(
        `${label}: published verified output, but could not remove backup ${backup}: `
        + commandErrorDetail(cleanupError),
      );
    }
  }
}

/** Resolve the exact feature/target graph consumed by the wasm-pack command. */
export function lockedVoplayCargoMetadata(voplayRoot) {
  const canonicalVoplayRoot = canonicalGitRepositoryRoot(
    canonicalPathWithoutLayoutAlias(voplayRoot, 'voplay Cargo metadata root', 'directory'),
    'voplay Cargo metadata root',
    { requireVoMod: true },
  );
  const manifestPath = canonicalPathWithoutLayoutAlias(
    path.join(canonicalVoplayRoot, 'rust', 'Cargo.toml'),
    'voplay Cargo.toml',
    'file',
  );
  const lockPath = canonicalPathWithoutLayoutAlias(
    path.join(canonicalVoplayRoot, 'rust', 'Cargo.lock'),
    'voplay Cargo.lock',
    'file',
  );
  const lockDigestBefore = sha256(readBoundedUtf8File(lockPath, 'voplay Cargo.lock').bytes);
  let metadataText;
  try {
    metadataText = execFileSync('cargo', [
      'metadata',
      '--format-version',
      '1',
      '--locked',
      '--manifest-path',
      manifestPath,
      ...VOPLAY_CARGO_METADATA_FEATURE_ARGS,
    ], {
      cwd: root,
      encoding: 'utf8',
      env: cleanGitEnvironment(),
      maxBuffer: 128 * 1024 * 1024,
      stdio: ['ignore', 'pipe', 'pipe'],
    });
  } catch (error) {
    const detail = commandErrorDetail(error);
    const staleOrInconsistent = /(?:lock file.*needs to be updated|failed to select a version|versions? that meet the requirements.*conflict|--locked was passed)/is.test(detail);
    throw new Error(
      staleOrInconsistent
        ? `voplay Cargo.lock is stale or internally inconsistent for the current Volang dependency graph: ${lockPath}. `
          + `Refresh and commit that lockfile in the voplay repository, update Volang's voplay expected_commit pin, then rerun the Quickplay task. `
          + `Cargo reported: ${detail}`
        : `locked voplay Cargo metadata could not be collected from ${lockPath}. `
          + `Keep Cargo.lock unchanged, make its recorded dependency sources available, then rerun the Quickplay task. `
          + `Cargo reported: ${detail}`,
    );
  }
  if (sha256(readBoundedUtf8File(lockPath, 'voplay Cargo.lock').bytes) !== lockDigestBefore) {
    throw new Error('voplay Cargo.lock changed while locked Cargo metadata was collected');
  }
  let metadata;
  try {
    metadata = JSON.parse(metadataText);
  } catch (error) {
    throw new Error(`cargo metadata returned invalid JSON for voplay: ${commandErrorDetail(error)}`);
  }
  return metadata;
}

export function lockedVoplayBuildInputs(
  voplayRoot,
  {
    volangRoot = root,
    environment = process.env,
    requireClean = false,
    voAuthority = null,
  } = {},
) {
  const metadata = lockedVoplayCargoMetadata(voplayRoot);
  const volangBuildInputs = deriveVoplayVolangBuildInputs(metadata, volangRoot);
  const { sourceClosure, workspaceFile } = deriveVoplaySourceClosure(metadata, voplayRoot, {
    volangRoot,
    environment,
    requireClean,
    voAuthority,
  });
  return {
    sourceClosure,
    workspaceFile,
    volangBuildInputs,
    ffiSourceFingerprint: voplayFfiSourceFingerprint(
      sourceClosure.contentDigest,
      volangBuildInputs.digest,
    ),
  };
}

/** Resolve the complete Volang-local path package set from voplay's locked graph. */
export function lockedVoplayVolangBuildInputs(voplayRoot, { volangRoot = root } = {}) {
  return deriveVoplayVolangBuildInputs(lockedVoplayCargoMetadata(voplayRoot), volangRoot);
}

/** Fail before wasm-pack can silently rewrite a first-party sibling lockfile. */
export function assertCleanVoplayBuildInputs(
  voplayRoot,
  { volangRoot = root, environment = process.env, voAuthority = null } = {},
) {
  return lockedVoplayBuildInputs(voplayRoot, {
    volangRoot,
    environment,
    requireClean: true,
    voAuthority,
  });
}

export function verifyCurrentVoplayWasm({
  voplayRoot,
  volangRoot = root,
  outDir = defaultOutDir,
  expectedCiRunId = null,
  expectedVolangBuildInputs = null,
  expectedSourceClosure = null,
  environment = process.env,
  requireClean = false,
  voAuthority = null,
}) {
  const issues = [];
  const manifestPath = path.join(outDir, 'producer-manifest.json');
  if (!existsSync(manifestPath)) {
    return { issues: ['producer-manifest.json missing'], manifest: null };
  }
  let manifest;
  try {
    manifest = JSON.parse(readBoundedUtf8File(
      manifestPath,
      'producer-manifest.json',
      VOPLAY_PRODUCER_MANIFEST_MAX_BYTES,
    ).text);
  } catch (error) {
    return { issues: [`producer manifest parse failed: ${error.message}`], manifest: null };
  }
  if (!manifest || typeof manifest !== 'object' || Array.isArray(manifest)) {
    return { issues: ['producer manifest must be an object'], manifest };
  }
  if (!exactObjectKeys(manifest, [
    'buildPlatform',
    'ciRunId',
    'command',
    'ffiSourceFingerprint',
    'generatedAt',
    'kind',
    'outputs',
    'schemaVersion',
    'sourceClosure',
    'toolchain',
    'volangBuildInputs',
  ])) {
    issues.push(
      `producer manifest must contain the exact schema-v${VOPLAY_WASM_PRODUCER_SCHEMA_VERSION} fields`,
    );
  }
  if (manifest.schemaVersion !== VOPLAY_WASM_PRODUCER_SCHEMA_VERSION) {
    issues.push(`schemaVersion must be ${VOPLAY_WASM_PRODUCER_SCHEMA_VERSION}`);
  }
  if (manifest.kind !== 'voplay.currentSourceWasm') {
    issues.push('kind must be voplay.currentSourceWasm');
  }
  if (
    typeof manifest.generatedAt !== 'string'
    || manifest.generatedAt.length > 64
    || Number.isNaN(Date.parse(manifest.generatedAt))
    || new Date(manifest.generatedAt).toISOString() !== manifest.generatedAt
  ) {
    issues.push('generatedAt must be a canonical ISO-8601 instant');
  }
  if (
    manifest.ciRunId !== null
    && (
      typeof manifest.ciRunId !== 'string'
      || manifest.ciRunId.length === 0
      || Buffer.byteLength(manifest.ciRunId, 'utf8') > 255
      || /[\u0000-\u001f\u007f]/u.test(manifest.ciRunId)
    )
  ) {
    issues.push('ciRunId must be null or a bounded non-empty string');
  }
  if (
    !exactObjectKeys(manifest.toolchain, ['rustc', 'wasmPack'])
    || [manifest.toolchain?.rustc, manifest.toolchain?.wasmPack].some((value) => (
      typeof value !== 'string'
      || value.length === 0
      || Buffer.byteLength(value, 'utf8') > 1024
      || /[\u0000-\u001f\u007f]/u.test(value)
    ))
  ) {
    issues.push('toolchain must contain bounded rustc and wasmPack versions');
  }
  if (JSON.stringify(manifest.command) !== JSON.stringify(VOPLAY_WASM_PRODUCER_COMMAND)) {
    issues.push('producer command does not match canonical wasm-pack command');
  }
  if (JSON.stringify(manifest.buildPlatform) !== JSON.stringify(currentVoplayWasmBuildPlatform())) {
    issues.push('buildPlatform does not match the current build host');
  }
  if (expectedCiRunId && manifest.ciRunId !== expectedCiRunId) {
    issues.push(`ciRunId ${manifest.ciRunId ?? '(missing)'} did not match ${expectedCiRunId}`);
  }
  let lockedVolangBuildInputs = expectedVolangBuildInputs;
  let currentSourceClosure = expectedSourceClosure;
  let workspaceFile = null;
  if (!lockedVolangBuildInputs || !currentSourceClosure) {
    try {
      const current = lockedVoplayBuildInputs(voplayRoot, {
        volangRoot,
        environment,
        requireClean: false,
        voAuthority,
      });
      lockedVolangBuildInputs ??= current.volangBuildInputs;
      currentSourceClosure ??= current.sourceClosure;
      workspaceFile = current.workspaceFile;
    } catch (error) {
      issues.push(`locked local source graph cannot be derived: ${commandErrorDetail(error)}`);
    }
  }
  issues.push(...verifyVoplayVolangBuildInputs(manifest.volangBuildInputs, volangRoot, {
    expected: lockedVolangBuildInputs,
  }));
  issues.push(...verifyVoplaySourceClosure(manifest.sourceClosure, {
    expected: currentSourceClosure,
  }));
  if (
    requireClean
    && currentSourceClosure?.repositories?.some((repository) => repository.dirty)
  ) {
    issues.push('current voplay local source closure contains a dirty repository');
  }
  let manifestFfiSourceFingerprint = null;
  try {
    manifestFfiSourceFingerprint = voplayFfiSourceFingerprint(
      manifest.sourceClosure?.contentDigest,
      manifest.volangBuildInputs?.digest,
    );
    if (manifest.ffiSourceFingerprint !== manifestFfiSourceFingerprint) {
      issues.push('ffiSourceFingerprint does not match the manifest source digests');
    }
  } catch (error) {
    issues.push(`ffiSourceFingerprint cannot be derived: ${commandErrorDetail(error)}`);
  }
  if (lockedVolangBuildInputs && currentSourceClosure) {
    try {
      const currentFfiSourceFingerprint = voplayFfiSourceFingerprint(
        currentSourceClosure.contentDigest,
        lockedVolangBuildInputs.digest,
      );
      if (manifest.ffiSourceFingerprint !== currentFfiSourceFingerprint) {
        issues.push('ffiSourceFingerprint does not match the current sibling closure and Volang inputs');
      }
    } catch (error) {
      issues.push(`current FFI source fingerprint cannot be derived: ${commandErrorDetail(error)}`);
    }
  }
  const outputs = VOPLAY_WASM_REQUIRED_OUTPUTS.map((name) => outputEntry(outDir, name));
  if (JSON.stringify(manifest.outputs) !== JSON.stringify(outputs)) {
    issues.push('output digest set does not match current WASM artifacts');
  }
  if (outputs.some((output) => output.missing || output.invalid)) {
    issues.push('required current-source WASM output is missing, oversized, or non-regular');
  } else {
    try {
      const catalog = extensionExportCatalogFromDirectory(voplayRoot, {
        modulePath: VOPLAY_MODULE,
        extensionName: 'voplay',
        rustEntrySource: 'rust/src/externs/mod.rs',
      });
      assertBindgenWasmExtensionV3(
        readFileSync(path.join(outDir, 'voplay_island_bg.wasm')),
        readFileSync(path.join(outDir, 'voplay_island.js')),
        {
          expectedExportKeys: catalog.exportKeys,
          label: 'current-source voplay bindgen extension',
          maxWasmBytes: VOPLAY_WASM_OUTPUT_MAX_BYTES,
          maxJsBytes: VOPLAY_WASM_OUTPUT_MAX_BYTES,
        },
      );
    } catch (error) {
      issues.push(`voplay browser protocol-v3 contract failed: ${commandErrorDetail(error)}`);
    }
  }
  return {
    issues,
    manifest,
    outputs,
    currentSourceClosure,
    workspaceFile,
    volangBuildInputs: manifest.volangBuildInputs ?? null,
  };
}

function buildCurrentVoplayWasm() {
  const voplayRoot = requireRepoRoot('VOPLAY_ROOT', 'voplay');
  const outDir = path.resolve(process.env.VOPLAY_CURRENT_WASM_OUT_DIR ?? defaultOutDir);
  return withDirectoryReplacementLock(outDir, () => buildCurrentVoplayWasmLocked(voplayRoot, outDir));
}

function buildCurrentVoplayWasmLocked(voplayRoot, outDir) {
  recoverInterruptedDirectoryReplacement(outDir);
  if (existsSync(outDir) && completeProducerDirectory(outDir)) {
    cleanupDirectoryReplacementBackups(outDir);
  }
  const voAuthority = createVoBinaryAuthority({
    root,
    environment: cleanGitEnvironment(),
  });
  const preflight = lockedVoplayBuildInputs(voplayRoot, {
    requireClean: false,
    voAuthority,
  });
  mkdirSync(path.dirname(outDir), { recursive: true });
  const stagingDir = mkdtempSync(
    path.join(path.dirname(outDir), `.${path.basename(outDir)}.staging-`),
  );
  try {
    execFileSync('wasm-pack', [
      'build',
      '--target',
      'web',
      '--release',
      '--out-dir',
      stagingDir,
      '--out-name',
      'voplay_island',
      path.join(voplayRoot, 'rust'),
      '--no-default-features',
      '--features',
      'wasm-island',
      '--locked',
    ], {
      cwd: root,
      env: voplayWasmBuildEnvironment(preflight, cleanGitEnvironment()),
      stdio: 'inherit',
    });
    const postflight = lockedVoplayBuildInputs(voplayRoot, {
      requireClean: false,
      voAuthority,
    });
    if (JSON.stringify(postflight) !== JSON.stringify(preflight)) {
      throw new Error('voplay or scoped Volang build inputs changed while wasm-pack was running');
    }
    const manifest = {
      schemaVersion: VOPLAY_WASM_PRODUCER_SCHEMA_VERSION,
      kind: 'voplay.currentSourceWasm',
      generatedAt: new Date().toISOString(),
      ciRunId: process.env.VO_DEV_CI_RUN_ID ?? null,
      command: VOPLAY_WASM_PRODUCER_COMMAND,
      sourceClosure: preflight.sourceClosure,
      volangBuildInputs: preflight.volangBuildInputs,
      ffiSourceFingerprint: preflight.ffiSourceFingerprint,
      toolchain: {
        rustc: toolVersion('rustc', ['--version']),
        wasmPack: toolVersion('wasm-pack', ['--version']),
      },
      buildPlatform: currentVoplayWasmBuildPlatform(),
      outputs: VOPLAY_WASM_REQUIRED_OUTPUTS.map((name) => outputEntry(stagingDir, name)),
    };
    writeFileSync(
      path.join(stagingDir, 'producer-manifest.json'),
      `${JSON.stringify(manifest, null, 2)}\n`,
    );
    const verification = verifyCurrentVoplayWasm({
      voplayRoot,
      outDir: stagingDir,
      expectedCiRunId: process.env.VO_DEV_CI_RUN_ID ?? null,
      expectedVolangBuildInputs: postflight.volangBuildInputs,
      expectedSourceClosure: postflight.sourceClosure,
      voAuthority,
    });
    if (verification.issues.length > 0) {
      throw new Error(`current-source voplay WASM verification failed: ${verification.issues.join('; ')}`);
    }
    publishStagedDirectoryWithRollback(stagingDir, outDir);
    cleanupDirectoryReplacementBackups(outDir);
  } finally {
    rmSync(stagingDir, { recursive: true, force: true });
  }
  console.log(`voplay current-source WASM: ok (${path.relative(root, outDir)})`);
}

if (import.meta.url === pathToFileURL(process.argv[1] ?? '').href) {
  try {
    buildCurrentVoplayWasm();
  } catch (error) {
    console.error(`voplay current-source WASM: ${error instanceof Error ? error.message : String(error)}`);
    process.exitCode = 1;
  }
}
