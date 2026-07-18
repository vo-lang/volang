#!/usr/bin/env node
import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import {
  existsSync,
  lstatSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  rmSync,
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
  gitCommit,
  sourceTreeDeclaredOutputPaths,
  sourceTreeDigestExcludingDeclaredOutputs,
} from './source_bound_evidence.mjs';
import {
  cleanupDirectoryReplacementBackups,
  deriveVoplayVolangBuildInputs,
  publishStagedDirectoryWithRollback,
  recoverInterruptedDirectoryReplacement,
  verifyVoplayVolangBuildInputs,
  withDirectoryReplacementLock,
} from './voplay_current_wasm.mjs';
import {
  assertStandaloneWasmExtensionV3,
  extensionExportCatalogFromDirectory,
  standaloneHostImportsFromRustSource,
  VOGUI_STANDALONE_HOST_IMPORTS_V3,
} from './wasm_protocol_v3.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const defaultOutDir = path.join(root, 'target', 'vogui-current-wasm');
const VOGUI_OUTPUT_MAX_BYTES = 35_000_000;
const VOGUI_MANIFEST_MAX_BYTES = 8 * 1024 * 1024;
const VOGUI_SOURCE_TEXT_MAX_BYTES = 16 * 1024 * 1024;
const VOGUI_MODULE = 'github.com/vo-lang/vogui';
const VOGUI_STANDALONE_HOST_AUTHORITY = 'rust/ext/src/standalone.rs';

export const VOGUI_WASM_PRODUCER_SCHEMA_VERSION = 3;
export const VOGUI_SOURCE_SCHEMA_VERSION = 2;
export const VOGUI_FFI_SOURCE_FINGERPRINT_ENV = 'VO_FFI_SOURCE_FINGERPRINT';
export const VOGUI_WORKSPACE_ENV = 'VOWORK';
export const VOGUI_FFI_SOURCE_FINGERPRINT_NAMESPACE = 'vogui-ffi-source-v2';
export const VOGUI_SOURCE_OUTPUT_DECLARATION = 'vogui.current-source-wasm';
export const VOGUI_CARGO_METADATA_FEATURE_ARGS = Object.freeze([
  '--no-default-features',
  '--features',
  'wasm-standalone',
  '--filter-platform',
  'wasm32-unknown-unknown',
]);
export const VOGUI_WASM_PRODUCER_COMMAND = Object.freeze([
  'cargo',
  'build',
  '--release',
  '--target',
  'wasm32-unknown-unknown',
  '--target-dir',
  '<BUILD_DIR>',
  '--manifest-path',
  '<VOGUI_ROOT>/rust/ext/Cargo.toml',
  '--no-default-features',
  '--features',
  'wasm-standalone',
  '--locked',
]);
export const VOGUI_CARGO_OUTPUT = 'vo_vogui.wasm';
export const VOGUI_WASM_REQUIRED_OUTPUTS = Object.freeze(['vogui.wasm']);
const VOGUI_DECLARED_SOURCE_OUTPUTS = sourceTreeDeclaredOutputPaths(
  VOGUI_SOURCE_OUTPUT_DECLARATION,
);
if (
  JSON.stringify(VOGUI_DECLARED_SOURCE_OUTPUTS.map((relative) => path.posix.basename(relative)))
  !== JSON.stringify(VOGUI_WASM_REQUIRED_OUTPUTS)
) {
  throw new Error('vogui source-output declaration does not match its required WASM outputs');
}

function sha256(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function commandErrorDetail(error) {
  return String(error?.stderr || error?.message || error).trim();
}

function exactObjectKeys(value, expected) {
  return value
    && typeof value === 'object'
    && !Array.isArray(value)
    && JSON.stringify(Object.keys(value).sort()) === JSON.stringify([...expected].sort());
}

function validDigest(value) {
  return typeof value === 'string' && /^sha256:[0-9a-f]{64}$/.test(value);
}

function validCommit(value) {
  return typeof value === 'string' && /^[0-9a-f]{40}(?:[0-9a-f]{24})?$/.test(value);
}

function validBoundedText(value, maxBytes = 1024) {
  return typeof value === 'string'
    && value.length > 0
    && Buffer.byteLength(value, 'utf8') <= maxBytes
    && !/[\u0000-\u001f\u007f]/u.test(value);
}

function canonicalFile(file, label) {
  const lexical = path.resolve(file);
  let canonical;
  let metadata;
  try {
    canonical = realpathSync.native(lexical);
    metadata = lstatSync(canonical);
  } catch (error) {
    throw new Error(`${label} cannot be resolved: ${commandErrorDetail(error)}`);
  }
  if (canonical !== lexical || metadata.isSymbolicLink() || !metadata.isFile()) {
    throw new Error(`${label} must be a canonical regular file without layout aliases`);
  }
  return canonical;
}

function readBoundedFile(file, label, maxBytes = VOGUI_SOURCE_TEXT_MAX_BYTES) {
  const canonical = canonicalFile(file, label);
  const metadata = lstatSync(canonical);
  if (metadata.size > maxBytes) {
    throw new Error(`${label} exceeds the ${maxBytes}-byte limit`);
  }
  return Buffer.from(readFileSync(canonical));
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

function gitStatusText(repoRoot) {
  return gitOutput([
    'status',
    '--porcelain=v2',
    '--untracked-files=all',
    '--',
    '.',
    ...VOGUI_DECLARED_SOURCE_OUTPUTS
      .map((relative) => `:(top,exclude,literal)${relative}`),
  ], repoRoot);
}

function moduleFromVoMod(bytes) {
  let text;
  try {
    text = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true }).decode(bytes);
  } catch (error) {
    throw new Error(`vogui vo.mod is not valid UTF-8: ${commandErrorDetail(error)}`);
  }
  const matches = [...text.matchAll(/^\s*module\s*=\s*"([^"]+)"\s*$/gmu)];
  if (matches.length !== 1 || matches[0][1] !== VOGUI_MODULE) {
    throw new Error(`vogui vo.mod must declare exactly module = "${VOGUI_MODULE}"`);
  }
  return matches[0][1];
}

export function currentVoguiWasmBuildPlatform() {
  return { os: process.platform, arch: process.arch };
}

export function voguiFfiSourceFingerprint(sourceDigest, volangBuildInputsDigest) {
  if (!validDigest(sourceDigest)) {
    throw new Error('vogui source digest must be a canonical sha256 digest');
  }
  if (!validDigest(volangBuildInputsDigest)) {
    throw new Error('Volang build-input digest must be a canonical sha256 digest');
  }
  return sha256(Buffer.from(
    `${VOGUI_FFI_SOURCE_FINGERPRINT_NAMESPACE}\0${sourceDigest}\0${volangBuildInputsDigest}\0`,
    'utf8',
  ));
}

/** Capture the current vogui checkout, including dirty tracked and untracked source. */
export function captureVoguiSource(voguiRoot) {
  const canonicalRoot = canonicalGitRepositoryRoot(voguiRoot, 'vogui source root', {
    requireVoMod: true,
  });
  const modPath = path.join(canonicalRoot, 'vo.mod');
  const workspacePath = path.join(canonicalRoot, 'vo.work');
  const lockPath = path.join(canonicalRoot, 'rust', 'Cargo.lock');

  const statusBefore = gitStatusText(canonicalRoot);
  const commitBefore = gitCommit(canonicalRoot);
  const modBefore = readBoundedFile(modPath, 'vogui vo.mod');
  const workspaceBefore = readBoundedFile(workspacePath, 'vogui vo.work');
  const lockBefore = readBoundedFile(lockPath, 'vogui Cargo.lock');
  const digest = sourceTreeDigestExcludingDeclaredOutputs(
    canonicalRoot,
    VOGUI_SOURCE_OUTPUT_DECLARATION,
  );
  const modAfter = readBoundedFile(modPath, 'vogui vo.mod');
  const workspaceAfter = readBoundedFile(workspacePath, 'vogui vo.work');
  const lockAfter = readBoundedFile(lockPath, 'vogui Cargo.lock');
  const commitAfter = gitCommit(canonicalRoot);
  const statusAfter = gitStatusText(canonicalRoot);

  if (
    commitBefore !== commitAfter
    || statusBefore !== statusAfter
    || !modBefore.equals(modAfter)
    || !workspaceBefore.equals(workspaceAfter)
    || !lockBefore.equals(lockAfter)
  ) {
    throw new Error('vogui source changed while provenance was collected');
  }
  if (!validCommit(commitAfter)) {
    throw new Error('vogui source has no canonical Git commit');
  }
  return {
    schemaVersion: VOGUI_SOURCE_SCHEMA_VERSION,
    kind: 'vogui.currentSource',
    module: moduleFromVoMod(modAfter),
    commit: commitAfter,
    dirty: statusAfter !== '',
    digest,
    workspace: {
      path: 'vo.work',
      size: workspaceAfter.byteLength,
      digest: sha256(workspaceAfter),
    },
    cargoLock: {
      path: 'rust/Cargo.lock',
      size: lockAfter.byteLength,
      digest: sha256(lockAfter),
    },
  };
}

export function verifyVoguiSource(value, { expected = null } = {}) {
  const issues = [];
  if (!exactObjectKeys(value, [
    'cargoLock',
    'commit',
    'digest',
    'dirty',
    'kind',
    'module',
    'schemaVersion',
    'workspace',
  ])) {
    return ['source must contain the exact vogui current-source fields'];
  }
  if (value.schemaVersion !== VOGUI_SOURCE_SCHEMA_VERSION) {
    issues.push(`source.schemaVersion must be ${VOGUI_SOURCE_SCHEMA_VERSION}`);
  }
  if (value.kind !== 'vogui.currentSource') issues.push('source.kind must be vogui.currentSource');
  if (value.module !== VOGUI_MODULE) issues.push(`source.module must be ${VOGUI_MODULE}`);
  if (!validCommit(value.commit)) issues.push('source.commit must be a canonical Git commit');
  if (typeof value.dirty !== 'boolean') issues.push('source.dirty must be boolean');
  if (!validDigest(value.digest)) issues.push('source.digest must be sha256');
  for (const [field, pathValue] of [
    ['workspace', 'vo.work'],
    ['cargoLock', 'rust/Cargo.lock'],
  ]) {
    const entry = value[field];
    if (!exactObjectKeys(entry, ['digest', 'path', 'size'])) {
      issues.push(`source.${field} must contain the exact file binding fields`);
      continue;
    }
    if (entry.path !== pathValue) issues.push(`source.${field}.path must be ${pathValue}`);
    if (!Number.isSafeInteger(entry.size) || entry.size < 0 || entry.size > VOGUI_SOURCE_TEXT_MAX_BYTES) {
      issues.push(`source.${field}.size is invalid`);
    }
    if (!validDigest(entry.digest)) issues.push(`source.${field}.digest must be sha256`);
  }
  if (expected && JSON.stringify(value) !== JSON.stringify(expected)) {
    issues.push('source does not match the current vogui checkout');
  }
  return issues;
}

function selectedRootCargoMetadata(metadata, manifestPath) {
  if (!metadata?.resolve || !Array.isArray(metadata.packages)) {
    throw new Error('cargo metadata did not contain a resolved package graph');
  }
  const roots = metadata.packages.filter((cargoPackage) => {
    if (typeof cargoPackage?.manifest_path !== 'string') return false;
    try {
      return realpathSync.native(cargoPackage.manifest_path) === manifestPath;
    } catch {
      return false;
    }
  });
  if (roots.length !== 1 || roots[0].name !== 'vogui' || typeof roots[0].id !== 'string') {
    throw new Error('locked metadata did not identify exactly one vogui extension package');
  }
  return {
    ...metadata,
    resolve: { ...metadata.resolve, root: roots[0].id },
  };
}

/** Resolve the exact standalone Cargo graph without allowing Cargo.lock updates. */
export function lockedVoguiCargoMetadata(voguiRoot) {
  const canonicalRoot = canonicalGitRepositoryRoot(voguiRoot, 'vogui Cargo metadata root', {
    requireVoMod: true,
  });
  const manifestPath = canonicalFile(
    path.join(canonicalRoot, 'rust', 'ext', 'Cargo.toml'),
    'vogui extension Cargo.toml',
  );
  const lockPath = canonicalFile(
    path.join(canonicalRoot, 'rust', 'Cargo.lock'),
    'vogui Cargo.lock',
  );
  const lockDigestBefore = sha256(readBoundedFile(lockPath, 'vogui Cargo.lock'));
  let text;
  try {
    text = execFileSync('cargo', [
      'metadata',
      '--format-version',
      '1',
      '--locked',
      '--manifest-path',
      manifestPath,
      ...VOGUI_CARGO_METADATA_FEATURE_ARGS,
    ], {
      cwd: root,
      encoding: 'utf8',
      env: cleanGitEnvironment(),
      maxBuffer: 128 * 1024 * 1024,
      stdio: ['ignore', 'pipe', 'pipe'],
    });
  } catch (error) {
    const detail = commandErrorDetail(error);
    throw new Error(
      `locked vogui Cargo metadata could not be collected from ${lockPath}: ${detail}`,
    );
  }
  if (sha256(readBoundedFile(lockPath, 'vogui Cargo.lock')) !== lockDigestBefore) {
    throw new Error('vogui Cargo.lock changed while locked metadata was collected');
  }
  let metadata;
  try {
    metadata = JSON.parse(text);
  } catch (error) {
    throw new Error(`cargo metadata returned invalid JSON for vogui: ${commandErrorDetail(error)}`);
  }
  return selectedRootCargoMetadata(metadata, manifestPath);
}

export function lockedVoguiBuildInputs(voguiRoot, { volangRoot = root } = {}) {
  const metadata = lockedVoguiCargoMetadata(voguiRoot);
  assertVoguiStandaloneHostImportAuthority(voguiRoot);
  const source = captureVoguiSource(voguiRoot);
  const volangBuildInputs = deriveVoplayVolangBuildInputs(metadata, volangRoot);
  return {
    source,
    volangBuildInputs,
    ffiSourceFingerprint: voguiFfiSourceFingerprint(source.digest, volangBuildInputs.digest),
  };
}

/** Prove the protocol table is identical to Vogui's authenticated Rust extern ABI. */
export function assertVoguiStandaloneHostImportAuthority(voguiRoot) {
  const canonicalRoot = canonicalGitRepositoryRoot(voguiRoot, 'vogui host ABI root', {
    requireVoMod: true,
  });
  const derived = standaloneHostImportsFromRustSource(
    readBoundedFile(
      path.join(canonicalRoot, ...VOGUI_STANDALONE_HOST_AUTHORITY.split('/')),
      'vogui standalone Rust host ABI',
    ),
    'vogui standalone Rust host ABI',
  );
  if (JSON.stringify(derived) !== JSON.stringify(VOGUI_STANDALONE_HOST_IMPORTS_V3)) {
    throw new Error('vogui standalone Rust host ABI differs from the protocol-v3 authority table');
  }
  return derived;
}

export function voguiWasmBuildEnvironment(preflight, voguiRoot, baseEnvironment = process.env) {
  const canonicalRoot = canonicalGitRepositoryRoot(voguiRoot, 'vogui build root', {
    requireVoMod: true,
  });
  const workspaceFile = canonicalFile(path.join(canonicalRoot, 'vo.work'), 'vogui vo.work');
  const fingerprint = voguiFfiSourceFingerprint(
    preflight?.source?.digest,
    preflight?.volangBuildInputs?.digest,
  );
  if (preflight?.ffiSourceFingerprint !== fingerprint) {
    throw new Error('vogui preflight FFI source fingerprint does not match its input digests');
  }
  const workspaceBytes = readBoundedFile(workspaceFile, 'vogui vo.work');
  if (
    preflight.source?.workspace?.path !== 'vo.work'
    || preflight.source.workspace.size !== workspaceBytes.byteLength
    || preflight.source.workspace.digest !== sha256(workspaceBytes)
  ) {
    throw new Error('vogui preflight workspace binding is stale');
  }
  return {
    ...baseEnvironment,
    [VOGUI_FFI_SOURCE_FINGERPRINT_ENV]: fingerprint,
    [VOGUI_WORKSPACE_ENV]: workspaceFile,
  };
}

function outputEntry(outDir, name) {
  const file = path.join(outDir, name);
  try {
    if (!existsSync(file)) return { name, missing: true };
    const metadata = lstatSync(file);
    if (!metadata.isFile() || metadata.isSymbolicLink() || realpathSync.native(file) !== path.resolve(file)) {
      return { name, invalid: 'not-canonical-regular-file' };
    }
    if (metadata.size > VOGUI_OUTPUT_MAX_BYTES) return { name, invalid: 'oversized' };
    const bytes = readFileSync(file);
    return { name, size: bytes.byteLength, digest: sha256(bytes) };
  } catch {
    return { name, invalid: 'unreadable' };
  }
}

export function verifyCurrentVoguiWasm({
  voguiRoot,
  volangRoot = root,
  outDir = defaultOutDir,
  expectedCiRunId = undefined,
  expectedSource = null,
  expectedVolangBuildInputs = null,
}) {
  const manifestPath = path.join(outDir, 'producer-manifest.json');
  if (!existsSync(manifestPath)) {
    return { issues: ['producer-manifest.json missing'], manifest: null, outputs: [] };
  }
  let manifest;
  try {
    const bytes = readBoundedFile(
      manifestPath,
      'vogui producer-manifest.json',
      VOGUI_MANIFEST_MAX_BYTES,
    );
    manifest = JSON.parse(new TextDecoder('utf-8', { fatal: true }).decode(bytes));
  } catch (error) {
    return {
      issues: [`producer manifest parse failed: ${commandErrorDetail(error)}`],
      manifest: null,
      outputs: [],
    };
  }
  const issues = [];
  if (!exactObjectKeys(manifest, [
    'buildPlatform',
    'ciRunId',
    'command',
    'ffiSourceFingerprint',
    'generatedAt',
    'kind',
    'outputs',
    'schemaVersion',
    'source',
    'toolchain',
    'volangBuildInputs',
  ])) {
    issues.push(`producer manifest must contain the exact schema-v${VOGUI_WASM_PRODUCER_SCHEMA_VERSION} fields`);
  }
  if (manifest.schemaVersion !== VOGUI_WASM_PRODUCER_SCHEMA_VERSION) {
    issues.push(`schemaVersion must be ${VOGUI_WASM_PRODUCER_SCHEMA_VERSION}`);
  }
  if (manifest.kind !== 'vogui.currentSourceWasm') {
    issues.push('kind must be vogui.currentSourceWasm');
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
    && (!validBoundedText(manifest.ciRunId, 255))
  ) {
    issues.push('ciRunId must be null or a bounded non-empty string');
  }
  if (expectedCiRunId !== undefined && manifest.ciRunId !== expectedCiRunId) {
    issues.push(`ciRunId ${manifest.ciRunId ?? '(missing)'} did not match ${expectedCiRunId ?? '(null)'}`);
  }
  if (JSON.stringify(manifest.command) !== JSON.stringify(VOGUI_WASM_PRODUCER_COMMAND)) {
    issues.push('producer command does not match the canonical cargo command');
  }
  if (JSON.stringify(manifest.buildPlatform) !== JSON.stringify(currentVoguiWasmBuildPlatform())) {
    issues.push('buildPlatform does not match the current build host');
  }
  if (
    !exactObjectKeys(manifest.toolchain, ['cargo', 'rustc'])
    || !validBoundedText(manifest.toolchain?.cargo)
    || !validBoundedText(manifest.toolchain?.rustc)
  ) {
    issues.push('toolchain must contain bounded cargo and rustc versions');
  }

  try {
    assertVoguiStandaloneHostImportAuthority(voguiRoot);
  } catch (error) {
    issues.push(`vogui standalone host ABI authority failed: ${commandErrorDetail(error)}`);
  }

  let currentSource = expectedSource;
  let lockedVolangBuildInputs = expectedVolangBuildInputs;
  if (!currentSource || !lockedVolangBuildInputs) {
    try {
      const current = lockedVoguiBuildInputs(voguiRoot, { volangRoot });
      currentSource ??= current.source;
      lockedVolangBuildInputs ??= current.volangBuildInputs;
    } catch (error) {
      issues.push(`locked vogui build inputs cannot be derived: ${commandErrorDetail(error)}`);
    }
  }
  issues.push(...verifyVoguiSource(manifest.source, { expected: currentSource }));
  issues.push(...verifyVoplayVolangBuildInputs(manifest.volangBuildInputs, volangRoot, {
    expected: lockedVolangBuildInputs,
  }));
  try {
    const fingerprint = voguiFfiSourceFingerprint(
      manifest.source?.digest,
      manifest.volangBuildInputs?.digest,
    );
    if (manifest.ffiSourceFingerprint !== fingerprint) {
      issues.push('ffiSourceFingerprint does not match the manifest input digests');
    }
  } catch (error) {
    issues.push(`ffiSourceFingerprint cannot be derived: ${commandErrorDetail(error)}`);
  }
  const outputs = VOGUI_WASM_REQUIRED_OUTPUTS.map((name) => outputEntry(outDir, name));
  if (JSON.stringify(manifest.outputs) !== JSON.stringify(outputs)) {
    issues.push('output digest set does not match the frozen vogui artifact');
  }
  if (outputs.some((output) => output.missing || output.invalid)) {
    issues.push('required vogui WASM output is missing, oversized, or non-regular');
  } else {
    try {
      const catalog = extensionExportCatalogFromDirectory(voguiRoot, {
        modulePath: VOGUI_MODULE,
        extensionName: 'vogui',
        rustEntrySource: 'rust/ext/src/externs.rs',
      });
      assertStandaloneWasmExtensionV3(
        readFileSync(path.join(outDir, 'vogui.wasm')),
        {
          expectedExportKeys: catalog.exportKeys,
          expectedImports: VOGUI_STANDALONE_HOST_IMPORTS_V3,
          label: 'current-source vogui standalone extension',
          maxBytes: VOGUI_OUTPUT_MAX_BYTES,
        },
      );
    } catch (error) {
      issues.push(`vogui browser protocol-v3 contract failed: ${commandErrorDetail(error)}`);
    }
  }
  return {
    issues,
    manifest,
    outputs,
    source: currentSource,
    volangBuildInputs: manifest.volangBuildInputs ?? null,
  };
}

function completeProducerDirectory(directory) {
  try {
    return lstatSync(path.join(directory, 'producer-manifest.json')).isFile()
      && VOGUI_WASM_REQUIRED_OUTPUTS.every(
        (name) => lstatSync(path.join(directory, name)).isFile(),
      );
  } catch {
    return false;
  }
}

export function recoverInterruptedVoguiDirectoryReplacement(outDir) {
  return recoverInterruptedDirectoryReplacement(outDir, {
    completeDirectory: completeProducerDirectory,
    label: 'vogui current-source WASM',
  });
}

function freezeCargoOutput(buildDir, stagingDir) {
  const generated = path.join(
    buildDir,
    'wasm32-unknown-unknown',
    'release',
    VOGUI_CARGO_OUTPUT,
  );
  const bytes = readBoundedFile(
    generated,
    `cargo output ${VOGUI_CARGO_OUTPUT}`,
    VOGUI_OUTPUT_MAX_BYTES,
  );
  writeFileSync(path.join(stagingDir, VOGUI_WASM_REQUIRED_OUTPUTS[0]), bytes, { flag: 'wx' });
}

function toolVersion(command, args) {
  return execFileSync(command, args, { encoding: 'utf8' }).trim();
}

function buildCurrentVoguiWasm() {
  const voguiRoot = requireRepoRoot('VOGUI_ROOT', 'vogui');
  const outDir = path.resolve(process.env.VOGUI_CURRENT_WASM_OUT_DIR ?? defaultOutDir);
  return withDirectoryReplacementLock(outDir, () => buildCurrentVoguiWasmLocked(voguiRoot, outDir));
}

function buildCurrentVoguiWasmLocked(voguiRoot, outDir) {
  recoverInterruptedVoguiDirectoryReplacement(outDir);
  if (existsSync(outDir) && completeProducerDirectory(outDir)) {
    cleanupDirectoryReplacementBackups(outDir, {
      completeDirectory: completeProducerDirectory,
      label: 'vogui current-source WASM',
    });
  }
  const preflight = lockedVoguiBuildInputs(voguiRoot);
  mkdirSync(path.dirname(outDir), { recursive: true });
  const stagingDir = mkdtempSync(
    path.join(path.dirname(outDir), `.${path.basename(outDir)}.staging-`),
  );
  const buildDir = mkdtempSync(
    path.join(path.dirname(outDir), `.${path.basename(outDir)}.cargo-`),
  );
  try {
    execFileSync('cargo', [
      'build',
      '--release',
      '--target',
      'wasm32-unknown-unknown',
      '--target-dir',
      buildDir,
      '--manifest-path',
      path.join(voguiRoot, 'rust', 'ext', 'Cargo.toml'),
      '--no-default-features',
      '--features',
      'wasm-standalone',
      '--locked',
    ], {
      cwd: root,
      env: voguiWasmBuildEnvironment(preflight, voguiRoot, cleanGitEnvironment()),
      stdio: 'inherit',
    });
    freezeCargoOutput(buildDir, stagingDir);
    const postflight = lockedVoguiBuildInputs(voguiRoot);
    if (JSON.stringify(postflight) !== JSON.stringify(preflight)) {
      throw new Error('vogui or scoped Volang build inputs changed while cargo was running');
    }
    const manifest = {
      schemaVersion: VOGUI_WASM_PRODUCER_SCHEMA_VERSION,
      kind: 'vogui.currentSourceWasm',
      generatedAt: new Date().toISOString(),
      ciRunId: process.env.VO_DEV_CI_RUN_ID ?? null,
      command: [...VOGUI_WASM_PRODUCER_COMMAND],
      source: preflight.source,
      volangBuildInputs: preflight.volangBuildInputs,
      ffiSourceFingerprint: preflight.ffiSourceFingerprint,
      toolchain: {
        cargo: toolVersion('cargo', ['--version']),
        rustc: toolVersion('rustc', ['--version']),
      },
      buildPlatform: currentVoguiWasmBuildPlatform(),
      outputs: VOGUI_WASM_REQUIRED_OUTPUTS.map((name) => outputEntry(stagingDir, name)),
    };
    writeFileSync(
      path.join(stagingDir, 'producer-manifest.json'),
      `${JSON.stringify(manifest, null, 2)}\n`,
      { flag: 'wx' },
    );
    const verification = verifyCurrentVoguiWasm({
      voguiRoot,
      outDir: stagingDir,
      expectedCiRunId: process.env.VO_DEV_CI_RUN_ID ?? null,
      expectedSource: postflight.source,
      expectedVolangBuildInputs: postflight.volangBuildInputs,
    });
    if (verification.issues.length > 0) {
      throw new Error(`current-source vogui WASM verification failed: ${verification.issues.join('; ')}`);
    }
    publishStagedDirectoryWithRollback(stagingDir, outDir);
    cleanupDirectoryReplacementBackups(outDir, {
      completeDirectory: completeProducerDirectory,
      label: 'vogui current-source WASM',
    });
  } finally {
    rmSync(stagingDir, { recursive: true, force: true });
    rmSync(buildDir, { recursive: true, force: true });
  }
  console.log(`vogui current-source WASM: ok (${path.relative(root, outDir)})`);
}

if (import.meta.url === pathToFileURL(process.argv[1] ?? '').href) {
  try {
    buildCurrentVoguiWasm();
  } catch (error) {
    console.error(`vogui current-source WASM: ${error instanceof Error ? error.message : String(error)}`);
    process.exitCode = 1;
  }
}
