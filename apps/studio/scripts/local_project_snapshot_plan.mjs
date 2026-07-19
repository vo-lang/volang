import { spawnSync } from 'node:child_process';
import { lstatSync, realpathSync } from 'node:fs';
import path from 'node:path';

const DEFAULT_MAX_OUTPUT_BYTES = 16 * 1024 * 1024;
const DEFAULT_MAX_ROOTS = 10_000;
const DEFAULT_TIMEOUT_MS = 60_000;
const MAX_VO_BINARY_BYTES = 256 * 1024 * 1024;

function compareUtf8(left, right) {
  return Buffer.compare(Buffer.from(left, 'utf8'), Buffer.from(right, 'utf8'));
}

function checkedPositiveInteger(value, fallback, label) {
  const candidate = value ?? fallback;
  if (!Number.isSafeInteger(candidate) || candidate <= 0) {
    throw new Error(`${label} must be a positive safe integer`);
  }
  return candidate;
}

function canonicalAbsolutePath(value, label) {
  if (
    typeof value !== 'string'
    || value.length === 0
    || /[\u0000-\u001f\u007f]/u.test(value)
    || !path.isAbsolute(value)
    || path.resolve(value) !== value
  ) {
    throw new Error(`${label} must be a normalized absolute path`);
  }
  return value;
}

function canonicalExistingPath(value, label, expectedKind) {
  const requested = canonicalAbsolutePath(value, label);
  let metadata;
  let real;
  try {
    metadata = lstatSync(requested);
    real = realpathSync.native(requested);
  } catch (error) {
    const detail = error instanceof Error ? error.message : String(error);
    throw new Error(`${label} cannot be inspected: ${detail}`);
  }
  if (metadata.isSymbolicLink()) {
    throw new Error(`${label} must not be a symbolic link`);
  }
  if (expectedKind === 'directory' ? !metadata.isDirectory() : !metadata.isFile()) {
    throw new Error(`${label} must be a real ${expectedKind}`);
  }
  if (real !== requested) {
    throw new Error(`${label} must not traverse a filesystem alias`);
  }
  return { path: real, metadata };
}

function optionalCanonicalRegularFile(file, label) {
  try {
    lstatSync(file);
  } catch (error) {
    if (error && typeof error === 'object' && error.code === 'ENOENT') return null;
    const detail = error instanceof Error ? error.message : String(error);
    throw new Error(`${label} cannot be inspected: ${detail}`);
  }
  return canonicalExistingPath(file, label, 'file').path;
}

function canonicalVoBinary(value) {
  if (typeof value !== 'string' || value.trim() !== value || value.length === 0) {
    throw new Error(
      'Studio local module snapshots require VO_BIN to name the current Vo CLI with an absolute path',
    );
  }
  const binary = canonicalExistingPath(value, 'VO_BIN', 'file');
  if (
    binary.metadata.nlink !== 1
    || !Number.isSafeInteger(binary.metadata.size)
    || binary.metadata.size <= 0
    || binary.metadata.size > MAX_VO_BINARY_BYTES
  ) {
    throw new Error(
      `VO_BIN must be a singly-linked 1..${MAX_VO_BINARY_BYTES}-byte regular file`,
    );
  }
  if (process.platform !== 'win32' && (binary.metadata.mode & 0o111) === 0) {
    throw new Error('VO_BIN must be executable');
  }
  return binary;
}

function sameFileGeneration(left, right) {
  return left.dev === right.dev
    && left.ino === right.ino
    && left.mode === right.mode
    && left.nlink === right.nlink
    && left.size === right.size
    && left.mtimeMs === right.mtimeMs
    && left.ctimeMs === right.ctimeMs;
}

function commandOutputBytes(value, label, maxBytes) {
  const bytes = typeof value === 'string'
    ? Buffer.from(value, 'utf8')
    : Buffer.from(value ?? []);
  if (bytes.byteLength > maxBytes) {
    throw new Error(`${label} exceeds the ${maxBytes}-byte limit`);
  }
  return bytes;
}

function commandErrorText(value, maxBytes) {
  const bytes = commandOutputBytes(value, 'vo mod snapshot stderr', maxBytes);
  try {
    return new TextDecoder('utf-8', { fatal: true }).decode(bytes).trim();
  } catch {
    return '<non-UTF-8 stderr>';
  }
}

function validateLocalProjectSnapshot(value) {
  if (
    !value
    || typeof value !== 'object'
    || Array.isArray(value)
    || !value.root
    || typeof value.root !== 'object'
    || Array.isArray(value.root)
    || typeof value.root.module !== 'string'
  ) {
    throw new Error('vo mod snapshot output has no root module identity');
  }
  if (!Array.isArray(value.modules)) {
    throw new Error('vo mod snapshot output has no module list');
  }
  if (
    value.workspace !== undefined
    && (
      !value.workspace
      || typeof value.workspace !== 'object'
      || Array.isArray(value.workspace)
      || typeof value.workspace.file !== 'string'
    )
  ) {
    throw new Error('vo mod snapshot output has an invalid workspace descriptor');
  }
  return value;
}

function captureEffectiveProjectSnapshot(projectRoot, options) {
  const maxOutputBytes = checkedPositiveInteger(
    options.maxOutputBytes,
    DEFAULT_MAX_OUTPUT_BYTES,
    'local project snapshot output limit',
  );
  const timeoutMs = checkedPositiveInteger(
    options.timeoutMs,
    DEFAULT_TIMEOUT_MS,
    'local project snapshot timeout',
  );
  const voBinary = canonicalVoBinary(options.voBin);
  const environment = { ...(options.environment ?? process.env) };
  const invoke = options.invoke ?? spawnSync;
  const args = ['mod', 'snapshot', projectRoot];
  const result = invoke(voBinary.path, args, {
    cwd: projectRoot,
    env: environment,
    encoding: null,
    maxBuffer: maxOutputBytes,
    timeout: timeoutMs,
    windowsHide: true,
  });
  if (!result || typeof result !== 'object') {
    throw new Error('vo mod snapshot returned no process result');
  }
  const finalVoBinary = canonicalVoBinary(voBinary.path);
  if (!sameFileGeneration(voBinary.metadata, finalVoBinary.metadata)) {
    throw new Error('VO_BIN changed while Studio captured the project snapshot');
  }
  if (result.error || result.status !== 0) {
    const detail = result.error instanceof Error
      ? result.error.message
      : commandErrorText(result.stderr, maxOutputBytes)
        || (result.signal ? `terminated by ${result.signal}` : `exit status ${String(result.status)}`);
    throw new Error(`vo mod snapshot failed: ${detail}`);
  }
  const stdout = commandOutputBytes(result.stdout, 'vo mod snapshot stdout', maxOutputBytes);
  let parsed;
  try {
    parsed = JSON.parse(new TextDecoder('utf-8', { fatal: true }).decode(stdout));
  } catch {
    throw new Error('vo mod snapshot output is invalid UTF-8 JSON');
  }
  const snapshot = validateLocalProjectSnapshot(parsed);
  const canonical = Buffer.from(`${JSON.stringify(snapshot, null, 2)}\n`, 'utf8');
  if (!stdout.equals(canonical)) {
    throw new Error('vo mod snapshot output must use canonical ProjectSnapshot v2 JSON encoding');
  }
  return snapshot;
}

function findNearestWorkspaceFile(start) {
  let current = start;
  while (true) {
    const candidate = path.join(current, 'vo.work');
    const selected = optionalCanonicalRegularFile(candidate, `workspace candidate ${candidate}`);
    if (selected !== null) return selected;
    const parent = path.dirname(current);
    if (parent === current) return null;
    current = parent;
  }
}

/**
 * Resolve the exact local source closure exported by the core ProjectSnapshot
 * protocol. Studio intentionally has no second vo.work parser: the Vo CLI owns
 * TOML syntax, member path rules, duplicate detection, and stable no-follow
 * workspace discovery.
 */
export function planLocalProjectSources(projectRoot, options = {}) {
  const root = canonicalExistingPath(projectRoot, 'local project root', 'directory').path;
  const maxRoots = checkedPositiveInteger(
    options.maxRoots,
    DEFAULT_MAX_ROOTS,
    'local project root limit',
  );
  const rootManifest = optionalCanonicalRegularFile(
    path.join(root, 'vo.mod'),
    'local project vo.mod',
  );
  if (rootManifest === null) {
    const workspace = findNearestWorkspaceFile(root);
    if (workspace !== null) {
      throw new Error(`local standalone project cannot select ${workspace} without a vo.mod`);
    }
    return { roots: [root], files: [] };
  }

  const snapshot = captureEffectiveProjectSnapshot(root, options);
  const roots = new Set([root]);
  const namespaceRoot = path.parse(root).root;
  for (const module of snapshot.modules) {
    if (module.source.kind !== 'workspace') continue;
    const directory = canonicalExistingPath(
      module.source.directory,
      `${module.module} workspace source`,
      'directory',
    ).path;
    if (path.parse(directory).root !== namespaceRoot) {
      throw new Error(`${module.module} workspace source crosses the project filesystem namespace`);
    }
    canonicalExistingPath(path.join(directory, 'vo.mod'), `${module.module} vo.mod`, 'file');
    if (roots.has(directory)) {
      throw new Error(`${module.module} repeats a local project source directory`);
    }
    roots.add(directory);
    if (roots.size > maxRoots) {
      throw new Error(`local project workspace exceeds the ${maxRoots}-root limit`);
    }
  }

  const files = [];
  if (snapshot.workspace !== undefined) {
    const workspace = canonicalExistingPath(
      snapshot.workspace.file,
      'ProjectSnapshot workspace file',
      'file',
    ).path;
    if (path.parse(workspace).root !== namespaceRoot) {
      throw new Error('ProjectSnapshot workspace file crosses the project filesystem namespace');
    }
    files.push(workspace);
  }
  return {
    roots: [...roots].sort(compareUtf8),
    files: files.sort(compareUtf8),
  };
}
