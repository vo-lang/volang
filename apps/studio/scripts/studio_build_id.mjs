import { Buffer } from 'node:buffer';
import { createHash } from 'node:crypto';
import {
  closeSync,
  constants as fsConstants,
  existsSync,
  fstatSync,
  openSync,
  readSync,
} from 'node:fs';
import { dirname, relative, resolve, sep } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { validatePortableRelativePath } from '../../../scripts/ci/quickplay_artifact_paths.mjs';
import { portablePathCollisionKey } from '../../../scripts/ci/portable_path_key.mjs';
import { parseBoundedStrictJsonBytes } from '../../../scripts/ci/quickplay_web_manifest_contract.mjs';

const scriptDir = dirname(fileURLToPath(import.meta.url));
export const STUDIO_ROOT = resolve(scriptDir, '..');

export const QUICKPLAY_PACKAGE_ROOT = 'public/quickplay/blockkart';
export const QUICKPLAY_PACKAGE_BASE_FILES = [
  'public/quickplay/blockkart/project.json',
  'public/quickplay/blockkart/deps.json',
];
const QUICKPLAY_PACKAGE_JSON_MAX_BYTES = 128 * 1024 * 1024;
const QUICKPLAY_PACKAGE_MAX_MODULES = 10_000;
const QUICKPLAY_PACKAGE_MAX_FILES = 20_000;
const QUICKPLAY_PACKAGE_MAX_FILE_BYTES = 256 * 1024 * 1024;
const QUICKPLAY_PACKAGE_MAX_TOTAL_BYTES = 512 * 1024 * 1024;
const STUDIO_BUILD_ID_PATTERN = /^[A-Za-z0-9._-]{1,256}$/;

function validateStudioBuildId(value, label) {
  if (!STUDIO_BUILD_ID_PATTERN.test(value)) {
    throw new Error(`${label} must contain 1 to 256 ASCII letters, digits, '.', '_' or '-'`);
  }
  return value;
}

function compareUtf8(left, right) {
  return Buffer.compare(Buffer.from(left, 'utf8'), Buffer.from(right, 'utf8'));
}

function isPathWithin(candidate, root) {
  const rel = relative(root, candidate);
  return rel === '' || (!rel.startsWith(`..${sep}`) && rel !== '..' && !rel.startsWith(sep));
}

function readStableFileBytesLimited(file, maxBytes, label) {
  const noFollow = typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0;
  const descriptor = openSync(file, fsConstants.O_RDONLY | noFollow);
  try {
    const before = fstatSync(descriptor);
    if (
      !before.isFile()
      || !Number.isSafeInteger(before.size)
      || before.size < 0
      || before.size > maxBytes
    ) {
      throw new Error(`${label} exceeds the ${maxBytes}-byte limit`);
    }
    const chunks = [];
    let size = 0;
    const chunk = Buffer.allocUnsafe(64 * 1024);
    while (true) {
      const count = readSync(descriptor, chunk, 0, chunk.byteLength, null);
      if (count === 0) break;
      size += count;
      if (!Number.isSafeInteger(size) || size > maxBytes) {
        throw new Error(`${label} exceeds the ${maxBytes}-byte limit`);
      }
      chunks.push(Buffer.from(chunk.subarray(0, count)));
    }
    const after = fstatSync(descriptor);
    if (
      before.dev !== after.dev
      || before.ino !== after.ino
      || before.size !== after.size
      || before.mtimeMs !== after.mtimeMs
      || before.ctimeMs !== after.ctimeMs
      || size !== before.size
    ) {
      throw new Error(`${label} changed while Studio was reading it`);
    }
    return Buffer.concat(chunks, size);
  } finally {
    closeSync(descriptor);
  }
}

function quickplayArtifactPackagePath(url, studioRoot) {
  const prefix = '/quickplay/blockkart/';
  if (!url.startsWith(prefix) || url.includes('?') || url.includes('#') || url.includes('\\')) {
    throw new Error(`invalid quickplay artifact URL: ${url}`);
  }
  let parts;
  try {
    parts = url.slice(prefix.length).split('/').map((part) => decodeURIComponent(part));
  } catch {
    throw new Error(`invalid quickplay artifact URL encoding: ${url}`);
  }
  if (parts.some((part) => (
    !part
    || part === '.'
    || part === '..'
    || part.includes('/')
    || part.includes('\\')
    || /[\u0000-\u001f\u007f]/u.test(part)
  ))) {
    throw new Error(`invalid quickplay artifact URL path: ${url}`);
  }
  const canonicalUrl = prefix + parts.map((part) => (
    encodeURIComponent(part).replace(/[!'()*]/g, (character) => (
      `%${character.charCodeAt(0).toString(16).toUpperCase()}`
    ))
  )).join('/');
  if (url !== canonicalUrl) {
    throw new Error(`non-canonical quickplay artifact URL: ${url}`);
  }
  const relativePath = parts.join('/');
  const components = validatePortableRelativePath(relativePath, 'quickplay artifact URL path');
  if (components.length > 256) {
    throw new Error(`quickplay artifact URL path exceeds the 256-component limit: ${url}`);
  }
  const packageRoot = resolve(studioRoot, QUICKPLAY_PACKAGE_ROOT);
  const absolute = resolve(packageRoot, ...parts);
  if (!isPathWithin(absolute, packageRoot)) {
    throw new Error(`quickplay artifact escapes its package root: ${url}`);
  }
  return `${QUICKPLAY_PACKAGE_ROOT}/${relativePath}`;
}

export function quickplayPackageFiles({ studioRoot = STUDIO_ROOT } = {}) {
  const depsPath = resolve(studioRoot, QUICKPLAY_PACKAGE_ROOT, 'deps.json');
  if (!existsSync(depsPath)) {
    return QUICKPLAY_PACKAGE_BASE_FILES;
  }

  const deps = parseBoundedStrictJsonBytes(
    readStableFileBytesLimited(
      depsPath,
      QUICKPLAY_PACKAGE_JSON_MAX_BYTES,
      'quickplay deps package',
    ),
    'quickplay deps package',
    { maxBytes: QUICKPLAY_PACKAGE_JSON_MAX_BYTES },
  );
  if (!deps || typeof deps !== 'object' || Array.isArray(deps)) {
    throw new Error('quickplay deps package must contain a JSON object');
  }
  if (deps.schemaVersion !== 2) {
    throw new Error(
      'quickplay deps package has an unsupported schema version; '
      + 'run vo-dev task run task:quickplay-blockkart-package',
    );
  }
  if (!Array.isArray(deps.modules) || deps.modules.length > QUICKPLAY_PACKAGE_MAX_MODULES) {
    throw new Error(`quickplay deps package exceeds the ${QUICKPLAY_PACKAGE_MAX_MODULES}-module limit`);
  }
  const files = new Set(QUICKPLAY_PACKAGE_BASE_FILES);
  const collisionKeys = new Map(QUICKPLAY_PACKAGE_BASE_FILES.map((file) => [
    portablePathCollisionKey(file),
    file,
  ]));
  let artifactsSeen = 0;
  for (const [moduleIndex, modulePack] of deps.modules.entries()) {
    if (!modulePack || typeof modulePack !== 'object' || Array.isArray(modulePack)) {
      throw new Error(`quickplay deps module ${moduleIndex} is invalid`);
    }
    const artifacts = modulePack.artifacts ?? [];
    if (!Array.isArray(artifacts)) {
      throw new Error(`quickplay deps module ${moduleIndex} artifacts must be an array`);
    }
    for (const artifact of artifacts) {
      artifactsSeen += 1;
      if (artifactsSeen > QUICKPLAY_PACKAGE_MAX_FILES - QUICKPLAY_PACKAGE_BASE_FILES.length) {
        throw new Error(`quickplay deps package exceeds the ${QUICKPLAY_PACKAGE_MAX_FILES}-file limit`);
      }
      if (!artifact || typeof artifact !== 'object' || Array.isArray(artifact) || typeof artifact.url !== 'string') {
        throw new Error(`quickplay deps module ${moduleIndex} contains an invalid artifact`);
      }
      const file = quickplayArtifactPackagePath(artifact.url, studioRoot);
      const collisionKey = portablePathCollisionKey(file);
      if (collisionKeys.has(collisionKey)) {
        throw new Error(`quickplay deps package declares a duplicate artifact path: ${collisionKeys.get(collisionKey)} and ${file}`);
      }
      collisionKeys.set(collisionKey, file);
      files.add(file);
    }
  }
  for (const [collisionKey, file] of collisionKeys) {
    const parts = collisionKey.split('/');
    for (let length = 1; length < parts.length; length += 1) {
      const ancestor = parts.slice(0, length).join('/');
      if (collisionKeys.has(ancestor)) {
        throw new Error(`quickplay package has a file/directory ancestor collision: ${collisionKeys.get(ancestor)} and ${file}`);
      }
    }
  }
  return [...files].sort(compareUtf8);
}

function updateQuickplayBuildHash(
  hash,
  absolute,
  label,
  budget,
  maxBytes = QUICKPLAY_PACKAGE_MAX_FILE_BYTES,
) {
  const noFollow = typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0;
  const descriptor = openSync(absolute, fsConstants.O_RDONLY | noFollow);
  try {
    const before = fstatSync(descriptor);
    if (
      !before.isFile()
      || !Number.isSafeInteger(before.size)
      || before.size < 0
      || before.size > maxBytes
    ) {
      throw new Error(`${label} exceeds the ${maxBytes}-byte file limit`);
    }
    const nextFiles = budget.files + 1;
    const nextBytes = budget.bytes + before.size;
    if (
      !Number.isSafeInteger(nextBytes)
      || nextFiles > QUICKPLAY_PACKAGE_MAX_FILES
      || nextBytes > QUICKPLAY_PACKAGE_MAX_TOTAL_BYTES
    ) {
      throw new Error(`quickplay package exceeds the ${QUICKPLAY_PACKAGE_MAX_TOTAL_BYTES}-byte aggregate limit`);
    }
    const chunk = Buffer.allocUnsafe(64 * 1024);
    let size = 0;
    while (size < before.size) {
      const count = readSync(
        descriptor,
        chunk,
        0,
        Math.min(chunk.byteLength, before.size - size),
        null,
      );
      if (count <= 0) throw new Error(`${label} changed while Studio was hashing it`);
      size += count;
      hash.update(chunk.subarray(0, count));
    }
    const after = fstatSync(descriptor);
    if (
      before.dev !== after.dev
      || before.ino !== after.ino
      || before.size !== after.size
      || before.mtimeMs !== after.mtimeMs
      || before.ctimeMs !== after.ctimeMs
      || size !== before.size
    ) {
      throw new Error(`${label} changed while Studio was hashing it`);
    }
    budget.files = nextFiles;
    budget.bytes = nextBytes;
  } finally {
    closeSync(descriptor);
  }
}

export function readQuickplayPackageBuildId({ studioRoot = STUDIO_ROOT } = {}) {
  const present = QUICKPLAY_PACKAGE_BASE_FILES.filter((file) => existsSync(resolve(studioRoot, file)));
  if (present.length === 0) return null;
  if (present.length !== QUICKPLAY_PACKAGE_BASE_FILES.length) {
    const missing = QUICKPLAY_PACKAGE_BASE_FILES.filter((file) => !present.includes(file));
    throw new Error(`quickplay package is incomplete; missing: ${missing.join(', ')}`);
  }
  const hash = createHash('sha256');
  const budget = { files: 0, bytes: 0 };
  for (const file of quickplayPackageFiles({ studioRoot })) {
    const absolute = resolve(studioRoot, file);
    if (!existsSync(absolute)) {
      throw new Error(`quickplay package is missing declared file: ${file}`);
    }
    hash.update(file);
    hash.update('\0');
    updateQuickplayBuildHash(
      hash,
      absolute,
      `quickplay package file ${file}`,
      budget,
      file.endsWith('.json') ? QUICKPLAY_PACKAGE_JSON_MAX_BYTES : QUICKPLAY_PACKAGE_MAX_FILE_BYTES,
    );
    hash.update('\0');
  }
  return `qp-${hash.digest('hex').slice(0, 12)}`;
}

export function resolveStudioBuildId(env = process.env, { studioRoot = STUDIO_ROOT } = {}) {
  const explicit = (env.VO_STUDIO_BUILD_ID ?? '').trim();
  if (explicit.length > 0) {
    return validateStudioBuildId(explicit, 'VO_STUDIO_BUILD_ID');
  }

  const base = [env.GITHUB_SHA, env.GITHUB_RUN_ID, env.GITHUB_RUN_ATTEMPT]
    .map((value) => (value ?? '').trim())
    .filter((value) => value.length > 0)
    .join('-') || `local-${Date.now().toString(16)}`;

  const quickplayBuildId = readQuickplayPackageBuildId({ studioRoot });
  return validateStudioBuildId(
    [base, quickplayBuildId].filter((value) => typeof value === 'string' && value.length > 0).join('-'),
    'derived Studio build ID',
  );
}

export function validateStudioWasmBuildId(
  value,
  env = process.env,
  { studioRoot = STUDIO_ROOT } = {},
) {
  const buildId = validateStudioBuildId(value.trim(), 'Studio WASM build ID');
  const quickplayBuildId = readQuickplayPackageBuildId({ studioRoot });
  const explicit = (env.VO_STUDIO_BUILD_ID ?? '').trim();
  if (explicit.length > 0) {
    const expected = validateStudioBuildId(explicit, 'VO_STUDIO_BUILD_ID');
    if (buildId !== expected) {
      throw new Error(`Studio WASM build ID ${buildId} does not match VO_STUDIO_BUILD_ID ${expected}`);
    }
    return buildId;
  }

  const githubBase = [env.GITHUB_SHA, env.GITHUB_RUN_ID, env.GITHUB_RUN_ATTEMPT]
    .map((part) => (part ?? '').trim())
    .filter((part) => part.length > 0)
    .join('-');
  if (githubBase.length > 0) {
    const expected = validateStudioBuildId(
      [githubBase, quickplayBuildId].filter(Boolean).join('-'),
      'derived GitHub Studio build ID',
    );
    if (buildId !== expected) {
      throw new Error(`Studio WASM build ID ${buildId} does not match current GitHub inputs ${expected}`);
    }
    return buildId;
  }

  if (quickplayBuildId && !buildId.endsWith(`-${quickplayBuildId}`)) {
    throw new Error(
      `Studio WASM build ID ${buildId} does not bind the current Quickplay package ${quickplayBuildId}; run npm run build:wasm`,
    );
  }
  return buildId;
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  console.log(resolveStudioBuildId(process.env));
}
