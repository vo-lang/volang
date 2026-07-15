import { execFileSync } from 'node:child_process';
import { createHash, randomUUID } from 'node:crypto';
import { constants as fsConstants, promises as fs } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  artifactCachePath,
  artifactKey,
  artifactOutputRelativePath,
  moduleCacheDir,
  quickplayArtifactRelativePathFromUrl,
  quickplayArtifactUrl,
  validatePortableComponent,
  validatePortableRelativePath,
} from '../../../scripts/ci/quickplay_artifact_paths.mjs';
import {
  assertVoCliBuildInputs,
  currentVoCliToolchain,
  currentVoCliBuildInputs,
  VO_CLI_GUEST_ENVIRONMENT,
  verifyVoCliExecutionIdentity,
} from '../../../scripts/ci/quickplay_cli_producer_contract.mjs';
import {
  QUICKPLAY_ARTIFACT_NAME,
  QUICKPLAY_ARTIFACT_PATH,
  QUICKPLAY_GENERATOR_COMMAND,
  QUICKPLAY_GENERATOR_INPUTS,
  QUICKPLAY_GENERATOR_SOURCE_INPUTS,
  QUICKPLAY_GENERATOR_VERSION,
  QUICKPLAY_SOURCE_ROOTS,
  QUICKPLAY_TASK_ID,
  quickplayBlockKartSourceAllowlist,
  quickplayGeneratorSourceDigest,
} from '../../../scripts/ci/quickplay_generator_contract.mjs';
import { requireRepoRoot, requireVolangRoot } from '../../../scripts/ci/repo_roots.mjs';
import { portablePathCollisionKey } from '../../../scripts/ci/portable_path_key.mjs';
import {
  parseBoundedStrictJsonBytes,
  validateWebManifestVoModContract,
} from '../../../scripts/ci/quickplay_web_manifest_contract.mjs';
import { compareUtf8 } from '../../../scripts/ci/utf8_order.mjs';
import {
  decodeModuleTextUtf8,
  parseVoLockV2,
  parseVoLockForV2Migration,
  parseVoModRootContract,
  renderVoLockV2,
  validatePackagedModuleSet,
  validateVoLockV2RootGraph,
} from '../../../scripts/ci/vo_lock_v2.mjs';
import { verifyCurrentVoplayWasm } from '../../../scripts/ci/voplay_current_wasm.mjs';

const STUDIO_ROOT = path.resolve(fileURLToPath(new URL('..', import.meta.url)));
const VOLANG_ROOT = requireVolangRoot(path.resolve(STUDIO_ROOT, '../..'));
const BLOCKKART_ROOT = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const MOD_CACHE_ROOT = path.resolve(
  process.env.VO_MOD_CACHE ?? path.join(VOLANG_ROOT, 'target', 'quickplay-module-cache', 'mod'),
);
const VOPLAY_ROOT = requireRepoRoot('VOPLAY_ROOT', 'voplay');
const VOPLAY_CURRENT_WASM_ROOT = path.resolve(
  process.env.VOPLAY_CURRENT_WASM_OUT_DIR ?? path.join(VOLANG_ROOT, 'target', 'voplay-current-wasm'),
);
const BLOCKKART_VPAK_BUILD_REPORT = path.join(
  VOLANG_ROOT,
  'target',
  'blockkart-vpak-build',
  'report.json',
);
const OUT_ROOT = path.resolve(process.env.BLOCKKART_QUICKPLAY_OUT_ROOT ?? path.join(STUDIO_ROOT, 'public', 'quickplay', 'blockkart'));
let activeOutRoot = OUT_ROOT;
const WASM_TARGET = 'wasm32-unknown-unknown';
const BLOCKKART_RUNTIME_ASSETS = [
  'assets/blockkart.vpak',
  'assets/blockkart.vpak.provenance.json',
];
const QUICKPLAY_MAX_WALK_ENTRIES = 100_000;
const QUICKPLAY_MAX_FILES = 20_000;
const QUICKPLAY_MAX_FILE_BYTES = 256 * 1024 * 1024;
const QUICKPLAY_MAX_TOTAL_BYTES = 512 * 1024 * 1024;
const QUICKPLAY_MAX_SOURCE_PAYLOAD_BYTES = 64 * 1024 * 1024;
const QUICKPLAY_MAX_JSON_BYTES = 128 * 1024 * 1024;
const QUICKPLAY_MAX_DEPTH = 256;
const QUICKPLAY_MAX_METADATA_BYTES = 16 * 1024 * 1024;
const QUICKPLAY_MAX_PATH_BYTES = 4 * 1024;
const QUICKPLAY_PROJECT_VFS_ROOT = '/home/vo/.studio/sessions/quickplay/BlockKart/current';
const BLOCKKART_RUNTIME_EXTRA_ASSETS = [
  'assets/effects/grass_card_atlas.png',
  'assets/skybox/right.png',
  'assets/skybox/left.png',
  'assets/skybox/top.png',
  'assets/skybox/bottom.png',
  'assets/skybox/front.png',
  'assets/skybox/back.png',
  'assets/audio/kart_engine.wav',
  'assets/audio/kart_skid.wav',
  'assets/audio/kart_boost.wav',
  'assets/audio/kart_grass.wav',
  'assets/audio/kart_hit.wav',
];
const BLOCKKART_TERRAIN_PAINT_INPUTS = [
  'tools/paint_terrain_textures.mjs',
  'docs/images/terrain-upgrade-concept-v1.png',
];
const BLOCKKART_TERRAIN_PAINT_OUTPUTS = [
  'assets/source/terrain_painted/grass_painted_v1.png',
  'assets/source/terrain_painted/meadow_painted_v1.png',
  'assets/source/terrain_painted/dirt_painted_v1.png',
  'assets/source/terrain_painted/rock_painted_v1.png',
  'assets/effects/grass_card_atlas.png',
];
const BLOCKKART_TERRAIN_GENERATOR_INPUTS = [
  'tools/generate_primitive_terrain.mjs',
  'tools/terrain_heightfield_spec.mjs',
  'tools/terrain_recipe.mjs',
  'terrain/recipes/primitive_concept_v1.json',
  ...BLOCKKART_TERRAIN_PAINT_OUTPUTS,
];
const BLOCKKART_TERRAIN_REQUIRED_OUTPUTS = [
  'assets/maps/primitive_track/lowpoly_terrain.glb',
  'assets/maps/primitive_track/lowpoly_terrain_lod.glb',
  'assets/maps/primitive_track/lowpoly_terrain_height_grid.bin',
  'assets/maps/primitive_track/terrain_splat_large.png',
];

function gitOutput(args, cwd) {
  try {
    return execFileSync('git', args, {
      cwd,
      encoding: 'utf8',
      env: { ...process.env, GIT_OPTIONAL_LOCKS: '0' },
      maxBuffer: QUICKPLAY_MAX_METADATA_BYTES,
      timeout: 30_000,
      stdio: ['ignore', 'pipe', 'pipe'],
    }).trim();
  } catch (error) {
    const stderr = error?.stderr ? String(error.stderr).trim() : '';
    throw new Error(`git ${args.join(' ')} failed in ${cwd}: ${stderr || error.message}`);
  }
}

function gitStatus(cwd) {
  return gitOutput(['status', '--porcelain=v2', '--untracked-files=all'], cwd);
}

function sha256Digest(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

async function volangGeneratorSourceDigest() {
  const entries = [];
  for (const relative of QUICKPLAY_GENERATOR_SOURCE_INPUTS) {
    const file = await digestRegularFileLimited(
      path.join(VOLANG_ROOT, relative),
      `generator input ${relative}`,
      QUICKPLAY_MAX_METADATA_BYTES,
    );
    entries.push({
      digest: file.digest,
      path: relative,
      size: file.size,
    });
  }
  return quickplayGeneratorSourceDigest(entries);
}

function jsonText(value) {
  return `${JSON.stringify(value, null, 2)}\n`;
}

function packagedFileBytes(file) {
  if (file.content != null) return Buffer.from(file.content, 'utf8');
  if (file.contentBase64 != null) return Buffer.from(file.contentBase64, 'base64');
  throw new Error(`Packaged file is missing content: ${file.path}`);
}

function packagedFilesDigest(files) {
  const entries = files
    .map((file) => {
      const bytes = packagedFileBytes(file);
      return {
        digest: sha256Digest(bytes),
        path: file.path,
        size: bytes.byteLength,
      };
    })
    .sort((a, b) => compareUtf8(a.path, b.path));
  return sourceSetDigest(entries);
}

async function pathExists(filePath) {
  try {
    await fs.access(filePath);
    return true;
  } catch {
    return false;
  }
}

function pathContains(root, candidate) {
  const relative = path.relative(root, candidate);
  return relative === ''
    || (!relative.startsWith(`..${path.sep}`) && relative !== '..' && !path.isAbsolute(relative));
}

async function readRegularFileLimited(filePath, label, maxBytes = QUICKPLAY_MAX_FILE_BYTES) {
  if (!Number.isSafeInteger(maxBytes) || maxBytes < 0 || maxBytes > QUICKPLAY_MAX_FILE_BYTES) {
    throw new Error(`Invalid read limit for ${label}`);
  }
  const noFollow = typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0;
  const handle = await fs.open(filePath, fsConstants.O_RDONLY | noFollow);
  try {
    const before = await handle.stat();
    if (!before.isFile()) {
      throw new Error(`${label} must be a regular file`);
    }
    if (!Number.isSafeInteger(before.size) || before.size < 0 || before.size > maxBytes) {
      throw new Error(`${label} exceeds the ${maxBytes}-byte limit`);
    }
    const chunks = [];
    let size = 0;
    for await (const chunk of handle.createReadStream({ autoClose: false })) {
      size += chunk.byteLength;
      if (!Number.isSafeInteger(size) || size > maxBytes) {
        throw new Error(`${label} exceeds the ${maxBytes}-byte limit`);
      }
      chunks.push(chunk);
    }
    const bytes = Buffer.concat(chunks, size);
    const after = await handle.stat();
    if (
      before.dev !== after.dev
      || before.ino !== after.ino
      || before.size !== after.size
      || before.mtimeMs !== after.mtimeMs
      || before.ctimeMs !== after.ctimeMs
      || bytes.byteLength !== before.size
    ) {
      throw new Error(`${label} changed while the Quickplay packager was reading it`);
    }
    if (bytes.byteLength > maxBytes) {
      throw new Error(`${label} exceeds the ${maxBytes}-byte limit`);
    }
    return bytes;
  } finally {
    await handle.close();
  }
}

async function digestRegularFileLimited(filePath, label, maxBytes = QUICKPLAY_MAX_FILE_BYTES) {
  if (!Number.isSafeInteger(maxBytes) || maxBytes < 0 || maxBytes > QUICKPLAY_MAX_FILE_BYTES) {
    throw new Error(`Invalid digest limit for ${label}`);
  }
  const noFollow = typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0;
  const handle = await fs.open(filePath, fsConstants.O_RDONLY | noFollow);
  try {
    const before = await handle.stat();
    if (!before.isFile()) {
      throw new Error(`${label} must be a regular file`);
    }
    if (!Number.isSafeInteger(before.size) || before.size < 0 || before.size > maxBytes) {
      throw new Error(`${label} exceeds the ${maxBytes}-byte limit`);
    }
    const hash = createHash('sha256');
    let size = 0;
    for await (const chunk of handle.createReadStream({ autoClose: false })) {
      size += chunk.byteLength;
      if (!Number.isSafeInteger(size) || size > maxBytes) {
        throw new Error(`${label} exceeds the ${maxBytes}-byte limit`);
      }
      hash.update(chunk);
    }
    const after = await handle.stat();
    if (
      before.dev !== after.dev
      || before.ino !== after.ino
      || before.size !== after.size
      || before.mtimeMs !== after.mtimeMs
      || before.ctimeMs !== after.ctimeMs
      || size !== before.size
    ) {
      throw new Error(`${label} changed while the Quickplay packager was hashing it`);
    }
    return { digest: `sha256:${hash.digest('hex')}`, size };
  } finally {
    await handle.close();
  }
}

async function copyRegularFileLimited(sourcePath, destinationPath, label, maxBytes = QUICKPLAY_MAX_FILE_BYTES) {
  if (!Number.isSafeInteger(maxBytes) || maxBytes < 0 || maxBytes > QUICKPLAY_MAX_FILE_BYTES) {
    throw new Error(`Invalid copy limit for ${label}`);
  }
  const noFollow = typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0;
  const source = await fs.open(sourcePath, fsConstants.O_RDONLY | noFollow);
  let destination = null;
  let completed = false;
  try {
    const before = await source.stat();
    if (!before.isFile()) throw new Error(`${label} must be a regular file`);
    if (!Number.isSafeInteger(before.size) || before.size < 0 || before.size > maxBytes) {
      throw new Error(`${label} exceeds the ${maxBytes}-byte limit`);
    }
    destination = await fs.open(
      destinationPath,
      fsConstants.O_WRONLY | fsConstants.O_CREAT | fsConstants.O_EXCL,
      before.mode & 0o777,
    );
    const hash = createHash('sha256');
    const chunk = Buffer.allocUnsafe(64 * 1024);
    let size = 0;
    while (true) {
      const { bytesRead } = await source.read(chunk, 0, chunk.byteLength, null);
      if (bytesRead === 0) break;
      size += bytesRead;
      if (!Number.isSafeInteger(size) || size > maxBytes) {
        throw new Error(`${label} exceeds the ${maxBytes}-byte limit`);
      }
      hash.update(chunk.subarray(0, bytesRead));
      let written = 0;
      while (written < bytesRead) {
        const result = await destination.write(chunk, written, bytesRead - written, null);
        if (result.bytesWritten <= 0) throw new Error(`${label} could not be copied completely`);
        written += result.bytesWritten;
      }
    }
    const after = await source.stat();
    if (
      before.dev !== after.dev
      || before.ino !== after.ino
      || before.size !== after.size
      || before.mtimeMs !== after.mtimeMs
      || before.ctimeMs !== after.ctimeMs
      || size !== before.size
    ) {
      throw new Error(`${label} changed while the Quickplay packager was copying it`);
    }
    await destination.sync();
    const copied = await destination.stat();
    if (!copied.isFile() || copied.size !== size) {
      throw new Error(`${label} staged copy has an unexpected size`);
    }
    completed = true;
    return { digest: `sha256:${hash.digest('hex')}`, size };
  } finally {
    await destination?.close().catch(() => {});
    await source.close().catch(() => {});
    if (!completed) await fs.rm(destinationPath, { force: true }).catch(() => {});
  }
}

function chargePackageBytes(
  budget,
  byteLength,
  label,
  maxBytes = QUICKPLAY_MAX_TOTAL_BYTES,
) {
  budget.files += 1;
  budget.bytes += byteLength;
  if (
    !Number.isSafeInteger(budget.files)
    || !Number.isSafeInteger(budget.bytes)
    || budget.files > QUICKPLAY_MAX_FILES
    || budget.bytes > maxBytes
  ) {
    throw new Error(
      `${label} exceeds the Quickplay package budget `
      + `(${budget.files}/${QUICKPLAY_MAX_FILES} files, `
      + `${budget.bytes}/${maxBytes} bytes)`,
    );
  }
}

function assertBrowserPortablePathSet(paths, destinationRoot, label) {
  const rootComponents = destinationRoot.split('/').filter(Boolean);
  const collisionKeys = new Map();
  for (const relative of paths) {
    const components = validatePortableRelativePath(relative, `${label} path`);
    const finalPath = `${destinationRoot}/${relative}`;
    if (
      rootComponents.length + components.length > QUICKPLAY_MAX_DEPTH
      || Buffer.byteLength(finalPath, 'utf8') > QUICKPLAY_MAX_PATH_BYTES
    ) {
      throw new Error(`${label} final browser path exceeds the VFS path limit: ${finalPath}`);
    }
    const collisionKey = portablePathCollisionKey(components.join('/'));
    const previous = collisionKeys.get(collisionKey);
    if (previous !== undefined) {
      throw new Error(`${label} contains case-insensitive duplicate paths: ${previous} and ${relative}`);
    }
    collisionKeys.set(collisionKey, relative);
  }
  for (const [collisionKey, relative] of collisionKeys) {
    const components = collisionKey.split('/');
    for (let length = 1; length < components.length; length += 1) {
      const ancestor = components.slice(0, length).join('/');
      if (collisionKeys.has(ancestor)) {
        throw new Error(`${label} contains a file/directory ancestor collision: ${collisionKeys.get(ancestor)} and ${relative}`);
      }
    }
  }
}

function assertBrowserStaticFileBudget(files, label, destinationRoot) {
  if (!Array.isArray(files) || files.length > QUICKPLAY_MAX_FILES) {
    throw new Error(`${label} exceeds the ${QUICKPLAY_MAX_FILES}-file browser limit`);
  }
  assertBrowserPortablePathSet(files.map((file) => file.path), destinationRoot, label);
  let bytes = 0;
  for (const file of files) {
    const fileBytes = packagedFileBytes(file);
    if (fileBytes.byteLength > QUICKPLAY_MAX_FILE_BYTES) {
      throw new Error(`${label} file ${file.path} exceeds the ${QUICKPLAY_MAX_FILE_BYTES}-byte browser limit`);
    }
    bytes += fileBytes.byteLength;
    if (!Number.isSafeInteger(bytes) || bytes > QUICKPLAY_MAX_SOURCE_PAYLOAD_BYTES) {
      throw new Error(`${label} exceeds the ${QUICKPLAY_MAX_SOURCE_PAYLOAD_BYTES}-byte browser source limit`);
    }
  }
  return bytes;
}

function assertBrowserDependencyBudget(dependencyPackage) {
  let files = 0;
  let bytes = 0;
  for (const modulePack of dependencyPackage.modules) {
    const destinationRoot = `/${modulePack.cacheDir}`;
    const sourceBytes = assertBrowserStaticFileBudget(
      modulePack.files,
      `BlockKart dependency source package ${modulePack.module}`,
      destinationRoot,
    );
    assertBrowserPortablePathSet(
      [
        ...modulePack.files.map((file) => file.path),
        ...modulePack.artifacts.map((artifact) => artifact.path),
      ],
      destinationRoot,
      `BlockKart dependency package ${modulePack.module}`,
    );
    files += modulePack.files.length;
    bytes += sourceBytes;
    for (const artifact of modulePack.artifacts) {
      if (!Number.isSafeInteger(artifact.size) || artifact.size < 0 || artifact.size > QUICKPLAY_MAX_FILE_BYTES) {
        throw new Error(`BlockKart dependency artifact ${artifact.name} has an invalid browser size`);
      }
      files += 1;
      bytes += artifact.size;
    }
    if (
      !Number.isSafeInteger(files)
      || !Number.isSafeInteger(bytes)
      || files > QUICKPLAY_MAX_FILES
      || bytes > QUICKPLAY_MAX_TOTAL_BYTES
    ) {
      throw new Error('BlockKart dependency package exceeds the browser VFS package budget');
    }
  }
  return { files, bytes };
}

function sameFilesystemEntry(left, right) {
  return left.dev === right.dev
    && left.ino === right.ino
    && left.mode === right.mode
    && left.size === right.size
    && left.mtimeMs === right.mtimeMs
    && left.ctimeMs === right.ctimeMs;
}

async function readStableRealDirectoryEntries(directory, label) {
  const before = await fs.lstat(directory);
  if (before.isSymbolicLink() || !before.isDirectory()) {
    throw new Error(`${label} must be a real directory: ${directory}`);
  }
  const entries = await fs.readdir(directory, { withFileTypes: true });
  const after = await fs.lstat(directory);
  if (
    after.isSymbolicLink()
    || !after.isDirectory()
    || !sameFilesystemEntry(before, after)
  ) {
    throw new Error(`${label} changed while being enumerated: ${directory}`);
  }
  entries.sort((left, right) => compareUtf8(left.name, right.name));
  return { entries, snapshot: after };
}

async function assertUnchangedRealDirectory(directory, snapshot, label) {
  const after = await fs.lstat(directory);
  if (
    after.isSymbolicLink()
    || !after.isDirectory()
    || !sameFilesystemEntry(snapshot, after)
  ) {
    throw new Error(`${label} changed during traversal: ${directory}`);
  }
}

async function walkFiles(root) {
  const out = [];
  let entriesSeen = 0;
  async function walk(current, depth) {
    const { entries, snapshot } = await readStableRealDirectoryEntries(
      current,
      'Dependency tree path',
    );
    for (const entry of entries) {
      entriesSeen += 1;
      if (entriesSeen > QUICKPLAY_MAX_WALK_ENTRIES) {
        throw new Error(`Dependency tree exceeds the ${QUICKPLAY_MAX_WALK_ENTRIES}-entry traversal limit`);
      }
      const absolute = path.join(current, entry.name);
      const metadata = await fs.lstat(absolute);
      if (metadata.isSymbolicLink()) {
        throw new Error(`Dependency tree contains a symbolic link: ${absolute}`);
      }
      if (metadata.isDirectory()) {
        if (shouldSkipDependencyDirectory(entry.name)) {
          continue;
        }
        if (depth >= QUICKPLAY_MAX_DEPTH) {
          throw new Error(`Dependency tree exceeds the ${QUICKPLAY_MAX_DEPTH}-level depth limit at ${absolute}`);
        }
        await walk(absolute, depth + 1);
      } else if (metadata.isFile()) {
        out.push(absolute);
        if (out.length > QUICKPLAY_MAX_FILES) {
          throw new Error(`Dependency tree exceeds the ${QUICKPLAY_MAX_FILES}-file limit`);
        }
      } else {
        throw new Error(`Dependency tree contains a special filesystem entry: ${absolute}`);
      }
    }
    await assertUnchangedRealDirectory(current, snapshot, 'Dependency tree path');
  }
  await walk(root, 0);
  return out.sort(compareUtf8);
}

function toPosixRelative(root, absolute) {
  return path.relative(root, absolute).split(path.sep).join('/');
}

async function digestBlockKartPath(relative) {
  const absolute = path.join(BLOCKKART_ROOT, relative);
  const file = await digestRegularFileLimited(absolute, `BlockKart input ${relative}`);
  return {
    digest: file.digest,
    path: relative,
    size: file.size,
  };
}

async function digestBlockKartPaths(paths) {
  const seen = new Set();
  const entries = [];
  for (const relative of paths) {
    if (seen.has(relative)) continue;
    seen.add(relative);
    entries.push(await digestBlockKartPath(relative));
  }
  return entries.sort((a, b) => compareUtf8(a.path, b.path));
}

async function digestBlockKartDirectory(relativeDir) {
  const absoluteDir = path.join(BLOCKKART_ROOT, relativeDir);
  const files = await walkFiles(absoluteDir);
  return digestBlockKartPaths(files.map((absolute) => toPosixRelative(BLOCKKART_ROOT, absolute)));
}

function shouldSkipDependencyDirectory(name) {
  return name.startsWith('.')
    || name === 'node_modules'
    || name === 'target'
    || name === 'tmp_checks';
}

async function walkProjectSourceFiles(root) {
  const out = [];
  let entriesSeen = 0;
  async function walk(current, depth) {
    const { entries, snapshot } = await readStableRealDirectoryEntries(
      current,
      'BlockKart tree path',
    );
    for (const entry of entries) {
      entriesSeen += 1;
      if (entriesSeen > QUICKPLAY_MAX_WALK_ENTRIES) {
        throw new Error(`BlockKart tree exceeds the ${QUICKPLAY_MAX_WALK_ENTRIES}-entry traversal limit`);
      }
      const absolute = path.join(current, entry.name);
      const metadata = await fs.lstat(absolute);
      if (metadata.isSymbolicLink()) {
        throw new Error(`BlockKart tree contains a symbolic link: ${absolute}`);
      }
      if (metadata.isDirectory()) {
        if (entry.name === '.git' || entry.name === 'target' || entry.name === 'node_modules' || entry.name === 'tmp_checks') {
          continue;
        }
        if (depth >= QUICKPLAY_MAX_DEPTH) {
          throw new Error(`BlockKart tree exceeds the ${QUICKPLAY_MAX_DEPTH}-level depth limit at ${absolute}`);
        }
        await walk(absolute, depth + 1);
      } else if (metadata.isFile()) {
        out.push(absolute);
        if (out.length > QUICKPLAY_MAX_FILES) {
          throw new Error(`BlockKart tree exceeds the ${QUICKPLAY_MAX_FILES}-file limit`);
        }
      } else {
        throw new Error(`BlockKart tree contains a special filesystem entry: ${absolute}`);
      }
    }
    await assertUnchangedRealDirectory(current, snapshot, 'BlockKart tree path');
  }
  await walk(root, 0);
  return out.sort(compareUtf8);
}

function parseLockFile(content) {
  return parseVoLockForV2Migration(content, 'BlockKart vo.lock').resolved;
}

function fileEntry(files, filePath) {
  const file = files.find((entry) => entry.path === filePath);
  if (!file) {
    throw new Error(`Packaged module is missing ${filePath}`);
  }
  return file;
}

function fileText(files, filePath) {
  return decodeModuleTextUtf8(
    packagedFileBytes(fileEntry(files, filePath)),
    `Packaged ${filePath}`,
  );
}

function strictUtf8(bytes, label) {
  return decodeModuleTextUtf8(bytes, label);
}

function parsePackagedJsonObject(moduleName, fileName, bytes) {
  const label = `Packaged ${moduleName} ${fileName}`;
  const value = parseBoundedStrictJsonBytes(bytes, label, {
    maxBytes: QUICKPLAY_MAX_METADATA_BYTES,
  });
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new Error(`${label} must contain a JSON object`);
  }
  return value;
}

function assertPublishedObjectKeys(value, required, optional, label) {
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  const allowed = new Set([...required, ...optional]);
  for (const key of Object.keys(value)) {
    if (!allowed.has(key)) throw new Error(`${label} contains unsupported field ${key}`);
  }
  for (const key of required) {
    if (!Object.hasOwn(value, key)) throw new Error(`${label} is missing field ${key}`);
  }
}

function normalizedPublishedRequirements(value, label, { requireSorted = true } = {}) {
  if (!Array.isArray(value) || value.length > QUICKPLAY_MAX_FILES) {
    throw new Error(`${label} must be a bounded array`);
  }
  const requirements = value.map((entry, index) => {
    assertPublishedObjectKeys(entry, ['module', 'constraint'], [], `${label}[${index}]`);
    if (typeof entry.module !== 'string' || typeof entry.constraint !== 'string') {
      throw new Error(`${label}[${index}] must contain string module and constraint fields`);
    }
    return { module: entry.module, constraint: entry.constraint };
  });
  if (!requireSorted) requirements.sort((left, right) => compareUtf8(left.module, right.module));
  for (let index = 1; index < requirements.length; index += 1) {
    const order = compareUtf8(requirements[index - 1].module, requirements[index].module);
    if (order === 0) throw new Error(`${label} contains duplicate module ${requirements[index].module}`);
    if (requireSorted && order > 0) throw new Error(`${label} must be sorted by module path`);
  }
  return requirements;
}

function normalizedPublishedArtifacts(value, label, { requireSorted = true } = {}) {
  if (!Array.isArray(value) || value.length > QUICKPLAY_MAX_FILES) {
    throw new Error(`${label} must be a bounded array`);
  }
  const artifacts = value.map((entry, index) => {
    assertPublishedObjectKeys(
      entry,
      ['kind', 'target', 'name', 'size', 'digest'],
      [],
      `${label}[${index}]`,
    );
    if (
      typeof entry.kind !== 'string'
      || typeof entry.target !== 'string'
      || typeof entry.name !== 'string'
      || !Number.isSafeInteger(entry.size)
      || entry.size < 0
      || entry.size > QUICKPLAY_MAX_FILE_BYTES
      || !/^sha256:[0-9a-f]{64}$/.test(entry.digest)
    ) {
      throw new Error(`${label}[${index}] has invalid artifact metadata`);
    }
    return {
      kind: entry.kind,
      target: entry.target,
      name: entry.name,
      size: entry.size,
      digest: entry.digest,
    };
  });
  if (!requireSorted) artifacts.sort((left, right) => compareUtf8(artifactKey(left), artifactKey(right)));
  for (let index = 1; index < artifacts.length; index += 1) {
    const order = compareUtf8(artifactKey(artifacts[index - 1]), artifactKey(artifacts[index]));
    if (order === 0) throw new Error(`${label} contains duplicate artifact ${artifactKey(artifacts[index])}`);
    if (requireSorted && order > 0) throw new Error(`${label} must be sorted by artifact identity`);
  }
  return artifacts;
}

function exactPublishedRecords(left, right) {
  return JSON.stringify(left) === JSON.stringify(right);
}

function verifyPublishedReleaseManifest(files, locked) {
  const releaseFile = files.find((file) => file.path === 'vo.release.json');
  if (!releaseFile) throw new Error(`Published ${locked.path} is missing vo.release.json`);
  if (!/^sha256:[0-9a-f]{64}$/.test(locked.release_manifest ?? '')) {
    throw new Error(`BlockKart vo.lock has no valid release manifest digest for ${locked.path}`);
  }
  const found = sha256Digest(packagedFileBytes(releaseFile));
  if (found !== locked.release_manifest) {
    throw new Error(`Published ${locked.path} vo.release.json does not match BlockKart vo.lock`);
  }
  const releaseBytes = packagedFileBytes(releaseFile);
  const release = parsePackagedJsonObject(locked.path, 'vo.release.json', releaseBytes);
  assertPublishedObjectKeys(
    release,
    ['schema_version', 'module', 'version', 'commit', 'module_root', 'vo', 'require', 'source', 'web_manifest'],
    ['artifacts'],
    `Published ${locked.path} vo.release.json`,
  );
  if (
    release.schema_version !== 1
    || release.module !== locked.path
    || release.version !== locked.version
    || release.commit !== locked.commit
  ) {
    throw new Error(`Published ${locked.path} release identity does not match BlockKart vo.lock`);
  }
  assertPublishedObjectKeys(
    release.source,
    ['name', 'size', 'digest', 'files_size', 'files_digest'],
    [],
    `Published ${locked.path} vo.release.json source`,
  );
  assertPublishedObjectKeys(
    release.web_manifest,
    ['size', 'digest'],
    [],
    `Published ${locked.path} vo.release.json web_manifest`,
  );
  if (
    typeof release.source.name !== 'string'
    || release.source.name.length === 0
    || !Number.isSafeInteger(release.source.size)
    || release.source.size <= 0
    || release.source.size > QUICKPLAY_MAX_FILE_BYTES
    || !/^sha256:[0-9a-f]{64}$/.test(release.source.digest ?? '')
    || release.source.digest !== locked.source
  ) {
    throw new Error(`Published ${locked.path} release source does not match BlockKart vo.lock`);
  }
  validatePortableComponent(
    release.source.name,
    `Published ${locked.path} vo.release.json source.name`,
  );
  const webFile = files.find((file) => file.path === 'vo.web.json');
  if (!webFile) throw new Error(`Published ${locked.path} is missing vo.web.json`);
  const webBytes = packagedFileBytes(webFile);
  if (
    !Number.isSafeInteger(release.web_manifest.size)
    || release.web_manifest.size <= 0
    || release.web_manifest.size > QUICKPLAY_MAX_METADATA_BYTES
    || !/^sha256:[0-9a-f]{64}$/.test(release.web_manifest.digest ?? '')
    || release.web_manifest.size !== webBytes.byteLength
    || release.web_manifest.digest !== sha256Digest(webBytes)
  ) {
    throw new Error(`Published ${locked.path} vo.web.json does not match vo.release.json`);
  }
  const web = parsePackagedJsonObject(locked.path, 'vo.web.json', webBytes);
  const contract = validateWebManifestVoModContract(
    web,
    fileText(files, 'vo.mod'),
    `Published ${locked.path} browser release contract`,
    {
      expectedModule: locked.path,
      expectedVersion: locked.version,
      expectedCommit: locked.commit,
      expectedVo: release.vo,
    },
  );
  if (release.module_root !== contract.manifest.module_root) {
    throw new Error(`Published ${locked.path} release module_root does not match vo.web.json`);
  }
  const releaseRequirements = normalizedPublishedRequirements(
    release.require,
    `Published ${locked.path} vo.release.json require`,
  );
  const webRequirements = contract.manifest.require.map((entry) => ({
    module: entry.module,
    constraint: entry.constraint,
  }));
  const lockedRequirements = normalizedPublishedRequirements(
    locked.deps ?? [],
    `BlockKart vo.lock dependencies for ${locked.path}`,
    { requireSorted: false },
  );
  const legacyLockedRequirements = lockedRequirements.some(
    (entry) => entry.constraint === '__legacy_requires_replacement__',
  );
  const lockedRequirementsMatch = legacyLockedRequirements
    ? exactPublishedRecords(
        releaseRequirements.map((entry) => entry.module),
        lockedRequirements.map((entry) => entry.module),
      )
    : exactPublishedRecords(releaseRequirements, lockedRequirements);
  if (
    !exactPublishedRecords(releaseRequirements, webRequirements)
    || !lockedRequirementsMatch
  ) {
    throw new Error(`Published ${locked.path} dependency edges disagree across release, web manifest, and vo.lock`);
  }
  const sourceFilesSize = contract.manifest.source.reduce((total, entry) => total + entry.size, 0);
  if (
    !Number.isSafeInteger(release.source.files_size)
    || release.source.files_size <= 0
    || release.source.files_size > QUICKPLAY_MAX_SOURCE_PAYLOAD_BYTES
    || release.source.files_size !== sourceFilesSize
    || release.source.files_digest !== contract.manifest.source_digest
  ) {
    throw new Error(`Published ${locked.path} browser source set does not match vo.release.json`);
  }
  const releaseArtifacts = normalizedPublishedArtifacts(
    release.artifacts ?? [],
    `Published ${locked.path} vo.release.json artifacts`,
  );
  const lockedArtifacts = normalizedPublishedArtifacts(
    locked.artifacts ?? [],
    `BlockKart vo.lock artifacts for ${locked.path}`,
    { requireSorted: false },
  );
  const webArtifacts = contract.manifest.artifacts.map((entry) => ({
    kind: entry.kind,
    target: entry.target,
    name: entry.name,
    size: entry.size,
    digest: entry.digest,
  }));
  const releaseBrowserArtifacts = releaseArtifacts.filter((entry) => entry.kind !== 'extension-native');
  const releaseArtifactsByKey = new Map(releaseArtifacts.map((entry) => [artifactKey(entry), entry]));
  const lockedArtifactsByKey = new Map(lockedArtifacts.map((entry) => [artifactKey(entry), entry]));
  const declaredArtifactKeys = contract.metadata.declaredArtifacts
    .map((entry) => artifactKey(entry))
    .sort(compareUtf8);
  const releaseArtifactKeys = releaseArtifacts.map((entry) => artifactKey(entry));
  const lockedArtifactsMatchRelease = lockedArtifacts.every((entry) => {
    const published = releaseArtifactsByKey.get(artifactKey(entry));
    return published !== undefined && exactPublishedRecords(published, entry);
  });
  const lockedContainsBrowserArtifacts = releaseBrowserArtifacts.every(
    (entry) => lockedArtifactsByKey.has(artifactKey(entry)),
  );
  if (
    !exactPublishedRecords(releaseArtifactKeys, declaredArtifactKeys)
    || !lockedArtifactsMatchRelease
    || !lockedContainsBrowserArtifacts
    || !exactPublishedRecords(releaseBrowserArtifacts, webArtifacts)
  ) {
    throw new Error(`Published ${locked.path} artifact set disagrees across release, web manifest, and vo.lock`);
  }
  if (fileText(files, '.vo-source-digest') !== `${release.source.digest}\n`) {
    throw new Error(`Published ${locked.path} .vo-source-digest does not match vo.release.json`);
  }
  if (fileText(files, '.vo-version') !== `${locked.version}\n`) {
    throw new Error(`Published ${locked.path} .vo-version does not match BlockKart vo.lock`);
  }
  return { ...contract, release };
}

function lockEntryForPackagedModule(module, originalLock) {
  const releaseFile = fileEntry(module.files, 'vo.release.json');
  const releaseBytes = packagedFileBytes(releaseFile);
  const release = parsePackagedJsonObject(module.module, 'vo.release.json', releaseBytes);
  if (release.module !== module.module || release.version !== module.version) {
    throw new Error(`Packaged ${module.module} release identity does not match dependency metadata`);
  }
  if (!originalLock || originalLock.version !== module.version) {
    throw new Error(`BlockKart vo.lock does not resolve ${module.module} at ${module.version}`);
  }
  if (typeof release.vo !== 'string' || typeof release.commit !== 'string') {
    throw new Error(`Packaged ${module.module} release is missing vo or commit metadata`);
  }
  if (!Array.isArray(release.require)) {
    throw new Error(`Packaged ${module.module} release.require must be an array`);
  }
  const deps = release.require.map((entry, index) => {
    if (
      !entry
      || typeof entry !== 'object'
      || Array.isArray(entry)
      || typeof entry.module !== 'string'
      || typeof entry.constraint !== 'string'
    ) {
      throw new Error(`Packaged ${module.module} release.require[${index}] is invalid`);
    }
    return { module: entry.module, constraint: entry.constraint };
  }).sort((left, right) => compareUtf8(left.module, right.module));
  for (let index = 1; index < deps.length; index += 1) {
    if (deps[index - 1].module === deps[index].module) {
      throw new Error(`Packaged ${module.module} release.require contains duplicate ${deps[index].module}`);
    }
  }
  const webFile = fileEntry(module.files, 'vo.web.json');
  const webBytes = packagedFileBytes(webFile);
  if (
    !release.web_manifest
    || !Number.isSafeInteger(release.web_manifest.size)
    || release.web_manifest.size < 0
    || !/^sha256:[0-9a-f]{64}$/.test(release.web_manifest.digest ?? '')
  ) {
    throw new Error(`Packaged ${module.module} vo.release.json has invalid web_manifest metadata`);
  }
  if (release.web_manifest.size !== webBytes.byteLength) {
    throw new Error(`Packaged ${module.module} vo.web.json size does not match vo.release.json`);
  }
  if (release.web_manifest.digest !== sha256Digest(webBytes)) {
    throw new Error(`Packaged ${module.module} vo.web.json digest does not match vo.release.json`);
  }
  const web = parsePackagedJsonObject(module.module, 'vo.web.json', webBytes);
  const source = fileText(module.files, '.vo-source-digest').trim();
  if (!Array.isArray(web.artifacts)) {
    throw new Error(`Packaged ${module.module} vo.web.json artifacts must be an array`);
  }
  const artifactsByKey = new Map();
  for (const artifact of web.artifacts) {
    const key = artifactKey(artifact);
    if (artifactsByKey.has(key)) {
      throw new Error(`Packaged ${module.module} vo.web.json contains duplicate artifact ${artifact.name}`);
    }
    artifactsByKey.set(key, artifact);
  }
  const artifacts = [];
  for (const artifact of module.artifacts ?? []) {
    const key = artifactKey(artifact);
    const webArtifact = artifactsByKey.get(key);
    if (!webArtifact) {
      throw new Error(`Packaged ${module.module} artifact ${artifact.name} is missing from vo.web.json`);
    }
    if (webArtifact.size !== artifact.size || webArtifact.digest !== artifact.digest) {
      throw new Error(`Packaged ${module.module} artifact ${artifact.name} bytes do not match vo.web.json`);
    }
    artifacts.push({
      kind: artifact.kind,
      target: artifact.target,
      name: artifact.name,
      size: webArtifact.size,
      digest: webArtifact.digest,
    });
  }
  if (artifactsByKey.size !== artifacts.length) {
    throw new Error(`Packaged ${module.module} vo.web.json declares an unpackaged artifact`);
  }
  artifacts.sort((a, b) => compareUtf8(artifactKey(a), artifactKey(b)));
  return {
    path: module.module,
    version: module.version,
    vo: release.vo,
    commit: release.commit,
    release_manifest: sha256Digest(releaseBytes),
    source,
    deps,
    artifacts,
  };
}

function lockRewriteEvidence(entry) {
  return {
    module: entry.path,
    version: entry.version,
    commit: entry.commit,
    releaseManifestDigest: entry.release_manifest,
    sourceDigest: entry.source,
    dependencies: (entry.deps ?? []).map((dependency) => ({
      module: dependency.module,
      constraint: dependency.constraint,
    })),
    artifacts: (entry.artifacts ?? []).map((artifact) => ({
      kind: artifact.kind,
      target: artifact.target,
      name: artifact.name,
      size: artifact.size,
      digest: artifact.digest,
    })),
  };
}

function rewriteProjectLockForPackagedDependencies(projectPackage, dependencyPackage, originalLocks) {
  const lockFile = projectPackage.files.find((file) => file.path === 'vo.lock');
  if (!lockFile?.content) {
    throw new Error('Project package is missing vo.lock');
  }
  const sourceLockDigest = sha256Digest(Buffer.from(lockFile.content, 'utf8'));
  const originalByPath = new Map(originalLocks.map((entry) => [entry.path, entry]));
  const parsedLock = parseVoLockForV2Migration(lockFile.content, 'BlockKart packaged vo.lock');
  const replacements = new Map();
  for (const module of dependencyPackage.modules) {
    replacements.set(module.module, lockEntryForPackagedModule(module, originalByPath.get(module.module)));
  }
  parsedLock.resolved = parsedLock.resolved.map((entry) => replacements.get(entry.path) ?? entry);
  for (const modulePath of replacements.keys()) {
    if (!parsedLock.resolved.some((entry) => entry.path === modulePath)) {
      throw new Error(`BlockKart vo.lock does not contain packaged dependency ${modulePath}`);
    }
  }
  if (
    parsedLock.migratedFromVersion === 1
    && parsedLock.resolved.some((entry) =>
      entry.deps.some((dependency) => dependency.constraint === '__legacy_requires_replacement__'))
  ) {
    throw new Error('BlockKart v1 lock contains an unresolved legacy dependency edge');
  }
  lockFile.content = renderVoLockV2(parsedLock);
  const modFile = projectPackage.files.find((file) => file.path === 'vo.mod');
  if (!modFile?.content) {
    throw new Error('Project package is missing vo.mod');
  }
  const authoritativeLock = parseVoLockV2(lockFile.content, 'BlockKart packaged vo.lock');
  const rootContract = parseVoModRootContract(modFile.content, 'BlockKart packaged vo.mod');
  validateVoLockV2RootGraph(authoritativeLock, rootContract, 'BlockKart packaged vo.lock');
  validatePackagedModuleSet(
    authoritativeLock,
    dependencyPackage.modules,
    'BlockKart packaged dependencies',
  );
  projectPackage.lockRewrite = {
    schemaVersion: 1,
    path: 'vo.lock',
    sourceDigest: sourceLockDigest,
    packagedDigest: sha256Digest(Buffer.from(lockFile.content, 'utf8')),
    modules: [...replacements.values()]
      .map(lockRewriteEvidence)
      .sort((a, b) => compareUtf8(a.module, b.module)),
  };
}

const PUBLISHED_RELEASE_CONTROL_PATHS = Object.freeze([
  '.vo-source-digest',
  '.vo-version',
  'vo.mod',
  'vo.release.json',
  'vo.web.json',
]);

function dependencyFileInventory(moduleDir, absoluteFiles, label) {
  const inventory = new Map();
  for (const absolute of absoluteFiles) {
    const relative = toPosixRelative(moduleDir, absolute);
    validatePortableRelativePath(relative, `${label} repository path`);
    if (inventory.has(relative)) throw new Error(`${label} contains duplicate repository path ${relative}`);
    inventory.set(relative, absolute);
  }
  return inventory;
}

function publishedBrowserRuntimeFileEntries(publishedContract, label) {
  const manifest = publishedContract?.manifest;
  if (!manifest || !Array.isArray(manifest.source)) {
    throw new Error(`${label} has no authenticated browser source manifest`);
  }
  const sourceByPath = new Map(manifest.source.map((entry) => [entry.path, entry]));
  const selected = new Map();
  const selectSource = (entry) => {
    const previous = selected.get(entry.path);
    if (previous && (previous.size !== entry.size || previous.digest !== entry.digest)) {
      throw new Error(`${label} has conflicting authenticated metadata for ${entry.path}`);
    }
    selected.set(entry.path, { ...entry, encoding: 'utf8', origin: 'source' });
  };
  for (const entry of manifest.source) {
    if (entry.path === 'vo.mod' || entry.path === 'vo.lock' || entry.path.endsWith('.vo')) {
      selectSource(entry);
    }
  }

  const includePaths = [
    ...(manifest.web?.include ?? []),
    ...(manifest.extension?.include ?? []),
  ];
  for (const includePath of includePaths) {
    let matches = 0;
    for (const entry of manifest.source) {
      if (entry.path === includePath || entry.path.startsWith(`${includePath}/`)) {
        selectSource(entry);
        matches += 1;
      }
    }
    if (matches === 0) {
      throw new Error(`${label} include path ${includePath} has no authenticated browser source files`);
    }
  }

  for (const [role, modulePath] of Object.entries(manifest.extension?.web?.js_modules ?? {})) {
    const entry = sourceByPath.get(modulePath);
    if (!entry) {
      throw new Error(`${label} browser JS module ${role} declares absent source ${modulePath}`);
    }
    selectSource(entry);
  }

  const selectLocalArtifact = (localPath, kind, field) => {
    if (localPath == null) return;
    const artifact = manifest.artifacts.find((entry) => entry.kind === kind);
    if (!artifact) throw new Error(`${label} ${field} has no authenticated browser artifact`);
    if (artifact.path !== localPath) {
      throw new Error(`${label} ${field} path ${localPath} does not match artifact path ${artifact.path}`);
    }
    const sourceEntry = sourceByPath.get(localPath);
    if (sourceEntry) {
      if (sourceEntry.size !== artifact.size || sourceEntry.digest !== artifact.digest) {
        throw new Error(`${label} ${field} disagrees between source and artifact metadata`);
      }
      selectSource(sourceEntry);
      return;
    }
    selected.set(localPath, {
      path: localPath,
      size: artifact.size,
      digest: artifact.digest,
      encoding: 'base64',
      origin: 'artifact',
    });
  };
  selectLocalArtifact(
    manifest.extension?.wasm?.local_wasm ?? null,
    'extension-wasm',
    '[extension.wasm].local_wasm',
  );
  selectLocalArtifact(
    manifest.extension?.wasm?.local_js_glue ?? null,
    'extension-js-glue',
    '[extension.wasm].local_js_glue',
  );

  if (!selected.has('vo.mod')) throw new Error(`${label} runtime closure is missing vo.mod`);
  return [...selected.values()].sort((left, right) => compareUtf8(left.path, right.path));
}

function assertAuthenticatedRepositoryRuntimeClosure(
  inventoryPaths,
  runtimeEntries,
  publishedContract,
  label,
) {
  const inventory = new Set(inventoryPaths);
  if (inventory.size !== inventoryPaths.length) throw new Error(`${label} repository inventory has duplicate paths`);
  const manifest = publishedContract.manifest;
  const required = new Set();
  const includePaths = [
    ...(manifest.web?.include ?? []),
    ...(manifest.extension?.include ?? []),
  ];
  for (const filePath of inventory) {
    if (filePath === 'vo.mod' || filePath === 'vo.lock' || filePath.endsWith('.vo')) {
      required.add(filePath);
    }
    if (includePaths.some((includePath) => (
      filePath === includePath || filePath.startsWith(`${includePath}/`)
    ))) {
      required.add(filePath);
    }
  }
  for (const filePath of Object.values(manifest.extension?.web?.js_modules ?? {})) {
    required.add(filePath);
  }
  for (const filePath of [
    manifest.extension?.wasm?.local_wasm ?? null,
    manifest.extension?.wasm?.local_js_glue ?? null,
  ]) {
    if (filePath !== null) required.add(filePath);
  }

  const authenticated = new Set(runtimeEntries.map((entry) => entry.path));
  if (authenticated.size !== runtimeEntries.length) {
    throw new Error(`${label} authenticated runtime closure has duplicate paths`);
  }
  const unauthenticated = [...required]
    .filter((filePath) => !authenticated.has(filePath))
    .sort(compareUtf8);
  const absent = [...authenticated]
    .filter((filePath) => !inventory.has(filePath))
    .sort(compareUtf8);
  if (unauthenticated.length > 0 || absent.length > 0) {
    throw new Error(
      `${label} repository runtime files do not exactly match authenticated metadata; `
      + `unauthenticated=[${unauthenticated.join(', ')}], absent=[${absent.join(', ')}]`,
    );
  }
}

function expectedPublishedPackagePaths(runtimeEntries) {
  return [...new Set([
    ...PUBLISHED_RELEASE_CONTROL_PATHS,
    ...runtimeEntries.map((entry) => entry.path),
  ])].sort(compareUtf8);
}

function assertExactPublishedPackagePaths(files, expectedPaths, label) {
  const foundPaths = files.map((file) => file.path);
  const foundSet = new Set(foundPaths);
  if (foundSet.size !== foundPaths.length) throw new Error(`${label} contains duplicate packaged paths`);
  const expectedSet = new Set(expectedPaths);
  if (expectedSet.size !== expectedPaths.length) throw new Error(`${label} expected closure contains duplicate paths`);
  const missing = [...expectedSet].filter((filePath) => !foundSet.has(filePath)).sort(compareUtf8);
  const unexpected = [...foundSet].filter((filePath) => !expectedSet.has(filePath)).sort(compareUtf8);
  if (missing.length > 0 || unexpected.length > 0) {
    throw new Error(
      `${label} does not exactly match its authenticated runtime closure; `
      + `missing=[${missing.join(', ')}], unexpected=[${unexpected.join(', ')}]`,
    );
  }
}

function verifyPublishedRuntimeFileBytes(entry, bytes, label) {
  if (bytes.byteLength !== entry.size) {
    throw new Error(`${label} size does not match authenticated metadata`);
  }
  if (sha256Digest(bytes) !== entry.digest) {
    throw new Error(`${label} digest does not match authenticated metadata`);
  }
}

function shouldDeclarePackagedSourceFile(file) {
  if (
    file.path === 'vo.web.json'
    || file.path === 'vo.release.json'
    || file.path === '.vo-version'
    || file.path === '.vo-source-digest'
  ) {
    return false;
  }
  return file.content != null || file.contentBase64 != null;
}

function upsertTextFile(files, filePath, content) {
  const existing = files.find((file) => file.path === filePath);
  if (existing) {
    existing.content = content;
    delete existing.contentBase64;
    return;
  }
  files.push({ path: filePath, content });
}

function dropNativeExtensionTables(voModSource) {
  const lines = voModSource.split(/\r?\n/);
  const out = [];
  let skip = false;
  for (const line of lines) {
    const table = line.trim().match(/^\[\[?([^\]]+)\]\]?$/);
    if (table) {
      const name = table[1].trim();
      skip = name === 'extension.native' || name.startsWith('extension.native.');
    }
    if (!skip) {
      out.push(line);
    }
  }
  return `${out.join('\n').replace(/\n{3,}/g, '\n\n').trimEnd()}\n`;
}

function parseVoModRequirements(voModSource) {
  const require = [];
  for (const rawLine of voModSource.split(/\r?\n/)) {
    const line = rawLine.trim();
    if (!line || line.startsWith('#')) continue;
    const match = line.match(/^require\s+(\S+)\s+(\S+)$/);
    if (match) {
      require.push({ module: match[1], constraint: match[2] });
    }
  }
  return require.sort((a, b) => compareUtf8(a.module, b.module));
}

function packagedSourceEntry(file) {
  const bytes = packagedFileBytes(file);
  return {
    digest: sha256Digest(bytes),
    path: file.path,
    size: bytes.byteLength,
  };
}

function sourceSetDigest(entries) {
  const canonical = entries
    .map((entry) => ({ path: entry.path, size: entry.size, digest: entry.digest }))
    .sort((a, b) => compareUtf8(a.path, b.path));
  return sha256Digest(Buffer.from(JSON.stringify(canonical), 'utf8'));
}

async function sourceFileEntry(root, absolute) {
  const relative = path.relative(root, absolute).split(path.sep).join('/');
  const file = await digestRegularFileLimited(absolute, `BlockKart source ${relative}`);
  return {
    path: relative,
    digest: file.digest,
    size: file.size,
  };
}

async function blockKartSourceFiles() {
  const files = [];
  const budget = { files: 0, bytes: 0 };
  for (const absolute of await walkProjectSourceFiles(BLOCKKART_ROOT)) {
    if (!absolute.endsWith('.vo')) continue;
    const entry = await sourceFileEntry(BLOCKKART_ROOT, absolute);
    chargePackageBytes(budget, entry.size, 'BlockKart source inventory');
    files.push(entry);
  }
  files.sort((a, b) => compareUtf8(a.path, b.path));
  return files;
}

function syntheticBrowserReleaseManifest(manifest, webManifestContent) {
  const artifacts = (manifest.artifacts ?? [])
    .map((artifact) => ({
      kind: artifact.kind,
      target: artifact.target,
      name: artifact.name,
      size: artifact.size,
      digest: artifact.digest,
    }))
    .sort((a, b) => (
      compareUtf8(a.kind, b.kind)
      || compareUtf8(a.target, b.target)
      || compareUtf8(a.name, b.name)
    ));
  const require = (manifest.require ?? [])
    .map((entry) => ({ module: entry.module, constraint: entry.constraint }))
    .sort((a, b) => compareUtf8(a.module, b.module));
  return {
    schema_version: 1,
    module: manifest.module,
    version: manifest.version,
    commit: manifest.commit,
    module_root: manifest.module_root ?? '.',
    vo: manifest.vo,
    require,
    source: {
      name: 'vo.web.json',
      size: manifest.source.reduce((total, entry) => total + entry.size, 0),
      digest: manifest.source_digest,
      files_size: manifest.source.reduce((total, entry) => total + entry.size, 0),
      files_digest: manifest.source_digest,
    },
    web_manifest: {
      size: Buffer.byteLength(webManifestContent, 'utf8'),
      digest: sha256Digest(Buffer.from(webManifestContent, 'utf8')),
    },
    artifacts,
  };
}

async function rewritePackagedWebManifest(
  moduleDir,
  files,
  artifacts,
  locked,
  publishedContract,
) {
  const manifestFile = files.find((file) => file.path === 'vo.web.json');
  if (!manifestFile) throw new Error(`Published ${locked.path} is missing vo.web.json`);

  // Authenticate a published release before deriving the Quickplay cache
  // snapshot from it. Every resulting web manifest describes the exact files
  // mounted into the browser VFS; installed-cache validation requires this
  // bidirectional closure even when the source release contains build-only
  // files that Quickplay deliberately omits.
  if (!publishedContract?.manifest || !publishedContract?.release) {
    throw new Error(`Published ${locked.path} release contract was not verified before packaging`);
  }

  const modFile = files.find((file) => file.path === 'vo.mod');
  if (modFile) {
    modFile.content = dropNativeExtensionTables(modFile.content);
  }

  const manifest = parsePackagedJsonObject(
    locked.path,
    'vo.web.json',
    Buffer.from(manifestFile.content, 'utf8'),
  );
  manifest.module = locked.path;
  manifest.version = locked.version;
  if (modFile) {
    manifest.require = parseVoModRequirements(modFile.content);
  }
  if (locked.commit) {
    manifest.commit = locked.commit;
  }
  const source = files
    .filter(shouldDeclarePackagedSourceFile)
    .map(packagedSourceEntry)
    .sort((a, b) => compareUtf8(a.path, b.path));
  manifest.source = source;
  manifest.source_digest = sourceSetDigest(source);

  if (Array.isArray(manifest.artifacts)) {
    const packagedArtifacts = new Map(artifacts.map((artifact) => [artifactKey(artifact), artifact]));
    const nextArtifacts = [];
    for (const artifact of manifest.artifacts) {
      const artifactInfo = packagedArtifacts.get(artifactKey(artifact));
      if (!artifactInfo) {
        throw new Error(`Packaged ${locked.path} omits authenticated artifact ${artifactKey(artifact)}`);
      }
      const file = await digestRegularFileLimited(
        artifactInfo.sourcePath ?? path.join(moduleDir, artifactInfo.path),
        `Packaged ${locked.path} artifact ${artifactInfo.path}`,
      );
      nextArtifacts.push({
        ...artifact,
        digest: file.digest,
        path: artifactInfo.path,
        size: file.size,
      });
    }
    if (nextArtifacts.length !== packagedArtifacts.size) {
      throw new Error(`Packaged ${locked.path} contains an artifact absent from its authenticated manifest`);
    }
    manifest.artifacts = nextArtifacts;
  }

  const webManifestContent = `${JSON.stringify(manifest, null, 2)}\n`;
  manifestFile.content = webManifestContent;
  upsertTextFile(files, '.vo-version', `${locked.version}\n`);
  upsertTextFile(files, '.vo-source-digest', `${manifest.source_digest}\n`);
  upsertTextFile(
    files,
    'vo.release.json',
    `${JSON.stringify(syntheticBrowserReleaseManifest(manifest, webManifestContent), null, 2)}\n`,
  );
}

async function dependencyModuleDir(locked) {
  const cacheDir = moduleCacheDir(locked.path, locked.version);
  const [key, version] = cacheDir.split('/');
  const moduleDir = path.join(MOD_CACHE_ROOT, key, version);
  if (!(await pathExists(moduleDir))) {
    throw new Error(`Missing installed dependency cache: ${moduleDir}`);
  }
  return {
    cacheDir,
    commit: locked.commit ?? null,
    dirty: false,
    moduleDir,
    source: 'module-cache',
  };
}

async function dependencyArtifactSourcePath(moduleDir, artifact, locked) {
  const relative = artifactCachePath(artifact);
  const candidate = path.join(moduleDir, ...relative.split('/'));
  if (await pathExists(candidate)) {
    return candidate;
  }
  throw new Error(`Missing dependency artifact ${artifactKey(artifact)}; checked ${candidate}`);
}

async function buildProjectPackage() {
  const files = [];
  const budget = { files: 0, bytes: 0 };
  const sourceFiles = await blockKartSourceFiles();
  const sourceAllowlist = quickplayBlockKartSourceAllowlist(
    sourceFiles.map((entry) => entry.path),
  );
  const rootEntries = await fs.readdir(BLOCKKART_ROOT, { withFileTypes: true });
  for (const entry of rootEntries) {
    if (!entry.isFile()) continue;
    if (!entry.name.endsWith('.vo') && entry.name !== 'vo.mod' && entry.name !== 'vo.lock') {
      continue;
    }
    const bytes = await readRegularFileLimited(
      path.join(BLOCKKART_ROOT, entry.name),
      `BlockKart project file ${entry.name}`,
    );
    chargePackageBytes(
      budget,
      bytes.byteLength,
      'BlockKart project package',
      QUICKPLAY_MAX_SOURCE_PAYLOAD_BYTES,
    );
    files.push({
      path: entry.name,
      content: decodeModuleTextUtf8(bytes, `BlockKart ${entry.name}`),
    });
  }
  for (const relative of BLOCKKART_RUNTIME_ASSETS) {
    const absolute = path.join(BLOCKKART_ROOT, relative);
    if (!(await pathExists(absolute))) {
      throw new Error(`Missing BlockKart runtime asset: ${absolute}`);
    }
    const bytes = await readRegularFileLimited(absolute, `BlockKart runtime asset ${relative}`);
    chargePackageBytes(
      budget,
      bytes.byteLength,
      'BlockKart project package',
      QUICKPLAY_MAX_SOURCE_PAYLOAD_BYTES,
    );
    files.push({
      path: relative,
      contentBase64: bytes.toString('base64'),
    });
  }
  files.sort((a, b) => compareUtf8(a.path, b.path));
  return {
    schemaVersion: 2,
    name: 'BlockKart',
    module: 'github.com/vo-lang/blockkart',
    commit: gitOutput(['rev-parse', 'HEAD'], BLOCKKART_ROOT),
    dirty: gitStatus(BLOCKKART_ROOT) !== '',
    sourceFiles,
    sourceAllowlist,
    files,
  };
}

async function buildDependencyPackage(lockModules) {
  const modules = [];
  const budget = { files: 0, bytes: 0 };
  for (const locked of lockModules) {
    const { cacheDir, commit, dirty, moduleDir, source } = await dependencyModuleDir(locked);
    const packagedLock = { ...locked, commit: commit ?? locked.commit };

    const inventory = dependencyFileInventory(
      moduleDir,
      await walkFiles(moduleDir),
      `Published ${locked.path}`,
    );
    const files = [];
    const originalBytes = new Map();
    const sourceBudget = { files: 0, bytes: 0 };
    for (const relative of PUBLISHED_RELEASE_CONTROL_PATHS) {
      const absolute = inventory.get(relative);
      if (!absolute) throw new Error(`Published ${locked.path} is missing required control file ${relative}`);
      const bytes = await readRegularFileLimited(
        absolute,
        `Published ${locked.path} control file ${relative}`,
        relative.startsWith('.vo-') ? 1024 : QUICKPLAY_MAX_METADATA_BYTES,
      );
      originalBytes.set(relative, bytes);
      files.push({
        path: relative,
        content: strictUtf8(bytes, `Published ${locked.path} control file ${relative}`),
      });
    }
    const publishedContract = verifyPublishedReleaseManifest(files, packagedLock);
    const runtimeEntries = publishedBrowserRuntimeFileEntries(
      publishedContract,
      `Published ${locked.path}`,
    );
    assertAuthenticatedRepositoryRuntimeClosure(
      [...inventory.keys()],
      runtimeEntries,
      publishedContract,
      `Published ${locked.path}`,
    );
    const expectedPackagePaths = expectedPublishedPackagePaths(runtimeEntries);
    for (const entry of runtimeEntries) {
      const absolute = inventory.get(entry.path);
      if (!absolute) throw new Error(`Published ${locked.path} runtime closure is missing ${entry.path}`);
      const bytes = originalBytes.get(entry.path) ?? await readRegularFileLimited(
        absolute,
        `Published ${locked.path} runtime file ${entry.path}`,
        entry.size,
      );
      verifyPublishedRuntimeFileBytes(
        entry,
        bytes,
        `Published ${locked.path} runtime file ${entry.path}`,
      );
      if (!originalBytes.has(entry.path)) {
        originalBytes.set(entry.path, bytes);
        files.push(entry.encoding === 'base64'
          ? { path: entry.path, contentBase64: bytes.toString('base64') }
          : {
              path: entry.path,
              content: strictUtf8(bytes, `Published ${locked.path} runtime file ${entry.path}`),
            });
      }
    }
    assertExactPublishedPackagePaths(
      files,
      expectedPackagePaths,
      `Published ${locked.path} dependency package`,
    );
    for (const file of files) {
      const bytes = packagedFileBytes(file);
      chargePackageBytes(budget, bytes.byteLength, 'BlockKart dependency package');
      chargePackageBytes(
        sourceBudget,
        bytes.byteLength,
        `BlockKart dependency source package ${locked.path}`,
        QUICKPLAY_MAX_SOURCE_PAYLOAD_BYTES,
      );
    }

    const artifacts = [];
    for (const artifact of locked.artifacts) {
      if (artifact.target !== WASM_TARGET) continue;
      if (artifact.kind !== 'extension-wasm' && artifact.kind !== 'extension-js-glue') continue;
      const sourcePath = await dependencyArtifactSourcePath(moduleDir, artifact, locked);
      const cachePath = artifactCachePath(artifact);
      const outRelative = artifactOutputRelativePath(cacheDir, artifact);
      const stagedPath = path.join(activeOutRoot, outRelative);
      await fs.mkdir(path.dirname(stagedPath), { recursive: true });
      const stagedFile = await copyRegularFileLimited(
        sourcePath,
        stagedPath,
        `Dependency artifact ${locked.path}/${artifact.name}`,
      );
      if (stagedFile.size !== artifact.size || stagedFile.digest !== artifact.digest) {
        throw new Error(`Dependency artifact ${locked.path}/${artifact.name} does not match BlockKart vo.lock`);
      }
      chargePackageBytes(budget, stagedFile.size, 'BlockKart dependency package');
      artifacts.push({
        kind: artifact.kind,
        target: artifact.target,
        name: artifact.name,
        size: stagedFile.size,
        digest: stagedFile.digest,
        path: cachePath,
        sourcePath: stagedPath,
        url: quickplayArtifactUrl(cacheDir, artifact),
      });
    }

    await rewritePackagedWebManifest(
      moduleDir,
      files,
      artifacts,
      packagedLock,
      publishedContract,
    );
    assertExactPublishedPackagePaths(
      files,
      expectedPackagePaths,
      `Packaged ${locked.path} dependency package`,
    );
    assertBrowserStaticFileBudget(
      files,
      `BlockKart dependency source package ${locked.path}`,
      `/${cacheDir}`,
    );
    assertBrowserPortablePathSet(
      [...files.map((file) => file.path), ...artifacts.map((artifact) => artifact.path)],
      `/${cacheDir}`,
      `BlockKart dependency package ${locked.path}`,
    );
    files.sort((a, b) => compareUtf8(a.path, b.path));
    artifacts.sort((a, b) => compareUtf8(a.path, b.path));
    modules.push({
      module: locked.path,
      version: locked.version,
      commit: packagedLock.commit ?? null,
      cacheDir,
      dirty,
      source,
      files,
      artifacts: artifacts.map(({ sourcePath, ...artifact }) => artifact),
    });
  }
  modules.sort((a, b) => compareUtf8(a.module, b.module));
  return {
    schemaVersion: 2,
    name: 'BlockKart dependencies',
    modules,
  };
}

async function quickplayArtifactDigest(artifact) {
  const relative = quickplayArtifactRelativePathFromUrl(artifact.url);
  const file = await digestRegularFileLimited(
    path.join(activeOutRoot, relative),
    `Quickplay artifact ${relative}`,
  );
  return {
    digest: file.digest,
    kind: artifact.kind,
    target: artifact.target,
    name: artifact.name,
    path: artifact.path,
    size: file.size,
    url: artifact.url,
  };
}

async function dependencyProvenance(module) {
  const artifacts = [];
  for (const artifact of module.artifacts ?? []) {
    artifacts.push(await quickplayArtifactDigest(artifact));
  }
  artifacts.sort((a, b) => compareUtf8(a.path, b.path));
  return {
    artifacts,
    cacheDir: module.cacheDir,
    dirty: module.dirty,
    filesDigest: packagedFilesDigest(module.files),
    module: module.module,
    source: module.source,
    version: module.version,
    commit: module.commit ?? null,
  };
}

async function buildRuntimeAssetProducerProvenance() {
  const currentWasm = verifyCurrentVoplayWasm({
    voplayRoot: VOPLAY_ROOT,
    outDir: VOPLAY_CURRENT_WASM_ROOT,
    expectedCiRunId: process.env.VO_DEV_CI_RUN_ID ?? null,
  });
  if (currentWasm.issues.length > 0) {
    throw new Error(`Current-source voplay WASM provenance is invalid: ${currentWasm.issues.join('; ')}`);
  }
  execFileSync(process.execPath, ['tools/vpak_provenance.mjs', '--check'], {
    cwd: BLOCKKART_ROOT,
    env: process.env,
    stdio: 'pipe',
  });
  const vpakManifestPath = path.join(BLOCKKART_ROOT, 'assets/blockkart.vpak.provenance.json');
  const vpakManifestBytes = await readRegularFileLimited(
    vpakManifestPath,
    'BlockKart VPAK provenance manifest',
    QUICKPLAY_MAX_METADATA_BYTES,
  );
  const vpakManifest = parseBoundedStrictJsonBytes(
    vpakManifestBytes,
    'BlockKart VPAK provenance manifest',
    { maxBytes: QUICKPLAY_MAX_METADATA_BYTES },
  );
  if (JSON.stringify(vpakManifest.command) !== JSON.stringify(VO_CLI_GUEST_ENVIRONMENT.command)) {
    throw new Error('BlockKart VPAK manifest command does not match the authenticated Vo CLI guest command');
  }
  const buildReportBytes = await readRegularFileLimited(
    BLOCKKART_VPAK_BUILD_REPORT,
    'BlockKart VPAK build report',
    QUICKPLAY_MAX_METADATA_BYTES,
  );
  const buildReport = parseBoundedStrictJsonBytes(
    buildReportBytes,
    'BlockKart VPAK build report',
    { maxBytes: QUICKPLAY_MAX_METADATA_BYTES },
  );
  const reportKeys = [
    'archiveEntryCount',
    'ciRunId',
    'kind',
    'pack',
    'payloadInputCount',
    'producerDigest',
    'producerManifest',
    'schemaVersion',
    'toolchain',
    'voBinary',
    'voCliBuildInputs',
    'voCliExecutionDigest',
    'workspaceSourceInputCount',
  ];
  if (JSON.stringify(Object.keys(buildReport).sort(compareUtf8)) !== JSON.stringify(reportKeys)) {
    throw new Error('BlockKart VPAK build report fields do not match schema v2');
  }
  if (buildReport.schemaVersion !== 2 || buildReport.kind !== 'blockkart.vpakBuildReport') {
    throw new Error('BlockKart VPAK build report identity is invalid');
  }
  const expectedCiRunId = process.env.VO_DEV_CI_RUN_ID ?? null;
  if (buildReport.ciRunId !== expectedCiRunId) {
    throw new Error('BlockKart VPAK build report ciRunId does not match this task graph run');
  }
  const currentCliInputs = currentVoCliBuildInputs(VOLANG_ROOT);
  assertVoCliBuildInputs(buildReport.voCliBuildInputs, {
    expected: currentCliInputs,
    label: 'BlockKart VPAK build report Vo CLI inputs',
  });
  const currentCliToolchain = currentVoCliToolchain(VOLANG_ROOT);
  if (JSON.stringify(buildReport.toolchain) !== JSON.stringify(currentCliToolchain)) {
    throw new Error('BlockKart VPAK build report Rust toolchain does not match this task graph run');
  }
  if (
    buildReport.producerDigest !== vpakManifest.producerDigest
    || buildReport.archiveEntryCount !== vpakManifest.archiveEntryCount
    || buildReport.payloadInputCount !== vpakManifest.payloadInputCount
    || buildReport.workspaceSourceInputCount !== vpakManifest.workspaceSourceInputCount
    || JSON.stringify(buildReport.pack) !== JSON.stringify(vpakManifest.pack)
  ) {
    throw new Error('BlockKart VPAK build report does not match the current producer manifest');
  }
  const expectedProducerManifest = {
    path: 'assets/blockkart.vpak.provenance.json',
    digest: sha256Digest(vpakManifestBytes),
    size: vpakManifestBytes.byteLength,
  };
  if (JSON.stringify(buildReport.producerManifest) !== JSON.stringify(expectedProducerManifest)) {
    throw new Error('BlockKart VPAK build report producerManifest binding is stale');
  }
  if (
    !buildReport.voBinary
    || JSON.stringify(Object.keys(buildReport.voBinary).sort(compareUtf8))
      !== JSON.stringify(['digest', 'path', 'size'])
    || typeof buildReport.voBinary.path !== 'string'
    || !/^sha256:[0-9a-f]{64}$/.test(buildReport.voBinary.digest ?? '')
    || !Number.isSafeInteger(buildReport.voBinary.size)
    || buildReport.voBinary.size < 0
  ) {
    throw new Error('BlockKart VPAK build report Vo binary binding is invalid');
  }
  if (
    !buildReport.producerManifest
    || JSON.stringify(Object.keys(buildReport.producerManifest).sort(compareUtf8))
      !== JSON.stringify(['digest', 'path', 'size'])
  ) {
    throw new Error('BlockKart VPAK build report producerManifest binding is invalid');
  }
  const voBinaryPath = path.resolve(VOLANG_ROOT, buildReport.voBinary.path);
  if (!pathContains(VOLANG_ROOT, voBinaryPath)) {
    throw new Error('BlockKart VPAK build report Vo binary path escapes the Volang root');
  }
  const currentVoBinary = await digestRegularFileLimited(
    voBinaryPath,
    'BlockKart VPAK producer Vo binary',
  );
  if (
    currentVoBinary.digest !== buildReport.voBinary.digest
    || currentVoBinary.size !== buildReport.voBinary.size
  ) {
    throw new Error('BlockKart VPAK build report Vo binary binding is stale');
  }
  const executionIssues = verifyVoCliExecutionIdentity(
    buildReport.toolchain,
    buildReport.voBinary,
    {
      buildInputs: buildReport.voCliBuildInputs,
      executionDigest: buildReport.voCliExecutionDigest,
      expectedToolchain: currentCliToolchain,
    },
  );
  if (executionIssues.length > 0) {
    throw new Error(`BlockKart VPAK build report execution identity is invalid: ${executionIssues.join('; ')}`);
  }
  const toQuickplayDigest = (entry) => ({
    path: entry.path,
    digest: `sha256:${entry.sha256}`,
    size: entry.size,
  });
  const vpakUpstream = (vpakManifest.upstream ?? []).map((entry) => ({
    ...entry,
    owner: 'BlockKart',
    kind: entry.id === 'painted-terrain-textures'
      ? 'offline-texture-generation'
      : 'offline-terrain-generation',
    inputs: (entry.inputs ?? []).map(toQuickplayDigest),
    outputs: (entry.outputs ?? []).map(toQuickplayDigest),
  }));
  return [
    {
      id: 'voplay-current-source-wasm',
      owner: 'voplay/rust',
      kind: 'wasm-bindgen',
      command: currentWasm.manifest.command,
      sourceClosure: currentWasm.manifest.sourceClosure,
      volangBuildInputs: currentWasm.manifest.volangBuildInputs,
      ffiSourceFingerprint: currentWasm.manifest.ffiSourceFingerprint,
      toolchain: currentWasm.manifest.toolchain,
      buildPlatform: currentWasm.manifest.buildPlatform,
      outputs: currentWasm.manifest.outputs,
    },
    {
      id: 'blockkart-runtime-vpak',
      owner: 'BlockKart',
      kind: 'vpak',
      output: 'assets/blockkart.vpak',
      command: vpakManifest.command,
      toolchain: buildReport.toolchain,
      voBinary: buildReport.voBinary,
      voCliBuildInputs: buildReport.voCliBuildInputs,
      voCliExecutionDigest: buildReport.voCliExecutionDigest,
      inputs: vpakManifest.inputs.map(toQuickplayDigest),
      outputs: [toQuickplayDigest(vpakManifest.pack)],
      upstream: vpakUpstream,
      producerManifest: {
        path: 'assets/blockkart.vpak.provenance.json',
        sha256: sha256Digest(vpakManifestBytes),
        size: vpakManifestBytes.length,
        producerDigest: vpakManifest.producerDigest,
      },
      archiveEntryCount: vpakManifest.archiveEntryCount,
      payloadInputCount: vpakManifest.payloadInputCount,
      workspaceSourceInputCount: vpakManifest.workspaceSourceInputCount,
      archiveEntries: vpakManifest.archiveEntries,
    },
  ];
}

async function buildProvenance(projectPackage, dependencyPackage, outputBytes) {
  const dependencies = [];
  for (const module of dependencyPackage.modules) {
    dependencies.push(await dependencyProvenance(module));
  }
  dependencies.sort((a, b) => compareUtf8(a.module, b.module));

  return {
    schemaVersion: 3,
    artifact: QUICKPLAY_ARTIFACT_NAME,
    path: QUICKPLAY_ARTIFACT_PATH,
    task: {
      id: QUICKPLAY_TASK_ID,
      command: QUICKPLAY_GENERATOR_COMMAND,
    },
    generator: {
      command: QUICKPLAY_GENERATOR_COMMAND,
      script: 'apps/studio/scripts/package_blockkart_quickplay.mjs',
      version: QUICKPLAY_GENERATOR_VERSION,
    },
    toolchain: {
      node: `v${process.versions.node.split('.')[0]}`,
      voDevSourceDigest: await volangGeneratorSourceDigest(),
      wasmTarget: WASM_TARGET,
    },
    sourceRoots: QUICKPLAY_SOURCE_ROOTS,
    inputs: QUICKPLAY_GENERATOR_INPUTS,
    project: {
      commit: projectPackage.commit,
      dirty: projectPackage.dirty,
      filesDigest: packagedFilesDigest(projectPackage.files),
      lockRewrite: projectPackage.lockRewrite ?? null,
      module: projectPackage.module,
      sourceFiles: projectPackage.sourceFiles,
      sourceAllowlist: projectPackage.sourceAllowlist,
      sourceFilesDigest: sourceSetDigest(projectPackage.sourceFiles),
    },
    producers: await buildRuntimeAssetProducerProvenance(),
    dependencies,
    outputs: [
      {
        digest: sha256Digest(outputBytes.project),
        path: 'project.json',
        size: outputBytes.project.byteLength,
      },
      {
        digest: sha256Digest(outputBytes.deps),
        path: 'deps.json',
        size: outputBytes.deps.byteLength,
      },
    ],
  };
}

async function completeQuickplayOutputDirectory(directory) {
  for (const name of ['project.json', 'deps.json', 'provenance.json']) {
    try {
      const metadata = await fs.lstat(path.join(directory, name));
      if (!metadata.isFile() || metadata.isSymbolicLink()) return false;
    } catch {
      return false;
    }
  }
  return true;
}

async function recoverInterruptedQuickplayOutput(parent, name) {
  const prefix = `.${name}.previous-`;
  const candidates = (await fs.readdir(parent, { withFileTypes: true }))
    .filter((entry) => entry.isDirectory() && entry.name.startsWith(prefix))
    .map((entry) => path.join(parent, entry.name))
    .sort(compareUtf8)
    .reverse();
  if (await pathExists(OUT_ROOT)) {
    for (const candidate of candidates) {
      await fs.rm(candidate, { recursive: true, force: true });
    }
    return;
  }
  for (const candidate of candidates) {
    if (await completeQuickplayOutputDirectory(candidate)) {
      await fs.rename(candidate, OUT_ROOT);
      for (const stale of candidates) {
        if (stale !== candidate) await fs.rm(stale, { recursive: true, force: true });
      }
      console.warn(`Quickplay packager restored interrupted output ${candidate}`);
      return;
    }
  }
  if (candidates.length > 0) {
    throw new Error(`Quickplay output is missing and no complete previous tree can be restored: ${candidates.join(', ')}`);
  }
}

async function prepareAtomicOutput() {
  const parent = path.dirname(OUT_ROOT);
  const name = path.basename(OUT_ROOT);
  await fs.mkdir(parent, { recursive: true });

  if (await pathExists(OUT_ROOT)) {
    const metadata = await fs.lstat(OUT_ROOT);
    if (!metadata.isDirectory() || metadata.isSymbolicLink()) {
      throw new Error(`Quickplay output must be a real directory: ${OUT_ROOT}`);
    }
  }
  const stagingRootInput = path.resolve(
    process.env.BLOCKKART_QUICKPLAY_STAGING_ROOT
      ?? path.join(
        VOLANG_ROOT,
        'target',
        'quickplay-package-staging',
        createHash('sha256').update(OUT_ROOT).digest('hex').slice(0, 16),
      ),
  );
  await fs.mkdir(stagingRootInput, { recursive: true });
  const [canonicalParent, stagingRoot] = await Promise.all([
    fs.realpath(parent),
    fs.realpath(stagingRootInput),
  ]);
  const canonicalOutRoot = path.join(canonicalParent, name);
  if (pathContains(canonicalOutRoot, stagingRoot)) {
    throw new Error(`Quickplay staging must be outside the output tree ${canonicalOutRoot}`);
  }
  const [outputParentMetadata, stagingMetadata] = await Promise.all([
    fs.stat(canonicalParent),
    fs.stat(stagingRoot),
  ]);
  if (outputParentMetadata.dev !== stagingMetadata.dev) {
    throw new Error(
      `Quickplay staging must share a filesystem with ${parent}; `
      + `set BLOCKKART_QUICKPLAY_STAGING_ROOT to an ignored directory on that filesystem`,
    );
  }
  await recoverInterruptedQuickplayOutput(stagingRoot, name);
  const pending = await fs.mkdtemp(path.join(stagingRoot, `.${name}.pending-`));
  const previous = path.join(
    stagingRoot,
    `.${name}.previous-${String(Date.now()).padStart(13, '0')}-${process.pid}-${randomUUID()}`,
  );
  return { pending, previous };
}

async function publishAtomicOutput({ pending, previous }) {
  let retainedPrevious = false;
  if (await pathExists(OUT_ROOT)) {
    await fs.rename(OUT_ROOT, previous);
    retainedPrevious = true;
  }
  try {
    await fs.rename(pending, OUT_ROOT);
  } catch (error) {
    if (retainedPrevious && !(await pathExists(OUT_ROOT))) {
      await fs.rename(previous, OUT_ROOT);
    } else if (retainedPrevious) {
      await fs.rm(previous, { recursive: true, force: true });
    }
    throw error;
  }
  if (retainedPrevious) {
    try {
      await fs.rm(previous, { recursive: true, force: true });
    } catch (error) {
      // The new directory is already authoritative. A retained previous tree
      // is harmless and prepareAtomicOutput removes it on the next run.
      console.warn(
        `Quickplay output committed, but the previous tree could not be removed: `
        + `${error instanceof Error ? error.message : String(error)}`,
      );
    }
  }
}

async function validateStagedQuickplayPackage() {
  const reportParent = path.join(VOLANG_ROOT, 'target');
  await fs.mkdir(reportParent, { recursive: true });
  const reportDir = await fs.mkdtemp(path.join(reportParent, '.quickplay-staged-validate-'));
  try {
    execFileSync(process.execPath, [path.join(VOLANG_ROOT, 'scripts/ci/quickplay_validate.mjs')], {
      cwd: VOLANG_ROOT,
      env: {
        ...process.env,
        QUICKPLAY_DIR: activeOutRoot,
        QUICKPLAY_VALIDATE_OUT_DIR: reportDir,
      },
      encoding: 'utf8',
      maxBuffer: 20 * 1024 * 1024,
      stdio: ['ignore', 'pipe', 'pipe'],
    });
  } catch (error) {
    const stdout = String(error?.stdout ?? '').trim();
    const stderr = String(error?.stderr ?? '').trim();
    throw new Error(
      `Staged Quickplay validation failed${stdout || stderr ? `:\n${[stdout, stderr].filter(Boolean).join('\n')}` : ''}`,
    );
  } finally {
    await fs.rm(reportDir, { recursive: true, force: true });
  }
}

async function main() {
  const atomicOutput = await prepareAtomicOutput();
  activeOutRoot = atomicOutput.pending;
  let published = false;
  try {
    const lockPath = path.join(BLOCKKART_ROOT, 'vo.lock');
    const lockModules = parseLockFile(
      decodeModuleTextUtf8(
        await readRegularFileLimited(lockPath, 'BlockKart vo.lock', QUICKPLAY_MAX_METADATA_BYTES),
        'BlockKart vo.lock',
      ),
    );
    if (lockModules.length === 0) {
      throw new Error(`No resolved dependencies found in ${lockPath}`);
    }

    const projectPackage = await buildProjectPackage();
    const dependencyPackage = await buildDependencyPackage(lockModules);
    rewriteProjectLockForPackagedDependencies(projectPackage, dependencyPackage, lockModules);
    const projectPayloadBytes = assertBrowserStaticFileBudget(
      projectPackage.files,
      'BlockKart project package',
      QUICKPLAY_PROJECT_VFS_ROOT,
    );
    const dependencyBudget = assertBrowserDependencyBudget(dependencyPackage);
    const combinedFiles = projectPackage.files.length + dependencyBudget.files;
    const combinedBytes = projectPayloadBytes + dependencyBudget.bytes;
    if (
      !Number.isSafeInteger(combinedFiles)
      || !Number.isSafeInteger(combinedBytes)
      || combinedFiles > QUICKPLAY_MAX_FILES
      || combinedBytes > QUICKPLAY_MAX_TOTAL_BYTES
    ) {
      throw new Error('Combined BlockKart project and dependency payload exceeds the browser VFS package budget');
    }
    const outputBytes = {
      project: Buffer.from(jsonText(projectPackage), 'utf8'),
      deps: Buffer.from(jsonText(dependencyPackage), 'utf8'),
    };
    for (const [name, bytes] of Object.entries(outputBytes)) {
      if (bytes.byteLength > QUICKPLAY_MAX_JSON_BYTES) {
        throw new Error(`BlockKart ${name}.json exceeds the ${QUICKPLAY_MAX_JSON_BYTES}-byte browser limit`);
      }
    }

    await fs.writeFile(path.join(activeOutRoot, 'project.json'), outputBytes.project);
    await fs.writeFile(path.join(activeOutRoot, 'deps.json'), outputBytes.deps);
    const provenance = await buildProvenance(projectPackage, dependencyPackage, outputBytes);
    const provenanceBytes = Buffer.from(jsonText(provenance), 'utf8');
    if (provenanceBytes.byteLength > QUICKPLAY_MAX_METADATA_BYTES) {
      throw new Error(`BlockKart provenance exceeds the ${QUICKPLAY_MAX_METADATA_BYTES}-byte metadata limit`);
    }
    await fs.writeFile(path.join(activeOutRoot, 'provenance.json'), provenanceBytes);

    await validateStagedQuickplayPackage();
    await publishAtomicOutput(atomicOutput);
    published = true;

    console.log(`BlockKart quickplay package written to ${OUT_ROOT}`);
    console.log(`project json: ${projectPackage.files.length} files, ${outputBytes.project.byteLength} bytes`);
    console.log(`deps json: ${dependencyPackage.modules.length} modules, ${outputBytes.deps.byteLength} bytes`);
    console.log(`provenance: BlockKart ${projectPackage.commit}, ${dependencyPackage.modules.length} modules`);
  } finally {
    activeOutRoot = OUT_ROOT;
    if (!published) {
      await fs.rm(atomicOutput.pending, { recursive: true, force: true });
    }
  }
}

main().catch((error) => {
  console.error(error instanceof Error ? error.message : String(error));
  process.exitCode = 1;
});
