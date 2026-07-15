#!/usr/bin/env node
import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import {
  lstatSync,
  mkdirSync,
  readFileSync,
  readdirSync,
  realpathSync,
  rmSync,
  writeFileSync,
} from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  artifactCachePath,
  moduleCacheDir,
  validateArtifactIdentity,
  validatePortableComponent,
  validatePortableRelativePath,
} from './quickplay_artifact_paths.mjs';
import { canonicalExistingDirectory, requireRepoRoot } from './repo_roots.mjs';
import {
  parseBoundedStrictJsonBytes,
  validatePortablePathComponent,
  validateWebManifestVoModContract,
} from './quickplay_web_manifest_contract.mjs';
import {
  decodeModuleTextUtf8,
  selectLockedModuleSnapshotForCache,
} from './vo_lock_v2.mjs';

const MAX_GIT_ARCHIVE_BYTES = 512 * 1024 * 1024;
const MAX_TEXT_FILE_BYTES = 16 * 1024 * 1024;
const MAX_SOURCE_BYTES = 64 * 1024 * 1024;
const ARTIFACT_DOWNLOAD_TIMEOUT_MS = 60_000;
const MAX_EXPORTED_ENTRIES = 100_000;
const MAX_EXPORTED_DEPTH = 256;
const WASM_TARGET = 'wasm32-unknown-unknown';

const root = fileURLToPath(new URL('../..', import.meta.url));
const workRoot = path.join(root, 'target', 'quickplay-module-cache');
const cacheRoot = path.resolve(process.env.VO_MOD_CACHE ?? path.join(workRoot, 'mod'));
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const repoRoots = new Map([
  ['github.com/vo-lang/vogui', requireRepoRoot('VOGUI_ROOT', 'vogui')],
  ['github.com/vo-lang/vopack', requireRepoRoot('VOPACK_ROOT', 'vopack')],
  ['github.com/vo-lang/voplay', requireRepoRoot('VOPLAY_ROOT', 'voplay')],
]);
const localReleaseAssetRoot = process.env.QUICKPLAY_RELEASE_ASSET_ROOT
  ? canonicalExistingDirectory(
      process.env.QUICKPLAY_RELEASE_ASSET_ROOT,
      'QUICKPLAY_RELEASE_ASSET_ROOT',
    )
  : null;

if (cacheRoot === workRoot || cacheRoot.startsWith(`${workRoot}${path.sep}`)) {
  rmSync(workRoot, { recursive: true, force: true });
}
mkdirSync(cacheRoot, { recursive: true });

const lockSource = decodeModuleTextUtf8(
  readFileSync(path.join(blockKartRoot, 'vo.lock')),
  'BlockKart vo.lock',
);

function lockedModule(module) {
  const locked = selectLockedModuleSnapshotForCache(
    lockSource,
    module,
    'BlockKart vo.lock',
  );
  const { version, commit } = locked;
  if (!version || !/^[0-9a-f]{40}$/.test(commit ?? '')) {
    throw new Error(`BlockKart vo.lock has incomplete version/commit metadata for ${module}`);
  }
  const { artifacts } = locked;
  for (const artifact of artifacts) {
    validateArtifactIdentity(artifact);
    if (!Number.isSafeInteger(artifact.size) || artifact.size < 0) {
      throw new Error(`BlockKart vo.lock has invalid artifact size for ${module}`);
    }
    if (!/^sha256:[0-9a-f]{64}$/.test(artifact.digest ?? '')) {
      throw new Error(`BlockKart vo.lock has invalid artifact digest for ${module}`);
    }
  }
  return locked;
}

function commandFailure(command, args, result) {
  const detail = result.error?.message
    || result.stderr?.toString('utf8').trim()
    || `status ${result.status}`;
  return new Error(`${command} ${args.join(' ')} failed: ${detail}`);
}

function exportCommit(repoRoot, commit, destination) {
  const verifyArgs = ['cat-file', '-e', `${commit}^{commit}`];
  const verify = spawnSync('git', verifyArgs, { cwd: repoRoot, encoding: 'utf8' });
  if (verify.error || verify.status !== 0) {
    throw commandFailure('git', verifyArgs, verify);
  }

  const archiveArgs = ['archive', '--format=tar', commit];
  const archive = spawnSync('git', archiveArgs, {
    cwd: repoRoot,
    encoding: null,
    maxBuffer: MAX_GIT_ARCHIVE_BYTES,
  });
  if (archive.error || archive.status !== 0) {
    throw commandFailure('git', archiveArgs, archive);
  }

  rmSync(destination, { recursive: true, force: true });
  mkdirSync(destination, { recursive: true });
  const extractArgs = ['-xf', '-', '-C', destination];
  const extract = spawnSync('tar', extractArgs, {
    input: archive.stdout,
    encoding: null,
    maxBuffer: MAX_GIT_ARCHIVE_BYTES,
  });
  if (extract.error || extract.status !== 0) {
    rmSync(destination, { recursive: true, force: true });
    throw commandFailure('tar', extractArgs, extract);
  }
}

function auditExportedTree(rootDir, dir = rootDir, depth = 0, state = { entries: 0 }) {
  if (depth > MAX_EXPORTED_DEPTH) {
    throw new Error(`exported module tree exceeds depth ${MAX_EXPORTED_DEPTH}: ${dir}`);
  }
  const entries = readdirSync(dir, { withFileTypes: true });
  state.entries += entries.length;
  if (state.entries > MAX_EXPORTED_ENTRIES) {
    throw new Error(`exported module tree exceeds ${MAX_EXPORTED_ENTRIES} entries: ${rootDir}`);
  }
  for (const entry of entries) {
    const absolute = path.join(dir, entry.name);
    const metadata = lstatSync(absolute);
    if (metadata.isSymbolicLink()) {
      throw new Error(`exported module tree contains a symbolic link: ${absolute}`);
    }
    if (metadata.isDirectory()) {
      auditExportedTree(rootDir, absolute, depth + 1, state);
    } else if (!metadata.isFile()) {
      throw new Error(`exported module tree contains a non-regular entry: ${absolute}`);
    }
  }
}

function sha256(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function containedPath(rootDir, relative) {
  const components = validatePortableRelativePath(relative, 'vo.web.json artifact path');
  const candidate = path.resolve(rootDir, ...components);
  const normalizedRoot = path.resolve(rootDir);
  if (candidate !== normalizedRoot && !candidate.startsWith(`${normalizedRoot}${path.sep}`)) {
    throw new Error(`artifact path escapes exported module: ${relative}`);
  }
  return candidate;
}

function releaseAssetUrl(locked, assetName) {
  const repo = locked.module.split('/').slice(1).map(encodeURIComponent).join('/');
  return `https://github.com/${repo}/releases/download/${encodeURIComponent(locked.version)}/${encodeURIComponent(assetName)}`;
}

function localReleaseAssetBytes(locked, assetName, maxBytes) {
  validatePortableComponent(assetName, `${locked.module} release asset name`);
  const candidate = path.resolve(
    localReleaseAssetRoot,
    ...locked.module.split('/'),
    locked.version,
    assetName,
  );
  if (!candidate.startsWith(`${localReleaseAssetRoot}${path.sep}`)) {
    throw new Error(`${locked.module} local release asset escapes QUICKPLAY_RELEASE_ASSET_ROOT`);
  }
  let canonical;
  try {
    canonical = realpathSync.native(candidate);
  } catch (error) {
    throw new Error(`${locked.module} release asset ${assetName} is unavailable: ${error.message}`);
  }
  if (canonical !== candidate) {
    throw new Error(`${locked.module} release asset ${assetName} must not traverse a symbolic link`);
  }
  const before = lstatSync(canonical);
  if (!before.isFile() || before.isSymbolicLink()) {
    throw new Error(`${locked.module} release asset ${assetName} must be a regular file`);
  }
  if (!Number.isSafeInteger(before.size) || before.size < 0 || before.size > maxBytes) {
    throw new Error(`${locked.module} release asset ${assetName} exceeds its ${maxBytes}-byte limit`);
  }
  const bytes = readFileSync(canonical);
  const after = lstatSync(canonical);
  if (
    before.dev !== after.dev
    || before.ino !== after.ino
    || before.size !== after.size
    || before.mtimeMs !== after.mtimeMs
    || before.ctimeMs !== after.ctimeMs
    || bytes.byteLength !== before.size
  ) {
    throw new Error(`${locked.module} release asset ${assetName} changed while being read`);
  }
  return bytes;
}

async function downloadLockedAsset(
  locked,
  assetName,
  { digest, size = null, maxBytes = MAX_GIT_ARCHIVE_BYTES },
) {
  if (!/^sha256:[0-9a-f]{64}$/.test(digest ?? '')) {
    throw new Error(`${locked.module} release asset ${assetName} has no canonical expected digest`);
  }
  if (
    !Number.isSafeInteger(maxBytes)
    || maxBytes <= 0
    || (size !== null && (!Number.isSafeInteger(size) || size < 0 || size > maxBytes))
  ) {
    throw new Error(`${locked.module} release asset ${assetName} has invalid size bounds`);
  }
  if (localReleaseAssetRoot !== null) {
    const bytes = localReleaseAssetBytes(locked, assetName, maxBytes);
    if ((size !== null && bytes.byteLength !== size) || sha256(bytes) !== digest) {
      throw new Error(`${locked.module} local release asset does not match authenticated metadata: ${assetName}`);
    }
    return bytes;
  }

  const url = releaseAssetUrl(locked, assetName);
  const response = await fetch(url, {
    redirect: 'follow',
    signal: AbortSignal.timeout(ARTIFACT_DOWNLOAD_TIMEOUT_MS),
  });
  if (!response.ok || !response.body) {
    throw new Error(`failed to download ${locked.module} release asset ${assetName}: HTTP ${response.status}`);
  }
  const contentLength = response.headers.get('content-length');
  if (contentLength != null) {
    const declared = Number(contentLength);
    if (!Number.isSafeInteger(declared) || declared < 0 || declared > maxBytes) {
      throw new Error(`${locked.module} release asset ${assetName} has invalid Content-Length`);
    }
    if (size !== null && declared !== size) {
      throw new Error(`${locked.module} release asset ${assetName} Content-Length does not match authenticated metadata`);
    }
  }
  const chunks = [];
  let total = 0;
  for await (const chunk of response.body) {
    const bytes = Buffer.from(chunk);
    total += bytes.byteLength;
    if (!Number.isSafeInteger(total) || total > maxBytes || (size !== null && total > size)) {
      throw new Error(`${locked.module} release asset ${assetName} exceeds its authenticated size bound`);
    }
    chunks.push(bytes);
  }
  const bytes = Buffer.concat(chunks, total);
  if ((size !== null && bytes.byteLength !== size) || sha256(bytes) !== digest) {
    throw new Error(`${locked.module} downloaded release asset does not match authenticated metadata: ${assetName}`);
  }
  return bytes;
}

function parseJsonAsset(bytes, label) {
  const value = parseBoundedStrictJsonBytes(bytes, label, {
    maxBytes: MAX_TEXT_FILE_BYTES,
  });
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new Error(`${label} must contain a JSON object`);
  }
  return value;
}

function canonicalSourceSetDigest(entries) {
  const canonical = entries
    .map((entry) => ({ path: entry.path, size: entry.size, digest: entry.digest }))
    .sort((left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path)));
  return sha256(Buffer.from(JSON.stringify(canonical), 'utf8'));
}

function assertExactObjectKeys(value, required, optional, label) {
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  const allowed = new Set([...required, ...optional]);
  for (const key of Object.keys(value)) {
    if (!allowed.has(key)) throw new Error(`${label} contains unsupported field ${key}`);
  }
  for (const key of required) {
    if (!Object.hasOwn(value, key)) throw new Error(`${label} is missing required field ${key}`);
  }
}

function normalizedReleaseArtifacts(value, label) {
  if (!Array.isArray(value) || value.length > MAX_EXPORTED_ENTRIES) {
    throw new Error(`${label} must be a bounded array`);
  }
  const normalized = value.map((artifact, index) => {
    const artifactLabel = `${label}[${index}]`;
    assertExactObjectKeys(
      artifact,
      ['kind', 'target', 'name', 'size', 'digest'],
      [],
      artifactLabel,
    );
    const identity = validateArtifactIdentity(artifact);
    if (
      !Number.isSafeInteger(artifact.size)
      || artifact.size < 0
      || artifact.size > MAX_GIT_ARCHIVE_BYTES
      || !/^sha256:[0-9a-f]{64}$/.test(artifact.digest ?? '')
    ) {
      throw new Error(`${artifactLabel} has invalid artifact metadata`);
    }
    return { ...identity, size: artifact.size, digest: artifact.digest };
  });
  for (let index = 1; index < normalized.length; index += 1) {
    const previous = `${normalized[index - 1].kind}\0${normalized[index - 1].target}\0${normalized[index - 1].name}`;
    const current = `${normalized[index].kind}\0${normalized[index].target}\0${normalized[index].name}`;
    if (Buffer.compare(Buffer.from(previous), Buffer.from(current)) >= 0) {
      throw new Error(`${label} must be unique and sorted by artifact identity`);
    }
  }
  return normalized;
}

function exportedSourceBytes(moduleDir, relative, label) {
  const source = containedPath(moduleDir, relative);
  const before = lstatSync(source);
  if (
    !before.isFile()
    || before.isSymbolicLink()
    || !Number.isSafeInteger(before.size)
    || before.size < 0
    || before.size > MAX_TEXT_FILE_BYTES
  ) {
    throw new Error(`${label} must be a regular file within the source-file size limit`);
  }
  const canonicalModule = realpathSync.native(moduleDir);
  const canonicalSource = realpathSync.native(source);
  if (!canonicalSource.startsWith(`${canonicalModule}${path.sep}`)) {
    throw new Error(`${label} resolves outside the exported module`);
  }
  const bytes = readFileSync(source);
  const after = lstatSync(source);
  if (
    before.dev !== after.dev
    || before.ino !== after.ino
    || before.size !== after.size
    || before.mtimeMs !== after.mtimeMs
    || before.ctimeMs !== after.ctimeMs
    || bytes.byteLength !== before.size
  ) {
    throw new Error(`${label} changed while being authenticated`);
  }
  return bytes;
}

function validateCurrentReleaseContract(release, web, locked, moduleDir) {
  const releaseLabel = `${locked.module}@${locked.version} vo.release.json`;
  assertExactObjectKeys(
    release,
    ['schema_version', 'module', 'version', 'commit', 'module_root', 'vo', 'require', 'source', 'web_manifest'],
    ['artifacts'],
    releaseLabel,
  );
  if (
    release.schema_version !== 1
    || release.module !== locked.module
    || release.version !== locked.version
    || release.commit !== locked.commit
    || release.source?.digest !== locked.source
  ) {
    throw new Error(`${locked.module}@${locked.version} vo.release.json identity does not match BlockKart vo.lock`);
  }
  const currentSource = release.source
    && typeof release.source.name === 'string'
    && release.source.name.length > 0
    && Number.isSafeInteger(release.source.size)
    && release.source.size > 0
    && release.source.size <= MAX_GIT_ARCHIVE_BYTES
    && release.source.digest === locked.source
    && Number.isSafeInteger(release.source.files_size)
    && release.source.files_size > 0
    && release.source.files_size <= MAX_SOURCE_BYTES
    && /^sha256:[0-9a-f]{64}$/.test(release.source.files_digest ?? '');
  const currentWebBinding = release.web_manifest
    && Number.isSafeInteger(release.web_manifest.size)
    && release.web_manifest.size > 0
    && release.web_manifest.size <= MAX_TEXT_FILE_BYTES
    && /^sha256:[0-9a-f]{64}$/.test(release.web_manifest.digest ?? '');
  if (!currentSource || !currentWebBinding) {
    throw new Error(
      `${locked.module}@${locked.version} uses a legacy release contract without `
      + 'source.files_size, source.files_digest, and web_manifest bindings; '
      + 'publish a current-protocol release and update BlockKart vo.lock',
    );
  }
  assertExactObjectKeys(
    release.source,
    ['name', 'size', 'digest', 'files_size', 'files_digest'],
    [],
    `${releaseLabel} source`,
  );
  assertExactObjectKeys(
    release.web_manifest,
    ['size', 'digest'],
    [],
    `${releaseLabel} web_manifest`,
  );
  validatePortablePathComponent(release.source.name, `${releaseLabel} source.name`);
  const voModBytes = exportedSourceBytes(moduleDir, 'vo.mod', `${locked.module} vo.mod`);
  const voModSource = decodeModuleTextUtf8(voModBytes, `${locked.module} vo.mod`);
  const contract = validateWebManifestVoModContract(
    web,
    voModSource,
    `${locked.module}@${locked.version} browser release contract`,
    {
      expectedModule: locked.module,
      expectedVersion: locked.version,
      expectedCommit: locked.commit,
      expectedVo: locked.vo,
      sourceBytes: (relative) => exportedSourceBytes(
        moduleDir,
        relative,
        `${locked.module} source ${relative}`,
      ),
    },
  );
  const filesSize = contract.manifest.source.reduce((total, entry) => total + entry.size, 0);
  if (
    canonicalSourceSetDigest(contract.manifest.source) !== contract.manifest.source_digest
    || contract.manifest.source_digest !== release.source.files_digest
    || filesSize !== release.source.files_size
    || release.module_root !== contract.manifest.module_root
    || release.vo !== contract.manifest.vo
    || JSON.stringify(release.require) !== JSON.stringify(contract.manifest.require)
  ) {
    throw new Error(`${locked.module}@${locked.version} release and web source-set bindings disagree`);
  }
  const releaseArtifacts = normalizedReleaseArtifacts(
    release.artifacts ?? [],
    `${releaseLabel} artifacts`,
  );
  const lockedArtifacts = normalizedReleaseArtifacts(
    locked.artifacts,
    `${locked.module} BlockKart vo.lock artifacts`,
  );
  const declaredKeys = contract.metadata.declaredArtifacts
    .map((artifact) => `${artifact.kind}\0${artifact.target}\0${artifact.name}`);
  const releaseKeys = releaseArtifacts
    .map((artifact) => `${artifact.kind}\0${artifact.target}\0${artifact.name}`);
  const webArtifacts = contract.manifest.artifacts.map((artifact) => ({
    kind: artifact.kind,
    target: artifact.target,
    name: artifact.name,
    size: artifact.size,
    digest: artifact.digest,
  }));
  if (
    JSON.stringify(releaseArtifacts) !== JSON.stringify(lockedArtifacts)
    || JSON.stringify(releaseKeys) !== JSON.stringify(declaredKeys)
    || JSON.stringify(releaseArtifacts.filter((artifact) => artifact.kind !== 'extension-native'))
      !== JSON.stringify(webArtifacts)
  ) {
    throw new Error(`${locked.module}@${locked.version} artifact set disagrees across release, web manifest, and vo.lock`);
  }
  return contract.manifest;
}

async function installPublishedReleaseControls(moduleDir, locked) {
  const releaseBytes = await downloadLockedAsset(locked, 'vo.release.json', {
    digest: locked.release_manifest,
    maxBytes: MAX_TEXT_FILE_BYTES,
  });
  const release = parseJsonAsset(
    releaseBytes,
    `${locked.module}@${locked.version} vo.release.json`,
  );
  if (
    release.schema_version !== 1
    || release.module !== locked.module
    || release.version !== locked.version
    || release.commit !== locked.commit
    || release.source?.digest !== locked.source
  ) {
    throw new Error(`${locked.module}@${locked.version} vo.release.json identity does not match BlockKart vo.lock`);
  }
  if (
    !release.web_manifest
    || !Number.isSafeInteger(release.web_manifest.size)
    || release.web_manifest.size <= 0
    || release.web_manifest.size > MAX_TEXT_FILE_BYTES
    || !/^sha256:[0-9a-f]{64}$/.test(release.web_manifest.digest ?? '')
  ) {
    throw new Error(
      `${locked.module}@${locked.version} uses a legacy release contract without a valid web_manifest binding; `
      + 'publish a current-protocol release and update BlockKart vo.lock',
    );
  }
  const webBytes = await downloadLockedAsset(locked, 'vo.web.json', {
    digest: release.web_manifest.digest,
    size: release.web_manifest.size,
    maxBytes: MAX_TEXT_FILE_BYTES,
  });
  const web = parseJsonAsset(webBytes, `${locked.module}@${locked.version} vo.web.json`);
  const manifest = validateCurrentReleaseContract(release, web, locked, moduleDir);

  writeFileSync(path.join(moduleDir, 'vo.release.json'), releaseBytes);
  writeFileSync(path.join(moduleDir, 'vo.web.json'), webBytes);
  writeFileSync(path.join(moduleDir, '.vo-source-digest'), `${locked.source}\n`);
  writeFileSync(path.join(moduleDir, '.vo-version'), `${locked.version}\n`);
  return manifest;
}

async function installBrowserArtifacts(moduleDir, locked, web) {
  if (!Array.isArray(web.artifacts)) {
    throw new Error(`${locked.module} vo.web.json artifacts must be an array`);
  }
  const declared = new Map();
  for (const artifact of web.artifacts) {
    const id = validateArtifactIdentity(artifact);
    const key = `${id.kind}\0${id.target}\0${id.name}`;
    if (declared.has(key)) throw new Error(`${locked.module} vo.web.json has duplicate artifact ${key}`);
    declared.set(key, artifact);
  }
  const sourceByPath = new Map(web.source.map((entry) => [entry.path, entry]));

  for (const artifact of locked.artifacts) {
    if (artifact.target !== WASM_TARGET) continue;
    if (artifact.kind !== 'extension-wasm' && artifact.kind !== 'extension-js-glue') continue;
    const key = `${artifact.kind}\0${artifact.target}\0${artifact.name}`;
    const webArtifact = declared.get(key);
    if (
      !webArtifact
      || typeof webArtifact.path !== 'string'
      || webArtifact.size !== artifact.size
      || webArtifact.digest !== artifact.digest
    ) {
      throw new Error(`${locked.module} vo.web.json does not declare locked artifact ${key}`);
    }
    const source = containedPath(moduleDir, webArtifact.path);
    const sourceEntry = sourceByPath.get(webArtifact.path);
    if (
      sourceEntry
      && (sourceEntry.size !== artifact.size || sourceEntry.digest !== artifact.digest)
    ) {
      throw new Error(`${locked.module} artifact ${artifact.name} disagrees with its source entry`);
    }
    let committedBytes = null;
    try {
      const metadata = lstatSync(source);
      if (metadata.isFile() && !metadata.isSymbolicLink() && metadata.size <= MAX_GIT_ARCHIVE_BYTES) {
        committedBytes = readFileSync(source);
      }
    } catch {
      committedBytes = null;
    }
    const bytes = committedBytes
      && committedBytes.byteLength === artifact.size
      && sha256(committedBytes) === artifact.digest
      ? committedBytes
      : await downloadLockedAsset(locked, artifact.name, {
          digest: artifact.digest,
          size: artifact.size,
          maxBytes: MAX_GIT_ARCHIVE_BYTES,
        });
    const destination = path.join(moduleDir, ...artifactCachePath(artifact).split('/'));
    mkdirSync(path.dirname(source), { recursive: true });
    mkdirSync(path.dirname(destination), { recursive: true });
    writeFileSync(source, bytes);
    writeFileSync(destination, bytes);
  }
}

async function installLockedBrowserSnapshot(locked, repoRoot) {
  const cacheDir = path.join(cacheRoot, ...moduleCacheDir(locked.module, locked.version).split('/'));
  exportCommit(repoRoot, locked.commit, cacheDir);
  auditExportedTree(cacheDir);

  const declaredModule = decodeModuleTextUtf8(
    readFileSync(path.join(cacheDir, 'vo.mod')),
    `${locked.module} vo.mod`,
  )
    .match(/^module\s+(\S+)$/m)?.[1];
  if (declaredModule !== locked.module) {
    throw new Error(`${repoRoot} commit ${locked.commit} declares ${declaredModule ?? 'no module'}`);
  }
  for (const protocolPath of [
    '.vo-source-digest',
    '.vo-version',
    'vo.release.json',
    'vo.web.json',
  ]) {
    rmSync(path.join(cacheDir, protocolPath), { force: true });
  }
  const web = await installPublishedReleaseControls(cacheDir, locked);
  await installBrowserArtifacts(cacheDir, locked, web);
  auditExportedTree(cacheDir);
  console.log(`quickplay module snapshot: ${locked.module}@${locked.version} ${locked.commit}`);
}

for (const [module, repoRoot] of repoRoots) {
  await installLockedBrowserSnapshot(lockedModule(module), repoRoot);
}

console.log(`quickplay module cache: ok ${cacheRoot}`);
