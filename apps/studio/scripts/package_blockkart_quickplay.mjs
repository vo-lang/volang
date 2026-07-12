import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { promises as fs } from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { requireRepoRoot, requireVolangRoot } from '../../../scripts/ci/repo_roots.mjs';
import { verifyCurrentVoplayWasm } from '../../../scripts/ci/voplay_current_wasm.mjs';

const STUDIO_ROOT = path.resolve(new URL('..', import.meta.url).pathname);
const VOLANG_ROOT = requireVolangRoot(path.resolve(STUDIO_ROOT, '../..'));
const BLOCKKART_ROOT = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const MOD_CACHE_ROOT = path.resolve(
  process.env.VO_MOD_CACHE ?? path.join(VOLANG_ROOT, 'target', 'quickplay-module-cache', 'mod'),
);
const VOPLAY_ROOT = requireRepoRoot('VOPLAY_ROOT', 'voplay');
const VOPLAY_CURRENT_WASM_ROOT = path.resolve(
  process.env.VOPLAY_CURRENT_WASM_OUT_DIR ?? path.join(VOLANG_ROOT, 'target', 'voplay-current-wasm'),
);
const OUT_ROOT = path.resolve(process.env.BLOCKKART_QUICKPLAY_OUT_ROOT ?? path.join(STUDIO_ROOT, 'public', 'quickplay', 'blockkart'));
const WASM_TARGET = 'wasm32-unknown-unknown';
const BLOCKKART_RUNTIME_ASSETS = [
  'assets/blockkart.vpak',
  'assets/blockkart.vpak.provenance.json',
];
const BLOCKKART_SOURCE_ALLOWLIST = [
  {
    path: 'tools/pack_primitive_assets.vo',
    reason: 'Asset-pack generation tool; quickplay runtime embeds the generated assets/blockkart.vpak payload.',
    expiresAt: '2027-01-31T00:00:00.000Z',
  },
  {
    path: 'runtimepack/runtime_pack.vo',
    reason: 'Asset-pack schema package; quickplay runtime consumes its generated assets/blockkart.vpak payload.',
    expiresAt: '2027-01-31T00:00:00.000Z',
  },
];
const QUICKPLAY_ARTIFACT_NAME = 'studio.quickplay.blockkart';
const QUICKPLAY_ARTIFACT_PATH = 'apps/studio/public/quickplay/blockkart';
const QUICKPLAY_GENERATOR_VERSION = 8;
const QUICKPLAY_TASK_ID = 'quickplay-blockkart-package';
const QUICKPLAY_GENERATOR_COMMAND = ['vo-dev', 'task', 'run', 'task:quickplay-blockkart-package'];
const QUICKPLAY_SOURCE_ROOTS = {
  volang: '.',
  blockKart: 'external:BlockKart',
  voplay: 'first-party:voplay',
};
const QUICKPLAY_GENERATOR_INPUTS = [
  'apps/studio/scripts/package_blockkart_quickplay.mjs',
  'scripts/ci/voplay_current_wasm.mjs',
  'eng/project.toml',
  'external:BlockKart',
  'external:BlockKart/tools/pack_primitive_assets.vo',
  'external:BlockKart/tools/generate_primitive_terrain.mjs',
  'external:BlockKart/tools/paint_terrain_textures.mjs',
  'external:BlockKart/tools/vpak_provenance.mjs',
  'external:BlockKart/tools/terrain_heightfield_spec.mjs',
  'external:BlockKart/tools/terrain_recipe.mjs',
  'external:BlockKart/terrain/recipes/primitive_concept_v1.json',
  'first-party:voplay',
  'module-cache:vopack',
  'module-cache:vogui',
];
const CLEAN_DEPENDENCY_SNAPSHOTS = [];
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

function cacheKey(modulePath) {
  return modulePath.replaceAll('/', '@');
}

function gitOutput(args, cwd) {
  return execFileSync('git', args, { cwd, encoding: 'utf8' }).trim();
}

function gitStatus(cwd) {
  return gitOutput(['status', '--porcelain'], cwd);
}

async function cleanGitSnapshot(cwd, label) {
  const safeLabel = label.replaceAll(/[^A-Za-z0-9_.-]/g, '-');
  const tempDir = await fs.mkdtemp(path.join(os.tmpdir(), `${safeLabel}-git-head-`));
  const archivePath = path.join(tempDir, 'source.tar');
  execFileSync('git', ['archive', '--format=tar', '--output', archivePath, 'HEAD'], { cwd });
  execFileSync('tar', ['-xf', archivePath, '-C', tempDir]);
  await fs.rm(archivePath, { force: true });
  CLEAN_DEPENDENCY_SNAPSHOTS.push(tempDir);
  return tempDir;
}

async function cleanupCleanGitSnapshots() {
  for (const tempDir of CLEAN_DEPENDENCY_SNAPSHOTS.splice(0)) {
    await fs.rm(tempDir, { recursive: true, force: true });
  }
}

function sha256Digest(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

async function volangGeneratorSourceDigest() {
  const inputs = [
    'apps/studio/scripts/package_blockkart_quickplay.mjs',
    'eng/project.toml',
    'eng/tasks.toml',
    'eng/ci.toml',
    'scripts/ci/repo_roots.mjs',
  ];
  const entries = [];
  for (const relative of inputs) {
    const bytes = await fs.readFile(path.join(VOLANG_ROOT, relative));
    entries.push({
      digest: sha256Digest(bytes),
      path: relative,
      size: bytes.byteLength,
    });
  }
  return sourceSetDigest(entries.sort((a, b) => a.path.localeCompare(b.path)));
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
    .sort((a, b) => a.path.localeCompare(b.path));
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

async function walkFiles(root) {
  const out = [];
  async function walk(current) {
    const entries = await fs.readdir(current, { withFileTypes: true });
    for (const entry of entries) {
      const absolute = path.join(current, entry.name);
      if (entry.isDirectory()) {
        if (shouldSkipDependencyDirectory(entry.name)) {
          continue;
        }
        await walk(absolute);
      } else if (entry.isFile()) {
        out.push(absolute);
      }
    }
  }
  await walk(root);
  return out.sort();
}

function toPosixRelative(root, absolute) {
  return path.relative(root, absolute).split(path.sep).join('/');
}

async function digestBlockKartPath(relative) {
  const absolute = path.join(BLOCKKART_ROOT, relative);
  const bytes = await fs.readFile(absolute);
  return {
    digest: sha256Digest(bytes),
    path: relative,
    size: bytes.byteLength,
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
  return entries.sort((a, b) => a.path.localeCompare(b.path));
}

async function digestBlockKartDirectory(relativeDir) {
  const absoluteDir = path.join(BLOCKKART_ROOT, relativeDir);
  const files = await walkFiles(absoluteDir);
  return digestBlockKartPaths(files.map((absolute) => toPosixRelative(BLOCKKART_ROOT, absolute)));
}

function shouldSkipDependencyDirectory(name) {
  return name.startsWith('.') || name === 'target' || name === 'tmp_checks';
}

async function walkProjectSourceFiles(root) {
  const out = [];
  async function walk(current) {
    const entries = await fs.readdir(current, { withFileTypes: true });
    for (const entry of entries) {
      const absolute = path.join(current, entry.name);
      if (entry.isDirectory()) {
        if (entry.name === '.git' || entry.name === 'target' || entry.name === 'node_modules' || entry.name === 'tmp_checks') {
          continue;
        }
        await walk(absolute);
      } else if (entry.isFile()) {
        out.push(absolute);
      }
    }
  }
  await walk(root);
  return out.sort();
}

function parseLockFile(content) {
  const modules = [];
  let current = null;
  let artifact = null;

  for (const rawLine of content.split(/\r?\n/)) {
    const line = rawLine.trim();
    if (!line || line.startsWith('#')) continue;
    if (line === '[[resolved]]') {
      if (artifact && current) current.artifacts.push(artifact);
      artifact = null;
      current = { path: '', version: '', artifacts: [] };
      modules.push(current);
      continue;
    }
    if (line === '[[resolved.artifact]]') {
      if (!current) throw new Error('vo.lock artifact appears before a resolved module');
      if (artifact) current.artifacts.push(artifact);
      artifact = { kind: '', target: '', name: '' };
      continue;
    }
    const match = line.match(/^([A-Za-z0-9_.-]+)\s*=\s*(.+)$/);
    if (!match) continue;
    const [, key, value] = match;
    if (artifact) {
      artifact[key] = parseLockValue(value);
    } else if (current) {
      current[key] = parseLockValue(value);
    }
  }
  if (artifact && current) current.artifacts.push(artifact);
  return modules.filter((module) => module.path && module.version);
}

function parseLockValue(value) {
  if (value.startsWith('"') || value.startsWith('[')) {
    return JSON.parse(value);
  }
  if (/^\d+$/.test(value)) {
    return Number(value);
  }
  return value;
}

function formatLockValue(value) {
  if (Array.isArray(value)) return JSON.stringify(value);
  if (typeof value === 'number') return String(value);
  return JSON.stringify(String(value ?? ''));
}

function artifactKey(artifact) {
  return `${artifact.kind}\u0000${artifact.target}\u0000${artifact.name}`;
}

function fileEntry(files, filePath) {
  const file = files.find((entry) => entry.path === filePath);
  if (!file) {
    throw new Error(`Packaged module is missing ${filePath}`);
  }
  return file;
}

function fileText(files, filePath) {
  return packagedFileBytes(fileEntry(files, filePath)).toString('utf8');
}

function fileJson(files, filePath) {
  return JSON.parse(fileText(files, filePath));
}

function lockEntryForPackagedModule(module, originalLock) {
  const releaseFile = fileEntry(module.files, 'vo.release.json');
  const release = JSON.parse(packagedFileBytes(releaseFile).toString('utf8'));
  const web = fileJson(module.files, 'vo.web.json');
  const source = fileText(module.files, '.vo-source-digest').trim();
  const artifactsByKey = new Map((web.artifacts ?? []).map((artifact) => [artifactKey(artifact), artifact]));
  const artifacts = [];
  for (const artifact of module.artifacts ?? []) {
    const webArtifact = artifactsByKey.get(artifactKey(artifact));
    if (!webArtifact) {
      throw new Error(`Packaged ${module.module} artifact ${artifact.name} is missing from vo.web.json`);
    }
    artifacts.push({
      kind: artifact.kind,
      target: artifact.target,
      name: artifact.name,
      size: webArtifact.size,
      digest: webArtifact.digest,
    });
  }
  artifacts.sort((a, b) => artifactKey(a).localeCompare(artifactKey(b)));
  return {
    path: module.module,
    version: module.version,
    vo: originalLock?.vo ?? '^0.1.0',
    commit: release.commit ?? module.commit,
    release_manifest: sha256Digest(packagedFileBytes(releaseFile)),
    source,
    deps: Array.isArray(originalLock?.deps)
      ? originalLock.deps
      : (release.require ?? []).map((entry) => entry.module).sort(),
    artifacts,
  };
}

function serializeLockEntry(entry) {
  const lines = [
    '[[resolved]]',
    `path = ${formatLockValue(entry.path)}`,
    `version = ${formatLockValue(entry.version)}`,
    `vo = ${formatLockValue(entry.vo)}`,
    `commit = ${formatLockValue(entry.commit)}`,
    `release_manifest = ${formatLockValue(entry.release_manifest)}`,
    `source = ${formatLockValue(entry.source)}`,
    `deps = ${formatLockValue(entry.deps ?? [])}`,
  ];
  for (const artifact of entry.artifacts ?? []) {
    lines.push(
      '',
      '[[resolved.artifact]]',
      `kind = ${formatLockValue(artifact.kind)}`,
      `target = ${formatLockValue(artifact.target)}`,
      `name = ${formatLockValue(artifact.name)}`,
      `size = ${formatLockValue(artifact.size)}`,
      `digest = ${formatLockValue(artifact.digest)}`,
    );
  }
  return lines.join('\n');
}

function lockRewriteEvidence(entry) {
  return {
    module: entry.path,
    version: entry.version,
    commit: entry.commit,
    releaseManifestDigest: entry.release_manifest,
    sourceDigest: entry.source,
    artifacts: (entry.artifacts ?? []).map((artifact) => ({
      kind: artifact.kind,
      target: artifact.target,
      name: artifact.name,
      size: artifact.size,
      digest: artifact.digest,
    })),
  };
}

function replaceResolvedLockEntries(source, replacements) {
  const lines = source.split(/\r?\n/);
  const out = [];
  let block = null;

  function flushBlock() {
    if (!block) return;
    const pathLine = block.find((line) => line.trim().startsWith('path = '));
    const match = pathLine?.trim().match(/^path\s*=\s*"([^"]+)"$/);
    const replacement = match ? replacements.get(match[1]) : null;
    if (replacement) {
      out.push(serializeLockEntry(replacement));
    } else {
      out.push(block.join('\n').replace(/\n*$/, ''));
    }
    block = null;
  }

  for (const line of lines) {
    if (line.trim() === '[[resolved]]') {
      flushBlock();
      block = [line];
      continue;
    }
    if (block) {
      block.push(line);
    } else {
      out.push(line);
    }
  }
  flushBlock();
  return `${out.join('\n').replace(/\n{3,}/g, '\n\n').trimEnd()}\n`;
}

function rewriteProjectLockForPackagedDependencies(projectPackage, dependencyPackage, originalLocks) {
  const lockFile = projectPackage.files.find((file) => file.path === 'vo.lock');
  if (!lockFile?.content) {
    throw new Error('Project package is missing vo.lock');
  }
  const sourceLockDigest = sha256Digest(Buffer.from(lockFile.content, 'utf8'));
  const originalByPath = new Map(originalLocks.map((entry) => [entry.path, entry]));
  const replacements = new Map();
  for (const module of dependencyPackage.modules) {
    replacements.set(module.module, lockEntryForPackagedModule(module, originalByPath.get(module.module)));
  }
  lockFile.content = replaceResolvedLockEntries(lockFile.content, replacements);
  projectPackage.lockRewrite = {
    schemaVersion: 1,
    path: 'vo.lock',
    sourceDigest: sourceLockDigest,
    packagedDigest: sha256Digest(Buffer.from(lockFile.content, 'utf8')),
    modules: [...replacements.values()]
      .map(lockRewriteEvidence)
      .sort((a, b) => a.module.localeCompare(b.module)),
  };
}

function shouldPackageDependencyFile(relativePath) {
  if (
    relativePath === 'vo.mod'
    || relativePath === 'vo.lock'
    || relativePath === 'vo.release.json'
    || relativePath === 'vo.web.json'
    || relativePath === '.vo-version'
    || relativePath === '.vo-source-digest'
  ) {
    return true;
  }
  if (relativePath.endsWith('.vo')) {
    return true;
  }
  if (relativePath.startsWith('js/dist/')) {
    return true;
  }
  return false;
}

function shouldDeclarePackagedSourceFile(file) {
  if (file.content == null) return false;
  if (
    file.path === 'vo.web.json'
    || file.path === 'vo.release.json'
    || file.path === '.vo-version'
    || file.path === '.vo-source-digest'
  ) {
    return false;
  }
  return true;
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
  return require.sort((a, b) => a.module.localeCompare(b.module));
}

function packagedSourceEntry(file) {
  const bytes = Buffer.from(file.content, 'utf8');
  return {
    digest: sha256Digest(bytes),
    path: file.path,
    size: bytes.byteLength,
  };
}

function sourceSetDigest(entries) {
  return sha256Digest(Buffer.from(JSON.stringify(entries), 'utf8'));
}

async function sourceFileEntry(root, absolute) {
  const bytes = await fs.readFile(absolute);
  return {
    path: path.relative(root, absolute).split(path.sep).join('/'),
    digest: sha256Digest(bytes),
    size: bytes.byteLength,
  };
}

async function blockKartSourceFiles() {
  const files = [];
  for (const absolute of await walkProjectSourceFiles(BLOCKKART_ROOT)) {
    if (!absolute.endsWith('.vo')) continue;
    files.push(await sourceFileEntry(BLOCKKART_ROOT, absolute));
  }
  files.sort((a, b) => a.path.localeCompare(b.path));
  return files;
}

async function blockKartSourceAllowlist() {
  const allowlist = [];
  for (const entry of BLOCKKART_SOURCE_ALLOWLIST) {
    const absolute = path.join(BLOCKKART_ROOT, entry.path);
    if (await pathExists(absolute)) {
      allowlist.push(entry);
    }
  }
  allowlist.sort((a, b) => a.path.localeCompare(b.path));
  return allowlist;
}

function syntheticBrowserReleaseManifest(manifest) {
  const artifacts = (manifest.artifacts ?? [])
    .map((artifact) => ({
      kind: artifact.kind,
      target: artifact.target,
      name: artifact.name,
      size: artifact.size,
      digest: artifact.digest,
    }))
    .sort((a, b) => (
      a.kind.localeCompare(b.kind)
      || a.target.localeCompare(b.target)
      || a.name.localeCompare(b.name)
    ));
  const require = (manifest.require ?? [])
    .map((entry) => ({ module: entry.module, constraint: entry.constraint }))
    .sort((a, b) => a.module.localeCompare(b.module));
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
    },
    artifacts,
  };
}

async function rewritePackagedWebManifest(moduleDir, files, artifacts, locked) {
  const manifestFile = files.find((file) => file.path === 'vo.web.json');
  if (!manifestFile) return;

  // Fully installed release modules carry vo.release.json/.vo-source-digest and
  // should stay byte-for-byte published. Web-only local quickplay snapshots do
  // not have those release markers, so their vo.web.json must describe the
  // exact packaged VFS payload or browser runtime integrity checks fail later.
  if (files.some((file) => file.path === 'vo.release.json')) return;

  const modFile = files.find((file) => file.path === 'vo.mod');
  if (modFile) {
    modFile.content = dropNativeExtensionTables(modFile.content);
  }

  const manifest = JSON.parse(manifestFile.content);
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
    .sort((a, b) => a.path.localeCompare(b.path));
  manifest.source = source;
  manifest.source_digest = sourceSetDigest(source);

  if (Array.isArray(manifest.artifacts) && artifacts.length > 0) {
    const packagedArtifactPaths = new Map(artifacts.map((artifact) => [path.posix.basename(artifact.path), artifact.path]));
    const nextArtifacts = [];
    for (const artifact of manifest.artifacts) {
      const packagedPath = packagedArtifactPaths.get(artifact.name);
      if (!packagedPath) {
        continue;
      }
      const artifactInfo = artifacts.find((entry) => path.posix.basename(entry.path) === artifact.name);
      const bytes = await fs.readFile(artifactInfo?.sourcePath ?? path.join(moduleDir, packagedPath));
      nextArtifacts.push({
        ...artifact,
        digest: sha256Digest(bytes),
        path: packagedPath,
        size: bytes.byteLength,
      });
    }
    manifest.artifacts = nextArtifacts;
  }

  manifestFile.content = `${JSON.stringify(manifest, null, 2)}\n`;
  upsertTextFile(files, '.vo-version', `${locked.version}\n`);
  upsertTextFile(files, '.vo-source-digest', `${manifest.source_digest}\n`);
  upsertTextFile(files, 'vo.release.json', `${JSON.stringify(syntheticBrowserReleaseManifest(manifest), null, 2)}\n`);
}

async function dependencyModuleDir(locked) {
  if (locked.path === 'github.com/vo-lang/voplay' && await pathExists(VOPLAY_ROOT)) {
    const head = gitOutput(['rev-parse', 'HEAD'], VOPLAY_ROOT);
    const clean = gitStatus(VOPLAY_ROOT) === '';
    return {
      cacheDir: `${cacheKey(locked.path)}/${locked.version}`,
      commit: head,
      dirty: !clean,
      moduleDir: VOPLAY_ROOT,
      source: clean ? 'external' : 'external-working-tree',
    };
  }
  const key = cacheKey(locked.path);
  const moduleDir = path.join(MOD_CACHE_ROOT, key, locked.version);
  if (!(await pathExists(moduleDir))) {
    throw new Error(`Missing installed dependency cache: ${moduleDir}`);
  }
  return {
    cacheDir: `${key}/${locked.version}`,
    commit: locked.commit ?? null,
    dirty: false,
    moduleDir,
    source: 'module-cache',
  };
}

async function dependencyArtifactSourcePath(moduleDir, artifact, locked) {
  if (locked.path === 'github.com/vo-lang/voplay') {
    const verification = verifyCurrentVoplayWasm({
      voplayRoot: VOPLAY_ROOT,
      outDir: VOPLAY_CURRENT_WASM_ROOT,
      expectedCiRunId: process.env.VO_DEV_CI_RUN_ID ?? null,
      requireClean: false,
    });
    if (verification.issues.length > 0) {
      throw new Error(`Current-source voplay WASM is invalid: ${verification.issues.join('; ')}`);
    }
    return path.join(VOPLAY_CURRENT_WASM_ROOT, artifact.name);
  }
  const candidates = [
    path.join(moduleDir, 'artifacts', artifact.name),
    path.join(moduleDir, 'web-artifacts', artifact.name),
    path.join(moduleDir, artifact.name),
  ];
  for (const candidate of candidates) {
    if (await pathExists(candidate)) {
      return candidate;
    }
  }
  throw new Error(`Missing dependency artifact ${artifact.name}; checked ${candidates.join(', ')}`);
}

async function buildProjectPackage() {
  const files = [];
  const sourceFiles = await blockKartSourceFiles();
  const sourceAllowlist = await blockKartSourceAllowlist();
  const rootEntries = await fs.readdir(BLOCKKART_ROOT, { withFileTypes: true });
  for (const entry of rootEntries) {
    if (!entry.isFile()) continue;
    if (!entry.name.endsWith('.vo') && entry.name !== 'vo.mod' && entry.name !== 'vo.lock') {
      continue;
    }
    files.push({
      path: entry.name,
      content: await fs.readFile(path.join(BLOCKKART_ROOT, entry.name), 'utf8'),
    });
  }
  for (const relative of BLOCKKART_RUNTIME_ASSETS) {
    const absolute = path.join(BLOCKKART_ROOT, relative);
    if (!(await pathExists(absolute))) {
      throw new Error(`Missing BlockKart runtime asset: ${absolute}`);
    }
    files.push({
      path: relative,
      contentBase64: (await fs.readFile(absolute)).toString('base64'),
    });
  }
  files.sort((a, b) => a.path.localeCompare(b.path));
  return {
    schemaVersion: 1,
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
  for (const locked of lockModules) {
    const key = cacheKey(locked.path);
    const { cacheDir, commit, dirty, moduleDir, source } = await dependencyModuleDir(locked);
    const packagedLock = { ...locked, commit: commit ?? locked.commit };

    const files = [];
    for (const absolute of await walkFiles(moduleDir)) {
      const relative = path.relative(moduleDir, absolute).split(path.sep).join('/');
      if (!shouldPackageDependencyFile(relative)) continue;
      files.push({
        path: relative,
        content: await fs.readFile(absolute, 'utf8'),
      });
    }

    const artifacts = [];
    for (const artifact of locked.artifacts) {
      if (artifact.target !== WASM_TARGET) continue;
      if (artifact.kind !== 'extension-wasm' && artifact.kind !== 'extension-js-glue') continue;
      const sourcePath = await dependencyArtifactSourcePath(moduleDir, artifact, locked);
      const bytes = await fs.readFile(sourcePath);
      const outRelative = path.posix.join('artifacts', key, locked.version, artifact.name);
      artifacts.push({
        kind: artifact.kind,
        target: artifact.target,
        name: artifact.name,
        size: bytes.byteLength,
        digest: sha256Digest(bytes),
        path: path.posix.join('artifacts', artifact.name),
        sourcePath,
        url: `/quickplay/blockkart/${outRelative}`,
      });
      await fs.mkdir(path.join(OUT_ROOT, 'artifacts', key, locked.version), { recursive: true });
      await fs.copyFile(sourcePath, path.join(OUT_ROOT, outRelative));
    }

    await rewritePackagedWebManifest(moduleDir, files, artifacts, packagedLock);
    files.sort((a, b) => a.path.localeCompare(b.path));
    artifacts.sort((a, b) => a.path.localeCompare(b.path));
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
  modules.sort((a, b) => a.module.localeCompare(b.module));
  return {
    schemaVersion: 1,
    name: 'BlockKart dependencies',
    modules,
  };
}

async function quickplayArtifactDigest(artifact) {
  const prefix = '/quickplay/blockkart/';
  if (!artifact.url.startsWith(prefix)) {
    throw new Error(`Unexpected quickplay artifact URL: ${artifact.url}`);
  }
  const relative = artifact.url.slice(prefix.length);
  const bytes = await fs.readFile(path.join(OUT_ROOT, relative));
  return {
    digest: sha256Digest(bytes),
    path: artifact.path,
    size: bytes.byteLength,
    url: artifact.url,
  };
}

async function dependencyProvenance(module) {
  const artifacts = [];
  for (const artifact of module.artifacts ?? []) {
    artifacts.push(await quickplayArtifactDigest(artifact));
  }
  artifacts.sort((a, b) => a.path.localeCompare(b.path));
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
  const vpakManifestBytes = await fs.readFile(vpakManifestPath);
  const vpakManifest = JSON.parse(vpakManifestBytes.toString('utf8'));
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
      source: currentWasm.manifest.source,
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
  dependencies.sort((a, b) => a.module.localeCompare(b.module));

  return {
    schemaVersion: 2,
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

async function main() {
  try {
    const lockPath = path.join(BLOCKKART_ROOT, 'vo.lock');
    const lockModules = parseLockFile(await fs.readFile(lockPath, 'utf8'));
    if (lockModules.length === 0) {
      throw new Error(`No resolved dependencies found in ${lockPath}`);
    }

    await fs.rm(OUT_ROOT, { recursive: true, force: true });
    await fs.mkdir(OUT_ROOT, { recursive: true });

    const projectPackage = await buildProjectPackage();
    const dependencyPackage = await buildDependencyPackage(lockModules);
    rewriteProjectLockForPackagedDependencies(projectPackage, dependencyPackage, lockModules);
    const outputBytes = {
      project: Buffer.from(jsonText(projectPackage), 'utf8'),
      deps: Buffer.from(jsonText(dependencyPackage), 'utf8'),
    };

    await fs.writeFile(path.join(OUT_ROOT, 'project.json'), outputBytes.project);
    await fs.writeFile(path.join(OUT_ROOT, 'deps.json'), outputBytes.deps);
    const provenance = await buildProvenance(projectPackage, dependencyPackage, outputBytes);
    await fs.writeFile(path.join(OUT_ROOT, 'provenance.json'), jsonText(provenance));

    console.log(`BlockKart quickplay package written to ${OUT_ROOT}`);
    console.log(`project json: ${projectPackage.files.length} files, ${outputBytes.project.byteLength} bytes`);
    console.log(`deps json: ${dependencyPackage.modules.length} modules, ${outputBytes.deps.byteLength} bytes`);
    console.log(`provenance: BlockKart ${projectPackage.commit}, ${dependencyPackage.modules.length} modules`);
  } finally {
    await cleanupCleanGitSnapshots();
  }
}

main().catch((error) => {
  console.error(error instanceof Error ? error.message : String(error));
  process.exitCode = 1;
});
