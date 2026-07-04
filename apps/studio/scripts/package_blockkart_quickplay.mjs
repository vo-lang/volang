import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { promises as fs } from 'node:fs';
import os from 'node:os';
import path from 'node:path';

const STUDIO_ROOT = path.resolve(new URL('..', import.meta.url).pathname);
const REPO_ROOT = path.resolve(STUDIO_ROOT, '../..');
const BLOCKKART_ROOT = path.resolve(process.env.BLOCKKART_ROOT ?? path.join(REPO_ROOT, '..', 'BlockKart'));
const MOD_CACHE_ROOT = path.resolve(process.env.VO_MOD_CACHE ?? path.join(os.homedir(), '.vo', 'mod'));
const VOPLAY_ROOT = path.resolve(process.env.VOPLAY_ROOT ?? path.join(REPO_ROOT, '..', 'voplay'));
const OUT_ROOT = path.resolve(process.env.BLOCKKART_QUICKPLAY_OUT_ROOT ?? path.join(STUDIO_ROOT, 'public', 'quickplay', 'blockkart'));
const WASM_TARGET = 'wasm32-unknown-unknown';
const BLOCKKART_RUNTIME_ASSETS = ['assets/blockkart.vpak'];
const QUICKPLAY_ARTIFACT_NAME = 'studio.quickplay.blockkart';
const QUICKPLAY_ARTIFACT_PATH = 'apps/studio/public/quickplay/blockkart';
const QUICKPLAY_GENERATOR_COMMAND = ['vo-dev', 'task', 'run', 'task:quickplay-blockkart-package'];
const QUICKPLAY_GENERATOR_INPUTS = [
  'apps/studio/scripts/package_blockkart_quickplay.mjs',
  'eng/project.toml',
  'external:BlockKart',
  'module-cache:voplay',
  'module-cache:vopack',
  'module-cache:vogui',
];

function cacheKey(modulePath) {
  return modulePath.replaceAll('/', '@');
}

function gitOutput(args, cwd) {
  return execFileSync('git', args, { cwd, encoding: 'utf8' }).trim();
}

function requireCleanGitTree(cwd, label) {
  const status = gitOutput(['status', '--porcelain'], cwd);
  if (status) {
    throw new Error(`${label} has uncommitted changes; refusing to create checked-in quickplay package`);
  }
}

function requireGitHead(cwd, label, expected) {
  if (!expected) return;
  const head = gitOutput(['rev-parse', 'HEAD'], cwd);
  if (head !== expected) {
    throw new Error(`${label} HEAD ${head} does not match locked commit ${expected}`);
  }
}

function sha256Digest(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
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

function shouldSkipDependencyDirectory(name) {
  return name.startsWith('.') || name === 'target' || name === 'tmp_checks';
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
    const match = line.match(/^([A-Za-z0-9_.-]+)\s*=\s*"([^"]*)"$/);
    if (!match) continue;
    const [, key, value] = match;
    if (artifact) {
      artifact[key] = value;
    } else if (current) {
      current[key] = value;
    }
  }
  if (artifact && current) current.artifacts.push(artifact);
  return modules.filter((module) => module.path && module.version);
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
    requireCleanGitTree(VOPLAY_ROOT, locked.path);
    requireGitHead(VOPLAY_ROOT, locked.path, locked.commit);
    return {
      cacheDir: `${cacheKey(locked.path)}/${locked.version}`,
      moduleDir: VOPLAY_ROOT,
      source: 'external',
    };
  }
  const key = cacheKey(locked.path);
  const moduleDir = path.join(MOD_CACHE_ROOT, key, locked.version);
  if (!(await pathExists(moduleDir))) {
    throw new Error(`Missing installed dependency cache: ${moduleDir}`);
  }
  return {
    cacheDir: `${key}/${locked.version}`,
    moduleDir,
    source: 'module-cache',
  };
}

async function dependencyArtifactSourcePath(moduleDir, artifact) {
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
    files,
  };
}

async function buildDependencyPackage(lockModules) {
  const modules = [];
  for (const locked of lockModules) {
    const key = cacheKey(locked.path);
    const { cacheDir, moduleDir } = await dependencyModuleDir(locked);

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
      const sourcePath = await dependencyArtifactSourcePath(moduleDir, artifact);
      const outRelative = path.posix.join('artifacts', key, locked.version, artifact.name);
      artifacts.push({
        path: path.posix.join('artifacts', artifact.name),
        sourcePath,
        url: `/quickplay/blockkart/${outRelative}`,
      });
      await fs.mkdir(path.join(OUT_ROOT, 'artifacts', key, locked.version), { recursive: true });
      await fs.copyFile(sourcePath, path.join(OUT_ROOT, outRelative));
    }

    await rewritePackagedWebManifest(moduleDir, files, artifacts, locked);
    files.sort((a, b) => a.path.localeCompare(b.path));
    artifacts.sort((a, b) => a.path.localeCompare(b.path));
    modules.push({
      module: locked.path,
      version: locked.version,
      commit: locked.commit ?? null,
      cacheDir,
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
    filesDigest: packagedFilesDigest(module.files),
    module: module.module,
    version: module.version,
    commit: module.commit ?? null,
  };
}

async function buildProvenance(projectPackage, dependencyPackage, outputBytes) {
  const dependencies = [];
  for (const module of dependencyPackage.modules) {
    dependencies.push(await dependencyProvenance(module));
  }
  dependencies.sort((a, b) => a.module.localeCompare(b.module));

  return {
    schemaVersion: 1,
    artifact: QUICKPLAY_ARTIFACT_NAME,
    path: QUICKPLAY_ARTIFACT_PATH,
    generator: {
      command: QUICKPLAY_GENERATOR_COMMAND,
      script: 'apps/studio/scripts/package_blockkart_quickplay.mjs',
      version: 1,
    },
    inputs: QUICKPLAY_GENERATOR_INPUTS,
    project: {
      commit: projectPackage.commit,
      dirty: false,
      filesDigest: packagedFilesDigest(projectPackage.files),
      module: projectPackage.module,
    },
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
  requireCleanGitTree(BLOCKKART_ROOT, 'BlockKart');
  const lockPath = path.join(BLOCKKART_ROOT, 'vo.lock');
  const lockModules = parseLockFile(await fs.readFile(lockPath, 'utf8'));
  if (lockModules.length === 0) {
    throw new Error(`No resolved dependencies found in ${lockPath}`);
  }

  await fs.rm(OUT_ROOT, { recursive: true, force: true });
  await fs.mkdir(OUT_ROOT, { recursive: true });

  const projectPackage = await buildProjectPackage();
  const dependencyPackage = await buildDependencyPackage(lockModules);
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
}

main().catch((error) => {
  console.error(error instanceof Error ? error.message : String(error));
  process.exitCode = 1;
});
