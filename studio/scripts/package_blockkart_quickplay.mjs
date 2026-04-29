import { execFileSync } from 'node:child_process';
import { promises as fs } from 'node:fs';
import os from 'node:os';
import path from 'node:path';

const STUDIO_ROOT = path.resolve(new URL('..', import.meta.url).pathname);
const REPO_ROOT = path.resolve(STUDIO_ROOT, '..');
const BLOCKKART_ROOT = path.resolve(process.env.BLOCKKART_ROOT ?? path.join(REPO_ROOT, '..', 'BlockKart'));
const MOD_CACHE_ROOT = path.resolve(process.env.VO_MOD_CACHE ?? path.join(os.homedir(), '.vo', 'mod'));
const OUT_ROOT = path.join(STUDIO_ROOT, 'public', 'quickplay', 'blockkart');
const WASM_TARGET = 'wasm32-unknown-unknown';

function cacheKey(modulePath) {
  return modulePath.replaceAll('/', '@');
}

function gitOutput(args, cwd) {
  return execFileSync('git', args, { cwd, encoding: 'utf8' }).trim();
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
    const moduleDir = path.join(MOD_CACHE_ROOT, key, locked.version);
    if (!(await pathExists(moduleDir))) {
      throw new Error(`Missing installed dependency cache: ${moduleDir}`);
    }

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
      const sourcePath = path.join(moduleDir, 'artifacts', artifact.name);
      if (!(await pathExists(sourcePath))) {
        throw new Error(`Missing dependency artifact: ${sourcePath}`);
      }
      const outRelative = path.posix.join('artifacts', key, locked.version, artifact.name);
      artifacts.push({
        path: path.posix.join('artifacts', artifact.name),
        url: `/quickplay/blockkart/${outRelative}`,
      });
      await fs.mkdir(path.join(OUT_ROOT, 'artifacts', key, locked.version), { recursive: true });
      await fs.copyFile(sourcePath, path.join(OUT_ROOT, outRelative));
    }

    files.sort((a, b) => a.path.localeCompare(b.path));
    artifacts.sort((a, b) => a.path.localeCompare(b.path));
    modules.push({
      module: locked.path,
      version: locked.version,
      cacheDir: `${key}/${locked.version}`,
      files,
      artifacts,
    });
  }
  modules.sort((a, b) => a.module.localeCompare(b.module));
  return {
    schemaVersion: 1,
    name: 'BlockKart dependencies',
    modules,
  };
}

async function main() {
  const lockPath = path.join(BLOCKKART_ROOT, 'vo.lock');
  const lockModules = parseLockFile(await fs.readFile(lockPath, 'utf8'));
  if (lockModules.length === 0) {
    throw new Error(`No resolved dependencies found in ${lockPath}`);
  }

  await fs.rm(OUT_ROOT, { recursive: true, force: true });
  await fs.mkdir(OUT_ROOT, { recursive: true });

  const projectPackage = await buildProjectPackage();
  const dependencyPackage = await buildDependencyPackage(lockModules);

  await fs.writeFile(
    path.join(OUT_ROOT, 'project.json'),
    `${JSON.stringify(projectPackage, null, 2)}\n`,
  );
  await fs.writeFile(
    path.join(OUT_ROOT, 'deps.json'),
    `${JSON.stringify(dependencyPackage, null, 2)}\n`,
  );

  const projectBytes = Buffer.byteLength(JSON.stringify(projectPackage));
  const depBytes = Buffer.byteLength(JSON.stringify(dependencyPackage));
  console.log(`BlockKart quickplay package written to ${OUT_ROOT}`);
  console.log(`project json: ${projectPackage.files.length} files, ${projectBytes} bytes`);
  console.log(`deps json: ${dependencyPackage.modules.length} modules, ${depBytes} bytes`);
}

main().catch((error) => {
  console.error(error instanceof Error ? error.message : String(error));
  process.exitCode = 1;
});
