#!/usr/bin/env node
import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { existsSync, mkdirSync, readFileSync, readdirSync, statSync, writeFileSync } from 'node:fs';
import { join, posix, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';
import { sourceBoundEvidence } from './source_bound_evidence.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const quickplayDir = resolve(process.env.QUICKPLAY_DIR ?? join(root, 'apps/studio/public/quickplay/blockkart'));
const projectPath = join(quickplayDir, 'project.json');
const depsPath = join(quickplayDir, 'deps.json');
const provenancePath = join(quickplayDir, 'provenance.json');
const quickplayTsPath = join(root, 'apps/studio/src/lib/quickplay.ts');
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const outDir = resolve(process.env.QUICKPLAY_VALIDATE_OUT_DIR || join(root, 'target/quickplay-validate'));
const dependencyRepos = [
  { name: 'github.com/vo-lang/vogui', root: requireRepoRoot('VOGUI_ROOT', 'vogui') },
  { name: 'github.com/vo-lang/voplay', root: requireRepoRoot('VOPLAY_ROOT', 'voplay') },
  { name: 'github.com/vo-lang/vopack', root: requireRepoRoot('VOPACK_ROOT', 'vopack') },
];

const expected = {
  projectName: 'BlockKart',
  projectModule: 'github.com/vo-lang/blockkart',
  requiredModules: [
    'github.com/vo-lang/vogui',
    'github.com/vo-lang/voplay',
  ],
  artifactName: 'studio.quickplay.blockkart',
  artifactPath: 'apps/studio/public/quickplay/blockkart',
  generatorCommand: ['vo-dev', 'task', 'run', 'task:quickplay-blockkart-package'],
  generatorInputs: [
    'apps/studio/scripts/package_blockkart_quickplay.mjs',
    'eng/project.toml',
    'external:BlockKart',
    'module-cache:voplay',
    'module-cache:vopack',
    'module-cache:vogui',
  ],
};

function writeReport(status, details = {}) {
  const generatedAt = new Date().toISOString();
  mkdirSync(outDir, { recursive: true });
  writeFileSync(join(outDir, 'report.json'), `${JSON.stringify({
    schemaVersion: 1,
    kind: 'quickplay.validateReport',
    gate: 'quickplay-validate',
    status,
    generatedAt,
    freshEvidence: sourceBoundEvidence({
      gate: 'quickplay-validate',
      generatedAt,
      root,
      repos: [
        { name: 'volang', root },
        { name: 'BlockKart', root: blockKartRoot },
        ...dependencyRepos,
      ],
      gateFiles: [
        'scripts/ci/quickplay_validate.mjs',
        'scripts/ci/repo_roots.mjs',
        'scripts/ci/source_bound_evidence.mjs',
        'apps/studio/scripts/package_blockkart_quickplay.mjs',
        'apps/studio/src/lib/quickplay.ts',
        'eng/artifacts.toml',
        'eng/tasks.toml',
        'eng/project.toml',
      ],
      artifacts: [quickplayDir],
    }),
    quickplayDir,
    ...details,
  }, null, 2)}\n`);
}

function fail(message) {
  writeReport('failed', { message });
  console.error(`quickplay validate: ${message}`);
  process.exit(1);
}

function readJson(path) {
  try {
    return JSON.parse(readFileSync(path, 'utf8'));
  } catch (error) {
    fail(`cannot read JSON ${path}: ${error.message}`);
  }
}

function gitOutput(args, cwd) {
  try {
    return execFileSync('git', args, { cwd, encoding: 'utf8' }).trim();
  } catch (error) {
    const stderr = error?.stderr ? String(error.stderr).trim() : '';
    fail(`git ${args.join(' ')} failed in ${cwd}: ${stderr || error.message}`);
  }
}

function gitDirty(cwd) {
  return gitOutput(['status', '--porcelain'], cwd) !== '';
}

function assert(condition, message) {
  if (!condition) {
    fail(message);
  }
}

function localPathForArtifact(url) {
  const prefix = '/quickplay/blockkart/';
  assert(url.startsWith(prefix), `artifact URL must be quickplay-local: ${url}`);
  return join(quickplayDir, url.slice(prefix.length));
}

function moduleByName(modules, moduleName) {
  const mod = modules.get(moduleName);
  assert(mod, `deps modules must include ${moduleName}`);
  assert(typeof mod.version === 'string' && mod.version.length > 0, `${moduleName} must have a version`);
  return mod;
}

function requiredVoplayArtifacts(voplay) {
  const artifacts = voplay.artifacts ?? [];
  const js = artifacts.find((artifact) => artifact.url?.endsWith('/voplay_island.js'));
  const wasm = artifacts.find((artifact) => artifact.url?.endsWith('/voplay_island_bg.wasm'));
  assert(js, 'voplay JS prefetch artifact missing from deps');
  assert(wasm, 'voplay WASM prefetch artifact missing from deps');
  return [js, wasm];
}

function sha256Digest(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function jsonFileDigest(path) {
  return sha256Digest(readFileSync(path));
}

function moduleFileBytes(file) {
  if (file.content != null) return Buffer.from(file.content, 'utf8');
  if (file.contentBase64 != null) return Buffer.from(file.contentBase64, 'base64');
  fail(`module file is missing content: ${file.path}`);
}

function sourceSetDigest(entries) {
  return sha256Digest(Buffer.from(JSON.stringify(entries), 'utf8'));
}

function listProjectSourceFiles(projectRoot, extension) {
  const files = [];
  const visit = (dir) => {
    for (const entry of readdirSync(dir)) {
      const file = join(dir, entry);
      const stat = statSync(file);
      if (stat.isDirectory()) {
        if (entry === '.git' || entry === 'target' || entry === 'node_modules' || entry === 'tmp_checks') continue;
        visit(file);
      } else if (file.endsWith(extension)) {
        const bytes = readFileSync(file);
        files.push({
          path: posix.normalize(file.slice(projectRoot.length + 1).split(/[\\/]/).join('/')),
          digest: sha256Digest(bytes),
          size: bytes.byteLength,
        });
      }
    }
  };
  visit(projectRoot);
  return files.sort((a, b) => a.path.localeCompare(b.path));
}

function packagedFilesDigest(files) {
  const entries = files
    .map((file) => {
      const bytes = moduleFileBytes(file);
      return {
        digest: sha256Digest(bytes),
        path: file.path,
        size: bytes.byteLength,
      };
    })
    .sort((a, b) => a.path.localeCompare(b.path));
  return sourceSetDigest(entries);
}

function sameStringArray(actual, wanted) {
  return Array.isArray(actual)
    && actual.length === wanted.length
    && actual.every((value, index) => value === wanted[index]);
}

function quickplayArtifactUrlsFromSource(source) {
  return [...source.matchAll(/\/quickplay\/blockkart\/artifacts\/[^'"`)\\\s]+/g)].map((match) => match[0]);
}

function validatePackagedWebManifest(mod) {
  const manifestFile = mod.files.find((file) => file.path === 'vo.web.json');
  if (!manifestFile || mod.files.some((file) => file.path === 'vo.release.json')) return;

  let manifest;
  try {
    manifest = JSON.parse(manifestFile.content);
  } catch (error) {
    fail(`${mod.module} vo.web.json is invalid JSON: ${error.message}`);
  }

  assert(Array.isArray(manifest.source), `${mod.module} vo.web.json source must be an array`);
  const files = new Map(mod.files.map((file) => [file.path, file]));
  for (const entry of manifest.source) {
    const file = files.get(entry.path);
    assert(file, `${mod.module} vo.web.json declares missing source ${entry.path}`);
    const bytes = moduleFileBytes(file);
    assert(bytes.byteLength === entry.size, `${mod.module} source size mismatch for ${entry.path}`);
    assert(sha256Digest(bytes) === entry.digest, `${mod.module} source digest mismatch for ${entry.path}`);
  }
  assert(
    sha256Digest(Buffer.from(JSON.stringify(manifest.source), 'utf8')) === manifest.source_digest,
    `${mod.module} vo.web.json source_digest mismatch`,
  );

  const webArtifacts = new Map((manifest.artifacts ?? []).map((artifact) => [artifact.path, artifact]));
  for (const artifact of mod.artifacts ?? []) {
    const webArtifact = webArtifacts.get(artifact.path);
    assert(webArtifact, `${mod.module} vo.web.json does not declare packaged artifact ${artifact.path}`);
    const localPath = localPathForArtifact(artifact.url);
    const bytes = readFileSync(localPath);
    assert(bytes.byteLength === webArtifact.size, `${mod.module} artifact size mismatch for ${artifact.path}`);
    assert(sha256Digest(bytes) === webArtifact.digest, `${mod.module} artifact digest mismatch for ${artifact.path}`);
  }
}

function parseReleaseManifest(mod) {
  const source = requireModuleFile(mod, 'vo.release.json');
  try {
    return JSON.parse(source);
  } catch (error) {
    fail(`${mod.module} vo.release.json is invalid JSON: ${error.message}`);
  }
}

function validateSourceEntry(entry, moduleName) {
  assert(entry && typeof entry === 'object', `${moduleName} vo.web.json source entries must be objects`);
  assert(typeof entry.path === 'string' && entry.path.length > 0, `${moduleName} vo.web.json source entry path must be a string`);
  assert(Number.isInteger(entry.size) && entry.size >= 0, `${moduleName} vo.web.json source entry ${entry.path} size must be a non-negative integer`);
  assert(sha256Field(entry.digest), `${moduleName} vo.web.json source entry ${entry.path} digest must be sha256`);
}

function shouldValidateEmbeddedSourceFile(file) {
  if (file.content == null) return false;
  return ![
    '.vo-source-digest',
    '.vo-version',
    'vo.release.json',
    'vo.web.json',
  ].includes(file.path);
}

function validateReleaseSourceContracts(mod, lockedModule, provenanceDependency) {
  const dependencyDirty = provenanceDependency?.dirty === true;
  assert(!dependencyDirty, `${mod.module} provenance dirty flag must be false in strict validation`);
  const releaseFile = mod.files.find((file) => file.path === 'vo.release.json');
  if (!releaseFile) return;

  const releaseSource = moduleFileBytes(releaseFile);
  const release = parseReleaseManifest(mod);
  const webManifest = parseVoWebManifest(mod);
  const sourceMarker = requireModuleFile(mod, '.vo-source-digest').trim();

  assert(release.schema_version === 1, `${mod.module} vo.release.json schema_version must be 1`);
  assert(release.module === mod.module, `${mod.module} vo.release.json module mismatch`);
  assert(release.version === mod.version, `${mod.module} vo.release.json version mismatch`);
  assert(release.commit === lockedModule.commit, `${mod.module} vo.release.json commit must match project vo.lock`);
  assert(sha256Digest(releaseSource) === lockedModule.release_manifest, `${mod.module} vo.release.json digest must match project vo.lock`);
  assert(release.source?.digest === lockedModule.source, `${mod.module} vo.release.json source digest must match project vo.lock`);
  assert(sourceMarker === lockedModule.source, `${mod.module} .vo-source-digest must match project vo.lock`);
  assert(sourceMarker === release.source?.digest, `${mod.module} .vo-source-digest must match vo.release.json source digest`);
  validateReleaseArtifactContract(mod, release);

  assert(webManifest.module === mod.module, `${mod.module} vo.web.json module mismatch`);
  assert(webManifest.version === mod.version, `${mod.module} vo.web.json version mismatch`);
  assert(webManifest.commit === lockedModule.commit, `${mod.module} vo.web.json commit must match project vo.lock`);
  assert(Array.isArray(webManifest.source), `${mod.module} vo.web.json source must be an array`);
  const seen = new Set();
  for (const entry of webManifest.source) {
    validateSourceEntry(entry, mod.module);
    assert(!seen.has(entry.path), `${mod.module} vo.web.json duplicate source entry ${entry.path}`);
    seen.add(entry.path);
  }
  assert(sourceSetDigest(webManifest.source) === webManifest.source_digest, `${mod.module} vo.web.json source_digest mismatch`);

  const sourceByPath = new Map(webManifest.source.map((entry) => [entry.path, entry]));
  for (const file of mod.files) {
    if (!shouldValidateEmbeddedSourceFile(file)) continue;
    const entry = sourceByPath.get(file.path);
    assert(entry, `${mod.module} embeds source file not declared by vo.web.json: ${file.path}`);
    const bytes = moduleFileBytes(file);
    assert(bytes.byteLength === entry.size, `${mod.module} embedded source size mismatch for ${file.path}`);
    assert(sha256Digest(bytes) === entry.digest, `${mod.module} embedded source digest mismatch for ${file.path}`);
  }
}

function requireModuleFile(mod, path) {
  const file = mod.files.find((file) => file.path === path);
  assert(file, `${mod.module} must include ${path}`);
  return moduleFileBytes(file).toString('utf8');
}

function requireProjectFile(project, path) {
  const file = project.files.find((file) => file.path === path);
  assert(file, `project package must include ${path}`);
  return moduleFileBytes(file).toString('utf8');
}

function parseVoModRequires(source) {
  const requires = new Map();
  for (const rawLine of source.split(/\r?\n/)) {
    const line = rawLine.trim();
    if (line === '' || line.startsWith('#')) continue;
    const require = line.match(/^require\s+(\S+)\s+(\S+)$/);
    if (require) {
      requires.set(require[1], require[2]);
    }
  }
  return requires;
}

function parseVoModDeclaredArtifacts(source) {
  const artifacts = [];
  let table = '';
  const wasm = {};
  let nativeTarget = null;

  function flushNativeTarget() {
    if (!nativeTarget) return;
    if (nativeTarget.target && nativeTarget.library) {
      artifacts.push({
        kind: 'extension-native',
        target: nativeTarget.target,
        name: nativeTarget.library,
      });
    }
    nativeTarget = null;
  }

  for (const rawLine of source.split(/\r?\n/)) {
    const line = rawLine.trim();
    if (line === '' || line.startsWith('#')) continue;
    const tableMatch = line.match(/^\[\[?([^\]]+)\]\]?$/);
    if (tableMatch) {
      flushNativeTarget();
      table = tableMatch[1].trim();
      if (table === 'extension.native.targets') {
        nativeTarget = {};
      }
      continue;
    }
    const pair = line.match(/^([A-Za-z0-9_.-]+)\s*=\s*"([^"]*)"$/);
    if (!pair) continue;
    const [, key, value] = pair;
    if (table === 'extension.wasm' && (key === 'wasm' || key === 'js_glue')) {
      wasm[key] = value;
    } else if (table === 'extension.native.targets' && nativeTarget && (key === 'target' || key === 'library')) {
      nativeTarget[key] = value;
    }
  }
  flushNativeTarget();
  if (wasm.wasm) {
    artifacts.push({
      kind: 'extension-wasm',
      target: 'wasm32-unknown-unknown',
      name: wasm.wasm,
    });
  }
  if (wasm.js_glue) {
    artifacts.push({
      kind: 'extension-js-glue',
      target: 'wasm32-unknown-unknown',
      name: wasm.js_glue,
    });
  }
  return artifacts.sort((a, b) => artifactKey(a).localeCompare(artifactKey(b)));
}

function parseVoLockValue(value) {
  if (value.startsWith('"') || value.startsWith('[')) {
    try {
      return JSON.parse(value);
    } catch (error) {
      fail(`invalid vo.lock value ${value}: ${error.message}`);
    }
  }
  if (/^\d+$/.test(value)) {
    return Number(value);
  }
  return value;
}

function parseVoLockResolved(source) {
  const resolved = [];
  let current = null;
  let artifact = null;
  for (const rawLine of source.split(/\r?\n/)) {
    const line = rawLine.trim();
    if (line === '' || line.startsWith('#')) continue;
    if (line === '[[resolved]]') {
      current = { artifacts: [] };
      artifact = null;
      resolved.push(current);
      continue;
    }
    if (line === '[[resolved.artifact]]') {
      assert(current, 'vo.lock artifact block must follow a resolved module');
      artifact = {};
      current.artifacts.push(artifact);
      continue;
    }

    const pair = line.match(/^([A-Za-z0-9_]+)\s*=\s*(.+)$/);
    if (!pair || current == null) continue;
    const target = artifact ?? current;
    target[pair[1]] = parseVoLockValue(pair[2]);
  }
  return new Map(resolved.map((entry) => [entry.path, entry]));
}

function parseVoWebManifest(mod) {
  const manifestSource = requireModuleFile(mod, 'vo.web.json');
  try {
    return JSON.parse(manifestSource);
  } catch (error) {
    fail(`${mod.module} vo.web.json is invalid JSON: ${error.message}`);
  }
}

function sha256Field(value) {
  return typeof value === 'string' && /^sha256:[0-9a-f]{64}$/.test(value);
}

function artifactKey(artifact) {
  return `${artifact.kind}\u0000${artifact.target}\u0000${artifact.name}`;
}

function artifactLabel(artifact) {
  return `${artifact.kind} ${artifact.target} ${artifact.name}`;
}

function validateReleaseArtifactContract(mod, release) {
  const declared = parseVoModDeclaredArtifacts(requireModuleFile(mod, 'vo.mod'));
  const published = (release.artifacts ?? [])
    .map((artifact) => ({
      kind: artifact.kind,
      target: artifact.target,
      name: artifact.name,
    }))
    .sort((a, b) => artifactKey(a).localeCompare(artifactKey(b)));
  const declaredKeys = new Set(declared.map(artifactKey));
  const publishedKeys = new Set(published.map(artifactKey));
  const missing = declared.filter((artifact) => !publishedKeys.has(artifactKey(artifact)));
  const undeclared = published.filter((artifact) => !declaredKeys.has(artifactKey(artifact)));
  assert(
    missing.length === 0 && undeclared.length === 0,
    `${mod.module} vo.mod artifact contract mismatch: missing [${missing.map(artifactLabel).join(', ')}] undeclared [${undeclared.map(artifactLabel).join(', ')}]`,
  );
}

function validateProjectDependencyContracts(project, deps, provenance) {
  const directRequires = parseVoModRequires(requireProjectFile(project, 'vo.mod'));
  const locked = parseVoLockResolved(requireProjectFile(project, 'vo.lock'));
  const provenanceDeps = new Map((provenance.dependencies ?? []).map((mod) => [mod.module, mod]));

  for (const moduleName of expected.requiredModules) {
    const requiredVersion = directRequires.get(moduleName);
    assert(requiredVersion, `project vo.mod must directly require ${moduleName}`);
    const mod = moduleByName(new Map(deps.modules.map((entry) => [entry.module, entry])), moduleName);
    assert(
      requiredVersion === mod.version,
      `project vo.mod requires ${moduleName} ${requiredVersion}, but deps package embeds ${mod.version}`,
    );
  }

  for (const mod of deps.modules) {
    const lockedModule = locked.get(mod.module);
    assert(lockedModule, `project vo.lock must resolve packaged dependency ${mod.module}`);
    assert(
      lockedModule.version === mod.version,
      `project vo.lock resolves ${mod.module} ${lockedModule.version}, but deps package embeds ${mod.version}`,
    );
    assert(sha256Field(lockedModule.source), `project vo.lock must bind ${mod.module} source digest`);
    assert(sha256Field(lockedModule.release_manifest), `project vo.lock must bind ${mod.module} release manifest digest`);

    const webManifest = parseVoWebManifest(mod);
    validateReleaseSourceContracts(mod, lockedModule, provenanceDeps.get(mod.module));
    assert(webManifest.module === mod.module, `${mod.module} vo.web.json module mismatch`);
    assert(webManifest.version === mod.version, `${mod.module} vo.web.json version mismatch`);
    assert(webManifest.commit === lockedModule.commit, `${mod.module} vo.web.json commit must match project vo.lock`);

    const webArtifacts = new Map((webManifest.artifacts ?? []).map((artifact) => [artifactKey(artifact), artifact]));
    for (const lockedArtifact of lockedModule.artifacts ?? []) {
      const webArtifact = webArtifacts.get(artifactKey(lockedArtifact));
      assert(webArtifact, `${mod.module} vo.web.json missing locked artifact ${lockedArtifact.name}`);
      assert(
        webArtifact.size === lockedArtifact.size,
        `${mod.module} artifact size mismatch for ${lockedArtifact.name}`,
      );
      assert(
        webArtifact.digest === lockedArtifact.digest,
        `${mod.module} artifact digest mismatch for ${lockedArtifact.name}`,
      );
    }
  }
}

function validateProjectSourceCheckout(project, provenance) {
  assert(existsSync(blockKartRoot), `BlockKart source checkout is required at ${blockKartRoot}`);
  assert(gitOutput(['rev-parse', '--is-inside-work-tree'], blockKartRoot) === 'true', `BlockKart source is not a git checkout: ${blockKartRoot}`);
  const head = gitOutput(['rev-parse', 'HEAD'], blockKartRoot);
  assert(head === project.commit, `BlockKart source HEAD ${head} does not match project commit ${project.commit}`);
  const status = gitOutput(['status', '--porcelain'], blockKartRoot);
  assert((status !== '') === provenance.project?.dirty, 'BlockKart source dirty state must match provenance project.dirty');
  assert(status === '', 'BlockKart source checkout must be clean for strict quickplay validation');
  assert(provenance.project?.dirty === false, 'provenance project.dirty must be false for strict quickplay validation');

  for (const file of project.files ?? []) {
    const sourcePath = join(blockKartRoot, file.path);
    assert(existsSync(sourcePath), `BlockKart source checkout is missing packaged file ${file.path}`);
    const sourceBytes = readFileSync(sourcePath);
    const packagedBytes = moduleFileBytes(file);
    assert(sourceBytes.byteLength === packagedBytes.byteLength, `BlockKart source size mismatch for ${file.path}`);
    assert(sha256Digest(sourceBytes) === sha256Digest(packagedBytes), `BlockKart source digest mismatch for ${file.path}`);
  }

  const sourceFiles = listProjectSourceFiles(blockKartRoot, '.vo');
  const manifestSourceFiles = project.sourceFiles ?? provenance.project?.sourceFiles;
  assert(Array.isArray(manifestSourceFiles), 'quickplay package must record BlockKart sourceFiles digest list');
  assert(
    sourceSetDigest(manifestSourceFiles) === sourceSetDigest(sourceFiles),
    'quickplay package sourceFiles digest list must match BlockKart source checkout',
  );
  assert(
    provenance.project?.sourceFilesDigest === sourceSetDigest(sourceFiles),
    'provenance project sourceFilesDigest must match BlockKart source checkout',
  );

  const packagedVo = new Set((project.files ?? [])
    .map((file) => file.path)
    .filter((file) => file.endsWith('.vo')));
  const sourceAllowlist = new Map((provenance.project?.sourceAllowlist ?? project.sourceAllowlist ?? [])
    .map((entry) => [entry.path, entry]));
  const unpackaged = sourceFiles
    .filter((entry) => !packagedVo.has(entry.path))
    .filter((entry) => !sourceAllowlist.has(entry.path));
  assert(unpackaged.length === 0, `BlockKart source files missing from quickplay package or allowlist: ${unpackaged.map((entry) => entry.path).join(', ')}`);
  for (const entry of sourceAllowlist.values()) {
    assert(
      typeof entry.reason === 'string'
        && entry.reason.trim().length >= 12
        && sourceFiles.some((source) => source.path === entry.path),
      `invalid BlockKart source allowlist entry: ${entry.path ?? '<missing>'}`,
    );
  }
}

function moduleFileMap(mod) {
  return new Map(mod.files.map((file) => [file.path, file]));
}

function localJsImportSpecifiers(source) {
  const specs = [];
  const staticImport = /\b(?:import|export)\s+(?:[^'"]*?\s+from\s+)?["']([^"']+)["']/g;
  const dynamicImport = /\bimport\s*\(\s*["']([^"']+)["']\s*\)/g;
  for (const match of source.matchAll(staticImport)) {
    if (match[1].startsWith('.')) specs.push(match[1]);
  }
  for (const match of source.matchAll(dynamicImport)) {
    if (match[1].startsWith('.')) specs.push(match[1]);
  }
  return specs;
}

function resolveLocalJsImport(files, fromPath, specifier) {
  const base = posix.normalize(posix.join(posix.dirname(fromPath), specifier));
  const candidates = posix.extname(base)
    ? [base]
    : [`${base}.js`, `${base}/index.js`, base];
  const resolved = candidates.find((candidate) => files.has(candidate));
  assert(resolved, `cannot resolve local JS import ${specifier} from ${fromPath}`);
  return resolved;
}

function collectLocalJsImportGraph(mod, entryPath) {
  const files = moduleFileMap(mod);
  assert(files.has(entryPath), `${mod.module} renderer entry missing from package: ${entryPath}`);
  const seen = new Set();
  const queue = [entryPath];
  const sources = [];
  while (queue.length > 0) {
    const path = queue.shift();
    if (seen.has(path)) continue;
    seen.add(path);
    const source = moduleFileBytes(files.get(path)).toString('utf8');
    sources.push({ path, source });
    for (const specifier of localJsImportSpecifiers(source)) {
      const resolved = resolveLocalJsImport(files, path, specifier);
      if (!seen.has(resolved)) queue.push(resolved);
    }
  }
  return sources;
}

function validateVoplayHostWakeKeyContract(voplay) {
  const manifest = parseVoWebManifest(voplay);
  const rendererEntry = manifest.extension?.web?.js_modules?.renderer;
  assert(typeof rendererEntry === 'string' && rendererEntry.length > 0, 'voplay vo.web.json must declare a renderer JS module');
  const graph = collectLocalJsImportGraph(voplay, rendererEntry);
  const graphSource = graph.map((file) => file.source).join('\n');
  const dts = requireModuleFile(voplay, 'js/dist/render_bootstrap.d.ts');
  assert(
    graph.some((file) => file.path === 'js/dist/render_bootstrap.js'),
    'voplay renderer import graph must reach render_bootstrap.js host wake bridge',
  );
  assert(dts.includes('key: string;'), 'voplay render bootstrap pending host events must expose HostWaitKey');
  assert(dts.includes('wakeHostEvent(key: string): void;'), 'voplay render bootstrap wake API must accept HostWaitKey');
  for (const stale of [
    'hostTimers.has(ev.token)',
    'displayPulseWaiters.set(ev.token',
    'hostTimers.set(ev.token',
    'wakeHostEvent(token',
    'wakeHostEvent(ev.token',
    'this.vm.wakeHostEvent(token)',
  ]) {
    assert(!graphSource.includes(stale), `voplay renderer graph must not use token-only host wake path: ${stale}`);
  }
  for (const current of [
    'hostTimers.has(ev.key)',
    'displayPulseWaiters.set(ev.key',
    'hostTimers.set(ev.key',
    'wakeHostEvent(ev.key',
    'this.vm.wakeHostEvent(key)',
  ]) {
    assert(graphSource.includes(current), `voplay renderer graph must use HostWaitKey path: ${current}`);
  }
}

function validateProvenance(project, deps) {
  const provenance = readJson(provenancePath);
  assert(provenance.schemaVersion === 2, 'provenance schemaVersion must be 2');
  assert(provenance.artifact === expected.artifactName, `provenance artifact must be ${expected.artifactName}`);
  assert(provenance.path === expected.artifactPath, `provenance path must be ${expected.artifactPath}`);
  assert(provenance.task?.id === 'quickplay-blockkart-package', 'provenance task id mismatch');
  assert(
    sameStringArray(provenance.task?.command, expected.generatorCommand),
    'provenance task command mismatch',
  );
  assert(
    sameStringArray(provenance.generator?.command, expected.generatorCommand),
    'provenance generator command mismatch',
  );
  assert(Number(provenance.generator?.version) >= 2, 'provenance generator version must be at least 2');
  assert(typeof provenance.toolchain?.node === 'string' && provenance.toolchain.node.length > 0, 'provenance node toolchain version missing');
  assert(typeof provenance.toolchain?.voDev === 'string' && provenance.toolchain.voDev.length >= 7, 'provenance vo-dev toolchain version missing');
  assert(provenance.toolchain?.wasmTarget === 'wasm32-unknown-unknown', 'provenance wasm target mismatch');
  assert(typeof provenance.sourceRoots?.volang === 'string' && provenance.sourceRoots.volang.length > 0, 'provenance volang source root missing');
  assert(typeof provenance.sourceRoots?.blockKart === 'string' && provenance.sourceRoots.blockKart.length > 0, 'provenance BlockKart source root missing');
  assert(typeof provenance.sourceRoots?.voplay === 'string' && provenance.sourceRoots.voplay.length > 0, 'provenance voplay source root missing');
  assert(
    sameStringArray(provenance.inputs, expected.generatorInputs),
    'provenance generator inputs mismatch',
  );
  assert(provenance.project?.module === project.module, 'provenance project module mismatch');
  assert(provenance.project?.commit === project.commit, 'provenance project commit mismatch');
  assert(provenance.project?.dirty === gitDirty(blockKartRoot), 'provenance project dirty flag mismatch');
  assert(provenance.project?.dirty === false, 'provenance project dirty flag must be false');
  assert(
    provenance.project?.filesDigest === packagedFilesDigest(project.files),
    'provenance project files digest mismatch',
  );
  assert(
    sourceSetDigest(provenance.project?.sourceFiles ?? []) === sourceSetDigest(project.sourceFiles ?? []),
    'provenance project sourceFiles must match project sourceFiles',
  );
  assert(
    provenance.project?.sourceFilesDigest === sourceSetDigest(project.sourceFiles ?? []),
    'provenance project sourceFilesDigest mismatch',
  );

  const outputMap = new Map((provenance.outputs ?? []).map((output) => [output.path, output]));
  const projectOutput = outputMap.get('project.json');
  const depsOutput = outputMap.get('deps.json');
  assert(projectOutput?.digest === jsonFileDigest(projectPath), 'provenance project.json digest mismatch');
  assert(projectOutput?.size === statSync(projectPath).size, 'provenance project.json size mismatch');
  assert(depsOutput?.digest === jsonFileDigest(depsPath), 'provenance deps.json digest mismatch');
  assert(depsOutput?.size === statSync(depsPath).size, 'provenance deps.json size mismatch');

  const provenanceDeps = new Map((provenance.dependencies ?? []).map((mod) => [mod.module, mod]));
  for (const mod of deps.modules) {
    const provenanceMod = provenanceDeps.get(mod.module);
    assert(provenanceMod, `provenance missing dependency ${mod.module}`);
    assert(provenanceMod.version === mod.version, `provenance version mismatch for ${mod.module}`);
    assert(provenanceMod.cacheDir === mod.cacheDir, `provenance cacheDir mismatch for ${mod.module}`);
    assert(provenanceMod.commit === mod.commit, `provenance commit mismatch for ${mod.module}`);
    assert(typeof provenanceMod.dirty === 'boolean', `provenance dirty flag missing for ${mod.module}`);
    assert(provenanceMod.dirty === false, `provenance dirty flag must be false for ${mod.module}`);
    if (provenanceMod.source === 'external' && mod.module === 'github.com/vo-lang/voplay') {
      assert(provenanceMod.dirty === gitDirty(provenance.sourceRoots.voplay), `provenance dirty flag mismatch for ${mod.module}`);
    }
    assert(provenanceMod.filesDigest === packagedFilesDigest(mod.files), `provenance files digest mismatch for ${mod.module}`);

    const artifactDigests = new Map((provenanceMod.artifacts ?? []).map((artifact) => [artifact.url, artifact]));
    for (const artifact of mod.artifacts ?? []) {
      const provenanceArtifact = artifactDigests.get(artifact.url);
      assert(provenanceArtifact, `provenance missing artifact ${artifact.url}`);
      assert(provenanceArtifact.path === artifact.path, `provenance artifact path mismatch for ${artifact.url}`);
      const localPath = localPathForArtifact(artifact.url);
      const bytes = readFileSync(localPath);
      assert(provenanceArtifact.size === bytes.byteLength, `provenance artifact size mismatch for ${artifact.url}`);
      assert(provenanceArtifact.digest === sha256Digest(bytes), `provenance artifact digest mismatch for ${artifact.url}`);
    }
  }
}

const project = readJson(projectPath);
const deps = readJson(depsPath);
const provenance = readJson(provenancePath);
const quickplayTs = readFileSync(quickplayTsPath, 'utf8');

assert(project.schemaVersion === 1, 'project schemaVersion must be 1');
assert(deps.schemaVersion === 1, 'deps schemaVersion must be 1');
assert(project.name === expected.projectName, `project name must be ${expected.projectName}`);
assert(project.module === expected.projectModule, `project module must be ${expected.projectModule}`);
assert(Array.isArray(project.files) && project.files.length > 0, 'project files must be embedded');
assert(project.files.some((file) => file.path === 'main.vo'), 'project package must include main.vo');
const projectFiles = new Map(project.files.map((file) => [file.path, file]));
const runtimeAsset = projectFiles.get('assets/blockkart.vpak');
assert(runtimeAsset, 'project package must include assets/blockkart.vpak');
assert(moduleFileBytes(runtimeAsset).byteLength > 1024 * 1024, 'assets/blockkart.vpak must contain the runtime asset pack');
assert(!projectFiles.has('apps/studio/fixtures/blockkart/blockkart.vpak'), 'project package must not embed the Studio fixture path');

assert(Array.isArray(deps.modules) && deps.modules.length > 0, 'deps modules must be embedded');
const modules = new Map(deps.modules.map((mod) => [mod.module, mod]));
for (const moduleName of expected.requiredModules) {
  moduleByName(modules, moduleName);
}
validateProjectDependencyContracts(project, deps, provenance);
validateProvenance(project, deps);
validateProjectSourceCheckout(project, provenance);

for (const mod of deps.modules) {
  assert(typeof mod.cacheDir === 'string' && mod.cacheDir.length > 0, `${mod.module} must have cacheDir`);
  assert(Array.isArray(mod.files) && mod.files.length > 0, `${mod.module} must embed files`);
  for (const file of mod.files) {
    assert(!file.path.includes('.codex-release-stage'), `${mod.module} packaged transient file: ${file.path}`);
  }
  for (const artifact of mod.artifacts ?? []) {
    assert(typeof artifact.url === 'string', `${mod.module} artifact must have url`);
    const localPath = localPathForArtifact(artifact.url);
    assert(existsSync(localPath), `missing packaged artifact: ${artifact.url}`);
    assert(statSync(localPath).size > 0, `empty packaged artifact: ${artifact.url}`);
  }
  validatePackagedWebManifest(mod);
}

assert(quickplayTs.includes('staticPackageUrl'), 'quickplay manifests must be build-versioned');
assert(quickplayTs.includes('/quickplay/blockkart/project.json'), 'quickplay project URL missing');
assert(quickplayTs.includes('/quickplay/blockkart/deps.json'), 'quickplay deps URL missing');
for (const url of quickplayArtifactUrlsFromSource(quickplayTs)) {
  assert(false, `quickplay.ts must derive artifact URLs from deps.json, not hard-code ${url}`);
}
const declaredArtifactUrls = new Set();
for (const mod of deps.modules) {
  for (const artifact of mod.artifacts ?? []) {
    declaredArtifactUrls.add(artifact.url);
  }
}
const voplay = moduleByName(modules, 'github.com/vo-lang/voplay');
validateVoplayHostWakeKeyContract(voplay);
for (const artifact of requiredVoplayArtifacts(voplay)) {
  assert(declaredArtifactUrls.has(artifact.url), `voplay artifact is not declared: ${artifact.url}`);
}

writeReport('ok', {
  project: {
    module: project.module,
    commit: project.commit ?? null,
    provenanceCommit: provenance.project?.commit ?? null,
  },
  dependencies: (deps.modules ?? []).map((mod) => ({
    module: mod.module,
    version: mod.version,
    commit: mod.commit ?? null,
  })),
});
console.log('quickplay validate: ok');
