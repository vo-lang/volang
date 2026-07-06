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
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const outDir = resolve(argValue('--out-dir') || process.env.QUICKPLAY_SOURCE_AUDIT_OUT_DIR || join(root, 'target/quickplay-source-audit'));

const dependencyRepos = new Map([
  ['github.com/vo-lang/vogui', requireRepoRoot('VOGUI_ROOT', 'vogui')],
  ['github.com/vo-lang/voplay', requireRepoRoot('VOPLAY_ROOT', 'voplay')],
  ['github.com/vo-lang/vopack', requireRepoRoot('VOPACK_ROOT', 'vopack')],
]);

function argValue(name) {
  const index = process.argv.indexOf(name);
  return index === -1 ? '' : (process.argv[index + 1] ?? '');
}

function sha256(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function readJson(path) {
  return JSON.parse(readFileSync(path, 'utf8'));
}

function fileBytes(file) {
  if (file?.content != null) return Buffer.from(file.content, 'utf8');
  if (file?.contentBase64 != null) return Buffer.from(file.contentBase64, 'base64');
  return null;
}

function issue(issues, owner, subsystem, severity, message, evidence = {}) {
  issues.push({ owner, subsystem, severity, message, evidence });
}

function git(args, cwd) {
  return execFileSync('git', args, { cwd, encoding: 'utf8', maxBuffer: 50 * 1024 * 1024 }).trim();
}

function tryGit(args, cwd) {
  try {
    return { ok: true, stdout: git(args, cwd) };
  } catch (error) {
    return {
      ok: false,
      error: String(error?.stderr || error?.message || error).trim(),
    };
  }
}

function tryGitBytes(args, cwd) {
  try {
    return { ok: true, bytes: execFileSync('git', args, { cwd, maxBuffer: 50 * 1024 * 1024 }) };
  } catch (error) {
    return {
      ok: false,
      error: String(error?.stderr || error?.message || error).trim(),
    };
  }
}

function parseVoLockValue(value) {
  if (value.startsWith('"') || value.startsWith('[')) {
    return JSON.parse(value);
  }
  if (/^\d+$/.test(value)) return Number(value);
  return value;
}

function parseVoLockResolved(source) {
  const resolved = [];
  let current = null;
  let artifact = null;
  for (const rawLine of source.split(/\r?\n/)) {
    const line = rawLine.trim();
    if (!line || line.startsWith('#')) continue;
    if (line === '[[resolved]]') {
      current = { artifacts: [] };
      artifact = null;
      resolved.push(current);
      continue;
    }
    if (line === '[[resolved.artifact]]') {
      if (!current) continue;
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

function sourceSetDigest(entries) {
  return sha256(Buffer.from(JSON.stringify(entries), 'utf8'));
}

function packagedFilesDigest(files) {
  const entries = (files ?? [])
    .map((file) => {
      const bytes = fileBytes(file);
      return {
        digest: sha256(bytes),
        path: file.path,
        size: bytes.byteLength,
      };
    })
    .sort((a, b) => a.path.localeCompare(b.path));
  return sourceSetDigest(entries);
}

function validatePackagedLockRewrite(project, sourceBytes, packagedBytes, issues) {
  const rewrite = project.lockRewrite;
  if (!rewrite || rewrite.schemaVersion !== 1 || rewrite.path !== 'vo.lock') {
    issue(issues, 'BlockKart', 'LockRewrite', 'P0', 'Packaged vo.lock differs from source without a structured lockRewrite contract', {
      expected: 'project.lockRewrite schemaVersion=1 path=vo.lock',
    });
    return false;
  }
  const sourceDigest = sha256(sourceBytes);
  const packagedDigest = sha256(packagedBytes);
  let ok = true;
  if (rewrite.sourceDigest !== sourceDigest) {
    issue(issues, 'BlockKart', 'LockRewrite', 'P0', 'lockRewrite sourceDigest does not match source vo.lock', { expected: sourceDigest, found: rewrite.sourceDigest ?? null });
    ok = false;
  }
  if (rewrite.packagedDigest !== packagedDigest) {
    issue(issues, 'BlockKart', 'LockRewrite', 'P0', 'lockRewrite packagedDigest does not match packaged vo.lock', { expected: packagedDigest, found: rewrite.packagedDigest ?? null });
    ok = false;
  }
  if (!Array.isArray(rewrite.modules) || rewrite.modules.length === 0) {
    issue(issues, 'BlockKart', 'LockRewrite', 'P0', 'lockRewrite must list rewritten dependency modules', { modules: rewrite.modules ?? null });
    ok = false;
  }
  return ok;
}

function listSourceFiles(projectRoot, extension) {
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
          digest: sha256(bytes),
          size: bytes.byteLength,
        });
      }
    }
  };
  visit(projectRoot);
  return files.sort((a, b) => a.path.localeCompare(b.path));
}

function shouldValidateEmbeddedSource(file) {
  if (file.content == null) return false;
  return !['.vo-source-digest', '.vo-version', 'vo.release.json', 'vo.web.json'].includes(file.path);
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

function repoSourceBytesForAudit(mod, files, entry, sourceBytes) {
  if (entry.path !== 'vo.mod') {
    return sourceBytes;
  }
  const packaged = files.get('vo.mod');
  if (!packaged?.content) {
    return sourceBytes;
  }
  const rewritten = Buffer.from(dropNativeExtensionTables(sourceBytes.toString('utf8')), 'utf8');
  const packagedBytes = Buffer.from(packaged.content, 'utf8');
  if (rewritten.byteLength === packagedBytes.byteLength && sha256(rewritten) === sha256(packagedBytes)) {
    return rewritten;
  }
  return sourceBytes;
}

function auditBlockKart(project, provenance, issues) {
  if (!existsSync(blockKartRoot)) {
    issue(issues, 'BlockKart', 'Source', 'P0', 'BlockKart source checkout is missing', { blockKartRoot });
    return;
  }
  if (provenance?.schemaVersion !== 2) {
    issue(issues, 'BlockKart', 'Provenance', 'P0', 'Quickplay provenance must use schema v2', { schemaVersion: provenance?.schemaVersion ?? null });
  }
  if (provenance?.project?.commit && provenance.project.commit !== project.commit) {
    issue(issues, 'BlockKart', 'Provenance', 'P0', 'Project provenance commit does not match project package', { expected: project.commit, found: provenance.project.commit });
  }
  if (provenance?.project?.filesDigest && provenance.project.filesDigest !== packagedFilesDigest(project.files)) {
    issue(issues, 'BlockKart', 'Provenance', 'P0', 'Project provenance filesDigest does not match project package files', { expected: packagedFilesDigest(project.files), found: provenance.project.filesDigest });
  }
  if (JSON.stringify(provenance?.project?.lockRewrite ?? null) !== JSON.stringify(project.lockRewrite ?? null)) {
    issue(issues, 'BlockKart', 'Provenance', 'P0', 'Project provenance lockRewrite must match project package lockRewrite', {
      provenance: provenance?.project?.lockRewrite ?? null,
      project: project.lockRewrite ?? null,
    });
  }
  const inside = tryGit(['rev-parse', '--is-inside-work-tree'], blockKartRoot);
  if (!inside.ok || inside.stdout !== 'true') {
    issue(issues, 'BlockKart', 'Source', 'P0', 'BlockKart source path is not a git checkout', { blockKartRoot, error: inside.error });
    return;
  }
  const head = git(['rev-parse', 'HEAD'], blockKartRoot);
  if (head !== project.commit) {
    issue(issues, 'BlockKart', 'Source', 'P0', 'BlockKart source HEAD does not match packaged commit', { expected: project.commit, found: head });
  }
  const status = git(['status', '--porcelain'], blockKartRoot);
  const dirty = status !== '';
  if (dirty) {
    issue(issues, 'BlockKart', 'Source', 'P0', 'BlockKart source checkout must be clean for strict quickplay source audit', { status });
  }
  if (dirty && provenance?.project?.dirty !== true) {
    issue(issues, 'BlockKart', 'Source', 'P0', 'BlockKart source checkout is dirty but provenance project.dirty is not true', { status, provenanceDirty: provenance?.project?.dirty ?? null });
  }
  if (!dirty && provenance?.project?.dirty === true) {
    issue(issues, 'BlockKart', 'Source', 'P0', 'BlockKart provenance project.dirty is true but source checkout is clean');
  }
  if (provenance?.project?.dirty === true) {
    issue(issues, 'BlockKart', 'Provenance', 'P0', 'BlockKart provenance project.dirty must be false for strict source audit');
  }

  const packaged = new Map((project.files ?? []).map((file) => [file.path, file]));
  const sourceFiles = listSourceFiles(blockKartRoot, '.vo');
  const packagedVo = new Set([...packaged.keys()].filter((file) => file.endsWith('.vo')));
  const sourceAllowlist = new Map((provenance?.project?.sourceAllowlist ?? project.sourceAllowlist ?? [])
    .map((entry) => [entry.path, entry]));
  const unpackaged = sourceFiles
    .filter((entry) => !packagedVo.has(entry.path))
    .filter((entry) => !sourceAllowlist.has(entry.path));
  if (unpackaged.length > 0) {
    issue(issues, 'BlockKart', 'SourceCoverage', 'P0', 'BlockKart source files are missing from quickplay package and have no allowlist reason', { unpackaged });
  }
  const invalidAllowlist = [...sourceAllowlist.values()].filter((entry) => (
    !entry?.path
    || typeof entry.reason !== 'string'
    || entry.reason.trim().length < 12
    || typeof entry.expiresAt !== 'string'
    || Number.isNaN(Date.parse(entry.expiresAt))
    || Date.parse(entry.expiresAt) <= Date.now()
    || !sourceFiles.some((source) => source.path === entry.path)
  ));
  if (invalidAllowlist.length > 0) {
    issue(issues, 'BlockKart', 'SourceCoverage', 'P0', 'BlockKart quickplay source allowlist entries must name an existing source file and include a reason plus future expiresAt', { invalidAllowlist });
  }
  const manifestSourceFiles = project.sourceFiles ?? provenance?.project?.sourceFiles ?? null;
  if (!Array.isArray(manifestSourceFiles)) {
    issue(issues, 'BlockKart', 'SourceCoverage', 'P0', 'Quickplay package manifest must record BlockKart source file digest list', { expected: 'project.sourceFiles[] or provenance.project.sourceFiles[]' });
  } else if (sourceSetDigest(manifestSourceFiles) !== sourceSetDigest(sourceFiles)) {
    issue(issues, 'BlockKart', 'SourceCoverage', 'P0', 'Quickplay package source digest list does not match BlockKart/**/*.vo', {
      expected: sourceSetDigest(sourceFiles),
      found: sourceSetDigest(manifestSourceFiles),
    });
  }
  if (provenance?.project?.sourceFilesDigest !== sourceSetDigest(sourceFiles)) {
    issue(issues, 'BlockKart', 'SourceCoverage', 'P0', 'Quickplay provenance sourceFilesDigest does not match BlockKart/**/*.vo', {
      expected: sourceSetDigest(sourceFiles),
      found: provenance?.project?.sourceFilesDigest ?? null,
    });
  }
  for (const [path, file] of packaged) {
    const sourcePath = join(blockKartRoot, path);
    if (!existsSync(sourcePath)) {
      issue(issues, 'BlockKart', 'Source', 'P0', 'Packaged BlockKart file is missing from source checkout', { path });
      continue;
    }
    const source = readFileSync(sourcePath);
    const packagedBytes = fileBytes(file);
    if (!packagedBytes) {
      issue(issues, 'BlockKart', 'Source', 'P0', 'Packaged BlockKart file has no bytes', { path });
      continue;
    }
    if (source.byteLength !== packagedBytes.byteLength || sha256(source) !== sha256(packagedBytes)) {
      if (path === 'vo.lock' && validatePackagedLockRewrite(project, source, packagedBytes, issues)) {
        continue;
      }
      issue(issues, 'BlockKart', 'Source', 'P0', 'Packaged BlockKart file differs from source checkout', {
        path,
        source: { digest: sha256(source), size: source.byteLength },
        packaged: { digest: sha256(packagedBytes), size: packagedBytes.byteLength },
      });
    }
  }
}

function auditDependencyRelease(mod, locked, provenanceDependency, issues) {
  const dependencyDirty = provenanceDependency?.dirty === true;
  if (dependencyDirty) {
    issue(issues, mod.module, 'Provenance', 'P0', 'Dependency provenance dirty flag must be false for strict source audit', { module: mod.module });
  }
  const files = new Map((mod.files ?? []).map((file) => [file.path, file]));
  const releaseFile = files.get('vo.release.json');
  const webFile = files.get('vo.web.json');
  if (!webFile) {
    issue(issues, mod.module, 'Release', 'P0', 'Dependency package is missing vo.web.json', { module: mod.module, version: mod.version });
    return;
  }
  let web;
  try {
    web = JSON.parse(webFile.content);
  } catch (error) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.web.json is invalid JSON', { error: error.message });
    return;
  }

  if (web.module !== mod.module) issue(issues, mod.module, 'Release', 'P0', 'vo.web.json module mismatch', { expected: mod.module, found: web.module });
  if (web.version !== mod.version) issue(issues, mod.module, 'Release', 'P0', 'vo.web.json version mismatch', { expected: mod.version, found: web.version });
  if (locked?.commit && web.commit !== locked.commit) issue(issues, mod.module, 'Release', 'P0', 'vo.web.json commit mismatch', { expected: locked.commit, found: web.commit });
  if (!Array.isArray(web.source)) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.web.json source is not an array');
  } else if (sourceSetDigest(web.source) !== web.source_digest) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.web.json source_digest mismatch', { expected: web.source_digest, found: sourceSetDigest(web.source) });
  }

  if (releaseFile) {
    let release = null;
    try {
      release = JSON.parse(releaseFile.content);
    } catch (error) {
      issue(issues, mod.module, 'Release', 'P0', 'vo.release.json is invalid JSON', { error: error.message });
    }
    const releaseDigest = sha256(fileBytes(releaseFile));
    if (locked?.release_manifest && releaseDigest !== locked.release_manifest) {
      issue(issues, mod.module, 'Release', 'P0', 'vo.release.json digest does not match project vo.lock', { expected: locked.release_manifest, found: releaseDigest });
    }
    if (release) {
      if (release.module !== mod.module) issue(issues, mod.module, 'Release', 'P0', 'vo.release.json module mismatch', { expected: mod.module, found: release.module });
      if (release.version !== mod.version) issue(issues, mod.module, 'Release', 'P0', 'vo.release.json version mismatch', { expected: mod.version, found: release.version });
      if (locked?.commit && release.commit !== locked.commit) issue(issues, mod.module, 'Release', 'P0', 'vo.release.json commit mismatch', { expected: locked.commit, found: release.commit });
      if (locked?.source && release.source?.digest !== locked.source) {
        issue(issues, mod.module, 'Release', 'P0', 'vo.release.json source digest does not match project vo.lock', { expected: locked.source, found: release.source?.digest });
      }
      const marker = files.get('.vo-source-digest')?.content?.trim() ?? '';
      if (locked?.source && marker !== locked.source) {
        issue(issues, mod.module, 'Release', 'P0', '.vo-source-digest does not match project vo.lock', { expected: locked.source, found: marker });
      }
    }
  }

  const sourceByPath = new Map((web.source ?? []).map((entry) => [entry.path, entry]));
  for (const file of mod.files ?? []) {
    if (!shouldValidateEmbeddedSource(file)) continue;
    const entry = sourceByPath.get(file.path);
    if (!entry) {
      issue(issues, mod.module, 'Release', 'P0', 'Embedded source file is not declared by vo.web.json', { path: file.path });
      continue;
    }
    const bytes = fileBytes(file);
    const digest = sha256(bytes);
    if (bytes.byteLength !== entry.size || digest !== entry.digest) {
      issue(issues, mod.module, 'Release', 'P0', 'Embedded source file differs from vo.web.json source entry', {
        path: file.path,
        expected: { digest: entry.digest, size: entry.size },
        found: { digest, size: bytes.byteLength },
      });
    }
  }
}

function auditDependencyRepoCommit(mod, locked, provenanceDependency, issues) {
  if (!locked?.commit) return;
  const repoRoot = dependencyRepos.get(mod.module);
  if (!repoRoot || !existsSync(repoRoot)) {
    issue(issues, mod.module, 'SourceRepo', 'P1', 'Dependency source repo checkout is missing', { repoRoot });
    return;
  }
  const head = tryGit(['rev-parse', 'HEAD'], repoRoot);
  if (provenanceDependency?.dirty === true) {
    if (!head.ok || head.stdout !== locked.commit) {
      issue(issues, mod.module, 'SourceRepo', 'P0', 'Dirty dependency source repo HEAD does not match locked commit', { expected: locked.commit, found: head.ok ? head.stdout : null, repoRoot, error: head.error });
      return;
    }
  }
  const commitType = tryGit(['cat-file', '-t', locked.commit], repoRoot);
  if (!commitType.ok || commitType.stdout !== 'commit') {
    issue(issues, mod.module, 'SourceRepo', 'P1', 'Dependency source repo does not contain locked commit', { commit: locked.commit, repoRoot, error: commitType.error });
    return;
  }
  const files = new Map((mod.files ?? []).map((file) => [file.path, file]));
  const webFile = files.get('vo.web.json');
  if (!webFile) return;
  let web;
  try {
    web = JSON.parse(webFile.content);
  } catch {
    return;
  }
  for (const entry of web.source ?? []) {
    const source = tryGitBytes(['show', `${locked.commit}:${entry.path}`], repoRoot);
    if (!source.ok) {
      issue(issues, mod.module, 'SourceRepo', 'P0', 'Locked dependency commit is missing release source file', { commit: locked.commit, path: entry.path });
      continue;
    }
    const sourceBytes = source.bytes;
    const expectedBytes = repoSourceBytesForAudit(mod, files, entry, sourceBytes);
    if (expectedBytes.byteLength !== entry.size || sha256(expectedBytes) !== entry.digest) {
      issue(issues, mod.module, 'SourceRepo', 'P0', 'Locked dependency commit source differs from vo.web.json source entry', {
        commit: locked.commit,
        path: entry.path,
        expected: { digest: entry.digest, size: entry.size },
        found: { digest: sha256(expectedBytes), size: expectedBytes.byteLength },
      });
    }
  }
}

function markdown(report) {
  const lines = ['# Quickplay Source Audit', '', `- Status: ${report.status}`, `- Issues: ${report.issues.length}`, `- Quickplay dir: ${report.quickplayDir}`, `- BlockKart root: ${report.blockKartRoot}`, ''];
  if (report.issues.length > 0) {
    lines.push('## Issues', '');
    for (const item of report.issues) {
      lines.push(`- ${item.severity} ${item.owner}/${item.subsystem}: ${item.message}`);
      const evidence = JSON.stringify(item.evidence ?? {});
      if (evidence !== '{}') lines.push(`  - evidence: \`${evidence}\``);
    }
    lines.push('');
  }
  return `${lines.join('\n')}\n`;
}

const project = readJson(join(quickplayDir, 'project.json'));
const deps = readJson(join(quickplayDir, 'deps.json'));
const provenance = readJson(join(quickplayDir, 'provenance.json'));
const issues = [];
auditBlockKart(project, provenance, issues);
const projectFiles = new Map((project.files ?? []).map((file) => [file.path, file]));
const locked = parseVoLockResolved(fileBytes(projectFiles.get('vo.lock')).toString('utf8'));
const provenanceDependencies = new Map((provenance.dependencies ?? []).map((entry) => [entry.module, entry]));
for (const mod of deps.modules ?? []) {
  const lockedModule = locked.get(mod.module);
  if (!lockedModule) {
    issue(issues, mod.module, 'Lock', 'P0', 'Dependency module is missing from project vo.lock', { version: mod.version });
    continue;
  }
  const provenanceDependency = provenanceDependencies.get(mod.module);
  auditDependencyRelease(mod, lockedModule, provenanceDependency, issues);
  auditDependencyRepoCommit(mod, lockedModule, provenanceDependency, issues);
}

const generatedAt = new Date().toISOString();
const report = {
  schemaVersion: 1,
  kind: 'quickplay.sourceAuditReport',
  status: issues.length === 0 ? 'ok' : 'failed',
  generatedAt,
  freshEvidence: sourceBoundEvidence({
    gate: 'quickplay-source-audit',
    generatedAt,
    root,
    repos: [
      { name: 'volang', root },
      { name: 'BlockKart', root: blockKartRoot },
      ...[...dependencyRepos.entries()].map(([module, repoRoot]) => ({ name: module, root: repoRoot })),
    ],
    gateFiles: [
      'scripts/ci/quickplay_source_audit.mjs',
      'scripts/ci/repo_roots.mjs',
      'scripts/ci/source_bound_evidence.mjs',
      'apps/studio/scripts/package_blockkart_quickplay.mjs',
      'eng/tasks.toml',
      'eng/artifacts.toml',
      'eng/project.toml',
    ],
    artifacts: [quickplayDir],
  }),
  quickplayDir,
  blockKartRoot,
  checkedAt: generatedAt,
  sourceFiles: listSourceFiles(blockKartRoot, '.vo'),
  packagedSources: (project.files ?? [])
    .filter((file) => file.path.endsWith('.vo'))
    .map((file) => {
      const bytes = fileBytes(file);
      return {
        path: file.path,
        digest: sha256(bytes),
        size: bytes.byteLength,
      };
    })
    .sort((a, b) => a.path.localeCompare(b.path)),
  packageManifest: {
    sourceFiles: project.sourceFiles ?? null,
    sourceAllowlist: project.sourceAllowlist ?? provenance.project?.sourceAllowlist ?? [],
  },
  issues,
};

mkdirSync(outDir, { recursive: true });
writeFileSync(join(outDir, 'quickplay-source-audit.json'), `${JSON.stringify(report, null, 2)}\n`);
writeFileSync(join(outDir, 'quickplay-source-audit.md'), markdown(report));

if (issues.length > 0) {
  console.error(`quickplay source audit: failed with ${issues.length} issue(s); wrote ${outDir}`);
  process.exit(1);
}
console.log(`quickplay source audit: ok; wrote ${outDir}`);
