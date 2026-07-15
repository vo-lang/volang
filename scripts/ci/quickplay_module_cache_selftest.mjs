#!/usr/bin/env node
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { artifactCachePath, moduleCacheDir } from './quickplay_artifact_paths.mjs';
import { canonicalWebSourceDigest } from './quickplay_web_manifest_contract.mjs';
import { renderVoLockV2 } from './vo_lock_v2.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const cacheScript = path.join(root, 'scripts', 'ci', 'quickplay_module_cache.mjs');
const wasmTarget = 'wasm32-unknown-unknown';
const temporaryRoot = mkdtempSync(path.join(os.tmpdir(), 'volang-quickplay-cache-'));

function sha256(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function write(rootDir, relative, bytes) {
  const destination = path.join(rootDir, ...relative.split('/'));
  mkdirSync(path.dirname(destination), { recursive: true });
  writeFileSync(destination, bytes);
}

function run(command, args, options = {}) {
  const result = spawnSync(command, args, {
    cwd: options.cwd ?? root,
    env: options.env ?? process.env,
    encoding: 'utf8',
    maxBuffer: 16 * 1024 * 1024,
  });
  if (options.allowFailure) return result;
  assert.equal(
    result.status,
    0,
    `${command} ${args.join(' ')} failed:\n${result.stderr || result.stdout}`,
  );
  return result;
}

function initializeRepository(repoRoot, files) {
  mkdirSync(repoRoot, { recursive: true });
  for (const [relative, bytes] of Object.entries(files)) write(repoRoot, relative, bytes);
  run('git', ['init', '--quiet'], { cwd: repoRoot });
  run('git', ['config', 'user.name', 'Quickplay Cache Selftest'], { cwd: repoRoot });
  run('git', ['config', 'user.email', 'quickplay-cache-selftest@example.invalid'], { cwd: repoRoot });
  run('git', ['add', '--all'], { cwd: repoRoot });
  run('git', ['commit', '--quiet', '-m', 'fixture'], { cwd: repoRoot });
  return run('git', ['rev-parse', 'HEAD'], { cwd: repoRoot }).stdout.trim();
}

function jsonBytes(value) {
  return Buffer.from(`${JSON.stringify(value, null, 2)}\n`, 'utf8');
}

function sourceEntries(files) {
  return Object.entries(files)
    .map(([filePath, bytes]) => {
      const content = Buffer.from(bytes);
      return { path: filePath, size: content.byteLength, digest: sha256(content) };
    })
    .sort((left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path)));
}

function createPublishedModule({ shortName, version, withArtifact = false }) {
  const module = `github.com/vo-lang/${shortName}`;
  const repoRoot = path.join(temporaryRoot, 'repos', shortName);
  const artifactBytes = withArtifact ? Buffer.from([0, 97, 115, 109, 1, 0, 0, 0]) : null;
  const artifactName = `${shortName}.wasm`;
  const voMod = withArtifact
    ? [
        `module ${module}`,
        'vo ^0.1.0',
        '',
        '[extension]',
        `name = "${shortName}"`,
        'include = []',
        '',
        '[extension.wasm]',
        'type = "standalone"',
        `wasm = "${artifactName}"`,
        `local_wasm = "web-artifacts/${artifactName}"`,
        '',
      ].join('\n')
    : `module ${module}\nvo ^0.1.0\n`;
  const files = {
    'main.vo': Buffer.from(`package ${shortName}\n\nfunc Ready() bool { return true }\n`, 'utf8'),
    'vo.mod': Buffer.from(voMod, 'utf8'),
  };
  const commit = initializeRepository(repoRoot, files);
  const source = sourceEntries(files);
  const sourceDigest = canonicalWebSourceDigest(source);
  const artifacts = withArtifact
    ? [{
        kind: 'extension-wasm',
        target: wasmTarget,
        name: artifactName,
        size: artifactBytes.byteLength,
        digest: sha256(artifactBytes),
      }]
    : [];
  const extension = withArtifact
    ? {
        name: shortName,
        include: [],
        wasm: {
          kind: 'Standalone',
          wasm: artifactName,
          local_wasm: `web-artifacts/${artifactName}`,
        },
        web: null,
      }
    : null;
  const web = {
    schema_version: 1,
    module,
    version,
    commit,
    module_root: '.',
    vo: '^0.1.0',
    require: [],
    source_digest: sourceDigest,
    source,
    artifacts: artifacts.map((artifact) => ({
      ...artifact,
      path: `web-artifacts/${artifact.name}`,
    })),
    web: null,
    extension,
  };
  const webBytes = jsonBytes(web);
  const sourceArchiveDigest = sha256(Buffer.from(`${module}@${version} source archive`, 'utf8'));
  const release = {
    schema_version: 1,
    module,
    version,
    commit,
    module_root: '.',
    vo: '^0.1.0',
    require: [],
    source: {
      name: `${shortName}-${version}.tar.gz`,
      size: 123,
      digest: sourceArchiveDigest,
      files_size: source.reduce((total, entry) => total + entry.size, 0),
      files_digest: sourceDigest,
    },
    web_manifest: {
      size: webBytes.byteLength,
      digest: sha256(webBytes),
    },
    artifacts,
  };
  const releaseBytes = jsonBytes(release);
  const assetDir = path.join(
    temporaryRoot,
    'assets',
    ...module.split('/'),
    version,
  );
  write(assetDir, 'vo.web.json', webBytes);
  write(assetDir, 'vo.release.json', releaseBytes);
  if (artifactBytes) write(assetDir, artifactName, artifactBytes);
  return {
    module,
    shortName,
    version,
    repoRoot,
    commit,
    files,
    web,
    webBytes,
    release,
    releaseBytes,
    source: sourceArchiveDigest,
    releaseManifest: sha256(releaseBytes),
    artifacts,
    artifactBytes,
    assetDir,
  };
}

function lockModule(published) {
  return {
    path: published.module,
    version: published.version,
    vo: '^0.1.0',
    commit: published.commit,
    release_manifest: published.releaseManifest,
    source: published.source,
    deps: [],
    artifacts: published.artifacts,
  };
}

function writeLock(blockKartRoot, modules) {
  write(blockKartRoot, 'vo.lock', renderVoLockV2({
    version: 2,
    created_by: 'quickplay module cache selftest',
    root: { module: 'github.com/vo-lang/blockkart', vo: '^0.1.0' },
    resolved: modules.map(lockModule),
  }));
}

function runCache(fixture, cacheName) {
  const cacheRoot = path.join(temporaryRoot, cacheName);
  const result = run(process.execPath, [cacheScript], {
    allowFailure: true,
    env: {
      ...process.env,
      BLOCKKART_ROOT: fixture.blockKartRoot,
      VOGUI_ROOT: fixture.modules[0].repoRoot,
      VOPACK_ROOT: fixture.modules[1].repoRoot,
      VOPLAY_ROOT: fixture.modules[2].repoRoot,
      VO_MOD_CACHE: cacheRoot,
      QUICKPLAY_RELEASE_ASSET_ROOT: fixture.assetRoot,
    },
  });
  return { cacheRoot, result, output: `${result.stdout}\n${result.stderr}` };
}

function assertFailure(runResult, pattern) {
  assert.notEqual(runResult.result.status, 0, 'cache provisioning unexpectedly succeeded');
  assert.match(runResult.output, pattern);
}

function restorePublishedAssets(published) {
  write(published.assetDir, 'vo.web.json', published.webBytes);
  write(published.assetDir, 'vo.release.json', published.releaseBytes);
  if (published.artifactBytes) {
    write(published.assetDir, published.artifacts[0].name, published.artifactBytes);
  }
}

try {
  const modules = [
    createPublishedModule({ shortName: 'vogui', version: 'v1.2.3', withArtifact: true }),
    createPublishedModule({ shortName: 'vopack', version: 'v1.2.4' }),
    createPublishedModule({ shortName: 'voplay', version: 'v1.2.5' }),
  ];
  const blockKartRoot = path.join(temporaryRoot, 'repos', 'blockkart');
  initializeRepository(blockKartRoot, {
    'vo.mod': Buffer.from('module github.com/vo-lang/blockkart\nvo ^0.1.0\n', 'utf8'),
  });
  writeLock(blockKartRoot, modules);
  const fixture = {
    modules,
    blockKartRoot,
    assetRoot: path.join(temporaryRoot, 'assets'),
  };

  const success = runCache(fixture, 'cache-success');
  assert.equal(success.result.status, 0, success.output);
  assert.match(success.output, /quickplay module cache: ok/);
  for (const published of modules) {
    const moduleDir = path.join(
      success.cacheRoot,
      ...moduleCacheDir(published.module, published.version).split('/'),
    );
    assert.equal(readFileSync(path.join(moduleDir, '.vo-version'), 'utf8'), `${published.version}\n`);
    assert.equal(readFileSync(path.join(moduleDir, '.vo-source-digest'), 'utf8'), `${published.source}\n`);
    assert.deepEqual(readFileSync(path.join(moduleDir, 'vo.release.json')), published.releaseBytes);
    assert.deepEqual(readFileSync(path.join(moduleDir, 'vo.web.json')), published.webBytes);
    if (published.artifactBytes) {
      const artifact = published.artifacts[0];
      assert.deepEqual(
        readFileSync(path.join(moduleDir, 'web-artifacts', artifact.name)),
        published.artifactBytes,
      );
      assert.deepEqual(
        readFileSync(path.join(moduleDir, ...artifactCachePath(artifact).split('/'))),
        published.artifactBytes,
      );
    }
  }

  const target = modules[0];
  write(target.assetDir, 'vo.release.json', Buffer.concat([target.releaseBytes, Buffer.from(' ')]));
  assertFailure(runCache(fixture, 'cache-tampered-release'), /does not match authenticated metadata/);
  restorePublishedAssets(target);

  const legacyRelease = structuredClone(target.release);
  delete legacyRelease.web_manifest;
  delete legacyRelease.source.files_size;
  delete legacyRelease.source.files_digest;
  const legacyBytes = jsonBytes(legacyRelease);
  write(target.assetDir, 'vo.release.json', legacyBytes);
  target.releaseManifest = sha256(legacyBytes);
  writeLock(blockKartRoot, modules);
  assertFailure(
    runCache(fixture, 'cache-legacy-release'),
    /legacy release contract without a valid web_manifest binding/,
  );

  restorePublishedAssets(target);
  const duplicateReleaseBytes = Buffer.from(
    target.releaseBytes.toString('utf8').replace(
      '    "files_digest":',
      `    "files_digest": "sha256:${'0'.repeat(64)}",\n    "files_digest":`,
    ),
    'utf8',
  );
  assert.notDeepEqual(duplicateReleaseBytes, target.releaseBytes);
  write(target.assetDir, 'vo.release.json', duplicateReleaseBytes);
  target.releaseManifest = sha256(duplicateReleaseBytes);
  writeLock(blockKartRoot, modules);
  assertFailure(
    runCache(fixture, 'cache-duplicate-release'),
    /duplicate object key "files_digest"/,
  );

  restorePublishedAssets(target);
  const duplicateWebBytes = Buffer.from(
    target.webBytes.toString('utf8').replace(
      '      "digest":',
      `      "digest": "sha256:${'0'.repeat(64)}",\n      "digest":`,
    ),
    'utf8',
  );
  assert.notDeepEqual(duplicateWebBytes, target.webBytes);
  const webBoundRelease = structuredClone(target.release);
  webBoundRelease.web_manifest = {
    size: duplicateWebBytes.byteLength,
    digest: sha256(duplicateWebBytes),
  };
  const webBoundReleaseBytes = jsonBytes(webBoundRelease);
  write(target.assetDir, 'vo.web.json', duplicateWebBytes);
  write(target.assetDir, 'vo.release.json', webBoundReleaseBytes);
  target.releaseManifest = sha256(webBoundReleaseBytes);
  writeLock(blockKartRoot, modules);
  assertFailure(runCache(fixture, 'cache-duplicate-web'), /duplicate object key "digest"/);

  assert.equal(existsSync(success.cacheRoot), true);
  process.stdout.write('quickplay module cache selftest: ok\n');
} finally {
  rmSync(temporaryRoot, { recursive: true, force: true });
}
