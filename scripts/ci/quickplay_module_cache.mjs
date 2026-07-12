#!/usr/bin/env node
import { execFileSync, spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { copyFileSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const vopackRoot = requireRepoRoot('VOPACK_ROOT', 'vopack');
const cacheRoot = path.resolve(
  process.env.VO_MOD_CACHE ?? path.join(root, 'target', 'quickplay-module-cache', 'mod'),
);

mkdirSync(cacheRoot, { recursive: true });
const projectRoot = path.join(root, 'target', 'quickplay-module-cache', 'project');
mkdirSync(projectRoot, { recursive: true });
copyFileSync(path.join(blockKartRoot, 'vo.mod'), path.join(projectRoot, 'vo.mod'));
const lockSource = readFileSync(path.join(blockKartRoot, 'vo.lock'), 'utf8');
const lockSections = lockSource.split(/\n(?=\[\[resolved\]\]\n)/);
const releaseModules = new Set(['github.com/vo-lang/vogui']);
const resolved = lockSections.slice(1).filter((section) => {
  const modulePath = section.match(/^path = "([^"]+)"$/m)?.[1];
  return modulePath != null && releaseModules.has(modulePath);
});
if (resolved.length !== releaseModules.size) {
  throw new Error('BlockKart vo.lock must contain the locked vogui module');
}
writeFileSync(
  path.join(projectRoot, 'vo.lock'),
  `${lockSections[0].trimEnd()}\n\n${resolved.join('\n').trimEnd()}\n`,
);
const result = spawnSync(
  'cargo',
  ['run', '-q', '-p', 'vo', '--', 'mod', 'download', projectRoot],
  {
    cwd: root,
    env: { ...process.env, VO_MOD_CACHE: cacheRoot },
    stdio: 'inherit',
  },
);
if (result.error) {
  throw result.error;
}
if (result.status !== 0) {
  throw new Error(`quickplay module cache download failed with status ${result.status}`);
}

const vopackSection = lockSections.slice(1).find(
  (section) => section.match(/^path = "([^"]+)"$/m)?.[1] === 'github.com/vo-lang/vopack',
);
const vopackVersion = vopackSection?.match(/^version = "([^"]+)"$/m)?.[1];
const vopackCommit = vopackSection?.match(/^commit = "([0-9a-f]{40})"$/m)?.[1];
const vopackReleaseDigest = vopackSection?.match(/^release_manifest = "(sha256:[0-9a-f]{64})"$/m)?.[1];
const vopackSourceDigest = vopackSection?.match(/^source = "(sha256:[0-9a-f]{64})"$/m)?.[1];
if (!vopackVersion || !vopackCommit || !vopackReleaseDigest || !vopackSourceDigest) {
  throw new Error('BlockKart vo.lock must contain a versioned vopack commit');
}
const vopackHead = execFileSync('git', ['rev-parse', 'HEAD'], {
  cwd: vopackRoot,
  encoding: 'utf8',
}).trim();
const vopackStatus = execFileSync('git', ['status', '--porcelain'], {
  cwd: vopackRoot,
  encoding: 'utf8',
}).trim();
if (vopackHead !== vopackCommit || vopackStatus !== '') {
  throw new Error(`vopack checkout must be clean at locked commit ${vopackCommit}`);
}
const vopackCache = path.join(
  cacheRoot,
  'github.com@vo-lang@vopack',
  vopackVersion,
);
mkdirSync(vopackCache, { recursive: true });
const vopackStage = path.join(root, 'target', 'quickplay-module-cache', 'vopack-stage');
const stage = spawnSync('cargo', [
  'run', '-q', '-p', 'vo', '--', 'release', 'stage', vopackRoot,
  '--version', vopackVersion,
  '--commit', vopackCommit,
  '--out-dir', vopackStage,
], {
  cwd: root,
  stdio: 'inherit',
});
if (stage.error) {
  throw stage.error;
}
if (stage.status !== 0) {
  throw new Error(`locked vopack release staging failed with status ${stage.status}`);
}
const releasePath = path.join(vopackStage, 'vo.release.json');
const sourcePath = path.join(vopackStage, `vopack-${vopackVersion}.tar.gz`);
const digest = (bytes) => `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
if (digest(readFileSync(releasePath)) !== vopackReleaseDigest) {
  throw new Error('staged vopack release manifest does not match BlockKart vo.lock');
}
if (digest(readFileSync(sourcePath)) !== vopackSourceDigest) {
  throw new Error('staged vopack source archive does not match BlockKart vo.lock');
}
const extract = spawnSync('tar', [
  '-xzf', sourcePath,
  '--strip-components=1',
  '-C', vopackCache,
], {
  stdio: 'inherit',
});
if (extract.error) {
  throw extract.error;
}
if (extract.status !== 0) {
  throw new Error(`locked vopack archive extraction failed with status ${extract.status}`);
}
copyFileSync(releasePath, path.join(vopackCache, 'vo.release.json'));
writeFileSync(path.join(vopackCache, '.vo-version'), `${vopackVersion}\n`);
writeFileSync(path.join(vopackCache, '.vo-source-digest'), `${vopackSourceDigest}\n`);

console.log(`quickplay module cache: ok ${cacheRoot}`);
