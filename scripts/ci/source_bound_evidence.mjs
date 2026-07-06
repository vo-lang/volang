import { execFileSync } from 'node:child_process';
import { createHash, randomUUID } from 'node:crypto';
import {
  existsSync,
  readdirSync,
  readFileSync,
  statSync,
} from 'node:fs';
import path from 'node:path';

function sha256(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function digestJson(value) {
  return sha256(Buffer.from(JSON.stringify(value), 'utf8'));
}

function gitOutput(args, cwd) {
  try {
    return execFileSync('git', args, { cwd, encoding: 'utf8' }).trim();
  } catch (error) {
    return '';
  }
}

function gitFiles(repoRoot) {
  const output = gitOutput(['ls-files', '-z', '--cached', '--others', '--exclude-standard'], repoRoot);
  if (!output) {
    return [];
  }
  return output.split('\0').filter(Boolean).sort();
}

function fileEntry(root, absolutePath) {
  if (!existsSync(absolutePath)) {
    return {
      path: path.relative(root, absolutePath).split(path.sep).join('/'),
      missing: true,
    };
  }
  const stat = statSync(absolutePath);
  if (!stat.isFile()) {
    return {
      path: path.relative(root, absolutePath).split(path.sep).join('/'),
      kind: stat.isDirectory() ? 'directory' : 'other',
    };
  }
  const bytes = readFileSync(absolutePath);
  return {
    path: path.relative(root, absolutePath).split(path.sep).join('/'),
    size: bytes.byteLength,
    digest: sha256(bytes),
  };
}

function directoryEntries(root, dir, prefix = '') {
  if (!existsSync(dir)) {
    return [{ path: prefix || '.', missing: true }];
  }
  const entries = [];
  for (const name of readdirSync(dir).sort()) {
    const absolute = path.join(dir, name);
    const relative = prefix ? `${prefix}/${name}` : name;
    const stat = statSync(absolute);
    if (stat.isDirectory()) {
      entries.push(...directoryEntries(root, absolute, relative));
    } else if (stat.isFile()) {
      const bytes = readFileSync(absolute);
      entries.push({
        path: path.relative(root, absolute).split(path.sep).join('/'),
        size: bytes.byteLength,
        digest: sha256(bytes),
      });
    }
  }
  return entries;
}

export function sourceTreeDigest(repoRoot) {
  const files = gitFiles(repoRoot)
    .filter((relative) => existsSync(path.join(repoRoot, relative)))
    .map((relative) => fileEntry(repoRoot, path.join(repoRoot, relative)));
  return digestJson(files);
}

export function fileSetDigest(root, files) {
  const entries = files
    .map((file) => fileEntry(root, path.resolve(root, file)))
    .sort((a, b) => a.path.localeCompare(b.path));
  return digestJson(entries);
}

export function artifactSetDigest(root, artifacts) {
  const entries = [];
  for (const artifact of artifacts.filter(Boolean)) {
    const absolute = path.resolve(root, artifact);
    if (!existsSync(absolute)) {
      entries.push({
        path: path.relative(root, absolute).split(path.sep).join('/'),
        missing: true,
      });
      continue;
    }
    const stat = statSync(absolute);
    if (stat.isDirectory()) {
      entries.push(...directoryEntries(root, absolute));
    } else {
      entries.push(fileEntry(root, absolute));
    }
  }
  entries.sort((a, b) => a.path.localeCompare(b.path));
  return digestJson(entries);
}

export function gitCommit(repoRoot) {
  return gitOutput(['rev-parse', 'HEAD'], repoRoot) || null;
}

export function gitDirty(repoRoot) {
  const status = gitOutput(['status', '--porcelain'], repoRoot);
  return status !== '';
}

export function sourceBoundEvidence({
  gate,
  generatedAt,
  root,
  repos,
  gateFiles,
  artifacts = [],
}) {
  const repoEntries = repos.map((repo) => ({
    name: repo.name,
    root: repo.root,
    commit: gitCommit(repo.root),
    dirty: gitDirty(repo.root),
    sourceDigest: sourceTreeDigest(repo.root),
  }));
  const testedCommits = Object.fromEntries(repoEntries.map((repo) => [repo.name, repo.commit]));
  const dirtyFlags = Object.fromEntries(repoEntries.map((repo) => [repo.name, repo.dirty]));
  const sourceDigest = digestJson(repoEntries.map(({ name, commit, dirty, sourceDigest }) => ({
    name,
    commit,
    dirty,
    sourceDigest,
  })));
  const gateDigest = fileSetDigest(root, gateFiles);
  const artifactDigest = artifactSetDigest(root, artifacts);
  const missingCommitRepos = repoEntries
    .filter((repo) => !repo.commit)
    .map((repo) => repo.name);
  const dirtyRepos = repoEntries
    .filter((repo) => repo.dirty)
    .map((repo) => repo.name);
  const verdictStatus = missingCommitRepos.length === 0 && dirtyRepos.length === 0 ? 'pass' : 'fail';
  const runBindingDigest = digestJson({
    gate,
    sourceDigest,
    gateDigest,
    artifactDigest,
    testedCommits,
    dirtyFlags,
  });
  return {
    schemaVersion: 1,
    taskRunId: `${gate}:${randomUUID()}`,
    gate,
    generatedAt,
    sourceDigest,
    gateDigest,
    artifactDigest,
    runBindingDigest,
    testedCommits,
    dirtyFlags,
    verdict: {
      status: verdictStatus,
      dirtyProvenance: dirtyRepos.length > 0,
      missingCommitRepos,
      dirtyRepos,
    },
    repos: repoEntries,
  };
}
