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

function normalizedPaths(values) {
  return [...new Set((values ?? []).map((value) => String(value)))].sort();
}

function repoDigestEntries(repoEntries) {
  return repoEntries.map(({ name, commit, dirty, sourceDigest }) => ({
    name,
    commit,
    dirty,
    sourceDigest,
  }));
}

function repoSummary(repoEntries) {
  return {
    testedCommits: Object.fromEntries(repoEntries.map((repo) => [repo.name, repo.commit])),
    dirtyFlags: Object.fromEntries(repoEntries.map((repo) => [repo.name, repo.dirty])),
  };
}

function bindingDigest({ gate, ciRunId, sourceDigest, gateDigest, artifactDigest, testedCommits, dirtyFlags }) {
  return digestJson({
    gate,
    ciRunId,
    sourceDigest,
    gateDigest,
    artifactDigest,
    testedCommits,
    dirtyFlags,
  });
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
  const normalizedGateFiles = normalizedPaths(gateFiles);
  const normalizedArtifacts = normalizedPaths(artifacts);
  const repoEntries = repos.map((repo) => ({
    name: repo.name,
    root: repo.root,
    commit: gitCommit(repo.root),
    dirty: gitDirty(repo.root),
    sourceDigest: sourceTreeDigest(repo.root),
  }));
  const { testedCommits, dirtyFlags } = repoSummary(repoEntries);
  const sourceDigest = digestJson(repoDigestEntries(repoEntries));
  const gateDigest = fileSetDigest(root, normalizedGateFiles);
  const artifactDigest = artifactSetDigest(root, normalizedArtifacts);
  const missingCommitRepos = repoEntries
    .filter((repo) => !repo.commit)
    .map((repo) => repo.name);
  const dirtyRepos = repoEntries
    .filter((repo) => repo.dirty)
    .map((repo) => repo.name);
  const verdictStatus = missingCommitRepos.length === 0 && dirtyRepos.length === 0 ? 'pass' : 'fail';
  const ciRunId = process.env.VO_DEV_CI_RUN_ID || `standalone:${randomUUID()}`;
  const runBindingDigest = bindingDigest({
    gate,
    ciRunId,
    sourceDigest,
    gateDigest,
    artifactDigest,
    testedCommits,
    dirtyFlags,
  });
  return {
    schemaVersion: 2,
    ciRunId,
    taskRunId: `${gate}:${ciRunId}`,
    gate,
    generatedAt,
    sourceDigest,
    gateDigest,
    artifactDigest,
    runBindingDigest,
    testedCommits,
    dirtyFlags,
    inputs: {
      gateFiles: normalizedGateFiles,
      artifacts: normalizedArtifacts,
    },
    verdict: {
      status: verdictStatus,
      dirtyProvenance: dirtyRepos.length > 0,
      missingCommitRepos,
      dirtyRepos,
    },
    repos: repoEntries,
  };
}

export function verifySourceBoundEvidence({
  evidence,
  expectedGate,
  root,
  expectedCiRunId = null,
}) {
  const issues = [];
  if (!evidence || typeof evidence !== 'object') {
    return ['missing freshEvidence'];
  }
  if (evidence.schemaVersion !== 2) {
    issues.push(`schemaVersion ${evidence.schemaVersion ?? '(missing)'} did not match 2`);
  }
  if (evidence.gate !== expectedGate) {
    issues.push(`gate ${evidence.gate ?? '(missing)'} did not match ${expectedGate}`);
  }
  if (typeof evidence.ciRunId !== 'string' || evidence.ciRunId.length === 0) {
    issues.push('ciRunId missing');
  } else if (expectedCiRunId && evidence.ciRunId !== expectedCiRunId) {
    issues.push(`ciRunId ${evidence.ciRunId} did not match current run ${expectedCiRunId}`);
  }
  if (evidence.taskRunId !== `${evidence.gate}:${evidence.ciRunId}`) {
    issues.push('taskRunId does not bind gate and ciRunId');
  }
  if (typeof evidence.generatedAt !== 'string' || Number.isNaN(Date.parse(evidence.generatedAt))) {
    issues.push('generatedAt missing or invalid');
  }

  const gateFiles = normalizedPaths(evidence.inputs?.gateFiles);
  const artifacts = normalizedPaths(evidence.inputs?.artifacts);
  if (!Array.isArray(evidence.inputs?.gateFiles) || gateFiles.length === 0) {
    issues.push('gateFiles missing');
  }
  if (!Array.isArray(evidence.inputs?.artifacts)) {
    issues.push('artifacts missing');
  }
  if (JSON.stringify(evidence.inputs?.gateFiles) !== JSON.stringify(gateFiles)) {
    issues.push('gateFiles are not canonical');
  }
  if (JSON.stringify(evidence.inputs?.artifacts) !== JSON.stringify(artifacts)) {
    issues.push('artifacts are not canonical');
  }

  const declaredRepos = Array.isArray(evidence.repos) ? evidence.repos : [];
  if (declaredRepos.length === 0) {
    issues.push('repos missing');
  }
  const currentRepos = declaredRepos.map((repo) => ({
    name: repo?.name,
    root: repo?.root,
    commit: repo?.root ? gitCommit(repo.root) : null,
    dirty: repo?.root ? gitDirty(repo.root) : null,
    sourceDigest: repo?.root ? sourceTreeDigest(repo.root) : null,
  }));
  for (let index = 0; index < declaredRepos.length; index++) {
    const declared = declaredRepos[index];
    const current = currentRepos[index];
    if (!declared?.name || !declared?.root) {
      issues.push(`repo evidence ${index} missing name or root`);
      continue;
    }
    for (const field of ['commit', 'dirty', 'sourceDigest']) {
      if (declared[field] !== current[field]) {
        issues.push(`${declared.name} ${field} does not match current source`);
      }
    }
  }

  const { testedCommits, dirtyFlags } = repoSummary(currentRepos);
  const sourceDigest = digestJson(repoDigestEntries(currentRepos));
  const gateDigest = fileSetDigest(root, gateFiles);
  const artifactDigest = artifactSetDigest(root, artifacts);
  const runBindingDigest = bindingDigest({
    gate: evidence.gate,
    ciRunId: evidence.ciRunId,
    sourceDigest,
    gateDigest,
    artifactDigest,
    testedCommits,
    dirtyFlags,
  });
  for (const [field, expected] of Object.entries({
    sourceDigest,
    gateDigest,
    artifactDigest,
    runBindingDigest,
  })) {
    if (evidence[field] !== expected) {
      issues.push(`${field} does not match recomputed value`);
    }
  }
  if (JSON.stringify(evidence.testedCommits) !== JSON.stringify(testedCommits)) {
    issues.push('testedCommits do not match current repos');
  }
  if (JSON.stringify(evidence.dirtyFlags) !== JSON.stringify(dirtyFlags)) {
    issues.push('dirtyFlags do not match current repos');
  }

  const missingCommitRepos = currentRepos.filter((repo) => !repo.commit).map((repo) => repo.name);
  const dirtyRepos = currentRepos.filter((repo) => repo.dirty).map((repo) => repo.name);
  const verdictStatus = missingCommitRepos.length === 0 && dirtyRepos.length === 0 ? 'pass' : 'fail';
  if (evidence.verdict?.status !== verdictStatus) {
    issues.push(`freshEvidence verdict ${evidence.verdict?.status ?? '(missing)'} did not match ${verdictStatus}`);
  }
  if (Boolean(evidence.verdict?.dirtyProvenance) !== (dirtyRepos.length > 0)) {
    issues.push('freshEvidence dirtyProvenance does not match current repos');
  }
  if (JSON.stringify(evidence.verdict?.missingCommitRepos) !== JSON.stringify(missingCommitRepos)) {
    issues.push('freshEvidence missingCommitRepos do not match current repos');
  }
  if (JSON.stringify(evidence.verdict?.dirtyRepos) !== JSON.stringify(dirtyRepos)) {
    issues.push('freshEvidence dirtyRepos do not match current repos');
  }
  return issues;
}
