import { execFileSync } from 'node:child_process';
import { existsSync } from 'node:fs';
import path from 'node:path';

function expectedCommitEnvName(envName) {
  return envName.replace(/_ROOT$/, '_EXPECTED_COMMIT');
}

function gitOutput(args, cwd) {
  try {
    return execFileSync('git', args, { cwd, encoding: 'utf8' }).trim();
  } catch (error) {
    const stderr = error?.stderr ? String(error.stderr).trim() : '';
    console.error(`repo root: git ${args.join(' ')} failed in ${cwd}: ${stderr || error.message}`);
    process.exit(1);
  }
}

function verifyExpectedHead(envName, repoName, resolved) {
  const expected = process.env[expectedCommitEnvName(envName)];
  if (!expected || String(expected).trim() === '') {
    return;
  }
  const inside = gitOutput(['rev-parse', '--is-inside-work-tree'], resolved);
  if (inside !== 'true') {
    console.error(`repo root: ${envName}=${resolved} is not a git checkout for ${repoName}`);
    process.exit(1);
  }
  const head = gitOutput(['rev-parse', 'HEAD'], resolved);
  if (head !== expected) {
    console.error(`repo root: ${envName}=${resolved} HEAD ${head} does not match expected ${expected} for ${repoName}`);
    process.exit(1);
  }
}

export function requireRepoRoot(envName, repoName) {
  const value = process.env[envName];
  if (!value || String(value).trim() === '') {
    console.error(`repo root: ${envName} is required for ${repoName}; run through vo-dev task so repo provisioning is injected`);
    process.exit(1);
  }
  const resolved = path.resolve(value);
  if (!existsSync(path.join(resolved, 'vo.mod'))) {
    console.error(`repo root: ${envName}=${resolved} does not contain vo.mod for ${repoName}; check CI checkout provisioning`);
    process.exit(1);
  }
  verifyExpectedHead(envName, repoName, resolved);
  return resolved;
}

export function requireVolangRoot(expectedRoot) {
  const value = process.env.VOLANG_ROOT;
  if (!value || String(value).trim() === '') {
    console.error('repo root: VOLANG_ROOT is required; run through vo-dev task so repo provisioning is injected');
    process.exit(1);
  }
  const resolved = path.resolve(value);
  if (path.resolve(expectedRoot) !== resolved) {
    console.error(`repo root: VOLANG_ROOT=${resolved} does not match current volang root ${path.resolve(expectedRoot)}`);
    process.exit(1);
  }
  return resolved;
}
