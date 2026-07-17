import { execFileSync } from 'node:child_process';
import { lstatSync, realpathSync } from 'node:fs';
import { devNull } from 'node:os';
import path from 'node:path';

const REDIRECTED_GIT_ENVIRONMENT = new Set([
  'GIT_ALTERNATE_OBJECT_DIRECTORIES',
  'GIT_ATTR_NOSYSTEM',
  'GIT_CEILING_DIRECTORIES',
  'GIT_COMMON_DIR',
  'GIT_CONFIG',
  'GIT_CONFIG_COUNT',
  'GIT_CONFIG_GLOBAL',
  'GIT_CONFIG_NOSYSTEM',
  'GIT_CONFIG_PARAMETERS',
  'GIT_CONFIG_SYSTEM',
  'GIT_DIR',
  'GIT_DISCOVERY_ACROSS_FILESYSTEM',
  'GIT_EXEC_PATH',
  'GIT_GRAFT_FILE',
  'GIT_GLOB_PATHSPECS',
  'GIT_ICASE_PATHSPECS',
  'GIT_IMPLICIT_WORK_TREE',
  'GIT_INDEX_FILE',
  'GIT_NAMESPACE',
  'GIT_NOGLOB_PATHSPECS',
  'GIT_NO_REPLACE_OBJECTS',
  'GIT_OBJECT_DIRECTORY',
  'GIT_OPTIONAL_LOCKS',
  'GIT_PREFIX',
  'GIT_QUARANTINE_PATH',
  'GIT_REPLACE_REF_BASE',
  'GIT_SHALLOW_FILE',
  'GIT_TERMINAL_PROMPT',
  'GIT_LITERAL_PATHSPECS',
  'GIT_WORK_TREE',
]);

export function cleanGitEnvironment(source = process.env) {
  const environment = {};
  for (const [key, value] of Object.entries(source)) {
    const upper = key.toUpperCase();
    if (
      REDIRECTED_GIT_ENVIRONMENT.has(upper)
      || upper.startsWith('GIT_CONFIG_KEY_')
      || upper.startsWith('GIT_CONFIG_VALUE_')
    ) continue;
    environment[key] = value;
  }
  environment.GIT_ATTR_NOSYSTEM = '1';
  environment.GIT_CONFIG_COUNT = '0';
  environment.GIT_CONFIG_GLOBAL = devNull;
  environment.GIT_CONFIG_NOSYSTEM = '1';
  environment.GIT_CONFIG_SYSTEM = devNull;
  environment.GIT_OPTIONAL_LOCKS = '0';
  environment.GIT_TERMINAL_PROMPT = '0';
  return environment;
}

function expectedCommitEnvName(envName) {
  return envName.replace(/_ROOT$/, '_EXPECTED_COMMIT');
}

function gitOutput(args, cwd) {
  try {
    return execFileSync('git', args, {
      cwd,
      encoding: 'utf8',
      env: cleanGitEnvironment(),
      maxBuffer: 1024 * 1024,
      timeout: 30_000,
      stdio: ['ignore', 'pipe', 'pipe'],
    }).trim();
  } catch (error) {
    const stderr = error?.stderr ? String(error.stderr).trim() : '';
    throw new Error(`git ${args.join(' ')} failed in ${cwd}: ${stderr || error.message}`);
  }
}

export function canonicalExistingDirectory(value, label) {
  let canonical;
  try {
    canonical = realpathSync.native(path.resolve(value));
    const metadata = lstatSync(canonical);
    if (!metadata.isDirectory() || metadata.isSymbolicLink()) {
      throw new Error('resolved path is not a real directory');
    }
  } catch (error) {
    throw new Error(`${label} is not a canonical existing directory: ${error.message}`);
  }
  return canonical;
}

export function canonicalGitRepositoryRoot(value, label = 'repository root', { requireVoMod = false } = {}) {
  const canonical = canonicalExistingDirectory(value, label);
  if (gitOutput(['rev-parse', '--is-inside-work-tree'], canonical) !== 'true') {
    throw new Error(`${label} is not a Git work tree: ${canonical}`);
  }
  const topLevel = canonicalExistingDirectory(
    gitOutput(['rev-parse', '--show-toplevel'], canonical),
    `${label} Git top level`,
  );
  if (topLevel !== canonical) {
    throw new Error(`${label} must name the Git top level ${topLevel}, found ${canonical}`);
  }
  if (requireVoMod) {
    const modPath = path.join(canonical, 'vo.mod');
    let metadata;
    try {
      metadata = lstatSync(modPath);
    } catch (error) {
      throw new Error(`${label} does not contain vo.mod: ${error.message}`);
    }
    if (!metadata.isFile() || metadata.isSymbolicLink()) {
      throw new Error(`${label} vo.mod must be a regular file without symlinks`);
    }
  }
  return canonical;
}

function verifyExpectedHead(envName, repoName, resolved) {
  const expected = process.env[expectedCommitEnvName(envName)];
  if (!expected || String(expected).trim() === '') {
    return;
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
  try {
    const resolved = canonicalGitRepositoryRoot(value, `${envName} for ${repoName}`, { requireVoMod: true });
    verifyExpectedHead(envName, repoName, resolved);
    return resolved;
  } catch (error) {
    console.error(`repo root: ${error.message}`);
    process.exit(1);
  }
}

export function requireVolangRoot(expectedRoot) {
  const value = process.env.VOLANG_ROOT;
  if (!value || String(value).trim() === '') {
    console.error('repo root: VOLANG_ROOT is required; run through vo-dev task so repo provisioning is injected');
    process.exit(1);
  }
  try {
    const resolved = canonicalGitRepositoryRoot(value, 'VOLANG_ROOT');
    const expected = canonicalGitRepositoryRoot(expectedRoot, 'current Volang root');
    if (expected !== resolved) {
      console.error(`repo root: VOLANG_ROOT=${resolved} does not match current volang root ${expected}`);
      process.exit(1);
    }
    return resolved;
  } catch (error) {
    console.error(`repo root: ${error.message}`);
    process.exit(1);
  }
}
