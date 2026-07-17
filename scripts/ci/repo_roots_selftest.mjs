#!/usr/bin/env node
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import {
  mkdirSync,
  mkdtempSync,
  realpathSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { canonicalGitRepositoryRoot, cleanGitEnvironment } from './repo_roots.mjs';

const temp = realpathSync.native(mkdtempSync(path.join(os.tmpdir(), 'volang-repo-roots-')));

function git(cwd, args) {
  return execFileSync('git', args, {
    cwd,
    encoding: 'utf8',
    stdio: ['ignore', 'pipe', 'pipe'],
  }).trim();
}

function initRepo(name) {
  const root = path.join(temp, name);
  mkdirSync(root);
  writeFileSync(
    path.join(root, 'vo.mod'),
    `module = "github.com/acme/${name}"\nvo = "^0.1.0"\n`,
  );
  git(root, ['init', '-q']);
  git(root, ['add', 'vo.mod']);
  git(root, [
    '-c', 'user.name=Repository Root Test',
    '-c', 'user.email=repo-root@example.invalid',
    'commit', '-qm', 'fixture',
  ]);
  return root;
}

try {
  const first = initRepo('first');
  const second = initRepo('second');
  const cleaned = cleanGitEnvironment({
    PATH: process.env.PATH,
    Git_Dir: path.join(second, '.git'),
    git_config_key_0: 'core.bare',
    git_config_value_0: 'true',
  });
  assert.equal(cleaned.PATH, process.env.PATH);
  assert.equal(Object.keys(cleaned).some((key) => key.toUpperCase() === 'GIT_DIR'), false);
  assert.equal(
    Object.keys(cleaned).some((key) => key.toUpperCase().startsWith('GIT_CONFIG_KEY_')),
    false,
  );
  assert.equal(
    canonicalGitRepositoryRoot(first, 'first fixture', { requireVoMod: true }),
    first,
  );

  const redirectedKeys = [
    'GIT_CONFIG_COUNT',
    'GIT_CONFIG_KEY_0',
    'GIT_CONFIG_VALUE_0',
    'GIT_DIR',
    'GIT_WORK_TREE',
  ];
  const previousRedirects = Object.fromEntries(
    redirectedKeys.map((key) => [key, process.env[key]]),
  );
  process.env.GIT_CONFIG_COUNT = '1';
  process.env.GIT_CONFIG_KEY_0 = 'core.bare';
  process.env.GIT_CONFIG_VALUE_0 = 'true';
  process.env.GIT_DIR = path.join(second, '.git');
  process.env.GIT_WORK_TREE = second;
  try {
    assert.equal(
      canonicalGitRepositoryRoot(first, 'redirect-resistant fixture', { requireVoMod: true }),
      first,
    );
  } finally {
    for (const key of redirectedKeys) {
      const previous = previousRedirects[key];
      if (previous === undefined) delete process.env[key];
      else process.env[key] = previous;
    }
  }

  const link = path.join(temp, 'active');
  symlinkSync(first, link, 'dir');
  const pinned = canonicalGitRepositoryRoot(link, 'symlink fixture', { requireVoMod: true });
  assert.equal(pinned, first, 'repository roots must resolve to an immutable canonical spelling');
  rmSync(link);
  symlinkSync(second, link, 'dir');
  assert.equal(pinned, first, 'retargeting the input symlink must not change an already returned root');
  assert.equal(
    canonicalGitRepositoryRoot(link, 'retargeted fixture', { requireVoMod: true }),
    second,
  );

  const nested = path.join(first, 'nested');
  mkdirSync(nested);
  assert.throws(
    () => canonicalGitRepositoryRoot(nested, 'nested fixture'),
    /must name the Git top level/,
  );

  const outsideMod = path.join(temp, 'outside.vo.mod');
  writeFileSync(outsideMod, 'module = "github.com/acme/outside"\nvo = "^0.1.0"\n');
  rmSync(path.join(second, 'vo.mod'));
  symlinkSync(outsideMod, path.join(second, 'vo.mod'));
  assert.throws(
    () => canonicalGitRepositoryRoot(second, 'symlinked vo.mod fixture', { requireVoMod: true }),
    /regular file without symlinks/,
  );

  const plain = path.join(temp, 'plain');
  mkdirSync(plain);
  assert.throws(() => canonicalGitRepositoryRoot(plain, 'plain fixture'), /git .* failed|Git work tree/i);
  console.log('repo roots selftest: ok');
} finally {
  rmSync(temp, { recursive: true, force: true });
}
