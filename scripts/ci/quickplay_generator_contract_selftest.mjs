#!/usr/bin/env node
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { createRequire } from 'node:module';
import {
  existsSync,
  linkSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  readdirSync,
  realpathSync,
  renameSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { parseBoundedJsonBytes as parseBoundedJsonBytesDirect } from './bounded_json.mjs';
import {
  BLOCKKART_VPAK_PATH,
  BLOCKKART_VPAK_PRODUCER_PATH,
  QUICKPLAY_ARTIFACT_INPUTS,
  MAX_MODULE_ARTIFACT_BYTES,
  MAX_PROTOCOL_METADATA_BYTES,
  MAX_SOURCE_ARCHIVE_BYTES,
  SOURCE_ARCHIVE_ASSET_NAME,
  assertSameBlockKartVpakProducerState,
  createVoBinaryAuthority,
  encodeStaticFile,
  gitCurrentSourceFiles,
  gitTrackedFiles,
  invokeVoBinary,
  observeBlockKartVpakProducer,
  observeCurrentSourceFilesystemClosure,
  packageSnapshotModule,
  parseBoundedJsonBytes,
  readBoundedRegularFileSnapshot,
  readRootedBoundedRegularFileSnapshot,
  recoverableQuickplayDirectory,
  selectModuleCacheRoot,
  sourceInputDigest,
  validatePackageManifest,
  validateRelease,
  withCleanEffectiveSnapshot,
} from './quickplay_vnext.mjs';
import {
  recoverInterruptedDirectoryReplacement,
  withDirectoryReplacementLock,
} from './voplay_current_wasm.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
assert.equal(parseBoundedJsonBytes, parseBoundedJsonBytesDirect);
const studioRequire = createRequire(new URL('../../apps/studio/package.json', import.meta.url));
const { parse: parseWithAcorn } = studioRequire('acorn');

function createModuleRepositoryFixture(parent, directoryName, modulePath) {
  const directory = path.join(parent, directoryName);
  mkdirSync(directory, { recursive: true });
  writeFileSync(
    path.join(directory, 'vo.mod'),
    `module = "${modulePath}"\nvo = "^0.1.0"\n`,
  );
  const initialized = spawnSync('git', ['init', '-q'], {
    cwd: directory,
    encoding: 'utf8',
  });
  assert.equal(
    initialized.status,
    0,
    `could not initialize ${modulePath} fixture: ${initialized.stderr}`,
  );
  return realpathSync.native(directory);
}

function localStaticImportSpecifiers(source, label) {
  const ast = parseWithAcorn(source, {
    ecmaVersion: 'latest',
    sourceType: 'module',
    allowHashBang: true,
  });
  const specifiers = new Set();
  const pending = [ast];
  let visitedNodes = 0;
  while (pending.length > 0) {
    const value = pending.pop();
    if (Array.isArray(value)) {
      pending.push(...value);
      continue;
    }
    if (!value || typeof value !== 'object') continue;
    visitedNodes += 1;
    assert(visitedNodes <= 1_000_000, `${label} AST is too large`);
    if (
      ['ImportDeclaration', 'ExportAllDeclaration', 'ExportNamedDeclaration'].includes(value.type)
      && typeof value.source?.value === 'string'
    ) {
      specifiers.add(value.source.value);
    }
    if (value.type === 'ImportExpression' && typeof value.source?.value === 'string') {
      specifiers.add(value.source.value);
    }
    for (const child of Object.values(value)) {
      if (child && typeof child === 'object') pending.push(child);
    }
  }
  return [...specifiers];
}

function declaredLocalScriptClosure() {
  const declared = QUICKPLAY_ARTIFACT_INPUTS.filter((input) => input.endsWith('.mjs'));
  const declaredSet = new Set(declared);
  const visited = new Set();
  const pending = [...declared];
  let totalBytes = 0;
  while (pending.length > 0) {
    const relative = pending.pop();
    if (visited.has(relative)) continue;
    visited.add(relative);
    assert(visited.size <= 4096, 'Quickplay local script import closure is too large');
    const absolute = path.resolve(root, ...relative.split('/'));
    assert.equal(realpathSync.native(absolute), absolute, `script input must be canonical: ${relative}`);
    const source = readFileSync(absolute, 'utf8');
    totalBytes += Buffer.byteLength(source, 'utf8');
    assert(totalBytes <= 64 * 1024 * 1024, 'Quickplay local script import closure is too large');
    for (const specifier of localStaticImportSpecifiers(source, relative)) {
      if (!specifier.startsWith('./') && !specifier.startsWith('../')) continue;
      const imported = path.resolve(path.dirname(absolute), specifier);
      const importedRelative = path.relative(root, imported).split(path.sep).join('/');
      assert(
        importedRelative !== '..'
          && !importedRelative.startsWith('../')
          && !path.isAbsolute(importedRelative),
        `${relative} imports outside the Volang repository: ${specifier}`,
      );
      assert(importedRelative.endsWith('.mjs'), `${relative} imports an unsupported local script: ${specifier}`);
      assert(declaredSet.has(importedRelative), `missing imported Quickplay source input ${importedRelative}`);
      pending.push(importedRelative);
    }
  }
  return visited;
}

assert.deepEqual(
  [...declaredLocalScriptClosure()].sort(),
  QUICKPLAY_ARTIFACT_INPUTS.filter((input) => input.endsWith('.mjs')).sort(),
);
const generator = readFileSync(`${root}/apps/studio/scripts/package_blockkart_quickplay.mjs`, 'utf8');
const protocol = readFileSync(`${root}/scripts/ci/quickplay_vnext.mjs`, 'utf8');
const tasks = readFileSync(`${root}/eng/tasks.toml`, 'utf8');
assert.doesNotMatch(protocol, /\['run',\s*'-q',\s*'-p',\s*'vo'/u);
assert.match(generator, /withCleanEffectiveSnapshot/);
assert.match(generator, /packageSnapshotModule/);
assert.match(generator, /filesOutsideNestedModules/);
assert.match(generator, /isPortableModuleManifestPath/);
assert.match(generator, /excludeNestedModules:\s*true/);
assert.match(generator, /verifyCurrentVoguiWasm/);
assert.match(generator, /freezeVerifiedVoguiArtifact/);
assert.match(generator, /verifyCurrentVoplayWasm/);
assert.match(generator, /freezeVerifiedVoplayArtifacts/);
assert.match(generator, /artifact changed after producer verification/);
assert.match(generator, /recoverInterruptedDirectoryReplacement/);
assert.match(generator, /publishStagedDirectoryWithRollback/);
assert.match(generator, /withDirectoryReplacementLock/);
assert.match(generator, /cleanupDirectoryReplacementBackups/);
assert.doesNotMatch(
  generator,
  /path\.join\(module\.source\.directory,\s*'web-artifacts',\s*'vogui\.wasm'\)/,
);
assert.match(generator, /allowVoBin:\s*false/);
assert.match(generator, /installPath:\s*'web-artifacts\//);
assert.doesNotMatch(generator, /VOPLAY_CURRENT_WASM_OUT_DIR|vo\.web\.json|parseVoLock|release_manifest/);
for (const input of [
  'apps/studio/package.json',
  'apps/studio/scripts/package_blockkart_quickplay.mjs',
  'scripts/ci/repo_roots.mjs',
  'scripts/ci/quickplay_vnext.mjs',
  'scripts/ci/vogui_current_wasm.mjs',
]) {
  assert.ok(QUICKPLAY_ARTIFACT_INPUTS.includes(input), `missing generator input ${input}`);
}

for (const taskName of ['quickplay-blockkart-package', 'quickplay-regenerate-check']) {
  const task = tasks.split(`name = "${taskName}"`)[1]?.split('[[task]]')[0];
  assert.match(task ?? '', /needs = \[[^\n]*"vogui-current-wasm-build"/);
}
const baselineSourceInputDigest = sourceInputDigest(root);
assert.match(baselineSourceInputDigest, /^sha256:[0-9a-f]{64}$/);

const defaultModuleCacheRoot = path.join(root, 'target', 'quickplay-module-cache', 'mod');
const explicitModuleCacheRoot = path.join(os.tmpdir(), 'quickplay-explicit-module-cache');
assert.equal(selectModuleCacheRoot(defaultModuleCacheRoot, undefined), defaultModuleCacheRoot);
assert.equal(
  selectModuleCacheRoot(defaultModuleCacheRoot, explicitModuleCacheRoot),
  explicitModuleCacheRoot,
);
assert.throws(
  () => selectModuleCacheRoot(defaultModuleCacheRoot, ''),
  /VO_MOD_CACHE must not be empty/,
);
assert.throws(
  () => selectModuleCacheRoot(defaultModuleCacheRoot, 'relative-cache'),
  /VO_MOD_CACHE must be an absolute path/,
);

const deletedTrackedFixture = mkdtempSync(path.join(os.tmpdir(), 'quickplay-deleted-tracked-'));
try {
  mkdirSync(path.join(deletedTrackedFixture, 'examples', 'smoke'), { recursive: true });
  mkdirSync(path.join(deletedTrackedFixture, 'fixtures'), { recursive: true });
  mkdirSync(path.join(deletedTrackedFixture, 'ignored-module'), { recursive: true });
  writeFileSync(
    path.join(deletedTrackedFixture, '.gitignore'),
    'ignored.vo\nignored-module/vo.mod\nvo.work\nassets/blockkart.vpak\n'
      + 'assets/blockkart.vpak.provenance.json\njs/dist/\n',
  );
  writeFileSync(path.join(deletedTrackedFixture, 'vo.mod'), 'module = "github.com/acme/root"\n');
  writeFileSync(path.join(deletedTrackedFixture, 'vo.lock'), 'version = 3\n');
  writeFileSync(path.join(deletedTrackedFixture, 'vo.web.json'), '{}\n');
  writeFileSync(path.join(deletedTrackedFixture, 'main.vo'), 'package main\n');
  writeFileSync(
    path.join(deletedTrackedFixture, 'ignored-module', 'main.vo'),
    'package ignoredmodule\n',
  );
  writeFileSync(
    path.join(deletedTrackedFixture, 'examples', 'smoke', 'vo.mod'),
    'module = "github.com/acme/smoke"\n',
  );
  writeFileSync(
    path.join(deletedTrackedFixture, 'examples', 'smoke', 'vo.lock'),
    'version = 3\n',
  );
  writeFileSync(path.join(deletedTrackedFixture, 'fixtures', 'vo.lock'), 'fixture lock\n');
  const git = (args) => {
    const result = spawnSync('git', ['-C', deletedTrackedFixture, ...args], {
      encoding: 'utf8',
      timeout: 30_000,
    });
    assert.equal(result.status, 0, result.error?.message || result.stderr);
    return result.stdout;
  };
  git(['init', '-q']);
  git(['add', '.']);
  git([
    '-c', 'user.name=Quickplay Test',
    '-c', 'user.email=quickplay@example.invalid',
    'commit', '-qm', 'fixture',
  ]);

  rmSync(path.join(deletedTrackedFixture, 'vo.lock'));
  rmSync(path.join(deletedTrackedFixture, 'vo.web.json'));
  rmSync(path.join(deletedTrackedFixture, 'examples', 'smoke', 'vo.lock'));
  writeFileSync(path.join(deletedTrackedFixture, 'untracked.vo'), 'package untracked\n');
  writeFileSync(path.join(deletedTrackedFixture, 'ignored.vo'), 'package ignored\n');
  writeFileSync(path.join(deletedTrackedFixture, 'vo.work'), 'version = 1\nmembers = []\n');
  writeFileSync(
    path.join(deletedTrackedFixture, 'ignored-module', 'vo.mod'),
    'module = "github.com/acme/ignored"\n',
  );
  mkdirSync(path.join(deletedTrackedFixture, 'nested'));
  writeFileSync(
    path.join(deletedTrackedFixture, 'nested', 'vo.mod'),
    'module = "github.com/acme/nested"\n',
  );
  writeFileSync(path.join(deletedTrackedFixture, 'nested', 'main.vo'), 'package nested\n');
  mkdirSync(path.join(deletedTrackedFixture, 'assets'), { recursive: true });
  mkdirSync(path.join(deletedTrackedFixture, 'js', 'dist'), { recursive: true });
  writeFileSync(path.join(deletedTrackedFixture, BLOCKKART_VPAK_PATH), 'ignored pack\n');
  writeFileSync(path.join(deletedTrackedFixture, BLOCKKART_VPAK_PRODUCER_PATH), '{}\n');
  writeFileSync(path.join(deletedTrackedFixture, 'js', 'dist', 'generated.js'), 'export {};\n');
  assert.match(git(['status', '--short', '--', 'vo.lock']), /^ D vo\.lock$/mu);
  assert.throws(
    () => gitTrackedFiles(deletedTrackedFixture),
    /tracked source file is deleted from the working tree: examples\/smoke\/vo\.lock|tracked source file is deleted from the working tree: vo\.lock/,
  );
  const currentTracked = gitTrackedFiles(deletedTrackedFixture, {
    allowDeleted: new Set(['vo.web.json']),
    allowDeletedModuleLocks: true,
  });
  assert(currentTracked.includes('main.vo'));
  assert(currentTracked.includes('examples/smoke/vo.mod'));
  assert(!currentTracked.includes('vo.lock'));
  assert(!currentTracked.includes('examples/smoke/vo.lock'));
  assert(!currentTracked.includes('vo.web.json'));
  assert(!currentTracked.includes('untracked.vo'));
  writeFileSync(path.join(deletedTrackedFixture, 'untracked.txt'), 'not source\n');
  const ambientIgnore = path.join(deletedTrackedFixture, 'ambient-ignore');
  writeFileSync(ambientIgnore, 'untracked.vo\nlang/crates/vo-syntax/src/token.rs\n');
  const redirectedGitEnvironment = [
    'GIT_CONFIG_COUNT',
    'GIT_CONFIG_KEY_0',
    'GIT_CONFIG_PARAMETERS',
    'GIT_CONFIG_VALUE_0',
    'GIT_INDEX_FILE',
    'GIT_LITERAL_PATHSPECS',
  ];
  const previousGitEnvironment = Object.fromEntries(
    redirectedGitEnvironment.map((key) => [key, process.env[key]]),
  );
  process.env.GIT_INDEX_FILE = path.join(deletedTrackedFixture, 'redirected-index');
  process.env.GIT_CONFIG_PARAMETERS = `'core.excludesFile=${ambientIgnore}'`;
  process.env.GIT_CONFIG_COUNT = '1';
  process.env.GIT_CONFIG_KEY_0 = 'core.excludesFile';
  process.env.GIT_CONFIG_VALUE_0 = ambientIgnore;
  process.env.GIT_LITERAL_PATHSPECS = '1';
  let currentSource;
  try {
    currentSource = gitCurrentSourceFiles(deletedTrackedFixture, {
      allowDeleted: new Set(['vo.web.json']),
      allowDeletedModuleLocks: true,
    });
    assert.equal(sourceInputDigest(root), baselineSourceInputDigest);
  } finally {
    for (const key of redirectedGitEnvironment) {
      const previous = previousGitEnvironment[key];
      if (previous === undefined) delete process.env[key];
      else process.env[key] = previous;
    }
  }
  assert(currentSource.includes('untracked.vo'));
  assert(currentSource.includes('nested/vo.mod'));
  assert(currentSource.includes('nested/main.vo'));
  assert(currentSource.includes('ignored-module/vo.mod'));
  assert(currentSource.includes('ignored-module/main.vo'));
  assert(currentSource.includes('vo.work'));
  assert(currentSource.includes('ignored.vo'));
  assert(!currentSource.includes('untracked.txt'));

  const callerSelected = gitCurrentSourceFiles(deletedTrackedFixture, {
    allowDeleted: new Set(['vo.web.json']),
    allowDeletedModuleLocks: true,
    additionalFileIncluded: (relative) => (
      relative === BLOCKKART_VPAK_PATH
      || relative === BLOCKKART_VPAK_PRODUCER_PATH
      || relative.startsWith('js/dist/')
    ),
    additionalIgnoredPathspecs: [
      `:(literal)${BLOCKKART_VPAK_PATH}`,
      `:(literal)${BLOCKKART_VPAK_PRODUCER_PATH}`,
      ':(glob)js/dist/**',
    ],
  });
  assert(callerSelected.includes(BLOCKKART_VPAK_PATH));
  assert(callerSelected.includes(BLOCKKART_VPAK_PRODUCER_PATH));
  assert(callerSelected.includes('js/dist/generated.js'));

  writeFileSync(path.join(deletedTrackedFixture, 'VO.WORK'), 'version = 1\nmembers = []\n');
  if (readdirSync(deletedTrackedFixture).includes('VO.WORK')) {
    assert.throws(
      () => gitCurrentSourceFiles(deletedTrackedFixture, {
        allowDeleted: new Set(['vo.web.json']),
        allowDeletedModuleLocks: true,
      }),
      /exact portable spelling vo\.work/,
    );
    rmSync(path.join(deletedTrackedFixture, 'VO.WORK'));
  } else {
    writeFileSync(path.join(deletedTrackedFixture, 'vo.work'), 'version = 1\nmembers = []\n');
  }

  rmSync(path.join(deletedTrackedFixture, 'fixtures', 'vo.lock'));
  assert.throws(
    () => gitTrackedFiles(deletedTrackedFixture, {
      allowDeleted: new Set(['vo.web.json']),
      allowDeletedModuleLocks: true,
    }),
    /tracked source file is deleted from the working tree: fixtures\/vo\.lock/,
  );
  writeFileSync(path.join(deletedTrackedFixture, 'fixtures', 'vo.lock'), 'fixture lock\n');
  rmSync(path.join(deletedTrackedFixture, 'main.vo'));
  assert.throws(
    () => gitTrackedFiles(deletedTrackedFixture, {
      allowDeleted: new Set(['vo.web.json']),
      allowDeletedModuleLocks: true,
    }),
    /tracked source file is deleted from the working tree: main\.vo/,
  );
} finally {
  rmSync(deletedTrackedFixture, { recursive: true, force: true });
}

const sourceClosureFixture = realpathSync.native(
  mkdtempSync(path.join(os.tmpdir(), 'quickplay-source-closure-')),
);
try {
  writeFileSync(
    path.join(sourceClosureFixture, 'vo.mod'),
    'module = "github.com/acme/root"\nvo = "^0.1.0"\n',
  );
  writeFileSync(path.join(sourceClosureFixture, 'main.vo'), 'package main\n');
  writeFileSync(path.join(sourceClosureFixture, 'ignored.vo'), 'package ignored\n');
  const rootSources = ['ignored.vo', 'main.vo', 'vo.mod'];
  assert.throws(
    () => observeCurrentSourceFilesystemClosure(
      sourceClosureFixture,
      ['main.vo', 'vo.mod'],
      'omitted ignored source fixture',
    ),
    /omitted Vo source file ignored\.vo/,
  );
  const initialClosure = observeCurrentSourceFilesystemClosure(
    sourceClosureFixture,
    rootSources,
    'complete ignored source fixture',
  );
  assert.deepEqual(initialClosure.voSources, ['ignored.vo', 'main.vo']);
  assert.deepEqual(initialClosure.moduleBoundaries, ['vo.mod']);

  mkdirSync(path.join(sourceClosureFixture, 'nested'));
  writeFileSync(
    path.join(sourceClosureFixture, 'nested', 'vo.mod'),
    'module = "github.com/acme/nested"\nvo = "^0.1.0"\n',
  );
  writeFileSync(path.join(sourceClosureFixture, 'nested', 'main.vo'), 'package nested\n');
  const nestedSources = [...rootSources, 'nested/vo.mod'].sort();
  if (process.platform !== 'win32') {
    symlinkSync(
      path.join(sourceClosureFixture, 'main.vo'),
      path.join(sourceClosureFixture, 'nested', 'ignored-link'),
    );
  }
  const nestedClosure = observeCurrentSourceFilesystemClosure(
    sourceClosureFixture,
    nestedSources,
    'nested module pruning fixture',
  );
  assert.deepEqual(nestedClosure.voSources, ['ignored.vo', 'main.vo']);
  assert.deepEqual(nestedClosure.moduleBoundaries, ['nested/vo.mod', 'vo.mod']);

  mkdirSync(path.join(sourceClosureFixture, 'alias'));
  writeFileSync(
    path.join(sourceClosureFixture, 'alias', 'VO.MOD'),
    'module = "github.com/acme/alias"\nvo = "^0.1.0"\n',
  );
  assert.throws(
    () => observeCurrentSourceFilesystemClosure(
      sourceClosureFixture,
      [...nestedSources, 'alias/VO.MOD'].sort(),
      'portable marker alias fixture',
    ),
    /exact basename vo\.mod/,
  );
  rmSync(path.join(sourceClosureFixture, 'alias'), { recursive: true, force: true });

  mkdirSync(path.join(sourceClosureFixture, 'invalid-marker', 'vo.mod'), { recursive: true });
  assert.throws(
    () => observeCurrentSourceFilesystemClosure(
      sourceClosureFixture,
      [...nestedSources, 'invalid-marker/vo.mod'].sort(),
      'directory marker fixture',
    ),
    /module boundary must be a single-link regular file/,
  );
  rmSync(path.join(sourceClosureFixture, 'invalid-marker'), { recursive: true, force: true });

  if (process.platform !== 'win32') {
    symlinkSync(
      path.join(sourceClosureFixture, 'main.vo'),
      path.join(sourceClosureFixture, 'linked-entry'),
    );
    assert.throws(
      () => observeCurrentSourceFilesystemClosure(
        sourceClosureFixture,
        nestedSources,
        'source symlink fixture',
      ),
      /regular file or directory without links/,
    );
    rmSync(path.join(sourceClosureFixture, 'linked-entry'), { force: true });

    const specialEntry = path.join(sourceClosureFixture, 'special-entry');
    const specialResult = spawnSync('mkfifo', [specialEntry], {
      encoding: 'utf8',
      timeout: 30_000,
    });
    assert.equal(specialResult.status, 0, specialResult.error?.message || specialResult.stderr);
    assert.throws(
      () => observeCurrentSourceFilesystemClosure(
        sourceClosureFixture,
        nestedSources,
        'special source entry fixture',
      ),
      /regular file or directory without links.*special/,
    );
    rmSync(specialEntry, { force: true });

    const markerTarget = path.join(sourceClosureFixture, 'marker-target');
    writeFileSync(markerTarget, 'module = "github.com/acme/hardlinked"\nvo = "^0.1.0"\n');
    mkdirSync(path.join(sourceClosureFixture, 'hardlinked-marker'));
    linkSync(markerTarget, path.join(sourceClosureFixture, 'hardlinked-marker', 'vo.mod'));
    assert.throws(
      () => observeCurrentSourceFilesystemClosure(
        sourceClosureFixture,
        [...nestedSources, 'hardlinked-marker/vo.mod'].sort(),
        'hardlinked marker fixture',
      ),
      /module boundary must be a single-link regular file/,
    );
    rmSync(path.join(sourceClosureFixture, 'hardlinked-marker'), { recursive: true, force: true });
    rmSync(markerTarget, { force: true });
  }
} finally {
  rmSync(sourceClosureFixture, { recursive: true, force: true });
}

function blockKartVpakProducerFixture(packBytes) {
  const producerIntent = {
    schemaVersion: 1,
    kind: 'blockkart.vpakProducerManifest',
    owner: 'BlockKart',
    command: ['vo', 'run', 'tools/pack_primitive_assets.vo'],
    pack: {
      path: BLOCKKART_VPAK_PATH,
      sha256: createHash('sha256').update(packBytes).digest('hex'),
      size: packBytes.byteLength,
    },
    inputs: [{ path: 'main.vo' }],
    workspaceSourceInputCount: 1,
    payloadInputCount: 37,
    archiveEntryCount: 37,
    archiveEntries: Array.from({ length: 37 }, (_, index) => ({ path: `entry-${index}` })),
    internalManifest: {},
    upstream: [],
  };
  return {
    ...producerIntent,
    producerDigest: createHash('sha256')
      .update(Buffer.from(JSON.stringify(producerIntent), 'utf8'))
      .digest('hex'),
  };
}

const vpakFixture = realpathSync.native(
  mkdtempSync(path.join(os.tmpdir(), 'quickplay-vpak-producer-')),
);
try {
  mkdirSync(path.join(vpakFixture, 'assets'));
  const packBytes = Buffer.from('authenticated BlockKart VPAK fixture', 'utf8');
  const producer = blockKartVpakProducerFixture(packBytes);
  const producerBytes = Buffer.from(`${JSON.stringify(producer, null, 2)}\n`, 'utf8');
  writeFileSync(path.join(vpakFixture, BLOCKKART_VPAK_PATH), packBytes);
  writeFileSync(path.join(vpakFixture, BLOCKKART_VPAK_PRODUCER_PATH), producerBytes);
  const observed = observeBlockKartVpakProducer(vpakFixture, 'VPAK producer fixture');
  assert.doesNotThrow(() => assertSameBlockKartVpakProducerState(
    observed,
    observed,
    'stable VPAK producer fixture',
  ));

  for (const nonCanonical of [
    JSON.stringify(producer),
    `${JSON.stringify(producer)}\n`,
    JSON.stringify(producer, null, 2),
  ]) {
    writeFileSync(
      path.join(vpakFixture, BLOCKKART_VPAK_PRODUCER_PATH),
      Buffer.from(nonCanonical, 'utf8'),
    );
    assert.throws(
      () => observeBlockKartVpakProducer(vpakFixture, 'non-canonical producer fixture'),
      /JSON\.stringify\(value, null, 2\).*trailing LF/,
    );
  }
  writeFileSync(path.join(vpakFixture, BLOCKKART_VPAK_PRODUCER_PATH), producerBytes);

  writeFileSync(
    path.join(vpakFixture, BLOCKKART_VPAK_PATH),
    Buffer.concat([packBytes, Buffer.from('tampered', 'utf8')]),
  );
  assert.throws(
    () => observeBlockKartVpakProducer(vpakFixture, 'tampered VPAK fixture'),
    /VPAK bytes do not match the producer manifest/,
  );
  writeFileSync(path.join(vpakFixture, BLOCKKART_VPAK_PATH), packBytes);

  const invalidProducer = { ...producer, producerDigest: '0'.repeat(64) };
  writeFileSync(
    path.join(vpakFixture, BLOCKKART_VPAK_PRODUCER_PATH),
    Buffer.from(`${JSON.stringify(invalidProducer, null, 2)}\n`, 'utf8'),
  );
  assert.throws(
    () => observeBlockKartVpakProducer(vpakFixture, 'tampered producer fixture'),
    /producerDigest does not bind/,
  );
  writeFileSync(path.join(vpakFixture, BLOCKKART_VPAK_PRODUCER_PATH), producerBytes);

  if (process.platform !== 'win32') {
    const manifestPath = path.join(vpakFixture, BLOCKKART_VPAK_PRODUCER_PATH);
    const displacedManifest = `${manifestPath}.original`;
    renameSync(manifestPath, displacedManifest);
    linkSync(displacedManifest, manifestPath);
    assert.throws(
      () => observeBlockKartVpakProducer(vpakFixture, 'hardlinked producer fixture'),
      /exactly one filesystem link/,
    );
    rmSync(manifestPath, { force: true });
    renameSync(displacedManifest, manifestPath);

    renameSync(manifestPath, displacedManifest);
    symlinkSync(displacedManifest, manifestPath);
    assert.throws(
      () => observeBlockKartVpakProducer(vpakFixture, 'symlinked producer fixture'),
      /must be a regular file/,
    );
    rmSync(manifestPath, { force: true });
    renameSync(displacedManifest, manifestPath);
  }
} finally {
  rmSync(vpakFixture, { recursive: true, force: true });
}

const cleanSnapshotFixture = realpathSync.native(
  mkdtempSync(path.join(os.tmpdir(), 'quickplay-clean-snapshot-')),
);
try {
  const sourceRoot = path.join(cleanSnapshotFixture, 'project');
  const authorityRoot = path.join(cleanSnapshotFixture, 'authority');
  const stagingParent = path.join(cleanSnapshotFixture, 'staging');
  mkdirSync(sourceRoot);
  mkdirSync(authorityRoot);
  mkdirSync(stagingParent);
  mkdirSync(path.join(sourceRoot, 'assets'));
  mkdirSync(path.join(sourceRoot, 'js', 'dist'), { recursive: true });
  mkdirSync(path.join(sourceRoot, 'nested'));
  mkdirSync(path.join(sourceRoot, 'target'));
  writeFileSync(
    path.join(sourceRoot, '.gitignore'),
    'ignored.vo\nassets/blockkart.vpak\nassets/blockkart.vpak.provenance.json\n'
      + 'js/dist/\ntarget/\n',
  );
  writeFileSync(
    path.join(sourceRoot, 'vo.mod'),
    'module = "github.com/acme/root"\nvo = "^0.1.0"\n',
  );
  writeFileSync(path.join(sourceRoot, 'vo.work'), 'version = 1\nmembers = []\n');
  writeFileSync(path.join(sourceRoot, 'main.vo'), 'package main\n');
  writeFileSync(path.join(sourceRoot, 'ignored.vo'), 'package ignored\n');
  writeFileSync(path.join(sourceRoot, BLOCKKART_VPAK_PATH), 'pack\n');
  writeFileSync(path.join(sourceRoot, BLOCKKART_VPAK_PRODUCER_PATH), '{}\n');
  writeFileSync(path.join(sourceRoot, 'js', 'dist', 'generated.js'), 'export {};\n');
  writeFileSync(
    path.join(sourceRoot, 'js', 'dist', 'vo.mod'),
    'module = "github.com/acme/generated"\nvo = "^0.1.0"\n',
  );
  writeFileSync(path.join(sourceRoot, 'target', 'skipped.vo'), 'package skipped\n');
  writeFileSync(
    path.join(sourceRoot, 'nested', 'vo.mod'),
    'module = "github.com/acme/nested"\nvo = "^0.1.0"\n',
  );
  writeFileSync(path.join(sourceRoot, 'nested', 'main.vo'), 'package nested\n');
  const git = (args) => spawnSync('git', ['-C', sourceRoot, ...args], {
    encoding: 'utf8',
    timeout: 30_000,
  });
  assert.equal(git(['init', '-q']).status, 0);
  assert.equal(git(['add', '.']).status, 0);
  assert.equal(git([
    '-c', 'user.name=Quickplay Test',
    '-c', 'user.email=quickplay@example.invalid',
    'commit', '-qm', 'fixture',
  ]).status, 0);

  const binary = path.join(authorityRoot, process.platform === 'win32' ? 'vo.exe' : 'vo');
  writeFileSync(binary, '#!/bin/sh\nexit 0\n', { mode: 0o755 });
  const expectedRoot = 'github.com/acme/root';
  const authority = createVoBinaryAuthority({
    root: authorityRoot,
    allowVoBin: true,
    voBin: binary,
    environment: { HOME: authorityRoot, PATH: process.env.PATH },
    processRunner(commandName, args) {
      assert.equal(commandName, binary);
      assert.deepEqual(args.slice(0, 2), ['mod', 'snapshot']);
      return {
        status: 0,
        stderr: '',
        stdout: JSON.stringify({
          schema_version: 2,
          mode: 'effective',
          authority: 'empty',
          root: { module: expectedRoot, vo: '^0.1.0', dependencies: [] },
          modules: [],
        }),
      };
    },
  });
  const consumed = withCleanEffectiveSnapshot({
    root: authorityRoot,
    projectRoot: sourceRoot,
    workspaceRoots: [],
    cacheRoot: path.join(cleanSnapshotFixture, 'cache'),
    stagingParent,
    expectedRoot,
    voAuthority: authority,
    sourceFileIncluded: (_index, relative) => (
      relative === 'vo.mod'
      || relative.endsWith('/vo.mod')
      || relative === 'vo.work'
      || relative.endsWith('.vo')
      || relative === BLOCKKART_VPAK_PATH
      || relative === BLOCKKART_VPAK_PRODUCER_PATH
      || relative.startsWith('js/dist/')
    ),
    sourceIgnoredPathspecs: () => [
      `:(literal)${BLOCKKART_VPAK_PATH}`,
      `:(literal)${BLOCKKART_VPAK_PRODUCER_PATH}`,
      ':(glob)js/dist/**',
    ],
    excludeNestedModules: true,
  }, ({ projectRoot }) => {
    for (const relative of [
      'ignored.vo',
      'main.vo',
      'vo.mod',
      'vo.work',
      BLOCKKART_VPAK_PATH,
      BLOCKKART_VPAK_PRODUCER_PATH,
      'js/dist/generated.js',
    ]) {
      assert.equal(existsSync(path.join(projectRoot, ...relative.split('/'))), true, relative);
    }
    for (const relative of [
      'js/dist/vo.mod',
      'nested/vo.mod',
      'nested/main.vo',
      'target/skipped.vo',
    ]) {
      assert.equal(existsSync(path.join(projectRoot, ...relative.split('/'))), false, relative);
    }
    return true;
  });
  assert.equal(consumed, true);
} finally {
  rmSync(cleanSnapshotFixture, { recursive: true, force: true });
}

const releaseSnapshotModule = {
  module: 'github.com/acme/library',
  version: '0.1.0',
  vo: '^0.1.0',
  dependencies: [],
};
const canonicalRelease = {
  schema_version: 2,
  module: releaseSnapshotModule.module,
  version: releaseSnapshotModule.version,
  commit: '1'.repeat(40),
  vo: releaseSnapshotModule.vo,
  dependencies: [],
  source: {
    name: SOURCE_ARCHIVE_ASSET_NAME,
    size: 1,
    digest: `sha256:${'2'.repeat(64)}`,
  },
  package: { size: 1, digest: `sha256:${'3'.repeat(64)}` },
  artifacts: [],
};
assert.equal(MAX_SOURCE_ARCHIVE_BYTES, 64 * 1024 * 1024);
assert.doesNotThrow(() => validateRelease(canonicalRelease, releaseSnapshotModule));
for (const version of ['1.0.0-RC.1', '1.0.0-alpha.lock']) {
  assert.throws(
    () => validateRelease(
      { ...canonicalRelease, version },
      { ...releaseSnapshotModule, version },
    ),
    /exact module version|canonical SemVer/,
  );
}
assert.throws(
  () => validateRelease({
    ...canonicalRelease,
    source: { ...canonicalRelease.source, name: 'renamed-source.tar.gz' },
  }, releaseSnapshotModule),
  /source name must be source\.tar\.gz/,
);
assert.throws(
  () => validateRelease({
    ...canonicalRelease,
    source: { ...canonicalRelease.source, size: MAX_SOURCE_ARCHIVE_BYTES + 1 },
  }, releaseSnapshotModule),
  /source size is invalid/,
);

const boundaryArtifact = {
  kind: 'extension-wasm',
  target: 'wasm32-unknown-unknown',
  name: 'library.wasm',
  size: MAX_MODULE_ARTIFACT_BYTES,
  digest: `sha256:${'4'.repeat(64)}`,
};
assert.doesNotThrow(() => validateRelease({
  ...canonicalRelease,
  artifacts: [boundaryArtifact],
}, releaseSnapshotModule));
assert.throws(
  () => validateRelease({
    ...canonicalRelease,
    artifacts: [{ ...boundaryArtifact, size: MAX_MODULE_ARTIFACT_BYTES + 1 }],
  }, releaseSnapshotModule),
  /artifacts\[0\]\.size is invalid/,
);

const packageDigest = `sha256:${'5'.repeat(64)}`;
const boundaryPackage = {
  schema_version: 1,
  files: [{
    path: 'vo.mod',
    mode: 'regular',
    size: MAX_PROTOCOL_METADATA_BYTES,
    digest: packageDigest,
  }],
};
assert.doesNotThrow(() => validatePackageManifest(boundaryPackage, 'boundary package'));
assert.throws(
  () => validatePackageManifest({
    ...boundaryPackage,
    files: [{ ...boundaryPackage.files[0], size: MAX_PROTOCOL_METADATA_BYTES + 1 }],
  }, 'oversized text package'),
  /Vo text-file limit/,
);
for (const nestedModuleManifest of ['nested/vo.mod', 'nested/VO.MOD', 'VO.MOD']) {
  assert.throws(
    () => validatePackageManifest({
      ...boundaryPackage,
      files: [
        ...boundaryPackage.files,
        {
          path: nestedModuleManifest,
          mode: 'regular',
          size: 1,
          digest: packageDigest,
        },
      ].sort((left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path))),
    }, 'nested module package'),
    /reserved by the module protocol/,
  );
}
for (const rootSourceArchive of ['source.tar.gz', 'SOURCE.TAR.GZ', 'ſource.tar.gz']) {
  assert.throws(
    () => validatePackageManifest({
      ...boundaryPackage,
      files: [
        ...boundaryPackage.files,
        {
          path: rootSourceArchive,
          mode: 'regular',
          size: 1,
          digest: packageDigest,
        },
      ].sort((left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path))),
    }, 'reserved source archive package'),
    /reserved by the module protocol/,
  );
}
assert.doesNotThrow(() => validatePackageManifest({
  ...boundaryPackage,
  files: [
    {
      path: 'docs/source.tar.gz',
      mode: 'regular',
      size: 1,
      digest: packageDigest,
    },
    ...boundaryPackage.files,
  ],
}, 'nested source archive fixture'));
assert.doesNotThrow(() => validatePackageManifest({
  ...boundaryPackage,
  files: [
    'fixtures/.vo-project.lock',
    'fixtures/vo.lock',
    'fixtures/vo.sum',
    'fixtures/vo.web.json',
    'fixtures/vo.work',
  ].map((path) => ({ path, mode: 'regular', size: 1, digest: packageDigest }))
    .concat(boundaryPackage.files)
    .sort((left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path))),
}, 'nested protocol-looking source fixtures'));

const nestedWorkspaceFixture = mkdtempSync(path.join(os.tmpdir(), 'quickplay-nested-workspace-'));
const nestedWorkspaceOutput = mkdtempSync(path.join(os.tmpdir(), 'quickplay-nested-output-'));
try {
  writeFileSync(
    path.join(nestedWorkspaceFixture, 'vo.mod'),
    'module = "github.com/acme/root"\nvo = "^0.1.0"\n',
  );
  writeFileSync(path.join(nestedWorkspaceFixture, 'main.vo'), 'package main\n');
  mkdirSync(path.join(nestedWorkspaceFixture, 'examples', 'child'), { recursive: true });
  writeFileSync(
    path.join(nestedWorkspaceFixture, 'examples', 'child', 'vo.mod'),
    'module = "github.com/acme/child"\nvo = "^0.1.0"\n',
  );
  writeFileSync(
    path.join(nestedWorkspaceFixture, 'examples', 'child', 'main.vo'),
    'package child\n',
  );
  writeFileSync(
    path.join(nestedWorkspaceFixture, 'examples', 'sibling.vo'),
    'package examples\n',
  );

  const packaged = packageSnapshotModule({
    module: 'github.com/acme/root',
    vo: '^0.1.0',
    dependencies: [],
    source: { kind: 'workspace', directory: nestedWorkspaceFixture },
  }, nestedWorkspaceOutput, {
    workspaceArtifacts: () => [],
    workspaceFileIncluded: (_module, relative) => (
      relative === 'vo.mod' || relative.endsWith('.vo')
    ),
  });
  assert.deepEqual(
    packaged.files.map((file) => file.path),
    ['examples/sibling.vo', 'main.vo', 'vo.mod'],
  );

  mkdirSync(path.join(nestedWorkspaceFixture, 'examples', 'alias'), { recursive: true });
  writeFileSync(
    path.join(nestedWorkspaceFixture, 'examples', 'alias', 'VO.MOD'),
    'module = "github.com/acme/alias"\nvo = "^0.1.0"\n',
  );
  writeFileSync(
    path.join(nestedWorkspaceFixture, 'examples', 'alias', 'main.vo'),
    'package alias\n',
  );
  assert.throws(
    () => packageSnapshotModule({
      module: 'github.com/acme/root',
      vo: '^0.1.0',
      dependencies: [],
      source: { kind: 'workspace', directory: nestedWorkspaceFixture },
    }, nestedWorkspaceOutput, {
      workspaceArtifacts: () => [],
      workspaceFileIncluded: (_module, relative) => (
        relative === 'vo.mod' || relative.endsWith('.vo')
      ),
    }),
    /exact basename vo\.mod/,
  );
} finally {
  rmSync(nestedWorkspaceFixture, { recursive: true, force: true });
  rmSync(nestedWorkspaceOutput, { recursive: true, force: true });
}

if (process.platform !== 'win32') {
  const modeFixture = mkdtempSync(path.join(os.tmpdir(), 'quickplay-source-mode-'));
  const outsideFixture = mkdtempSync(path.join(os.tmpdir(), 'quickplay-source-outside-'));
  try {
    const regular = path.join(modeFixture, 'regular.vo');
    const executable = path.join(modeFixture, 'tool');
    writeFileSync(regular, 'package regular\n', { mode: 0o644 });
    writeFileSync(executable, '#!/bin/sh\n', { mode: 0o755 });
    const regularSnapshot = readBoundedRegularFileSnapshot(regular, 'regular source');
    const executableSnapshot = readBoundedRegularFileSnapshot(executable, 'executable source');
    assert.equal(regularSnapshot.mode, 0o644);
    assert.equal(executableSnapshot.mode, 0o755);
    assert.equal(encodeStaticFile('regular.vo', regularSnapshot.bytes, regularSnapshot.mode).mode, 0o644);
    assert.equal(encodeStaticFile('bin/tool', executableSnapshot.bytes, executableSnapshot.mode).mode, 0o755);
    const rooted = path.join(modeFixture, 'rooted.vo');
    writeFileSync(rooted, 'package rooted\n');
    assert.equal(
      readRootedBoundedRegularFileSnapshot(modeFixture, 'rooted.vo', 'rooted source').bytes.toString(),
      'package rooted\n',
    );

    const outsideSource = path.join(outsideFixture, 'outside.vo');
    writeFileSync(outsideSource, 'package outside\n');
    linkSync(outsideSource, path.join(modeFixture, 'hardlinked.vo'));
    assert.throws(
      () => readRootedBoundedRegularFileSnapshot(modeFixture, 'hardlinked.vo', 'hardlinked source'),
      /exactly one filesystem link/,
    );

    symlinkSync(outsideFixture, path.join(modeFixture, 'escaped'));
    assert.throws(
      () => readRootedBoundedRegularFileSnapshot(modeFixture, 'escaped/outside.vo', 'escaped source'),
      /symbolic-link ancestor/,
    );
    symlinkSync(outsideSource, path.join(modeFixture, 'linked.vo'));
    assert.throws(
      () => readRootedBoundedRegularFileSnapshot(modeFixture, 'linked.vo', 'linked source'),
      /must be a regular file/,
    );
  } finally {
    rmSync(modeFixture, { recursive: true, force: true });
    rmSync(outsideFixture, { recursive: true, force: true });
  }
}

const authorityFixture = realpathSync.native(
  mkdtempSync(path.join(os.tmpdir(), 'quickplay-vo-authority-')),
);
try {
  const cargoTarget = path.join(authorityFixture, 'cargo-target');
  const fixtureHome = path.join(authorityFixture, 'home');
  mkdirSync(cargoTarget);
  mkdirSync(fixtureHome);
  const host = 'fixture-host-target';
  const binary = path.join(
    cargoTarget,
    host,
    'debug',
    process.platform === 'win32' ? 'vo.exe' : 'vo',
  );
  const calls = [];
  const runner = (command, args, options) => {
    calls.push({ command, args: [...args], environment: { ...options.env } });
    if (command === 'rustc') {
      return { status: 0, stdout: `rustc fixture\nhost: ${host}\n`, stderr: '' };
    }
    if (command === 'cargo') {
      assert.equal(args[0], 'build');
      assert.equal(args.includes('run'), false);
      assert.equal(args.includes('--locked'), true);
      assert.equal(args.includes('--offline'), false);
      assert.equal(options.env.VOWORK, 'off');
      assert.equal(Object.keys(options.env).some((key) => key.toUpperCase() === 'VO_BIN'), false);
      mkdirSync(path.dirname(binary), { recursive: true });
      writeFileSync(binary, '#!/bin/sh\nexit 0\n', { mode: 0o755 });
      return { status: 0, stdout: '', stderr: '' };
    }
    assert.equal(command, binary);
    return { status: 0, stdout: `${options.env.VOWORK}\n`, stderr: '' };
  };
  const authority = createVoBinaryAuthority({
    root: authorityFixture,
    environment: {
      HOME: fixtureHome,
      PATH: process.env.PATH,
      VO_BIN: path.join(authorityFixture, 'ambient-vo-must-be-ignored'),
      VO_GITHUB_TOKEN: 'ambient-token-must-be-ignored',
      VOWORK: path.join(authorityFixture, 'ambient-vo.work'),
    },
    cargoTargetDirectory: cargoTarget,
    processRunner: runner,
  });
  assert.equal(authority.kind, 'current-root-build');
  assert.equal(invokeVoBinary(authority, ['mod', 'snapshot', 'one'], {
    VO_MOD_CACHE: path.join(authorityFixture, 'cache-one'),
    VOWORK: path.join(authorityFixture, 'one.vo.work'),
  }), `${path.join(authorityFixture, 'one.vo.work')}\n`);
  assert.equal(invokeVoBinary(authority, ['mod', 'snapshot', 'two'], {
    VO_MOD_CACHE: path.join(authorityFixture, 'cache-two'),
    VOWORK: path.join(authorityFixture, 'two.vo.work'),
  }), `${path.join(authorityFixture, 'two.vo.work')}\n`);
  assert.equal(invokeVoBinary(authority, ['version'], {
    VO_GITHUB_TOKEN: 'explicit-bound-token',
    VOWORK: 'off',
  }), 'off\n');
  assert.equal(calls.filter((call) => call.command === 'cargo').length, 1);
  const binaryCalls = calls.filter((call) => call.command === binary);
  assert.equal(binaryCalls.length, 3);
  for (const call of binaryCalls.slice(0, 2)) {
    assert.equal(Object.keys(call.environment).some((key) => key.toUpperCase() === 'VO_BIN'), false);
    assert.equal(Object.keys(call.environment).some((key) => key.toUpperCase() === 'VO_GITHUB_TOKEN'), false);
  }
  assert.equal(Object.keys(binaryCalls[2].environment).some((key) => key.toUpperCase() === 'VO_BIN'), false);
  assert.equal(binaryCalls[2].environment.VO_GITHUB_TOKEN, 'explicit-bound-token');
  assert.throws(
    () => invokeVoBinary(authority, ['version'], { VOWORK: 'one', vowork: 'two' }),
    /duplicate VOWORK spellings/,
  );

  writeFileSync(binary, '#!/bin/sh\nexit 9\n', { mode: 0o755 });
  assert.throws(
    () => invokeVoBinary(authority, ['mod', 'snapshot', 'drift'], { VOWORK: 'off' }),
    /changed after its binary authority was established/,
  );

  const explicitBinary = path.join(authorityFixture, process.platform === 'win32' ? 'explicit.exe' : 'explicit-vo');
  writeFileSync(explicitBinary, '#!/bin/sh\nexit 0\n', { mode: 0o755 });
  const explicitCalls = [];
  const explicitAuthority = createVoBinaryAuthority({
    root: authorityFixture,
    allowVoBin: true,
    voBin: explicitBinary,
    environment: { HOME: fixtureHome, PATH: process.env.PATH },
    processRunner(command, args, options) {
      explicitCalls.push({ command, args, options });
      return { status: 0, stdout: 'explicit\n', stderr: '' };
    },
  });
  assert.equal(
    invokeVoBinary(explicitAuthority, ['version'], { VOWORK: 'off' }),
    'explicit\n',
  );
  const displacedBinary = `${explicitBinary}.old`;
  renameSync(explicitBinary, displacedBinary);
  writeFileSync(explicitBinary, readFileSync(displacedBinary), { mode: 0o755 });
  assert.throws(
    () => invokeVoBinary(explicitAuthority, ['version'], { VOWORK: 'off' }),
    /changed after its binary authority was established/,
  );
  assert.equal(explicitCalls.length, 1);
  assert.throws(
    () => createVoBinaryAuthority({
      root: authorityFixture,
      allowVoBin: true,
      environment: { HOME: fixtureHome, PATH: process.env.PATH },
      processRunner: runner,
    }),
    /explicitly bound absolute voBin path/,
  );
  const duringBinary = path.join(authorityFixture, process.platform === 'win32' ? 'during.exe' : 'during-vo');
  writeFileSync(duringBinary, '#!/bin/sh\nexit 0\n', { mode: 0o755 });
  const duringAuthority = createVoBinaryAuthority({
    root: authorityFixture,
    allowVoBin: true,
    voBin: duringBinary,
    environment: { HOME: fixtureHome, PATH: process.env.PATH },
    processRunner() {
      writeFileSync(duringBinary, '#!/bin/sh\nexit 8\n', { mode: 0o755 });
      return { status: 0, stdout: '', stderr: '' };
    },
  });
  assert.throws(
    () => invokeVoBinary(duringAuthority, ['version'], { VOWORK: 'off' }),
    /during guest execution/,
  );
  if (process.platform !== 'win32') {
    const hardlinkedBinary = path.join(authorityFixture, 'hardlinked-vo');
    linkSync(displacedBinary, hardlinkedBinary);
    assert.throws(
      () => createVoBinaryAuthority({
        root: authorityFixture,
        allowVoBin: true,
        voBin: hardlinkedBinary,
        environment: { HOME: fixtureHome, PATH: process.env.PATH },
        processRunner: runner,
      }),
      /exactly one filesystem link/,
    );
    const linkedBinary = path.join(authorityFixture, 'linked-vo');
    symlinkSync(displacedBinary, linkedBinary);
    assert.throws(
      () => createVoBinaryAuthority({
        root: authorityFixture,
        allowVoBin: true,
        voBin: linkedBinary,
        environment: { HOME: fixtureHome, PATH: process.env.PATH },
        processRunner: runner,
      }),
      /must not traverse symbolic-link path components/,
    );
  }
} finally {
  rmSync(authorityFixture, { recursive: true, force: true });
}

const transactionRoot = mkdtempSync(path.join(os.tmpdir(), 'quickplay-publish-recovery-'));
try {
  const destination = path.join(transactionRoot, 'blockkart');
  mkdirSync(destination);
  writeFileSync(path.join(destination, 'project.json'), '{}\n');
  writeFileSync(path.join(destination, 'deps.json'), '{}\n');
  writeFileSync(path.join(destination, 'provenance.json'), `${JSON.stringify({
    artifact: 'studio.quickplay.blockkart',
    path: 'apps/studio/public/quickplay/blockkart',
  })}\n`);
  assert.equal(recoverableQuickplayDirectory(destination), true);
  const interruptedBackup = `${destination}.backup-9000000000000-1-00000000-0000-4000-8000-000000000000`;
  const incompleteNewerBackup = `${destination}.backup-9999999999999-1-00000000-0000-4000-8000-000000000001`;
  renameSync(destination, interruptedBackup);
  mkdirSync(incompleteNewerBackup);
  writeFileSync(path.join(incompleteNewerBackup, 'provenance.json'), '{}\n');
  withDirectoryReplacementLock(destination, () => {
    assert.equal(
      recoverInterruptedDirectoryReplacement(destination, {
        completeDirectory: recoverableQuickplayDirectory,
        label: 'BlockKart Quickplay',
        warn() {},
      }),
      interruptedBackup,
    );
  });
  assert.equal(existsSync(destination), true);
  assert.equal(existsSync(interruptedBackup), false);
  assert.equal(existsSync(incompleteNewerBackup), false);

  withDirectoryReplacementLock(destination, () => {
    assert.throws(
      () => withDirectoryReplacementLock(destination, () => {}),
      /another directory replacement is active/,
    );
  });

  const crashedDestination = path.join(transactionRoot, 'crashed-blockkart');
  const lockHelperUrl = new URL('./voplay_current_wasm.mjs', import.meta.url).href;
  const crashed = spawnSync(process.execPath, [
    '--input-type=module',
    '--eval',
    `import { withDirectoryReplacementLock } from ${JSON.stringify(lockHelperUrl)}; `
      + `withDirectoryReplacementLock(${JSON.stringify(crashedDestination)}, () => process.exit(37));`,
  ], { encoding: 'utf8' });
  assert.equal(crashed.status, 37, crashed.stderr);
  let staleClaimRecovered = false;
  withDirectoryReplacementLock(crashedDestination, () => {
    staleClaimRecovered = true;
  });
  assert.equal(staleClaimRecovered, true);
  assert.equal(existsSync(`${crashedDestination}.publication-locks`), false);

  if (process.platform !== 'win32') {
    const linkedDestination = path.join(transactionRoot, 'linked-blockkart');
    const linkedBackup = `${linkedDestination}.backup-9000000000000-1-00000000-0000-4000-8000-000000000002`;
    symlinkSync(destination, linkedBackup, 'dir');
    withDirectoryReplacementLock(linkedDestination, () => {
      assert.throws(
        () => recoverInterruptedDirectoryReplacement(linkedDestination, {
          completeDirectory: recoverableQuickplayDirectory,
          label: 'BlockKart Quickplay',
          warn() {},
        }),
        /backup is not a real directory/,
      );
    });
  }
} finally {
  rmSync(transactionRoot, { recursive: true, force: true });
}

for (const taskName of [
  'quickplay-blockkart-package',
  'quickplay-regenerate-check',
  'quickplay-source-audit',
  'quickplay-validate',
  'voplay-engineering-quality-readiness',
]) {
  const task = tasks.split(`name = "${taskName}"`)[1]?.split('[[task]]')[0];
  assert.ok(task, `missing task ${taskName}`);
  const encodedInputs = task.match(/^inputs = \[(.*?)^\]/ms)?.[1]
    ?? task.match(/^inputs = \[(.*)\]$/m)?.[1];
  assert.notEqual(encodedInputs, undefined, `missing task inputs ${taskName}`);
  const inputs = [...encodedInputs.matchAll(/"([^"]+)"/g)].map((match) => match[1]);
  assert.deepEqual(
    QUICKPLAY_ARTIFACT_INPUTS.filter((input) => !inputs.includes(input)),
    [],
    `${taskName} does not cover every Quickplay source input`,
  );
}

const boundaryFixtureRoot = realpathSync.native(
  mkdtempSync(path.join(os.tmpdir(), 'quickplay-generator-boundary-')),
);
try {
  const unsafe = spawnSync(process.execPath, [`${root}/apps/studio/scripts/package_blockkart_quickplay.mjs`], {
    cwd: root,
    env: {
      ...process.env,
      VOLANG_ROOT: root,
      BLOCKKART_ROOT: createModuleRepositoryFixture(
        boundaryFixtureRoot,
        'blockkart',
        'github.com/vo-lang/blockkart',
      ),
      VOGUI_ROOT: createModuleRepositoryFixture(
        boundaryFixtureRoot,
        'vogui',
        'github.com/vo-lang/vogui',
      ),
      VOPACK_ROOT: createModuleRepositoryFixture(
        boundaryFixtureRoot,
        'vopack',
        'github.com/vo-lang/vopack',
      ),
      VOPLAY_ROOT: createModuleRepositoryFixture(
        boundaryFixtureRoot,
        'voplay',
        'github.com/vo-lang/voplay',
      ),
      BLOCKKART_EXPECTED_COMMIT: '',
      VOGUI_EXPECTED_COMMIT: '',
      VOPACK_EXPECTED_COMMIT: '',
      VOPLAY_EXPECTED_COMMIT: '',
      BLOCKKART_QUICKPLAY_OUT_ROOT: `${root}/apps/studio`,
    },
    encoding: 'utf8',
    timeout: 30_000,
  });
  assert.notEqual(unsafe.status, 0);
  assert.match(unsafe.stderr, /must end in a blockkart directory|outside an approved generated-artifact root/);
} finally {
  rmSync(boundaryFixtureRoot, { recursive: true, force: true });
}
console.log('Quickplay generator vNext contract selftest: ok');
