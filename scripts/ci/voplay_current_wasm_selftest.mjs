#!/usr/bin/env node
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  renameSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {
  createVoplayVolangBuildInputs,
  currentVoplayWasmBuildPlatform,
  assertCurrentVoplayBuildInputs,
  publishStagedDirectoryWithRollback,
  recoverInterruptedDirectoryReplacement,
  verifyVoplayVolangBuildInputs,
  verifyVoplaySourceClosure,
  verifyCurrentVoplayWasm,
  voplayFfiSourceFingerprint,
  voplayWasmBuildEnvironment,
  VOPLAY_FFI_SOURCE_FINGERPRINT_ENV,
  VOPLAY_WORKSPACE_ENV,
  VOPLAY_WASM_PRODUCER_SCHEMA_VERSION,
  VOPLAY_WASM_PRODUCER_COMMAND,
  VOPLAY_WASM_REQUIRED_OUTPUTS,
} from './voplay_current_wasm.mjs';

const tempRoot = realpathSync.native(mkdtempSync(path.join(os.tmpdir(), 'voplay-current-wasm-selftest-')));
const fixtureRoot = path.join(tempRoot, 'voplay');
const volangFixtureRoot = path.join(tempRoot, 'volang');
const voguiFixtureRoot = path.join(tempRoot, 'vogui');
const vopackFixtureRoot = path.join(tempRoot, 'vopack');
const outDir = path.join(tempRoot, 'out');

assert(VOPLAY_WASM_PRODUCER_COMMAND.includes('--locked'));

function git(args) {
  return gitIn(fixtureRoot, args);
}

function gitIn(root, args) {
  return execFileSync('git', args, { cwd: root, encoding: 'utf8' }).trim();
}

function commitFixtureRepo(root, message = 'fixture') {
  gitIn(root, ['init', '-q']);
  gitIn(root, ['add', '.']);
  gitIn(root, ['-c', 'user.name=WASM Test', '-c', 'user.email=wasm@example.invalid', 'commit', '-qm', message]);
}

function digest(file) {
  return `sha256:${createHash('sha256').update(readFileSync(file)).digest('hex')}`;
}

function writeCargoPackage(name, { dependency = null, stdlibSource = false } = {}) {
  const directory = path.join(volangFixtureRoot, 'lang', 'crates', name);
  mkdirSync(directory, { recursive: true });
  const dependencies = [
    dependency ? `${dependency} = { path = "../${dependency}" }` : null,
    stdlibSource ? 'vo-stdlib-source = { path = "../../stdlib" }' : null,
  ].filter(Boolean);
  const dependencyToml = dependencies.length > 0
    ? `\n[dependencies]\n${dependencies.join('\n')}\n`
    : '';
  writeFileSync(
    path.join(directory, 'Cargo.toml'),
    `[package]\nname = "${name}"\nversion = "0.1.0"\nedition = "2021"\n\n[lib]\npath = "lib.rs"\n${dependencyToml}`,
  );
  writeFileSync(path.join(directory, 'lib.rs'), `pub fn ${name}() {}\n`);
}

try {
  mkdirSync(fixtureRoot);
  mkdirSync(voguiFixtureRoot);
  mkdirSync(vopackFixtureRoot);
  mkdirSync(path.join(volangFixtureRoot, '.cargo'), { recursive: true });
  mkdirSync(path.join(volangFixtureRoot, 'lang', 'stdlib'), { recursive: true });
  mkdirSync(path.join(volangFixtureRoot, 'docs'), { recursive: true });
  writeFileSync(path.join(volangFixtureRoot, '.cargo', 'config.toml'), '[build]\nincremental = false\n');
  writeFileSync(path.join(volangFixtureRoot, 'Cargo.lock'), '# scoped Volang lock fixture\n');
  writeFileSync(
    path.join(volangFixtureRoot, 'Cargo.toml'),
    '[workspace]\nresolver = "2"\nmembers = ["lang/crates/dep_new", "lang/crates/dep_one", "lang/crates/dep_optional", "lang/crates/dep_target", "lang/crates/dep_two", "lang/crates/dep_unused", "lang/stdlib"]\n',
  );
  writeFileSync(path.join(volangFixtureRoot, 'rust-toolchain.toml'), '[toolchain]\nchannel = "stable"\n');
  writeFileSync(
    path.join(volangFixtureRoot, 'lang', 'stdlib', 'Cargo.toml'),
    '[package]\nname = "vo-stdlib-source"\nversion = "0.1.0"\nedition = "2021"\n\n[lib]\npath = "lib.rs"\n',
  );
  writeFileSync(path.join(volangFixtureRoot, 'lang', 'stdlib', 'lib.rs'), 'pub fn source() {}\n');
  writeFileSync(path.join(volangFixtureRoot, 'lang', 'stdlib', 'stdlib.toml'), 'packages = []\n');
  writeFileSync(path.join(volangFixtureRoot, 'docs', 'unrelated.md'), 'unrelated v1\n');
  writeCargoPackage('dep_two', { stdlibSource: true });
  writeCargoPackage('dep_one', { dependency: 'dep_two' });
  writeCargoPackage('dep_new');
  writeCargoPackage('dep_optional');
  writeCargoPackage('dep_target');
  writeCargoPackage('dep_unused');
  commitFixtureRepo(volangFixtureRoot, 'Volang fixture');

  mkdirSync(path.join(voguiFixtureRoot, 'rust'));
  writeFileSync(
    path.join(voguiFixtureRoot, 'vo.mod'),
    'module github.com/vo-lang/vogui\n\nvo ^0.1.0\n',
  );
  writeFileSync(
    path.join(voguiFixtureRoot, 'rust', 'Cargo.toml'),
    '[package]\nname = "vogui"\nversion = "0.1.0"\nedition = "2021"\n\n[lib]\npath = "lib.rs"\n\n[workspace]\n',
  );
  writeFileSync(path.join(voguiFixtureRoot, 'rust', 'lib.rs'), 'pub fn vogui() {}\n');
  commitFixtureRepo(voguiFixtureRoot, 'vogui fixture');

  writeFileSync(
    path.join(vopackFixtureRoot, 'vo.mod'),
    'module github.com/vo-lang/vopack\n\nvo ^0.1.0\n',
  );
  writeFileSync(path.join(vopackFixtureRoot, 'pack.vo'), 'package vopack\n');
  commitFixtureRepo(vopackFixtureRoot, 'vopack fixture');

  mkdirSync(path.join(fixtureRoot, 'rust'));
  writeFileSync(
    path.join(fixtureRoot, 'vo.mod'),
    'module github.com/vo-lang/voplay\n\nvo ^0.1.0\n\nrequire github.com/vo-lang/vogui v0.1.0\nrequire github.com/vo-lang/vopack v0.1.0\n',
  );
  writeFileSync(
    path.join(fixtureRoot, 'vo.work'),
    'version = 1\n\n[[use]]\npath = "__PARENT__/vopack"\n\n[[use]]\npath = "__PARENT__/vogui"\n'
      .replaceAll('__PARENT__', '..'),
  );
  writeFileSync(path.join(fixtureRoot, 'renderer.rs'), 'pub fn render() {}\n');
  const cargoManifest = path.join(fixtureRoot, 'rust', 'Cargo.toml');
  writeFileSync(
    cargoManifest,
    '[package]\nname = "vo-voplay"\nversion = "0.1.0"\nedition = "2021"\n\n[lib]\npath = "lib.rs"\n\n[features]\ndefault = []\nunused = ["dep:dep_optional"]\nwasm-island = []\n\n[dependencies]\ndep_one = { path = "../../volang/lang/crates/dep_one" }\ndep_optional = { path = "../../volang/lang/crates/dep_optional", optional = true }\nvogui = { path = "__TWO_PARENTS__/vogui/rust" }\n\n[dev-dependencies]\ndep_unused = { path = "../../volang/lang/crates/dep_unused" }\n\n[target.x86_64-pc-windows-msvc.dependencies]\ndep_target = { path = "../../volang/lang/crates/dep_target" }\n\n[workspace]\n'
      .replaceAll('__TWO_PARENTS__', '../..'),
  );
  writeFileSync(path.join(fixtureRoot, 'rust', 'lib.rs'), 'pub fn fixture() {}\n');
  execFileSync('cargo', ['generate-lockfile', '--manifest-path', cargoManifest], {
    cwd: fixtureRoot,
    stdio: 'pipe',
  });
  commitFixtureRepo(fixtureRoot, 'voplay fixture');
  const sourceEnvironment = {
    VOWORK: path.join(fixtureRoot, 'vo.work'),
    VOPLAY_ROOT: fixtureRoot,
    VOGUI_ROOT: voguiFixtureRoot,
    VOPACK_ROOT: vopackFixtureRoot,
  };
  const preflight = assertCurrentVoplayBuildInputs(fixtureRoot, {
    volangRoot: volangFixtureRoot,
    environment: sourceEnvironment,
  });
  const { sourceClosure, volangBuildInputs } = preflight;
  assert.equal(preflight.workspaceFile, path.join(fixtureRoot, 'vo.work'));
  assert.deepEqual(
    sourceClosure.repositories.map((repository) => repository.name),
    [
      'github.com/vo-lang/vogui',
      'github.com/vo-lang/vopack',
      'github.com/vo-lang/voplay',
    ],
  );
  assert.deepEqual(
    sourceClosure.repositories.find((repository) => repository.name.endsWith('/vogui')).roles,
    ['cargo-path', 'vo-workspace'],
  );
  assert.deepEqual(
    sourceClosure.repositories.find((repository) => repository.name.endsWith('/vopack')).roles,
    ['vo-workspace'],
  );
  assert(!JSON.stringify(sourceClosure).includes(tempRoot));
  assert.deepEqual(verifyVoplaySourceClosure(sourceClosure, { expected: sourceClosure }), []);
  assert.equal(
    preflight.ffiSourceFingerprint,
    voplayFfiSourceFingerprint(sourceClosure.digest, volangBuildInputs.digest),
  );
  assert.notEqual(
    voplayFfiSourceFingerprint(`sha256:${'0'.repeat(64)}`, volangBuildInputs.digest),
    preflight.ffiSourceFingerprint,
  );
  assert.notEqual(
    voplayFfiSourceFingerprint(sourceClosure.digest, `sha256:${'0'.repeat(64)}`),
    preflight.ffiSourceFingerprint,
  );
  assert.deepEqual(
    voplayWasmBuildEnvironment(preflight, {
      PATH: '/fixture/bin',
      [VOPLAY_FFI_SOURCE_FINGERPRINT_ENV]: 'stale inherited value',
      [VOPLAY_WORKSPACE_ENV]: 'stale inherited workspace',
    }),
    {
      PATH: '/fixture/bin',
      [VOPLAY_FFI_SOURCE_FINGERPRINT_ENV]: preflight.ffiSourceFingerprint,
      [VOPLAY_WORKSPACE_ENV]: path.join(fixtureRoot, 'vo.work'),
    },
  );
  assert.throws(
    () => voplayWasmBuildEnvironment({
      ...preflight,
      workspaceFile: path.join(voguiFixtureRoot, 'vo.mod'),
    }),
    /workspace file (?:does not match|is not)/,
  );
  assert.throws(
    () => assertCurrentVoplayBuildInputs(fixtureRoot, {
      volangRoot: volangFixtureRoot,
      environment: { ...sourceEnvironment, VOWORK: 'off' },
    }),
    /VOWORK=off is unsupported/,
  );
  assert.deepEqual(
    volangBuildInputs.packages.map((entry) => entry.path),
    ['lang/crates/dep_one', 'lang/crates/dep_two', 'lang/stdlib'],
  );
  assert.deepEqual(verifyVoplayVolangBuildInputs(volangBuildInputs, volangFixtureRoot), []);

  mkdirSync(outDir);
  for (const name of VOPLAY_WASM_REQUIRED_OUTPUTS) {
    writeFileSync(path.join(outDir, name), `fixture:${name}\n`);
  }
  const outputs = VOPLAY_WASM_REQUIRED_OUTPUTS.map((name) => {
    const file = path.join(outDir, name);
    return { name, size: readFileSync(file).byteLength, digest: digest(file) };
  });
  const producerManifest = {
    schemaVersion: VOPLAY_WASM_PRODUCER_SCHEMA_VERSION,
    kind: 'voplay.currentSourceWasm',
    generatedAt: new Date().toISOString(),
    ciRunId: 'wasm-selftest',
    command: VOPLAY_WASM_PRODUCER_COMMAND,
    sourceClosure,
    volangBuildInputs,
    ffiSourceFingerprint: preflight.ffiSourceFingerprint,
    toolchain: { rustc: 'fixture', wasmPack: 'fixture' },
    buildPlatform: currentVoplayWasmBuildPlatform(),
    outputs,
  };
  const writeProducerManifest = () => writeFileSync(
    path.join(outDir, 'producer-manifest.json'),
    `${JSON.stringify(producerManifest, null, 2)}\n`,
  );
  writeProducerManifest();

  assert.deepEqual(verifyCurrentVoplayWasm({
    voplayRoot: fixtureRoot,
    volangRoot: volangFixtureRoot,
    outDir,
    expectedCiRunId: 'wasm-selftest',
    expectedVolangBuildInputs: volangBuildInputs,
    expectedSourceClosure: sourceClosure,
    environment: sourceEnvironment,
  }).issues, []);

  const omittedClosure = structuredClone(sourceClosure);
  omittedClosure.repositories = omittedClosure.repositories.filter(
    (repository) => repository.name !== 'github.com/vo-lang/vopack',
  );
  assert(
    verifyVoplaySourceClosure(omittedClosure, { expected: sourceClosure }).some(
      (issue) => issue.includes('sourceClosure'),
    ),
  );
  const forgedClosure = structuredClone(sourceClosure);
  forgedClosure.repositories[0].commit = 'f'.repeat(40);
  assert(
    verifyVoplaySourceClosure(forgedClosure, { expected: sourceClosure }).some(
      (issue) => issue.includes('digest') || issue.includes('current locked local source graph'),
    ),
  );
  assert.doesNotThrow(() => verifyVoplaySourceClosure({
    ...sourceClosure,
    repositories: [{}, {}],
  }));
  assert(
    verifyVoplaySourceClosure({
      ...sourceClosure,
      repositories: [{}, {}],
    }).some((issue) => issue.includes('repository')),
  );

  const voguiSource = path.join(voguiFixtureRoot, 'rust', 'lib.rs');
  writeFileSync(voguiSource, 'pub fn vogui_dirty() {}\n');
  assert.throws(
    () => assertCurrentVoplayBuildInputs(fixtureRoot, {
      volangRoot: volangFixtureRoot,
      environment: sourceEnvironment,
    }),
    /source repository must be clean.*github\.com\/vo-lang\/vogui/s,
  );
  gitIn(voguiFixtureRoot, ['checkout', '--', 'rust/lib.rs']);

  const originalVoguiCommit = gitIn(voguiFixtureRoot, ['rev-parse', 'HEAD']);
  writeFileSync(voguiSource, 'pub fn vogui_changed() {}\n');
  gitIn(voguiFixtureRoot, ['add', 'rust/lib.rs']);
  gitIn(voguiFixtureRoot, [
    '-c',
    'user.name=WASM Test',
    '-c',
    'user.email=wasm@example.invalid',
    'commit',
    '-qm',
    'changed sibling',
  ]);
  const changedSibling = assertCurrentVoplayBuildInputs(fixtureRoot, {
    volangRoot: volangFixtureRoot,
    environment: sourceEnvironment,
  });
  assert.notEqual(changedSibling.sourceClosure.digest, sourceClosure.digest);
  assert.notEqual(changedSibling.ffiSourceFingerprint, preflight.ffiSourceFingerprint);
  assert(verifyCurrentVoplayWasm({
    voplayRoot: fixtureRoot,
    volangRoot: volangFixtureRoot,
    outDir,
    expectedCiRunId: 'wasm-selftest',
    environment: sourceEnvironment,
  }).issues.some((issue) => issue.includes('sourceClosure')));
  gitIn(voguiFixtureRoot, ['checkout', '-q', originalVoguiCommit]);

  const workfilePath = path.join(fixtureRoot, 'vo.work');
  writeFileSync(workfilePath, `${readFileSync(workfilePath, 'utf8')}# changed workspace generation\n`);
  assert(verifyCurrentVoplayWasm({
    voplayRoot: fixtureRoot,
    volangRoot: volangFixtureRoot,
    outDir,
    expectedCiRunId: 'wasm-selftest',
    environment: sourceEnvironment,
  }).issues.some((issue) => issue.includes('sourceClosure')));
  git(['checkout', '--', 'vo.work']);

  const voguiAlias = path.join(tempRoot, 'vogui-alias');
  symlinkSync(voguiFixtureRoot, voguiAlias);
  writeFileSync(
    workfilePath,
    'version = 1\n\n[[use]]\npath = "__PARENT__/vopack"\n\n[[use]]\npath = "__PARENT__/vogui-alias"\n'
      .replaceAll('__PARENT__', '..'),
  );
  assert.throws(
    () => assertCurrentVoplayBuildInputs(fixtureRoot, {
      volangRoot: volangFixtureRoot,
      environment: sourceEnvironment,
    }),
    /layout alias/,
  );
  git(['checkout', '--', 'vo.work']);
  rmSync(voguiAlias);

  writeFileSync(path.join(fixtureRoot, 'renderer.rs'), 'pub fn render_changed() {}\n');
  assert(verifyCurrentVoplayWasm({
    voplayRoot: fixtureRoot,
    volangRoot: volangFixtureRoot,
    outDir,
    expectedCiRunId: 'wasm-selftest',
    environment: sourceEnvironment,
  }).issues.some((issue) => issue.includes('sourceClosure')));

  git(['checkout', '--', 'renderer.rs']);
  writeFileSync(path.join(volangFixtureRoot, 'lang', 'crates', 'dep_two', 'lib.rs'), 'pub fn dep_two_changed() {}\n');
  assert(verifyCurrentVoplayWasm({
    voplayRoot: fixtureRoot,
    volangRoot: volangFixtureRoot,
    outDir,
    expectedCiRunId: 'wasm-selftest',
    environment: sourceEnvironment,
  }).issues.some((issue) => issue.includes('volangBuildInputs.digest')));
  writeFileSync(path.join(volangFixtureRoot, 'lang', 'crates', 'dep_two', 'lib.rs'), 'pub fn dep_two() {}\n');

  const cargoConfigPath = path.join(volangFixtureRoot, '.cargo', 'config.toml');
  writeFileSync(cargoConfigPath, '[build]\nincremental = true\n');
  assert(verifyCurrentVoplayWasm({
    voplayRoot: fixtureRoot,
    volangRoot: volangFixtureRoot,
    outDir,
    expectedCiRunId: 'wasm-selftest',
    environment: sourceEnvironment,
  }).issues.some((issue) => issue.includes('volangBuildInputs.digest')));
  writeFileSync(cargoConfigPath, '[build]\nincremental = false\n');

  const stdlibManifestPath = path.join(volangFixtureRoot, 'lang', 'stdlib', 'stdlib.toml');
  writeFileSync(stdlibManifestPath, 'packages = ["changed"]\n');
  assert(verifyCurrentVoplayWasm({
    voplayRoot: fixtureRoot,
    volangRoot: volangFixtureRoot,
    outDir,
    expectedCiRunId: 'wasm-selftest',
    environment: sourceEnvironment,
  }).issues.some((issue) => issue.includes('volangBuildInputs.digest')));
  writeFileSync(stdlibManifestPath, 'packages = []\n');

  writeFileSync(path.join(volangFixtureRoot, 'docs', 'unrelated.md'), 'unrelated v2\n');
  assert.deepEqual(verifyCurrentVoplayWasm({
    voplayRoot: fixtureRoot,
    volangRoot: volangFixtureRoot,
    outDir,
    expectedCiRunId: 'wasm-selftest',
    environment: sourceEnvironment,
  }).issues, []);

  const incompleteInputs = createVoplayVolangBuildInputs(volangFixtureRoot, [
    volangBuildInputs.packages[0],
  ]);
  producerManifest.volangBuildInputs = incompleteInputs;
  writeProducerManifest();
  assert(verifyCurrentVoplayWasm({
    voplayRoot: fixtureRoot,
    volangRoot: volangFixtureRoot,
    outDir,
    expectedCiRunId: 'wasm-selftest',
    expectedVolangBuildInputs: volangBuildInputs,
    expectedSourceClosure: sourceClosure,
    environment: sourceEnvironment,
  }).issues.includes('volangBuildInputs do not match locked Cargo metadata'));
  producerManifest.volangBuildInputs = volangBuildInputs;
  writeProducerManifest();

  producerManifest.ffiSourceFingerprint = `sha256:${'f'.repeat(64)}`;
  writeProducerManifest();
  assert(verifyCurrentVoplayWasm({
    voplayRoot: fixtureRoot,
    volangRoot: volangFixtureRoot,
    outDir,
    expectedCiRunId: 'wasm-selftest',
    expectedVolangBuildInputs: volangBuildInputs,
    expectedSourceClosure: sourceClosure,
    environment: sourceEnvironment,
  }).issues.some((issue) => issue.includes('ffiSourceFingerprint')));
  producerManifest.ffiSourceFingerprint = preflight.ffiSourceFingerprint;
  writeProducerManifest();

  writeFileSync(path.join(outDir, 'producer-manifest.json'), 'null\n');
  assert.deepEqual(verifyCurrentVoplayWasm({
    voplayRoot: fixtureRoot,
    volangRoot: volangFixtureRoot,
    outDir,
    expectedVolangBuildInputs: volangBuildInputs,
    expectedSourceClosure: sourceClosure,
    environment: sourceEnvironment,
  }).issues, ['producer manifest must be an object']);
  writeProducerManifest();

  assert(verifyVoplayVolangBuildInputs({
    ...volangBuildInputs,
    packages: [{ name: 'escape', version: '0.1.0', path: 'scripts/ci' }],
  }, volangFixtureRoot).some((issue) => issue.includes('non-canonical')));

  writeFileSync(path.join(outDir, 'voplay_island_bg.wasm'), 'stale artifact\n');
  assert(verifyCurrentVoplayWasm({
    voplayRoot: fixtureRoot,
    volangRoot: volangFixtureRoot,
    outDir,
    expectedCiRunId: 'wasm-selftest',
    environment: sourceEnvironment,
  }).issues.some((issue) => issue.includes('output digest')));

  writeFileSync(
    cargoManifest,
    readFileSync(cargoManifest, 'utf8').replace(
      '\n[workspace]\n',
      '\ndep_new = { path = "../../volang/lang/crates/dep_new" }\n\n[workspace]\n',
    ),
  );
  assert.throws(
    () => assertCurrentVoplayBuildInputs(fixtureRoot, {
      volangRoot: volangFixtureRoot,
      environment: sourceEnvironment,
    }),
    /voplay Cargo\.lock is stale.*Refresh and commit that lockfile/s,
  );

  git(['checkout', '--', 'rust/Cargo.toml']);
  writeFileSync(path.join(fixtureRoot, 'renderer.rs'), 'pub fn dirty() {}\n');
  assert.throws(
    () => assertCurrentVoplayBuildInputs(fixtureRoot, {
      volangRoot: volangFixtureRoot,
      environment: sourceEnvironment,
    }),
    /source repository must be clean.*renderer\.rs/s,
  );

  const transactionRoot = path.join(tempRoot, 'transaction');
  const publishedDir = path.join(transactionRoot, 'published');
  const stagingDir = path.join(transactionRoot, 'staging');
  mkdirSync(publishedDir, { recursive: true });
  mkdirSync(stagingDir);
  writeFileSync(path.join(publishedDir, 'old.txt'), 'last valid output\n');
  writeFileSync(path.join(stagingDir, 'new.txt'), 'new verified output\n');
  let renameCalls = 0;
  assert.throws(
    () => publishStagedDirectoryWithRollback(stagingDir, publishedDir, {
      renameSync(from, to) {
        renameCalls += 1;
        if (renameCalls === 2) throw new Error('injected publish rename failure');
        renameSync(from, to);
      },
    }),
    /previous output was restored.*injected publish rename failure/s,
  );
  assert.equal(readFileSync(path.join(publishedDir, 'old.txt'), 'utf8'), 'last valid output\n');
  assert.equal(readFileSync(path.join(stagingDir, 'new.txt'), 'utf8'), 'new verified output\n');

  publishStagedDirectoryWithRollback(stagingDir, publishedDir);
  assert(!existsSync(stagingDir));
  assert(!existsSync(path.join(publishedDir, 'old.txt')));
  assert.equal(readFileSync(path.join(publishedDir, 'new.txt'), 'utf8'), 'new verified output\n');

  writeFileSync(path.join(publishedDir, 'producer-manifest.json'), '{}\n');
  for (const name of VOPLAY_WASM_REQUIRED_OUTPUTS) {
    writeFileSync(path.join(publishedDir, name), `recovery:${name}\n`);
  }
  const interruptedBackup = `${publishedDir}.backup-9000000000000-1-00000000-0000-4000-8000-000000000000`;
  const incompleteNewerBackup = `${publishedDir}.backup-9999999999999-1-00000000-0000-4000-8000-000000000001`;
  renameSync(publishedDir, interruptedBackup);
  mkdirSync(incompleteNewerBackup);
  writeFileSync(path.join(incompleteNewerBackup, 'producer-manifest.json'), '{}\n');
  assert(!existsSync(publishedDir));
  assert.equal(
    recoverInterruptedDirectoryReplacement(publishedDir, { warn() {} }),
    interruptedBackup,
  );
  assert.equal(readFileSync(path.join(publishedDir, 'new.txt'), 'utf8'), 'new verified output\n');
  assert(!existsSync(interruptedBackup));
  assert(existsSync(incompleteNewerBackup));

  console.log('voplay current-source WASM selftest: ok');
} finally {
  rmSync(tempRoot, { recursive: true, force: true });
}
