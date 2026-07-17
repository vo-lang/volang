#!/usr/bin/env node
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import {
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
  assertVoguiStandaloneHostImportAuthority,
  captureVoguiSource,
  currentVoguiWasmBuildPlatform,
  recoverInterruptedVoguiDirectoryReplacement,
  verifyCurrentVoguiWasm,
  verifyVoguiSource,
  voguiFfiSourceFingerprint,
  voguiWasmBuildEnvironment,
  VOGUI_FFI_SOURCE_FINGERPRINT_ENV,
  VOGUI_WASM_PRODUCER_COMMAND,
  VOGUI_WASM_PRODUCER_SCHEMA_VERSION,
  VOGUI_WASM_REQUIRED_OUTPUTS,
  VOGUI_WORKSPACE_ENV,
} from './vogui_current_wasm.mjs';
import {
  createVoplayVolangBuildInputs,
  withDirectoryReplacementLock,
} from './voplay_current_wasm.mjs';
import {
  extensionExportCatalogFromDirectory,
  standaloneHostImportsFromRustSource,
  VOGUI_STANDALONE_HOST_IMPORTS_V3,
} from './wasm_protocol_v3.mjs';
import {
  protocolWasmFixture,
} from './wasm_protocol_v3_fixtures.mjs';

const tempRoot = realpathSync.native(
  mkdtempSync(path.join(os.tmpdir(), 'vogui-current-wasm-selftest-')),
);
const voguiRoot = path.join(tempRoot, 'vogui');
const volangRoot = path.join(tempRoot, 'volang');
const outDir = path.join(tempRoot, 'out');
const voguiVoSource = 'package vogui\n\nfunc fixtureExtern()\n';

function sha256(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function git(args) {
  return execFileSync('git', args, { cwd: voguiRoot, encoding: 'utf8' }).trim();
}

function writeManifest(manifest) {
  writeFileSync(
    path.join(outDir, 'producer-manifest.json'),
    `${JSON.stringify(manifest, null, 2)}\n`,
  );
}

function fixtureRustHostAuthority(imports) {
  const rustType = (type) => {
    if (type === 0x7f) return 'i32';
    if (type === 0x7c) return 'f64';
    throw new Error(`unsupported fixture WASM type ${type}`);
  };
  const declarations = imports.map((entry) => {
    const parameters = entry.parameters
      .map((type, index) => `arg${index}: ${rustType(type)}`)
      .join(', ');
    const result = entry.results.length === 0 ? '' : ` -> ${rustType(entry.results[0])}`;
    return `    fn ${entry.name}(${parameters})${result};`;
  });
  return [
    '#[cfg_attr(target_arch = "wasm32", link(wasm_import_module = "env"))]',
    'extern "C" {',
    ...declarations,
    '}',
    '',
  ].join('\n');
}

try {
  assert(VOGUI_WASM_PRODUCER_COMMAND.includes('--locked'));
  assert.deepEqual(VOGUI_WASM_REQUIRED_OUTPUTS, ['vogui.wasm']);

  mkdirSync(path.join(voguiRoot, 'rust', 'ext', 'src'), { recursive: true });
  writeFileSync(
    path.join(voguiRoot, 'vo.mod'),
    'module = "github.com/vo-lang/vogui"\nvo = "^0.1.0"\n',
  );
  writeFileSync(path.join(voguiRoot, 'vo.work'), 'version = 1\nmembers = []\n');
  writeFileSync(path.join(voguiRoot, 'gui.vo'), voguiVoSource);
  writeFileSync(path.join(voguiRoot, 'rust', 'Cargo.lock'), '# fixture lock\n');
  writeFileSync(
    path.join(voguiRoot, 'rust', 'ext', 'Cargo.toml'),
    '[package]\nname = "vogui"\nversion = "0.1.0"\nedition = "2021"\n',
  );
  writeFileSync(path.join(voguiRoot, 'rust', 'ext', 'src', 'lib.rs'), 'pub fn fixture() {}\n');
  const rustHostAuthority = fixtureRustHostAuthority(VOGUI_STANDALONE_HOST_IMPORTS_V3);
  writeFileSync(
    path.join(voguiRoot, 'rust', 'ext', 'src', 'standalone.rs'),
    rustHostAuthority,
  );
  writeFileSync(
    path.join(voguiRoot, 'rust', 'ext', 'src', 'externs.rs'),
    'vo_ext::vo_extension_entry!("vogui", "fixtureExtern");\n',
  );
  git(['init', '-q']);
  git(['add', '.']);
  git([
    '-c',
    'user.name=WASM Test',
    '-c',
    'user.email=wasm@example.invalid',
    'commit',
    '-qm',
    'fixture',
  ]);

  assert.deepEqual(
    standaloneHostImportsFromRustSource(Buffer.from(`
#[cfg_attr(target_arch = "wasm32", link(wasm_import_module = "env"))]
extern "C" {
    fn host_pointer_fixture(input: *const u8, out_len: *mut u32) -> *const u8;
}
`, 'utf8')),
    [{
      module: 'env',
      name: 'host_pointer_fixture',
      parameters: [0x7f, 0x7f],
      results: [0x7f],
    }],
  );
  assert.deepEqual(
    assertVoguiStandaloneHostImportAuthority(voguiRoot),
    VOGUI_STANDALONE_HOST_IMPORTS_V3,
  );

  mkdirSync(path.join(volangRoot, '.cargo'), { recursive: true });
  mkdirSync(path.join(volangRoot, 'lang', 'stdlib'), { recursive: true });
  mkdirSync(path.join(volangRoot, 'lang', 'crates', 'vo-vm'), { recursive: true });
  writeFileSync(path.join(volangRoot, '.cargo', 'config.toml'), '[build]\nincremental = false\n');
  writeFileSync(path.join(volangRoot, 'Cargo.lock'), '# fixture Volang lock\n');
  writeFileSync(path.join(volangRoot, 'Cargo.toml'), '[workspace]\nmembers = []\n');
  writeFileSync(path.join(volangRoot, 'rust-toolchain.toml'), '[toolchain]\nchannel = "stable"\n');
  writeFileSync(path.join(volangRoot, 'lang', 'stdlib', 'stdlib.toml'), 'packages = []\n');
  writeFileSync(
    path.join(volangRoot, 'lang', 'crates', 'vo-vm', 'Cargo.toml'),
    '[package]\nname = "vo-vm"\nversion = "0.1.0"\n',
  );
  writeFileSync(path.join(volangRoot, 'lang', 'crates', 'vo-vm', 'lib.rs'), 'pub fn vm() {}\n');

  const source = captureVoguiSource(voguiRoot);
  assert.deepEqual(verifyVoguiSource(source, { expected: source }), []);
  assert.equal(source.module, 'github.com/vo-lang/vogui');
  assert.equal(source.dirty, false);
  assert.equal(source.cargoLock.digest, sha256(readFileSync(path.join(voguiRoot, 'rust', 'Cargo.lock'))));
  const rustHostAuthorityPath = path.join(voguiRoot, 'rust', 'ext', 'src', 'standalone.rs');
  writeFileSync(rustHostAuthorityPath, rustHostAuthority.replace('arg1: i32', 'arg1: f64'));
  assert.throws(
    () => assertVoguiStandaloneHostImportAuthority(voguiRoot),
    /differs from the protocol-v3 authority table/,
  );
  writeFileSync(rustHostAuthorityPath, rustHostAuthority);
  assert.deepEqual(captureVoguiSource(voguiRoot), source);

  const volangBuildInputs = createVoplayVolangBuildInputs(volangRoot, [{
    name: 'vo-vm',
    version: '0.1.0',
    path: 'lang/crates/vo-vm',
  }]);
  const ffiSourceFingerprint = voguiFfiSourceFingerprint(
    source.digest,
    volangBuildInputs.digest,
  );
  assert.notEqual(
    voguiFfiSourceFingerprint(`sha256:${'0'.repeat(64)}`, volangBuildInputs.digest),
    ffiSourceFingerprint,
  );

  const preflight = { source, volangBuildInputs, ffiSourceFingerprint };
  assert.deepEqual(
    voguiWasmBuildEnvironment(preflight, voguiRoot, {
      PATH: '/fixture/bin',
      [VOGUI_FFI_SOURCE_FINGERPRINT_ENV]: 'stale',
      [VOGUI_WORKSPACE_ENV]: 'stale',
    }),
    {
      PATH: '/fixture/bin',
      [VOGUI_FFI_SOURCE_FINGERPRINT_ENV]: ffiSourceFingerprint,
      [VOGUI_WORKSPACE_ENV]: path.join(voguiRoot, 'vo.work'),
    },
  );
  assert.throws(
    () => voguiWasmBuildEnvironment({
      ...preflight,
      ffiSourceFingerprint: `sha256:${'1'.repeat(64)}`,
    }, voguiRoot),
    /fingerprint/,
  );

  mkdirSync(outDir);
  const browserCatalog = extensionExportCatalogFromDirectory(voguiRoot, {
    modulePath: 'github.com/vo-lang/vogui',
    extensionName: 'vogui',
    rustEntrySource: 'rust/ext/src/externs.rs',
  });
  const validWasm = protocolWasmFixture({
    standaloneExportKeys: browserCatalog.exportKeys,
    imports: VOGUI_STANDALONE_HOST_IMPORTS_V3,
  });
  writeFileSync(path.join(outDir, 'vogui.wasm'), validWasm);
  const outputBytes = readFileSync(path.join(outDir, 'vogui.wasm'));
  const manifest = {
    schemaVersion: VOGUI_WASM_PRODUCER_SCHEMA_VERSION,
    kind: 'vogui.currentSourceWasm',
    generatedAt: '2026-07-16T00:00:00.000Z',
    ciRunId: 'vogui-selftest',
    command: [...VOGUI_WASM_PRODUCER_COMMAND],
    source,
    volangBuildInputs,
    ffiSourceFingerprint,
    toolchain: { cargo: 'fixture-cargo', rustc: 'fixture-rustc' },
    buildPlatform: currentVoguiWasmBuildPlatform(),
    outputs: [{
      name: 'vogui.wasm',
      size: outputBytes.byteLength,
      digest: sha256(outputBytes),
    }],
  };
  writeManifest(manifest);

  const verify = () => verifyCurrentVoguiWasm({
    voguiRoot,
    volangRoot,
    outDir,
    expectedCiRunId: 'vogui-selftest',
    expectedSource: source,
    expectedVolangBuildInputs: volangBuildInputs,
  });
  const refreshOutputBinding = () => {
    const bytes = readFileSync(path.join(outDir, 'vogui.wasm'));
    manifest.outputs = [{
      name: 'vogui.wasm',
      size: bytes.byteLength,
      digest: sha256(bytes),
    }];
    writeManifest(manifest);
  };
  assert.deepEqual(verify().issues, []);

  writeFileSync(
    path.join(outDir, 'vogui.wasm'),
    protocolWasmFixture({
      standaloneExportKeys: browserCatalog.exportKeys,
      imports: VOGUI_STANDALONE_HOST_IMPORTS_V3.slice(1),
    }),
  );
  refreshOutputBinding();
  assert(verify().issues.some((issue) => issue.includes('missing host import host_start_timeout')));

  writeFileSync(
    path.join(outDir, 'vogui.wasm'),
    protocolWasmFixture({
      standaloneExportKeys: browserCatalog.exportKeys,
      imports: [
        ...VOGUI_STANDALONE_HOST_IMPORTS_V3,
        { module: 'env', name: 'host_fixture', parameters: [], results: [] },
      ],
    }),
  );
  refreshOutputBinding();
  assert(verify().issues.some((issue) => issue.includes('unexpected host import env.host_fixture')));

  writeFileSync(
    path.join(outDir, 'vogui.wasm'),
    protocolWasmFixture({
      standaloneExportKeys: browserCatalog.exportKeys,
      imports: VOGUI_STANDALONE_HOST_IMPORTS_V3.map((entry, index) => (
        index === 0 ? { ...entry, module: 'wrong' } : entry
      )),
    }),
  );
  refreshOutputBinding();
  assert(verify().issues.some((issue) => issue.includes('must be imported from module env')));

  writeFileSync(
    path.join(outDir, 'vogui.wasm'),
    protocolWasmFixture({
      standaloneExportKeys: browserCatalog.exportKeys,
      imports: VOGUI_STANDALONE_HOST_IMPORTS_V3.map((entry, index) => (
        index === 0 ? { ...entry, kind: 'table' } : entry
      )),
    }),
  );
  refreshOutputBinding();
  assert(verify().issues.some((issue) => issue.includes('may import functions only')));

  writeFileSync(
    path.join(outDir, 'vogui.wasm'),
    protocolWasmFixture({
      standaloneExportKeys: browserCatalog.exportKeys,
      imports: VOGUI_STANDALONE_HOST_IMPORTS_V3.map((entry, index) => (
        index === 0 ? { ...entry, parameters: [] } : entry
      )),
    }),
  );
  refreshOutputBinding();
  assert(verify().issues.some((issue) => issue.includes('invalid WebAssembly function signature')));

  writeFileSync(
    path.join(outDir, 'vogui.wasm'),
    protocolWasmFixture({
      standaloneExportKeys: browserCatalog.exportKeys,
      imports: [
        ...VOGUI_STANDALONE_HOST_IMPORTS_V3,
        VOGUI_STANDALONE_HOST_IMPORTS_V3[0],
      ],
    }),
  );
  refreshOutputBinding();
  assert(verify().issues.some((issue) => issue.includes('duplicate host import')));

  writeFileSync(
    path.join(outDir, 'vogui.wasm'),
    protocolWasmFixture({
      includeProtocol: false,
      standaloneExportKeys: browserCatalog.exportKeys,
      customSectionText: 'vo_ext_protocol_version',
    }),
  );
  refreshOutputBinding();
  assert(verify().issues.some((issue) => issue.includes('missing vo_ext_protocol_version')));

  writeFileSync(
    path.join(outDir, 'vogui.wasm'),
    protocolWasmFixture({
      protocolVersion: 2,
      standaloneExportKeys: browserCatalog.exportKeys,
    }),
  );
  refreshOutputBinding();
  assert(verify().issues.some((issue) => issue.includes('pure body i32.const 3')));

  writeFileSync(
    path.join(outDir, 'vogui.wasm'),
    protocolWasmFixture({
      standaloneExportKeys: [],
      imports: VOGUI_STANDALONE_HOST_IMPORTS_V3,
    }),
  );
  refreshOutputBinding();
  assert(verify().issues.some((issue) => issue.includes('exact standalone export allowlist')));

  writeFileSync(
    path.join(outDir, 'vogui.wasm'),
    protocolWasmFixture({
      standaloneExportKeys: browserCatalog.exportKeys,
      extraFunctionExports: ['legacyShortExport'],
      imports: VOGUI_STANDALONE_HOST_IMPORTS_V3,
    }),
  );
  refreshOutputBinding();
  assert(verify().issues.some((issue) => issue.includes('exact standalone export allowlist')));

  writeFileSync(path.join(outDir, 'vogui.wasm'), validWasm);
  refreshOutputBinding();
  assert.deepEqual(verify().issues, []);

  writeFileSync(path.join(outDir, 'vogui.wasm'), 'tampered wasm\n');
  assert(verify().issues.some((issue) => issue.includes('output digest set')));
  writeFileSync(path.join(outDir, 'vogui.wasm'), outputBytes);

  const commandTamper = structuredClone(manifest);
  commandTamper.command = commandTamper.command.filter((part) => part !== '--locked');
  writeManifest(commandTamper);
  assert(verify().issues.some((issue) => issue.includes('canonical cargo command')));

  const unknownField = structuredClone(manifest);
  unknownField.unbound = true;
  writeManifest(unknownField);
  assert(verify().issues.some((issue) => issue.includes('exact schema-v2 fields')));
  writeManifest(manifest);

  writeFileSync(path.join(voguiRoot, 'gui.vo'), `${voguiVoSource}// source drift\n`);
  const driftedSource = captureVoguiSource(voguiRoot);
  assert(
    verifyCurrentVoguiWasm({
      voguiRoot,
      volangRoot,
      outDir,
      expectedSource: driftedSource,
      expectedVolangBuildInputs: volangBuildInputs,
    }).issues.some((issue) => issue.includes('current vogui checkout')),
  );
  writeFileSync(path.join(voguiRoot, 'gui.vo'), voguiVoSource);
  assert.deepEqual(captureVoguiSource(voguiRoot), source);

  writeFileSync(path.join(voguiRoot, 'rust', 'Cargo.lock'), '# drifted fixture lock\n');
  const lockDriftedSource = captureVoguiSource(voguiRoot);
  assert.notEqual(lockDriftedSource.cargoLock.digest, source.cargoLock.digest);
  assert(
    verifyCurrentVoguiWasm({
      voguiRoot,
      volangRoot,
      outDir,
      expectedSource: lockDriftedSource,
      expectedVolangBuildInputs: volangBuildInputs,
    }).issues.some((issue) => issue.includes('current vogui checkout')),
  );
  writeFileSync(path.join(voguiRoot, 'rust', 'Cargo.lock'), '# fixture lock\n');
  assert.deepEqual(captureVoguiSource(voguiRoot), source);

  writeFileSync(path.join(volangRoot, 'lang', 'crates', 'vo-vm', 'lib.rs'), 'pub fn drift() {}\n');
  assert(
    verify().issues.some((issue) => issue.includes('current scoped Volang inputs')),
  );
  writeFileSync(path.join(volangRoot, 'lang', 'crates', 'vo-vm', 'lib.rs'), 'pub fn vm() {}\n');
  assert.deepEqual(verify().issues, []);

  rmSync(path.join(outDir, 'vogui.wasm'));
  writeFileSync(path.join(outDir, 'real.wasm'), outputBytes);
  symlinkSync(path.join(outDir, 'real.wasm'), path.join(outDir, 'vogui.wasm'));
  assert(verify().issues.some((issue) => issue.includes('missing, oversized, or non-regular')));
  rmSync(path.join(outDir, 'vogui.wasm'));
  rmSync(path.join(outDir, 'real.wasm'));
  writeFileSync(path.join(outDir, 'vogui.wasm'), outputBytes);

  writeFileSync(path.join(outDir, 'producer-manifest.json'), '{ malformed');
  assert(verify().issues.some((issue) => issue.includes('parse failed')));
  writeManifest(manifest);

  const backup = `${outDir}.backup-9000000000000-1-00000000-0000-4000-8000-000000000000`;
  renameSync(outDir, backup);
  withDirectoryReplacementLock(outDir, () => {
    assert.equal(recoverInterruptedVoguiDirectoryReplacement(outDir), backup);
  });
  assert.deepEqual(verify().issues, []);

  console.log('vogui current-source WASM selftest: ok');
} finally {
  rmSync(tempRoot, { recursive: true, force: true });
}
