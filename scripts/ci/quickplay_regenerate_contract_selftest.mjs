#!/usr/bin/env node
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  acceptedCrossPlatformVoplayVariant,
  assertQuickplayVoCliProducerInputs,
  compareQuickplaySnapshots,
  ensureRealDirectory,
  parseStrictJsonBytes,
  snapshotQuickplayDirectory,
  writeFileAtomically,
} from './quickplay_regenerate_contract.mjs';
import {
  currentVoCliBuildInputs,
  currentVoCliToolchain,
  voCliExecutionDigest,
} from './quickplay_cli_producer_contract.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const cliInputs = currentVoCliBuildInputs(root);
const currentCliToolchain = currentVoCliToolchain(root);

const fixture = realpathSync.native(
  mkdtempSync(path.join(os.tmpdir(), 'quickplay-regenerate-contract-')),
);
const generated = path.join(fixture, 'generated');
const checked = path.join(fixture, 'checked');

function cliExecutionIdentity(osName, arch) {
  const host = `${arch}-${osName}-selftest`;
  const toolchain = {
    cargo: currentCliToolchain.cargo,
    host,
    rustc: currentCliToolchain.rustc,
    target: host,
  };
  const binary = {
    path: 'target/blockkart-vpak-build/vo',
    digest: `sha256:${createHash('sha256').update(`${osName}/${arch}/vo`).digest('hex')}`,
    size: osName.length * 1000 + arch.length,
  };
  return {
    toolchain,
    binary,
    digest: voCliExecutionDigest(cliInputs, toolchain, binary),
  };
}

function provenance(osName, arch, overrides = {}) {
  const cliExecution = cliExecutionIdentity(osName, arch);
  return {
    schemaVersion: 3,
    artifact: 'studio.quickplay.blockkart',
    producers: [{
      id: 'voplay-current-source-wasm',
      owner: 'voplay/rust',
      kind: 'wasm-bindgen',
      command: ['wasm-pack', 'build'],
      sourceClosure: {
        schemaVersion: 1,
        kind: 'voplay.localSourceClosure',
        workspace: {
          repository: 'github.com/vo-lang/voplay',
          path: 'vo.work',
          digest: `sha256:${'1'.repeat(64)}`,
          modules: ['github.com/vo-lang/vogui'],
        },
        repositories: [{
          roles: ['cargo-path', 'root', 'workspace-owner'],
          name: 'github.com/vo-lang/voplay',
          commit: '1'.repeat(40),
          dirty: false,
          digest: `sha256:${'2'.repeat(64)}`,
          cargoPackages: [{ name: 'vo-voplay', version: '0.1.0', manifest: 'rust/Cargo.toml' }],
          workspaceModules: [],
        }, {
          roles: ['vo-workspace'],
          name: 'github.com/vo-lang/vogui',
          commit: '4'.repeat(40),
          dirty: false,
          digest: `sha256:${'6'.repeat(64)}`,
          cargoPackages: [],
          workspaceModules: ['github.com/vo-lang/vogui'],
        }],
        digest: `sha256:${'9'.repeat(64)}`,
      },
      volangBuildInputs: {
        schemaVersion: 1,
        kind: 'volang.scopedCargoBuildInputs',
        rootFiles: ['.cargo/config.toml', 'Cargo.lock', 'Cargo.toml', 'rust-toolchain.toml'],
        stdlib: 'lang/stdlib',
        packages: [{ name: 'vo-runtime', version: '0.1.1', path: 'lang/crates/vo-runtime' }],
        digest: `sha256:${'5'.repeat(64)}`,
      },
      ffiSourceFingerprint: `sha256:${'7'.repeat(64)}`,
      toolchain: { rustc: 'rustc test', wasmPack: 'wasm-pack test' },
      buildPlatform: { os: osName, arch },
      outputs: [{ name: 'voplay.wasm', size: 4, digest: `sha256:${'3'.repeat(64)}` }],
      ...overrides,
    }, {
      id: 'blockkart-runtime-vpak',
      owner: 'BlockKart',
      kind: 'vpak',
      output: 'assets/blockkart.vpak',
      command: ['vo', 'run', 'tools/pack_primitive_assets.vo'],
      inputs: [{ path: 'tools/pack_primitive_assets.vo', digest: `sha256:${'a'.repeat(64)}`, size: 100 }],
      outputs: [{ path: 'assets/blockkart.vpak', digest: `sha256:${'b'.repeat(64)}`, size: 2048 }],
      producerManifest: {
        path: 'assets/blockkart.vpak.provenance.json',
        sha256: `sha256:${'c'.repeat(64)}`,
        size: 1024,
        producerDigest: 'd'.repeat(64),
      },
      toolchain: cliExecution.toolchain,
      voBinary: cliExecution.binary,
      voCliBuildInputs: structuredClone(cliInputs),
      voCliExecutionDigest: cliExecution.digest,
    }],
  };
}

function writePackage(directory, platform, overrides = {}) {
  mkdirSync(path.join(directory, 'artifacts'), { recursive: true });
  writeFileSync(path.join(directory, 'project.json'), overrides.project ?? '{"schemaVersion":2}\n');
  writeFileSync(path.join(directory, 'deps.json'), overrides.deps ?? '{"schemaVersion":2}\n');
  writeFileSync(path.join(directory, 'artifacts/voplay.wasm'), overrides.artifact ?? 'wasm');
  writeFileSync(
    path.join(directory, 'provenance.json'),
    overrides.provenance ?? `${JSON.stringify(platform)}\n`,
  );
}

try {
  writePackage(generated, provenance('linux', 'x64'));
  writePackage(checked, provenance('darwin', 'arm64'));
  assert.deepEqual(
    assertQuickplayVoCliProducerInputs(
      generated,
      cliInputs,
      'generated selftest package',
      currentCliToolchain,
    ),
    cliInputs,
  );
  const missingCliInputs = provenance('linux', 'x64');
  delete missingCliInputs.producers[1].voCliBuildInputs;
  writeFileSync(path.join(generated, 'provenance.json'), `${JSON.stringify(missingCliInputs)}\n`);
  assert.throws(
    () => assertQuickplayVoCliProducerInputs(
      generated,
      cliInputs,
      'missing-input selftest package',
      currentCliToolchain,
    ),
    /Vo CLI producer inputs are invalid/,
  );
  const driftedCliInputs = provenance('linux', 'x64');
  driftedCliInputs.producers[1].voCliBuildInputs.inputs[0].digest = `sha256:${'0'.repeat(64)}`;
  writeFileSync(path.join(generated, 'provenance.json'), `${JSON.stringify(driftedCliInputs)}\n`);
  assert.throws(
    () => assertQuickplayVoCliProducerInputs(
      generated,
      cliInputs,
      'drifted-input selftest package',
      currentCliToolchain,
    ),
    /Vo CLI producer inputs are invalid/,
  );
  const driftedCliBinary = provenance('linux', 'x64');
  driftedCliBinary.producers[1].voBinary.digest = `sha256:${'0'.repeat(64)}`;
  writeFileSync(path.join(generated, 'provenance.json'), `${JSON.stringify(driftedCliBinary)}\n`);
  assert.throws(
    () => assertQuickplayVoCliProducerInputs(
      generated,
      cliInputs,
      'drifted-binary selftest package',
      currentCliToolchain,
    ),
    /Vo CLI execution identity is invalid/,
  );
  const missingExecutionDigest = provenance('linux', 'x64');
  delete missingExecutionDigest.producers[1].voCliExecutionDigest;
  writeFileSync(path.join(generated, 'provenance.json'), `${JSON.stringify(missingExecutionDigest)}\n`);
  assert.throws(
    () => assertQuickplayVoCliProducerInputs(
      generated,
      cliInputs,
      'missing-execution-digest selftest package',
      currentCliToolchain,
    ),
    /Vo CLI execution identity is invalid/,
  );
  writeFileSync(path.join(generated, 'provenance.json'), `${JSON.stringify(provenance('linux', 'x64'))}\n`);
  let generatedSnapshot = snapshotQuickplayDirectory(generated);
  const checkedSnapshot = snapshotQuickplayDirectory(checked);
  let diff = compareQuickplaySnapshots(generatedSnapshot, checkedSnapshot);
  assert.deepEqual(diff.missing, []);
  assert.deepEqual(diff.extra, []);
  assert.deepEqual(diff.changed.map((entry) => entry.path), ['provenance.json']);
  assert.deepEqual(
    acceptedCrossPlatformVoplayVariant(diff, generated, checked),
    {
      generatedPlatform: { os: 'linux', arch: 'x64' },
      checkedPlatform: { os: 'darwin', arch: 'arm64' },
      voCliBinaryVariant: true,
      voCliHostVariant: true,
      changedFiles: ['provenance.json'],
    },
  );

  const vpakInputDrift = provenance('linux', 'x64');
  vpakInputDrift.producers[1].inputs[0].digest = `sha256:${'0'.repeat(64)}`;
  writeFileSync(path.join(generated, 'provenance.json'), `${JSON.stringify(vpakInputDrift)}\n`);
  generatedSnapshot = snapshotQuickplayDirectory(generated);
  diff = compareQuickplaySnapshots(generatedSnapshot, checkedSnapshot);
  assert.equal(
    acceptedCrossPlatformVoplayVariant(diff, generated, checked),
    null,
    'cross-platform comparison must reject VPAK source/input drift',
  );

  const cargoVersionDrift = provenance('linux', 'x64');
  const cargoDriftProducer = cargoVersionDrift.producers[1];
  cargoDriftProducer.toolchain.cargo = 'cargo 9.9.9 (selftest)';
  cargoDriftProducer.voCliExecutionDigest = voCliExecutionDigest(
    cargoDriftProducer.voCliBuildInputs,
    cargoDriftProducer.toolchain,
    cargoDriftProducer.voBinary,
  );
  writeFileSync(path.join(generated, 'provenance.json'), `${JSON.stringify(cargoVersionDrift)}\n`);
  generatedSnapshot = snapshotQuickplayDirectory(generated);
  diff = compareQuickplaySnapshots(generatedSnapshot, checkedSnapshot);
  assert.equal(
    acceptedCrossPlatformVoplayVariant(diff, generated, checked),
    null,
    'cross-platform comparison must reject Rust/Cargo version drift',
  );

  const sameHostBinaryDrift = provenance('darwin', 'arm64');
  const checkedCliExecution = cliExecutionIdentity('darwin', 'arm64');
  const sameHostProducer = sameHostBinaryDrift.producers[1];
  sameHostProducer.toolchain = checkedCliExecution.toolchain;
  sameHostProducer.voBinary = {
    ...checkedCliExecution.binary,
    digest: `sha256:${'e'.repeat(64)}`,
  };
  sameHostProducer.voCliExecutionDigest = voCliExecutionDigest(
    sameHostProducer.voCliBuildInputs,
    sameHostProducer.toolchain,
    sameHostProducer.voBinary,
  );
  writeFileSync(path.join(generated, 'provenance.json'), `${JSON.stringify(sameHostBinaryDrift)}\n`);
  generatedSnapshot = snapshotQuickplayDirectory(generated);
  diff = compareQuickplaySnapshots(generatedSnapshot, checkedSnapshot);
  assert.equal(
    acceptedCrossPlatformVoplayVariant(diff, generated, checked),
    null,
    'same-host comparison must reject a different Vo binary identity',
  );

  const sameHostBinaryAndInputDrift = structuredClone(sameHostBinaryDrift);
  sameHostBinaryAndInputDrift.producers[1].inputs[0].digest = `sha256:${'f'.repeat(64)}`;
  writeFileSync(
    path.join(generated, 'provenance.json'),
    `${JSON.stringify(sameHostBinaryAndInputDrift)}\n`,
  );
  generatedSnapshot = snapshotQuickplayDirectory(generated);
  diff = compareQuickplaySnapshots(generatedSnapshot, checkedSnapshot);
  assert.equal(
    acceptedCrossPlatformVoplayVariant(diff, generated, checked),
    null,
    'same-host binary comparison must reject VPAK input drift',
  );

  const crossPlatformCommandDrift = provenance('linux', 'x64');
  crossPlatformCommandDrift.producers[1].command = ['vo', 'run', 'tools/forged_pack.vo'];
  writeFileSync(
    path.join(generated, 'provenance.json'),
    `${JSON.stringify(crossPlatformCommandDrift)}\n`,
  );
  generatedSnapshot = snapshotQuickplayDirectory(generated);
  diff = compareQuickplaySnapshots(generatedSnapshot, checkedSnapshot);
  assert.throws(
    () => acceptedCrossPlatformVoplayVariant(diff, generated, checked),
    /non-canonical BlockKart VPAK guest command/,
    'cross-platform comparison must reject a non-canonical VPAK producer command',
  );

  writeFileSync(path.join(generated, 'provenance.json'), `${JSON.stringify(provenance('linux', 'x64'))}\n`);

  writeFileSync(
    path.join(generated, 'provenance.json'),
    `${JSON.stringify(provenance('linux', 'x64', {
      outputs: [{ name: 'voplay.wasm', size: 4, digest: `sha256:${'4'.repeat(64)}` }],
    }))}\n`,
  );
  generatedSnapshot = snapshotQuickplayDirectory(generated);
  diff = compareQuickplaySnapshots(generatedSnapshot, checkedSnapshot);
  assert.equal(acceptedCrossPlatformVoplayVariant(diff, generated, checked), null);

  writeFileSync(
    path.join(generated, 'provenance.json'),
    `${JSON.stringify(provenance('linux', 'x64', {
      volangBuildInputs: {
        schemaVersion: 1,
        kind: 'volang.scopedCargoBuildInputs',
        rootFiles: ['.cargo/config.toml', 'Cargo.lock', 'Cargo.toml', 'rust-toolchain.toml'],
        stdlib: 'lang/stdlib',
        packages: [{ name: 'vo-runtime', version: '0.1.1', path: 'lang/crates/vo-runtime' }],
        digest: `sha256:${'6'.repeat(64)}`,
      },
    }))}\n`,
  );
  generatedSnapshot = snapshotQuickplayDirectory(generated);
  diff = compareQuickplaySnapshots(generatedSnapshot, checkedSnapshot);
  assert.equal(acceptedCrossPlatformVoplayVariant(diff, generated, checked), null);

  writeFileSync(
    path.join(generated, 'provenance.json'),
    `${JSON.stringify(provenance('linux', 'x64', {
      ffiSourceFingerprint: `sha256:${'8'.repeat(64)}`,
    }))}\n`,
  );
  generatedSnapshot = snapshotQuickplayDirectory(generated);
  diff = compareQuickplaySnapshots(generatedSnapshot, checkedSnapshot);
  assert.equal(acceptedCrossPlatformVoplayVariant(diff, generated, checked), null);

  writeFileSync(path.join(generated, 'provenance.json'), `${JSON.stringify(provenance('linux', 'x64'))}\n`);
  writeFileSync(path.join(generated, 'artifacts/voplay.wasm'), 'evil');
  generatedSnapshot = snapshotQuickplayDirectory(generated);
  diff = compareQuickplaySnapshots(generatedSnapshot, checkedSnapshot);
  assert.equal(acceptedCrossPlatformVoplayVariant(diff, generated, checked), null);

  writeFileSync(path.join(generated, 'artifacts/voplay.wasm'), 'wasm');
  writeFileSync(path.join(generated, 'project.json'), '{"schemaVersion":2,"forged":true}\n');
  generatedSnapshot = snapshotQuickplayDirectory(generated);
  diff = compareQuickplaySnapshots(generatedSnapshot, checkedSnapshot);
  assert.equal(acceptedCrossPlatformVoplayVariant(diff, generated, checked), null);

  writeFileSync(path.join(generated, 'project.json'), '{"schemaVersion":2}\n');
  writeFileSync(path.join(generated, 'deps.json'), '{"schemaVersion":2,"forged":true}\n');
  generatedSnapshot = snapshotQuickplayDirectory(generated);
  diff = compareQuickplaySnapshots(generatedSnapshot, checkedSnapshot);
  assert.equal(acceptedCrossPlatformVoplayVariant(diff, generated, checked), null);

  writeFileSync(path.join(generated, 'deps.json'), '{"schemaVersion":2}\n');
  writeFileSync(
    path.join(generated, 'provenance.json'),
    '{"schemaVersion":2,"schemaVersion":2,"producers":[]}\n',
  );
  generatedSnapshot = snapshotQuickplayDirectory(generated);
  diff = compareQuickplaySnapshots(generatedSnapshot, checkedSnapshot);
  assert.throws(
    () => acceptedCrossPlatformVoplayVariant(diff, generated, checked),
    /duplicate object key "schemaVersion"/,
  );

  assert.throws(
    () => parseStrictJsonBytes(Buffer.from('{"value":1,"value":2}'), 'duplicate JSON'),
    /duplicate object key "value"/,
  );
  assert.throws(
    () => parseStrictJsonBytes(Buffer.from('{"\\ud800":1}'), 'surrogate key JSON'),
    /isolated Unicode surrogate/,
  );
  assert.throws(
    () => parseStrictJsonBytes(Buffer.from('{"nested":["\\udc00"]}'), 'surrogate value JSON'),
    /isolated Unicode surrogate/,
  );
  assert.deepEqual(
    parseStrictJsonBytes(Buffer.from('{"scalar":"\\ud83d\\ude42"}'), 'scalar JSON'),
    { scalar: '🙂' },
  );
  assert.throws(
    () => parseStrictJsonBytes(Buffer.from('{"value":1e400}'), 'out-of-range JSON number'),
    /outside the finite f64 range/,
  );
  assert.throws(
    () => parseStrictJsonBytes(Buffer.alloc(9), 'oversized JSON', { maxJsonBytes: 8 }),
    /8-byte JSON limit/,
  );

  const reportDir = path.join(fixture, 'reports');
  writeFileAtomically(reportDir, 'report.json', '{"status":"first"}\n');
  writeFileAtomically(reportDir, 'report.json', '{"status":"second"}\n');
  assert.equal(readFileSync(path.join(reportDir, 'report.json'), 'utf8'), '{"status":"second"}\n');

  let symlinksSupported = true;
  try {
    symlinkSync(checked, path.join(fixture, 'linked-package'));
  } catch (error) {
    if (['EACCES', 'ENOSYS', 'EPERM'].includes(error?.code)) symlinksSupported = false;
    else throw error;
  }
  if (symlinksSupported) {
    assert.throws(
      () => snapshotQuickplayDirectory(path.join(fixture, 'linked-package')),
      /only real directories/,
    );
    const linkedReportParent = path.join(fixture, 'linked-reports');
    symlinkSync(reportDir, linkedReportParent);
    assert.throws(
      () => ensureRealDirectory(linkedReportParent),
      /only real directories/,
    );
  }

  console.log(
    'quickplay regenerate contract selftest: ok '
    + '(1 allowed producer-identity variant, 10 platform-boundary rejections, '
    + '4 CLI lineage rejections)',
  );
} finally {
  rmSync(fixture, { recursive: true, force: true });
}
