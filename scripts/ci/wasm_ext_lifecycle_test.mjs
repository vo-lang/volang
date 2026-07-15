#!/usr/bin/env node

import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import test from 'node:test';
import { fileURLToPath } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const hosts = [
  {
    label: 'Studio',
    path: path.join(root, 'apps/studio/src/lib/studio_wasm.ts'),
    studio: true,
  },
  {
    label: 'legacy Playground',
    path: path.join(root, 'apps/playground-legacy/src/wasm/vo.ts'),
    studio: false,
  },
];

function stripTypeScript(source) {
  return source
    .replaceAll(': string', '')
    .replaceAll(': void', '')
    .replaceAll(': boolean', '')
    .replaceAll(': BindgenModule', '')
    .replaceAll(': StandaloneRef', '')
    .replaceAll(': PreparedExtensionArtifact', '')
    .replace('(prepared): prepared is PreparedExtensionArtifact', '(prepared)');
}

function extractFunction(source, name, nextName) {
  const startMarker = `function ${name}(`;
  const start = source.indexOf(startMarker);
  assert.notEqual(start, -1, `missing ${name}`);
  const end = source.indexOf(`function ${nextName}(`, start + startMarker.length);
  assert.notEqual(end, -1, `missing function after ${name}`);
  return stripTypeScript(source.slice(start, end).trim());
}

function compileFunction(functionSource, name, bindings) {
  const bindingNames = Object.keys(bindings);
  const factory = new Function(
    ...bindingNames,
    `${functionSource}\nreturn {
      call: ${name},
      resetGeneration: () => typeof extResetGeneration === 'undefined' ? undefined : extResetGeneration,
      leaseGeneration: () => typeof nextExtLoadLease === 'undefined' ? undefined : nextExtLoadLease,
    };`,
  );
  return factory(...Object.values(bindings));
}

function compileCleanupHelpers(source, studio, consoleMock) {
  const firstName = studio ? 'disposeBindgenModule' : 'disposePreparedExtensionArtifact';
  const start = source.indexOf(`function ${firstName}(`);
  const end = source.indexOf('function removeExtensionLoadLeases(', start);
  assert.notEqual(start, -1, `missing ${firstName}`);
  assert.notEqual(end, -1, 'missing cleanup-helper boundary');
  const helperSource = stripTypeScript(source.slice(start, end));
  const returned = studio
    ? '{ disposeBindgenModule, disposeStandaloneRef, disposePreparedExtensionArtifact }'
    : '{ disposePreparedExtensionArtifact }';
  return new Function('console', `${helperSource}\nreturn ${returned};`)(consoleMock);
}

function nextExtensionGeneration(current, label) {
  if (!Number.isSafeInteger(current) || current < 0 || current >= Number.MAX_SAFE_INTEGER) {
    throw new Error(`${label} lifecycle generation is exhausted`);
  }
  return current + 1;
}

function snapshotMap(map) {
  return Array.from(map.entries());
}

function makeLifecycleHarness(host, rustError) {
  const owner = 'github.com/acme/graphics';
  const artifactToken = '3:7';
  const bindgen = { __voDispose() {} };
  const prepared = {
    mode: 'bindgen',
    artifact: { artifactToken },
    bindgenModule: bindgen,
  };
  const extArtifacts = new Map([[owner, prepared.artifact]]);
  const extBindgenModules = new Map([[owner, bindgen]]);
  const extInstances = new Map();
  const extStandaloneRefs = new Map();
  const extOwnerLoadGenerations = new Map([[owner, 7]]);
  const extLoadOperations = new Map([[
    owner,
    { artifactToken, prepared },
  ]]);
  const extLoadLeases = new Map([['11', { owner, artifactToken }]]);
  const extExhaustedOwnerLoads = new Set([owner]);
  let cleanupCount = 0;

  const removeExtensionLoadLeases = (target) => {
    for (const [leaseToken, lease] of extLoadLeases) {
      if (lease.owner === target) extLoadLeases.delete(leaseToken);
    }
  };
  const disposePreparedExtensionArtifact = () => { cleanupCount += 1; };
  const cancelPendingExtensionLoad = (target) => {
    const operation = extLoadOperations.get(target);
    if (!operation) return false;
    extLoadOperations.delete(target);
    removeExtensionLoadLeases(target);
    if (operation.prepared) disposePreparedExtensionArtifact(operation.prepared);
    return true;
  };
  const throwRustError = () => { throw rustError; };
  const extOwnerStateBridge = host.studio
    ? { forgetWasmExtModuleOwner: throwRustError, clearWasmExtModuleOwners: throwRustError }
    : null;
  const wasmModule = host.studio
    ? null
    : { forgetWasmExtModuleOwner: throwRustError, clearWasmExtModuleOwners: throwRustError };

  return {
    owner,
    state: {
      extArtifacts,
      extBindgenModules,
      extInstances,
      extStandaloneRefs,
      extOwnerLoadGenerations,
      extLoadOperations,
      extLoadLeases,
      extExhaustedOwnerLoads,
    },
    bindings: {
      validateCanonicalModuleOwner() {},
      extArtifacts,
      extBindgenModules,
      extInstances,
      extStandaloneRefs,
      extOwnerStateBridge,
      wasmModule,
      extOwnerLoadGenerations,
      nextExtensionGeneration,
      cancelPendingExtensionLoad,
      removeExtensionLoadLeases,
      disposeStandaloneRef() { cleanupCount += 1; },
      disposeBindgenModule() { cleanupCount += 1; },
      extResetGeneration: 3,
      extLoadOperations,
      extLoadLeases,
      extExhaustedOwnerLoads,
      disposePreparedExtensionArtifact,
      console: { error() {} },
    },
    cleanupCount: () => cleanupCount,
  };
}

for (const host of hosts) {
  const source = fs.readFileSync(host.path, 'utf8');

  test(`${host.label}: Rust forget failure preserves the complete JavaScript owner transaction`, () => {
    const rustError = new Error('injected Rust forget failure');
    const harness = makeLifecycleHarness(host, rustError);
    const before = Object.fromEntries(
      Object.entries(harness.state)
        .filter(([, value]) => value instanceof Map)
        .map(([name, value]) => [name, snapshotMap(value)]),
    );
    const sourceFunction = extractFunction(source, 'unloadExtModule', 'bytesEqual');
    const compiled = compileFunction(sourceFunction, 'unloadExtModule', harness.bindings);

    assert.throws(() => compiled.call(harness.owner), (error) => error === rustError);
    for (const [name, expected] of Object.entries(before)) {
      assert.deepEqual(snapshotMap(harness.state[name]), expected, `${name} changed after Rust failure`);
    }
    assert.equal(harness.cleanupCount(), 0);
  });

  test(`${host.label}: Rust clear failure preserves generations, pending loads, and active dispatch`, () => {
    const rustError = new Error('injected Rust clear failure');
    const harness = makeLifecycleHarness(host, rustError);
    const before = Object.fromEntries(
      Object.entries(harness.state)
        .filter(([, value]) => value instanceof Map)
        .map(([name, value]) => [name, snapshotMap(value)]),
    );
    const sourceFunction = extractFunction(source, 'unloadAllExtModules', 'commitExtModule');
    const compiled = compileFunction(sourceFunction, 'unloadAllExtModules', harness.bindings);

    assert.throws(() => compiled.call(), (error) => error === rustError);
    assert.equal(compiled.resetGeneration(), 3);
    for (const [name, expected] of Object.entries(before)) {
      assert.deepEqual(snapshotMap(harness.state[name]), expected, `${name} changed after Rust failure`);
    }
    assert.equal(harness.cleanupCount(), 0);
  });

  test(`${host.label}: owner-generation exhaustion is rejected before Rust synchronization`, () => {
    const rustError = new Error('Rust bridge must not run');
    const harness = makeLifecycleHarness(host, rustError);
    harness.state.extOwnerLoadGenerations.set(harness.owner, Number.MAX_SAFE_INTEGER);
    const before = Object.fromEntries(
      Object.entries(harness.state)
        .filter(([, value]) => value instanceof Map)
        .map(([name, value]) => [name, snapshotMap(value)]),
    );
    const sourceFunction = extractFunction(source, 'unloadExtModule', 'bytesEqual');
    const compiled = compileFunction(sourceFunction, 'unloadExtModule', harness.bindings);

    assert.throws(
      () => compiled.call(harness.owner),
      /lifecycle generation is exhausted/,
    );
    for (const [name, expected] of Object.entries(before)) {
      assert.deepEqual(snapshotMap(harness.state[name]), expected, `${name} changed at exhaustion`);
    }
    assert.equal(harness.cleanupCount(), 0);
  });

  test(`${host.label}: single-owner cleanup runs after every JavaScript route is detached`, () => {
    const harness = makeLifecycleHarness(host, new Error('unused Rust error'));
    harness.state.extExhaustedOwnerLoads.clear();
    const assertDetached = () => {
      for (const name of [
        'extArtifacts',
        'extBindgenModules',
        'extInstances',
        'extStandaloneRefs',
        'extLoadOperations',
        'extLoadLeases',
      ]) {
        assert.equal(harness.state[name].size, 0, `${name} remained visible to cleanup`);
      }
    };
    let cleanupCount = 0;
    const cleanup = () => {
      assertDetached();
      cleanupCount += 1;
    };
    const activeBindgen = { __voDispose: cleanup };
    harness.state.extBindgenModules.set(harness.owner, activeBindgen);
    harness.bindings.disposePreparedExtensionArtifact = cleanup;
    if (host.studio) {
      harness.bindings.extOwnerStateBridge = {
        forgetWasmExtModuleOwner() {},
        clearWasmExtModuleOwners() {},
      };
      harness.bindings.disposeBindgenModule = cleanup;
      harness.bindings.disposeStandaloneRef = cleanup;
    } else {
      harness.bindings.wasmModule = {
        forgetWasmExtModuleOwner() {},
        clearWasmExtModuleOwners() {},
      };
    }
    const sourceFunction = extractFunction(source, 'unloadExtModule', 'bytesEqual');
    const compiled = compileFunction(sourceFunction, 'unloadExtModule', harness.bindings);

    assert.doesNotThrow(() => compiled.call(harness.owner));
    assertDetached();
    assert.equal(harness.state.extOwnerLoadGenerations.get(harness.owner), 8);
    assert.equal(cleanupCount, 2, 'prepared and active artifacts must both be cleaned');
  });

  test(`${host.label}: lease publication failure does not consume the lease generation`, () => {
    const injected = new Error('injected lease-map publication failure');
    class ThrowingLeaseMap extends Map {
      set(key, value) {
        super.set(key, value);
        throw injected;
      }
    }
    const extLoadLeases = new ThrowingLeaseMap();
    const sourceFunction = extractFunction(
      source,
      'allocateExtensionLoadLease',
      'extensionLoadHandle',
    );
    const compiled = compileFunction(sourceFunction, 'allocateExtensionLoadLease', {
      nextExtLoadLease: 19,
      nextExtensionGeneration,
      extLoadLeases,
    });

    assert.throws(
      () => compiled.call('github.com/acme/graphics', '3:7'),
      (error) => error === injected,
    );
    assert.equal(compiled.leaseGeneration(), 19);
    assert.equal(extLoadLeases.size, 0, 'failed publication left an orphaned lease');
  });

  test(`${host.label}: reset-generation exhaustion is rejected without a partial reset`, () => {
    const rustError = new Error('Rust bridge must not run');
    const harness = makeLifecycleHarness(host, rustError);
    harness.bindings.extResetGeneration = Number.MAX_SAFE_INTEGER;
    const before = Object.fromEntries(
      Object.entries(harness.state)
        .filter(([, value]) => value instanceof Map)
        .map(([name, value]) => [name, snapshotMap(value)]),
    );
    const sourceFunction = extractFunction(source, 'unloadAllExtModules', 'commitExtModule');
    const compiled = compileFunction(sourceFunction, 'unloadAllExtModules', harness.bindings);

    assert.throws(() => compiled.call(), /lifecycle generation is exhausted/);
    assert.equal(compiled.resetGeneration(), Number.MAX_SAFE_INTEGER);
    for (const [name, expected] of Object.entries(before)) {
      assert.deepEqual(snapshotMap(harness.state[name]), expected, `${name} changed at exhaustion`);
    }
    assert.equal(harness.cleanupCount(), 0);
  });

  test(`${host.label}: JavaScript commit exception rolls back every active dispatch map`, () => {
    const owner = 'github.com/acme/graphics';
    const artifactToken = '3:7';
    const leaseToken = '12';
    const injected = new Error('injected artifact publication failure');
    class ThrowAfterSetMap extends Map {
      set(key, value) {
        super.set(key, value);
        throw injected;
      }
    }
    const prepared = {
      mode: 'bindgen',
      artifact: { artifactToken },
      bindgenModule: { __voDispose() {} },
    };
    const operation = {
      artifactToken,
      expectedResetGeneration: 3,
      expectedOwnerGeneration: 7,
      prepared,
    };
    const extArtifacts = new ThrowAfterSetMap();
    const extBindgenModules = new Map();
    const extInstances = new Map();
    const extStandaloneRefs = new Map();
    const extLoadOperations = new Map([[owner, operation]]);
    const extLoadLeases = new Map([[leaseToken, { owner, artifactToken }]]);
    let disposeCount = 0;
    const bindings = {
      validateCanonicalModuleOwner() {},
      extLoadLeases,
      extensionLoadGenerationToken: (reset, generation) => `${reset}:${generation}`,
      extResetGeneration: 3,
      extOwnerLoadGenerations: new Map([[owner, 7]]),
      extArtifacts,
      extBindgenModules,
      extInstances,
      extStandaloneRefs,
      extLoadOperations,
      assertExtensionLoadActive() {},
      disposePreparedExtensionArtifact() { disposeCount += 1; },
    };
    const sourceFunction = extractFunction(
      source,
      'commitExtModule',
      host.studio ? 'throwVoCallExtFailure' : 'wasmU32',
    );
    const compiled = compileFunction(sourceFunction, 'commitExtModule', bindings);

    assert.throws(
      () => compiled.call(owner, artifactToken, leaseToken),
      (error) => error === injected,
    );
    assert.equal(extArtifacts.size, 0);
    assert.equal(extBindgenModules.size, 0);
    assert.equal(extInstances.size, 0);
    assert.equal(extStandaloneRefs.size, 0);
    assert.equal(extLoadOperations.size, 0);
    assert.equal(extLoadLeases.size, 0);
    assert.equal(operation.prepared, null);
    assert.equal(disposeCount, 1);
  });

  test(`${host.label}: cleanup exceptions are best-effort after a successful reset`, () => {
    const harness = makeLifecycleHarness(host, new Error('unused Rust error'));
    let cleanupErrors = 0;
    const consoleMock = { error() { cleanupErrors += 1; } };
    const cleanupHelpers = compileCleanupHelpers(source, host.studio, consoleMock);
    const throwingBindgen = {
      __voDispose() { throw new Error('injected cleanup failure'); },
    };
    const artifactToken = '3:7';
    harness.state.extBindgenModules.set(harness.owner, throwingBindgen);
    harness.state.extLoadOperations.set(harness.owner, {
      artifactToken,
      prepared: {
        mode: 'bindgen',
        artifact: { artifactToken },
        bindgenModule: throwingBindgen,
      },
    });
    if (host.studio) {
      harness.bindings.extOwnerStateBridge = {
        forgetWasmExtModuleOwner() {},
        clearWasmExtModuleOwners() {},
      };
      harness.bindings.disposeBindgenModule = cleanupHelpers.disposeBindgenModule;
      harness.bindings.disposeStandaloneRef = cleanupHelpers.disposeStandaloneRef;
    } else {
      harness.bindings.wasmModule = {
        forgetWasmExtModuleOwner() {},
        clearWasmExtModuleOwners() {},
      };
    }
    harness.bindings.disposePreparedExtensionArtifact =
      cleanupHelpers.disposePreparedExtensionArtifact;
    harness.bindings.console = consoleMock;
    const sourceFunction = extractFunction(source, 'unloadAllExtModules', 'commitExtModule');
    const compiled = compileFunction(sourceFunction, 'unloadAllExtModules', harness.bindings);

    assert.doesNotThrow(() => compiled.call());
    assert.equal(harness.state.extArtifacts.size, 0);
    assert.equal(harness.state.extBindgenModules.size, 0);
    assert.equal(harness.state.extLoadOperations.size, 0);
    assert.ok(cleanupErrors >= 2, 'prepared and active cleanup failures must both be reported');
  });
}
