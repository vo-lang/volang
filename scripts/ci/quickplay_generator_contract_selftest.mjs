#!/usr/bin/env node
import assert from 'node:assert/strict';
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  unlinkSync,
  writeFileSync,
} from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { createRequire } from 'node:module';
import { fileURLToPath } from 'node:url';
import {
  QUICKPLAY_GENERATOR_AUXILIARY_SOURCE_INPUTS,
  QUICKPLAY_GENERATOR_INPUTS,
  QUICKPLAY_GENERATOR_SOURCE_INPUTS,
  QUICKPLAY_SOURCE_ROOTS,
} from './quickplay_generator_contract.mjs';
import {
  assertVoCliCargoConfigBoundary,
  currentVoCliBuildInputs,
  isVoCliGeneratedIdentityDirectory,
  voCliBuildCommand,
  voCliBuildEnvironment,
  voCliExecutionDigest,
  voCliGuestEnvironment,
  verifyVoCliBuildInputs,
  VO_CLI_BUILD_ENVIRONMENT,
  VO_CLI_BUILD_COMMAND,
  VO_CLI_GUEST_ENVIRONMENT,
  VO_CLI_PRODUCER_TASK_INPUTS,
} from './quickplay_cli_producer_contract.mjs';
import { compareUtf8 } from './utf8_order.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const requireFromStudio = createRequire(path.join(root, 'apps/studio/package.json'));
const { parse } = requireFromStudio('acorn');
const entry = 'apps/studio/scripts/package_blockkart_quickplay.mjs';
const MAX_SOURCE_BYTES = 16 * 1024 * 1024;
const MAX_AST_NODES = 500_000;
const MAX_AST_DEPTH = 256;

function localModulePath(importer, specifier) {
  if (!specifier.startsWith('.')) return null;
  const absolute = path.resolve(root, path.dirname(importer), specifier);
  const relative = path.relative(root, absolute).split(path.sep).join('/');
  if (relative === '..' || relative.startsWith('../') || path.isAbsolute(relative)) {
    throw new Error(`${importer} imports a module outside the Volang root: ${specifier}`);
  }
  if (!relative.endsWith('.mjs')) {
    throw new Error(`${importer} has a local generator import without an explicit .mjs suffix: ${specifier}`);
  }
  return relative;
}

function staticImports(relativePath) {
  const absolute = path.join(root, ...relativePath.split('/'));
  const source = readFileSync(absolute, 'utf8');
  if (Buffer.byteLength(source, 'utf8') > MAX_SOURCE_BYTES) {
    throw new Error(`${relativePath} exceeds the ${MAX_SOURCE_BYTES}-byte generator source limit`);
  }
  const ast = parse(source, {
    allowHashBang: true,
    ecmaVersion: 'latest',
    sourceType: 'module',
  });
  const imports = [];
  const pending = [{ node: ast, depth: 0 }];
  let nodes = 0;
  while (pending.length > 0) {
    const { node, depth } = pending.pop();
    nodes += 1;
    if (nodes > MAX_AST_NODES) throw new Error(`${relativePath} exceeds the ${MAX_AST_NODES}-node AST limit`);
    if (depth > MAX_AST_DEPTH) throw new Error(`${relativePath} exceeds the ${MAX_AST_DEPTH}-level AST depth limit`);
    if (
      (node.type === 'ImportDeclaration'
        || node.type === 'ExportNamedDeclaration'
        || node.type === 'ExportAllDeclaration')
      && typeof node.source?.value === 'string'
    ) {
      imports.push(node.source.value);
    } else if (node.type === 'ImportExpression' && typeof node.source?.value === 'string') {
      imports.push(node.source.value);
    }
    for (const value of Object.values(node)) {
      if (!value || typeof value !== 'object') continue;
      if (Array.isArray(value)) {
        for (let index = value.length - 1; index >= 0; index -= 1) {
          if (value[index] && typeof value[index].type === 'string') {
            pending.push({ node: value[index], depth: depth + 1 });
          }
        }
      } else if (typeof value.type === 'string') {
        pending.push({ node: value, depth: depth + 1 });
      }
    }
  }
  return imports.map((specifier) => localModulePath(relativePath, specifier)).filter(Boolean);
}

const closure = new Set();
const pending = [entry, ...QUICKPLAY_GENERATOR_AUXILIARY_SOURCE_INPUTS];
while (pending.length > 0) {
  const relativePath = pending.pop();
  if (closure.has(relativePath)) continue;
  closure.add(relativePath);
  pending.push(...staticImports(relativePath));
}

const declaredJavaScript = QUICKPLAY_GENERATOR_SOURCE_INPUTS
  .filter((relativePath) => relativePath.endsWith('.mjs'))
  .sort(compareUtf8);
const importedJavaScript = [...closure].sort(compareUtf8);
assert.deepEqual(
  declaredJavaScript,
  importedJavaScript,
  'Quickplay generator source inputs must exactly cover the local static import closure',
);
assert.deepEqual(
  QUICKPLAY_GENERATOR_INPUTS.slice(0, QUICKPLAY_GENERATOR_SOURCE_INPUTS.length),
  QUICKPLAY_GENERATOR_SOURCE_INPUTS,
  'Quickplay generator inputs must begin with the authenticated local source contract',
);
assert.equal(
  new Set(QUICKPLAY_GENERATOR_INPUTS).size,
  QUICKPLAY_GENERATOR_INPUTS.length,
  'Quickplay generator inputs must be unique',
);
assert.deepEqual(
  QUICKPLAY_SOURCE_ROOTS,
  {
    volang: '.',
    blockKart: 'external:BlockKart',
    vogui: 'first-party:vogui',
    vopack: 'first-party:vopack',
    voplay: 'first-party:voplay',
  },
  'Quickplay generator source roots must declare the complete sibling producer closure',
);
for (const source of ['first-party:vogui', 'first-party:vopack', 'first-party:voplay']) {
  assert(QUICKPLAY_GENERATOR_INPUTS.includes(source), `Quickplay generator inputs are missing ${source}`);
}

const cliInputOffset = QUICKPLAY_GENERATOR_SOURCE_INPUTS.length;
assert.deepEqual(
  QUICKPLAY_GENERATOR_INPUTS.slice(
    cliInputOffset,
    cliInputOffset + VO_CLI_PRODUCER_TASK_INPUTS.length,
  ),
  VO_CLI_PRODUCER_TASK_INPUTS,
  'Quickplay generator inputs must explicitly declare the Vo CLI producer task closure',
);
const cliInputs = currentVoCliBuildInputs(root);
assert.deepEqual(
  verifyVoCliBuildInputs(cliInputs, { expected: cliInputs }),
  [],
  'current Vo CLI producer closure must validate',
);
const voStdlibPackage = cliInputs.packages.find((entry) => entry.name === 'vo-stdlib');
assert(
  voStdlibPackage?.targets.some((target) => (
    target.kind.includes('rlib')
    && target.source === 'lang/crates/vo-stdlib/src/lib.rs'
  )),
  'Vo CLI producer closure must classify the explicit vo-stdlib rlib as a production target',
);
const taskInputCovers = (relativePath) => VO_CLI_PRODUCER_TASK_INPUTS.some((input) => (
  input.endsWith('/**')
    ? relativePath === input.slice(0, -3) || relativePath.startsWith(input.slice(0, -2))
    : relativePath === input
));
for (const input of cliInputs.inputs) {
  assert(taskInputCovers(input.path), `Vo CLI producer input lacks task/artifact coverage: ${input.path}`);
}

const missingInput = structuredClone(cliInputs);
missingInput.inputs.splice(Math.floor(missingInput.inputs.length / 2), 1);
assert(
  verifyVoCliBuildInputs(missingInput, { expected: cliInputs }).length > 0,
  'Vo CLI producer contract must reject an omitted source input',
);
const driftedInput = structuredClone(cliInputs);
driftedInput.inputs[0].digest = `sha256:${'0'.repeat(64)}`;
assert(
  verifyVoCliBuildInputs(driftedInput, { expected: cliInputs }).length > 0,
  'Vo CLI producer contract must reject source digest drift',
);
const missingPackage = structuredClone(cliInputs);
missingPackage.packages.pop();
assert(
  verifyVoCliBuildInputs(missingPackage, { expected: cliInputs }).length > 0,
  'Vo CLI producer contract must reject an omitted locked local package',
);
const missingTarget = structuredClone(cliInputs);
missingTarget.packages[0].targets = [];
assert(
  verifyVoCliBuildInputs(missingTarget, { expected: cliInputs }).length > 0,
  'Vo CLI producer contract must reject an omitted production target',
);
for (const [label, malformed] of [
  ['null contract', null],
  ['packages object', { ...structuredClone(cliInputs), packages: {} }],
  ['packages null entry', { ...structuredClone(cliInputs), packages: [null] }],
  ['packages sparse entry', { ...structuredClone(cliInputs), packages: Array(1) }],
  ['packages oversized array', {
    ...structuredClone(cliInputs),
    packages: Array.from({ length: 1025 }, () => null),
  }],
  ['package path object', (() => {
    const candidate = structuredClone(cliInputs);
    candidate.packages[0].path = {};
    return candidate;
  })()],
  ['targets object', (() => {
    const candidate = structuredClone(cliInputs);
    candidate.packages[0].targets = {};
    return candidate;
  })()],
  ['targets null entry', (() => {
    const candidate = structuredClone(cliInputs);
    candidate.packages[0].targets = [null];
    return candidate;
  })()],
  ['targets sparse entry', (() => {
    const candidate = structuredClone(cliInputs);
    candidate.packages[0].targets = Array(1);
    return candidate;
  })()],
  ['targets oversized array', (() => {
    const candidate = structuredClone(cliInputs);
    candidate.packages[0].targets = Array.from({ length: 129 }, () => null);
    return candidate;
  })()],
  ['target kind sparse array', (() => {
    const candidate = structuredClone(cliInputs);
    candidate.packages[0].targets[0].kind = Array(1);
    return candidate;
  })()],
  ['target source number', (() => {
    const candidate = structuredClone(cliInputs);
    candidate.packages[0].targets[0].source = 7;
    return candidate;
  })()],
  ['inputs object', { ...structuredClone(cliInputs), inputs: {} }],
  ['inputs null entry', { ...structuredClone(cliInputs), inputs: [null] }],
  ['inputs sparse entry', { ...structuredClone(cliInputs), inputs: Array(1) }],
  ['input path number', (() => {
    const candidate = structuredClone(cliInputs);
    candidate.inputs[0].path = 7;
    return candidate;
  })()],
  ['input size string', (() => {
    const candidate = structuredClone(cliInputs);
    candidate.inputs[0].size = '7';
    return candidate;
  })()],
]) {
  assert.doesNotThrow(
    () => verifyVoCliBuildInputs(malformed, { expected: cliInputs }),
    `Vo CLI producer validator must be total for malformed ${label}`,
  );
  assert(
    verifyVoCliBuildInputs(malformed, { expected: cliInputs }).length > 0,
    `Vo CLI producer contract must reject malformed ${label}`,
  );
}

function reverseObjectKeyOrder(value) {
  if (Array.isArray(value)) return value.map(reverseObjectKeyOrder);
  if (value === null || typeof value !== 'object') return value;
  return Object.fromEntries(
    Object.keys(value).reverse().map((key) => [key, reverseObjectKeyOrder(value[key])]),
  );
}

const reorderedCliInputs = reverseObjectKeyOrder(cliInputs);
assert.deepEqual(
  verifyVoCliBuildInputs(reorderedCliInputs, { expected: cliInputs }),
  [],
  'Vo CLI producer digest must recursively canonicalize JSON object key order',
);

for (const invalidPath of [
  'lang\\crates\\vo-web\\src\\lib.rs',
  'lang/crates/vo-web/src/e\u0301.rs',
  'lang/crates/vo-web/src/CON.rs',
  'lang/crates/vo-web/src/trailing-dot.',
]) {
  const candidate = structuredClone(cliInputs);
  candidate.inputs[0].path = invalidPath;
  assert(
    verifyVoCliBuildInputs(candidate, { expected: cliInputs }).length > 0,
    `Vo CLI producer contract must reject non-portable path ${JSON.stringify(invalidPath)}`,
  );
}

for (const [left, right] of [
  ['lang/crates/vo-web/src/Case.rs', 'lang/crates/vo-web/src/case.rs'],
  ['lang/crates/vo-web/src/ϑ.rs', 'lang/crates/vo-web/src/ϴ.rs'],
]) {
  const candidate = structuredClone(cliInputs);
  candidate.inputs[0].path = left;
  candidate.inputs[1].path = right;
  const issues = verifyVoCliBuildInputs(candidate, { expected: cliInputs });
  assert(
    issues.some((issue) => issue.includes('portable path collision')),
    `Vo CLI producer contract must reject portable path collision ${left} / ${right}`,
  );
}

for (const generated of [
  'lang/crates/vo-web/target',
  'lang/crates/vo-web/node_modules',
  'lang/crates/vo-web/pkg',
  'lang/crates/vo-web/pkg-island',
]) {
  assert(isVoCliGeneratedIdentityDirectory(generated), `expected generated source root ${generated}`);
}
for (const sourceDirectory of [
  'cmd/vo/target',
  'cmd/vo/src/target',
  'lang/stdlib/target',
  'lang/stdlib/src/pkg',
  'lang/crates/vo-web/src/target',
  'lang/crates/vo-web/src/node_modules',
  'lang/crates/vo-engine/pkg',
  'lang/crates/vo-engine/target',
]) {
  assert(
    !isVoCliGeneratedIdentityDirectory(sourceDirectory),
    `source walker must retain real source directory ${sourceDirectory}`,
  );
}

const compilerEnvironmentPoison = Object.freeze([
  'AR',
  'BINDGEN_EXTRA_CLANG_ARGS',
  'CARGO_BUILD_TARGET',
  'CARGO_HOME',
  'CC',
  'CFLAGS',
  'CMAKE_GENERATOR',
  'CMAKE_PREFIX_PATH',
  'CPATH',
  'CXXFLAGS',
  'DYLD_INSERT_LIBRARIES',
  'DYLD_LIBRARY_PATH',
  'LDFLAGS',
  'LD_PRELOAD',
  'LIBRARY_PATH',
  'MAKEFLAGS',
  'PKG_CONFIG_ALLOW_CROSS',
  'PKG_CONFIG_PATH',
  'RUSTC_WRAPPER',
  'RUSTFLAGS',
  'RUSTUP_TOOLCHAIN',
  'SDKROOT',
  'SOURCE_DATE_EPOCH',
  'VO_BUILD_CACHE',
]);
const environmentFixture = {
  ComSpec: '/allowed/comspec',
  HOME: process.env.HOME ?? root,
  PATH: process.env.PATH ?? '/usr/bin',
  PATHEXT: '.EXE;.CMD',
  SystemDrive: '/allowed/system-drive',
  SystemRoot: '/allowed/system-root',
  TEMP: process.env.TEMP ?? '/tmp',
  TMP: process.env.TMP ?? '/tmp',
  TMPDIR: process.env.TMPDIR ?? '/tmp',
  USERPROFILE: process.env.USERPROFILE ?? process.env.HOME ?? root,
  WINDIR: '/allowed/windows',
  UNDECLARED_BUILD_SETTING: 'must-not-cross-the-boundary',
  VOWORK: 'poison:explicit-workspace',
  VoWoRk: 'poison:mixed-case-explicit-workspace',
  ...Object.fromEntries(compilerEnvironmentPoison.map((key) => [key, `poison:${key}`])),
};
const cargoEnvironment = voCliBuildEnvironment(environmentFixture, root);
assert.deepEqual(
  Object.keys(cargoEnvironment).sort(compareUtf8),
  [
    ...VO_CLI_BUILD_ENVIRONMENT.inherited,
    ...Object.keys(VO_CLI_BUILD_ENVIRONMENT.fixed),
    ...Object.keys(VO_CLI_BUILD_ENVIRONMENT.paths),
    'CARGO_ENCODED_RUSTFLAGS',
  ].sort(compareUtf8),
  'Vo CLI Cargo environment must contain only explicitly inherited and fixed fields',
);
const canonicalCompilerEnvironment = new Set(['CARGO_HOME', 'SOURCE_DATE_EPOCH']);
for (const key of compilerEnvironmentPoison) {
  if (canonicalCompilerEnvironment.has(key)) {
    assert.notEqual(cargoEnvironment[key], `poison:${key}`, `Vo CLI Cargo environment retained ${key}`);
  } else {
    assert(!Object.hasOwn(cargoEnvironment, key), `Vo CLI Cargo environment inherited ${key}`);
  }
}
assert(!Object.hasOwn(cargoEnvironment, 'UNDECLARED_BUILD_SETTING'));
assert.equal(cargoEnvironment.CARGO_HOME, path.join(environmentFixture.HOME, '.cargo'));
assert.equal(cargoEnvironment.CARGO_TARGET_DIR, path.join(root, 'target'));
assert.equal(cargoEnvironment.CARGO_INCREMENTAL, '0');
assert.equal(cargoEnvironment.CARGO_TERM_COLOR, 'never');
assert.equal(cargoEnvironment.LANG, 'C');
assert.equal(cargoEnvironment.LC_ALL, 'C');
assert.equal(cargoEnvironment.TZ, 'UTC');
assert.equal(cargoEnvironment.VOWORK, 'off');
assert.equal(cargoEnvironment.SOURCE_DATE_EPOCH, '1');
assert.equal(cargoEnvironment.ZERO_AR_DATE, '1');
assert.deepEqual(
  cargoEnvironment.CARGO_ENCODED_RUSTFLAGS.split('\u001f'),
  [
    `--remap-path-prefix=${path.resolve(root)}=/workspace/volang`,
    `--remap-path-prefix=${cargoEnvironment.CARGO_HOME}=/workspace/cargo-home`,
    `--remap-path-prefix=${cargoEnvironment.CARGO_TARGET_DIR}=/workspace/cargo-target`,
  ],
);
assert.deepEqual(
  voCliBuildCommand('aarch64-selftest'),
  VO_CLI_BUILD_COMMAND.map((argument) => argument === '<HOST>' ? 'aarch64-selftest' : argument),
);
assert.throws(
  () => voCliBuildEnvironment({ HOME: environmentFixture.HOME }, root),
  /requires an inherited PATH/,
  'Vo CLI Cargo environment must reject a missing PATH',
);
assert.throws(
  () => voCliBuildEnvironment({ PATH: environmentFixture.PATH }, root),
  /requires HOME or USERPROFILE/,
  'Vo CLI Cargo environment must reject a missing home directory identity',
);

const freshCargoTarget = mkdtempSync(path.join(root, 'target', '.blockkart-vpak-cargo-selftest-'));
const freshCargoHome = mkdtempSync(path.join(root, 'target', '.blockkart-vpak-cargo-home-selftest-'));
try {
  const freshCargoEnvironment = voCliBuildEnvironment(
    environmentFixture,
    root,
    freshCargoTarget,
    freshCargoHome,
  );
  assert.equal(freshCargoEnvironment.CARGO_TARGET_DIR, freshCargoTarget);
  assert.equal(freshCargoEnvironment.CARGO_HOME, freshCargoHome);
  assert(
    freshCargoEnvironment.CARGO_ENCODED_RUSTFLAGS.includes(
      `${freshCargoTarget}=/workspace/cargo-target`,
    ),
  );
} finally {
  rmSync(freshCargoTarget, { force: true, recursive: true });
  rmSync(freshCargoHome, { force: true, recursive: true });
}
assert.throws(
  () => voCliBuildEnvironment(environmentFixture, root, path.join(root, 'target')),
  /task-owned/,
  'Vo CLI producer build environment must reject the shared Cargo target',
);

const configFixture = mkdtempSync(path.join(tmpdir(), 'quickplay-cargo-config-selftest-'));
const configHome = path.join(configFixture, 'home', '.cargo');
try {
  mkdirSync(path.join(configFixture, '.cargo'), { recursive: true });
  mkdirSync(configHome, { recursive: true });
  writeFileSync(path.join(configFixture, '.cargo', 'config.toml'), '[build]\njobs = 1\n');
  assert.doesNotThrow(() => assertVoCliCargoConfigBoundary(configFixture, {
    CARGO_HOME: configHome,
  }));
  writeFileSync(path.join(configFixture, '.cargo', 'config'), '[build]\njobs = 2\n');
  assert.throws(
    () => assertVoCliCargoConfigBoundary(configFixture, { CARGO_HOME: configHome }),
    /undeclared Cargo config/,
    'Vo CLI Cargo config boundary must reject the root legacy config name',
  );
  unlinkSync(path.join(configFixture, '.cargo', 'config'));
  writeFileSync(path.join(configHome, 'config.toml'), '[build]\ntarget = "forged"\n');
  assert.throws(
    () => assertVoCliCargoConfigBoundary(configFixture, { CARGO_HOME: configHome }),
    /undeclared Cargo config/,
    'Vo CLI Cargo config boundary must reject home-directory config injection',
  );
} finally {
  rmSync(configFixture, { force: true, recursive: true });
}

const voBinary = path.join(
  root,
  'target',
  'blockkart-vpak-build',
  process.platform === 'win32' ? 'vo.exe' : 'vo',
);
const guestEnvironment = voCliGuestEnvironment(environmentFixture, root, voBinary);
assert.deepEqual(
  Object.keys(guestEnvironment).sort(compareUtf8),
  [
    ...VO_CLI_GUEST_ENVIRONMENT.inherited,
    ...Object.keys(VO_CLI_GUEST_ENVIRONMENT.fixed),
    ...Object.keys(VO_CLI_GUEST_ENVIRONMENT.paths),
  ].sort(compareUtf8),
  'Vo CLI guest environment must contain only its declared fields',
);
for (const key of compilerEnvironmentPoison) {
  assert(!Object.hasOwn(guestEnvironment, key), `Vo CLI guest environment inherited ${key}`);
}
assert(!Object.hasOwn(guestEnvironment, 'PATH'));
assert(!Object.hasOwn(guestEnvironment, 'UNDECLARED_BUILD_SETTING'));
assert(
  Object.keys(guestEnvironment).every((key) => key.toUpperCase() !== 'VOWORK'),
  'Vo CLI guest environment must leave VOWORK genuinely unset for nearest-ancestor discovery',
);
assert.deepEqual(VO_CLI_GUEST_ENVIRONMENT.command, ['vo', 'run', 'tools/pack_primitive_assets.vo']);
assert.equal(VO_CLI_GUEST_ENVIRONMENT.cwd, 'external:BlockKart');
assert.deepEqual(VO_CLI_GUEST_ENVIRONMENT.unset, ['VOWORK']);
assert.deepEqual(VO_CLI_GUEST_ENVIRONMENT.workspaceDiscovery, {
  environment: 'unset',
  mode: 'nearest-ancestor',
  selected: 'external:BlockKart/vo.work',
  start: 'project-root',
});
assert.equal(guestEnvironment.VO_BIN, voBinary);
assert(path.isAbsolute(guestEnvironment.VO_BIN));
assert.throws(
  () => voCliGuestEnvironment(environmentFixture, root, 'target/blockkart-vpak-build/vo'),
  /absolute VO_BIN path/,
  'Vo CLI guest environment must reject a relative VO_BIN path',
);
assert.throws(
  () => voCliGuestEnvironment(
    environmentFixture,
    root,
    path.join(path.dirname(root), process.platform === 'win32' ? 'vo.exe' : 'vo'),
  ),
  /requires VO_BIN=/,
  'Vo CLI guest environment must reject an absolute VO_BIN outside its fixed target path',
);

const executionToolchain = {
  cargo: 'cargo 1.94.0 (canonical-order-selftest)',
  host: 'aarch64-canonical-selftest',
  rustc: 'rustc 1.94.0 (canonical-order-selftest)',
  target: 'aarch64-canonical-selftest',
};
const executionBinary = {
  digest: `sha256:${'1'.repeat(64)}`,
  path: 'target/blockkart-vpak-build/vo',
  size: 4096,
};
assert.equal(
  voCliExecutionDigest(cliInputs, executionToolchain, executionBinary),
  voCliExecutionDigest(
    cliInputs,
    {
      target: executionToolchain.target,
      rustc: executionToolchain.rustc,
      host: executionToolchain.host,
      cargo: executionToolchain.cargo,
    },
    {
      size: executionBinary.size,
      path: executionBinary.path,
      digest: executionBinary.digest,
    },
  ),
  'Vo CLI execution digest must ignore toolchain and binary JSON insertion order',
);

console.log(
  `quickplay generator contract selftest: ok (${importedJavaScript.length} local modules, `
  + `${cliInputs.packages.length} CLI packages, ${cliInputs.inputs.length} CLI inputs, `
  + `${compilerEnvironmentPoison.length} environment pollution cases, `
  + '7 environment/config-boundary rejections, 17 malformed-shape rejections, '
  + '4 portable-path rejections, 2 portable-collision rejections, '
  + '12 source-walker policy cases, 2 recursive order canonicalizations)',
);
