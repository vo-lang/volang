import assert from 'node:assert/strict';
import { parse as parseJavaScriptModule } from 'acorn';
import { createHash } from 'node:crypto';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';
import ts from 'typescript';
import { fileURLToPath } from 'node:url';
import { compareUtf8 } from '../../../scripts/ci/utf8_order.mjs';
import { parseBoundedJsonBytes } from '../../../scripts/ci/bounded_json.mjs';
import {
  quickplayPackageFiles,
  readQuickplayPackageBuildId,
  resolveStudioBuildId,
  validateStudioWasmBuildId,
} from './studio_build_id.mjs';
import { planLocalProjectSources } from './local_project_snapshot_plan.mjs';
import {
  PortablePathTrie,
  portableCaseKey,
  portablePathCollisionKey,
} from '../../../scripts/ci/portable_path_key.mjs';
import { VOGUI_STANDALONE_HOST_IMPORTS_V3 } from '../../../scripts/ci/wasm_protocol_v3.mjs';

const studioRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');

function sourceFile(relative) {
  return fs.readFileSync(path.join(studioRoot, relative), 'utf8');
}

function typescriptPropertyName(node) {
  if (ts.isIdentifier(node) || ts.isStringLiteral(node)) return node.text;
  return null;
}

function studioStandaloneHostAbi(source) {
  const parsed = ts.createSourceFile(
    'studio_wasm.ts',
    source,
    ts.ScriptTarget.Latest,
    true,
    ts.ScriptKind.TS,
  );
  let buildFunction = null;
  const findBuildFunction = (node) => {
    if (
      ts.isFunctionDeclaration(node)
      && node.name?.text === 'buildStandaloneImports'
    ) {
      assert.equal(buildFunction, null, 'buildStandaloneImports must be declared exactly once');
      buildFunction = node;
    }
    ts.forEachChild(node, findBuildFunction);
  };
  findBuildFunction(parsed);
  assert.ok(buildFunction?.body, 'buildStandaloneImports must have a function body');

  let importsInitializer = null;
  const findImports = (node) => {
    if (
      ts.isVariableDeclaration(node)
      && ts.isIdentifier(node.name)
      && node.name.text === 'imports'
    ) {
      assert.equal(importsInitializer, null, 'standalone imports must be declared exactly once');
      importsInitializer = node.initializer;
    }
    ts.forEachChild(node, findImports);
  };
  findImports(buildFunction.body);
  assert.ok(
    importsInitializer && ts.isObjectLiteralExpression(importsInitializer),
    'standalone imports must be one static object literal',
  );
  assert.equal(
    importsInitializer.properties.length,
    1,
    'standalone imports must contain exactly the env module',
  );
  const env = importsInitializer.properties.find((property) => (
    ts.isPropertyAssignment(property) && typescriptPropertyName(property.name) === 'env'
  ));
  assert.ok(
    env && ts.isObjectLiteralExpression(env.initializer),
    'standalone imports must contain one static env object',
  );

  return env.initializer.properties.map((property) => {
    assert.ok(ts.isMethodDeclaration(property), 'standalone env entries must be method declarations');
    const name = typescriptPropertyName(property.name);
    assert.match(name ?? '', /^host_[a-z0-9_]+$/u);
    const parameters = property.parameters.map((parameter) => {
      assert.equal(parameter.dotDotDotToken, undefined, `${name} must not use rest parameters`);
      assert.equal(parameter.questionToken, undefined, `${name} parameters must be required`);
      assert.equal(parameter.initializer, undefined, `${name} parameters must not have defaults`);
      assert.equal(parameter.type?.kind, ts.SyntaxKind.NumberKeyword, `${name} parameters must be number`);
      return 'number';
    });
    const result = property.type?.kind === ts.SyntaxKind.NumberKeyword
      ? 'number'
      : property.type?.kind === ts.SyntaxKind.VoidKeyword
        ? 'void'
        : null;
    assert.notEqual(result, null, `${name} must return number or void`);
    return { name, parameters, result };
  }).sort((left, right) => compareUtf8(left.name, right.name));
}

test('Studio implements the exact Vogui standalone host ABI', () => {
  const expected = VOGUI_STANDALONE_HOST_IMPORTS_V3.map((entry) => {
    assert.ok(entry.results.length <= 1, `${entry.name} must have at most one result`);
    return {
      name: entry.name,
      parameters: entry.parameters.map(() => 'number'),
      result: entry.results.length === 0 ? 'void' : 'number',
    };
  }).sort((left, right) => compareUtf8(left.name, right.name));

  assert.deepEqual(studioStandaloneHostAbi(sourceFile('src/lib/studio_wasm.ts')), expected);
});

function compileWasmOutputOverlapGuard(relative) {
  const source = sourceFile(relative);
  const helpers = extract(
    source,
    'function wasmRangesOverlap(',
    'function bestEffortDealloc(',
  );
  return new Function(
    `${transpile(helpers)}\nreturn wasmOutputOverlapsBridgeMetadata;`,
  )();
}

test('standalone bridge accepts only the valid zero-length pointer alias', () => {
  for (const relative of [
    'src/lib/studio_wasm.ts',
    '../playground-legacy/src/wasm/vo.ts',
  ]) {
    const overlaps = compileWasmOutputOverlapGuard(relative);
    assert.equal(overlaps(16, 0, 16, 0, 24), false);
    assert.equal(overlaps(16, 1, 16, 0, 24), true);
    assert.equal(overlaps(16, 0, 16, 1, 24), true);
    assert.equal(overlaps(18, 4, 16, 4, 24), true);
    assert.equal(overlaps(28, 4, 16, 4, 24), false);
    assert.equal(overlaps(24, 0, 16, 0, 24), true);
    assert.match(
      sourceFile(relative),
      /if \(wasmOutputOverlapsBridgeMetadata\(/u,
    );
  }
});

function extract(source, startMarker, endMarker) {
  const start = source.indexOf(startMarker);
  assert.notEqual(start, -1, `missing source marker ${startMarker}`);
  const end = source.indexOf(endMarker, start + startMarker.length);
  assert.notEqual(end, -1, `missing source marker ${endMarker}`);
  return source.slice(start, end);
}

function transpile(source) {
  return ts.transpileModule(source, {
    compilerOptions: {
      module: ts.ModuleKind.None,
      target: ts.ScriptTarget.ES2020,
    },
    reportDiagnostics: true,
  }).outputText;
}

function compileGuiSessionProtocol() {
  const exports = {};
  return new Function(
    'exports',
    `${transpile(sourceFile('src/lib/gui_session.ts'))}\nreturn exports;`,
  )(exports);
}

function compileQuickplayProtocolValidators() {
  const source = sourceFile('src/lib/quickplay_package_validation.ts')
    .replace("import { PortablePathTrie, portableCaseKey } from './portable_path_key';\n", '')
    .replace("import { parseBoundedJsonBytes } from '../../../../scripts/ci/bounded_json.mjs';\n", '');
  const exports = {};
  return new Function(
    'exports',
    'PortablePathTrie',
    'portableCaseKey',
    'parseBoundedJsonBytes',
    `${transpile(source)}\nreturn exports;`,
  )(exports, PortablePathTrie, portableCaseKey, parseBoundedJsonBytes);
}

function compileQuickplayIdentityValidators() {
  const source = extract(
    sourceFile('src/lib/quickplay_package_validation.ts'),
    'function parseVersion(',
    'function validatePortableComponent(',
  );
  return new Function(
    'portableCaseKey',
    `const MAX_U64 = 18_446_744_073_709_551_615n;
const VERSION_PATTERN = /^(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)(?:-([0-9a-z-]+(?:\\.[0-9a-z-]+)*))?$/;
const textEncoder = new TextEncoder();
const fail = (message) => { throw new Error(message); };
const assertString = (value, label) => {
  if (typeof value !== 'string' || value.length === 0) fail(\`${'${label}'} must be a non-empty string\`);
};
${transpile(source)}
return { parseVersion, validateModulePath };`,
  )(portableCaseKey);
}

function compileProjectNameValidator() {
  const source = extract(
    sourceFile('src/lib/services/project_catalog_service.ts'),
    'function assertProjectName(',
    'function assertSafeRelativeRepoPath(',
  );
  return new Function(
    'portableCaseKey',
    `${transpile(source)}\nreturn assertProjectName;`,
  )(portableCaseKey);
}

function compileStaticModuleCacheDirValidator() {
  const source = extract(
    sourceFile('src/lib/backend/web_backend.ts'),
    'function normalizeStaticPackagePath(',
    'function validateStaticArtifact(',
  );
  return new Function(
    'textEncoder',
    'MAX_VFS_PATH_DEPTH',
    'portableCaseKey',
    `${transpile(source)}\nreturn validateStaticModuleCacheDir;`,
  )(new TextEncoder(), 256, portableCaseKey);
}

function compileBrowserUtf8Comparator() {
  const source = sourceFile('src/lib/utf8_order.ts').replace('export function', 'function');
  return new Function(`${transpile(source)}\nreturn compareUtf8;`)();
}

function compileStudioTarParser() {
  const source = sourceFile('src/lib/backend/web_backend.ts');
  const portablePaths = extract(
    source,
    'function normalizeStaticPackagePath(',
    'function canonicalUnsignedInteger(',
  );
  const tarParser = extract(
    source,
    'function parseTarFiles(',
    'function parseGitFileMode(',
  );
  return new Function(
    'textEncoder',
    'PortablePathTrie',
    'portableCaseKey',
    'portablePathCollisionKey',
    'MAX_VFS_PATH_DEPTH',
    'MAX_VFS_NODES',
    'MAX_STATIC_PACKAGE_FILES',
    'MAX_STATIC_PACKAGE_FILE_BYTES',
    'MAX_STATIC_PACKAGE_TOTAL_BYTES',
    `${transpile(portablePaths)}\n${transpile(tarParser)}\nreturn parseTarFiles;`,
  )(
    new TextEncoder(),
    PortablePathTrie,
    portableCaseKey,
    portablePathCollisionKey,
    256,
    100_000,
    20_000,
    256 * 1024 * 1024,
    512 * 1024 * 1024,
  );
}

function compilePortablePathRegistry() {
  const source = extract(
    sourceFile('src/lib/backend/web_backend.ts'),
    'function normalizeStaticPackagePath(',
    'function canonicalUnsignedInteger(',
  );
  return new Function(
    'textEncoder',
    'PortablePathTrie',
    'portableCaseKey',
    'MAX_VFS_PATH_DEPTH',
    'MAX_VFS_NODES',
    `${transpile(source)}\nreturn { newPortablePathRegistry, insertPortablePath };`,
  )(new TextEncoder(), PortablePathTrie, portableCaseKey, 256, 100_000);
}

function tarArchive(entries) {
  const encoder = new TextEncoder();
  const chunks = [];
  let total = 1024;
  for (const { path: entryPath, body = new Uint8Array(), type = 48 } of entries) {
    const pathBytes = encoder.encode(entryPath);
    assert.ok(pathBytes.byteLength <= 100);
    const header = new Uint8Array(512);
    header.set(pathBytes, 0);
    const size = body.byteLength.toString(8).padStart(11, '0');
    header.set(encoder.encode(`${size}\0`), 124);
    header.fill(0x20, 148, 156);
    header[156] = type;
    let checksum = 0;
    for (const byte of header) checksum += byte;
    header.set(encoder.encode(`${checksum.toString(8).padStart(6, '0')}\0 `), 148);
    const paddedBody = new Uint8Array(Math.ceil(body.byteLength / 512) * 512);
    paddedBody.set(body);
    chunks.push(header, paddedBody);
    total += header.byteLength + paddedBody.byteLength;
  }
  const out = new Uint8Array(total);
  let offset = 0;
  for (const chunk of chunks) {
    out.set(chunk, offset);
    offset += chunk.byteLength;
  }
  return out;
}

function compileRendererPathHelpers() {
  const source = sourceFile('src/lib/gui/renderer_bridge.ts');
  const pathHelpers = extract(
    source,
    'function normalizeVfsPath(',
    'function dirnameVfsPath(',
  );
  const selector = extract(
    source,
    'function selectVfsFile(',
    '// Load renderer module + full VFS snapshot',
  );
  const factory = new Function(
    'MAX_RENDERER_VFS_FILES',
    'MAX_RENDERER_VFS_FILE_BYTES',
    'MAX_RENDERER_VFS_TOTAL_BYTES',
    'MAX_RENDERER_VFS_PATH_BYTES',
    'MAX_RENDERER_VFS_NAME_BYTES',
    'MAX_RENDERER_VFS_DEPTH',
    'rendererPathEncoder',
    `${transpile(pathHelpers)}\n${transpile(selector)}\nreturn { validateVfsSnapshot, selectVfsFile };`,
  );
  return factory(4, 8, 12, 128, 64, 8, new TextEncoder());
}

function compileStaticTransaction(bindings) {
  const source = sourceFile('src/lib/backend/web_backend.ts');
  const transaction = extract(
    source,
    'function replacePreparedStaticTreesAtomically(',
    'function restoreMap<T>(',
  );
  const names = Object.keys(bindings);
  const factory = new Function(
    ...names,
    `${transpile(transaction)}\nreturn { replacePreparedStaticTreesAtomically, liveBytes: () => liveVfsBytes, livePathBytes: () => liveVfsPathBytes };`,
  );
  return factory(...Object.values(bindings));
}

function compileStaticPathPreflight(state) {
  const source = extract(
    sourceFile('src/lib/backend/web_backend.ts'),
    'function preflightPreparedStaticTrees(',
    'function setPreparedVfsFile(',
  );
  const encoder = new TextEncoder();
  const pathBytes = (value) => encoder.encode(value).byteLength;
  const aggregatePathBytes = (directories, files) => {
    let total = 0;
    for (const value of directories) total += pathBytes(value);
    for (const value of files) total += pathBytes(value);
    return total;
  };
  const dirname = (value) => {
    const index = value.lastIndexOf('/');
    return index <= 0 ? '/' : value.slice(0, index);
  };
  return new Function(
    'compareUtf8',
    'ROOT',
    'checkedRuntimeVfsPath',
    'normalizePath',
    'dirname',
    'openVfsFiles',
    'vfsPathContains',
    'directories',
    'vfsFiles',
    'orphanVfsNodes',
    'orphanVfsBytes',
    'orphanVfsPathBytes',
    'MAX_STATIC_PACKAGE_FILE_BYTES',
    'MAX_STATIC_PACKAGE_TOTAL_BYTES',
    'MAX_STATIC_PACKAGE_FILES',
    'MAX_VFS_NODES',
    'MAX_VFS_TOTAL_BYTES',
    'MAX_VFS_PATH_KEY_BYTES',
    'vfsPathKeyBytes',
    'aggregateVfsPathKeyBytes',
    `${transpile(source)}\nreturn preflightPreparedStaticTrees;`,
  )(
    compareUtf8,
    '/',
    (value) => [value, null],
    (value) => value,
    dirname,
    new Map(),
    (root, candidate) => candidate === root || candidate.startsWith(`${root}/`),
    state.directories,
    state.vfsFiles,
    0,
    0,
    0,
    256 * 1024 * 1024,
    512 * 1024 * 1024,
    20_000,
    100_000,
    512 * 1024 * 1024,
    16 * 1024 * 1024,
    pathBytes,
    aggregatePathBytes,
  );
}

function compileOpenVfsPathBudget(maxPathBytes) {
  const source = extract(
    sourceFile('src/lib/backend/web_backend.ts'),
    'function tryTrackOpenVfsFile(',
    'function vfsPathKeyBytes(',
  );
  return new Function(
    'MAX_PATH_BYTES',
    `const openVfsFiles = new Map();
let openVfsPathBytes = 0;
const vfsPathKeyBytes = (value) => new TextEncoder().encode(value).byteLength;
const canAllocateVfs = (_nodes, _bytes, pathBytes = 0) => openVfsPathBytes + pathBytes <= MAX_PATH_BYTES;
${transpile(source)}
return {
  tryTrackOpenVfsFile,
  deleteTrackedOpenVfsFile,
  projectedOpenVfsPathBytesAfterRename,
  entries: () => [...openVfsFiles.entries()],
  pathBytes: () => openVfsPathBytes,
};`,
  )(maxPathBytes);
}

function compileSetVfsFileBudgetFailure(onEnsureDir) {
  const source = extract(
    sourceFile('src/lib/backend/web_backend.ts'),
    'function setVfsFile(',
    'function deleteFile(',
  );
  const vfsFiles = new Map();
  return {
    setVfsFile: new Function(
      'normalizePath',
      'dirname',
      'vfsFiles',
      'missingVfsDirectories',
      'vfsPathsByteLength',
      'vfsPathKeyBytes',
      'canAllocateVfs',
      'ERR_OUT_OF_MEMORY',
      'ensureDir',
      `${transpile(source)}\nreturn setVfsFile;`,
    )(
      (value) => value,
      () => '/missing',
      vfsFiles,
      () => ['/missing'],
      () => 8,
      () => 8,
      () => false,
      'out of memory',
      onEnsureDir,
    ),
    vfsFiles,
  };
}

function compileTextFileCache(maxBytes) {
  const source = extract(
    sourceFile('src/lib/backend/web_backend.ts'),
    'function textCacheEntryBytes(',
    'function tryDecodeUtf8(',
  );
  return new Function(
    'MAX_VFS_TEXT_CACHE_BYTES',
    `const files = new Map();
let textCacheBytes = 0;
const vfsPathKeyBytes = (value) => new TextEncoder().encode(value).byteLength;
${transpile(source)}
return {
  cacheTextFile,
  deleteCachedTextFile,
  clearTextFileCache,
  entries: () => [...files.entries()],
  bytes: () => textCacheBytes,
};`,
  )(maxBytes);
}

function compileReadOnlyStaticTreeHelpers(readOnlyStaticRoots, vfsDirModes) {
  const source = sourceFile('src/lib/backend/web_backend.ts');
  const helpers = extract(
    source,
    'function vfsPathContains(',
    'function vfsParentLookupError(',
  );
  return new Function(
    'readOnlyStaticRoots',
    'vfsDirModes',
    'ROOT',
    `${transpile(helpers)}\nreturn { isWithinReadOnlyStaticTree, overlapsReadOnlyStaticTree, canMutateVfsDirectory };`,
  )(readOnlyStaticRoots, vfsDirModes, '/');
}

function compileGuestVfsPathHelpers(initialFloor, initialRoot = initialFloor) {
  const source = sourceFile('src/lib/backend/web_backend.ts');
  const normalize = extract(
    source,
    'function normalizePath(',
    'function checkedPublicVfsPath(',
  );
  const scopes = extract(
    source,
    'function setRuntimeVfsRoot(',
    'function checkedGuestVfsPath(',
  );
  const helpers = extract(
    source,
    'function checkedGuestVfsPath(',
    'function checkedRuntimeVfsPath(',
  );
  const runtimePath = extract(
    source,
    'function checkedRuntimeVfsPath(',
    'function vfsPathContains(',
  );
  return new Function(
    'initialFloor',
    'initialRoot',
    'textEncoder',
    'ROOT',
    'ERR_INVALID',
    'ERR_NOT_EXIST',
    'ERR_PERMISSION',
    'MAX_VFS_PATH_LENGTH',
    'MAX_VFS_NAME_LENGTH',
    'MAX_VFS_PATH_DEPTH',
    'WORKSPACE_ROOT',
    'HOST_PRIVATE_VFS_ROOT',
    'vfsPathContains',
    `let runtimeVfsFloor = initialFloor;\nlet runtimeVfsRoot = initialRoot;\n${transpile(normalize)}\n${transpile(scopes)}\n${transpile(helpers)}\n${transpile(runtimePath)}\nreturn { checkedGuestVfsPath, guestVfsGetwd, setRuntimeVfsRoot, setRoot: (root) => { runtimeVfsRoot = root; }, setFloor: (floor) => { runtimeVfsFloor = floor; } };`,
  )(
    initialFloor,
    initialRoot,
    new TextEncoder(),
    '/',
    'invalid argument',
    'file does not exist',
    'permission denied',
    4096,
    255,
    256,
    '/workspace',
    '/__volang_studio_host',
    (root, candidate) => root === '/' || candidate === root || candidate.startsWith(`${root}/`),
  );
}

function compileGuiPipelineHelpers() {
  const source = sourceFile('src/lib/gui/gui_pipeline.ts');
  const combine = extract(
    source,
    'function combineHostBridgeModules(',
    'function validateGuiCompileOutput(',
  );
  const validate = extract(
    source,
    'function validateGuiCompileOutput(',
    'export function resetGuiHostBridge(',
  );
  const factory = new Function(
    'MAX_GUI_BYTECODE_BYTES',
    'MAX_GUI_EXTENSION_COUNT',
    'MAX_GUI_EXTENSION_FILE_BYTES',
    'MAX_GUI_EXTENSION_TOTAL_BYTES',
    `${transpile(combine)}\n${transpile(validate)}\nreturn { combineHostBridgeModules, validateGuiCompileOutput };`,
  );
  return factory(16, 4, 8, 12);
}

function compileBlobUrlRevoker(url, logger) {
  const source = sourceFile('src/lib/gui/renderer_bridge.ts');
  const revoker = extract(
    source,
    'function revokeBlobUrls(',
    'function shouldEmitRendererBridgeDebug(',
  );
  return new Function(
    'URL',
    'console',
    'rendererImportMapsByBlobUrl',
    `${transpile(revoker)}\nreturn revokeBlobUrls;`,
  )(url, logger, new Map());
}

function compileRendererModuleSpecifierParser() {
  const source = sourceFile('src/lib/gui/renderer_bridge.ts');
  const parser = extract(
    source,
    'function rendererModuleSpecifiers(',
    'function rewriteRendererModule(',
  );
  return new Function(
    'parseJavaScriptModule',
    'MAX_RENDERER_MODULE_TOKENS_PER_FILE',
    'MAX_RENDERER_MODULE_GRAPH_TOKENS',
    'MAX_RENDERER_MODULE_AST_NODES_PER_FILE',
    'MAX_RENDERER_MODULE_GRAPH_AST_NODES',
    'MAX_RENDERER_MODULE_IMPORTS_PER_FILE',
    'MAX_RENDERER_MODULE_GRAPH_IMPORTS',
    `${transpile(parser)}\nreturn rendererModuleSpecifiers;`,
  )(parseJavaScriptModule, 10_000, 20_000, 10_000, 20_000, 1_000, 2_000);
}

function compileStaticArtifactValidator() {
  const source = sourceFile('src/lib/backend/web_backend.ts');
  const paths = extract(
    source,
    'function normalizeStaticPackagePath(',
    'function canonicalUnsignedInteger(',
  );
  const validator = extract(
    source,
    'function validateStaticArtifact(',
    'async function validateStaticArtifactBytes(',
  );
  return new Function(
    'textEncoder',
    'MAX_VFS_PATH_DEPTH',
    'MAX_STATIC_PACKAGE_FILE_BYTES',
    'portableCaseKey',
    `${transpile(paths)}\n${transpile(validator)}\nreturn validateStaticArtifact;`,
  )(new TextEncoder(), 256, 256 * 1024 * 1024, portableCaseKey);
}

function compileStaticArtifactInstaller() {
  const source = sourceFile('src/lib/backend/web_backend.ts');
  const paths = extract(
    source,
    'function normalizeStaticPackagePath(',
    'function canonicalUnsignedInteger(',
  );
  const validator = extract(
    source,
    'function validateStaticArtifact(',
    'async function validateStaticArtifactBytes(',
  );
  return new Function(
    'textEncoder',
    'MAX_VFS_PATH_DEPTH',
    'MAX_STATIC_PACKAGE_FILE_BYTES',
    'portableCaseKey',
    `${transpile(paths)}\n${transpile(validator)}\nreturn staticArtifactInstallFiles;`,
  )(new TextEncoder(), 256, 256 * 1024 * 1024, portableCaseKey);
}

test('renderer snapshot rejects duplicate normalized paths', () => {
  const { validateVfsSnapshot } = compileRendererPathHelpers();
  assert.throws(
    () => validateVfsSnapshot({
      rootPath: '/root',
      files: [
        { path: '/root/js/./renderer.js', bytes: new Uint8Array([1]) },
        { path: '/root/js/renderer.js', bytes: new Uint8Array([2]) },
      ],
    }),
    /duplicate path/,
  );
});

test('renderer snapshot enforces per-file and aggregate byte limits', () => {
  const { validateVfsSnapshot } = compileRendererPathHelpers();
  assert.throws(
    () => validateVfsSnapshot({
      rootPath: '/root',
      files: [{ path: '/root/large.js', bytes: new Uint8Array(9) }],
    }),
    /256 MiB limit/,
  );
  assert.throws(
    () => validateVfsSnapshot({
      rootPath: '/root',
      files: [
        { path: '/root/a.js', bytes: new Uint8Array(7) },
        { path: '/root/b.js', bytes: new Uint8Array(6) },
      ],
    }),
    /512 MiB aggregate limit/,
  );
});

test('renderer snapshot rejects non-canonical and traversing root paths', () => {
  const { validateVfsSnapshot } = compileRendererPathHelpers();
  for (const rootPath of ['relative/root', '/root/../escape', '/root//nested', '/root/']) {
    assert.throws(
      () => validateVfsSnapshot({ rootPath, files: [] }),
      /root path is invalid/,
    );
  }
  assert.equal(validateVfsSnapshot({ rootPath: '/', files: [] }).rootPath, '/');
});

test('renderer module selection rejects suffix ambiguity and respects boundaries', () => {
  const { selectVfsFile } = compileRendererPathHelpers();
  const files = [
    { path: '/one/js/renderer.js', bytes: new Uint8Array([1]) },
    { path: '/two/js/renderer.js', bytes: new Uint8Array([2]) },
    { path: '/two/js/notrenderer.js', bytes: new Uint8Array([3]) },
  ];
  assert.throws(() => selectVfsFile(files, 'js/renderer.js'), /ambiguous/);
  assert.equal(selectVfsFile(files, 'js/notrenderer.js'), files[2]);
  assert.equal(selectVfsFile(files, 'enderer.js'), null);
});

test('static package validation failure performs no VFS mutation', () => {
  const state = transactionState();
  let mutations = 0;
  const transaction = compileStaticTransaction({
    ...state,
    normalizePath: (value) => value,
    preflightPreparedStaticTrees() { throw new Error('injected validation failure'); },
    clearStaticPackageTree() { mutations += 1; },
    ensureDir() { mutations += 1; },
    setPreparedVfsFile() { mutations += 1; },
    markStaticTreeReadOnly() { mutations += 1; },
    restoreMap,
  });
  assert.throws(
    () => transaction.replacePreparedStaticTreesAtomically([{ root: '/package', files: [] }]),
    /injected validation failure/,
  );
  assert.equal(mutations, 0);
});

test('static package preflight bounds aggregate VFS path keys before mutation', () => {
  const state = transactionState();
  const expected = snapshotTransactionState(state);
  const preflight = compileStaticPathPreflight(state);
  const deepTail = [...Array.from({ length: 244 }, () => 'segment123456'), 'f.vo'].join('/');
  const files = Array.from({ length: 50 }, (_, index) => ({
    relative: `branch-${index.toString(36)}/${deepTail}`,
    bytes: new Uint8Array(),
    mode: 0o644,
  }));
  assert.ok(JSON.stringify(files.map((file) => file.relative)).length < 256 * 1024);
  assert.throws(
    () => preflight([{ root: '/package', files }]),
    /16 MiB planned VFS path-key limit/,
  );
  assert.deepEqual(snapshotTransactionState(state), expected);
});

test('open VFS descriptor paths share the aggregate budget and project rename growth', () => {
  const repeated = compileOpenVfsPathBudget(64);
  const longPath = `/${'x'.repeat(30)}`;
  assert.equal(repeated.tryTrackOpenVfsFile(1, { path: longPath, flags: 0, position: 0 }), true);
  assert.equal(repeated.tryTrackOpenVfsFile(2, { path: longPath, flags: 0, position: 0 }), true);
  const beforeRejectedOpen = repeated.entries();
  assert.equal(repeated.tryTrackOpenVfsFile(3, { path: longPath, flags: 0, position: 0 }), false);
  assert.deepEqual(repeated.entries(), beforeRejectedOpen);
  assert.equal(repeated.pathBytes(), 62);
  repeated.deleteTrackedOpenVfsFile(1);
  assert.equal(repeated.pathBytes(), 31);

  const rename = compileOpenVfsPathBudget(64);
  rename.tryTrackOpenVfsFile(1, { path: '/a/file', flags: 0, position: 0 });
  rename.tryTrackOpenVfsFile(2, { path: '/a/file', flags: 0, position: 0 });
  const beforeRejectedRename = rename.entries();
  const beforePathBytes = rename.pathBytes();
  assert.ok(rename.projectedOpenVfsPathBytesAfterRename('/a', `/${'destination'.repeat(4)}`) > 64);
  assert.deepEqual(rename.entries(), beforeRejectedRename);
  assert.equal(rename.pathBytes(), beforePathBytes);

  const source = sourceFile('src/lib/backend/web_backend.ts');
  const renameSource = extract(source, 'function vfsRename(', 'function vfsStat(');
  assert.ok(
    renameSource.indexOf('projectedOpenVfsPathBytesAfterRename')
      < renameSource.indexOf('if (targetIsFile) deleteFile'),
  );
});

test('VFS file allocation failure leaves missing parents and bytes untouched', () => {
  let ensureCalls = 0;
  const { setVfsFile, vfsFiles } = compileSetVfsFileBudgetFailure(() => { ensureCalls += 1; });
  assert.throws(
    () => setVfsFile('/missing/file.vo', new Uint8Array([1])),
    /out of memory/,
  );
  assert.equal(ensureCalls, 0);
  assert.deepEqual([...vfsFiles], []);
});

test('decoded VFS text is a bounded lazy cache and eviction preserves authoritative bytes', () => {
  const cache = compileTextFileCache(180);
  const authoritative = new Map([
    ['/one.vo', new Uint8Array([1, 2, 3])],
    ['/two.vo', new Uint8Array([4, 5, 6])],
  ]);
  const before = [...authoritative].map(([path, bytes]) => [path, [...bytes]]);

  assert.equal(cache.cacheTextFile('/one.vo', 'a'.repeat(20)), true);
  assert.equal(cache.cacheTextFile('/two.vo', 'b'.repeat(20)), true);
  assert.deepEqual(cache.entries().map(([path]) => path), ['/two.vo']);
  assert.ok(cache.bytes() <= 180);
  assert.equal(cache.cacheTextFile('/huge.vo', 'x'.repeat(200)), false);
  assert.ok(cache.bytes() <= 180);
  assert.deepEqual(
    [...authoritative].map(([path, bytes]) => [path, [...bytes]]),
    before,
  );

  const backend = sourceFile('src/lib/backend/web_backend.ts');
  assert.doesNotMatch(extract(backend, 'function setVfsFile(', 'function deleteFile('), /tryDecodeUtf8/);
  assert.doesNotMatch(
    extract(backend, 'function setPreparedVfsFile(', 'function markStaticTreeReadOnly('),
    /tryDecodeUtf8/,
  );
});

test('static package commit failure restores every VFS map', () => {
  const state = transactionState();
  const expected = snapshotTransactionState(state);
  const transaction = compileStaticTransaction({
    ...state,
    normalizePath: (value) => value,
    preflightPreparedStaticTrees() {},
    clearStaticPackageTree() {
      state.directories.clear();
      state.files.clear();
      state.vfsFiles.clear();
    },
    ensureDir(root) { state.directories.add(root); },
    setPreparedVfsFile(path, bytes) {
      state.vfsFiles.set(path, bytes);
      throw new Error('injected commit failure');
    },
    markStaticTreeReadOnly() {},
    restoreMap,
    vfsPathContains: (root, candidate) => candidate === root || candidate.startsWith(`${root}/`),
  });
  assert.throws(
    () => transaction.replacePreparedStaticTreesAtomically([{
      root: '/package',
      files: [{ relative: 'file.vo', bytes: new Uint8Array([9]), mode: 0o644 }],
    }]),
    /injected commit failure/,
  );
  assert.deepEqual(snapshotTransactionState(state), expected);
  assert.equal(transaction.liveBytes(), 3);
  assert.equal(transaction.livePathBytes(), 10);
});

test('static package authority commit observes the staged tree and rolls VFS back on failure', () => {
  const state = transactionState();
  const expected = snapshotTransactionState(state);
  let authorityCommitCalls = 0;
  const transaction = compileStaticTransaction({
    ...state,
    normalizePath: (value) => value,
    preflightPreparedStaticTrees() {},
    clearStaticPackageTree() {
      state.directories.clear();
      state.files.clear();
      state.vfsFiles.clear();
    },
    ensureDir(root) { state.directories.add(root); },
    setPreparedVfsFile(path, bytes) { state.vfsFiles.set(path, bytes); },
    markStaticTreeReadOnly(root) { state.readOnlyStaticRoots.add(root); },
    restoreMap,
    vfsPathContains: (root, candidate) => candidate === root || candidate.startsWith(`${root}/`),
  });
  assert.throws(
    () => transaction.replacePreparedStaticTreesAtomically(
      [{
        root: '/package',
        files: [{ relative: 'vo.release.json', bytes: new Uint8Array([9]), mode: 0o644 }],
        readOnly: true,
      }],
      () => {
        authorityCommitCalls += 1;
        assert.deepEqual([...state.vfsFiles.keys()], ['/package/vo.release.json']);
        assert.equal(state.readOnlyStaticRoots.has('/package'), true);
        throw new Error('injected capability batch failure');
      },
    ),
    /injected capability batch failure/,
  );
  assert.equal(authorityCommitCalls, 1);
  assert.deepEqual(snapshotTransactionState(state), expected);
  assert.equal(transaction.liveBytes(), 3);
  assert.equal(transaction.livePathBytes(), 10);
});

test('read-only static roots protect descendants and ancestor removal boundaries', () => {
  const roots = new Set(['/cache/module/v1']);
  const helpers = compileReadOnlyStaticTreeHelpers(roots, new Map([
    ['/', 0o755],
    ['/cache', 0o755],
    ['/cache/module/v1', 0o755],
  ]));
  assert.equal(helpers.isWithinReadOnlyStaticTree('/cache/module/v1/file.vo'), true);
  assert.equal(helpers.isWithinReadOnlyStaticTree('/cache/module/v2/file.vo'), false);
  assert.equal(helpers.overlapsReadOnlyStaticTree('/cache'), true);
  assert.equal(helpers.overlapsReadOnlyStaticTree('/other'), false);
  assert.equal(helpers.canMutateVfsDirectory('/cache/module/v1'), false);
  assert.equal(helpers.canMutateVfsDirectory('/cache'), true);

  const source = sourceFile('src/lib/backend/web_backend.ts');
  for (const [start, end, guard] of [
    ['function vfsOpenFile(', 'function vfsRead(', 'isWithinReadOnlyStaticTree(normalized)'],
    ['function vfsRemove(', 'function vfsRemoveAll(', 'overlapsReadOnlyStaticTree(normalized)'],
    ['function vfsRemoveAll(', 'function vfsRename(', 'overlapsReadOnlyStaticTree(normalized)'],
    ['function vfsRename(', 'function vfsStat(', 'overlapsReadOnlyStaticTree(oldNorm)'],
    ['function vfsChmod(', 'function vfsTruncate(', 'isWithinReadOnlyStaticTree(normalized)'],
    ['function vfsTruncate(', 'function vfsReadFile(', 'isWithinReadOnlyStaticTree(normalized)'],
    ['function vfsWriteFile(', 'function resetWorkspaceState(', 'isWithinReadOnlyStaticTree(normalized)'],
  ]) {
    assert.ok(extract(source, start, end).includes(guard), `${start} must enforce ${guard}`);
  }
});

test('guest VFS paths stay inside the project floor', () => {
  const guest = compileGuestVfsPathHelpers('/workspace/project');
  assert.deepEqual(guest.checkedGuestVfsPath('/etc/passwd'), ['/workspace/project/etc/passwd', null]);
  assert.deepEqual(guest.checkedGuestVfsPath('assets/../main.vo'), ['/workspace/project/main.vo', null]);
  assert.deepEqual(guest.checkedGuestVfsPath('../cache/vo.release.json'), [null, 'permission denied']);
  assert.deepEqual(guest.checkedGuestVfsPath('assets/../../cache'), [null, 'permission denied']);
  assert.deepEqual(guest.guestVfsGetwd(), ['/', null]);

  guest.setRoot('/workspace/project/assets');
  assert.deepEqual(guest.checkedGuestVfsPath('../main.vo'), ['/workspace/project/main.vo', null]);
  assert.deepEqual(guest.checkedGuestVfsPath('../../escape'), [null, 'permission denied']);
  assert.deepEqual(guest.guestVfsGetwd(), ['/assets', null]);

  guest.setRoot('/');
  guest.setFloor(null);
  assert.deepEqual(guest.checkedGuestVfsPath('/cache/vo.release.json'), [null, 'permission denied']);
  assert.deepEqual(guest.guestVfsGetwd(), ['', 'permission denied']);

  const source = sourceFile('src/lib/backend/web_backend.ts');
  const scopes = extract(source, 'function setRuntimeVfsRoot(', 'function checkedGuestVfsPath(');
  assert.match(scopes, /normalized === ROOT/);
  assert.match(scopes, /runtimeVfsFloor = null/);
  assert.match(scopes, /finally/);
});

test('Studio host-private compile cache is unreachable from every guest project root', () => {
  const privateRoot = '/__volang_studio_host';
  const guest = compileGuestVfsPathHelpers('/workspace');
  assert.throws(
    () => guest.setRuntimeVfsRoot(privateRoot),
    /canonical non-root VFS path/,
  );
  assert.throws(
    () => guest.setRuntimeVfsRoot(`${privateRoot}/nested-project`),
    /canonical non-root VFS path/,
  );
  assert.deepEqual(
    guest.checkedGuestVfsPath(`${privateRoot}/compile-cache/studio-wasm`),
    [`/workspace${privateRoot}/compile-cache/studio-wasm`, null],
  );

  const wasm = sourceFile('wasm/src/lib.rs');
  assert.match(wasm, /STUDIO_HOST_PRIVATE_VFS_ROOT: &str = "\/__volang_studio_host"/);
  assert.doesNotMatch(wasm, /\/workspace\/\.volang\/cache\/vo/);
  assert.match(wasm, /module_digest=\{module_digest\}/);
  assert.match(wasm, /validate_vfs_compile_cache_module_binding/);
});

test('renderer load failures revoke their complete blob graph', () => {
  const source = sourceFile('src/lib/gui/renderer_bridge.ts');
  const loader = extract(source, 'async function loadVfsModule<T>(', '// ---- Cached module slot helpers');
  assert.match(loader, /catch \(error\)[\s\S]*revokeBlobUrls\(blobGraph\.urls\)[\s\S]*throw error/);
});

test('blob URL cleanup continues after an individual revocation failure', () => {
  const revoked = [];
  const errors = [];
  const revokeBlobUrls = compileBlobUrlRevoker({
    revokeObjectURL(url) {
      revoked.push(url);
      if (url === 'blob:first') throw new Error('injected revocation failure');
    },
  }, {
    error(...args) { errors.push(args); },
  });
  revokeBlobUrls(['blob:first', 'blob:second', 'blob:second']);
  assert.deepEqual(revoked, ['blob:first', 'blob:second']);
  assert.equal(errors.length, 1);
});

test('renderer module cache keys include VFS snapshot identity', () => {
  const source = sourceFile('src/lib/gui/renderer_bridge.ts');
  const cache = extract(source, 'const vfsSnapshotIdentities', 'function clearCachedModules<T>');
  assert.match(cache, /WeakMap<VfsFile\[\], number>/);
  assert.match(cache, /moduleCacheKey\(entryPath: string, modulePath: string, files: VfsFile\[\]\)/);
  assert.match(cache, /vfsSnapshotIdentity\(files\)/);
});

test('renderer module parsing uses syntax nodes and ignores import-like text', () => {
  const parseSpecifiers = compileRendererModuleSpecifierParser();
  const source = [
    `const text = "import './string.js'";`,
    `// import './comment.js';`,
    `/* export * from './block-comment.js'; */`,
    `import value from './real.js';`,
    `void import('./dynamic.js');`,
    `void import(value);`,
  ].join('\n');
  const budget = { astNodes: 0, tokens: 0, imports: 0 };
  assert.deepEqual(
    parseSpecifiers(source, '/renderer.js', budget).map((entry) => entry.specifier),
    ['./real.js', './dynamic.js'],
  );
  assert.equal(budget.imports, 2);
});

test('renderer graph and GUI startup retain bounded rollback contracts', () => {
  const renderer = sourceFile('src/lib/gui/renderer_bridge.ts');
  assert.match(renderer, /MAX_RENDERER_MODULE_GRAPH_AST_NODES/);
  assert.match(renderer, /MAX_RENDERER_MODULE_GRAPH_REWRITTEN_BYTES/);
  assert.match(renderer, /index\.toString\(36\)/);
  assert.match(renderer, /for \(const renderer of initializedRenderers\)/);

  const backend = sourceFile('src/lib/backend/web_backend.ts');
  const runGui = extract(backend, '  async runGui(', '  async sendGuiEvent(');
  assert.match(runGui, /catch \(error\)/);
  assert.match(runGui, /resetGuiHostBridge\(\)/);
  assert.match(runGui, /wasm\.stopGui\(\)/);
});

test('public Web VFS mutations use checked operations and atomic copy planning', () => {
  const source = sourceFile('src/lib/backend/web_backend.ts');
  const publicMutations = extract(source, '  async writeFile(', '  async grep(');
  assert.match(publicMutations, /checkedPublicVfsPath/);
  assert.match(publicMutations, /vfsWriteFile/);
  assert.match(publicMutations, /vfsMkdirAll/);
  assert.match(publicMutations, /vfsRemoveAll/);
  assert.match(publicMutations, /vfsRename/);
  assert.match(publicMutations, /copyVfsEntryAtomically/);
  const copy = extract(source, 'function copyVfsEntryAtomically(', 'function setRuntimeVfsRoot(');
  assert.match(copy, /source and destination trees overlap/);
  assert.match(copy, /const snapshot =/);
  assert.match(copy, /restoreMap\(vfsFiles/);
});

function localProjectSnapshotFixture() {
  const created = fs.mkdtempSync(path.join(os.tmpdir(), 'studio-local-snapshot-v2-'));
  const fixtureRoot = fs.realpathSync.native(created);
  const workspaceRoot = path.join(fixtureRoot, 'workspace');
  const projectRoot = path.join(workspaceRoot, 'apps', 'app');
  const libRoot = path.join(workspaceRoot, 'modules', 'lib');
  const toolRoot = path.join(workspaceRoot, 'modules', 'tool');
  for (const [directory, module] of [
    [projectRoot, 'github.com/acme/app'],
    [libRoot, 'github.com/acme/lib'],
    [toolRoot, 'github.com/acme/tool'],
  ]) {
    fs.mkdirSync(directory, { recursive: true });
    fs.writeFileSync(path.join(directory, 'vo.mod'), `module = ${JSON.stringify(module)}\nvo = "^0.1.0"\n`);
    fs.writeFileSync(path.join(directory, 'main.vo'), 'package main\n');
  }
  const workspaceFile = path.join(workspaceRoot, 'vo.work');
  fs.writeFileSync(
    workspaceFile,
    'version = 1\nmembers = ["apps/app", "modules/lib", "modules/tool"]\n',
  );
  const snapshot = {
    schema_version: 2,
    mode: 'effective',
    authority: 'workspace',
    root: {
      module: 'github.com/acme/app',
      vo: '^0.1.0',
      dependencies: [
        { module: 'github.com/acme/lib', constraint: '^0.1.0' },
        { module: 'github.com/acme/tool', constraint: '^0.1.0' },
      ],
    },
    workspace: { file: workspaceFile },
    modules: [
      {
        module: 'github.com/acme/lib',
        vo: '^0.1.0',
        source: { kind: 'workspace', directory: libRoot },
        dependencies: [],
      },
      {
        module: 'github.com/acme/tool',
        vo: '^0.1.0',
        source: { kind: 'workspace', directory: toolRoot },
        dependencies: [],
      },
    ],
  };
  return {
    fixtureRoot,
    projectRoot,
    libRoot,
    toolRoot,
    workspaceFile,
    snapshot,
  };
}

function canonicalSnapshotOutput(snapshot) {
  return Buffer.from(`${JSON.stringify(snapshot, null, 2)}\n`, 'utf8');
}

function snapshotInvocation(stdout, inspect = () => {}) {
  return (command, args, options) => {
    inspect(command, args, options);
    return {
      status: 0,
      signal: null,
      stdout,
      stderr: Buffer.alloc(0),
    };
  };
}

test('Studio local module planning consumes one effective ProjectSnapshot v2 closure', () => {
  const fixture = localProjectSnapshotFixture();
  try {
    const voBin = fs.realpathSync.native(process.execPath);
    const environment = { ...process.env, VOWORK: fixture.workspaceFile };
    let invocationCount = 0;
    const plan = planLocalProjectSources(fixture.projectRoot, {
      voBin,
      environment,
      invoke: snapshotInvocation(canonicalSnapshotOutput(fixture.snapshot), (command, args, options) => {
        invocationCount += 1;
        assert.equal(command, voBin);
        assert.deepEqual(args, ['mod', 'snapshot', fixture.projectRoot]);
        assert.equal(options.cwd, fixture.projectRoot);
        assert.equal(options.env.VOWORK, fixture.workspaceFile);
        assert.equal(options.encoding, null);
      }),
    });
    assert.equal(invocationCount, 1);
    assert.deepEqual(
      plan.roots,
      [fixture.projectRoot, fixture.libRoot, fixture.toolRoot].sort(compareUtf8),
    );
    assert.deepEqual(plan.files, [fixture.workspaceFile]);

    const vite = sourceFile('vite.config.ts');
    assert.match(vite, /planLocalProjectSources\(projectRoot/);
    assert.doesNotMatch(vite, /parseVoWorkUsePaths|localVoWorkCandidates|\[\[use\]\]/);
  } finally {
    fs.rmSync(fixture.fixtureRoot, { recursive: true, force: true });
  }
});

test('Studio local module planning preserves VOWORK=off for the core snapshot policy', () => {
  const fixture = localProjectSnapshotFixture();
  try {
    const snapshot = {
      schema_version: 2,
      mode: 'effective',
      authority: 'empty',
      root: {
        module: 'github.com/acme/app',
        vo: '^0.1.0',
        dependencies: [],
      },
      modules: [],
    };
    const plan = planLocalProjectSources(fixture.projectRoot, {
      voBin: fs.realpathSync.native(process.execPath),
      environment: { ...process.env, VOWORK: 'off' },
      invoke: snapshotInvocation(canonicalSnapshotOutput(snapshot), (_command, _args, options) => {
        assert.equal(options.env.VOWORK, 'off');
      }),
    });
    assert.deepEqual(plan, { roots: [fixture.projectRoot], files: [] });
  } finally {
    fs.rmSync(fixture.fixtureRoot, { recursive: true, force: true });
  }
});

test('Studio local module planning fails closed on every core workspace rejection', () => {
  const fixture = localProjectSnapshotFixture();
  try {
    fs.writeFileSync(fixture.workspaceFile, 'version = 1\n[[use]]\npath = "../lib"\n');
    const voBin = fs.realpathSync.native(process.execPath);
    for (const detail of [
      "root: unknown key 'use'",
      "root: unknown key 'future'",
      'members[1]: duplicate member path',
      'members[0]: parent components are allowed only at the beginning',
      'members[0] vo.mod must be a regular file',
    ]) {
      assert.throws(
        () => planLocalProjectSources(fixture.projectRoot, {
          voBin,
          invoke: () => ({
            status: 1,
            signal: null,
            stdout: Buffer.alloc(0),
            stderr: Buffer.from(`[VO:MOD:SNAPSHOT] ${detail}\n`, 'utf8'),
          }),
        }),
        new RegExp(detail.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')),
      );
    }
  } finally {
    fs.rmSync(fixture.fixtureRoot, { recursive: true, force: true });
  }
});

test('Studio local module planning rejects malformed snapshots, duplicate roots, and path escape forms', () => {
  const fixture = localProjectSnapshotFixture();
  try {
    const voBin = fs.realpathSync.native(process.execPath);
    const plan = (snapshot, output = canonicalSnapshotOutput(snapshot)) => planLocalProjectSources(
      fixture.projectRoot,
      { voBin, invoke: snapshotInvocation(output) },
    );

    assert.throws(
      () => plan({ ...fixture.snapshot, schema_version: 1 }),
      /snapshot schema must be 2/,
    );
    assert.throws(
      () => plan({ ...fixture.snapshot, future: true }),
      /project snapshot has unexpected fields/,
    );

    const relativeSource = structuredClone(fixture.snapshot);
    relativeSource.modules[0].source.directory = '../modules/lib';
    assert.throws(() => plan(relativeSource), /must be a normalized absolute path/);

    const duplicateSource = structuredClone(fixture.snapshot);
    duplicateSource.modules[1].source.directory = duplicateSource.modules[0].source.directory;
    assert.throws(() => plan(duplicateSource), /repeats a local project source directory/);

    assert.throws(
      () => plan(fixture.snapshot, Buffer.from(JSON.stringify(fixture.snapshot), 'utf8')),
      /canonical ProjectSnapshot v2 JSON encoding/,
    );
    assert.throws(
      () => planLocalProjectSources(fixture.projectRoot),
      /require VO_BIN/,
    );

    const mutableVo = path.join(fixture.fixtureRoot, 'mutable-vo');
    fs.writeFileSync(mutableVo, '#!/bin/sh\nexit 0\n', { mode: 0o755 });
    assert.throws(
      () => planLocalProjectSources(fixture.projectRoot, {
        voBin: mutableVo,
        invoke: snapshotInvocation(canonicalSnapshotOutput(fixture.snapshot), () => {
          fs.appendFileSync(mutableVo, '# changed\n');
        }),
      }),
      /VO_BIN changed while Studio captured/,
    );

    const linkedVo = path.join(fixture.fixtureRoot, 'linked-vo');
    fs.linkSync(mutableVo, linkedVo);
    assert.throws(
      () => planLocalProjectSources(fixture.projectRoot, {
        voBin: mutableVo,
        invoke: snapshotInvocation(canonicalSnapshotOutput(fixture.snapshot)),
      }),
      /singly-linked/,
    );
  } finally {
    fs.rmSync(fixture.fixtureRoot, { recursive: true, force: true });
  }
});

test('Studio local module planning rejects workspace source symlink aliases', (context) => {
  const fixture = localProjectSnapshotFixture();
  try {
    const alias = path.join(path.dirname(fixture.libRoot), 'lib-alias');
    try {
      fs.symlinkSync(fixture.libRoot, alias, process.platform === 'win32' ? 'junction' : 'dir');
    } catch (error) {
      if (error && typeof error === 'object' && ['EPERM', 'EACCES', 'ENOTSUP'].includes(error.code)) {
        context.skip(`symbolic links are unavailable: ${error.code}`);
        return;
      }
      throw error;
    }
    const aliased = structuredClone(fixture.snapshot);
    aliased.modules[0].source.directory = alias;
    assert.throws(
      () => planLocalProjectSources(fixture.projectRoot, {
        voBin: fs.realpathSync.native(process.execPath),
        invoke: snapshotInvocation(canonicalSnapshotOutput(aliased)),
      }),
      /must not be a symbolic link/,
    );
  } finally {
    fs.rmSync(fixture.fixtureRoot, { recursive: true, force: true });
  }
});

test('Studio standalone local planning does not require a module protocol subprocess', () => {
  const created = fs.mkdtempSync(path.join(os.tmpdir(), 'studio-local-standalone-'));
  const fixtureRoot = fs.realpathSync.native(created);
  try {
    fs.writeFileSync(path.join(fixtureRoot, 'main.vo'), 'package main\n');
    assert.deepEqual(planLocalProjectSources(fixtureRoot), { roots: [fixtureRoot], files: [] });
    fs.writeFileSync(path.join(fixtureRoot, 'vo.work'), 'version = 1\nmembers = []\n');
    assert.throws(
      () => planLocalProjectSources(fixtureRoot),
      /standalone project cannot select .*vo\.work without a vo\.mod/,
    );
  } finally {
    fs.rmSync(fixtureRoot, { recursive: true, force: true });
  }
});

test('Quickplay build IDs require schema v2 and canonical artifact URLs', () => {
  const fixtureRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'studio-build-id-v2-'));
  try {
    const packageRoot = path.join(fixtureRoot, 'public/quickplay/blockkart');
    fs.mkdirSync(packageRoot, { recursive: true });
    fs.writeFileSync(path.join(packageRoot, 'project.json'), '{}\n');
    fs.writeFileSync(path.join(packageRoot, 'deps.json'), JSON.stringify({
      schemaVersion: 2,
      modules: [],
    }));
    const quickplayBuildId = readQuickplayPackageBuildId({ studioRoot: fixtureRoot });
    assert.match(quickplayBuildId, /^qp-[0-9a-f]{12}$/);
    assert.equal(
      validateStudioWasmBuildId(
        `local-build-${quickplayBuildId}`,
        {},
        { studioRoot: fixtureRoot },
      ),
      `local-build-${quickplayBuildId}`,
    );
    assert.throws(
      () => validateStudioWasmBuildId(
        'local-build-qp-000000000000',
        {},
        { studioRoot: fixtureRoot },
      ),
      /does not bind the current Quickplay package/,
    );
    assert.equal(
      validateStudioWasmBuildId(
        'release.2026_07-15',
        { VO_STUDIO_BUILD_ID: 'release.2026_07-15' },
        { studioRoot: fixtureRoot },
      ),
      'release.2026_07-15',
    );
    assert.throws(
      () => validateStudioWasmBuildId(
        'different-release',
        { VO_STUDIO_BUILD_ID: 'release.2026_07-15' },
        { studioRoot: fixtureRoot },
      ),
      /does not match VO_STUDIO_BUILD_ID/,
    );
    const githubEnv = {
      GITHUB_SHA: '0123456789abcdef',
      GITHUB_RUN_ID: '42',
      GITHUB_RUN_ATTEMPT: '3',
    };
    const githubBuildId = `0123456789abcdef-42-3-${quickplayBuildId}`;
    assert.equal(
      validateStudioWasmBuildId(githubBuildId, githubEnv, { studioRoot: fixtureRoot }),
      githubBuildId,
    );
    assert.throws(
      () => validateStudioWasmBuildId(
        `0123456789abcdef-42-2-${quickplayBuildId}`,
        githubEnv,
        { studioRoot: fixtureRoot },
      ),
      /does not match current GitHub inputs/,
    );

    fs.writeFileSync(path.join(packageRoot, 'deps.json'), JSON.stringify({
      schemaVersion: 1,
      modules: [],
    }));
    assert.throws(
      () => readQuickplayPackageBuildId({ studioRoot: fixtureRoot }),
      /unsupported schema version/,
    );

    fs.writeFileSync(path.join(packageRoot, 'deps.json'), JSON.stringify({
      schemaVersion: 2,
      modules: [{
        artifacts: [{ url: '/quickplay/blockkart/artifacts/cache@key/file.wasm' }],
      }],
    }));
    fs.mkdirSync(path.join(packageRoot, 'artifacts/cache@key'), { recursive: true });
    fs.writeFileSync(path.join(packageRoot, 'artifacts/cache@key/file.wasm'), 'wasm');
    assert.deepEqual(
      quickplayPackageFiles({ studioRoot: fixtureRoot }),
      [
        'public/quickplay/blockkart/deps.json',
        'public/quickplay/blockkart/project.json',
        'public/quickplay/blockkart/artifacts/cache@key/file.wasm',
      ].sort((left, right) => Buffer.compare(Buffer.from(left), Buffer.from(right))),
    );
    fs.writeFileSync(path.join(packageRoot, 'deps.json'), JSON.stringify({
      schemaVersion: 2,
      modules: [{
        artifacts: [{ url: '/quickplay/blockkart/artifacts/cache%40key/file.wasm' }],
      }],
    }));
    assert.throws(
      () => readQuickplayPackageBuildId({ studioRoot: fixtureRoot }),
      /non-canonical quickplay artifact URL/,
    );
  } finally {
    fs.rmSync(fixtureRoot, { recursive: true, force: true });
  }
});

test('Studio build IDs use the canonical product environment variable and bounded alphabet', () => {
  const fixtureRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'studio-build-id-env-'));
  try {
    assert.equal(
      resolveStudioBuildId({ VO_STUDIO_BUILD_ID: 'release.2026_07-15' }, { studioRoot: fixtureRoot }),
      'release.2026_07-15',
    );
    assert.throws(
      () => resolveStudioBuildId({ VO_STUDIO_BUILD_ID: 'bad/id' }, { studioRoot: fixtureRoot }),
      /VO_STUDIO_BUILD_ID must contain 1 to 256 ASCII/,
    );
    assert.throws(
      () => resolveStudioBuildId({ GITHUB_SHA: 'bad sha' }, { studioRoot: fixtureRoot }),
      /derived Studio build ID must contain 1 to 256 ASCII/,
    );
  } finally {
    fs.rmSync(fixtureRoot, { recursive: true, force: true });
  }
});

test('Quickplay build IDs parse deps JSON without ambiguous Unicode or object keys', () => {
  const fixtureRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'studio-build-id-json-'));
  try {
    const packageRoot = path.join(fixtureRoot, 'public/quickplay/blockkart');
    const depsPath = path.join(packageRoot, 'deps.json');
    fs.mkdirSync(packageRoot, { recursive: true });
    fs.writeFileSync(path.join(packageRoot, 'project.json'), '{}\n');

    fs.writeFileSync(
      depsPath,
      '{"schemaVersion":2,"modules":[],"description":"valid non-BMP 🚀"}\n',
    );
    assert.match(readQuickplayPackageBuildId({ studioRoot: fixtureRoot }), /^qp-[0-9a-f]{12}$/);

    for (const [contents, expected] of [
      [
        '{"schemaVersion":2,"schemaVersion":2,"modules":[]}',
        /duplicate object key "schemaVersion"/,
      ],
      [
        '{"schemaVersion":2,"modules":[{"artifacts":[],"artifacts":[]}]}',
        /duplicate object key "artifacts"/,
      ],
      [
        '{"schemaVersion":2,"modules":[],"description":"\\ud800"}',
        /JSON string must contain only Unicode scalar values/,
      ],
      [
        '{"schemaVersion":2,"modules":[],"description":"\\udfff"}',
        /JSON string must contain only Unicode scalar values/,
      ],
      [
        '{"schemaVersion":2,"modules":[],"\\ud800":"value"}',
        /object key must contain only Unicode scalar values/,
      ],
    ]) {
      fs.writeFileSync(depsPath, contents);
      assert.throws(
        () => readQuickplayPackageBuildId({ studioRoot: fixtureRoot }),
        expected,
      );
    }

    fs.writeFileSync(
      depsPath,
      Buffer.concat([
        Buffer.from([0xef, 0xbb, 0xbf]),
        Buffer.from('{"schemaVersion":2,"modules":[]}'),
      ]),
    );
    assert.throws(
      () => readQuickplayPackageBuildId({ studioRoot: fixtureRoot }),
      /quickplay deps package/,
    );
  } finally {
    fs.rmSync(fixtureRoot, { recursive: true, force: true });
  }
});

test('Studio Quickplay consumers use the shared Unicode full case-fold key', () => {
  assert.notEqual(portablePathCollisionKey('i'), portablePathCollisionKey('ı'));
  assert.equal(portablePathCollisionKey('İ'), portablePathCollisionKey('i\u0307'));

  const fixtureRoot = fs.mkdtempSync(path.join(os.tmpdir(), 'studio-portable-key-'));
  try {
    const packageRoot = path.join(fixtureRoot, 'public/quickplay/blockkart');
    fs.mkdirSync(packageRoot, { recursive: true });
    fs.writeFileSync(path.join(packageRoot, 'project.json'), '{}\n');
    const artifactUrl = (component) => (
      `/quickplay/blockkart/artifacts/${encodeURIComponent(component)}/file.wasm`
    );
    const writeArtifacts = (components) => fs.writeFileSync(
      path.join(packageRoot, 'deps.json'),
      JSON.stringify({
        schemaVersion: 2,
        modules: [{ artifacts: components.map((component) => ({ url: artifactUrl(component) })) }],
      }),
    );

    writeArtifacts(['i', 'ı']);
    assert.equal(quickplayPackageFiles({ studioRoot: fixtureRoot }).length, 4);

    writeArtifacts(['İ', 'i\u0307']);
    assert.throws(
      () => quickplayPackageFiles({ studioRoot: fixtureRoot }),
      /duplicate artifact path/,
    );

    const backend = sourceFile('src/lib/backend/web_backend.ts');
    assert.match(backend, /portablePathCollisionKey\(cacheDir\)/);
    assert.match(backend, /new PortablePathTrie\(MAX_VFS_NODES\)/);
    assert.doesNotMatch(backend, /spellings: new Map/);
    assert.match(backend, /insertPortablePath\(destinations, relative, false, 'static package path'\)/);
  } finally {
    fs.rmSync(fixtureRoot, { recursive: true, force: true });
  }
});

test('Studio canonical ordering follows UTF-8 bytes without host locale state', () => {
  const browserCompareUtf8 = compileBrowserUtf8Comparator();
  const values = ['\u{10000}', '\u{e000}', 'z', 'é'];
  assert.deepEqual(
    [...values].sort(browserCompareUtf8),
    [...values].sort(compareUtf8),
  );
  assert.deepEqual([...values].sort(browserCompareUtf8), ['z', 'é', '\u{e000}', '\u{10000}']);

  for (const relative of [
    'src/lib/types.ts',
    'src/lib/project_catalog/types.ts',
    'src/lib/services/project_catalog_service.ts',
    'src/lib/backend/web_backend.ts',
  ]) {
    assert.match(sourceFile(relative), /compareUtf8/);
  }
});

test('Studio tar import rejects links, corrupt headers, and portable path aliases', () => {
  const parseTarFiles = compileStudioTarParser();
  const body = new TextEncoder().encode('package main\n');
  const valid = tarArchive([{ path: 'bundle/main.vo', body }]);
  assert.deepEqual(parseTarFiles(valid), [{ path: 'bundle/main.vo', bytes: body }]);

  const linked = tarArchive([{ path: 'bundle/linked.vo', type: 50 }]);
  assert.throws(() => parseTarFiles(linked), /unsupported entry type/);

  const corrupt = valid.slice();
  corrupt[0] ^= 1;
  assert.throws(() => parseTarFiles(corrupt), /checksum mismatch/);

  const aliases = tarArchive([
    { path: 'bundle/Straße.vo', body },
    { path: 'bundle/STRASSE.vo', body },
  ]);
  assert.throws(() => parseTarFiles(aliases), /portable path alias/);

  const prefixAlias = tarArchive([
    { path: 'bundle/Dir/one.vo', body },
    { path: 'bundle/dir/two.vo', body },
  ]);
  assert.throws(() => parseTarFiles(prefixAlias), /portable path alias/);
});

test('Studio portable path registry rejects implicit-directory amplification', () => {
  const { newPortablePathRegistry, insertPortablePath } = compilePortablePathRegistry();
  const registry = newPortablePathRegistry();
  const deepTail = [...Array.from({ length: 244 }, () => 'd'), 'f.vo'].join('/');
  for (let index = 0; index < 406; index += 1) {
    insertPortablePath(registry, `branch-${index.toString(36)}/${deepTail}`, false, 'fixture path');
  }
  assert.equal(registry.nodeCount, 99_876);
  assert.throws(
    () => insertPortablePath(registry, `branch-${(406).toString(36)}/${deepTail}`, false, 'fixture path'),
    /100000-node path-closure limit/,
  );
  assert.equal(registry.nodeCount, 99_876);
});

function protocolDigest(character) {
  return `sha256:${character.repeat(64)}`;
}

function protocolTextFile(filePath, content, digestCharacter = '0', mode = 0o644) {
  return {
    path: filePath,
    mode,
    size: new TextEncoder().encode(content).byteLength,
    digest: protocolDigest(digestCharacter),
    content,
  };
}

function sortedProtocolFiles(files) {
  return files.sort((left, right) => compareUtf8(left.path, right.path));
}

function blockKartVpakProtocolFiles() {
  const pack = protocolTextFile('assets/blockkart.vpak', 'vpak', 'a');
  const producerIntent = {
    schemaVersion: 1,
    kind: 'blockkart.vpakProducerManifest',
    owner: 'BlockKart',
    command: ['vo', 'run', 'tools/pack_primitive_assets.vo'],
    pack: {
      path: pack.path,
      sha256: pack.digest.slice('sha256:'.length),
      size: pack.size,
    },
    inputs: [{}],
    workspaceSourceInputCount: 1,
    payloadInputCount: 37,
    archiveEntryCount: 37,
    archiveEntries: Array.from({ length: 37 }, () => ({})),
    internalManifest: {},
    upstream: [],
  };
  const producer = {
    ...producerIntent,
    producerDigest: createHash('sha256')
      .update(JSON.stringify(producerIntent), 'utf8')
      .digest('hex'),
  };
  return [
    pack,
    protocolTextFile(
      'assets/blockkart.vpak.provenance.json',
      `${JSON.stringify(producer, null, 2)}\n`,
      'c',
    ),
  ];
}

function preparedProtocolBytes(project) {
  return new Map(project.files.map((file) => {
    assert.equal(typeof file.content, 'string', `${file.path} must be a text fixture`);
    return [file.path, new TextEncoder().encode(file.content)];
  }));
}

function quickplayWorkspaceProtocolFixture(protocol, module = 'github.com/acme/lib') {
  const cacheKey = module.replaceAll('/', '@');
  const snapshot = {
    schema_version: 2,
    mode: 'effective',
    authority: 'workspace',
    root: {
      module: 'github.com/vo-lang/blockkart',
      vo: '^0.2.0',
      dependencies: [{ module, constraint: '0.1.0' }],
    },
    workspace: { file: 'vo.work' },
    modules: [{
      module,
      vo: '^0.2.0',
      source: {
        kind: 'workspace',
        directory: `../modules/${cacheKey}`,
      },
      dependencies: [],
    }],
  };
  const project = {
    schemaVersion: 2,
    name: 'BlockKart',
    module: 'github.com/vo-lang/blockkart',
    baseCommit: '1'.repeat(40),
    snapshot,
    files: sortedProtocolFiles([
      ...blockKartVpakProtocolFiles(),
      protocolTextFile('main.vo', 'package main\n'),
      protocolTextFile('vo.mod', protocol.renderQuickplayRootVoMod(snapshot)),
      protocolTextFile('vo.work', protocol.renderQuickplayVoWork(snapshot)),
    ]),
  };
  const deps = {
    schemaVersion: 2,
    name: 'BlockKart dependencies',
    snapshotDigest: protocolDigest('8'),
    modules: [{
      source: 'workspace',
      module,
      cacheDir: `workspace/${cacheKey}`,
      files: [protocolTextFile(
        'vo.mod',
        `module = ${JSON.stringify(module)}\nvo = "^0.2.0"\n`,
      )],
      artifacts: [{
        kind: 'extension-wasm',
        target: 'wasm32-unknown-unknown',
        name: 'lib.wasm',
        size: 8,
        digest: protocolDigest('9'),
        path: 'web-artifacts/lib.wasm',
        url: `/quickplay/blockkart/artifacts/workspace/${cacheKey}/extension-wasm/wasm32-unknown-unknown/lib.wasm`,
      }],
    }],
  };
  return { project, deps };
}

function quickplayRegistryProtocolFixture() {
  const module = 'github.com/acme/lib';
  const version = '0.1.0';
  const moduleVoMod = 'module = "github.com/acme/lib"\nvo = "^0.2.0"\n';
  const packageManifest = {
    schema_version: 1,
    files: [{
      path: 'vo.mod',
      mode: 'regular',
      size: new TextEncoder().encode(moduleVoMod).byteLength,
      digest: protocolDigest('5'),
    }],
  };
  const packageText = `${JSON.stringify(packageManifest, null, 2)}\n`;
  const release = {
    schema_version: 2,
    module,
    version,
    commit: '2'.repeat(40),
    vo: '^0.2.0',
    dependencies: [],
    source: { name: 'source.tar.gz', size: 1, digest: protocolDigest('3') },
    package: {
      size: new TextEncoder().encode(packageText).byteLength,
      digest: protocolDigest('6'),
    },
    artifacts: [{
      kind: 'extension-wasm',
      target: 'wasm32-unknown-unknown',
      name: 'lib.wasm',
      size: 8,
      digest: protocolDigest('7'),
    }],
  };
  const releaseText = `${JSON.stringify(release, null, 2)}\n`;
  const snapshot = {
    schema_version: 2,
    mode: 'effective',
    authority: 'lock',
    root: {
      module: 'github.com/vo-lang/blockkart',
      vo: '^0.2.0',
      dependencies: [{ module, constraint: version }],
    },
    modules: [{
      module,
      version,
      vo: '^0.2.0',
      release: protocolDigest('4'),
      source: { kind: 'registry', directory: 'github.com@acme@lib/0.1.0' },
      dependencies: [],
    }],
  };
  return {
    snapshot,
    deps: {
      schemaVersion: 2,
      name: 'BlockKart dependencies',
      snapshotDigest: protocolDigest('8'),
      modules: [{
        source: 'registry',
        module,
        version,
        cacheDir: 'github.com@acme@lib/0.1.0',
        files: sortedProtocolFiles([
          protocolTextFile('.vo-source-digest', `${protocolDigest('3')}\n`),
          protocolTextFile('.vo-version', `${version}\n`),
          { ...protocolTextFile('vo.mod', moduleVoMod), digest: protocolDigest('5') },
          { ...protocolTextFile('vo.package.json', packageText), digest: protocolDigest('6') },
          { ...protocolTextFile('vo.release.json', releaseText), digest: protocolDigest('4') },
        ]),
        artifacts: [{
          ...release.artifacts[0],
          path: 'artifacts/extension-wasm/wasm32-unknown-unknown/lib.wasm',
          url: '/quickplay/blockkart/artifacts/github.com@acme@lib/0.1.0/extension-wasm/wasm32-unknown-unknown/lib.wasm',
        }],
      }],
    },
  };
}

function quickplayLockProjectProtocolFixture(protocol) {
  const { snapshot } = quickplayRegistryProtocolFixture();
  return {
    schemaVersion: 2,
    name: 'BlockKart',
    module: 'github.com/vo-lang/blockkart',
    baseCommit: '1'.repeat(40),
    snapshot,
    files: sortedProtocolFiles([
      ...blockKartVpakProtocolFiles(),
      protocolTextFile('main.vo', 'package main\n'),
      protocolTextFile('vo.lock', protocol.renderQuickplayVoLock(snapshot)),
      protocolTextFile('vo.mod', protocol.renderQuickplayRootVoMod(snapshot)),
    ]),
  };
}

function replaceProtocolTextFile(files, filePath, content) {
  const index = files.findIndex((file) => file.path === filePath);
  assert.notEqual(index, -1, `missing protocol fixture ${filePath}`);
  files[index] = { ...files[index], content, size: new TextEncoder().encode(content).byteLength };
}

test('Quickplay browser producer parser enforces the shared bounded JSON envelope', () => {
  const protocol = compileQuickplayProtocolValidators();
  const original = quickplayWorkspaceProtocolFixture(protocol).project;
  const producerPath = 'assets/blockkart.vpak.provenance.json';
  const rejectProducer = (update, pattern) => {
    const project = structuredClone(original);
    const producerFile = project.files.find((file) => file.path === producerPath);
    const nextContent = update(producerFile.content);
    replaceProtocolTextFile(project.files, producerPath, nextContent);
    assert.throws(
      () => protocol.validateBlockKartProjectProtocol(project, preparedProtocolBytes(project)),
      pattern,
    );
  };

  rejectProducer(
    (content) => content.replace(
      '  "owner": "BlockKart",\n',
      '  "owner": "BlockKart",\n  "owner": "BlockKart",\n',
    ),
    /duplicate object key "owner"/,
  );
  rejectProducer((content) => {
    const producer = JSON.parse(content);
    producer.inputs[0] = '\ud800';
    return `${JSON.stringify(producer, null, 2)}\n`;
  }, /Unicode scalar values/);
  rejectProducer((content) => {
    const producer = JSON.parse(content);
    let nested = null;
    for (let depth = 0; depth < 130; depth += 1) nested = [nested];
    producer.inputs[0] = nested;
    return `${JSON.stringify(producer, null, 2)}\n`;
  }, /127-level JSON depth limit/);
  rejectProducer((content) => {
    const producer = JSON.parse(content);
    producer.pack = {
      size: producer.pack.size,
      sha256: producer.pack.sha256,
      path: producer.pack.path,
    };
    return `${JSON.stringify(producer, null, 2)}\n`;
  }, /pack identity|authenticated pack descriptor/);
});

test('Quickplay protocol validator accepts one exact ProjectSnapshot v2 workspace closure', async () => {
  const protocol = compileQuickplayProtocolValidators();
  const { project, deps } = quickplayWorkspaceProtocolFixture(protocol);
  assert.equal(
    protocol.renderQuickplayVoLock(project.snapshot),
    null,
  );
  assert.equal(
    protocol.renderQuickplayVoWork(project.snapshot),
    'version = 1\nmembers = ["../modules/github.com@acme@lib"]\n',
  );
  const workOrdering = structuredClone(project.snapshot);
  workOrdering.modules = [
    {
      ...structuredClone(project.snapshot.modules[0]),
      module: 'github.com/a/a/b',
      source: { kind: 'workspace', directory: '../modules/github.com@a@a@b' },
    },
    {
      ...structuredClone(project.snapshot.modules[0]),
      module: 'github.com/a/a0',
      source: { kind: 'workspace', directory: '../modules/github.com@a@a0' },
    },
  ];
  assert.equal(
    protocol.renderQuickplayVoWork(workOrdering),
    'version = 1\nmembers = ["../modules/github.com@a@a0", "../modules/github.com@a@a@b"]\n',
  );
  const validatedProject = protocol.validateBlockKartProjectProtocol(project, preparedProtocolBytes(project));
  assert.equal(validatedProject.snapshot.authority, 'workspace');
  const producerBinding = protocol.validateBlockKartVpakProducerBinding(
    validatedProject.files,
    preparedProtocolBytes(project),
  );
  await protocol.validateBlockKartVpakProducerDigest(producerBinding);
  const missingProducer = structuredClone(project);
  missingProducer.files = missingProducer.files.filter(
    (file) => file.path !== 'assets/blockkart.vpak.provenance.json',
  );
  assert.throws(
    () => protocol.validateBlockKartProjectProtocol(
      missingProducer,
      preparedProtocolBytes(missingProducer),
    ),
    /missing its VPAK producer binding|missing assets\/blockkart\.vpak\.provenance\.json/,
  );
  const mismatchedProducer = structuredClone(project);
  const producerFile = mismatchedProducer.files.find(
    (file) => file.path === 'assets/blockkart.vpak.provenance.json',
  );
  const producerValue = JSON.parse(producerFile.content);
  producerValue.pack.size += 1;
  replaceProtocolTextFile(
    mismatchedProducer.files,
    producerFile.path,
    `${JSON.stringify(producerValue, null, 2)}\n`,
  );
  assert.throws(
    () => protocol.validateBlockKartProjectProtocol(
      mismatchedProducer,
      preparedProtocolBytes(mismatchedProducer),
    ),
    /does not bind the authenticated pack descriptor/,
  );
  const staleProducerDigest = structuredClone(project);
  const staleProducerFile = staleProducerDigest.files.find(
    (file) => file.path === 'assets/blockkart.vpak.provenance.json',
  );
  const staleProducerValue = JSON.parse(staleProducerFile.content);
  staleProducerValue.inputs[0] = { tampered: true };
  replaceProtocolTextFile(
    staleProducerDigest.files,
    staleProducerFile.path,
    `${JSON.stringify(staleProducerValue, null, 2)}\n`,
  );
  const staleBytes = preparedProtocolBytes(staleProducerDigest);
  const structurallyValid = protocol.validateBlockKartProjectProtocol(
    staleProducerDigest,
    staleBytes,
  );
  await assert.rejects(
    () => protocol.validateBlockKartVpakProducerDigest(
      protocol.validateBlockKartVpakProducerBinding(structurallyValid.files, staleBytes),
    ),
    /producerDigest does not bind/,
  );
  const validatedDeps = protocol.validateBlockKartDependenciesProtocol(
    deps,
    validatedProject.snapshot,
    deps.snapshotDigest,
  );
  assert.deepEqual(validatedDeps.modules.map((module) => module.module), ['github.com/acme/lib']);

  const exactProfileLimit = structuredClone(project);
  const sourceLimit = 64 * 1024 * 1024;
  const bytesWithoutMain = exactProfileLimit.files
    .filter((file) => file.path !== 'main.vo')
    .reduce((total, file) => total + file.size, 0);
  exactProfileLimit.files.find((file) => file.path === 'main.vo').size = sourceLimit - bytesWithoutMain;
  assert.doesNotThrow(() => protocol.validateBlockKartProjectProtocol(
    exactProfileLimit,
    preparedProtocolBytes(exactProfileLimit),
  ));

  const profileOverflow = structuredClone(exactProfileLimit);
  profileOverflow.files.find((file) => file.path === 'main.vo').size += 1;
  assert.throws(
    () => protocol.validateBlockKartProjectProtocol(
      profileOverflow,
      preparedProtocolBytes(profileOverflow),
    ),
    /64 MiB Quickplay source byte limit/,
  );
});

test('Quickplay protocol treats a vN repository name as an unsuffixed module root', () => {
  const protocol = compileQuickplayProtocolValidators();
  for (const repository of ['v0', 'v1', 'v2', 'v02']) {
    const module = `github.com/acme/${repository}`;
    const { project, deps } = quickplayWorkspaceProtocolFixture(protocol, module);
    const validatedProject = protocol.validateBlockKartProjectProtocol(
      project,
      preparedProtocolBytes(project),
    );
    assert.equal(validatedProject.snapshot.modules[0].module, module);
    assert.doesNotThrow(() => protocol.validateBlockKartDependenciesProtocol(
      deps,
      validatedProject.snapshot,
      deps.snapshotDigest,
    ));
  }
});

test('Quickplay JS validator enforces canonical Git-ref and version boundaries', () => {
  const identity = compileQuickplayIdentityValidators();
  for (const repository of ['v0', 'v1', 'v2', 'v02']) {
    assert.equal(
      identity.validateModulePath(`github.com/acme/${repository}`, 'module', 1n),
      `github.com@acme@${repository}`,
    );
  }
  assert.equal(identity.validateModulePath('github.com/acme/lib/v2', 'module', 2n), 'github.com@acme@lib@v2');
  for (const module of [
    'github.com/acme/lib/foo..bar',
    'github.com/acme/lib/foo.lock',
    'github.com/acme/lib/v0',
    'github.com/acme/lib/v1',
    'github.com/acme/lib/v02',
    'github.com/acme/lib/v18446744073709551616',
  ]) {
    assert.throws(() => identity.validateModulePath(module, 'module', 1n), /module/);
  }

  const maximum = `0.1.0-${'a'.repeat(254 - '0.1.0-'.length)}`;
  assert.equal(new TextEncoder().encode(maximum).byteLength, 254);
  assert.doesNotThrow(() => identity.parseVersion(maximum, 'version'));
  assert.throws(() => identity.parseVersion(`${maximum}a`, 'version'), /length/);
  assert.throws(() => identity.parseVersion('1.0.0-RC.1', 'version'), /SemVer/);
  assert.throws(() => identity.parseVersion('1.0.0-alpha.lock', 'version'), /\.lock/);

});

test('Studio cache validator distinguishes repository names from major suffixes', () => {
  const validateCacheDir = compileStaticModuleCacheDirValidator();
  for (const repository of ['v0', 'v1', 'v2', 'v02']) {
    const module = `github.com/acme/${repository}`;
    assert.equal(validateCacheDir({
      source: 'registry',
      module,
      version: '1.2.3',
      cacheDir: `${module.replaceAll('/', '@')}/1.2.3`,
    }), `${module.replaceAll('/', '@')}/1.2.3`);
  }
  assert.equal(validateCacheDir({
    source: 'registry',
    module: 'github.com/acme/lib/v2',
    version: '2.1.0',
    cacheDir: 'github.com@acme@lib@v2/2.1.0',
  }), 'github.com@acme@lib@v2/2.1.0');
  for (const module of [
    'github.com/acme/lib/foo..bar',
    'github.com/acme/lib/foo.lock',
    'github.com/acme/lib/v0',
    'github.com/acme/lib/v1',
    'github.com/acme/lib/v02',
    'github.com/acme/lib/v18446744073709551616',
  ]) {
    assert.throws(() => validateCacheDir({
      source: 'registry',
      module,
      version: '1.2.3',
      cacheDir: `${module.replaceAll('/', '@')}/1.2.3`,
    }), /Invalid static dependency|module\/version mismatch/);
  }
  const maximum = `0.1.0-${'a'.repeat(254 - '0.1.0-'.length)}`;
  assert.equal(validateCacheDir({
    source: 'registry',
    module: 'github.com/acme/lib',
    version: maximum,
    cacheDir: `github.com@acme@lib/${maximum}`,
  }), `github.com@acme@lib/${maximum}`);
  assert.throws(() => validateCacheDir({
    source: 'registry',
    module: 'github.com/acme/lib',
    version: `${maximum}a`,
    cacheDir: `github.com@acme@lib/${maximum}a`,
  }), /Invalid static dependency version/);
  for (const version of ['1.0.0-RC.1', '1.0.0-alpha.lock']) {
    assert.throws(() => validateCacheDir({
      source: 'registry',
      module: 'github.com/acme/lib',
      version,
      cacheDir: `github.com@acme@lib/${version}`,
    }), /Invalid static dependency version/);
  }
});

test('Studio project names reject every portable Windows device spelling', () => {
  const validateProjectName = compileProjectNameValidator();
  assert.equal(validateProjectName('Comet'), 'Comet');
  for (const name of [
    'CON',
    'con.txt',
    'COM1',
    'com¹',
    'CoM².data',
    'LPT3',
    'lpt³.vo',
  ]) {
    assert.throws(() => validateProjectName(name), /reserved on supported filesystems/);
  }
});

test('Quickplay project protocol rejects snapshot, lock, workfile, and root intent tampering', () => {
  const protocol = compileQuickplayProtocolValidators();
  const fixture = () => quickplayWorkspaceProtocolFixture(protocol).project;

  const sourceShape = fixture();
  sourceShape.snapshot.modules[0].source.version = '0.1.0';
  assert.throws(
    () => protocol.validateBlockKartProjectProtocol(sourceShape, preparedProtocolBytes(sourceShape)),
    /source.*unexpected fields/,
  );

  const sourceDirectory = fixture();
  sourceDirectory.snapshot.modules[0].source.directory = '../modules/other';
  assert.throws(
    () => protocol.validateBlockKartProjectProtocol(sourceDirectory, preparedProtocolBytes(sourceDirectory)),
    /workspace source directory is not canonical/,
  );

  const selfDependency = fixture();
  selfDependency.snapshot.modules[0].dependencies = [{
    module: 'github.com/acme/lib',
    constraint: '0.1.0',
  }];
  assert.throws(
    () => protocol.validateBlockKartProjectProtocol(selfDependency, preparedProtocolBytes(selfDependency)),
    /must not depend on itself/,
  );

  const lock = quickplayLockProjectProtocolFixture(protocol);
  const lockFile = lock.files.find((file) => file.path === 'vo.lock');
  replaceProtocolTextFile(lock.files, 'vo.lock', `${lockFile.content}# forged\n`);
  assert.throws(
    () => protocol.validateBlockKartProjectProtocol(lock, preparedProtocolBytes(lock)),
    /vo\.lock.*canonical v3/,
  );

  const work = fixture();
  replaceProtocolTextFile(work.files, 'vo.work', 'version = 1\nmembers = []\n');
  assert.throws(
    () => protocol.validateBlockKartProjectProtocol(work, preparedProtocolBytes(work)),
    /vo\.work.*workspace module closure/,
  );

  const missingWork = fixture();
  missingWork.files = missingWork.files.filter((file) => file.path !== 'vo.work');
  assert.throws(
    () => protocol.validateBlockKartProjectProtocol(missingWork, preparedProtocolBytes(missingWork)),
    /missing vo\.work/,
  );

  const rootIntent = fixture();
  replaceProtocolTextFile(
    rootIntent.files,
    'vo.mod',
    rootIntent.files.find((file) => file.path === 'vo.mod').content.replace('0.1.0', '0.2.0'),
  );
  assert.throws(
    () => protocol.validateBlockKartProjectProtocol(rootIntent, preparedProtocolBytes(rootIntent)),
    /vo\.mod.*root intent/,
  );

  const bomRootIntent = fixture();
  const canonicalRootMod = bomRootIntent.files.find((file) => file.path === 'vo.mod').content;
  replaceProtocolTextFile(bomRootIntent.files, 'vo.mod', `\ufeff${canonicalRootMod}`);
  assert.throws(
    () => protocol.validateBlockKartProjectProtocol(
      bomRootIntent,
      preparedProtocolBytes(bomRootIntent),
    ),
    /vo\.mod.*root intent/,
  );
});

test('Quickplay dependency protocol rejects graph and browser artifact tampering', () => {
  const protocol = compileQuickplayProtocolValidators();
  const fixture = () => quickplayWorkspaceProtocolFixture(protocol);

  const digest = fixture();
  digest.deps.snapshotDigest = protocolDigest('a');
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(digest.deps, digest.project.snapshot, protocolDigest('8')),
    /snapshot digests differ/,
  );

  const module = fixture();
  module.deps.modules[0].module = 'github.com/acme/other';
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(module.deps, module.project.snapshot, module.deps.snapshotDigest),
    /exactly match ProjectSnapshot order/,
  );

  for (const mutation of [
    (artifact) => { artifact.kind = 'extension-native'; },
    (artifact) => { artifact.target = 'aarch64-apple-darwin'; },
    (artifact) => { artifact.size = 0; },
  ]) {
    const artifact = fixture();
    mutation(artifact.deps.modules[0].artifacts[0]);
    assert.throws(
      () => protocol.validateBlockKartDependenciesProtocol(
        artifact.deps,
        artifact.project.snapshot,
        artifact.deps.snapshotDigest,
      ),
      /browser|native|positive bounded size/,
    );
  }

  const workspaceVoMod = fixture();
  replaceProtocolTextFile(
    workspaceVoMod.deps.modules[0].files,
    'vo.mod',
    'module = "github.com/acme/other"\nvo = "^0.2.0"\n',
  );
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      workspaceVoMod.deps,
      workspaceVoMod.project.snapshot,
      workspaceVoMod.deps.snapshotDigest,
    ),
    /workspace vo\.mod.*ProjectSnapshot module intent/,
  );

  const workspaceDependencyIntent = fixture();
  replaceProtocolTextFile(
    workspaceDependencyIntent.deps.modules[0].files,
    'vo.mod',
    'module = "github.com/acme/lib"\nvo = "^0.2.0"\n\n[dependencies]\n"github.com/acme/other" = "0.1.0"\n',
  );
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      workspaceDependencyIntent.deps,
      workspaceDependencyIntent.project.snapshot,
      workspaceDependencyIntent.deps.snapshotDigest,
    ),
    /declares dependencies outside the ProjectSnapshot module intent/,
  );

  const artifactUrl = fixture();
  artifactUrl.deps.modules[0].artifacts[0].url = '/quickplay/blockkart/artifacts/forged.wasm';
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      artifactUrl.deps,
      artifactUrl.project.snapshot,
      artifactUrl.deps.snapshotDigest,
    ),
    /artifact URL differs from its canonical Quickplay path/,
  );

  const artifactAncestor = fixture();
  artifactAncestor.deps.modules[0].files.push(protocolTextFile('web-artifacts', 'occupied\n'));
  sortedProtocolFiles(artifactAncestor.deps.modules[0].files);
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      artifactAncestor.deps,
      artifactAncestor.project.snapshot,
      artifactAncestor.deps.snapshotDigest,
    ),
    /file\/directory collision/,
  );

  const prefixAliases = fixture();
  prefixAliases.deps.modules[0].files.push(protocolTextFile('Sources/one.vo', 'package lib\n'));
  prefixAliases.deps.modules[0].files.push(protocolTextFile('sources/two.vo', 'package lib\n'));
  sortedProtocolFiles(prefixAliases.deps.modules[0].files);
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      prefixAliases.deps,
      prefixAliases.project.snapshot,
      prefixAliases.deps.snapshotDigest,
    ),
    /portable path alias/,
  );

  const pathAmplification = fixture();
  for (let index = 0; index < 407; index += 1) {
    const deepPath = [
      `branch-${index.toString(36)}`,
      ...Array.from({ length: 244 }, () => 'd'),
      'f.vo',
    ].join('/');
    pathAmplification.deps.modules[0].files.push(protocolTextFile(deepPath, 'package lib\n'));
  }
  sortedProtocolFiles(pathAmplification.deps.modules[0].files);
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      pathAmplification.deps,
      pathAmplification.project.snapshot,
      pathAmplification.deps.snapshotDigest,
    ),
    /(?:path-closure|path-key) limit/,
  );

  const reservedAlias = fixture();
  reservedAlias.deps.modules[0].files.push(protocolTextFile('vo.web.jſon', '{}\n'));
  sortedProtocolFiles(reservedAlias.deps.modules[0].files);
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      reservedAlias.deps,
      reservedAlias.project.snapshot,
      reservedAlias.deps.snapshotDigest,
    ),
    /reserved protocol state/,
  );
});

test('Quickplay registry protocol binds canonical release and package metadata to the snapshot', () => {
  const protocol = compileQuickplayProtocolValidators();
  const fixture = () => quickplayRegistryProtocolFixture();
  const mutatePackage = (subject, mutate) => {
    const packageFile = subject.deps.modules[0].files.find((file) => file.path === 'vo.package.json');
    const packageManifest = JSON.parse(packageFile.content);
    mutate(packageManifest);
    const packageText = `${protocol.canonicalProtocolJsonText(packageManifest)}\n`;
    replaceProtocolTextFile(subject.deps.modules[0].files, 'vo.package.json', packageText);
    const releaseFile = subject.deps.modules[0].files.find((file) => file.path === 'vo.release.json');
    const release = JSON.parse(releaseFile.content);
    release.package.size = new TextEncoder().encode(packageText).byteLength;
    replaceProtocolTextFile(
      subject.deps.modules[0].files,
      'vo.release.json',
      `${protocol.canonicalProtocolJsonText(release)}\n`,
    );
  };
  const mutateRelease = (subject, mutate) => {
    const releaseFile = subject.deps.modules[0].files.find((file) => file.path === 'vo.release.json');
    const release = JSON.parse(releaseFile.content);
    mutate(release);
    replaceProtocolTextFile(
      subject.deps.modules[0].files,
      'vo.release.json',
      `${protocol.canonicalProtocolJsonText(release)}\n`,
    );
  };
  const valid = fixture();
  assert.doesNotThrow(() => protocol.validateBlockKartDependenciesProtocol(
    valid.deps,
    valid.snapshot,
    valid.deps.snapshotDigest,
  ));

  const artifactBoundary = fixture();
  mutateRelease(artifactBoundary, (release) => {
    release.artifacts[0].size = 256 * 1024 * 1024;
  });
  artifactBoundary.deps.modules[0].artifacts[0].size = 256 * 1024 * 1024;
  assert.doesNotThrow(() => protocol.validateBlockKartDependenciesProtocol(
    artifactBoundary.deps,
    artifactBoundary.snapshot,
    artifactBoundary.deps.snapshotDigest,
  ));

  const oversizedReleaseArtifact = fixture();
  mutateRelease(oversizedReleaseArtifact, (release) => {
    release.artifacts[0].size = 256 * 1024 * 1024 + 1;
  });
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      oversizedReleaseArtifact.deps,
      oversizedReleaseArtifact.snapshot,
      oversizedReleaseArtifact.deps.snapshotDigest,
    ),
    /positive bounded size/,
  );

  const oversizedInstallArtifact = fixture();
  oversizedInstallArtifact.deps.modules[0].artifacts[0].size = 256 * 1024 * 1024 + 1;
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      oversizedInstallArtifact.deps,
      oversizedInstallArtifact.snapshot,
      oversizedInstallArtifact.deps.snapshotDigest,
    ),
    /positive bounded size/,
  );

  const oversizedReleaseMetadata = fixture();
  oversizedReleaseMetadata.deps.modules[0].files.find(
    (file) => file.path === 'vo.release.json',
  ).size = 16 * 1024 * 1024 + 1;
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      oversizedReleaseMetadata.deps,
      oversizedReleaseMetadata.snapshot,
      oversizedReleaseMetadata.deps.snapshotDigest,
    ),
    /release metadata size.*positive bounded size/,
  );

  const oversizedSourceArchive = fixture();
  const oversizedReleaseFile = oversizedSourceArchive.deps.modules[0].files.find(
    (file) => file.path === 'vo.release.json',
  );
  const oversizedRelease = JSON.parse(oversizedReleaseFile.content);
  oversizedRelease.source.size = 64 * 1024 * 1024 + 1;
  replaceProtocolTextFile(
    oversizedSourceArchive.deps.modules[0].files,
    'vo.release.json',
    `${protocol.canonicalProtocolJsonText(oversizedRelease)}\n`,
  );
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      oversizedSourceArchive.deps,
      oversizedSourceArchive.snapshot,
      oversizedSourceArchive.deps.snapshotDigest,
    ),
    /release source size/,
  );

  const oversizedVoSource = fixture();
  mutatePackage(oversizedVoSource, (manifest) => {
    manifest.files.push({
      path: 'src/main.vo',
      mode: 'regular',
      size: 16 * 1024 * 1024 + 1,
      digest: protocolDigest('a'),
    });
    manifest.files.sort((left, right) => compareUtf8(left.path, right.path));
  });
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      oversizedVoSource.deps,
      oversizedVoSource.snapshot,
      oversizedVoSource.deps.snapshotDigest,
    ),
    /Vo text-file limit/,
  );

  for (const nestedModuleManifest of ['nested/vo.mod', 'nested/VO.MOD', 'VO.MOD']) {
    const nestedModule = fixture();
    mutatePackage(nestedModule, (manifest) => {
      manifest.files.push({
        path: nestedModuleManifest,
        mode: 'regular',
        size: 1,
        digest: protocolDigest('a'),
      });
      manifest.files.sort((left, right) => compareUtf8(left.path, right.path));
    });
    assert.throws(
      () => protocol.validateBlockKartDependenciesProtocol(
        nestedModule.deps,
        nestedModule.snapshot,
        nestedModule.deps.snapshotDigest,
      ),
      /reserved by the module protocol/,
    );
  }
  for (const rootSourceArchive of ['source.tar.gz', 'SOURCE.TAR.GZ', 'ſource.tar.gz']) {
    const reservedSourceArchive = fixture();
    mutatePackage(reservedSourceArchive, (manifest) => {
      manifest.files.push({
        path: rootSourceArchive,
        mode: 'regular',
        size: 1,
        digest: protocolDigest('a'),
      });
      manifest.files.sort((left, right) => compareUtf8(left.path, right.path));
    });
    assert.throws(
      () => protocol.validateBlockKartDependenciesProtocol(
        reservedSourceArchive.deps,
        reservedSourceArchive.snapshot,
        reservedSourceArchive.deps.snapshotDigest,
      ),
      /reserved by the module protocol/,
    );
  }
  assert.equal(
    protocol.canonicalProtocolJsonText({ value: '数据\u0001\n' }),
    '{\n  "value": "数据\\u0001\\n"\n}',
  );
  assert.throws(
    () => protocol.canonicalProtocolJsonText({ value: '\ud800' }),
    /Unicode scalar values/,
  );
  let overlyDeepJson = null;
  for (let depth = 0; depth <= 127; depth += 1) overlyDeepJson = [overlyDeepJson];
  assert.throws(() => protocol.canonicalProtocolJsonText(overlyDeepJson), /depth exceeds 127/);

  const missingStaticMode = fixture();
  delete missingStaticMode.deps.modules[0].files[0].mode;
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      missingStaticMode.deps,
      missingStaticMode.snapshot,
      missingStaticMode.deps.snapshotDigest,
    ),
    /unexpected fields/,
  );

  const unknownStaticMode = fixture();
  unknownStaticMode.deps.modules[0].files[0].mode = 0o640;
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      unknownStaticMode.deps,
      unknownStaticMode.snapshot,
      unknownStaticMode.deps.snapshotDigest,
    ),
    /mode must be 0644 or 0755/,
  );

  const missingPackageMode = fixture();
  mutatePackage(missingPackageMode, (manifest) => { delete manifest.files[0].mode; });
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      missingPackageMode.deps,
      missingPackageMode.snapshot,
      missingPackageMode.deps.snapshotDigest,
    ),
    /unexpected fields/,
  );

  const unknownPackageMode = fixture();
  mutatePackage(unknownPackageMode, (manifest) => { manifest.files[0].mode = 'readonly'; });
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      unknownPackageMode.deps,
      unknownPackageMode.snapshot,
      unknownPackageMode.deps.snapshotDigest,
    ),
    /mode is invalid/,
  );

  const packageModeDrift = fixture();
  packageModeDrift.deps.modules[0].files.find((file) => file.path === 'vo.mod').mode = 0o755;
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      packageModeDrift.deps,
      packageModeDrift.snapshot,
      packageModeDrift.deps.snapshotDigest,
    ),
    /differs from vo\.package\.json/,
  );

  const executableSource = fixture();
  const executableBytes = new TextEncoder().encode('#!/bin/sh\n');
  mutatePackage(executableSource, (manifest) => {
    manifest.files.push({
      path: 'bin/tool',
      mode: 'executable',
      size: executableBytes.byteLength,
      digest: protocolDigest('a'),
    });
    manifest.files.sort((left, right) => compareUtf8(left.path, right.path));
  });
  executableSource.deps.modules[0].files.push(
    protocolTextFile('bin/tool', '#!/bin/sh\n', 'a', 0o755),
  );
  sortedProtocolFiles(executableSource.deps.modules[0].files);
  assert.doesNotThrow(() => protocol.validateBlockKartDependenciesProtocol(
    executableSource.deps,
    executableSource.snapshot,
    executableSource.deps.snapshotDigest,
  ));

  const releaseDigest = fixture();
  releaseDigest.deps.modules[0].files.find((file) => file.path === 'vo.release.json').digest = protocolDigest('a');
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      releaseDigest.deps,
      releaseDigest.snapshot,
      releaseDigest.deps.snapshotDigest,
    ),
    /release digest differs/,
  );

  const releaseIdentity = fixture();
  const releaseFile = releaseIdentity.deps.modules[0].files.find((file) => file.path === 'vo.release.json');
  const release = JSON.parse(releaseFile.content);
  release.module = 'github.com/acme/other';
  replaceProtocolTextFile(
    releaseIdentity.deps.modules[0].files,
    'vo.release.json',
    `${JSON.stringify(release, null, 2)}\n`,
  );
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      releaseIdentity.deps,
      releaseIdentity.snapshot,
      releaseIdentity.deps.snapshotDigest,
    ),
    /release identity differs/,
  );

  const releaseOrder = fixture();
  const orderedReleaseFile = releaseOrder.deps.modules[0].files.find((file) => file.path === 'vo.release.json');
  const orderedRelease = JSON.parse(orderedReleaseFile.content);
  const reorderedRelease = {
    module: orderedRelease.module,
    schema_version: orderedRelease.schema_version,
    version: orderedRelease.version,
    commit: orderedRelease.commit,
    vo: orderedRelease.vo,
    dependencies: orderedRelease.dependencies,
    source: orderedRelease.source,
    package: orderedRelease.package,
    artifacts: orderedRelease.artifacts,
  };
  replaceProtocolTextFile(
    releaseOrder.deps.modules[0].files,
    'vo.release.json',
    `${JSON.stringify(reorderedRelease, null, 2)}\n`,
  );
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      releaseOrder.deps,
      releaseOrder.snapshot,
      releaseOrder.deps.snapshotDigest,
    ),
    /canonical field order/,
  );

  const packageBinding = fixture();
  packageBinding.deps.modules[0].files.find((file) => file.path === 'vo.package.json').digest = protocolDigest('b');
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      packageBinding.deps,
      packageBinding.snapshot,
      packageBinding.deps.snapshotDigest,
    ),
    /package metadata differs/,
  );

  const packageOrder = fixture();
  const orderedPackageFile = packageOrder.deps.modules[0].files.find((file) => file.path === 'vo.package.json');
  const orderedPackage = JSON.parse(orderedPackageFile.content);
  replaceProtocolTextFile(
    packageOrder.deps.modules[0].files,
    'vo.package.json',
    `${JSON.stringify({ files: orderedPackage.files, schema_version: orderedPackage.schema_version }, null, 2)}\n`,
  );
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      packageOrder.deps,
      packageOrder.snapshot,
      packageOrder.deps.snapshotDigest,
    ),
    /canonical field order/,
  );

  const artifactBinding = fixture();
  artifactBinding.deps.modules[0].artifacts[0].digest = protocolDigest('c');
  assert.throws(
    () => protocol.validateBlockKartDependenciesProtocol(
      artifactBinding.deps,
      artifactBinding.snapshot,
      artifactBinding.deps.snapshotDigest,
    ),
    /browser artifact differs/,
  );
});

test('Quickplay runtime accepts only the canonical vNext artifact layout', () => {
  const validateStaticArtifact = compileStaticArtifactValidator();
  const registryModule = {
    source: 'registry',
    cacheDir: 'github.com@vo-lang@vogui/0.1.15',
  };
  const canonical = {
    kind: 'extension-wasm',
    target: 'wasm32-unknown-unknown',
    name: 'vogui.wasm',
    path: 'artifacts/extension-wasm/wasm32-unknown-unknown/vogui.wasm',
    url: '/quickplay/blockkart/artifacts/github.com@vo-lang@vogui/0.1.15/extension-wasm/wasm32-unknown-unknown/vogui.wasm',
    size: 4,
    digest: `sha256:${'0'.repeat(64)}`,
  };
  assert.equal(
    validateStaticArtifact(registryModule, canonical),
    canonical.path,
  );
  assert.throws(
    () => validateStaticArtifact(registryModule, {
      ...canonical,
      path: 'artifacts/vogui.wasm',
    }),
    /path does not match/,
  );
  assert.throws(
    () => validateStaticArtifact(registryModule, {
      ...canonical,
      url: canonical.url.replaceAll('@', '%40'),
    }),
    /URL does not match/,
  );
  assert.throws(
    () => validateStaticArtifact(registryModule, {
      ...canonical,
      kind: 'extension-native',
      target: 'aarch64-apple-darwin',
    }),
    /artifact kind/,
  );
  assert.throws(
    () => validateStaticArtifact(registryModule, { ...canonical, size: 0 }),
    /artifact size/,
  );
  const workspaceModule = {
    source: 'workspace',
    cacheDir: 'workspace/github.com@vo-lang@vogui',
  };
  const workspace = {
    ...canonical,
    path: 'web-artifacts/vogui.wasm',
    url: '/quickplay/blockkart/artifacts/workspace/github.com@vo-lang@vogui/extension-wasm/wasm32-unknown-unknown/vogui.wasm',
  };
  assert.equal(validateStaticArtifact(workspaceModule, workspace), workspace.path);
  assert.throws(
    () => validateStaticArtifact(workspaceModule, { ...workspace, path: 'vogui.wasm' }),
    /local build asset/,
  );
});

test('Quickplay state commits serialize and publish snapshot authority last', async () => {
  const backend = sourceFile('src/lib/backend/web_backend.ts');
  const serializerSource = extract(
    backend,
    'function serializeBlockKartStateOperation',
    'function findProjectRootForEntry',
  );
  const serialize = new Function(
    `${transpile(`let blockKartStateOperationChain: Promise<void> = Promise.resolve();\n${serializerSource}`)}
return serializeBlockKartStateOperation;`,
  )();
  const events = [];
  let releaseFirst;
  const firstGate = new Promise((resolve) => { releaseFirst = resolve; });
  const first = serialize(async () => {
    events.push('first-start');
    await firstGate;
    events.push('first-end');
    return 1;
  });
  const second = serialize(async () => {
    events.push('second-start');
    events.push('second-end');
    return 2;
  });
  await Promise.resolve();
  await Promise.resolve();
  assert.deepEqual(events, ['first-start']);
  releaseFirst();
  assert.deepEqual(await Promise.all([first, second]), [1, 2]);
  assert.deepEqual(events, ['first-start', 'first-end', 'second-start', 'second-end']);

  assert.match(backend, /return serializeBlockKartStateOperation\(openBlockKartQuickPlaySession\)/);
  assert.match(
    backend,
    /serializeBlockKartStateOperation\(ensureBlockKartPackagedDependencies\)/,
  );
  const open = extract(
    backend,
    'async function openBlockKartQuickPlaySession',
    'async function validateBlockKartProjectPackage',
  );
  assert.ok(
    open.indexOf('blockKartProjectSnapshotDigest = snapshotDigest')
      > open.indexOf("if (!hasVfsFile(entryPath))"),
    'Quickplay snapshot authority must publish only after the complete VFS commit',
  );
});

test('Quickplay workspace artifacts install identical verified bytes at canonical and build paths', () => {
  const install = compileStaticArtifactInstaller();
  const bytes = new Uint8Array([0, 97, 115, 109]);
  const modulePack = {
    source: 'workspace',
    cacheDir: 'workspace/github.com@vo-lang@vogui',
  };
  const artifact = {
    kind: 'extension-wasm',
    target: 'wasm32-unknown-unknown',
    name: 'vogui.wasm',
    path: 'web-artifacts/vogui.wasm',
    url: '/quickplay/blockkart/artifacts/workspace/github.com@vo-lang@vogui/extension-wasm/wasm32-unknown-unknown/vogui.wasm',
    size: bytes.byteLength,
    digest: `sha256:${'0'.repeat(64)}`,
  };
  const files = install(modulePack, artifact, bytes, 0o644);
  assert.deepEqual(files.map((file) => file.relative), [
    'artifacts/extension-wasm/wasm32-unknown-unknown/vogui.wasm',
    'web-artifacts/vogui.wasm',
  ]);
  assert.strictEqual(files[0].bytes, bytes);
  assert.strictEqual(files[1].bytes, bytes);
});

test('Quickplay registry trees receive explicit release capabilities after atomic installation', () => {
  const backend = sourceFile('src/lib/backend/web_backend.ts');
  const preparation = extract(
    backend,
    'async function prepareBlockKartPackagedDependencies(',
    'function prepareStaticPackageFiles(',
  );
  assert.match(preparation, /modulePack\.files\.find\(\(file\) => file\.path === 'vo\.release\.json'\)/);
  assert.match(preparation, /releaseDigest: release\.digest/);
  assert.match(preparation, /wasm\.registerBrowserReleaseCapabilities\(/);

  const opening = extract(
    backend,
    'async function openBlockKartQuickPlaySession(',
    'async function validateBlockKartProjectPackage(',
  );
  assert.ok(
    opening.indexOf('prepareBrowserReleaseCapabilityCommit')
      < opening.indexOf('replacePreparedStaticTreesAtomically'),
    'the WASM capability registrar must be ready before VFS mutation starts',
  );
  assert.match(
    opening,
    /replacePreparedStaticTreesAtomically\(transactionTrees, \(\) => \{[\s\S]*commitCapabilities\(\);[\s\S]*\}\)/,
  );
  assert.ok(
    opening.indexOf('blockKartDepsInstalledDigest = snapshotDigest')
      > opening.indexOf('replacePreparedStaticTreesAtomically'),
    'snapshot authority must publish only after the VFS and capability batch commit',
  );
});

test('Quickplay project loading authenticates files before invoking the deep protocol validator', () => {
  const backend = sourceFile('src/lib/backend/web_backend.ts');
  const validation = extract(
    backend,
    'async function validateBlockKartProjectPackage(',
    'async function ensureBlockKartPackagedDependenciesForEntry(',
  );
  assert.ok(
    validation.indexOf('prepareAuthenticatedStaticPackageFiles')
      < validation.indexOf('validateBlockKartProjectProtocol'),
    'deep protocol validation must consume authenticated bytes',
  );
  assert.doesNotMatch(validation, /snapshot\.schema_version\s*!==\s*1|snapshot\.authority\s*!==\s*'lock'/);
  assert.doesNotMatch(validation, /\['main\.vo', 'vo\.mod', 'vo\.lock'\]/);
  assert.match(validation, /snapshotDigest: await sha256JsonValue\(pack\.snapshot\)/);
  assert.match(validation, /validateBlockKartVpakProducerBinding\(pack\.files, preparedBytes\)/);
  assert.match(validation, /await validateBlockKartVpakProducerDigest\(producer\)/);
});

test('Studio toolchain surface omits context-free module installation and cache browsing', () => {
  for (const relative of [
    'src/lib/backend/backend.ts',
    'src/lib/backend/native_backend.ts',
    'src/lib/backend/web_backend.ts',
    'src/lib/services/extension_service.ts',
    'src/lib/types.ts',
  ]) {
    const source = sourceFile(relative);
    assert.doesNotMatch(source, /\bvoGet\b|\blistInstalledModules\b|\bInstallEvent\b|\bInstalledModule\b/);
  }
  assert.match(
    sourceFile('src/lib/backend/backend.ts'),
    /voInit\(path: string, module: string, mainContent: string\): Promise<string>/,
  );
  assert.match(
    sourceFile('src/lib/backend/native_backend.ts'),
    /cmd_vo_init[\s\S]*\{ path, module, mainContent \}/,
  );

  const catalog = sourceFile('src/lib/services/project_catalog_service.ts');
  assert.match(catalog, /await this\.backend\.voInit\(dirPath, modulePath, mainContent\)/);
  assert.match(catalog, /trimmed !== trimmed\.normalize\('NFC'\)/);
  assert.match(catalog, /JSON\.stringify\(value\)/);
  assert.doesNotMatch(catalog, /assertCanonicalModulePath|vo = "\^0\.1\.0"/);
  assert.match(catalog, /return `package main\\n/);
  assert.doesNotMatch(catalog, /`module \$\{[^}]+\}\\n|\\nvo 1\.0\\n/);

  const nativeCommands = sourceFile('src-tauri/src/commands/extension.rs');
  assert.match(nativeCommands, /vo_module::ops::mod_init\(dir, module\)/);
  assert.match(nativeCommands, /remove_dir_all\(&dir\)/);
  assert.match(nativeCommands, /vo_module::TOOLCHAIN_VERSION/);
  assert.doesNotMatch(nativeCommands, /cmd_vo_get|install_module|list_installed/);

  const webBackend = sourceFile('src/lib/backend/web_backend.ts');
  assert.match(webBackend, /wasm\.renderInitialModuleManifest\(module\)/);
  assert.match(webBackend, /return wasm\.voVersion\(\)/);
  assert.match(webBackend, /vfsMkdir\(normalized, 0o755\)/);
  assert.match(webBackend, /vfsRemoveAll\(normalized\)/);
  assert.doesNotMatch(webBackend, /const existed = directories\.has\(normalized\)/);
  const studioWasm = sourceFile('src/lib/studio_wasm.ts');
  assert.match(
    studioWasm,
    /renderInitialModuleManifest:\s*requireStudioExport\([\s\S]*?'renderInitialModuleManifest'/,
  );
  assert.match(studioWasm, /voVersion:\s*requireStudioExport\([\s\S]*?'voVersion'/);
  for (const removedLegacyImport of [
    'host_focus',
    'host_blur',
    'host_scroll_to',
    'host_scroll_into_view',
    'host_select_text',
  ]) {
    assert.doesNotMatch(
      studioWasm,
      new RegExp(`\\b${removedLegacyImport}\\b`),
      `${removedLegacyImport} must stay removed from the v3 standalone host ABI`,
    );
  }
  const wasm = sourceFile('wasm/src/lib.rs');
  assert.match(wasm, /render_initial_mod_file\(module\)/);
});

test('Quickplay generator delegates module semantics to the core snapshot protocol', () => {
  const source = sourceFile('scripts/package_blockkart_quickplay.mjs');
  assert.match(source, /withCleanEffectiveSnapshot/);
  assert.match(source, /packageSnapshotModule/);
  assert.doesNotMatch(source, /vo\.web\.json|parseVoLock|release_manifest/);
});

test('GUI compile output rejects duplicate extension owners and aggregate overflow', () => {
  const { validateGuiCompileOutput } = compileGuiPipelineHelpers();
  const extension = (name, moduleKey, wasmBytes, jsGlueBytes = null) => ({
    name,
    moduleKey,
    wasmBytes: new Uint8Array(wasmBytes),
    jsGlueBytes: jsGlueBytes == null ? null : new Uint8Array(jsGlueBytes),
  });
  assert.throws(
    () => validateGuiCompileOutput({
      bytecode: new Uint8Array(),
      wasmExtensions: [extension('one', 'owner', 1), extension('two', 'owner', 1)],
    }),
    /duplicate extension owner/,
  );
  assert.throws(
    () => validateGuiCompileOutput({
      bytecode: new Uint8Array(),
      wasmExtensions: [extension('one', 'one', 7), extension('two', 'two', 6)],
    }),
    /aggregate limit/,
  );
});

test('GUI extension preload preserves the exact compiler-provided module owner', () => {
  const pipeline = sourceFile('src/lib/gui/gui_pipeline.ts');
  const execute = extract(
    pipeline,
    'export async function executeGuiFromCompileOutput(',
    '// Load the host bridge BEFORE runGuiFromBytecode',
  );
  assert.ok(
    execute.indexOf('resetLoadedWasmExtensions();')
      < execute.indexOf('await wasm.preloadExtModule(ext.moduleKey, ext.wasmBytes, jsGlueUrl);'),
    'the previous extension catalog must be cleared before exact owners are published',
  );
  assert.doesNotMatch(execute, /artifact_|extensionPreloadKey|extensionArtifactFingerprint/);
  const fullExecute = pipeline.slice(
    pipeline.indexOf('export async function executeGuiFromCompileOutput('),
  );
  assert.equal(
    (fullExecute.match(/resetLoadedWasmExtensions\(\);/gu) ?? []).length,
    3,
    'session start, preload failure, and startup failure must each reset extension routing',
  );

  const studioWasm = sourceFile('src/lib/studio_wasm.ts');
  const reset = extract(
    studioWasm,
    'export function resetLoadedWasmExtensions(',
    'function commitExtModule(',
  );
  assert.match(reset, /unloadAllExtModules\(\);/);
});

test('Voplay perf evidence is bound to the active GUI session', () => {
  const rendererBridge = sourceFile('src/lib/gui/renderer_bridge.ts');
  const perfBridge = sourceFile('src/lib/perf_report_bridge.ts');
  const webBackend = sourceFile('src/lib/backend/web_backend.ts');
  const backendContract = sourceFile('src/lib/backend/backend.ts');
  const runtimeService = sourceFile('src/lib/services/runtime_service.ts');
  assert.match(
    rendererBridge,
    /handleVoplayPerfHostLog\([\s\S]{0,200}?sessionId,[\s\S]{0,50}?\)/,
  );
  assert.match(perfBridge, /type PendingVoplayPerfReport = \{[\s\S]*studioSessionId: number \| null;/);
  assert.match(perfBridge, /return \{ \.\.\.payload, studioSessionId, studioPerfEpoch \};/);
  assert.match(
    perfBridge,
    /text === lastVoplayPerfReportText[\s\S]*studioSessionId === lastVoplayPerfReportSessionId/,
  );
  assert.match(
    perfBridge,
    /studioSessionId: number \| null = activeVoplayPerfSessionId/,
  );
  assert.match(
    backendContract,
    /runGui\(path: string, session: GuiSessionToken\): Promise<GuiRunOutput>/,
  );
  assert.match(runtimeService, /this\.backend\.runGui\(target, session\)/);
  assert.match(
    webBackend,
    /this\.guiSession\.activate\(session\);[\s\S]{0,100}?setActiveVoplayPerfSessionId\(sessionId\);/,
  );
  assert.doesNotMatch(webBackend, /guiSessionId\s*\+\s*1/);
});

test('GUI session authority survives stop, rerun, and stale completion races', async () => {
  const { GuiSessionAuthority, GuiSessionBinding } = compileGuiSessionProtocol();
  const authority = new GuiSessionAuthority();
  const backend = new GuiSessionBinding();

  const first = authority.begin();
  backend.activate(first);
  assert.equal(first.id, 1);
  assert.equal(backend.active, first);

  let releaseStaleCompletion;
  const staleCompletionBarrier = new Promise((resolve) => {
    releaseStaleCompletion = resolve;
  });
  const staleCompletion = staleCompletionBarrier.then(() => backend.clear(first));

  assert.equal(authority.invalidate(first), first);
  assert.equal(backend.clear(first), first);
  assert.equal(authority.active, null);
  assert.equal(backend.active, null);

  const second = authority.begin();
  backend.activate(second);
  assert.equal(second.id, 2);
  assert.equal(authority.invalidate(first), null);
  assert.equal(authority.active, second);

  releaseStaleCompletion();
  assert.equal(await staleCompletion, null);
  assert.equal(backend.active, second);
  assert.equal(backend.isActive(second), true);

  assert.equal(authority.invalidate(second), second);
  assert.equal(backend.clear(second), second);
  assert.equal(authority.active, null);
  assert.equal(backend.active, null);
});

test('combined host bridges reject duplicate imports', () => {
  const { combineHostBridgeModules } = compileGuiPipelineHelpers();
  const bridge = combineHostBridgeModules([
    { buildImports: () => ({ host_measure_text: () => 1 }) },
    { buildImports: () => ({ host_measure_text: () => 2 }) },
  ]);
  assert.throws(() => bridge.buildImports({}), /Multiple host bridge modules define import/);
});

function transactionState() {
  return {
    directories: new Set(['/']),
    files: new Map([['/sentinel', 'abc']]),
    vfsFiles: new Map([['/sentinel', new Uint8Array([1, 2, 3])]]),
    vfsFileModes: new Map([['/sentinel', 0o644]]),
    vfsFileModTimes: new Map([['/sentinel', 7]]),
    vfsDirModes: new Map([['/', 0o755]]),
    vfsDirModTimes: new Map([['/', 5]]),
    readOnlyStaticRoots: new Set(['/protected']),
    liveVfsBytes: 3,
    liveVfsPathBytes: 10,
    textCacheBytes: 73,
  };
}

function snapshotTransactionState(state) {
  return {
    directories: [...state.directories],
    files: [...state.files],
    vfsFiles: [...state.vfsFiles].map(([key, value]) => [key, [...value]]),
    vfsFileModes: [...state.vfsFileModes],
    vfsFileModTimes: [...state.vfsFileModTimes],
    vfsDirModes: [...state.vfsDirModes],
    vfsDirModTimes: [...state.vfsDirModTimes],
    readOnlyStaticRoots: [...state.readOnlyStaticRoots],
  };
}

function restoreMap(target, snapshot) {
  target.clear();
  for (const entry of snapshot) {
    if (target instanceof Set) target.add(entry);
    else target.set(...entry);
  }
}
