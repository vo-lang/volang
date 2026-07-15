import assert from 'node:assert/strict';
import { parse as parseJavaScriptModule } from 'acorn';
import { createHash } from 'node:crypto';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';
import ts from 'typescript';
import { fileURLToPath } from 'node:url';
import { artifactKey } from '../../../scripts/ci/quickplay_artifact_paths.mjs';
import { compareUtf8 } from '../../../scripts/ci/utf8_order.mjs';
import {
  quickplayPackageFiles,
  readQuickplayPackageBuildId,
  resolveStudioBuildId,
  validateStudioWasmBuildId,
} from './studio_build_id.mjs';
import {
  portableCaseKey,
  portablePathCollisionKey,
} from '../../../scripts/ci/portable_path_key.mjs';
import { parseBoundedStrictJsonBytes } from '../../../scripts/ci/quickplay_web_manifest_contract.mjs';
import {
  parseVoLockForV2Migration,
  parseVoLockV2,
  parseVoModRootContract,
  renderVoLockV2,
  validatePackagedModuleSet,
  validateVoLockV2RootGraph,
} from '../../../scripts/ci/vo_lock_v2.mjs';

const studioRoot = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');

function parseTestJsonObject(_moduleName, fileName, bytes) {
  const value = parseBoundedStrictJsonBytes(bytes, `fixture ${fileName}`, {
    maxBytes: 16 * 1024 * 1024,
  });
  if (!value || typeof value !== 'object' || Array.isArray(value)) {
    throw new Error(`fixture ${fileName} must contain a JSON object`);
  }
  return value;
}

function sourceFile(relative) {
  return fs.readFileSync(path.join(studioRoot, relative), 'utf8');
}

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
    'portableCaseKey',
    'portablePathCollisionKey',
    'MAX_VFS_PATH_DEPTH',
    'MAX_STATIC_PACKAGE_FILES',
    'MAX_STATIC_PACKAGE_FILE_BYTES',
    'MAX_STATIC_PACKAGE_TOTAL_BYTES',
    `${transpile(portablePaths)}\n${transpile(tarParser)}\nreturn parseTarFiles;`,
  )(
    new TextEncoder(),
    portableCaseKey,
    portablePathCollisionKey,
    256,
    20_000,
    256 * 1024 * 1024,
    512 * 1024 * 1024,
  );
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
    `${transpile(transaction)}\nreturn { replacePreparedStaticTreesAtomically, liveBytes: () => liveVfsBytes };`,
  );
  return factory(...Object.values(bindings));
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
    `${transpile(helpers)}\nreturn { isWithinReadOnlyStaticTree, overlapsReadOnlyStaticTree, canMutateVfsDirectory };`,
  )(readOnlyStaticRoots, vfsDirModes);
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

function compileQuickplayLockRewrite() {
  const source = sourceFile('scripts/package_blockkart_quickplay.mjs');
  const rewrite = extract(
    source,
    'function lockEntryForPackagedModule(',
    'const PUBLISHED_RELEASE_CONTROL_PATHS',
  );
  const bindings = {
    artifactKey,
    compareUtf8,
    fileEntry(files, filePath) {
      const file = files.find((entry) => entry.path === filePath);
      if (!file) throw new Error(`missing ${filePath}`);
      return file;
    },
    packagedFileBytes(file) {
      if (file.content != null) return Buffer.from(file.content, 'utf8');
      return Buffer.from(file.contentBase64, 'base64');
    },
    parsePackagedJsonObject: parseTestJsonObject,
    fileText(files, filePath) {
      return Buffer.from(files.find((entry) => entry.path === filePath).content, 'utf8').toString('utf8');
    },
    sha256Digest,
    parseVoLockForV2Migration,
    parseVoLockV2,
    parseVoModRootContract,
    renderVoLockV2,
    validatePackagedModuleSet,
    validateVoLockV2RootGraph,
  };
  const names = Object.keys(bindings);
  return new Function(
    ...names,
    `${rewrite}\nreturn { rewriteProjectLockForPackagedDependencies };`,
  )(...Object.values(bindings));
}

function compilePublishedDependencyClosure() {
  const source = sourceFile('scripts/package_blockkart_quickplay.mjs');
  const closure = extract(
    source,
    'const PUBLISHED_RELEASE_CONTROL_PATHS',
    'function shouldDeclarePackagedSourceFile(',
  );
  return new Function(
    'compareUtf8',
    'sha256Digest',
    'toPosixRelative',
    'validatePortableRelativePath',
    `${closure}\nreturn {
      controlPaths: PUBLISHED_RELEASE_CONTROL_PATHS,
      publishedBrowserRuntimeFileEntries,
      assertAuthenticatedRepositoryRuntimeClosure,
      expectedPublishedPackagePaths,
      assertExactPublishedPackagePaths,
      verifyPublishedRuntimeFileBytes,
    };`,
  )(
    compareUtf8,
    sha256Digest,
    (root, absolute) => path.relative(root, absolute).split(path.sep).join('/'),
    (value) => value.split('/'),
  );
}

function compilePublishedReleaseVerifier() {
  const source = sourceFile('scripts/package_blockkart_quickplay.mjs');
  const verifier = extract(
    source,
    'function assertPublishedObjectKeys(',
    'function lockEntryForPackagedModule(',
  );
  const packagedFileBytes = (file) => file.content != null
    ? Buffer.from(file.content, 'utf8')
    : Buffer.from(file.contentBase64, 'base64');
  return new Function(
    'QUICKPLAY_MAX_FILES',
    'QUICKPLAY_MAX_FILE_BYTES',
    'QUICKPLAY_MAX_METADATA_BYTES',
    'QUICKPLAY_MAX_SOURCE_PAYLOAD_BYTES',
    'compareUtf8',
    'artifactKey',
    'validatePortableComponent',
    'sha256Digest',
    'packagedFileBytes',
    'parsePackagedJsonObject',
    'fileText',
    'validateWebManifestVoModContract',
    `${verifier}\nreturn verifyPublishedReleaseManifest;`,
  )(
    100,
    256 * 1024 * 1024,
    16 * 1024 * 1024,
    64 * 1024 * 1024,
    compareUtf8,
    artifactKey,
    (value) => value,
    sha256Digest,
    packagedFileBytes,
    parseTestJsonObject,
    (files, filePath) => packagedFileBytes(files.find((file) => file.path === filePath)).toString('utf8'),
    (value, _voModSource, _label, options) => {
      assert.equal(value.module, options.expectedModule);
      assert.equal(value.version, options.expectedVersion);
      assert.equal(value.commit, options.expectedCommit);
      assert.equal(value.vo, options.expectedVo);
      return {
        manifest: value,
        metadata: {
          declaredArtifacts: value.artifacts.map((entry) => ({
            kind: entry.kind,
            target: entry.target,
            name: entry.name,
          })),
        },
      };
    },
  );
}

function compileDependencyWalker(fsBinding) {
  const source = sourceFile('scripts/package_blockkart_quickplay.mjs');
  const walker = extract(source, 'function sameFilesystemEntry(', 'function toPosixRelative(');
  const skip = extract(
    source,
    'function shouldSkipDependencyDirectory(',
    'async function walkProjectSourceFiles(',
  );
  return new Function(
    'fs',
    'path',
    'compareUtf8',
    'QUICKPLAY_MAX_WALK_ENTRIES',
    'QUICKPLAY_MAX_DEPTH',
    'QUICKPLAY_MAX_FILES',
    `${walker}\n${skip}\nreturn walkFiles;`,
  )(fsBinding, path, compareUtf8, 100, 8, 100);
}

function compileProjectSourceWalker(fsBinding) {
  const source = sourceFile('scripts/package_blockkart_quickplay.mjs');
  const helpers = extract(
    source,
    'function sameFilesystemEntry(',
    'async function walkFiles(',
  );
  const walker = extract(
    source,
    'async function walkProjectSourceFiles(',
    'function parseLockFile(',
  );
  return new Function(
    'fs',
    'path',
    'compareUtf8',
    'QUICKPLAY_MAX_WALK_ENTRIES',
    'QUICKPLAY_MAX_DEPTH',
    'QUICKPLAY_MAX_FILES',
    `${helpers}\n${walker}\nreturn walkProjectSourceFiles;`,
  )(fsBinding, path, compareUtf8, 100, 8, 100);
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
    assert.match(backend, /const folded = portableCaseKey\(component\)/);
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
  assert.throws(() => parseTarFiles(aliases), /conflicts with portable spelling/);

  const prefixAlias = tarArchive([
    { path: 'bundle/Dir/one.vo', body },
    { path: 'bundle/dir/two.vo', body },
  ]);
  assert.throws(() => parseTarFiles(prefixAlias), /conflicts with portable spelling/);
});

test('Quickplay runtime accepts only the canonical schema-v2 artifact layout', () => {
  const validateStaticArtifact = compileStaticArtifactValidator();
  const canonical = {
    kind: 'extension-wasm',
    target: 'wasm32-unknown-unknown',
    name: 'vogui.wasm',
    path: 'artifacts/extension-wasm/wasm32-unknown-unknown/vogui.wasm',
    url: '/quickplay/blockkart/artifacts/github.com%40vo-lang%40vogui/v0.1.15/extension-wasm/wasm32-unknown-unknown/vogui.wasm',
    size: 4,
    digest: `sha256:${'0'.repeat(64)}`,
  };
  assert.equal(
    validateStaticArtifact('github.com@vo-lang@vogui/v0.1.15', canonical),
    canonical.path,
  );
  assert.throws(
    () => validateStaticArtifact('github.com@vo-lang@vogui/v0.1.15', {
      ...canonical,
      path: 'artifacts/vogui.wasm',
    }),
    /path does not match/,
  );
  assert.throws(
    () => validateStaticArtifact('github.com@vo-lang@vogui/v0.1.15', {
      ...canonical,
      url: canonical.url.replaceAll('%40', '@'),
    }),
    /URL does not match/,
  );
});

test('Quickplay generator bounds artifact copying and emits schema v2 packages', () => {
  const source = sourceFile('scripts/package_blockkart_quickplay.mjs');
  const copier = extract(source, 'async function copyRegularFileLimited(', 'function chargePackageBytes(');
  assert.match(copier, /O_NOFOLLOW/);
  assert.match(copier, /O_EXCL/);
  assert.match(copier, /size > maxBytes/);
  assert.doesNotMatch(source, /fs\.copyFile\(sourcePath, stagedPath\)/);
  assert.match(source, /schemaVersion: 2,[\s\S]*name: 'BlockKart'/);
  assert.match(source, /schemaVersion: 2,[\s\S]*name: 'BlockKart dependencies'/);
});

test('Quickplay dependency packaging derives an exact authenticated runtime closure', () => {
  const helpers = compilePublishedDependencyClosure();
  const sourceBytes = new Map([
    ['assets/style.css', Buffer.from('body {}\n')],
    ['docs/readme.md', Buffer.from('build notes\n')],
    ['main.vo', Buffer.from('fn main() {}\n')],
    ['runtime/keep.js', Buffer.from('export const keep = true;\n')],
    ['runtime/renderer.js', Buffer.from('export const render = true;\n')],
    ['vo.lock', Buffer.from('version = 2\n')],
    ['vo.mod', Buffer.from('module github.com/acme/runtime\n')],
    ['web-artifacts/glue.js', Buffer.from('export default async function init() {}\n')],
  ]);
  const source = [...sourceBytes].map(([filePath, bytes]) => ({
    path: filePath,
    size: bytes.byteLength,
    digest: sha256Digest(bytes),
  })).sort((left, right) => compareUtf8(left.path, right.path));
  const wasmBytes = Buffer.from([0, 97, 115, 109, 1, 0, 0, 0]);
  const glue = source.find((entry) => entry.path === 'web-artifacts/glue.js');
  const contract = {
    manifest: {
      source,
      web: { entry: null, include: ['assets'] },
      extension: {
        name: 'runtime',
        include: ['runtime'],
        web: {
          entry: null,
          capabilities: [],
          js_modules: { renderer: 'runtime/renderer.js' },
        },
        wasm: {
          kind: 'Bindgen',
          wasm: 'runtime_bg.wasm',
          js_glue: 'glue.js',
          local_wasm: 'web-artifacts/runtime_bg.wasm',
          local_js_glue: 'web-artifacts/glue.js',
        },
      },
      artifacts: [
        {
          kind: 'extension-js-glue',
          target: 'wasm32-unknown-unknown',
          name: 'glue.js',
          path: glue.path,
          size: glue.size,
          digest: glue.digest,
        },
        {
          kind: 'extension-wasm',
          target: 'wasm32-unknown-unknown',
          name: 'runtime_bg.wasm',
          path: 'web-artifacts/runtime_bg.wasm',
          size: wasmBytes.byteLength,
          digest: sha256Digest(wasmBytes),
        },
      ],
    },
  };

  const runtimeEntries = helpers.publishedBrowserRuntimeFileEntries(contract, 'fixture');
  assert.deepEqual(runtimeEntries.map((entry) => entry.path), [
    'assets/style.css',
    'main.vo',
    'runtime/keep.js',
    'runtime/renderer.js',
    'vo.lock',
    'vo.mod',
    'web-artifacts/glue.js',
    'web-artifacts/runtime_bg.wasm',
  ]);
  assert.equal(
    runtimeEntries.find((entry) => entry.path === 'web-artifacts/runtime_bg.wasm').encoding,
    'base64',
  );
  assert(!runtimeEntries.some((entry) => entry.path === 'docs/readme.md'));
  const inventory = [...new Set([
    ...sourceBytes.keys(),
    'web-artifacts/runtime_bg.wasm',
    ...helpers.controlPaths,
  ])];
  assert.doesNotThrow(() => helpers.assertAuthenticatedRepositoryRuntimeClosure(
    inventory,
    runtimeEntries,
    contract,
    'fixture',
  ));
  assert.throws(
    () => helpers.assertAuthenticatedRepositoryRuntimeClosure(
      [...inventory, 'hidden.vo'],
      runtimeEntries,
      contract,
      'fixture',
    ),
    /unauthenticated=\[hidden\.vo\]/,
  );
  assert.throws(
    () => helpers.assertAuthenticatedRepositoryRuntimeClosure(
      [...inventory, 'assets/image.bin'],
      runtimeEntries,
      contract,
      'fixture',
    ),
    /unauthenticated=\[assets\/image\.bin\]/,
  );
  assert.throws(
    () => helpers.assertAuthenticatedRepositoryRuntimeClosure(
      inventory.filter((filePath) => filePath !== 'runtime/renderer.js'),
      runtimeEntries,
      contract,
      'fixture',
    ),
    /absent=\[runtime\/renderer\.js\]/,
  );

  const expected = helpers.expectedPublishedPackagePaths(runtimeEntries);
  const exactFiles = expected.map((filePath) => ({ path: filePath, content: '' }));
  assert.doesNotThrow(() => helpers.assertExactPublishedPackagePaths(exactFiles, expected, 'fixture'));
  assert.throws(
    () => helpers.assertExactPublishedPackagePaths(
      exactFiles.filter((file) => file.path !== 'runtime/renderer.js'),
      expected,
      'fixture',
    ),
    /missing=\[runtime\/renderer\.js\]/,
  );
  assert.throws(
    () => helpers.assertExactPublishedPackagePaths(
      [...exactFiles, { path: 'docs/readme.md', content: '' }],
      expected,
      'fixture',
    ),
    /unexpected=\[docs\/readme\.md\]/,
  );

  const voModEntry = runtimeEntries.find((entry) => entry.path === 'vo.mod');
  assert.doesNotThrow(() => helpers.verifyPublishedRuntimeFileBytes(
    voModEntry,
    sourceBytes.get('vo.mod'),
    'fixture vo.mod',
  ));
  assert.throws(
    () => helpers.verifyPublishedRuntimeFileBytes(
      { ...voModEntry, size: voModEntry.size + 1 },
      sourceBytes.get('vo.mod'),
      'fixture vo.mod',
    ),
    /size does not match/,
  );
  assert.throws(
    () => helpers.verifyPublishedRuntimeFileBytes(
      { ...voModEntry, digest: `sha256:${'0'.repeat(64)}` },
      sourceBytes.get('vo.mod'),
      'fixture vo.mod',
    ),
    /digest does not match/,
  );
});

test('Quickplay dependency release authentication rejects broken transitive bindings', () => {
  const verify = compilePublishedReleaseVerifier();
  const module = 'github.com/acme/runtime';
  const version = 'v1.2.3';
  const commit = 'a'.repeat(40);
  const sourceArchiveDigest = `sha256:${'b'.repeat(64)}`;
  const voModBytes = Buffer.from(`module ${module}\n\nvo ^0.1.0\n`);
  const source = [{
    path: 'vo.mod',
    size: voModBytes.byteLength,
    digest: sha256Digest(voModBytes),
  }];
  const sourceDigest = sha256Digest(Buffer.from(JSON.stringify(source), 'utf8'));
  const web = {
    schema_version: 1,
    module,
    version,
    commit,
    module_root: '.',
    vo: '^0.1.0',
    require: [],
    source_digest: sourceDigest,
    source,
    artifacts: [],
    web: null,
    extension: null,
  };
  const webContent = `${JSON.stringify(web, null, 2)}\n`;
  const release = {
    schema_version: 1,
    module,
    version,
    commit,
    module_root: '.',
    vo: '^0.1.0',
    require: [],
    source: {
      name: 'runtime-v1.2.3.tar.gz',
      size: 100,
      digest: sourceArchiveDigest,
      files_size: voModBytes.byteLength,
      files_digest: sourceDigest,
    },
    web_manifest: {
      size: Buffer.byteLength(webContent, 'utf8'),
      digest: sha256Digest(Buffer.from(webContent, 'utf8')),
    },
    artifacts: [],
  };
  const releaseContent = `${JSON.stringify(release, null, 2)}\n`;
  const locked = {
    path: module,
    version,
    commit,
    source: sourceArchiveDigest,
    release_manifest: sha256Digest(Buffer.from(releaseContent, 'utf8')),
    deps: [],
    artifacts: [],
  };
  const files = [
    { path: '.vo-source-digest', content: `${sourceArchiveDigest}\n` },
    { path: '.vo-version', content: `${version}\n` },
    { path: 'vo.mod', content: voModBytes.toString('utf8') },
    { path: 'vo.release.json', content: releaseContent },
    { path: 'vo.web.json', content: webContent },
  ];
  assert.doesNotThrow(() => verify(files, locked));

  assert.throws(
    () => verify(files.filter((file) => file.path !== 'vo.release.json'), locked),
    /missing vo\.release\.json/,
  );
  const changedWeb = files.map((file) => file.path === 'vo.web.json'
    ? { ...file, content: `${file.content}\n` }
    : file);
  assert.throws(() => verify(changedWeb, locked), /does not match vo\.release\.json/);

  const reboundRelease = structuredClone(release);
  reboundRelease.source.files_digest = `sha256:${'0'.repeat(64)}`;
  const reboundReleaseContent = `${JSON.stringify(reboundRelease, null, 2)}\n`;
  const reboundFiles = files.map((file) => file.path === 'vo.release.json'
    ? { ...file, content: reboundReleaseContent }
    : file);
  assert.throws(
    () => verify(reboundFiles, {
      ...locked,
      release_manifest: sha256Digest(Buffer.from(reboundReleaseContent, 'utf8')),
    }),
    /browser source set does not match/,
  );

  const changedMarker = files.map((file) => file.path === '.vo-source-digest'
    ? { ...file, content: `${`sha256:${'f'.repeat(64)}`}\n` }
    : file);
  assert.throws(() => verify(changedMarker, locked), /\.vo-source-digest does not match/);

  const duplicateReleaseContent = releaseContent.replace(
    '    "files_digest":',
    `    "files_digest": "sha256:${'0'.repeat(64)}",\n    "files_digest":`,
  );
  assert.notEqual(duplicateReleaseContent, releaseContent);
  const duplicateReleaseFiles = files.map((file) => file.path === 'vo.release.json'
    ? { ...file, content: duplicateReleaseContent }
    : file);
  assert.throws(
    () => verify(duplicateReleaseFiles, {
      ...locked,
      release_manifest: sha256Digest(Buffer.from(duplicateReleaseContent, 'utf8')),
    }),
    /duplicate object key "files_digest"/,
  );

  const duplicateWebContent = webContent.replace(
    '      "digest":',
    `      "digest": "sha256:${'0'.repeat(64)}",\n      "digest":`,
  );
  assert.notEqual(duplicateWebContent, webContent);
  const webBoundRelease = structuredClone(release);
  webBoundRelease.web_manifest = {
    size: Buffer.byteLength(duplicateWebContent, 'utf8'),
    digest: sha256Digest(Buffer.from(duplicateWebContent, 'utf8')),
  };
  const webBoundReleaseContent = `${JSON.stringify(webBoundRelease, null, 2)}\n`;
  const duplicateWebFiles = files.map((file) => {
    if (file.path === 'vo.release.json') return { ...file, content: webBoundReleaseContent };
    if (file.path === 'vo.web.json') return { ...file, content: duplicateWebContent };
    return file;
  });
  assert.throws(
    () => verify(duplicateWebFiles, {
      ...locked,
      release_manifest: sha256Digest(Buffer.from(webBoundReleaseContent, 'utf8')),
    }),
    /duplicate object key "digest"/,
  );
});

test('Quickplay dependency closure rejects unauthenticated metadata references', () => {
  const helpers = compilePublishedDependencyClosure();
  const bytes = Buffer.from('module github.com/acme/runtime\n');
  const base = {
    manifest: {
      source: [{ path: 'vo.mod', size: bytes.byteLength, digest: sha256Digest(bytes) }],
      web: { entry: null, include: [] },
      extension: {
        name: 'runtime',
        include: [],
        web: { entry: null, capabilities: [], js_modules: {} },
        wasm: null,
      },
      artifacts: [],
    },
  };
  const missingInclude = structuredClone(base);
  missingInclude.manifest.web.include = ['assets'];
  assert.throws(
    () => helpers.publishedBrowserRuntimeFileEntries(missingInclude, 'fixture'),
    /include path assets has no authenticated browser source files/,
  );

  const missingJs = structuredClone(base);
  missingJs.manifest.extension.web.js_modules.renderer = 'runtime/renderer.js';
  assert.throws(
    () => helpers.publishedBrowserRuntimeFileEntries(missingJs, 'fixture'),
    /browser JS module renderer declares absent source/,
  );

  const mismatchedLocalArtifact = structuredClone(base);
  mismatchedLocalArtifact.manifest.extension.wasm = {
    kind: 'Standalone',
    wasm: 'runtime.wasm',
    js_glue: null,
    local_wasm: 'web-artifacts/local.wasm',
    local_js_glue: null,
  };
  mismatchedLocalArtifact.manifest.artifacts = [{
    kind: 'extension-wasm',
    target: 'wasm32-unknown-unknown',
    name: 'runtime.wasm',
    path: 'web-artifacts/published.wasm',
    size: 8,
    digest: `sha256:${'1'.repeat(64)}`,
  }];
  assert.throws(
    () => helpers.publishedBrowserRuntimeFileEntries(mismatchedLocalArtifact, 'fixture'),
    /does not match artifact path/,
  );
});

test('Quickplay dependency walker rejects links and special entries explicitly', async () => {
  const directory = {
    dev: 1,
    ino: 1,
    mode: 0o40755,
    size: 64,
    mtimeMs: 1,
    ctimeMs: 1,
    isSymbolicLink: () => false,
    isDirectory: () => true,
    isFile: () => false,
  };
  const symbolicLink = {
    ...directory,
    mode: 0o120777,
    isSymbolicLink: () => true,
    isDirectory: () => false,
  };
  const special = {
    ...directory,
    mode: 0o140777,
    isDirectory: () => false,
  };
  const entry = (kind) => ({
    name: kind,
    isDirectory: () => kind === 'directory',
    isFile: () => kind === 'file',
    isSymbolicLink: () => kind === 'link',
  });
  const walkerFor = (kind) => compileDependencyWalker({
    lstat: async (current) => current === '/fixture'
      ? directory
      : kind === 'link'
        ? symbolicLink
        : special,
    readdir: async (current) => current === '/fixture' ? [entry(kind)] : [],
  });
  await assert.rejects(() => walkerFor('link')('/fixture'), /contains a symbolic link/);
  await assert.rejects(() => walkerFor('socket')('/fixture'), /special filesystem entry/);

  const rootLinkWalker = compileDependencyWalker({
    lstat: async () => symbolicLink,
    readdir: async () => [],
  });
  await assert.rejects(() => rootLinkWalker('/fixture'), /must be a real directory/);
});

test('Quickplay project source walker rejects links, special entries, and replaced directories', async () => {
  const directory = {
    dev: 1,
    ino: 1,
    mode: 0o40755,
    size: 64,
    mtimeMs: 1,
    ctimeMs: 1,
    isSymbolicLink: () => false,
    isDirectory: () => true,
    isFile: () => false,
  };
  const symbolicLink = {
    ...directory,
    mode: 0o120777,
    isSymbolicLink: () => true,
    isDirectory: () => false,
  };
  const special = {
    ...directory,
    mode: 0o140777,
    isDirectory: () => false,
  };
  const entry = (kind) => ({
    name: kind,
    isDirectory: () => kind === 'directory',
    isFile: () => kind === 'file',
    isSymbolicLink: () => kind === 'link',
  });
  const walkerFor = (kind) => compileProjectSourceWalker({
    lstat: async (current) => current === '/fixture'
      ? directory
      : kind === 'link'
        ? symbolicLink
        : special,
    readdir: async (current) => current === '/fixture' ? [entry(kind)] : [],
  });
  await assert.rejects(() => walkerFor('link')('/fixture'), /contains a symbolic link/);
  await assert.rejects(() => walkerFor('socket')('/fixture'), /special filesystem entry/);

  const rootLinkWalker = compileProjectSourceWalker({
    lstat: async () => symbolicLink,
    readdir: async () => [],
  });
  await assert.rejects(() => rootLinkWalker('/fixture'), /must be a real directory/);

  let childSnapshots = 0;
  const replacedDirectoryWalker = compileProjectSourceWalker({
    lstat: async (current) => {
      if (current !== path.join('/fixture', 'directory')) return directory;
      childSnapshots += 1;
      return childSnapshots === 1 ? directory : symbolicLink;
    },
    readdir: async (current) => current === '/fixture' ? [entry('directory')] : [],
  });
  await assert.rejects(
    () => replacedDirectoryWalker('/fixture'),
    /tree path must be a real directory/,
  );

  let rootSnapshots = 0;
  const mutatedSnapshotWalker = compileProjectSourceWalker({
    lstat: async () => ({
      ...directory,
      ino: rootSnapshots += 1,
    }),
    readdir: async () => [],
  });
  await assert.rejects(
    () => mutatedSnapshotWalker('/fixture'),
    /changed while being enumerated/,
  );
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

test('combined host bridges reject duplicate imports', () => {
  const { combineHostBridgeModules } = compileGuiPipelineHelpers();
  const bridge = combineHostBridgeModules([
    { buildImports: () => ({ host_measure_text: () => 1 }) },
    { buildImports: () => ({ host_measure_text: () => 2 }) },
  ]);
  assert.throws(() => bridge.buildImports({}), /Multiple host bridge modules define import/);
});

test('Quickplay lock rewrite commits a canonical v2 root graph', () => {
  const fixture = quickplayLockFixture([]);
  const rewrite = compileQuickplayLockRewrite();
  rewrite.rewriteProjectLockForPackagedDependencies(
    fixture.project,
    fixture.dependencies,
    fixture.original.resolved,
  );
  const lockSource = fixture.project.files.find((file) => file.path === 'vo.lock').content;
  const lock = parseVoLockV2(lockSource, 'test packaged vo.lock');
  const mod = parseVoModRootContract(fixture.modSource, 'test packaged vo.mod');
  validateVoLockV2RootGraph(lock, mod, 'test packaged vo.lock');
  validatePackagedModuleSet(lock, fixture.dependencies.modules, 'test dependencies');
  assert.equal(lock.version, 2);
  assert.deepEqual(lock.resolved[0].deps, []);
  assert.deepEqual(fixture.project.lockRewrite.modules[0].dependencies, []);
});

test('Quickplay lock rewrite failure leaves the packaged lock untouched', () => {
  const fixture = quickplayLockFixture([
    { module: 'github.com/acme/missing', constraint: '^0.1.0' },
  ]);
  const lockFile = fixture.project.files.find((file) => file.path === 'vo.lock');
  const before = lockFile.content;
  const rewrite = compileQuickplayLockRewrite();
  assert.throws(
    () => rewrite.rewriteProjectLockForPackagedDependencies(
      fixture.project,
      fixture.dependencies,
      fixture.original.resolved,
    ),
    /absent from vo\.lock/,
  );
  assert.equal(lockFile.content, before);
  assert.equal(fixture.project.lockRewrite, undefined);
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

function sha256Digest(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function quickplayLockFixture(requirements) {
  const module = 'github.com/acme/dependency';
  const version = 'v0.1.0';
  const commit = 'a'.repeat(40);
  const sourceDigest = `sha256:${'b'.repeat(64)}`;
  const webSource = `${JSON.stringify({ artifacts: [] }, null, 2)}\n`;
  const releaseSource = `${JSON.stringify({
    module,
    version,
    vo: '^0.1.0',
    commit,
    require: requirements,
    web_manifest: {
      size: Buffer.byteLength(webSource, 'utf8'),
      digest: sha256Digest(Buffer.from(webSource, 'utf8')),
    },
  }, null, 2)}\n`;
  const releaseDigest = sha256Digest(Buffer.from(releaseSource, 'utf8'));
  const modSource = [
    'module github.com/acme/application',
    '',
    'vo ^0.1.0',
    '',
    `require ${module} ^0.1.0`,
    '',
  ].join('\n');
  const lockSource = [
    'version = 1',
    'created_by = "test"',
    '',
    '[root]',
    'module = "github.com/acme/application"',
    'vo = "^0.1.0"',
    '',
    '[[resolved]]',
    `path = "${module}"`,
    `version = "${version}"`,
    'vo = "^0.1.0"',
    `commit = "${commit}"`,
    `release_manifest = "${releaseDigest}"`,
    `source = "${sourceDigest}"`,
    'deps = []',
    '',
  ].join('\n');
  const files = [
    { path: 'vo.release.json', content: releaseSource },
    { path: 'vo.web.json', content: webSource },
    { path: '.vo-source-digest', content: `${sourceDigest}\n` },
  ];
  return {
    modSource,
    original: parseVoLockForV2Migration(lockSource, 'test source vo.lock'),
    project: {
      files: [
        { path: 'vo.mod', content: modSource },
        { path: 'vo.lock', content: lockSource },
      ],
    },
    dependencies: {
      modules: [{ module, version, files, artifacts: [] }],
    },
  };
}
