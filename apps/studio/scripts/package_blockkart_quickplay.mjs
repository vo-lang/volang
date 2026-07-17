#!/usr/bin/env node
import {
  existsSync,
  lstatSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  realpathSync,
  rmSync,
  statSync,
  writeFileSync,
} from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { requireRepoRoot, requireVolangRoot } from '../../../scripts/ci/repo_roots.mjs';
import {
  BLOCKKART_VPAK_PATH,
  BLOCKKART_VPAK_PRODUCER_PATH,
  QUICKPLAY_ARTIFACT_INPUTS,
  QUICKPLAY_DEPS_SCHEMA,
  QUICKPLAY_GENERATOR_VERSION,
  QUICKPLAY_PROJECT_SCHEMA,
  SNAPSHOT_SCHEMA,
  assertSameBlockKartVpakProducerState,
  createVoBinaryAuthority,
  encodeStaticFile,
  filesOutsideNestedModules,
  gitHead,
  isPortableModuleManifestPath,
  listRegularFiles,
  outputFacts,
  observeBlockKartVpakProducer,
  packageSnapshotModule,
  portableProjectSnapshot,
  publicDependencyModule,
  quickplaySourceDigests,
  quickplayWorkspaceFile,
  readBoundedRegularFile,
  readBoundedRegularFileSnapshot,
  readJsonFile,
  recoverableQuickplayDirectory,
  selectModuleCacheRoot,
  sha256Digest,
  sourceInputDigest,
  validateQuickplayPackage,
  withCleanEffectiveSnapshot,
} from '../../../scripts/ci/quickplay_vnext.mjs';
import { verifyCurrentVoguiWasm } from '../../../scripts/ci/vogui_current_wasm.mjs';
import {
  cleanupDirectoryReplacementBackups,
  publishStagedDirectoryWithRollback,
  recoverInterruptedDirectoryReplacement,
  verifyCurrentVoplayWasm,
  withDirectoryReplacementLock,
} from '../../../scripts/ci/voplay_current_wasm.mjs';

const STUDIO_ROOT = path.resolve(fileURLToPath(new URL('..', import.meta.url)));
const VOLANG_ROOT = requireVolangRoot(path.resolve(STUDIO_ROOT, '../..'));
const BLOCKKART_ROOT = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const VOGUI_ROOT = requireRepoRoot('VOGUI_ROOT', 'vogui');
const VOPACK_ROOT = requireRepoRoot('VOPACK_ROOT', 'vopack');
const VOPLAY_ROOT = requireRepoRoot('VOPLAY_ROOT', 'voplay');
const WORKSPACE_ROOTS = Object.freeze([
  VOGUI_ROOT,
  VOPACK_ROOT,
  VOPLAY_ROOT,
]);
const SOURCE_REPOSITORIES = Object.freeze([
  VOLANG_ROOT,
  BLOCKKART_ROOT,
  ...WORKSPACE_ROOTS,
]);
const CACHE_ROOT = selectModuleCacheRoot(
  path.join(VOLANG_ROOT, 'target', 'quickplay-module-cache', 'mod'),
);
const DEFAULT_OUTPUT_ROOT = path.join(STUDIO_ROOT, 'public', 'quickplay', 'blockkart');
const OUTPUT_ROOT = path.resolve(process.env.BLOCKKART_QUICKPLAY_OUT_ROOT ?? DEFAULT_OUTPUT_ROOT);
const GENERATOR_COMMAND = Object.freeze([
  'vo-dev',
  'task',
  'run',
  'task:quickplay-blockkart-package',
]);
const VOPLAY_ARTIFACT_ROOT = path.join(VOLANG_ROOT, 'target', 'voplay-current-wasm');
const VOGUI_ARTIFACT_ROOT = path.join(VOLANG_ROOT, 'target', 'vogui-current-wasm');

function frozenArtifact(file, label) {
  const absolute = path.resolve(file);
  const bytes = readBoundedRegularFile(absolute, label);
  return {
    bytes,
    size: bytes.byteLength,
    digest: sha256Digest(bytes),
  };
}

function freezeVerifiedVoplayArtifacts(verification) {
  const outputs = new Map(verification.outputs.map((output) => [output.name, output]));
  const frozen = new Map();
  for (const name of ['voplay_island.js', 'voplay_island_bg.wasm']) {
    const expected = outputs.get(name);
    if (!expected || expected.missing || expected.invalid) {
      throw new Error(`verified voplay output is unavailable: ${name}`);
    }
    const artifact = frozenArtifact(path.join(VOPLAY_ARTIFACT_ROOT, name), `voplay artifact ${name}`);
    if (artifact.size !== expected.size || artifact.digest !== expected.digest) {
      throw new Error(`voplay artifact changed after producer verification: ${name}`);
    }
    frozen.set(name, artifact);
  }
  return frozen;
}

function freezeVerifiedVoguiArtifact(verification) {
  const expected = verification.outputs.find((output) => output.name === 'vogui.wasm');
  if (!expected || expected.missing || expected.invalid) {
    throw new Error('verified vogui output is unavailable: vogui.wasm');
  }
  const artifact = frozenArtifact(
    path.join(VOGUI_ARTIFACT_ROOT, 'vogui.wasm'),
    'vogui artifact vogui.wasm',
  );
  if (artifact.size !== expected.size || artifact.digest !== expected.digest) {
    throw new Error('vogui artifact changed after producer verification: vogui.wasm');
  }
  return artifact;
}

function workspaceArtifacts(module, frozenVoplayArtifacts, frozenVoguiArtifact) {
  if (module.module === 'github.com/vo-lang/vogui') {
    return [{
      kind: 'extension-wasm',
      target: 'wasm32-unknown-unknown',
      name: 'vogui.wasm',
      ...frozenVoguiArtifact,
      installPath: 'web-artifacts/vogui.wasm',
    }];
  }
  if (module.module === 'github.com/vo-lang/voplay') {
    return [
      {
        kind: 'extension-js-glue',
        target: 'wasm32-unknown-unknown',
        name: 'voplay_island.js',
        ...frozenVoplayArtifacts.get('voplay_island.js'),
        installPath: 'web-artifacts/voplay_island.js',
      },
      {
        kind: 'extension-wasm',
        target: 'wasm32-unknown-unknown',
        name: 'voplay_island_bg.wasm',
        ...frozenVoplayArtifacts.get('voplay_island_bg.wasm'),
        installPath: 'web-artifacts/voplay_island_bg.wasm',
      },
    ];
  }
  return [];
}

function workspaceFileIncluded(_module, relative) {
  return relative === 'vo.mod'
    || relative.endsWith('.vo')
    || relative.startsWith('js/dist/');
}

function currentSourceFileIncluded(index, relative) {
  if (index === 0) {
    return matchesProjectSourceInput(relative)
      || isPortableModuleManifestPath(relative);
  }
  return isPortableModuleManifestPath(relative)
    || relative.endsWith('.vo')
    || relative.startsWith('js/dist/');
}

function currentSourceIgnoredPathspecs(index) {
  if (index === 0) {
    return [
      `:(literal)${BLOCKKART_VPAK_PATH}`,
      `:(literal)${BLOCKKART_VPAK_PRODUCER_PATH}`,
    ];
  }
  return [':(glob)js/dist/**'];
}

function matchesProjectSourceInput(relative) {
  return matchesProjectControlFile(relative)
    || relative === BLOCKKART_VPAK_PATH
    || relative === BLOCKKART_VPAK_PRODUCER_PATH
    || relative.endsWith('.vo');
}

function matchesProjectControlFile(relative) {
  return relative === 'vo.mod' || relative === 'vo.lock' || relative === 'vo.work';
}

function projectFileIncluded(relative, lockAuthoritative) {
  if (relative === 'vo.mod') return true;
  if (relative === 'vo.lock') return lockAuthoritative;
  if (relative === BLOCKKART_VPAK_PATH || relative === BLOCKKART_VPAK_PRODUCER_PATH) {
    return true;
  }
  return relative.endsWith('.vo');
}

function collectProjectFiles(projectRoot, snapshot) {
  const lockAuthoritative = snapshot.authority === 'lock';
  const projectSourceFiles = filesOutsideNestedModules(
    listRegularFiles(projectRoot),
    'BlockKart project source closure',
  );
  const files = projectSourceFiles
    .filter((relative) => projectFileIncluded(relative, lockAuthoritative))
    .map((relative) => {
      const source = readBoundedRegularFileSnapshot(
        path.join(projectRoot, ...relative.split('/')),
        `BlockKart project file ${relative}`,
      );
      return encodeStaticFile(relative, source.bytes, source.mode);
    });
  const work = quickplayWorkspaceFile(snapshot.modules);
  if (work !== null) files.push(encodeStaticFile('vo.work', Buffer.from(work, 'utf8')));
  files.sort((left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path)));
  if (!files.some((file) => file.path === 'main.vo')) {
    throw new Error('BlockKart source snapshot is missing main.vo');
  }
  for (const relative of [BLOCKKART_VPAK_PATH, BLOCKKART_VPAK_PRODUCER_PATH]) {
    if (!files.some((file) => file.path === relative)) {
      throw new Error(`BlockKart source snapshot is missing ${relative}`);
    }
  }
  return files;
}

function writeJson(file, value) {
  writeFileSync(file, `${JSON.stringify(value, null, 2)}\n`, { flag: 'wx' });
}

function walkFiles(root, relative = '') {
  const current = relative === '' ? root : path.join(root, ...relative.split('/'));
  const output = [];
  for (const entry of readdirSync(current, { withFileTypes: true })) {
    const child = relative === '' ? entry.name : `${relative}/${entry.name}`;
    if (entry.isDirectory()) output.push(...walkFiles(root, child));
    else if (entry.isFile()) output.push(child);
    else throw new Error(`Quickplay output contains an unsupported entry: ${child}`);
  }
  return output;
}

function pathWithin(base, candidate) {
  const relative = path.relative(path.resolve(base), path.resolve(candidate));
  return relative !== '' && relative !== '..' && !relative.startsWith(`..${path.sep}`) && !path.isAbsolute(relative);
}

function validateOutputRoot(outputRoot) {
  if (path.basename(outputRoot) !== 'blockkart') {
    throw new Error(`Quickplay output must end in a blockkart directory: ${outputRoot}`);
  }
  if (outputRoot === path.resolve(DEFAULT_OUTPUT_ROOT)) return;
  const parent = path.dirname(outputRoot);
  if (!existsSync(parent)) mkdirSync(parent, { recursive: true });
  const canonicalOutput = path.join(realpathSync.native(parent), path.basename(outputRoot));
  const temporaryRoot = realpathSync.native(os.tmpdir());
  const targetRoot = realpathSync.native(path.join(VOLANG_ROOT, 'target'));
  if (!pathWithin(temporaryRoot, canonicalOutput) && !pathWithin(targetRoot, canonicalOutput)) {
    throw new Error(`Quickplay output is outside an approved generated-artifact root: ${outputRoot}`);
  }
}

function validateReplaceableOutput(outputRoot) {
  const metadata = lstatSync(outputRoot);
  if (!metadata.isDirectory() || metadata.isSymbolicLink()) {
    throw new Error(`Quickplay output must be a real directory: ${outputRoot}`);
  }
  const provenance = readJsonFile(path.join(outputRoot, 'provenance.json'), 'existing Quickplay provenance');
  if (
    provenance?.artifact !== 'studio.quickplay.blockkart'
    || provenance?.path !== 'apps/studio/public/quickplay/blockkart'
  ) {
    throw new Error(`refusing to replace a directory without BlockKart Quickplay identity: ${outputRoot}`);
  }
}

function captureRepositoryHeads() {
  return new Map(SOURCE_REPOSITORIES.map((repoRoot) => [repoRoot, gitHead(repoRoot)]));
}

function assertRepositoryHeads(expected) {
  for (const repoRoot of SOURCE_REPOSITORIES) {
    const current = gitHead(repoRoot);
    if (current !== expected.get(repoRoot)) {
      throw new Error(`source repository HEAD changed while Quickplay was packaged: ${repoRoot}`);
    }
  }
}

function verifyCurrentWasm(voAuthority) {
  const vogui = verifyCurrentVoguiWasm({
    voguiRoot: VOGUI_ROOT,
    volangRoot: VOLANG_ROOT,
    outDir: VOGUI_ARTIFACT_ROOT,
  });
  if (vogui.issues.length > 0) {
    throw new Error(`current vogui WASM is invalid: ${vogui.issues.join('; ')}`);
  }
  const voplay = verifyCurrentVoplayWasm({
    voplayRoot: VOPLAY_ROOT,
    volangRoot: VOLANG_ROOT,
    outDir: VOPLAY_ARTIFACT_ROOT,
    voAuthority,
  });
  if (voplay.issues.length > 0) {
    throw new Error(`current voplay WASM is invalid: ${voplay.issues.join('; ')}`);
  }
  return { vogui, voplay };
}

function verifyAndFreezeCurrentWasm(voAuthority) {
  const verified = verifyCurrentWasm(voAuthority);
  return {
    ...verified,
    frozenVoguiArtifact: freezeVerifiedVoguiArtifact(verified.vogui),
    frozenVoplayArtifacts: freezeVerifiedVoplayArtifacts(verified.voplay),
  };
}

function assertCurrentWasmInputsStable(expected, voAuthority) {
  const current = verifyCurrentWasm(voAuthority);
  for (const [label, left, right] of [
    ['vogui source', expected.vogui.source, current.vogui.source],
    ['vogui Volang inputs', expected.vogui.volangBuildInputs, current.vogui.volangBuildInputs],
    ['vogui outputs', expected.vogui.outputs, current.vogui.outputs],
    ['voplay source closure', expected.voplay.currentSourceClosure, current.voplay.currentSourceClosure],
    ['voplay Volang inputs', expected.voplay.volangBuildInputs, current.voplay.volangBuildInputs],
    ['voplay outputs', expected.voplay.outputs, current.voplay.outputs],
  ]) {
    if (JSON.stringify(left) !== JSON.stringify(right)) {
      throw new Error(`${label} changed while Quickplay was packaged`);
    }
  }
}

function buildPackage(stagingRoot) {
  const repositoryHeads = captureRepositoryHeads();
  const volangSourceDigest = sourceInputDigest(VOLANG_ROOT);
  const sourceVpak = observeBlockKartVpakProducer(
    BLOCKKART_ROOT,
    'BlockKart VPAK producer before Quickplay packaging',
  );
  const voAuthority = createVoBinaryAuthority({ root: VOLANG_ROOT });
  return withCleanEffectiveSnapshot({
    root: VOLANG_ROOT,
    projectRoot: BLOCKKART_ROOT,
    workspaceRoots: WORKSPACE_ROOTS,
    cacheRoot: CACHE_ROOT,
    allowVoBin: false,
    voAuthority,
    sourceFileIncluded: currentSourceFileIncluded,
    sourceIgnoredPathspecs: currentSourceIgnoredPathspecs,
    excludeNestedModules: true,
  }, ({ snapshot, projectRoot }) => {
    const stagedVpak = observeBlockKartVpakProducer(
      projectRoot,
      'staged BlockKart VPAK producer',
    );
    assertSameBlockKartVpakProducerState(
      sourceVpak,
      stagedVpak,
      'BlockKart VPAK producer from source to clean snapshot',
    );
    const verifiedWasm = verifyAndFreezeCurrentWasm(voAuthority);
    const portableSnapshot = portableProjectSnapshot(snapshot);
    const modules = snapshot.modules.map((module) => packageSnapshotModule(
      module,
      stagingRoot,
      {
        workspaceArtifacts: (snapshotModule) => workspaceArtifacts(
          snapshotModule,
          verifiedWasm.frozenVoplayArtifacts,
          verifiedWasm.frozenVoguiArtifact,
        ),
        workspaceFileIncluded,
      },
    ));
    for (const module of modules) {
      for (const artifact of module.artifacts) {
        mkdirSync(path.dirname(artifact.published), { recursive: true });
        writeFileSync(artifact.published, artifact.bytes, { flag: 'wx' });
      }
    }

    const project = {
      schemaVersion: QUICKPLAY_PROJECT_SCHEMA,
      name: 'BlockKart',
      module: 'github.com/vo-lang/blockkart',
      baseCommit: repositoryHeads.get(BLOCKKART_ROOT),
      snapshot: portableSnapshot,
      files: collectProjectFiles(projectRoot, snapshot),
    };
    const deps = {
      schemaVersion: QUICKPLAY_DEPS_SCHEMA,
      name: 'BlockKart dependencies',
      snapshotDigest: sha256Digest(Buffer.from(JSON.stringify(portableSnapshot), 'utf8')),
      modules: modules.map(publicDependencyModule),
    };
    writeJson(path.join(stagingRoot, 'project.json'), project);
    writeJson(path.join(stagingRoot, 'deps.json'), deps);

    const outputPaths = walkFiles(stagingRoot).filter((relative) => relative !== 'provenance.json');
    const provenance = {
      schemaVersion: 2,
      artifact: 'studio.quickplay.blockkart',
      path: 'apps/studio/public/quickplay/blockkart',
      task: { id: 'quickplay-blockkart-package', command: GENERATOR_COMMAND },
      generator: { version: QUICKPLAY_GENERATOR_VERSION, command: GENERATOR_COMMAND },
      toolchain: {
        snapshotSchema: SNAPSHOT_SCHEMA,
        releaseSchema: 2,
        packageSchema: 1,
      },
      sourceDigests: quickplaySourceDigests(project, deps, VOLANG_ROOT, {
        volangDigest: volangSourceDigest,
      }),
      inputs: [...QUICKPLAY_ARTIFACT_INPUTS],
      outputs: outputFacts(stagingRoot, outputPaths),
    };
    writeJson(path.join(stagingRoot, 'provenance.json'), provenance);
    validateQuickplayPackage(stagingRoot);
    assertCurrentWasmInputsStable(verifiedWasm, voAuthority);
    const stagedVpakAfter = observeBlockKartVpakProducer(
      projectRoot,
      'staged BlockKart VPAK producer after Quickplay packaging',
    );
    assertSameBlockKartVpakProducerState(
      stagedVpak,
      stagedVpakAfter,
      'staged BlockKart VPAK producer during Quickplay packaging',
    );
    const sourceVpakAfter = observeBlockKartVpakProducer(
      BLOCKKART_ROOT,
      'BlockKart VPAK producer after Quickplay packaging',
    );
    assertSameBlockKartVpakProducerState(
      sourceVpak,
      sourceVpakAfter,
      'BlockKart VPAK producer during Quickplay packaging',
    );
    if (sourceInputDigest(VOLANG_ROOT) !== volangSourceDigest) {
      throw new Error('Volang source inputs changed while Quickplay was packaged');
    }
    assertRepositoryHeads(repositoryHeads);
    return { project, deps, outputs: outputPaths.length + 1 };
  });
}

function main() {
  validateOutputRoot(OUTPUT_ROOT);
  return withDirectoryReplacementLock(OUTPUT_ROOT, packageBlockKartQuickplayLocked);
}

function packageBlockKartQuickplayLocked() {
  recoverInterruptedDirectoryReplacement(OUTPUT_ROOT, {
    completeDirectory: recoverableQuickplayDirectory,
    label: 'BlockKart Quickplay',
  });
  if (existsSync(OUTPUT_ROOT)) {
    validateReplaceableOutput(OUTPUT_ROOT);
    cleanupDirectoryReplacementBackups(OUTPUT_ROOT, {
      completeDirectory: recoverableQuickplayDirectory,
      label: 'BlockKart Quickplay',
    });
  }
  mkdirSync(path.dirname(OUTPUT_ROOT), { recursive: true });
  const stagingRoot = mkdtempSync(path.join(path.dirname(OUTPUT_ROOT), '.blockkart-vnext-'));
  let published = false;
  try {
    const result = buildPackage(stagingRoot);
    if (existsSync(OUTPUT_ROOT)) validateReplaceableOutput(OUTPUT_ROOT);
    publishStagedDirectoryWithRollback(stagingRoot, OUTPUT_ROOT, {
      label: 'BlockKart Quickplay',
    });
    cleanupDirectoryReplacementBackups(OUTPUT_ROOT, {
      completeDirectory: recoverableQuickplayDirectory,
      label: 'BlockKart Quickplay',
    });
    published = true;
    const bytes = walkFiles(OUTPUT_ROOT).reduce((total, relative) => (
      total + statSync(path.join(OUTPUT_ROOT, ...relative.split('/'))).size
    ), 0);
    console.log(`BlockKart Quickplay vNext: ${result.project.files.length} project files, ${result.deps.modules.length} modules, ${result.outputs} outputs, ${bytes} bytes`);
  } finally {
    if (!published) rmSync(stagingRoot, { recursive: true, force: true });
  }
}

try {
  main();
} catch (error) {
  console.error(error instanceof Error ? error.message : String(error));
  process.exitCode = 1;
}
