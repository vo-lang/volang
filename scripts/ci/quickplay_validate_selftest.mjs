#!/usr/bin/env node
import { execFileSync, spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { requiredVpakProducerInputPaths } from './blockkart_vpak_policy.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const validator = path.join(root, 'scripts/ci/quickplay_validate.mjs');

function fail(message) {
  console.error(`quickplay validate selftest: ${message}`);
  process.exit(1);
}

function sha256Digest(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function jsonText(value) {
  return `${JSON.stringify(value, null, 2)}\n`;
}

function moduleFileBytes(file) {
  if (file.content != null) return Buffer.from(file.content, 'utf8');
  if (file.contentBase64 != null) return Buffer.from(file.contentBase64, 'base64');
  throw new Error(`file has no bytes: ${file.path}`);
}

function sourceSetDigest(entries) {
  return sha256Digest(Buffer.from(JSON.stringify(entries), 'utf8'));
}

function packagedFilesDigest(files) {
  return sourceSetDigest(files
    .map((file) => {
      const bytes = moduleFileBytes(file);
      return { digest: sha256Digest(bytes), path: file.path, size: bytes.byteLength };
    })
    .sort((a, b) => a.path.localeCompare(b.path)));
}

function sourceEntry(path, content) {
  const bytes = Buffer.from(content, 'utf8');
  return { digest: sha256Digest(bytes), path, size: bytes.byteLength };
}

function projectSourceEntry(path, content) {
  const bytes = Buffer.from(content, 'utf8');
  return { path, digest: sha256Digest(bytes), size: bytes.byteLength };
}

function repoFileEntry(repoRoot, relative) {
  const bytes = readFileSync(path.join(repoRoot, relative));
  return { path: relative, digest: sha256Digest(bytes), size: bytes.byteLength };
}

function repoFileEntries(repoRoot, relatives) {
  return relatives
    .map((relative) => repoFileEntry(repoRoot, relative))
    .sort((a, b) => a.path.localeCompare(b.path));
}

function writeFile(filePath, bytes) {
  mkdirSync(path.dirname(filePath), { recursive: true });
  writeFileSync(filePath, bytes);
}

function git(cwd, args) {
  execFileSync('git', args, { cwd, stdio: 'pipe' });
}

function initBlockKartRepo(repoRoot) {
  mkdirSync(repoRoot, { recursive: true });
  const asset = Buffer.alloc(1024 * 1024 + 17, 7);
  writeFile(path.join(repoRoot, 'main.vo'), 'package main\n\nfunc main() {}\n');
  writeFile(path.join(repoRoot, 'vo.mod'), 'module github.com/vo-lang/blockkart\nvo ^0.1.0\n\nrequire github.com/vo-lang/vogui v0.1.15\nrequire github.com/vo-lang/voplay v0.1.28\n');
  writeFile(path.join(repoRoot, 'assets/blockkart.vpak'), asset);
  writeFile(path.join(repoRoot, 'tools/pack_primitive_assets.vo'), 'package main\n\nfunc main() {}\n');
  writeFile(path.join(repoRoot, 'tools/vpak_provenance.mjs'), 'export const provenance = true;\n');
  writeFile(path.join(repoRoot, 'tools/generate_primitive_terrain.mjs'), 'export const generated = true;\n');
  writeFile(path.join(repoRoot, 'tools/paint_terrain_textures.mjs'), 'export const painted = true;\n');
  writeFile(path.join(repoRoot, 'tools/terrain_heightfield_spec.mjs'), 'export const heightmapSize = 8;\n');
  writeFile(path.join(repoRoot, 'tools/terrain_recipe.mjs'), 'export const recipe = {};\n');
  writeFile(path.join(repoRoot, 'terrain/recipes/primitive_concept_v1.json'), '{"name":"selftest"}\n');
  writeFile(path.join(repoRoot, 'docs/images/terrain-upgrade-concept-v1.png'), Buffer.from('concept'));
  for (const relative of [
    'assets/source/terrain_painted/grass_painted_v1.png',
    'assets/source/terrain_painted/meadow_painted_v1.png',
    'assets/source/terrain_painted/dirt_painted_v1.png',
    'assets/source/terrain_painted/rock_painted_v1.png',
    'assets/effects/grass_card_atlas.png',
    'assets/maps/primitive_track/blockkart.map.json',
    'assets/maps/primitive_track/lowpoly_terrain.glb',
    'assets/maps/primitive_track/lowpoly_terrain_lod.glb',
    'assets/maps/primitive_track/lowpoly_terrain_height_grid.bin',
    'assets/maps/primitive_track/terrain_splat_large.png',
  ]) {
    writeFile(path.join(repoRoot, relative), Buffer.from(`selftest ${relative}\n`, 'utf8'));
  }
  for (const relative of requiredVpakProducerInputPaths.filter((entry) => !entry.startsWith('workspace:'))) {
    if (!existsSync(path.join(repoRoot, relative))) {
      writeFile(path.join(repoRoot, relative), Buffer.from(`selftest ${relative}\n`, 'utf8'));
    }
  }
  git(repoRoot, ['init']);
  git(repoRoot, ['config', 'user.email', 'quickplay-selftest@example.invalid']);
  git(repoRoot, ['config', 'user.name', 'Quickplay Selftest']);
  return asset;
}

function buildModule({ module, version, commit, sourceFiles, sourceDigest, artifacts = [], release = true, extension = null }) {
  const sourceEntries = sourceFiles.map(([filePath, content]) => sourceEntry(filePath, content));
  const webManifest = {
    schema_version: 1,
    module,
    version,
    commit,
    module_root: '.',
    vo: '^0.1.0',
    require: [],
    source_digest: sourceSetDigest(sourceEntries),
    source: sourceEntries,
    web: null,
    extension,
    artifacts: artifacts.map((artifact) => ({
      digest: artifact.digest,
      kind: artifact.kind,
      name: artifact.name,
      path: artifact.path,
      size: artifact.size,
      target: artifact.target,
    })),
  };
  const files = [
    ...sourceFiles.map(([filePath, content]) => ({ path: filePath, content })),
    { path: 'vo.web.json', content: jsonText(webManifest) },
  ];
  if (release) {
    const releaseManifest = {
      schema_version: 1,
      module,
      version,
      commit,
      module_root: '.',
      vo: '^0.1.0',
      require: [],
      source: {
        name: `${module.split('/').pop()}-${version}.tar.gz`,
        size: 123,
        digest: sourceDigest,
      },
      artifacts: artifacts.map((artifact) => ({
        digest: artifact.digest,
        kind: artifact.kind,
        name: artifact.name,
        size: artifact.size,
        target: artifact.target,
      })),
    };
    files.push({ path: 'vo.release.json', content: jsonText(releaseManifest) });
    files.push({ path: '.vo-source-digest', content: `${sourceDigest}\n` });
    files.push({ path: '.vo-version', content: `${version}\n` });
  }
  files.sort((a, b) => a.path.localeCompare(b.path));
  return {
    module,
    version,
    cacheDir: module.replaceAll('/', '@') + `/${version}`,
    files,
    artifacts: artifacts.map((artifact) => ({ path: artifact.path, url: artifact.url })),
  };
}

function writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes) {
  const voplayJs = Buffer.from('export const island = true;\n', 'utf8');
  const voplayWasm = Buffer.from([0, 97, 115, 109, 1, 0, 0, 0]);
  const voplayJsArtifact = {
    kind: 'extension-js-glue',
    target: 'wasm32-unknown-unknown',
    name: 'voplay_island.js',
    path: 'artifacts/voplay_island.js',
    url: '/quickplay/blockkart/artifacts/github.com@vo-lang@voplay/v0.1.28/voplay_island.js',
    size: voplayJs.byteLength,
    digest: sha256Digest(voplayJs),
  };
  const voplayWasmArtifact = {
    kind: 'extension-wasm',
    target: 'wasm32-unknown-unknown',
    name: 'voplay_island_bg.wasm',
    path: 'artifacts/voplay_island_bg.wasm',
    url: '/quickplay/blockkart/artifacts/github.com@vo-lang@voplay/v0.1.28/voplay_island_bg.wasm',
    size: voplayWasm.byteLength,
    digest: sha256Digest(voplayWasm),
  };
  writeFile(path.join(tempQuickplay, 'artifacts/github.com@vo-lang@voplay/v0.1.28/voplay_island.js'), voplayJs);
  writeFile(path.join(tempQuickplay, 'artifacts/github.com@vo-lang@voplay/v0.1.28/voplay_island_bg.wasm'), voplayWasm);

  const voplayRenderer = [
    "import { wakeHostEvent } from './render_bootstrap.js';",
    'const hostTimers = new Map();',
    'const displayPulseWaiters = new Map();',
    "const ev = { key: 'host:1' };",
    'hostTimers.has(ev.key);',
    'displayPulseWaiters.set(ev.key, true);',
    'hostTimers.set(ev.key, true);',
    'wakeHostEvent(ev.key);',
    'const key = ev.key;',
    'const runtime = { vm: { wakeHostEvent(_key) {} } };',
    'runtime.vm.wakeHostEvent(key);',
    'class HostRuntime { constructor() { this.vm = runtime.vm; } wake(key) { this.vm.wakeHostEvent(key); } }',
    '',
  ].join('\n');
  const voplayBootstrap = 'export function wakeHostEvent(key) { return key; }\n';
  const voplayDts = 'export interface PendingHostEvent { key: string; }\nexport function wakeHostEvent(key: string): void;\n';

  const fakeSource = (label) => `sha256:${createHash('sha256').update(label).digest('hex')}`;
  const vogui = buildModule({
    module: 'github.com/vo-lang/vogui',
    version: 'v0.1.15',
    commit: 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
    sourceDigest: fakeSource('vogui-source'),
    sourceFiles: [['vo.mod', 'module github.com/vo-lang/vogui\nvo ^0.1.0\n']],
  });
  const vopack = buildModule({
    module: 'github.com/vo-lang/vopack',
    version: 'v0.1.2',
    commit: 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
    sourceDigest: fakeSource('vopack-source'),
    sourceFiles: [
      ['vo.mod', 'module github.com/vo-lang/vopack\nvo ^0.1.0\n'],
      ['mount.vo', 'package vopack\n\nfunc MountSelfTest() int { return 1 }\n'],
    ],
  });
  const voplay = buildModule({
    module: 'github.com/vo-lang/voplay',
    version: 'v0.1.28',
    commit: 'cccccccccccccccccccccccccccccccccccccccc',
    sourceDigest: fakeSource('voplay-source'),
    artifacts: [voplayJsArtifact, voplayWasmArtifact],
    extension: { web: { js_modules: { renderer: 'js/dist/voplay-render-island.js' } } },
    sourceFiles: [
      [
        'vo.mod',
        'module github.com/vo-lang/voplay\nvo ^0.1.0\n\n[extension]\nname = "voplay"\n\n[extension.wasm]\ntype = "bindgen"\nwasm = "voplay_island_bg.wasm"\njs_glue = "voplay_island.js"\n',
      ],
      ['js/dist/render_bootstrap.d.ts', voplayDts],
      ['js/dist/render_bootstrap.js', voplayBootstrap],
      ['js/dist/voplay-render-island.js', voplayRenderer],
    ],
  });

  const lock = `version = 1
created_by = "quickplay validate selftest"

[root]
module = "github.com/vo-lang/blockkart"
vo = "^0.1.0"

[[resolved]]
path = "github.com/vo-lang/vogui"
version = "v0.1.15"
vo = "^0.1.0"
commit = "${vogui.files.find((file) => file.path === 'vo.web.json').content.match(/"commit": "([^"]+)"/)[1]}"
release_manifest = "${sha256Digest(Buffer.from(vogui.files.find((file) => file.path === 'vo.release.json').content, 'utf8'))}"
source = "${vogui.files.find((file) => file.path === '.vo-source-digest').content.trim()}"
deps = []

[[resolved]]
path = "github.com/vo-lang/vopack"
version = "v0.1.2"
vo = "^0.1.0"
commit = "${vopack.files.find((file) => file.path === 'vo.web.json').content.match(/"commit": "([^"]+)"/)[1]}"
release_manifest = "${sha256Digest(Buffer.from(vopack.files.find((file) => file.path === 'vo.release.json').content, 'utf8'))}"
source = "${vopack.files.find((file) => file.path === '.vo-source-digest').content.trim()}"
deps = []

[[resolved]]
path = "github.com/vo-lang/voplay"
version = "v0.1.28"
vo = "^0.1.0"
commit = "${voplay.files.find((file) => file.path === 'vo.web.json').content.match(/"commit": "([^"]+)"/)[1]}"
release_manifest = "${sha256Digest(Buffer.from(voplay.files.find((file) => file.path === 'vo.release.json').content, 'utf8'))}"
source = "${voplay.files.find((file) => file.path === '.vo-source-digest').content.trim()}"
deps = ["github.com/vo-lang/vogui", "github.com/vo-lang/vopack"]

[[resolved.artifact]]
kind = "extension-js-glue"
target = "wasm32-unknown-unknown"
name = "voplay_island.js"
size = ${voplayJs.byteLength}
digest = "${sha256Digest(voplayJs)}"

[[resolved.artifact]]
kind = "extension-wasm"
target = "wasm32-unknown-unknown"
name = "voplay_island_bg.wasm"
size = ${voplayWasm.byteLength}
digest = "${sha256Digest(voplayWasm)}"
`;
  writeFile(path.join(blockKartRoot, 'vo.lock'), lock);
  const sourceAllowlist = [
    {
      path: 'tools/pack_primitive_assets.vo',
      reason: 'Asset-pack generation tool; quickplay runtime embeds the generated assets/blockkart.vpak payload.',
      expiresAt: '2027-01-31T00:00:00.000Z',
    },
  ];
  const projectSourceFiles = [
    projectSourceEntry('main.vo', readFileSync(path.join(blockKartRoot, 'main.vo'), 'utf8')),
    projectSourceEntry('tools/pack_primitive_assets.vo', readFileSync(path.join(blockKartRoot, 'tools/pack_primitive_assets.vo'), 'utf8')),
  ];
  const vpakInputs = [
    'tools/pack_primitive_assets.vo',
    'tools/vpak_provenance.mjs',
    'assets/maps/primitive_track/blockkart.map.json',
    'assets/maps/primitive_track/lowpoly_terrain.glb',
    'assets/maps/primitive_track/lowpoly_terrain_lod.glb',
    'assets/maps/primitive_track/lowpoly_terrain_height_grid.bin',
    'assets/maps/primitive_track/terrain_splat_large.png',
    'assets/effects/grass_card_atlas.png',
  ];
  const paintInputs = [
    'tools/paint_terrain_textures.mjs',
    'docs/images/terrain-upgrade-concept-v1.png',
  ];
  const paintOutputs = [
    'assets/source/terrain_painted/grass_painted_v1.png',
    'assets/source/terrain_painted/meadow_painted_v1.png',
    'assets/source/terrain_painted/dirt_painted_v1.png',
    'assets/source/terrain_painted/rock_painted_v1.png',
    'assets/effects/grass_card_atlas.png',
  ];
  const terrainInputs = [
    'tools/generate_primitive_terrain.mjs',
    'tools/terrain_heightfield_spec.mjs',
    'tools/terrain_recipe.mjs',
    'terrain/recipes/primitive_concept_v1.json',
    ...paintOutputs,
  ];
  const terrainOutputs = [
    'assets/maps/primitive_track/blockkart.map.json',
    'assets/maps/primitive_track/lowpoly_terrain.glb',
    'assets/maps/primitive_track/lowpoly_terrain_lod.glb',
    'assets/maps/primitive_track/lowpoly_terrain_height_grid.bin',
    'assets/maps/primitive_track/terrain_splat_large.png',
  ];
  const canonicalInputPaths = [...new Set([
    ...requiredVpakProducerInputPaths.filter((entry) => !entry.startsWith('workspace:')),
    ...vpakInputs,
    ...paintInputs,
    ...paintOutputs,
    ...terrainInputs,
    ...terrainOutputs,
  ])].sort();
  const canonicalInputs = repoFileEntries(blockKartRoot, canonicalInputPaths).map((entry) => ({
    path: entry.path,
    sha256: entry.digest.slice('sha256:'.length),
    size: entry.size,
  }));
  for (const workspacePath of [
    ...requiredVpakProducerInputPaths.filter((entry) => entry.startsWith('workspace:')),
    'workspace:github.com/vo-lang/voplay/scene3d/blockkart_pack.vo',
  ]) {
    canonicalInputs.push({
      path: workspacePath,
      sha256: createHash('sha256').update(`selftest ${workspacePath}`).digest('hex'),
      size: Buffer.byteLength(`selftest ${workspacePath}`),
    });
  }
  canonicalInputs.sort((a, b) => a.path.localeCompare(b.path));
  const archiveEntries = Array.from({ length: 37 }, (_, index) => ({
    path: `assets/selftest-${String(index).padStart(2, '0')}.bin`,
    kind: 'selftest',
    sourcePath: `assets/selftest-${String(index).padStart(2, '0')}.bin`,
    sourceSha256: createHash('sha256').update(`selftest-${index}`).digest('hex'),
    sourceSize: index + 1,
    contentHash: `crc32:${index.toString(16).padStart(8, '0')}`,
    dependencies: [],
    compression: 0,
    rawSize: index + 1,
    storedSize: index + 1,
    storedChecksum: index,
  }));
  const canonicalManifest = {
    schemaVersion: 1,
    kind: 'blockkart.vpakProducerManifest',
    owner: 'BlockKart',
    command: ['vo', 'run', 'tools/pack_primitive_assets.vo'],
    pack: {
      path: 'assets/blockkart.vpak',
      sha256: sha256Digest(assetBytes).slice('sha256:'.length),
      size: assetBytes.byteLength,
    },
    inputs: canonicalInputs,
    payloadInputCount: 37,
    archiveEntryCount: 37,
    workspaceSourceInputCount: 1,
    archiveEntries,
    internalManifest: { pack: 'BlockKart', version: 'selftest', assetCount: 37, sha256: createHash('sha256').update('selftest manifest').digest('hex') },
    upstream: [],
  };
  canonicalManifest.producerDigest = createHash('sha256').update(JSON.stringify(canonicalManifest)).digest('hex');
  const canonicalManifestBytes = Buffer.from(jsonText(canonicalManifest), 'utf8');
  writeFile(path.join(blockKartRoot, 'assets/blockkart.vpak.provenance.json'), canonicalManifestBytes);
  git(blockKartRoot, ['add', '.']);
  git(blockKartRoot, ['commit', '-m', 'selftest fixture']);
  const commit = execFileSync('git', ['rev-parse', 'HEAD'], { cwd: blockKartRoot, encoding: 'utf8' }).trim();

  const project = {
    schemaVersion: 1,
    name: 'BlockKart',
    module: 'github.com/vo-lang/blockkart',
    commit,
    dirty: false,
    sourceFiles: projectSourceFiles,
    sourceAllowlist,
    files: [
      { path: 'assets/blockkart.vpak', contentBase64: assetBytes.toString('base64') },
      { path: 'assets/blockkart.vpak.provenance.json', content: canonicalManifestBytes.toString('utf8') },
      { path: 'main.vo', content: readFileSync(path.join(blockKartRoot, 'main.vo'), 'utf8') },
      { path: 'vo.lock', content: lock },
      { path: 'vo.mod', content: readFileSync(path.join(blockKartRoot, 'vo.mod'), 'utf8') },
    ].sort((a, b) => a.path.localeCompare(b.path)),
  };
  const deps = {
    schemaVersion: 1,
    name: 'BlockKart dependencies',
    modules: [vogui, vopack, voplay].sort((a, b) => a.module.localeCompare(b.module)),
  };

  const projectBytes = Buffer.from(jsonText(project), 'utf8');
  const depsBytes = Buffer.from(jsonText(deps), 'utf8');
  writeFile(path.join(tempQuickplay, 'project.json'), projectBytes);
  writeFile(path.join(tempQuickplay, 'deps.json'), depsBytes);

  const provenance = {
    schemaVersion: 2,
    artifact: 'studio.quickplay.blockkart',
    path: 'apps/studio/public/quickplay/blockkart',
    task: {
      id: 'quickplay-blockkart-package',
      command: ['vo-dev', 'task', 'run', 'task:quickplay-blockkart-package'],
    },
    generator: {
      command: ['vo-dev', 'task', 'run', 'task:quickplay-blockkart-package'],
      script: 'apps/studio/scripts/package_blockkart_quickplay.mjs',
      version: 3,
    },
    toolchain: {
      node: `v${process.versions.node.split('.')[0]}`,
      voDevSourceDigest: sha256Digest(Buffer.from('quickplay-validate-selftest-generator')),
      wasmTarget: 'wasm32-unknown-unknown',
    },
    sourceRoots: {
      volang: '.',
      blockKart: 'external:BlockKart',
      voplay: 'first-party:voplay',
    },
    inputs: [
      'apps/studio/scripts/package_blockkart_quickplay.mjs',
      'scripts/ci/voplay_current_wasm.mjs',
      'eng/project.toml',
      'external:BlockKart',
      'external:BlockKart/tools/pack_primitive_assets.vo',
      'external:BlockKart/tools/generate_primitive_terrain.mjs',
      'external:BlockKart/tools/paint_terrain_textures.mjs',
      'external:BlockKart/tools/vpak_provenance.mjs',
      'external:BlockKart/tools/terrain_heightfield_spec.mjs',
      'external:BlockKart/tools/terrain_recipe.mjs',
      'external:BlockKart/terrain/recipes/primitive_concept_v1.json',
      'first-party:voplay',
      'module-cache:vopack',
      'module-cache:vogui',
    ],
    project: {
      commit,
      dirty: false,
      filesDigest: packagedFilesDigest(project.files),
      module: project.module,
      sourceFiles: projectSourceFiles,
      sourceAllowlist,
      sourceFilesDigest: sourceSetDigest(projectSourceFiles),
    },
    producers: [
      {
        id: 'blockkart-runtime-vpak',
        owner: 'BlockKart',
        kind: 'vpak',
        output: 'assets/blockkart.vpak',
        command: ['vo', 'run', 'tools/pack_primitive_assets.vo'],
        inputs: canonicalInputs.map((entry) => ({ path: entry.path, digest: `sha256:${entry.sha256}`, size: entry.size })),
        outputs: repoFileEntries(blockKartRoot, ['assets/blockkart.vpak']),
        producerManifest: {
          path: 'assets/blockkart.vpak.provenance.json',
          sha256: sha256Digest(canonicalManifestBytes),
          size: canonicalManifestBytes.byteLength,
          producerDigest: canonicalManifest.producerDigest,
        },
        archiveEntryCount: 37,
        payloadInputCount: 37,
        workspaceSourceInputCount: 1,
        archiveEntries,
        upstream: [
          {
            id: 'painted-terrain-textures',
            owner: 'BlockKart',
            kind: 'offline-texture-generation',
            command: ['node', 'tools/paint_terrain_textures.mjs'],
            inputs: repoFileEntries(blockKartRoot, paintInputs),
            outputs: repoFileEntries(blockKartRoot, paintOutputs),
          },
          {
            id: 'primitive-terrain-assets',
            owner: 'BlockKart',
            kind: 'offline-terrain-generation',
            command: ['node', 'tools/generate_primitive_terrain.mjs'],
            inputs: repoFileEntries(blockKartRoot, terrainInputs),
            outputs: repoFileEntries(blockKartRoot, terrainOutputs),
          },
        ],
      },
    ],
    dependencies: deps.modules.map((mod) => ({
      artifacts: (mod.artifacts ?? []).map((artifact) => {
        const localPath = path.join(tempQuickplay, artifact.url.slice('/quickplay/blockkart/'.length));
        const bytes = readFileSync(localPath);
        return { digest: sha256Digest(bytes), path: artifact.path, size: bytes.byteLength, url: artifact.url };
      }),
      cacheDir: mod.cacheDir,
      commit: mod.commit,
      dirty: false,
      filesDigest: packagedFilesDigest(mod.files),
      module: mod.module,
      source: 'module-cache',
      version: mod.version,
    })),
    outputs: [
      { digest: sha256Digest(projectBytes), path: 'project.json', size: projectBytes.byteLength },
      { digest: sha256Digest(depsBytes), path: 'deps.json', size: depsBytes.byteLength },
    ],
  };
  writeFile(path.join(tempQuickplay, 'provenance.json'), jsonText(provenance));
}

function runValidator(dir, blockKartRoot) {
  const blockKartExpectedCommit = execFileSync('git', ['rev-parse', 'HEAD'], {
    cwd: blockKartRoot,
    encoding: 'utf8',
  }).trim();
  return spawnSync(process.execPath, [validator], {
    cwd: root,
    env: {
      ...process.env,
      QUICKPLAY_DIR: dir,
      BLOCKKART_ROOT: blockKartRoot,
      BLOCKKART_EXPECTED_COMMIT: blockKartExpectedCommit,
      QUICKPLAY_VALIDATE_SELFTEST: '1',
    },
    encoding: 'utf8',
    maxBuffer: 20 * 1024 * 1024,
  });
}

const tempRoot = mkdtempSync(path.join(os.tmpdir(), 'quickplay-validate-selftest-'));
const tempQuickplay = path.join(tempRoot, 'quickplay-package');
const blockKartRoot = path.join(tempRoot, 'BlockKart');

try {
  const assetBytes = initBlockKartRepo(blockKartRoot);
  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);

  const baseline = runValidator(tempQuickplay, blockKartRoot);
  if (baseline.status !== 0) {
    fail(`baseline fixture must validate before drift mutation\n${baseline.stdout}${baseline.stderr}`);
  }

  const depsPath = path.join(tempQuickplay, 'deps.json');
  const deps = JSON.parse(readFileSync(depsPath, 'utf8'));
  const vopack = deps.modules.find((mod) => mod.module === 'github.com/vo-lang/vopack');
  const mount = vopack?.files?.find((file) => file.path === 'mount.vo');
  if (!mount || typeof mount.content !== 'string') {
    fail('could not find embedded vopack mount.vo to mutate');
  }
  mount.content += '\n// source drift self-test mutation\n';
  writeFileSync(depsPath, jsonText(deps));

  const drift = runValidator(tempQuickplay, blockKartRoot);
  if (drift.status === 0) {
    fail('validator accepted mutated embedded dependency source');
  }
  const output = `${drift.stdout}${drift.stderr}`;
  if (!/embedded source (size|digest) mismatch/.test(output)) {
    fail(`validator failed for the wrong reason\n${output}`);
  }
  console.log('quickplay validate selftest: ok');
} finally {
  rmSync(tempRoot, { recursive: true, force: true });
}
