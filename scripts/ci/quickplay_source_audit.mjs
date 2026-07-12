#!/usr/bin/env node
import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { existsSync, mkdirSync, readFileSync, readdirSync, statSync, writeFileSync } from 'node:fs';
import { join, posix, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';
import { requiredVpakProducerInputPaths } from './blockkart_vpak_policy.mjs';
import { sourceBoundEvidence } from './source_bound_evidence.mjs';
import { verifyCurrentVoplayWasm } from './voplay_current_wasm.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const quickplayDir = resolve(process.env.QUICKPLAY_DIR ?? join(root, 'apps/studio/public/quickplay/blockkart'));
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const outDir = resolve(argValue('--out-dir') || process.env.QUICKPLAY_SOURCE_AUDIT_OUT_DIR || join(root, 'target/quickplay-source-audit'));
const voplayCurrentWasmRoot = resolve(process.env.VOPLAY_CURRENT_WASM_OUT_DIR ?? join(root, 'target/voplay-current-wasm'));

const dependencyRepos = new Map([
  ['github.com/vo-lang/vogui', requireRepoRoot('VOGUI_ROOT', 'vogui')],
  ['github.com/vo-lang/voplay', requireRepoRoot('VOPLAY_ROOT', 'voplay')],
  ['github.com/vo-lang/vopack', requireRepoRoot('VOPACK_ROOT', 'vopack')],
]);

function argValue(name) {
  const index = process.argv.indexOf(name);
  return index === -1 ? '' : (process.argv[index + 1] ?? '');
}

function sha256(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function readJson(path) {
  return JSON.parse(readFileSync(path, 'utf8'));
}

function fileBytes(file) {
  if (file?.content != null) return Buffer.from(file.content, 'utf8');
  if (file?.contentBase64 != null) return Buffer.from(file.contentBase64, 'base64');
  return null;
}

function lineInFile(filePath, needle) {
  if (!filePath || !existsSync(filePath)) return 1;
  const lines = readFileSync(filePath, 'utf8').split(/\r?\n/);
  const index = lines.findIndex((line) => line.includes(needle));
  return index === -1 ? 1 : index + 1;
}

function issue(issues, owner, subsystem, severity, message, evidence = {}) {
  const file = evidence.file ?? evidence.path ?? null;
  const line = evidence.line ?? (file ? lineInFile(resolve(root, file), evidence.needle ?? '') : 1);
  issues.push({
    owner,
    subsystem,
    severity,
    message,
    file,
    line,
    reason: evidence.reason ?? message,
    requiredFix: evidence.requiredFix ?? 'Update the current source/provenance so the gate can verify this contract from first-party evidence.',
    evidence,
  });
}

function git(args, cwd) {
  return execFileSync('git', args, { cwd, encoding: 'utf8', maxBuffer: 50 * 1024 * 1024 }).trim();
}

function tryGit(args, cwd) {
  try {
    return { ok: true, stdout: git(args, cwd) };
  } catch (error) {
    return {
      ok: false,
      error: String(error?.stderr || error?.message || error).trim(),
    };
  }
}

function tryGitBytes(args, cwd) {
  try {
    return { ok: true, bytes: execFileSync('git', args, { cwd, maxBuffer: 50 * 1024 * 1024 }) };
  } catch (error) {
    return {
      ok: false,
      error: String(error?.stderr || error?.message || error).trim(),
    };
  }
}

function parseVoLockValue(value) {
  if (value.startsWith('"') || value.startsWith('[')) {
    return JSON.parse(value);
  }
  if (/^\d+$/.test(value)) return Number(value);
  return value;
}

function parseVoLockResolved(source) {
  const resolved = [];
  let current = null;
  let artifact = null;
  for (const rawLine of source.split(/\r?\n/)) {
    const line = rawLine.trim();
    if (!line || line.startsWith('#')) continue;
    if (line === '[[resolved]]') {
      current = { artifacts: [] };
      artifact = null;
      resolved.push(current);
      continue;
    }
    if (line === '[[resolved.artifact]]') {
      if (!current) continue;
      artifact = {};
      current.artifacts.push(artifact);
      continue;
    }
    const pair = line.match(/^([A-Za-z0-9_]+)\s*=\s*(.+)$/);
    if (!pair || current == null) continue;
    const target = artifact ?? current;
    target[pair[1]] = parseVoLockValue(pair[2]);
  }
  return new Map(resolved.map((entry) => [entry.path, entry]));
}

function sourceSetDigest(entries) {
  return sha256(Buffer.from(JSON.stringify(entries), 'utf8'));
}

function packagedFilesDigest(files) {
  const entries = (files ?? [])
    .map((file) => {
      const bytes = fileBytes(file);
      return {
        digest: sha256(bytes),
        path: file.path,
        size: bytes.byteLength,
      };
    })
    .sort((a, b) => a.path.localeCompare(b.path));
  return sourceSetDigest(entries);
}

function validatePackagedLockRewrite(project, sourceBytes, packagedBytes, issues) {
  const rewrite = project.lockRewrite;
  if (!rewrite || rewrite.schemaVersion !== 1 || rewrite.path !== 'vo.lock') {
    issue(issues, 'BlockKart', 'LockRewrite', 'P0', 'Packaged vo.lock differs from source without a structured lockRewrite contract', {
      expected: 'project.lockRewrite schemaVersion=1 path=vo.lock',
    });
    return false;
  }
  const sourceDigest = sha256(sourceBytes);
  const packagedDigest = sha256(packagedBytes);
  let ok = true;
  if (rewrite.sourceDigest !== sourceDigest) {
    issue(issues, 'BlockKart', 'LockRewrite', 'P0', 'lockRewrite sourceDigest does not match source vo.lock', { expected: sourceDigest, found: rewrite.sourceDigest ?? null });
    ok = false;
  }
  if (rewrite.packagedDigest !== packagedDigest) {
    issue(issues, 'BlockKart', 'LockRewrite', 'P0', 'lockRewrite packagedDigest does not match packaged vo.lock', { expected: packagedDigest, found: rewrite.packagedDigest ?? null });
    ok = false;
  }
  if (!Array.isArray(rewrite.modules) || rewrite.modules.length === 0) {
    issue(issues, 'BlockKart', 'LockRewrite', 'P0', 'lockRewrite must list rewritten dependency modules', { modules: rewrite.modules ?? null });
    ok = false;
  }
  return ok;
}

function listSourceFiles(projectRoot, extension) {
  const files = [];
  const visit = (dir) => {
    for (const entry of readdirSync(dir)) {
      const file = join(dir, entry);
      const stat = statSync(file);
      if (stat.isDirectory()) {
        if (entry === '.git' || entry === 'target' || entry === 'node_modules' || entry === 'tmp_checks') continue;
        visit(file);
      } else if (file.endsWith(extension)) {
        const bytes = readFileSync(file);
        files.push({
          path: posix.normalize(file.slice(projectRoot.length + 1).split(/[\\/]/).join('/')),
          digest: sha256(bytes),
          size: bytes.byteLength,
        });
      }
    }
  };
  visit(projectRoot);
  return files.sort((a, b) => a.path.localeCompare(b.path));
}

function shouldValidateEmbeddedSource(file) {
  if (file.content == null) return false;
  return !['.vo-source-digest', '.vo-version', 'vo.release.json', 'vo.web.json'].includes(file.path);
}

function dropNativeExtensionTables(voModSource) {
  const lines = voModSource.split(/\r?\n/);
  const out = [];
  let skip = false;
  for (const line of lines) {
    const table = line.trim().match(/^\[\[?([^\]]+)\]\]?$/);
    if (table) {
      const name = table[1].trim();
      skip = name === 'extension.native' || name.startsWith('extension.native.');
    }
    if (!skip) {
      out.push(line);
    }
  }
  return `${out.join('\n').replace(/\n{3,}/g, '\n\n').trimEnd()}\n`;
}

function repoSourceBytesForAudit(mod, files, entry, sourceBytes) {
  if (entry.path !== 'vo.mod') {
    return sourceBytes;
  }
  const packaged = files.get('vo.mod');
  if (!packaged?.content) {
    return sourceBytes;
  }
  const rewritten = Buffer.from(dropNativeExtensionTables(sourceBytes.toString('utf8')), 'utf8');
  const packagedBytes = Buffer.from(packaged.content, 'utf8');
  if (rewritten.byteLength === packagedBytes.byteLength && sha256(rewritten) === sha256(packagedBytes)) {
    return rewritten;
  }
  return sourceBytes;
}

const requiredVpakProducerOutput = 'assets/blockkart.vpak';
const requiredVpakProducerManifest = 'assets/blockkart.vpak.provenance.json';
const requiredTerrainProducerInputs = [
  'tools/generate_primitive_terrain.mjs',
  'tools/terrain_heightfield_spec.mjs',
  'tools/terrain_recipe.mjs',
  'terrain/recipes/primitive_concept_v1.json',
  'assets/source/terrain_painted/grass_painted_v1.png',
  'assets/source/terrain_painted/meadow_painted_v1.png',
  'assets/source/terrain_painted/dirt_painted_v1.png',
  'assets/source/terrain_painted/rock_painted_v1.png',
  'assets/effects/grass_card_atlas.png',
];
const requiredTerrainProducerOutputs = [
  'assets/maps/primitive_track/lowpoly_terrain.glb',
  'assets/maps/primitive_track/lowpoly_terrain_lod.glb',
  'assets/maps/primitive_track/lowpoly_terrain_height_grid.bin',
  'assets/maps/primitive_track/terrain_splat_large.png',
];
const requiredPaintProducerInputs = [
  'tools/paint_terrain_textures.mjs',
  'docs/images/terrain-upgrade-concept-v1.png',
];
const requiredPaintProducerOutputs = [
  'assets/source/terrain_painted/grass_painted_v1.png',
  'assets/source/terrain_painted/meadow_painted_v1.png',
  'assets/source/terrain_painted/dirt_painted_v1.png',
  'assets/source/terrain_painted/rock_painted_v1.png',
  'assets/effects/grass_card_atlas.png',
];

function entryMap(entries) {
  return new Map((entries ?? []).map((entry) => [entry.path, entry]));
}

function sourceDigestEntry(relative) {
  const absolute = join(blockKartRoot, relative);
  if (!existsSync(absolute)) return { path: relative, missing: true };
  const bytes = readFileSync(absolute);
  return { path: relative, digest: sha256(bytes), size: bytes.byteLength };
}

function validateDigestEntries(entries, requiredPaths, issues, owner, subsystem, producerLabel, requiredFix) {
  const entriesByPath = entryMap(entries);
  for (const requiredPath of requiredPaths) {
    const actual = entriesByPath.get(requiredPath);
    if (!actual) {
      issue(issues, owner, subsystem, 'P0', `${producerLabel} missing producer digest entry for ${requiredPath}`, {
        file: 'apps/studio/public/quickplay/blockkart/provenance.json',
        needle: '"producers"',
        requiredPath,
        requiredFix,
      });
      continue;
    }
    if (requiredPath.startsWith('workspace:')) {
      continue;
    }
    const expected = sourceDigestEntry(requiredPath);
    if (expected.missing || actual.digest !== expected.digest || actual.size !== expected.size) {
      issue(issues, owner, subsystem, 'P0', `${producerLabel} producer digest entry is stale for ${requiredPath}`, {
        file: 'apps/studio/public/quickplay/blockkart/provenance.json',
        needle: requiredPath,
        requiredPath,
        expected,
        found: actual,
        requiredFix,
      });
    }
  }
}

function validateBlockKartProducerProvenance(project, provenance, packaged, issues) {
  try {
    execFileSync(process.execPath, ['tools/vpak_provenance.mjs', '--check'], {
      cwd: blockKartRoot,
      env: process.env,
      stdio: 'pipe',
    });
  } catch (error) {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'Current BlockKart vpak failed canonical manifest verification', {
      file: 'assets/blockkart.vpak.provenance.json',
      needle: '"producerDigest"',
      detail: String(error?.stderr || error?.message || error),
      requiredFix: 'Rebuild assets/blockkart.vpak and its canonical producer manifest before quickplay packaging.',
    });
  }
  const packagedVpak = packaged.get(requiredVpakProducerOutput);
  const packagedVpakBytes = fileBytes(packagedVpak);
  if (!packagedVpakBytes) {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'Quickplay package must include assets/blockkart.vpak bytes', {
      file: 'apps/studio/public/quickplay/blockkart/project.json',
      needle: requiredVpakProducerOutput,
      requiredFix: 'Regenerate the quickplay package with the BlockKart runtime vpak embedded.',
    });
    return;
  }
  const packagedManifestBytes = fileBytes(packaged.get(requiredVpakProducerManifest));
  const currentManifestPath = join(blockKartRoot, requiredVpakProducerManifest);
  const currentManifestBytes = existsSync(currentManifestPath) ? readFileSync(currentManifestPath) : null;
  let canonicalManifest = null;
  if (!packagedManifestBytes || !currentManifestBytes) {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'Quickplay package must include the canonical vpak producer manifest', {
      file: 'apps/studio/public/quickplay/blockkart/project.json',
      needle: requiredVpakProducerManifest,
      requiredFix: 'Rebuild assets/blockkart.vpak and package its canonical producer manifest sidecar.',
    });
  } else if (sha256(packagedManifestBytes) !== sha256(currentManifestBytes)) {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'Packaged vpak producer manifest differs from current BlockKart source', {
      file: 'apps/studio/public/quickplay/blockkart/project.json',
      needle: requiredVpakProducerManifest,
      requiredFix: 'Regenerate quickplay from the current canonical vpak producer manifest.',
    });
  } else {
    canonicalManifest = JSON.parse(currentManifestBytes.toString('utf8'));
  }

  const producer = (provenance.producers ?? []).find((entry) => entry?.output === requiredVpakProducerOutput);
  if (!producer) {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'assets/blockkart.vpak must declare first-party producer provenance', {
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      needle: '"outputs"',
      requiredFix: 'Regenerate quickplay provenance with a producer record for tools/pack_primitive_assets.vo, terrain generation inputs, output digests, and toolchain command.',
    });
    return;
  }

  if (producer.owner !== 'BlockKart' || producer.kind !== 'vpak') {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'assets/blockkart.vpak producer must name the BlockKart vpak owner contract', {
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      needle: requiredVpakProducerOutput,
      producer,
      requiredFix: 'Set producer.owner=BlockKart and producer.kind=vpak for the runtime asset pack producer.',
    });
  }
  if (!Array.isArray(producer.command) || !producer.command.includes('tools/pack_primitive_assets.vo')) {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'assets/blockkart.vpak producer command must name tools/pack_primitive_assets.vo', {
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      needle: requiredVpakProducerOutput,
      command: producer.command ?? null,
      requiredFix: 'Record the pack command that generated assets/blockkart.vpak.',
    });
  }
  validateDigestEntries(
    producer.inputs,
    requiredVpakProducerInputPaths,
    issues,
    'BlockKart',
    'ProducerProvenance',
    'assets/blockkart.vpak',
    'Record current source digests for the vpak pack script and map manifest inputs.',
  );
  validateDigestEntries(
    producer.outputs,
    [requiredVpakProducerOutput],
    issues,
    'BlockKart',
    'ProducerProvenance',
    'assets/blockkart.vpak',
    'Record the current output digest for assets/blockkart.vpak.',
  );
  const producerOutput = entryMap(producer.outputs).get(requiredVpakProducerOutput);
  if (producerOutput?.digest !== sha256(packagedVpakBytes) || producerOutput?.size !== packagedVpakBytes.byteLength) {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'assets/blockkart.vpak producer output must match packaged bytes', {
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      needle: requiredVpakProducerOutput,
      expected: { digest: sha256(packagedVpakBytes), size: packagedVpakBytes.byteLength },
      found: producerOutput ?? null,
      requiredFix: 'Regenerate quickplay provenance from the current packaged vpak bytes.',
    });
  }
  if (canonicalManifest) {
    const expectedInputs = canonicalManifest.inputs
      .map((entry) => ({ path: entry.path, digest: `sha256:${entry.sha256}`, size: entry.size }))
      .sort((a, b) => a.path.localeCompare(b.path));
    const foundInputs = [...(producer.inputs ?? [])].sort((a, b) => a.path.localeCompare(b.path));
    const manifestFact = producer.producerManifest;
    const manifestMatches = manifestFact?.path === requiredVpakProducerManifest
      && manifestFact?.sha256 === sha256(currentManifestBytes)
      && manifestFact?.size === currentManifestBytes.byteLength
      && manifestFact?.producerDigest === canonicalManifest.producerDigest;
    if (JSON.stringify(foundInputs) !== JSON.stringify(expectedInputs)
      || producer.archiveEntryCount !== canonicalManifest.archiveEntryCount
      || producer.payloadInputCount !== canonicalManifest.payloadInputCount
      || producer.workspaceSourceInputCount !== canonicalManifest.workspaceSourceInputCount
      || JSON.stringify(producer.archiveEntries) !== JSON.stringify(canonicalManifest.archiveEntries)
      || !manifestMatches) {
      issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'Quickplay vpak producer record is not the canonical 37-entry manifest', {
        file: 'apps/studio/public/quickplay/blockkart/provenance.json',
        needle: '"archiveEntries"',
        expectedArchiveEntryCount: canonicalManifest.archiveEntryCount,
        foundArchiveEntryCount: producer.archiveEntryCount ?? null,
        manifestMatches,
        requiredFix: 'Regenerate quickplay after rebuilding and checking the BlockKart canonical vpak producer manifest.',
      });
    }
  }

  const upstream = Array.isArray(producer.upstream) ? producer.upstream : [];
  const terrainProducer = upstream.find((entry) => entry?.id === 'primitive-terrain-assets');
  const paintProducer = upstream.find((entry) => entry?.id === 'painted-terrain-textures');
  if (!terrainProducer) {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'assets/blockkart.vpak provenance must include primitive terrain generator lineage', {
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      needle: requiredVpakProducerOutput,
      requiredFix: 'Record the tools/generate_primitive_terrain.mjs producer with terrain inputs and generated output digests.',
    });
  } else {
    validateDigestEntries(
      terrainProducer.inputs,
      requiredTerrainProducerInputs,
      issues,
      'BlockKart',
      'ProducerProvenance',
      'primitive-terrain-assets',
      'Record current terrain generator input digests.',
    );
    validateDigestEntries(
      terrainProducer.outputs,
      requiredTerrainProducerOutputs,
      issues,
      'BlockKart',
      'ProducerProvenance',
      'primitive-terrain-assets',
      'Record current generated terrain output digests.',
    );
  }
  if (!paintProducer) {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'assets/blockkart.vpak provenance must include painted terrain texture lineage', {
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      needle: requiredVpakProducerOutput,
      requiredFix: 'Record the tools/paint_terrain_textures.mjs producer with source concept and baked texture output digests.',
    });
  } else {
    validateDigestEntries(
      paintProducer.inputs,
      requiredPaintProducerInputs,
      issues,
      'BlockKart',
      'ProducerProvenance',
      'painted-terrain-textures',
      'Record current painted texture producer input digests.',
    );
    validateDigestEntries(
      paintProducer.outputs,
      requiredPaintProducerOutputs,
      issues,
      'BlockKart',
      'ProducerProvenance',
      'painted-terrain-textures',
      'Record current painted texture output digests.',
    );
  }
}

function validateVoplayWasmProducerProvenance(provenance, issues) {
  const voplayRoot = dependencyRepos.get('github.com/vo-lang/voplay');
  const verification = verifyCurrentVoplayWasm({
    voplayRoot,
    outDir: voplayCurrentWasmRoot,
    expectedCiRunId: process.env.VO_DEV_CI_RUN_ID ?? null,
  });
  if (verification.issues.length > 0) {
    issue(issues, 'voplay/rust', 'ProducerProvenance', 'P0', 'Current-source voplay WASM producer evidence is stale', {
      file: 'target/voplay-current-wasm/producer-manifest.json',
      freshnessIssues: verification.issues,
      requiredFix: 'Rebuild voplay-current-wasm from the current Rust source in the same task run.',
    });
    return;
  }
  const producer = (provenance.producers ?? []).find((entry) => entry?.id === 'voplay-current-source-wasm');
  if (!producer) {
    issue(issues, 'voplay/rust', 'ProducerProvenance', 'P0', 'Quickplay provenance must include the current-source voplay WASM producer', {
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      needle: '"producers"',
      requiredFix: 'Regenerate quickplay with the voplay-current-wasm-build producer output.',
    });
    return;
  }
  if (JSON.stringify(producer.command) !== JSON.stringify(verification.manifest.command)
      || JSON.stringify(producer.source) !== JSON.stringify(verification.manifest.source)
      || JSON.stringify(producer.outputs) !== JSON.stringify(verification.manifest.outputs)) {
    issue(issues, 'voplay/rust', 'ProducerProvenance', 'P0', 'Quickplay voplay WASM producer record does not match the current build manifest', {
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      needle: 'voplay-current-source-wasm',
      expected: verification.manifest,
      found: producer,
      requiredFix: 'Regenerate quickplay after rebuilding voplay-current-wasm in the same task run.',
    });
  }
  const dependency = (provenance.dependencies ?? []).find((entry) => entry?.module === 'github.com/vo-lang/voplay');
  const artifactByName = new Map((dependency?.artifacts ?? []).map((artifact) => [posix.basename(artifact.path), artifact]));
  for (const output of verification.manifest.outputs) {
    const packaged = artifactByName.get(output.name);
    if (!packaged || packaged.digest !== output.digest || packaged.size !== output.size) {
      issue(issues, 'voplay/rust', 'ProducerProvenance', 'P0', `Packaged ${output.name} does not match current-source WASM output`, {
        file: 'apps/studio/public/quickplay/blockkart/provenance.json',
        needle: output.name,
        expected: output,
        found: packaged ?? null,
        requiredFix: 'Regenerate quickplay from target/voplay-current-wasm.',
      });
    }
  }
}

function auditBlockKart(project, provenance, issues) {
  if (!existsSync(blockKartRoot)) {
    issue(issues, 'BlockKart', 'Source', 'P0', 'BlockKart source checkout is missing', { blockKartRoot });
    return;
  }
  if (provenance?.schemaVersion !== 2) {
    issue(issues, 'BlockKart', 'Provenance', 'P0', 'Quickplay provenance must use schema v2', { schemaVersion: provenance?.schemaVersion ?? null });
  }
  if (provenance?.project?.commit && provenance.project.commit !== project.commit) {
    issue(issues, 'BlockKart', 'Provenance', 'P0', 'Project provenance commit does not match project package', { expected: project.commit, found: provenance.project.commit });
  }
  if (provenance?.project?.filesDigest && provenance.project.filesDigest !== packagedFilesDigest(project.files)) {
    issue(issues, 'BlockKart', 'Provenance', 'P0', 'Project provenance filesDigest does not match project package files', { expected: packagedFilesDigest(project.files), found: provenance.project.filesDigest });
  }
  if (JSON.stringify(provenance?.project?.lockRewrite ?? null) !== JSON.stringify(project.lockRewrite ?? null)) {
    issue(issues, 'BlockKart', 'Provenance', 'P0', 'Project provenance lockRewrite must match project package lockRewrite', {
      provenance: provenance?.project?.lockRewrite ?? null,
      project: project.lockRewrite ?? null,
    });
  }
  const inside = tryGit(['rev-parse', '--is-inside-work-tree'], blockKartRoot);
  if (!inside.ok || inside.stdout !== 'true') {
    issue(issues, 'BlockKart', 'Source', 'P0', 'BlockKart source path is not a git checkout', { blockKartRoot, error: inside.error });
    return;
  }
  const head = git(['rev-parse', 'HEAD'], blockKartRoot);
  if (head !== project.commit) {
    issue(issues, 'BlockKart', 'Source', 'P0', 'BlockKart source HEAD does not match packaged commit', { expected: project.commit, found: head });
  }
  const status = git(['status', '--porcelain'], blockKartRoot);
  const dirty = status !== '';
  if (dirty) {
    issue(issues, 'BlockKart', 'Source', 'P0', 'BlockKart source checkout must be clean for strict quickplay source audit', { status });
  }
  if (dirty && provenance?.project?.dirty !== true) {
    issue(issues, 'BlockKart', 'Source', 'P0', 'BlockKart source checkout is dirty but provenance project.dirty is not true', { status, provenanceDirty: provenance?.project?.dirty ?? null });
  }
  if (!dirty && provenance?.project?.dirty === true) {
    issue(issues, 'BlockKart', 'Source', 'P0', 'BlockKart provenance project.dirty is true but source checkout is clean');
  }
  if (provenance?.project?.dirty === true) {
    issue(issues, 'BlockKart', 'Provenance', 'P0', 'BlockKart provenance project.dirty must be false for strict source audit');
  }

  const packaged = new Map((project.files ?? []).map((file) => [file.path, file]));
  validateBlockKartProducerProvenance(project, provenance, packaged, issues);
  const sourceFiles = listSourceFiles(blockKartRoot, '.vo');
  const packagedVo = new Set([...packaged.keys()].filter((file) => file.endsWith('.vo')));
  const sourceAllowlist = new Map((provenance?.project?.sourceAllowlist ?? project.sourceAllowlist ?? [])
    .map((entry) => [entry.path, entry]));
  const unpackaged = sourceFiles
    .filter((entry) => !packagedVo.has(entry.path))
    .filter((entry) => !sourceAllowlist.has(entry.path));
  if (unpackaged.length > 0) {
    issue(issues, 'BlockKart', 'SourceCoverage', 'P0', 'BlockKart source files are missing from quickplay package and have no allowlist reason', { unpackaged });
  }
  const invalidAllowlist = [...sourceAllowlist.values()].filter((entry) => (
    !entry?.path
    || typeof entry.reason !== 'string'
    || entry.reason.trim().length < 12
    || typeof entry.expiresAt !== 'string'
    || Number.isNaN(Date.parse(entry.expiresAt))
    || Date.parse(entry.expiresAt) <= Date.now()
    || !sourceFiles.some((source) => source.path === entry.path)
  ));
  if (invalidAllowlist.length > 0) {
    issue(issues, 'BlockKart', 'SourceCoverage', 'P0', 'BlockKart quickplay source allowlist entries must name an existing source file and include a reason plus future expiresAt', { invalidAllowlist });
  }
  const manifestSourceFiles = project.sourceFiles ?? provenance?.project?.sourceFiles ?? null;
  if (!Array.isArray(manifestSourceFiles)) {
    issue(issues, 'BlockKart', 'SourceCoverage', 'P0', 'Quickplay package manifest must record BlockKart source file digest list', { expected: 'project.sourceFiles[] or provenance.project.sourceFiles[]' });
  } else if (sourceSetDigest(manifestSourceFiles) !== sourceSetDigest(sourceFiles)) {
    issue(issues, 'BlockKart', 'SourceCoverage', 'P0', 'Quickplay package source digest list does not match BlockKart/**/*.vo', {
      expected: sourceSetDigest(sourceFiles),
      found: sourceSetDigest(manifestSourceFiles),
    });
  }
  if (provenance?.project?.sourceFilesDigest !== sourceSetDigest(sourceFiles)) {
    issue(issues, 'BlockKart', 'SourceCoverage', 'P0', 'Quickplay provenance sourceFilesDigest does not match BlockKart/**/*.vo', {
      expected: sourceSetDigest(sourceFiles),
      found: provenance?.project?.sourceFilesDigest ?? null,
    });
  }
  for (const [path, file] of packaged) {
    const sourcePath = join(blockKartRoot, path);
    if (!existsSync(sourcePath)) {
      issue(issues, 'BlockKart', 'Source', 'P0', 'Packaged BlockKart file is missing from source checkout', { path });
      continue;
    }
    const source = readFileSync(sourcePath);
    const packagedBytes = fileBytes(file);
    if (!packagedBytes) {
      issue(issues, 'BlockKart', 'Source', 'P0', 'Packaged BlockKart file has no bytes', { path });
      continue;
    }
    if (source.byteLength !== packagedBytes.byteLength || sha256(source) !== sha256(packagedBytes)) {
      if (path === 'vo.lock' && validatePackagedLockRewrite(project, source, packagedBytes, issues)) {
        continue;
      }
      issue(issues, 'BlockKart', 'Source', 'P0', 'Packaged BlockKart file differs from source checkout', {
        path,
        source: { digest: sha256(source), size: source.byteLength },
        packaged: { digest: sha256(packagedBytes), size: packagedBytes.byteLength },
      });
    }
  }
}

function auditDependencyRelease(mod, locked, provenanceDependency, issues) {
  const dependencyDirty = provenanceDependency?.dirty === true;
  if (dependencyDirty) {
    issue(issues, mod.module, 'Provenance', 'P0', 'Dependency provenance dirty flag must be false for strict source audit', { module: mod.module });
  }
  const files = new Map((mod.files ?? []).map((file) => [file.path, file]));
  const releaseFile = files.get('vo.release.json');
  const webFile = files.get('vo.web.json');
  if (!webFile) {
    issue(issues, mod.module, 'Release', 'P0', 'Dependency package is missing vo.web.json', { module: mod.module, version: mod.version });
    return;
  }
  let web;
  try {
    web = JSON.parse(webFile.content);
  } catch (error) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.web.json is invalid JSON', { error: error.message });
    return;
  }

  if (web.module !== mod.module) issue(issues, mod.module, 'Release', 'P0', 'vo.web.json module mismatch', { expected: mod.module, found: web.module });
  if (web.version !== mod.version) issue(issues, mod.module, 'Release', 'P0', 'vo.web.json version mismatch', { expected: mod.version, found: web.version });
  if (locked?.commit && web.commit !== locked.commit) issue(issues, mod.module, 'Release', 'P0', 'vo.web.json commit mismatch', { expected: locked.commit, found: web.commit });
  if (!Array.isArray(web.source)) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.web.json source is not an array');
  } else if (sourceSetDigest(web.source) !== web.source_digest) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.web.json source_digest mismatch', { expected: web.source_digest, found: sourceSetDigest(web.source) });
  }

  if (releaseFile) {
    let release = null;
    try {
      release = JSON.parse(releaseFile.content);
    } catch (error) {
      issue(issues, mod.module, 'Release', 'P0', 'vo.release.json is invalid JSON', { error: error.message });
    }
    const releaseDigest = sha256(fileBytes(releaseFile));
    if (locked?.release_manifest && releaseDigest !== locked.release_manifest) {
      issue(issues, mod.module, 'Release', 'P0', 'vo.release.json digest does not match project vo.lock', { expected: locked.release_manifest, found: releaseDigest });
    }
    if (release) {
      if (release.module !== mod.module) issue(issues, mod.module, 'Release', 'P0', 'vo.release.json module mismatch', { expected: mod.module, found: release.module });
      if (release.version !== mod.version) issue(issues, mod.module, 'Release', 'P0', 'vo.release.json version mismatch', { expected: mod.version, found: release.version });
      if (locked?.commit && release.commit !== locked.commit) issue(issues, mod.module, 'Release', 'P0', 'vo.release.json commit mismatch', { expected: locked.commit, found: release.commit });
      if (locked?.source && release.source?.digest !== locked.source) {
        issue(issues, mod.module, 'Release', 'P0', 'vo.release.json source digest does not match project vo.lock', { expected: locked.source, found: release.source?.digest });
      }
      const marker = files.get('.vo-source-digest')?.content?.trim() ?? '';
      if (locked?.source && marker !== locked.source) {
        issue(issues, mod.module, 'Release', 'P0', '.vo-source-digest does not match project vo.lock', { expected: locked.source, found: marker });
      }
    }
  }

  const sourceByPath = new Map((web.source ?? []).map((entry) => [entry.path, entry]));
  for (const file of mod.files ?? []) {
    if (!shouldValidateEmbeddedSource(file)) continue;
    const entry = sourceByPath.get(file.path);
    if (!entry) {
      issue(issues, mod.module, 'Release', 'P0', 'Embedded source file is not declared by vo.web.json', { path: file.path });
      continue;
    }
    const bytes = fileBytes(file);
    const digest = sha256(bytes);
    if (bytes.byteLength !== entry.size || digest !== entry.digest) {
      issue(issues, mod.module, 'Release', 'P0', 'Embedded source file differs from vo.web.json source entry', {
        path: file.path,
        expected: { digest: entry.digest, size: entry.size },
        found: { digest, size: bytes.byteLength },
      });
    }
  }
}

function auditDependencyRepoCommit(mod, locked, provenanceDependency, issues) {
  if (!locked?.commit) return;
  const repoRoot = dependencyRepos.get(mod.module);
  if (!repoRoot || !existsSync(repoRoot)) {
    issue(issues, mod.module, 'SourceRepo', 'P1', 'Dependency source repo checkout is missing', { repoRoot });
    return;
  }
  const head = tryGit(['rev-parse', 'HEAD'], repoRoot);
  const dirty = provenanceDependency?.dirty === true;
  if (dirty) {
    if (!head.ok || head.stdout !== locked.commit) {
      issue(issues, mod.module, 'SourceRepo', 'P0', 'Dirty dependency source repo HEAD does not match locked commit', { expected: locked.commit, found: head.ok ? head.stdout : null, repoRoot, error: head.error });
      return;
    }
  }
  const commitType = tryGit(['cat-file', '-t', locked.commit], repoRoot);
  if (!commitType.ok || commitType.stdout !== 'commit') {
    issue(issues, mod.module, 'SourceRepo', 'P1', 'Dependency source repo does not contain locked commit', { commit: locked.commit, repoRoot, error: commitType.error });
    return;
  }
  const files = new Map((mod.files ?? []).map((file) => [file.path, file]));
  const webFile = files.get('vo.web.json');
  if (!webFile) return;
  let web;
  try {
    web = JSON.parse(webFile.content);
  } catch {
    return;
  }
  for (const entry of web.source ?? []) {
    const workingTreePath = join(repoRoot, entry.path);
    const source = dirty
      ? (existsSync(workingTreePath)
        ? { ok: true, bytes: readFileSync(workingTreePath) }
        : { ok: false, error: `missing working-tree source ${entry.path}` })
      : tryGitBytes(['show', `${locked.commit}:${entry.path}`], repoRoot);
    if (!source.ok) {
      issue(issues, mod.module, 'SourceRepo', 'P0', dirty
        ? 'Dirty dependency working tree is missing packaged source file'
        : 'Locked dependency commit is missing release source file', { commit: locked.commit, path: entry.path, error: source.error });
      continue;
    }
    const sourceBytes = source.bytes;
    const expectedBytes = repoSourceBytesForAudit(mod, files, entry, sourceBytes);
    if (expectedBytes.byteLength !== entry.size || sha256(expectedBytes) !== entry.digest) {
      issue(issues, mod.module, 'SourceRepo', 'P0', dirty
        ? 'Dirty dependency working-tree source differs from vo.web.json source entry'
        : 'Locked dependency commit source differs from vo.web.json source entry', {
        commit: locked.commit,
        path: entry.path,
        expected: { digest: entry.digest, size: entry.size },
        found: { digest: sha256(expectedBytes), size: expectedBytes.byteLength },
      });
    }
  }
}

function markdown(report) {
  const lines = ['# Quickplay Source Audit', '', `- Status: ${report.status}`, `- Issues: ${report.issues.length}`, `- Quickplay dir: ${report.quickplayDir}`, `- BlockKart root: ${report.blockKartRoot}`, ''];
  if (report.issues.length > 0) {
    lines.push('## Issues', '');
    for (const item of report.issues) {
      lines.push(`- ${item.severity} ${item.owner}/${item.subsystem}: ${item.message}`);
      const evidence = JSON.stringify(item.evidence ?? {});
      if (evidence !== '{}') lines.push(`  - evidence: \`${evidence}\``);
    }
    lines.push('');
  }
  return `${lines.join('\n')}\n`;
}

const project = readJson(join(quickplayDir, 'project.json'));
const deps = readJson(join(quickplayDir, 'deps.json'));
const provenance = readJson(join(quickplayDir, 'provenance.json'));
const issues = [];
auditBlockKart(project, provenance, issues);
validateVoplayWasmProducerProvenance(provenance, issues);
const projectFiles = new Map((project.files ?? []).map((file) => [file.path, file]));
const locked = parseVoLockResolved(fileBytes(projectFiles.get('vo.lock')).toString('utf8'));
const provenanceDependencies = new Map((provenance.dependencies ?? []).map((entry) => [entry.module, entry]));
for (const mod of deps.modules ?? []) {
  const lockedModule = locked.get(mod.module);
  if (!lockedModule) {
    issue(issues, mod.module, 'Lock', 'P0', 'Dependency module is missing from project vo.lock', { version: mod.version });
    continue;
  }
  const provenanceDependency = provenanceDependencies.get(mod.module);
  auditDependencyRelease(mod, lockedModule, provenanceDependency, issues);
  auditDependencyRepoCommit(mod, lockedModule, provenanceDependency, issues);
}

const generatedAt = new Date().toISOString();
const freshEvidence = sourceBoundEvidence({
  gate: 'quickplay-source-audit',
  generatedAt,
  root,
  repos: [
    { name: 'volang', root },
    { name: 'BlockKart', root: blockKartRoot },
    ...[...dependencyRepos.entries()].map(([module, repoRoot]) => ({ name: module, root: repoRoot })),
  ],
  gateFiles: [
    'scripts/ci/quickplay_source_audit.mjs',
    'scripts/ci/blockkart_vpak_policy.mjs',
    'scripts/ci/repo_roots.mjs',
    'scripts/ci/source_bound_evidence.mjs',
    'apps/studio/scripts/package_blockkart_quickplay.mjs',
    'eng/tasks.toml',
    'eng/artifacts.toml',
    'eng/project.toml',
  ],
  artifacts: [quickplayDir],
});
if (freshEvidence.verdict.status !== 'pass') {
  issue(issues, 'Volang', 'FreshEvidence', 'P0', 'quickplay source audit freshEvidence verdict must pass', {
    file: 'scripts/ci/quickplay_source_audit.mjs',
    needle: 'sourceBoundEvidence',
    reason: 'Current source-bound report was produced from dirty or missing-commit repositories.',
    dirtyRepos: freshEvidence.verdict.dirtyRepos,
    missingCommitRepos: freshEvidence.verdict.missingCommitRepos,
    requiredFix: 'Regenerate the report from clean checked-out sources after all quickplay/provenance changes are committed.',
  });
}
const report = {
  schemaVersion: 1,
  kind: 'quickplay.sourceAuditReport',
  status: issues.length === 0 ? 'ok' : 'failed',
  generatedAt,
  freshEvidence,
  quickplayDir,
  blockKartRoot,
  checkedAt: generatedAt,
  sourceFiles: listSourceFiles(blockKartRoot, '.vo'),
  packagedSources: (project.files ?? [])
    .filter((file) => file.path.endsWith('.vo'))
    .map((file) => {
      const bytes = fileBytes(file);
      return {
        path: file.path,
        digest: sha256(bytes),
        size: bytes.byteLength,
      };
    })
    .sort((a, b) => a.path.localeCompare(b.path)),
  packageManifest: {
    sourceFiles: project.sourceFiles ?? null,
    sourceAllowlist: project.sourceAllowlist ?? provenance.project?.sourceAllowlist ?? [],
  },
  issues,
};

mkdirSync(outDir, { recursive: true });
writeFileSync(join(outDir, 'quickplay-source-audit.json'), `${JSON.stringify(report, null, 2)}\n`);
writeFileSync(join(outDir, 'quickplay-source-audit.md'), markdown(report));

if (issues.length > 0) {
  for (const item of issues) {
    console.error(JSON.stringify({
      owner: item.owner,
      subsystem: item.subsystem,
      severity: item.severity,
      message: item.message,
      evidence: item.evidence ?? null,
    }));
  }
  console.error(`quickplay source audit: failed with ${issues.length} issue(s); wrote ${outDir}`);
  process.exit(1);
}
console.log(`quickplay source audit: ok; wrote ${outDir}`);
