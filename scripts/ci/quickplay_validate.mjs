#!/usr/bin/env node
import {
  existsSync,
  mkdirSync,
  readFileSync,
  readdirSync,
  writeFileSync,
} from 'node:fs';
import path from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import {
  QUICKPLAY_ARTIFACT_INPUTS,
  QUICKPLAY_GENERATOR_VERSION,
  SNAPSHOT_SCHEMA,
  assertQuickplay,
  outputFacts,
  quickplaySourceDigests,
  readJsonFile,
  validateQuickplayPackage,
} from './quickplay_vnext.mjs';

const ROOT = fileURLToPath(new URL('../..', import.meta.url));
const DEFAULT_PACKAGE_ROOT = path.join(ROOT, 'apps', 'studio', 'public', 'quickplay', 'blockkart');
const DEFAULT_REPORT_ROOT = path.join(ROOT, 'target', 'quickplay-validate');

function walkFiles(root, relative = '') {
  const current = relative === '' ? root : path.join(root, ...relative.split('/'));
  const files = [];
  for (const entry of readdirSync(current, { withFileTypes: true })) {
    const child = relative === '' ? entry.name : `${relative}/${entry.name}`;
    if (entry.isDirectory()) files.push(...walkFiles(root, child));
    else if (entry.isFile()) files.push(child);
    else throw new Error(`Quickplay package contains an unsupported entry: ${child}`);
  }
  return files;
}

function exactKeys(value, expected, label) {
  assertQuickplay(value && typeof value === 'object' && !Array.isArray(value), `${label} must be an object`);
  assertQuickplay(
    JSON.stringify(Object.keys(value).sort()) === JSON.stringify([...expected].sort()),
    `${label} has unexpected fields`,
  );
}

function validateProvenance(packageRoot) {
  const provenance = readJsonFile(path.join(packageRoot, 'provenance.json'), 'Quickplay provenance');
  exactKeys(
    provenance,
    ['schemaVersion', 'artifact', 'path', 'task', 'generator', 'toolchain', 'sourceDigests', 'inputs', 'outputs'],
    'Quickplay provenance',
  );
  assertQuickplay(provenance.schemaVersion === 2, 'Quickplay provenance schema mismatch');
  assertQuickplay(provenance.artifact === 'studio.quickplay.blockkart', 'Quickplay provenance artifact mismatch');
  assertQuickplay(provenance.path === 'apps/studio/public/quickplay/blockkart', 'Quickplay provenance path mismatch');
  assertQuickplay(
    JSON.stringify(provenance.inputs) === JSON.stringify(QUICKPLAY_ARTIFACT_INPUTS),
    'Quickplay provenance inputs differ from eng/artifacts.toml',
  );
  assertQuickplay(
    provenance.generator?.version === QUICKPLAY_GENERATOR_VERSION,
    'Quickplay provenance generator version mismatch',
  );
  const command = ['vo-dev', 'task', 'run', 'task:quickplay-blockkart-package'];
  assertQuickplay(JSON.stringify(provenance.generator?.command) === JSON.stringify(command), 'Quickplay provenance generator command mismatch');
  assertQuickplay(JSON.stringify(provenance.task?.command) === JSON.stringify(command), 'Quickplay provenance task command mismatch');
  assertQuickplay(provenance.task?.id === 'quickplay-blockkart-package', 'Quickplay provenance task id mismatch');
  exactKeys(provenance.task, ['id', 'command'], 'Quickplay provenance task');
  exactKeys(provenance.generator, ['version', 'command'], 'Quickplay provenance generator');
  exactKeys(provenance.toolchain, ['snapshotSchema', 'releaseSchema', 'packageSchema'], 'Quickplay provenance toolchain');
  assertQuickplay(
    provenance.toolchain.snapshotSchema === SNAPSHOT_SCHEMA
    && provenance.toolchain.releaseSchema === 2
    && provenance.toolchain.packageSchema === 1,
    'Quickplay provenance toolchain schemas are invalid',
  );
  const sourceDigests = provenance.sourceDigests;
  assertQuickplay(
    sourceDigests && typeof sourceDigests === 'object' && !Array.isArray(sourceDigests),
    'Quickplay provenance sourceDigests must be an object',
  );
  const expectedSourceDigests = [
    'volang',
    'github.com/vo-lang/blockkart',
    ...readJsonFile(path.join(packageRoot, 'deps.json'), 'Quickplay deps source roots', {
      maxBytes: 128 * 1024 * 1024,
    }).modules.map((module) => module.module),
  ].sort();
  assertQuickplay(
    JSON.stringify(Object.keys(sourceDigests).sort()) === JSON.stringify(expectedSourceDigests),
    'Quickplay provenance sourceDigests differ from the package graph',
  );
  const project = readJsonFile(path.join(packageRoot, 'project.json'), 'Quickplay provenance project', {
    maxBytes: 128 * 1024 * 1024,
  });
  const deps = readJsonFile(path.join(packageRoot, 'deps.json'), 'Quickplay provenance dependencies', {
    maxBytes: 128 * 1024 * 1024,
  });
  const currentSourceDigests = quickplaySourceDigests(project, deps, ROOT);
  assertQuickplay(
    JSON.stringify(sourceDigests) === JSON.stringify(currentSourceDigests),
    'Quickplay provenance source digests are stale',
  );
  const outputPaths = walkFiles(packageRoot).filter((relative) => relative !== 'provenance.json');
  const expected = outputFacts(packageRoot, outputPaths);
  assertQuickplay(JSON.stringify(provenance.outputs) === JSON.stringify(expected), 'Quickplay provenance output facts are stale');
  return provenance;
}

export function validateQuickplay(packageRoot = DEFAULT_PACKAGE_ROOT) {
  assertQuickplay(existsSync(packageRoot), `Quickplay package does not exist: ${packageRoot}`);
  const { project, deps } = validateQuickplayPackage(packageRoot);
  for (const module of deps.modules) {
    assertQuickplay(
      !module.files.some((file) => file.path === 'vo.web.json'),
      `${module.module} still contains removed vo.web.json metadata`,
    );
  }
  validateProvenance(packageRoot);
  return {
    projectFiles: project.files.length,
    modules: deps.modules.length,
    artifacts: deps.modules.reduce((count, module) => count + module.artifacts.length, 0),
    files: walkFiles(packageRoot).length,
  };
}

function main() {
  const packageRoot = process.argv[2] ? path.resolve(process.argv[2]) : DEFAULT_PACKAGE_ROOT;
  const result = validateQuickplay(packageRoot);
  if (packageRoot === DEFAULT_PACKAGE_ROOT) {
    mkdirSync(DEFAULT_REPORT_ROOT, { recursive: true });
    writeFileSync(
      path.join(DEFAULT_REPORT_ROOT, 'report.json'),
      `${JSON.stringify({ schemaVersion: 1, status: 'ok', ...result }, null, 2)}\n`,
    );
  }
  console.log(`Quickplay vNext validate: ok (${result.projectFiles} project files, ${result.modules} modules, ${result.artifacts} browser artifacts)`);
}

if (import.meta.url === pathToFileURL(process.argv[1] ?? '').href) {
  try {
    main();
  } catch (error) {
    console.error(error instanceof Error ? error.message : String(error));
    process.exitCode = 1;
  }
}
