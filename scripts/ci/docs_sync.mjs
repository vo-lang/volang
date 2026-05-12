import { createHash } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { promises as fs } from 'node:fs';
import os from 'node:os';
import path from 'node:path';

const root = path.resolve(new URL('../..', import.meta.url).pathname);
const checkOnly = process.argv.includes('--check');

const docs = [
  { id: 'vo-for-gophers', source: 'lang/docs/vo-for-gophers.md', output: 'vo-for-gophers.md', title: 'Vo for Go Programmers' },
  { id: 'language', source: 'lang/docs/spec/language.md', output: 'spec/language.md', title: 'Language Spec' },
  { id: 'dynamic', source: 'lang/docs/spec/dynamic.md', output: 'spec/dynamic.md', title: 'Dynamic Semantics' },
  { id: 'module', source: 'lang/docs/spec/module.md', output: 'spec/module.md', title: 'Module System' },
  { id: 'native-ffi', source: 'lang/docs/spec/native-ffi.md', output: 'spec/native-ffi.md', title: 'Native FFI' },
  { id: 'memory-model', source: 'lang/docs/spec/memory-model-and-instructions.md', output: 'spec/memory-model-and-instructions.md', title: 'Memory Model' },
  { id: 'vm-bytecode', source: 'lang/docs/spec/vm-bytecode.md', output: 'spec/vm-bytecode.md', title: 'VM Bytecode' },
  { id: 'vm-jit', source: 'lang/docs/spec/vm-jit-design.md', output: 'spec/vm-jit-design.md', title: 'JIT Design' },
];
const artifactPath = 'apps/playground-legacy/src/assets/docs/generated';
const artifactInputs = [
  'scripts/ci/docs_sync.mjs',
  'lang/docs/spec/**',
  'lang/docs/vo-for-gophers.md',
];
const artifactGenerator = ['vo-dev', 'task', 'run', 'task:docs-sync'];

function digest(text) {
  return `sha256:${createHash('sha256').update(text).digest('hex')}`;
}

async function sourceTimestamp(file) {
  try {
    const value = execFileSync('git', ['log', '-1', '--format=%cI', '--', file], {
      cwd: root,
      encoding: 'utf8',
    }).trim();
    if (value) return value;
  } catch {
    // Untracked sources fall back to mtime so local generation still works.
  }
  const stat = await fs.stat(path.join(root, file));
  return stat.mtime.toISOString();
}

async function renderGeneratedDoc(entry) {
  const sourcePath = path.join(root, entry.source);
  const source = await fs.readFile(sourcePath, 'utf8');
  const sourceDigest = digest(source);
  const generatedAt = await sourceTimestamp(entry.source);
  const header = [
    '<!--',
    `Generated from ${entry.source}`,
    'Generator: node scripts/ci/docs_sync.mjs',
    `Source-Digest: ${sourceDigest}`,
    `Generated-At: ${generatedAt}`,
    '-->',
    '',
  ].join('\n');
  return {
    text: `${header}${source}`,
    manifest: {
      id: entry.id,
      title: entry.title,
      source: entry.source,
      output: entry.output,
      source_digest: sourceDigest,
      generated_at: generatedAt,
      generator: 'node scripts/ci/docs_sync.mjs',
    },
  };
}

async function writeTree(outRoot) {
  const manifest = {
    schemaVersion: 1,
    artifact: 'playground.generated-docs',
    path: artifactPath,
    generator: {
      command: artifactGenerator,
    },
    inputs: artifactInputs,
    docs: [],
  };
  await fs.rm(outRoot, { recursive: true, force: true });
  await fs.mkdir(outRoot, { recursive: true });
  for (const entry of docs) {
    const generated = await renderGeneratedDoc(entry);
    const outPath = path.join(outRoot, entry.output);
    await fs.mkdir(path.dirname(outPath), { recursive: true });
    await fs.writeFile(outPath, generated.text, 'utf8');
    manifest.docs.push(generated.manifest);
  }
  await fs.writeFile(path.join(outRoot, '_manifest.json'), `${JSON.stringify(manifest, null, 2)}\n`, 'utf8');
}

async function collectFiles(dir) {
  const files = [];
  async function walk(current) {
    let entries = [];
    try {
      entries = await fs.readdir(current, { withFileTypes: true });
    } catch (error) {
      if (error.code === 'ENOENT') return;
      throw error;
    }
    for (const entry of entries) {
      const absolute = path.join(current, entry.name);
      if (entry.isDirectory()) {
        await walk(absolute);
      } else if (entry.isFile()) {
        files.push(path.relative(dir, absolute).replaceAll(path.sep, '/'));
      }
    }
  }
  await walk(dir);
  return files.sort();
}

async function readRelative(rootDir, rel) {
  return fs.readFile(path.join(rootDir, rel), 'utf8');
}

async function compareTrees(expected, actual) {
  const expectedFiles = await collectFiles(expected);
  const actualFiles = await collectFiles(actual);
  const failures = [];
  const expectedSet = new Set(expectedFiles);
  const actualSet = new Set(actualFiles);
  for (const file of expectedFiles) {
    if (!actualSet.has(file)) failures.push(`missing generated doc: ${file}`);
  }
  for (const file of actualFiles) {
    if (!expectedSet.has(file)) failures.push(`extra generated doc: ${file}`);
  }
  for (const file of expectedFiles) {
    if (!actualSet.has(file)) continue;
    const [left, right] = await Promise.all([readRelative(expected, file), readRelative(actual, file)]);
    if (left !== right) failures.push(`stale generated doc: ${file}`);
  }
  return failures;
}

async function main() {
  const outputRoot = path.join(root, artifactPath);
  if (!checkOnly) {
    await writeTree(outputRoot);
    await fs.rm(path.join(root, 'apps/playground-legacy/src/assets/docs/spec'), { recursive: true, force: true });
    await fs.rm(path.join(root, 'apps/playground-legacy/src/assets/docs/vo-for-gophers.md'), { force: true });
    return;
  }

  const tmp = await fs.mkdtemp(path.join(os.tmpdir(), 'volang-docs-sync-'));
  try {
    await writeTree(tmp);
    const failures = await compareTrees(tmp, outputRoot);
    if (failures.length > 0) {
      throw new Error(`Playground generated docs are out of sync:\n${failures.join('\n')}`);
    }
  } finally {
    await fs.rm(tmp, { recursive: true, force: true });
  }
}

main().catch((error) => {
  console.error(error.message);
  process.exit(1);
});
