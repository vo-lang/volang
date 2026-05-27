import { spawnSync } from 'node:child_process';
import { promises as fs } from 'node:fs';
import path from 'node:path';

const root = path.resolve(new URL('../..', import.meta.url).pathname);

async function exists(file) {
  try {
    await fs.access(file);
    return true;
  } catch {
    return false;
  }
}

async function collectMarkdownFiles(relDir) {
  const dir = path.join(root, relDir);
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
      } else if (entry.isFile() && entry.name.endsWith('.md')) {
        files.push(path.relative(root, absolute).replaceAll(path.sep, '/'));
      }
    }
  }
  await walk(dir);
  return files.sort();
}

async function lintCurrentDocFacts() {
  const rustToolchainSource = await fs.readFile(path.join(root, 'rust-toolchain.toml'), 'utf8');
  const rustChannel = rustToolchainSource.match(/^\s*channel\s*=\s*"([^"]+)"/m)?.[1];
  if (!rustChannel) {
    throw new Error('docs lint expected rust-toolchain.toml to declare [toolchain].channel');
  }
  const toolchainsSource = await fs.readFile(path.join(root, 'eng/toolchains.toml'), 'utf8');
  if (!toolchainsSource.includes('source = "rust-toolchain.toml"')) {
    throw new Error('docs lint expected eng/toolchains.toml to source Rust from rust-toolchain.toml');
  }

  const testsSource = await fs.readFile(path.join(root, 'eng/tests.toml'), 'utf8');
  const targetEnvs = parseTestTargetEnvs(testsSource);
  requireEnv(targetEnvs, 'jit', 'VO_JIT_CALL_THRESHOLD', '1');
  requireEnv(targetEnvs, 'osr', 'VO_JIT_CALL_THRESHOLD', '1000');
  requireEnv(targetEnvs, 'osr', 'VO_JIT_LOOP_THRESHOLD', '1');
  requireEnv(targetEnvs, 'gc-jit', 'VO_GC_DEBUG', '1');
  requireEnv(targetEnvs, 'gc-jit', 'VO_JIT_CALL_THRESHOLD', '1');

  const benchmarksManifestSource = await fs.readFile(
    path.join(root, 'benchmarks/manifest.toml'),
    'utf8',
  );
  const benchmarkCount = [...benchmarksManifestSource.matchAll(/^\s*\[\[benchmark\]\]\s*$/gm)].length;
  if (benchmarkCount === 0) {
    throw new Error('docs lint expected benchmarks/manifest.toml to declare benchmark entries');
  }
  const benchRunnerSource = await fs.readFile(path.join(root, 'cmd/vo-dev/src/dev_bench.rs'), 'utf8');
  for (const expected of [
    'const DEFAULT_BENCH_WARMUP: u64 = 1;',
    'const DEFAULT_BENCH_RUNS: u64 = 3;',
    'target/bench/results',
    'target/bench/artifacts',
    'target/bench/go-cache',
    'GOCACHE',
    'summary.json',
    'warning_count',
  ]) {
    if (!benchRunnerSource.includes(expected)) {
      throw new Error(`docs lint expected benchmark runner source to include ${expected}`);
    }
  }

  const ffiMacroSource = await fs.readFile(
    path.join(root, 'lang/crates/vo-ffi-macro/src/lib.rs'),
    'utf8',
  );
  if (!ffiMacroSource.includes('pub fn vo_fn') || !ffiMacroSource.includes('pub fn vostd_fn')) {
    throw new Error('FFI docs lint expected #[vo_fn] and #[vostd_fn] to be exported by vo-ffi-macro');
  }
  if (ffiMacroSource.includes('pub fn vo_extern') || ffiMacroSource.includes('pub fn vo_extern_ctx')) {
    throw new Error('FFI docs lint expected old vo_extern macros to be absent from vo-ffi-macro');
  }

  const currentDocs = [
    'README.md',
    ...(await collectMarkdownFiles('lang/docs/spec')),
    ...(await collectMarkdownFiles('lang/docs/dev')),
    ...(await collectMarkdownFiles('apps/studio/docs')),
  ];
  const forbidden = [
    {
      pattern: /#\[\s*vo_extern(?:_ctx)?\b/,
      reason: 'current FFI docs must use #[vo_fn] / #[vostd_fn], not removed vo_extern macros',
    },
    {
      pattern: /all examples execute Vo via WASM/i,
      reason: 'Studio native sessions do not use the web WASM GUI pipeline',
    },
    {
      pattern: /test harness .*thresholds to 0/i,
      reason: 'JIT test target thresholds come from eng/tests.toml',
    },
    {
      pattern: /generational aging/i,
      reason: 'current GC docs should describe the implemented incremental mark-sweep collector',
    },
    {
      pattern: /Requires Rust toolchain \(1\.75\+\)/i,
      reason: `Rust toolchain docs must follow rust-toolchain.toml (${rustChannel})`,
    },
    {
      pattern: /Geometric-mean relative time/i,
      reason: 'checked-in docs must not present stale benchmark table math as current fact',
    },
    {
      pattern: /Measured on Apple M1/i,
      reason: 'checked-in docs must not present an untracked local benchmark run as current fact',
    },
    {
      pattern: /Vo-JIT is competitive with Go and Java/i,
      reason: 'benchmark comparisons require current benchmark results, not stale prose',
    },
    {
      pattern: /native C\/Go speed/i,
      reason: 'benchmark comparisons require current benchmark results, not stale prose',
    },
  ];

  for (const rel of currentDocs) {
    const text = await fs.readFile(path.join(root, rel), 'utf8');
    for (const { pattern, reason } of forbidden) {
      if (pattern.test(text)) {
        throw new Error(`${rel}: stale doc fact found (${reason})`);
      }
    }
  }

  const studioInstall = await fs.readFile(
    path.join(root, 'apps/studio/docs/pages/getting-started/installation.md'),
    'utf8',
  );
  if (!studioInstall.includes('rust-toolchain.toml') || !studioInstall.includes(`Rust ${rustChannel}`)) {
    throw new Error(
      `apps/studio/docs/pages/getting-started/installation.md must cite rust-toolchain.toml and Rust ${rustChannel}`,
    );
  }

  const backendDoc = await fs.readFile(
    path.join(root, 'apps/studio/docs/pages/advanced/backends.md'),
    'utf8',
  );
  const normalizedBackendDoc = backendDoc.replace(/\s+/g, ' ');
  for (const expected of [
    `jit\` sets \`VO_JIT_CALL_THRESHOLD=${targetEnvs.get('jit').VO_JIT_CALL_THRESHOLD}`,
    `osr\` sets \`VO_JIT_CALL_THRESHOLD=${targetEnvs.get('osr').VO_JIT_CALL_THRESHOLD}`,
    `VO_JIT_LOOP_THRESHOLD=${targetEnvs.get('osr').VO_JIT_LOOP_THRESHOLD}`,
    `gc-jit\` sets \`VO_GC_DEBUG=${targetEnvs.get('gc-jit').VO_GC_DEBUG}`,
    `VO_JIT_CALL_THRESHOLD=${targetEnvs.get('gc-jit').VO_JIT_CALL_THRESHOLD}`,
    `benchmark suite currently has ${benchmarkCount} manifest entries`,
  ]) {
    if (!normalizedBackendDoc.includes(expected)) {
      throw new Error(`apps/studio/docs/pages/advanced/backends.md missing source-backed fact: ${expected}`);
    }
  }

  const readme = await fs.readFile(path.join(root, 'README.md'), 'utf8');
  const normalizedReadme = readme.replace(/\s+/g, ' ');
  if (!normalizedReadme.includes(`benchmark suite currently has ${benchmarkCount} manifest entries`)) {
    throw new Error(`README.md must describe the current ${benchmarkCount}-entry benchmark suite`);
  }
  for (const expected of [
    '--warmup N',
    '--runs N',
    'target/bench/results/',
    'target/bench/artifacts/',
    'target/bench/go-cache/',
    'summary.json',
  ]) {
    if (!readme.includes(expected)) {
      throw new Error(`README.md missing source-backed benchmark runner fact: ${expected}`);
    }
    if (!backendDoc.includes(expected)) {
      throw new Error(`apps/studio/docs/pages/advanced/backends.md missing source-backed benchmark runner fact: ${expected}`);
    }
  }
}

function parseTestTargetEnvs(source) {
  const targets = new Map();
  for (const chunk of source.split(/\n(?=\[\[target\]\])/)) {
    if (!chunk.includes('[[target]]')) continue;
    const name = chunk.match(/^\s*name\s*=\s*"([^"]+)"/m)?.[1];
    if (!name) continue;
    const envSource = chunk.match(/^\s*env\s*=\s*\{([^}]*)\}/m)?.[1] ?? '';
    const env = {};
    for (const pair of envSource.matchAll(/([A-Za-z0-9_]+)\s*=\s*"([^"]*)"/g)) {
      env[pair[1]] = pair[2];
    }
    targets.set(name, env);
  }
  return targets;
}

function requireEnv(targets, target, key, expected) {
  const actual = targets.get(target)?.[key];
  if (actual !== expected) {
    throw new Error(`eng/tests.toml target ${target} expected ${key}=${expected}, found ${actual ?? '<missing>'}`);
  }
}

function parseStudioManifest(source) {
  const sections = [];
  let section = null;
  let page = null;
  for (const rawLine of source.split(/\r?\n/)) {
    const line = rawLine.trim();
    if (!line || line.startsWith('#')) continue;
    if (line === '[[section]]') {
      section = { title: '', slug: '', pages: [] };
      sections.push(section);
      page = null;
      continue;
    }
    if (line === '[[section.page]]') {
      if (!section) throw new Error('Studio docs manifest declares a page before a section');
      page = { title: '', file: '' };
      section.pages.push(page);
      continue;
    }
    const match = line.match(/^([A-Za-z0-9_.-]+)\s*=\s*"([^"]*)"$/);
    if (!match) continue;
    const [, key, value] = match;
    if (page) {
      if (key === 'title') page.title = value;
      if (key === 'file') page.file = value;
    } else if (section) {
      if (key === 'title') section.title = value;
      if (key === 'slug') section.slug = value;
    }
  }
  return sections;
}

async function lintStudioDocs() {
  const manifestPath = path.join(root, 'apps/studio/docs/manifest.toml');
  const source = await fs.readFile(manifestPath, 'utf8');
  const sections = parseStudioManifest(source);
  if (sections.length === 0) throw new Error('Studio docs manifest has no sections');
  const seen = new Set();
  for (const section of sections) {
    if (!section.title || !section.slug) throw new Error('Studio docs section is missing title or slug');
    if (section.pages.length === 0) throw new Error(`Studio docs section ${section.slug} has no pages`);
    for (const page of section.pages) {
      if (!page.title || !page.file) throw new Error(`Studio docs section ${section.slug} has an incomplete page`);
      if (seen.has(page.file)) throw new Error(`duplicate Studio docs page: ${page.file}`);
      seen.add(page.file);
      const pagePath = path.join(root, 'apps/studio/docs/pages', page.file);
      if (!(await exists(pagePath))) throw new Error(`Studio docs manifest references missing page: ${page.file}`);
    }
  }
  if (await exists(path.join(root, 'apps/studio/docs/_manifest.json'))) {
    throw new Error('Studio docs must use manifest.toml, not _manifest.json');
  }
}

async function lintPlaygroundDocs() {
  const oldSpec = path.join(root, 'apps/playground-legacy/src/assets/docs/spec');
  const oldGuide = path.join(root, 'apps/playground-legacy/src/assets/docs/vo-for-gophers.md');
  if (await exists(oldSpec)) throw new Error('Playground docs spec mirror must not exist; use generated/');
  if (await exists(oldGuide)) throw new Error('Playground guide mirror must not exist; use generated/');
  const result = spawnSync(process.execPath, ['scripts/ci/docs_sync.mjs', '--check'], {
    cwd: root,
    stdio: 'inherit',
  });
  if (result.status !== 0) {
    throw new Error('docs sync check failed');
  }
}

async function main() {
  await lintCurrentDocFacts();
  await lintStudioDocs();
  await lintPlaygroundDocs();
}

main().catch((error) => {
  console.error(error.message);
  process.exit(1);
});
