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
  await lintStudioDocs();
  await lintPlaygroundDocs();
}

main().catch((error) => {
  console.error(error.message);
  process.exit(1);
});
