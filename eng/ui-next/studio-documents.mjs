import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { readFile, readdir } from 'node:fs/promises';
import { resolve } from 'node:path';

const directory = 'apps/studio/next/documentation';
const digest = bytes => `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
const stale = path => `Studio documentation is stale: ${path}. Run cargo run -q -p vo-dev --locked -- generate studio-docs --write`;
const htmlText = value => value.replace(/[&<>"']/g, character => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[character]));
const scriptJSON = value => JSON.stringify(value).replace(/[<>&\u2028\u2029]/g, character => `\\u${character.charCodeAt(0).toString(16).padStart(4, '0')}`);

// Validate generated inputs once before compiling or serving. Bodies stay on
// disk; the server and browser read only the selected chapter.
export async function verifyStudioDocuments(root) {
  const provenance = JSON.parse(await readFile(resolve(root, directory, 'provenance.json'), 'utf8'));
  assert.equal(provenance.artifact, 'studio-next-documentation.generated');
  for (const [path, expected] of Object.entries(provenance.sourceDigests)) {
    assert.equal(digest(await readFile(resolve(root, path))), expected, stale(path));
  }
  for (const output of provenance.outputs) {
    const bytes = await readFile(resolve(root, directory, output.path));
    assert.equal(bytes.length, output.size, stale(output.path));
    assert.equal(digest(bytes), output.digest, stale(output.path));
  }
  assert.deepEqual((await readdir(resolve(root, directory))).sort(),
    ['provenance.json', ...provenance.outputs.map(output => output.path)].sort(), stale(directory));
}

export async function studioDocument(root, url, { includeBody = false, now = Date.now() } = {}) {
  const location = url.pathname + url.search;
  const result = { location, data: '', title: undefined, description: undefined };
  const match = /^\/studio\/docs(?:\/([a-z0-9-]+))?\/?$/.exec(url.pathname);
  if (!match) return result;
  const index = JSON.parse(await readFile(resolve(root, directory, 'index.json'), 'utf8'));
  assert.equal(index.version, 1);
  const page = index.pages.find(page => page.ID === (match[1] || 'first-steps'));
  if (!page) return result;
  result.title = `${page.Title} · Volang Studio`;
  result.description = page.Summary;
  if (includeBody) {
    assert.match(page.Asset, /^page-[a-z0-9-]+\.json$/);
    const bytes = await readFile(resolve(root, directory, page.Asset));
    assert(bytes.length <= 2 * 1024 * 1024, 'Studio document exceeds its byte limit.');
    assert.equal(digest(bytes), `sha256:${page.SHA256}`, stale(page.Asset));
    const value = new TextDecoder('utf-8', { fatal: true }).decode(bytes);
    result.data = JSON.stringify({ Version: 1, Values: [{
      Request: { Service: 'web.fetch-text', Value: `/studio-docs/${page.Asset}?v=${page.SHA256}`, TimeoutMilliseconds: 10000 },
      Value: value, UpdatedAtMilliseconds: now,
    }] });
  }
  result.initial = JSON.stringify({ Version: 1, Location: location, Data: result.data });
  assert(Buffer.byteLength(result.initial) <= 1024 * 1024, 'Studio initial data exceeds 1 MiB.');
  return result;
}

export function studioDocumentHtml(template, document, content) {
  let html = template;
  if (document.title) html = html.replace(/<title>[^<]*<\/title>/, () => `<title>${htmlText(document.title)}</title>`);
  if (document.description) html = html.replace(/<meta name="description" content="[^"]*">/,
    () => `<meta name="description" content="${htmlText(document.description)}">`);
  html = html.replace('<script id="studio-initial-data" type="application/json">""</script>',
    () => `<script id="studio-initial-data" type="application/json">${scriptJSON(document.data)}</script>`);
  if (content !== undefined) html = html.replace('<div id="root"></div>', () => `<div id="root">${content}</div>`);
  return html;
}
