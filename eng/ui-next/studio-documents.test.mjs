import assert from 'node:assert/strict';
import test from 'node:test';
import { mkdtemp, mkdir, readFile, writeFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { resolve } from 'node:path';
import { root } from './server.mjs';
import { studioDocument, studioDocumentHtml, verifyStudioDocuments } from './studio-documents.mjs';

test('maintained chapters produce one exact data cache request and no legacy UI pages', async () => {
  await verifyStudioDocuments(root);
  const metadata = JSON.parse(await readFile(resolve(root, 'apps/studio/next/documentation/index.json')));
  assert.equal(metadata.pages.length, 23);
  assert(!metadata.pages.some(page => page.SectionID === 'ui'));
  assert.deepEqual(metadata.pages.filter(page => page.SectionID === 'web-ui').map(page => page.ID), ['first-steps','state','lifecycle','migration']);
  const search = JSON.parse(await readFile(resolve(root, 'apps/studio/next/documentation', metadata.search.Asset)));
  assert.equal(search.version,1);
  assert.deepEqual(search.pages.map(page=>page.ID),metadata.pages.map(page=>page.ID));
  assert(search.pages.find(page=>page.ID==='first-steps').Text.includes('ui.inspectprop'));
  for (const page of metadata.pages) {
    const document = await studioDocument(root, new URL(`https://example.test/studio/docs/${page.ID}?ssr`), { includeBody: true, now: 42 });
    const values = JSON.parse(document.data).Values;
    assert.equal(values.length, 1);
    assert.equal(values[0].Request.Value, `/studio-docs/${page.Asset}?v=${page.SHA256}`);
    assert.equal(values[0].UpdatedAtMilliseconds, 42);
    assert.equal(JSON.parse(values[0].Value).headingID, page.HeadingID);
    assert.equal(document.title, `${page.Title} · Volang Studio`);
    assert.equal(JSON.parse(document.initial).Data, document.data);
  }
  const query = await studioDocument(root, new URL('https://example.test/studio/docs?topic=introduction'));
  assert.equal(query.title, 'Introduction · Volang Studio');
  assert.equal(query.data, '');
  const first = await studioDocument(root, new URL('https://example.test/studio/docs'), {includeBody:true});
  assert.equal(first.title,'First steps · Volang Studio');
  assert.equal(JSON.parse(first.data).Values.length,1);
  const unknown = await studioDocument(root, new URL('https://example.test/studio/docs/no-such-chapter'), { includeBody: true });
  assert.equal(unknown.data, '');
});

test('Studio initial data is inert and metadata replacement cannot rewrite rendered content', () => {
  const template = '<title>Default</title><meta name="description" content="Default"><div id="root"></div><script id="studio-initial-data" type="application/json">""</script>';
  const data = '</script><title>extra</title> & 中文\u2028', title = '<Documentation> & "links"';
  const content = '<p>&lt;title&gt;Default&lt;/title&gt;</p>';
  const html = studioDocumentHtml(template, { data, title, description: title }, content);
  assert(html.includes('&lt;Documentation&gt; &amp; &quot;links&quot;'));
  assert(html.includes(content));
  assert(!html.includes('<title>extra</title>'));
  assert.equal(JSON.parse(html.match(/type="application\/json">(.*?)<\/script>/s)[1]), data);
});

test('a modified generated chapter fails before server rendering', async t => {
  const temporary = await mkdtemp(resolve(tmpdir(), 'volang-studio-docs-'));
  t.after(() => rm(temporary, { recursive: true, force: true }));
  const directory = resolve(temporary, 'apps/studio/next/documentation');
  await mkdir(directory, { recursive: true });
  await writeFile(resolve(directory, 'index.json'), JSON.stringify({ version: 1, pages: [{
    ID: 'changed', Asset: 'page-changed.json', SHA256: '0'.repeat(64), Title: 'Changed', Summary: 'Changed source',
  }] }));
  await writeFile(resolve(directory, 'page-changed.json'), '{}');
  await assert.rejects(studioDocument(temporary, new URL('https://example.test/studio/docs/changed'), { includeBody: true }), /documentation is stale/);
});
