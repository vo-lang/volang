import assert from 'node:assert/strict';
import test from 'node:test';
import { prerenderPages, maxPages, maxPageDataBytes } from './prerender-pages.mjs';

test('static page declarations bind canonical paths, assets and exact initial data', () => {
  assert.deepEqual(prerenderPages({}), [{ path: '/', file: 'index.html', assets: './', data: '' }]);
  const pages = prerenderPages({ prerenderEntry: 'prerender', prerenderPages: [
    { path: '/', data: 'root' }, { path: '/a small idea/中文', data: '中文\u0000' },
  ] });
  assert.deepEqual(pages[1], { path: '/a small idea/中文/', file: 'a small idea/中文/index.html', assets: '../../', data: '中文\u0000' });
  assert.equal(prerenderPages({ prerenderEntry: 'prerender', prerenderPages: [{ path: '/', data: 'x'.repeat(maxPageDataBytes) }] })[0].data.length, maxPageDataBytes);
  const configured = prerenderPages({ prerenderEntry: 'prerender', document: { title: 'Shared title', description: 'Shared description' },
    prerenderPages: [{ path: '/' }, { path: '/chapter', title: 'Chapter', description: '' }] });
  assert.equal(configured[0].title, 'Shared title');
  assert.equal(configured[1].title, 'Chapter');
  assert.equal(configured[1].description, '');
});

test('ambiguous paths, incompatible data and unbounded page sets fail before rendering', () => {
  const pages = list => prerenderPages({ prerenderEntry: 'prerender', prerenderPages: list });
  for (const invalid of [null, {}, '', []]) assert.throws(() => pages(invalid), /1\.\.256/);
  for (const path of ['relative', '/a//b', '/a/../b', '/%2f', '/a?b', '/a#b', '/a\\b', '/a.', '/con', '/assets', '/INDEX.HTML', '/THIRD_PARTY_NOTICES.txt', '/' + 'x'.repeat(256), '/\ud800']) {
    assert.throws(() => pages([{ path: '/' }, { path }]), /page path|Static page path|Static page paths/);
  }
  assert.throws(() => pages([{ path: '/' }, { path: '/A' }, { path: '/a/' }]), /Duplicate/);
  assert.throws(() => pages([{ path: '/' }, { path: '/é' }, { path: '/e\u0301' }]), /Duplicate/);
  assert.throws(() => pages([{ path: '/missing-root' }]), /root page/);
  assert.throws(() => pages(Array(maxPages + 1).fill({ path: '/' })), /1\.\.256/);
  for (const data of [null, {}, 42, '\ud800', 'x'.repeat(maxPageDataBytes + 1)]) {
    assert.throws(() => pages([{ path: '/', data }]), /initial data/);
  }
  assert.throws(() => prerenderPages({ prerenderPages: [{ path: '/' }] }), /requires prerenderEntry/);
  assert.throws(() => pages([{ path: '/', date: 'typo' }]), /optional string data/);
  for (const document of [null, [], { unknown: 'ignored?' }, { title: '\u0000' }, { title: '\ud800' }, { title: 3 }, { description: 'x'.repeat(8193) }]) {
    assert.throws(() => prerenderPages({ document }), /Document/);
  }
  assert.throws(() => pages([{ path: '/', title: null }]), /Document title/);
});
