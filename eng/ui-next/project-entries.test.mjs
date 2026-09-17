import assert from 'node:assert/strict';
import test from 'node:test';
import {mkdir, mkdtemp, realpath, rm, symlink, writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {tmpdir} from 'node:os';
import {projectEntries, resolveProjectEntries, maxPageEntries, entryAssets} from './project-entries.mjs';
import {prerenderPages} from './prerender-pages.mjs';
import {prepareHtml, entryMarker} from './prerender.mjs';

const configuration = () => ({developmentEntry:'development', prerenderEntry:'prerender',
  pageEntries:{notes:{entry:'notes', developmentEntry:'notes/development', prerenderEntry:'notes/prerender'}},
  prerenderPages:[{path:'/'}, {path:'/library/notes', entry:'notes', data:'Selected note'}]});

test('page identities select the same independent source and image across page paths', () => {
  const config = configuration(), entries = projectEntries(config), pages = prerenderPages(config);
  assert.deepEqual([...entries.keys()], ['default', 'notes']);
  assert.equal(entries.get(pages[1].entry).entry, 'notes');
  assert.equal(pages[1].assets, '../../');
  assert.equal(entryAssets('default'), 'assets/app');
  assert.equal(entryAssets('notes'), 'assets/entries/notes/app');
  config.prerenderPages.push({path:'/library/another', entry:'notes', data:'Another note'});
  assert.equal(prerenderPages(config).length, 3);
  // A root page may select a named entry without forcing an unused default image.
  config.prerenderPages[0].entry = 'notes';
  delete config.prerenderEntry;
  assert.equal(prerenderPages(config)[0].entry, 'notes');
});

test('ambiguous entry declarations and mismatched development protocols are rejected', () => {
  const config = configuration();
  for (const id of ['default', 'Notes', '../notes', '/notes', '1notes', 'a/b', 'a\\b', 'a\n', 'a'.repeat(65), 'con', 'com9']) {
    assert.throws(() => projectEntries({...config, pageEntries:{[id]:config.pageEntries.notes}}), /Page entry names/);
  }
  for (const value of [null, [], {}, Object.fromEntries(Array.from({length:maxPageEntries + 1}, (_, index) => [`a${index}`, config.pageEntries.notes]))]) {
    assert.throws(() => projectEntries({...config, pageEntries:value}), /1\.\.32/);
  }
  for (const value of [null, [], {entry:'notes', unexpected:true}, {entry:42}, {entry:'/absolute'}, {entry:'a\\b'}]) {
    assert.throws(() => projectEntries({...config, pageEntries:{notes:value}}), /accepts|relative Vo entry/);
  }
  assert.throws(() => projectEntries({...config, pageEntries:{notes:{entry:'notes'}}}), /Every page entry/);
  assert.throws(() => projectEntries({...config, developmentEntry:undefined}), /Every page entry/);
  assert.throws(() => projectEntries({...config, serverEntry:'server'}), /serverEntry or prerenderPages/);
  const server = projectEntries({...config, serverEntry:'server', prerenderPages:undefined});
  assert.equal(server.get('notes').entry, 'notes');
  assert.throws(() => projectEntries({serverEntry:'server', prerenderPages:[{path:'/'}]}), /serverEntry or prerenderPages/);
  assert.throws(() => projectEntries({...config, prerenderPages:undefined}), /requires prerenderPages or serverEntry/);
  assert.throws(() => prerenderPages({...config, prerenderPages:[{path:'/', entry:'missing'}]}), /Unknown page entry/);
  delete config.pageEntries.notes.prerenderEntry;
  assert.throws(() => prerenderPages(config), /prerenderEntry for entry notes/);
});

test('source resolution checks every entry and rejects paths or symlinks outside the project', async () => {
  const temporary = await mkdtemp(join(tmpdir(), 'ui-page-entries-'));
  try {
    const directory = join(temporary, 'project');
    const config = configuration();
    for (const path of ['development', 'prerender', 'notes/development', 'notes/prerender']) await mkdir(join(directory, path), {recursive:true});
    const entries = await resolveProjectEntries(directory, config);
    assert.equal(entries.get('notes').prerenderEntry, await realpath(join(directory, 'notes/prerender')));
    await writeFile(join(temporary, 'outside.vo'), 'package main');
    config.pageEntries.notes.entry = '../outside.vo';
    await assert.rejects(resolveProjectEntries(directory, config), /notes.entry must stay inside/);
    await symlink(join(temporary, 'outside.vo'), join(directory, 'outside.vo'));
    config.pageEntries.notes.entry = 'outside.vo';
    await assert.rejects(resolveProjectEntries(directory, config), /notes.entry must stay inside/);
  } finally { await rm(temporary, {recursive:true, force:true}); }
});

test('page identity is explicit in HTML while existing single-entry documents remain valid', () => {
  const render = prepareHtml(`<meta name="ui-next-entry" content="${entryMarker}">`, false);
  assert.equal(render(), '<meta name="ui-next-entry" content="default">');
  assert.equal(render(undefined, {entry:'notes'}), '<meta name="ui-next-entry" content="notes">');
  assert.throws(() => render(undefined, {entry:'../notes'}), /Invalid page entry/);
  assert.throws(() => prepareHtml(entryMarker + entryMarker, false), /at most one/);
  assert.equal(prepareHtml('<main></main>', false)(), '<main></main>');
  assert.throws(() => prepareHtml('<main></main>', false)(undefined, {entry:'notes'}), /Named page entries require/);
});
