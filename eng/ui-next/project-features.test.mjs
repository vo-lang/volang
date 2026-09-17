import assert from 'node:assert/strict';
import test from 'node:test';
import {mkdtemp, readFile, rm} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join, resolve} from 'node:path';
import {build} from './node_modules/esbuild/lib/main.js';
import {projectFeatures, projectHost} from './project-features.mjs';
import {thirdPartyNotices} from './third-party.mjs';
import {root} from './server.mjs';

const bundle = (features, development = false, plugins = []) => build({
  absWorkingDir:root, stdin:{contents:"export {mountUi, createNavigationServices} from '@volang/ui-next';", resolveDir:root},
  outdir:resolve(root, 'target/ui-next/feature-bundle'), write:false, bundle:true,
  splitting:true, format:'esm', platform:'browser', target:'es2022', minify:true, metafile:true,
  plugins:[projectHost({features, development}), ...plugins],
});

test('project feature declarations reject unsupported or repeated dependencies', () => {
  assert.deepEqual(projectFeatures({}), []);
  assert.deepEqual(projectFeatures({features:[]}), []);
  assert.deepEqual(projectFeatures({features:['editor','canvas']}), ['canvas','editor']);
  const config = {features:['editor']};
  const result = projectFeatures(config); result.push('local mutation');
  assert.deepEqual(config.features, ['editor']);
  for (const features of [null, 'editor', {}, ['editor','editor'], ['canvas','canvas'], ['unknown'], ['toString'], [['canvas']], [['editor']], [new String('canvas')], [1], [null]]) {
    assert.throws(() => projectFeatures({features}), /unique supported names/);
  }
});

test('ordinary entries omit the optional editor; enabled builds split it and carry real licenses', async () => {
  const empty = await bundle([]);
  assert(!Object.keys(empty.metafile.inputs).some(path => /codemirror|editor-provider|canvas-bitmap/.test(path)));
  for (const development of [false, true]) {
    const enabled = await bundle(['editor'], development);
    assert(Object.keys(enabled.metafile.inputs).some(path => path.includes('@codemirror/view/')));
    assert(Object.keys(enabled.metafile.outputs).some(path => /editor-library.*\.js$/.test(path)));
    const entry = Object.values(enabled.metafile.outputs).find(output => output.entryPoint === '<stdin>');
    assert(entry.imports.some(imported => imported.kind === 'dynamic-import'));
    const directory = await mkdtemp(join(tmpdir(), 'ui-feature-licenses-'));
    try {
      const licenses = await thirdPartyNotices({inputs:enabled.metafile.inputs, workingDirectory:root, directory});
      assert(licenses.some(item => item.name === '@codemirror/view' && item.version === '6.43.11' && item.license === 'MIT'));
      assert(!licenses.some(item => item.name === 'fflate'));
      const notice = await readFile(join(directory, 'THIRD_PARTY_NOTICES.txt'), 'utf8');
      assert.match(notice, /@codemirror\/view 6\.43\.11/);
      assert.match(notice, /Permission is hereby granted/);
      assert(!notice.includes(root));
    } finally {await rm(directory, {recursive:true, force:true});}
  }
});

test('Canvas is independently optional and composes with the editor', async () => {
  for (const development of [false, true]) {
    const bitmap = await bundle(['canvas'], development);
    assert(Object.keys(bitmap.metafile.inputs).some(path => path.endsWith('/canvas-bitmap.ts')));
    assert(!Object.keys(bitmap.metafile.inputs).some(path => /codemirror|editor-provider/.test(path)));
    const combined = await bundle(['canvas','editor'], development);
    assert(Object.keys(combined.metafile.inputs).some(path => path.endsWith('/canvas-bitmap.ts')));
    assert(Object.keys(combined.metafile.inputs).some(path => path.includes('@codemirror/view/')));
  }
});

test('optional entry preserves root-bound services and explicit widget overrides', async () => {
  const result = await bundle(['canvas','editor','plot'], false, [{name:'fixture-host', setup(build) {
    build.onResolve({filter:/\/ui_next\/mount\.ts$/}, () => ({path:'host', namespace:'fixture-host'}));
    build.onResolve({filter:/\/ui_next\/plot-provider\.ts$/}, () => ({path:'plot', namespace:'fixture-host'}));
    build.onResolve({filter:/\/plot-library\.mjs$/}, () => ({path:'library', namespace:'fixture-host'}));
    build.onResolve({filter:/\/ui_next\/editor-provider\.ts$/}, () => ({path:'editor', namespace:'fixture-host'}));
    build.onResolve({filter:/\/ui_next\/canvas-bitmap\.ts$/}, () => ({path:'canvas', namespace:'fixture-host'}));
    build.onResolve({filter:/\/editor-library\.mjs$/}, () => ({path:'library', namespace:'fixture-host'}));
    build.onLoad({filter:/.*/, namespace:'fixture-host'}, ({path}) => ({loader:'js', contents:path === 'host'
      ? 'export function mountUi(container, options) { return {...options, bound:options.services(container)}; } export const createNavigationServices = "preserved";'
      : path === 'plot' ? 'export const createPlotProvider = load => ({load});' : path === 'canvas' ? 'export const canvasBitmap = () => {};' : path === 'editor' ? 'export const createCodeEditorProvider = load => ({load});' : 'export const library = true;'}));
  }}]);
  const entry = result.outputFiles.find(file => file.path.endsWith('stdin.js'));
  const module = await import('data:text/javascript;base64,' + Buffer.from(entry.text).toString('base64'));
  const initialData = async () => 'initial', custom = () => {}, other = () => {}, root = {};
  const supplied = {initialData, widgets:{uplot:custom, 'code-editor':custom, 'canvas-bitmap':custom, other}, watches:{watch:other}};
  let observed;
  const mounted = module.mountUi(root, {backend:'vm', services:target => {observed=target; return supplied;}});
  assert.equal(observed, root); assert.equal(mounted.backend, 'vm');
  assert.equal(mounted.bound.initialData, initialData);
  assert.deepEqual(mounted.bound.widgets, supplied.widgets);
  assert.equal(mounted.bound.watches, supplied.watches);
  assert.equal(module.createNavigationServices, 'preserved');
  const next = module.mountUi({}, {services:{initialData}});
  assert.equal(typeof next.bound.widgets.uplot.load, 'function');
  assert.equal(typeof next.bound.widgets['code-editor'].load, 'function');
  assert.equal(typeof next.bound.widgets['canvas-bitmap'], 'function');
  assert.equal(next.bound.initialData, initialData);
});

test('plot is independently lazy, retains upstream CSS and licenses, and composes with other packs', async () => {
  assert.deepEqual(projectFeatures({features:['plot','editor','canvas']}),['canvas','editor','plot']);
  assert.throws(()=>projectFeatures({features:['plot','plot']}),/unique supported names/);
  const plain=await bundle([]);
  assert(!Object.keys(plain.metafile.inputs).some(path=>/uplot|plot-provider|plot-library/.test(path)));
  for (const development of [false,true]) {
    const result=await bundle(['plot'],development);
    const inputs=Object.keys(result.metafile.inputs);
    assert(inputs.some(path=>path.endsWith('/uplot/dist/uPlot.min.css')));
    assert(!inputs.some(path=>/codemirror|canvas-bitmap/.test(path)));
    const output=Object.values(result.metafile.outputs).find(value=>value.entryPoint==='<stdin>');
    assert(output.imports.some(value=>value.kind==='dynamic-import' && /plot-library/.test(value.path)));
    const library=result.outputFiles.find(file=>/plot-library.*\.js$/.test(file.path));
    assert(library.text.includes('.uplot') && !plain.outputFiles.some(file=>file.text.includes('.uplot')));
    assert(!result.outputFiles.some(file=>file.path.endsWith('.css')));
    const directory=await mkdtemp(join(tmpdir(),'ui-plot-licenses-'));
    try {
      const licenses=await thirdPartyNotices({inputs:result.metafile.inputs,workingDirectory:root,directory});
      assert.deepEqual(licenses.map(({name,version,license})=>({name,version,license})),[{name:'uplot',version:'1.6.32',license:'MIT'}]);
      assert.match(await readFile(join(directory,'THIRD_PARTY_NOTICES.txt'),'utf8'),/Permission is hereby granted/);
    } finally {await rm(directory,{recursive:true,force:true});}
    const combined=await bundle(['canvas','editor','plot'],development);
    assert(Object.keys(combined.metafile.inputs).some(path=>path.endsWith('/uplot/dist/uPlot.min.css')));
    assert(Object.keys(combined.metafile.inputs).some(path=>path.includes('@codemirror/view/')));
  }
});
