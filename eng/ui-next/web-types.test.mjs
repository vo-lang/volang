import assert from 'node:assert/strict';
import {mkdtemp,rm,writeFile} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import test from 'node:test';
import ts from '../../lang/crates/vo-web/node_modules/typescript/lib/typescript.js';
import {root} from './server.mjs';

test('typed UI loaders accept both actual isolated Wasm packages and require the exchange contract',async () => {
  const directory=await mkdtemp(join(tmpdir(),'volang-ui-loader-types-'));
  const specifier=path => JSON.stringify(join(root,path).replaceAll('\\','/'));
  const entry=join(directory,'loaders.ts');
  try {
    await writeFile(entry,`
import type {MountOptions} from ${specifier('lang/crates/vo-web/js/ui_next/mount.js')};
import * as runtime from ${specifier('target/ui-next/wasm-runtime/vo_web.js')};
import * as compiler from ${specifier('target/ui-next/wasm-compiler/vo_web.js')};
const execution: MountOptions = {backend:'vm',artifact:'app.vob',loadVm:async () => runtime};
const playground: MountOptions = {backend:'vm',artifact:'app.vob',loadVm:async () => compiler};
// @ts-expect-error The loader must supply an owned, runnable exchange VM.
const incomplete: MountOptions = {backend:'vm',artifact:'app.vob',loadVm:async () => ({default:async () => {},VoVmIsland:class {}})};
void execution; void playground; void incomplete;
`);
    const program=ts.createProgram([entry],{noEmit:true,strict:true,allowJs:true,skipLibCheck:true,
      module:ts.ModuleKind.ESNext,moduleResolution:ts.ModuleResolutionKind.Bundler,target:ts.ScriptTarget.ES2020});
    const diagnostics=ts.getPreEmitDiagnostics(program);
    assert.equal(diagnostics.length,0,ts.formatDiagnostics(diagnostics,{
      getCanonicalFileName:path=>path,getCurrentDirectory:()=>root,getNewLine:()=> '\n',
    }));
  } finally {await rm(directory,{recursive:true,force:true});}
});
