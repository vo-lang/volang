import assert from 'node:assert/strict';
import {spawnSync} from 'node:child_process';
import {mkdtemp,readFile,rm} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import test from 'node:test';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';
import {root} from './server.mjs';

test('isolated browser compiler diagnoses, recovers and executes without the legacy UI kernel',async () => {
  const compiler=await import('../../target/ui-next/wasm-compiler/vo_web.js');
  await compiler.default({module_or_path:await readFile(join(root,'target/ui-next/wasm-compiler/vo_web_bg.wasm'))});
  for (const name of ['compile','compileProject','prepareWorkspaceLock','analyzeProject']) assert.equal(typeof compiler[name],'function',name);
  for (const name of ['reload','setUiLocation','setUiViewport','takeUiInvalidation','takeUiNavigationRequests','takeUiSystemRequests']) {
    assert.equal(compiler.VoVmIsland.prototype[name],undefined,name);
  }
  const invalid=compiler.compile('package main\nfunc main() { println(missingName) }','main.vo');
  try {assert.equal(invalid.success,false);assert.match(invalid.errorMessage,/missingName/);} finally {invalid.free();}
  const compiled=compiler.compile('package main\nfunc main() { println("isolated compiler: ok") }','main.vo');
  try {
    assert.equal(compiled.success,true,compiled.errorMessage);
    const result=compiler.run(compiled.bytecode);
    try {assert.equal(result.status,'ok',result.stderr);assert.equal(result.stdout,'isolated compiler: ok\n');} finally {result.free();}
  } finally {compiled.free();}
  const result=compiler.run(await readFile(join(root,'target/ui-next/runtime.vob')));
  try {assert.equal(result.status,'ok',result.stderr);assert.equal(result.stdout,'ui-next runtime contracts: ok\n');} finally {result.free();}
});

test('default Wasm runtime preserves legacy UI execution and reload; minimal runtime has only the new transport',async () => {
  const full=await import('../../lang/crates/vo-web/pkg/vo_web.js');
  const minimal=await import('../../target/ui-next/wasm-runtime/vo_web.js');
  for (const name of ['reload','takeUiInvalidation','setUiLocation','setUiViewport','takeUiNavigationRequests','takeUiSystemRequests']) {
    assert.equal(typeof full.VoVmIsland.prototype[name],'function',name);
    assert.equal(minimal.VoVmIsland.prototype[name],undefined,name);
  }
  for (const name of ['run','runScheduled','takeHostOutput','takePendingHostEvents','wakeHostEventWithData']) {
    assert.equal(typeof minimal.VoVmIsland.prototype[name],'function',name);
  }
  await full.default({module_or_path:await readFile(join(root,'lang/crates/vo-web/pkg/vo_web_bg.wasm'))});
  await minimal.default({module_or_path:await readFile(join(root,'target/ui-next/wasm-runtime/vo_web_bg.wasm'))});
  const directory=await mkdtemp(join(tmpdir(),'volang-legacy-ui-'));
  let vm;
  try {
    const source=join(root,'ui/next/tests/legacy-runtime'),image=join(directory,'app.vob');
    const compiled=spawnSync(compilerPath(),['emit','bytecode',source,'-o',image],{
      cwd:root,env:{...process.env,VOWORK:'off'},encoding:'utf8',timeout:120000,
    });
    assert.ifError(compiled.error);assert.equal(compiled.status,0,compiled.stderr);
    const bytes=await readFile(image);
    assert.throws(() => new minimal.VoVmIsland(bytes),/provider|extern/i);
    vm=new full.VoVmIsland(bytes);
    vm.setUiLocation('/legacy',false);vm.setUiViewport(800,600,1,false);
    assert.equal(vm.run(),'suspended_for_host_events');
    assert(vm.takeHostOutput()?.length > 0);
    assert.equal(vm.reload(bytes),'suspended_for_host_events');
    assert(vm.takeHostOutput()?.length > 0);
    assert.deepEqual(vm.takeUiNavigationRequests(),[]);assert.deepEqual(vm.takeUiSystemRequests(),[]);
  } finally {vm?.free();await rm(directory,{recursive:true,force:true});}
});
