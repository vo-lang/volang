import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {join} from 'node:path';
import test from 'node:test';
import {root} from './server.mjs';

test('isolated browser compiler diagnoses, recovers and executes through the Island exchange',async () => {
  const compiler=await import('../../target/ui-next/wasm-compiler/vo_web.js');
  await compiler.default({module_or_path:await readFile(join(root,'target/ui-next/wasm-compiler/vo_web_bg.wasm'))});
  for (const name of ['compile','compileProject','prepareWorkspaceLock','analyzeProject']) assert.equal(typeof compiler[name],'function',name);
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

test('browser compiler and execution runtime share the Island exchange API', async () => {
  for (const path of ['../../lang/crates/vo-web/pkg/vo_web.js', '../../target/ui-next/wasm-runtime/vo_web.js']) {
    const runtime = await import(path);
    for (const name of ['run','runScheduled','takeHostOutput','takePendingHostEvents','wakeHostEventWithData']) {
      assert.equal(typeof runtime.VoVmIsland.prototype[name], 'function', name);
    }
  }
});
