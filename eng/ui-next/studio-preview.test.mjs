import assert from 'node:assert/strict';
import {test} from 'node:test';
import {runInNewContext} from 'node:vm';
import {setImmediate} from 'node:timers/promises';
import {fileURLToPath} from 'node:url';
import {build} from './node_modules/esbuild/lib/main.js';

const bundled = await build({entryPoints:[fileURLToPath(new URL('../../apps/studio/next/preview.js',import.meta.url))],
  bundle:true,write:false,format:'iife',plugins:[{name:'worker-host-fixture',setup(builder) {
    builder.onResolve({filter:/^\/host\/ui_next\/worker-host\.js$/},()=>({path:'host',namespace:'fixture'}));
    builder.onLoad({filter:/.*/,namespace:'fixture'},()=>({contents:'export const createWorkerUi = globalThis.createWorkerUi;'}));
  }}]});

function fixture() {
  const workers={started:0,stopped:0}, pending=[], messages=[], window={};
  class Worker {
    listeners=new Set();
    addEventListener(_kind,listener){this.listeners.add(listener);}
    removeEventListener(_kind,listener){this.listeners.delete(listener);}
    postMessage(){}
    terminate(){}
    emit(data){for(const listener of this.listeners)listener({data});}
  }
  runInNewContext(bundled.outputFiles[0].text,{window,Worker,document:{getElementById:()=>({})},
    createWorkerUi(_element,worker) {
      const done=Promise.withResolvers();pending.push({worker,...done});
      return {ready:Promise.resolve(false),done:done.promise};
    }});
  return {workers,pending,messages,start:()=>window.startUiPreview('source',value=>messages.push(value),workers)};
}

test('preview retains a bounded Worker stack without putting it in the user message',async()=>{
  const f=fixture();f.start();
  const stack='Wasm compileProject\n'+'x'.repeat(70_000);
  f.pending[0].worker.emit({kind:'ui-exit',errorStack:stack});
  f.pending[0].reject(new Error('Out of bounds memory access'));
  await setImmediate();
  assert.equal(f.workers.preview.error,stack.slice(0,65536));
  assert.equal(f.messages[0].message,'Out of bounds memory access');
  assert.equal(f.workers.stopped,1);
  assert.equal(f.pending[0].worker.listeners.size,0);
});

test('a late preview failure cannot replace the next run diagnostics',async()=>{
  const f=fixture();f.start();const previous=f.workers.preview;f.start();
  f.pending[0].worker.emit({kind:'ui-exit',errorStack:'previous stack'});
  f.pending[0].reject(new Error('previous run'));
  await setImmediate();
  assert.equal(previous.error,'previous stack');
  assert.equal(f.workers.preview.error,null);
  const failure=new Error('current run');failure.stack='z'.repeat(70_000);
  f.pending[1].reject(failure);
  await setImmediate();
  assert.equal(f.workers.preview.error,failure.stack.slice(0,65536));
  assert.equal(f.workers.started,f.workers.stopped);
});
