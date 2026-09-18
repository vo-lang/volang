import test from 'node:test';
import assert from 'node:assert/strict';
import {createRunTask} from '../../apps/studio/next/run-task.js';

function fixture(deadlines = {loading:1000,compiling:1000,running:1000}) {
  const counters = {started:0,stopped:0}, controller = new AbortController();
  const worker = {terminated:0,terminate(){this.terminated++;},postMessage(){}};
  const run = createRunTask(counters,{createWorker:()=>worker,deadlines});
  return {counters,controller,worker,run};
}
test('run phases settle once and release the worker', async () => {
  const f=fixture(), result=f.run('source',f.controller.signal);
  f.worker.onmessage({data:{kind:'phase',phase:'compiling'}});
  f.worker.onmessage({data:{kind:'phase',phase:'running'}});
  f.worker.onmessage({data:{kind:'result',output:'done'}});
  assert.equal(JSON.parse(await result).output,'done');
  f.controller.abort();
  assert.deepEqual(f.counters,{started:1,stopped:1});
  assert.equal(f.worker.terminated,1);
});
test('cancelled input creates no worker; active cancellation terminates one', async () => {
  const f=fixture();f.controller.abort();
  await assert.rejects(f.run('source',f.controller.signal),/stopped/);
  assert.equal(f.counters.started,0);
  const g=fixture(), result=g.run('source',g.controller.signal);g.controller.abort();
  await assert.rejects(result,/stopped/);assert.equal(g.worker.terminated,1);
});
test('execution receives its own deadline after compiler preparation', async () => {
  const f=fixture({loading:5,compiling:1000,running:10});
  const result=f.run('source',f.controller.signal);
  f.worker.onmessage({data:{kind:'phase',phase:'compiling'}});
  await new Promise(resolve=>setTimeout(resolve,20));
  assert.equal(f.worker.terminated,0);
  f.worker.onmessage({data:{kind:'phase',phase:'running'}});
  await assert.rejects(result,/Execution exceeded/);
  assert.equal(f.worker.terminated,1);
});
test('duplicate phases cannot extend the deadline', async () => {
  const f=fixture(), result=f.run('source',f.controller.signal);
  f.worker.onmessage({data:{kind:'phase',phase:'compiling'}});
  f.worker.onmessage({data:{kind:'phase',phase:'compiling'}});
  await assert.rejects(result,/unreadable/);assert.equal(f.worker.terminated,1);
});
test('worker message failures release the worker', async () => {
  const f=fixture();f.worker.postMessage=()=>{throw new Error('send failed');};
  await assert.rejects(f.run('source',f.controller.signal),/send failed/);
  assert.equal(f.worker.terminated,1);
});
