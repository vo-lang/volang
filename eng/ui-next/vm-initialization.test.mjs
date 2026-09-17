import assert from 'node:assert/strict';
import test from 'node:test';
import {initializeUiVm} from '../../lang/crates/vo-web/dist/ui_next/vm.js';

test('concurrent roots and module wrappers share initialization without sharing Island lifetimes', async () => {
  let finish, calls=0, otherCalls=0;
  const pending=new Promise(resolve=>{finish=resolve;});
  const runtime={default() {assert.equal(this,runtime);calls++;return pending;}};
  const first=initializeUiVm(runtime), second=initializeUiVm({...runtime});
  assert.equal(first,second);
  const independent=initializeUiVm({default:async()=>{otherCalls++;return 'other';}});
  assert.equal(await independent,'other');assert.equal(otherCalls,1);assert.equal(calls,1);
  finish('initialized');assert.deepEqual(await Promise.all([first,second]),['initialized','initialized']);
  assert.equal(await initializeUiVm(runtime),'initialized');assert.equal(calls,1);
});

test('a failed shared initialization rejects all waiting roots and permits a later retry', async () => {
  let calls=0;
  const failure=new Error('Runtime download interrupted.');
  const runtime={default() {if (++calls === 1) throw failure;return Promise.resolve('retried');}};
  const results=await Promise.allSettled([initializeUiVm(runtime),initializeUiVm({...runtime})]);
  assert.deepEqual(results,[{status:'rejected',reason:failure},{status:'rejected',reason:failure}]);
  assert.equal(calls,1);assert.equal(await initializeUiVm(runtime),'retried');assert.equal(calls,2);
});
