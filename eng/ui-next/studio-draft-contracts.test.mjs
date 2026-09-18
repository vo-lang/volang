import assert from 'node:assert/strict';
import test from 'node:test';
import {waitStudioDraft} from './studio-draft-contracts.mjs';

test('draft polling awaits committed values, including an empty draft',async()=>{
  const values=[null,'previous draft',''];
  let reads=0;
  const page={async evaluate(_query,{key}){
    assert.equal(key,'ui-draft');
    return values[reads++];
  }};
  await waitStudioDraft(page,'','ui-draft');
  assert.equal(reads,3);
});

test('draft polling fails when the expected write never commits',async()=>{
  const page={async evaluate(){return 'previous draft';}};
  await assert.rejects(waitStudioDraft(page,'new draft','ui-draft',{timeout:0}),/did not commit/);
});

test('draft polling preserves storage failures',async()=>{
  const failure=new Error('storage unavailable');
  const page={async evaluate(){throw failure;}};
  await assert.rejects(waitStudioDraft(page,''),error=>error===failure);
});
