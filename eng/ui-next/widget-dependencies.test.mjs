import assert from 'node:assert/strict';
import test from 'node:test';
import {WidgetHost} from '../../lang/crates/vo-web/dist/ui_next/widgets.js';

test('native dependencies notify one owner, ancestors and value updates without scanning callbacks',()=>{
  const parent={parentNode:null},other={parentNode:null},nodes=Array.from({length:1000},()=>({parentNode:parent}));
  const calls=new Map(),host=new WidgetHost(()=>{}, {probe:({element})=>({
    update(){},afterCommit(){calls.set(element,(calls.get(element)??0)+1);},commitTargets:()=>[element],dispose(){},
  })});
  for(let id=0;id<nodes.length;id++)host.apply(id+1,nodes[id],'probe','');
  host.afterCommit(new Set(),true);calls.clear();
  host.afterCommit(new Set([other]));assert.equal(calls.size,0);
  host.afterCommit(new Set([nodes[7]]));assert.deepEqual([...calls],[[nodes[7],1]]);
  host.apply(10,nodes[9],'probe','updated');host.afterCommit(new Set());assert.equal(calls.get(nodes[9]),1);
  calls.clear();host.afterCommit(new Set([parent]));assert.equal(calls.size,1000);
  host.close();calls.clear();host.afterCommit(new Set([parent]));assert.equal(calls.size,0);
});

test('structural commits refresh dependency ancestry and retain default root-wide behavior',()=>{
  const oldParent={parentNode:null},newParent={parentNode:null},control={parentNode:oldParent};
  let targeted=0,global=0;
  const host=new WidgetHost(()=>{}, {
    targeted:()=>({update(){},afterCommit(){targeted++;},commitTargets:()=>[control],dispose(){}}),
    global:()=>({update(){},afterCommit(){global++;},dispose(){}}),
  });
  host.apply(1,control,'targeted','');host.apply(2,{},'global','');host.afterCommit();
  control.parentNode=newParent;host.afterCommit(new Set(),true);
  const before=targeted;host.afterCommit(new Set([oldParent]));assert.equal(targeted,before);
  host.afterCommit(new Set([newParent]));assert.equal(targeted,before+1);assert.equal(global,4);
  host.close();
});

test('callback failure and sibling disposal remove subscriptions before publishing cleanup',()=>{
  const node={parentNode:null},errors=[];let throwNow=false,secondCalls=0,disposed=0;
  const host=new WidgetHost((id,value,error)=>errors.push(error), {
    first:()=>({update(){},commitTargets:()=>[node],afterCommit(){if(throwNow){host.remove(2);throw Error('failed');}},dispose(){disposed++;}}),
    second:()=>({update(){},commitTargets:()=>[node],afterCommit(){secondCalls++;},dispose(){disposed++;}}),
  });
  host.apply(1,node,'first','');host.apply(2,node,'second','');host.afterCommit();
  throwNow=true;host.afterCommit(new Set([node]));host.afterCommit(new Set([node]));
  assert.deepEqual(errors,['failed']);assert.equal(secondCalls,1);assert.equal(disposed,2);
  host.close();assert.equal(disposed,2);
});
