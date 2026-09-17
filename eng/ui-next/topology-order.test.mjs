import assert from 'node:assert/strict';
import test from 'node:test';
import {prepareBatch} from '../../lang/crates/vo-web/dist/ui_next/topology.js';
import {WIRE_VERSION} from '../../lang/crates/vo-web/dist/ui_next/generated/protocol.js';

const namespace = 'http://www.w3.org/1999/xhtml';
const mutation = (op, id, parent = 0, before = 0, name = '', value = '') => ({op,id,parent,before,name,value});
function fixture(width) {
  const entry = (kind, parent, children = []) => Object.freeze({kind,parent,namespace,children:Object.freeze(children)});
  const nodes = new Map([[0,entry('#root',null,[1,2])], [1,entry('div',0,Array.from({length:width},(_,i)=>i+3))], [2,entry('div',0)]]);
  for(let id=3;id<width+3;id++)nodes.set(id,entry('#text',1));
  return {nodes,revision:0,inputSequence:0,sequence:0,highestId:width+2,hydrating:false};
}
const prepare = (state, mutations) => prepareBatch({version:WIRE_VERSION,revision:1,inputSequence:0,mutations},state,{supports:()=>false});

test('text updates borrow wide ancestor children and never mutate the live tree',()=>{
  const state=fixture(10000),result=prepare(state,[mutation('text',3,0,0,'','changed')]);
  assert.equal(result.shapes.get(3).text,'changed');
  assert.equal(result.shapes.get(1).children,state.nodes.get(1).children);
  assert.equal(result.shapes.get(0).children,state.nodes.get(0).children);
  assert.equal(state.nodes.get(3).text,undefined);
});

test('wide reversal materializes one final order with stable identities',()=>{
  const state=fixture(10000),original=state.nodes.get(1).children;
  const result=prepare(state,original.map((id,index)=>mutation('insert',id,1,index ? id-1 : 0)));
  assert.deepEqual(result.shapes.get(1).children,[...original].reverse());
  assert.equal(state.nodes.get(1).children,original);
  assert.equal(original[0],3);
});

test('interleaved reparenting and anchors match sequential sibling semantics',()=>{
  const state=fixture(64),lists=new Map([[1,[...state.nodes.get(1).children]],[2,[]]]),parents=new Map([...state.nodes].map(([id,value])=>[id,value.parent]));
  let seed=12345;
  const random=limit=>{seed=(Math.imul(seed,1664525)+1013904223)>>>0;return seed%limit;};
  const mutations=[];
  for(let step=0;step<1000;step++) {
    const id=3+random(64),parent=1+random(2),source=lists.get(parents.get(id));
    source.splice(source.indexOf(id),1);
    const destination=lists.get(parent),offset=random(destination.length+1),before=destination[offset]??0;
    destination.splice(offset,0,id);parents.set(id,parent);mutations.push(mutation('insert',id,parent,before));
  }
  const result=prepare(state,mutations);
  for(const [id,list] of lists)assert.deepEqual(result.shapes.get(id).children,list);
  for(let id=3;id<67;id++)assert.equal(result.shapes.get(id).parent,parents.get(id));
  assert.throws(()=>prepare(state,[...mutations,mutation('insert',1,1)]),/cyclic/);
  assert.deepEqual(state.nodes.get(2).children,[],'rejected batch changed its input');
});

test('removal follows the current batch order after nodes change parent',()=>{
  const state=fixture(3);
  const result=prepare(state,[mutation('insert',3,2),mutation('remove',1)]);
  assert.deepEqual(result.shapes.get(2).children,[3]);
  for(const id of [1,4,5])assert.equal(result.shapes.get(id),null);
  assert.equal(result.shapes.get(3).parent,2);
  assert.deepEqual(result.shapes.get(0).children,[2]);
});
