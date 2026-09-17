import assert from 'node:assert/strict';
import test from 'node:test';
import {prepareBatch} from '../../lang/crates/vo-web/dist/ui_next/topology.js';
import {htmlNamespace} from '../../lang/crates/vo-web/dist/ui_next/elements.js';
import {WIRE_VERSION, MAX_PORTALS} from '../../lang/crates/vo-web/dist/ui_next/generated/protocol.js';

const mutation = (op, id, parent = 0, name = '', before = 0) => ({op,id,parent,before,name,value:''});
const create = (id, parent = 0, kind = 'div') => [mutation('create', id, 0, kind),mutation('insert', id, parent)];
const fresh = () => ({nodes:new Map([[0,{kind:'#root',namespace:htmlNamespace,parent:null,children:[]}]]),
  portals:new Map(), revision:0,inputSequence:0,sequence:0,highestId:0,hydrating:false});
const prepare = (state, mutations) => prepareBatch({version:WIRE_VERSION,revision:state.revision+1,inputSequence:0,mutations,commands:[]}, state, {supports:() => true});
const committed = (state, prepared) => ({...state, revision:state.revision+1,highestId:prepared.highest,
  portals:prepared.portals ?? new Map(), nodes:new Map([...state.nodes,...prepared.shapes].filter(([,shape]) => shape))});

test('portal placement resolves the final target while preserving the logical tree', () => {
  const state=fresh(), first=prepare(state,[...create(1),mutation('portal',1,2),...create(2)]);
  assert.equal(first.portals.get(1),2);
  assert.equal(first.shapes.get(1).parent,0);
  assert.deepEqual(state.nodes.get(0).children,[],'preflight mutated the current root');
  const live=committed(state,first);
  const missing=prepare(live,[mutation('remove',2)]);
  assert.equal(missing.portals.size,0);
  assert.equal((missing.shapes.get(1) ?? live.nodes.get(1)).parent,0);
  const inline=prepare(live,[mutation('portal',1)]);
  assert.equal(inline.portals.size,0);
  assert.deepEqual([...live.portals],[[1,2]],'preflight changed current placements');
  const ordinary=prepare(fresh(),create(1));
  assert.equal(ordinary.portals,undefined,'ordinary batches entered portal bookkeeping');
});

test('self, descendant, cross-portal and later structural cycles are rejected atomically', () => {
  const invalid=[
    [...create(1),mutation('portal',1,1)],
    [...create(1),...create(2,1),mutation('portal',1,2)],
    [...create(1),...create(2),mutation('portal',1,2),mutation('portal',2,1)],
    [...create(1),...create(2),...create(3,1),...create(4,2),mutation('portal',1,4),mutation('portal',2,3)],
  ];
  for(const mutations of invalid) assert.throws(()=>prepare(fresh(),mutations),/own content|[Cc]yclic/);
  const state=fresh();
  const live=committed(state,prepare(state,[...create(1),...create(2),...create(3),mutation('portal',1,2)]));
  assert.throws(()=>prepare(live,[mutation('insert',2,1)]),/own content|[Cc]yclic/);
  assert.deepEqual(live.nodes.get(0).children,[1,2,3]);
  assert.deepEqual([...live.portals],[[1,2]]);
});

test('portal targets and counts use the same bounded HTML contract as the guest', () => {
  for(const kind of ['#fragment','#widget','#text','input','textarea','svg']) {
    assert.throws(()=>prepare(fresh(),[...create(1),...create(2,0,kind),mutation('portal',1,2)]),/HTML child container/);
  }
  assert.throws(()=>prepare(fresh(),[...create(1,0,'svg'),...create(2),mutation('portal',1,2)]),/HTML element/);
  assert.throws(()=>prepare(fresh(),[...create(1),mutation('portal',1,9)]),/unknown UI node/);
  assert.throws(()=>prepare(fresh(),[...create(1),...create(2),mutation('portal',1,2),mutation('remove',2)]),/removed UI node/);
  assert.throws(()=>prepare(fresh(),[...create(1),mutation('portal',1,0,'unexpected')]),/Invalid UI portal/);
  const mutations=create(1);
  for(let id=2;id<MAX_PORTALS+2;id++) mutations.push(...create(id),mutation('portal',id,1));
  assert.equal(prepare(fresh(),mutations).portals.size,MAX_PORTALS);
  mutations.push(...create(MAX_PORTALS+2),mutation('portal',MAX_PORTALS+2,1));
  assert.throws(()=>prepare(fresh(),mutations),/portal count/);
});
