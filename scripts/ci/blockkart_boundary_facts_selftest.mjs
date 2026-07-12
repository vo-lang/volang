#!/usr/bin/env node
import assert from 'node:assert/strict';
import { analyzeBlockKartBoundary } from './blockkart_boundary_facts.mjs';

function analyze(source) {
  return analyzeBlockKartBoundary([{ rel: 'fixture.vo', source }]).wideOwnerParameters;
}

for (const source of [
  `type BlockKartRuntimeContext struct {\n core BlockKartRuntimeCore\n}\ntype Narrow = BlockKartRuntimeContext\nfunc (r *RaceSession) Run(w *Narrow) {}`,
  `type BlockKartRuntimeContext struct {\n core BlockKartRuntimeCore\n}\ntype First = BlockKartRuntimeContext\ntype Second = First\nfunc (r *RaceSession) Run(w *Second) {}`,
  `type BlockKartRuntimeContext struct {\n core BlockKartRuntimeCore\n}\ntype Wrapped struct {\n ctx *BlockKartRuntimeContext\n}\nfunc (r *RaceSession) Run(w *Wrapped) {}`,
  `type BlockKartRuntimeContext struct {\n core BlockKartRuntimeCore\n}\ntype Inner struct {\n ctx *BlockKartRuntimeContext\n}\ntype Outer struct {\n inner *Inner\n}\nfunc (r *RaceSession) Run(w *Outer) {}`,
]) {
  assert.equal(analyze(source).length, 1, source);
}

const broadGroups = `
type Broad struct {
 core *BlockKartRuntimeCore
 input *BlockKartRuntimeInputState
 race *BlockKartRuntimeRaceState
 kart *BlockKartRuntimeKartState
}
func (r *RaceSession) Run(w *Broad) {}
`;
assert.equal(analyze(broadGroups).length, 1);

const broadExplicitParameters = `
func (r *RaceSession) Run(core *BlockKartRuntimeCore, input *BlockKartRuntimeInputState, race *BlockKartRuntimeRaceState, kart *BlockKartRuntimeKartState) {}
`;
assert.equal(analyze(broadExplicitParameters).length, 1);

const narrow = `
type Narrow struct {
 race *BlockKartRuntimeRaceState
 kart *BlockKartRuntimeKartState
}
func (r *RaceSession) Run(w *Narrow) {}
`;
assert.deepEqual(analyze(narrow), []);
console.log('blockkart boundary facts selftest: ok');
