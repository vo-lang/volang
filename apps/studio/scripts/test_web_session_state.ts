import assert from 'node:assert/strict';

import { WebSessionState } from '../src/lib/backend/web_session_state.ts';
import type { SessionInfo } from '../src/lib/types.ts';

function session(root: string, workspaceDiscovery: SessionInfo['workspaceDiscovery']): SessionInfo {
  return {
    root,
    origin: 'run-target',
    projectMode: 'single-file',
    entryPath: `${root}/main.vo`,
    singleFileRun: workspaceDiscovery === 'disabled',
    workspaceDiscovery,
    source: { kind: 'path', path: `${root}/main.vo` },
    share: null,
  };
}

const state = new WebSessionState();
const isolatedSession = session('/workspace/project/example', 'disabled');
const isolated = state.prepare(isolatedSession, '/workspace/sessions/isolated');
assert.equal(state.workspaceDiscoveryForPath('/workspace/project/example/main.vo'), 'auto');
assert.equal(state.pendingRoot(isolated.candidate), isolatedSession.root);
assert.equal(state.activate(isolated.candidate).session, isolatedSession);
assert.equal(state.workspaceDiscoveryForPath('/workspace/project/example/main.vo'), 'disabled');
assert.equal(state.workspaceDiscoveryForPath('/workspace/project/other.vo'), 'auto');

const ancestorSession = session('/workspace', 'auto');
const ancestor = state.prepare(ancestorSession, '/workspace/sessions/ancestor');
assert.equal(state.workspaceDiscoveryForPath('/workspace/project/example/main.vo'), 'disabled');
assert.equal(state.activate(ancestor.candidate).session, ancestorSession);
assert.equal(state.workspaceDiscoveryForPath('/workspace/project/example/main.vo'), 'auto');

const forgedPrevious = structuredClone(isolatedSession);
forgedPrevious.entryPath = '/workspace/project/example/forged.vo';
assert.throws(() => state.restore(forgedPrevious), /rollback candidate/);
const restored = state.restore(isolatedSession);
assert.equal(restored.session, isolatedSession);
assert.deepEqual(restored.retiredRoots, ['/workspace/sessions/ancestor']);
assert.equal(state.workspaceDiscoveryForPath('/workspace/project/example/main.vo'), 'disabled');
assert.throws(() => state.restore(isolatedSession), /rollback candidate/);

const discarded = state.prepare(ancestorSession, '/workspace/sessions/discarded');
const forgedCandidate = structuredClone(discarded.candidate);
forgedCandidate.token += '-forged';
assert.throws(() => state.activate(forgedCandidate), /pending candidate/);
assert.equal(state.workspaceDiscoveryForPath('/workspace/project/example/main.vo'), 'disabled');
assert.deepEqual(state.discard(discarded.candidate), ['/workspace/sessions/discarded']);
assert.equal(state.workspaceDiscoveryForPath('/workspace/project/example/main.vo'), 'disabled');
assert.throws(() => state.pendingRoot(discarded.candidate), /no longer available/);

state.prepare(ancestorSession, '/workspace/sessions/first');
const replacement = state.prepare(ancestorSession, '/workspace/sessions/replacement');
assert.deepEqual(replacement.retiredRoots, ['/workspace/sessions/first']);
