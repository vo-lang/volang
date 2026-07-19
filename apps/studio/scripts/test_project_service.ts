import assert from 'node:assert/strict';

import type { Backend } from '../src/lib/backend/backend.ts';
import { ProjectService, type BuiltinExampleSessionSpec } from '../src/lib/services/project_service.ts';
import { WorkspaceService } from '../src/lib/services/workspace_service.ts';
import { parseHash, resolveDocsFile } from '../src/lib/router.ts';
import type {
  BackendPlatform,
  BootstrapContext,
  LaunchSpec,
  PreparedSession,
  SessionInfo,
} from '../src/lib/types.ts';

type BackendEvent =
  | { kind: 'create'; files: { path: string; content: string }[] }
  | { kind: 'prepare'; spec: LaunchSpec }
  | { kind: 'activate'; candidate: PreparedSession }
  | { kind: 'discard'; candidate: PreparedSession }
  | { kind: 'restore'; previous: SessionInfo };

function createFixture(workspaceRoot = '/workspace/', platform: BackendPlatform = 'wasm') {
  const events: BackendEvent[] = [];
  const controls: {
    createError: Error | null;
    prepareError: Error | null;
    activateError: Error | null;
    sessionOverride: SessionInfo | null;
  } = {
    createError: null,
    prepareError: null,
    activateError: null,
    sessionOverride: null,
  };
  const bootstrap: BootstrapContext = {
    workspaceRoot,
    launch: null,
    mode: 'dev',
    platform,
  };
  let nextToken = 0;
  let pending: PreparedSession | null = null;
  let active: SessionInfo | null = null;
  let rollback: SessionInfo | null = null;
  const backend = {
    platform,
    async getBootstrapContext() {
      return bootstrap;
    },
    async createWorkspaceFiles(files: { path: string; content: string }[]) {
      events.push({ kind: 'create', files });
      if (controls.createError) throw controls.createError;
    },
    async openSession() {
      throw new Error('ProjectService must use the two-phase session API');
    },
    async prepareSession(spec: LaunchSpec) {
      events.push({ kind: 'prepare', spec });
      if (controls.prepareError) throw controls.prepareError;
      assert.ok(spec.proj);
      const session = controls.sessionOverride ?? {
        root: spec.proj.slice(0, spec.proj.lastIndexOf('/')),
        origin: 'run-target',
        projectMode: 'single-file',
        entryPath: spec.proj,
        singleFileRun: true,
        workspaceDiscovery: spec.isolation === 'single-file' ? 'disabled' : 'auto',
        source: { kind: 'path', path: spec.proj },
        share: null,
      } satisfies SessionInfo;
      pending = { token: `candidate-${++nextToken}`, session };
      return pending;
    },
    async activateSession(candidate: PreparedSession) {
      events.push({ kind: 'activate', candidate });
      if (controls.activateError) throw controls.activateError;
      assert.deepEqual(candidate, pending);
      rollback = active;
      active = candidate.session;
      pending = null;
      return active;
    },
    async restoreSession(previous: SessionInfo) {
      events.push({ kind: 'restore', previous });
      assert.deepEqual(previous, rollback);
      active = previous;
      rollback = null;
      return active;
    },
    async discardPreparedSession(candidate: PreparedSession) {
      events.push({ kind: 'discard', candidate });
      assert.deepEqual(candidate, pending);
      pending = null;
    },
    async listPreparedSessionDir() {
      return [];
    },
    async readPreparedSessionFile() {
      return 'module main';
    },
  } as unknown as Backend;
  const workspace = new WorkspaceService(backend);
  const project = new ProjectService(backend, workspace);
  return {
    controls,
    events,
    project,
    workspace,
    activeSession: () => active,
    pendingSession: () => pending,
  };
}

const first = createFixture();
await assert.rejects(
  first.project.openBuiltinExampleSession({
    id: 'channels',
    entryName: 'channels.vo',
    content: 'module main',
  }),
  /Bootstrap context has not been loaded/,
);
await first.project.initialize();
const session = await first.project.openBuiltinExampleSession({
  id: 'channels',
  entryName: 'channels.vo',
  content: 'module main',
});
const channelsPath = '/workspace/.volang/apps/studio/sessions/examples/channels/channels.vo';
assert.deepEqual(first.events, [
  { kind: 'create', files: [{ path: channelsPath, content: 'module main' }] },
  { kind: 'prepare', spec: { proj: channelsPath, mode: 'dev', isolation: 'single-file' } },
  { kind: 'activate', candidate: { token: 'candidate-1', session } },
]);
assert.deepEqual(session, {
  root: '/workspace/.volang/apps/studio/sessions/examples/channels',
  origin: 'run-target',
  projectMode: 'single-file',
  entryPath: channelsPath,
  singleFileRun: true,
  workspaceDiscovery: 'disabled',
  source: { kind: 'path', path: channelsPath },
  share: null,
});
assert.equal(first.project.sessionInfo, session);
assert.equal(first.workspace.session, session);

const native = createFixture('/workspace/module-root', 'native');
await native.project.initialize();
const nativeSession = await native.project.openBuiltinExampleSession({
  id: 'channels',
  entryName: 'channels.vo',
  content: 'module main',
});
assert.deepEqual(
  {
    root: nativeSession.root,
    entryPath: nativeSession.entryPath,
    origin: nativeSession.origin,
    projectMode: nativeSession.projectMode,
    singleFileRun: nativeSession.singleFileRun,
    workspaceDiscovery: nativeSession.workspaceDiscovery,
  },
  {
    root: '/workspace/module-root/.volang/apps/studio/sessions/examples/channels',
    entryPath: '/workspace/module-root/.volang/apps/studio/sessions/examples/channels/channels.vo',
    origin: 'run-target',
    projectMode: 'single-file',
    singleFileRun: true,
    workspaceDiscovery: 'disabled',
  },
);
assert.equal(native.workspace.session?.workspaceDiscovery, 'disabled');

const linkedWorkspace = createFixture('/linked/workspace', 'native');
await linkedWorkspace.project.initialize();
const linkedEntry = '/real/workspace/.volang/apps/studio/sessions/examples/channels/channels.vo';
linkedWorkspace.controls.sessionOverride = {
  root: '/real/workspace/.volang/apps/studio/sessions/examples/channels',
  origin: 'run-target',
  projectMode: 'single-file',
  entryPath: linkedEntry,
  singleFileRun: true,
  workspaceDiscovery: 'disabled',
  source: { kind: 'path', path: linkedEntry },
  share: null,
};
const linkedSession = await linkedWorkspace.project.openBuiltinExampleSession({
  id: 'channels',
  entryName: 'channels.vo',
  content: 'module main',
});
assert.equal(linkedSession.root, '/real/workspace/.volang/apps/studio/sessions/examples/channels');
assert.equal(linkedWorkspace.activeSession(), linkedSession);

const windowsNative = createFixture('C:\\Users\\Vo\\workspace\\', 'native');
await windowsNative.project.initialize();
windowsNative.controls.sessionOverride = {
  root: '\\\\?\\C:\\Users\\Vo\\workspace\\.volang\\apps\\studio\\sessions\\examples\\channels',
  origin: 'run-target',
  projectMode: 'single-file',
  entryPath: '\\\\?\\C:\\Users\\Vo\\workspace\\.volang\\apps\\studio\\sessions\\examples\\channels\\channels.vo',
  singleFileRun: true,
  workspaceDiscovery: 'disabled',
  source: null,
  share: null,
};
const windowsSession = await windowsNative.project.openBuiltinExampleSession({
  id: 'channels',
  entryName: 'channels.vo',
  content: 'module main',
});
assert.equal(windowsNative.workspace.session, windowsSession);

const isolated = createFixture('/workspace');
await isolated.project.initialize();
await isolated.project.openBuiltinExampleSession({
  id: 'error_handling',
  entryName: 'error_handling.vo',
  content: 'first',
});
await isolated.project.openBuiltinExampleSession({
  id: 'time',
  entryName: 'time.vo',
  content: 'second',
});
const createdPaths = isolated.events
  .filter((event): event is Extract<BackendEvent, { kind: 'create' }> => event.kind === 'create')
  .map((event) => event.files[0]?.path);
assert.deepEqual(createdPaths, [
  '/workspace/.volang/apps/studio/sessions/examples/error_handling/error_handling.vo',
  '/workspace/.volang/apps/studio/sessions/examples/time/time.vo',
]);

const transaction = createFixture('/workspace');
await transaction.project.initialize();
const transactionPrevious = await transaction.project.openSession({
  proj: '/workspace/previous.vo',
  mode: 'dev',
});
const transactionCandidate = await transaction.project.prepareSession({
  proj: '/workspace/next.vo',
  mode: 'dev',
});
assert.equal(transaction.project.sessionInfo, transactionPrevious);
assert.equal(transaction.workspace.session, transactionPrevious);
assert.equal(transaction.activeSession(), transactionPrevious);
assert.equal(transaction.pendingSession(), transactionCandidate);
assert.deepEqual(
  await transaction.project.listPreparedSessionDir(transactionCandidate, transactionCandidate.session.root),
  [],
);
assert.equal(
  await transaction.project.readPreparedSessionFile(transactionCandidate, transactionCandidate.session.entryPath!),
  'module main',
);
await transaction.project.discardPreparedSession(transactionCandidate);
assert.equal(transaction.pendingSession(), null);
assert.equal(transaction.activeSession(), transactionPrevious);

const activatedCandidate = await transaction.project.prepareSession({
  proj: '/workspace/next.vo',
  mode: 'dev',
});
const activatedSession = await transaction.project.activatePreparedSession(activatedCandidate);
assert.equal(transaction.project.sessionInfo, activatedSession);
assert.equal(transaction.workspace.session, activatedSession);
assert.equal(transaction.activeSession(), activatedSession);
const restoredSession = await transaction.project.restoreSession(transactionPrevious);
assert.deepEqual(restoredSession, transactionPrevious);
assert.equal(transaction.project.sessionInfo, transactionPrevious);
assert.equal(transaction.workspace.session, transactionPrevious);
assert.equal(transaction.activeSession(), transactionPrevious);

const invalid = createFixture();
await invalid.project.initialize();
for (const spec of [
  { id: '', entryName: 'main.vo', content: '' },
  { id: '../escape', entryName: 'main.vo', content: '' },
  { id: 'Example', entryName: 'main.vo', content: '' },
  { id: 'con', entryName: 'main.vo', content: '' },
  { id: 'safe', entryName: '../main.vo', content: '' },
  { id: 'safe', entryName: 'nested/main.vo', content: '' },
  { id: 'safe', entryName: 'main.txt', content: '' },
  { id: 'safe', entryName: 'main..vo', content: '' },
  { id: 'safe', entryName: 'nul.vo', content: '' },
] satisfies BuiltinExampleSessionSpec[]) {
  await assert.rejects(invalid.project.openBuiltinExampleSession(spec), /Invalid built-in example/);
}
await assert.rejects(
  invalid.project.openBuiltinExampleSession({
    id: 'safe',
    entryName: 'main.vo',
    content: null,
  } as unknown as BuiltinExampleSessionSpec),
  /Invalid built-in example content/,
);
assert.deepEqual(invalid.events, []);

const createFailure = createFixture();
await createFailure.project.initialize();
createFailure.controls.createError = new Error('create failed');
await assert.rejects(
  createFailure.project.openBuiltinExampleSession({ id: 'channels', entryName: 'channels.vo', content: '' }),
  /create failed/,
);
assert.equal(createFailure.project.sessionInfo, null);
assert.equal(createFailure.workspace.session, null);
assert.deepEqual(createFailure.events.map((event) => event.kind), ['create']);

const prepareFailure = createFixture();
await prepareFailure.project.initialize();
const previousSession = await prepareFailure.project.openSession({ proj: '/workspace/previous.vo', mode: 'dev' });
prepareFailure.events.length = 0;
prepareFailure.controls.prepareError = new Error('prepare failed');
await assert.rejects(
  prepareFailure.project.openBuiltinExampleSession({ id: 'channels', entryName: 'channels.vo', content: '' }),
  /prepare failed/,
);
assert.equal(prepareFailure.project.sessionInfo, previousSession);
assert.equal(prepareFailure.workspace.session, previousSession);
assert.equal(prepareFailure.activeSession(), previousSession);
assert.deepEqual(prepareFailure.events.map((event) => event.kind), ['create', 'prepare']);

const activationFailure = createFixture();
await activationFailure.project.initialize();
const activationPrevious = await activationFailure.project.openSession({
  proj: '/workspace/previous.vo',
  mode: 'dev',
});
activationFailure.events.length = 0;
activationFailure.controls.activateError = new Error('activation failed');
await assert.rejects(
  activationFailure.project.openBuiltinExampleSession({
    id: 'channels',
    entryName: 'channels.vo',
    content: '',
  }),
  /activation failed/,
);
assert.equal(activationFailure.project.sessionInfo, activationPrevious);
assert.equal(activationFailure.workspace.session, activationPrevious);
assert.equal(activationFailure.activeSession(), activationPrevious);
assert.equal(activationFailure.pendingSession(), null);
assert.deepEqual(
  activationFailure.events.map((event) => event.kind),
  ['create', 'prepare', 'activate', 'discard'],
);

const invalidContract = createFixture();
await invalidContract.project.initialize();
invalidContract.controls.sessionOverride = {
  root: '/workspace/.volang/apps/studio/sessions/examples/channels',
  origin: 'run-target',
  projectMode: 'single-file',
  entryPath: channelsPath,
  singleFileRun: true,
  workspaceDiscovery: 'auto',
  source: { kind: 'path', path: channelsPath },
  share: null,
};
await assert.rejects(
  invalidContract.project.openBuiltinExampleSession({ id: 'channels', entryName: 'channels.vo', content: '' }),
  /invalid built-in example session/,
);
assert.equal(invalidContract.project.sessionInfo, null);
assert.equal(invalidContract.workspace.session, null);
assert.equal(invalidContract.activeSession(), null);
assert.equal(invalidContract.pendingSession(), null);
assert.deepEqual(invalidContract.events.map((event) => event.kind), ['create', 'prepare', 'discard']);

assert.deepEqual(parseHash(''), { mode: 'manage', docsPath: null, exampleId: null });
assert.deepEqual(parseHash('#/'), { mode: 'manage', docsPath: null, exampleId: null });
assert.deepEqual(parseHash('#/dev'), { mode: 'develop', docsPath: null, exampleId: null });
assert.deepEqual(parseHash('#/develop'), { mode: 'develop', docsPath: null, exampleId: null });
assert.deepEqual(parseHash('#/develop?example=channels'), {
  mode: 'develop',
  docsPath: null,
  exampleId: 'channels',
});
assert.deepEqual(parseHash('#/docs'), { mode: 'docs', docsPath: null, exampleId: null });
assert.deepEqual(parseHash('#/docs/advanced/modules'), {
  mode: 'docs',
  docsPath: 'advanced/modules',
  exampleId: null,
});
assert.deepEqual(parseHash('#/unknown'), { mode: 'manage', docsPath: null, exampleId: null });
assert.equal(resolveDocsFile('advanced/modules'), 'advanced/modules.md');
assert.equal(resolveDocsFile('advanced/modules.md'), 'advanced/modules.md');

console.log('studio site contracts: ok');
