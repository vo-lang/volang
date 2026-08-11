import assert from 'node:assert/strict';
import { createServer } from 'vite';

class MemoryStorage extends Map<string, string> implements Storage {
  get length(): number {
    return this.size;
  }

  key(index: number): string | null {
    return [...this.keys()][index] ?? null;
  }

  getItem(key: string): string | null {
    return this.get(key) ?? null;
  }

  setItem(key: string, value: string): void {
    this.set(key, value);
  }

  removeItem(key: string): void {
    this.delete(key);
  }
}

const storage = new MemoryStorage();
Object.defineProperty(globalThis, 'localStorage', { configurable: true, value: storage });

const server = await createServer({
  root: process.cwd(),
  server: { middlewareMode: true },
  appType: 'custom',
  logLevel: 'silent',
});

try {
  const { WebBackend } = await server.ssrLoadModule('/src/lib/backend/web_backend.ts');
  const { ProjectCatalogService } = await server.ssrLoadModule(
    '/src/lib/services/project_catalog_service.ts',
  );
  const { RuntimeService } = await server.ssrLoadModule('/src/lib/services/runtime_service.ts');

  const web = new WebBackend();
  const createOnlyPath = '/workspace/create-only-contract.vo';
  await web.createProjectFile(createOnlyPath, 'first\n');
  await assert.rejects(web.createProjectFile(createOnlyPath, 'second\n'), /file already exists/);
  assert.equal(await web.readFile(createOnlyPath), 'first\n');

  const missingPath = '/workspace/missing/create-only-contract.vo';
  await assert.rejects(web.createProjectFile(missingPath, 'missing\n'), /file does not exist/);
  await assert.rejects(web.readFile(missingPath), /File not found/);

  const concurrentPath = '/workspace/concurrent-create-only-contract.vo';
  const concurrentContents = ['alpha\n', 'beta\n'];
  const results = await Promise.allSettled(
    concurrentContents.map((content) => web.createProjectFile(concurrentPath, content)),
  );
  assert.equal(results.filter((result) => result.status === 'fulfilled').length, 1);
  assert.equal(results.filter((result) => result.status === 'rejected').length, 1);
  const winner = results.findIndex((result) => result.status === 'fulfilled');
  assert.equal(await web.readFile(concurrentPath), concurrentContents[winner]);

  storage.setItem('vo_studio_project_config_v1', '{"sentinel":{"hasGui":true}}');
  storage.setItem(
    'vo_studio_recent_projects_v1',
    '[{"name":"sentinel","type":"single","localPath":"/sentinel.vo","entryPath":"/sentinel.vo","openedAt":1}]',
  );
  const storageBefore = [...storage.entries()];
  const createCalls: { path: string; content: string }[] = [];
  const catalogBackend = {
    platform: 'wasm',
    async createProjectFile(path: string, content: string) {
      createCalls.push({ path, content });
      throw new Error('file already exists');
    },
  };
  const catalog = new ProjectCatalogService(catalogBackend);
  let catalogState: unknown;
  const unsubscribe = catalog.catalog.subscribe((state: unknown) => {
    catalogState = structuredClone(state);
  });
  const catalogBefore = structuredClone(catalogState);
  let refreshCalls = 0;
  catalog.refresh = async () => {
    refreshCalls += 1;
  };

  await assert.rejects(catalog.createSingleProject('blocked', '/external'), /file already exists/);
  assert.deepEqual(createCalls.map(({ path }) => path), ['/external/blocked.vo']);
  assert.equal(refreshCalls, 0);
  assert.deepEqual([...storage.entries()], storageBefore);
  assert.deepEqual(catalogState, catalogBefore);
  unsubscribe();

  const runCalls: { target: string; sessionId: number }[] = [];
  const stopCalls: number[] = [];
  let releaseFirstRun: (() => void) | null = null;
  const runtimeBackend = {
    platform: 'wasm',
    setGuiGuestExitHandler() {},
    setGuiGuestErrorHandler() {},
    async runGui(target: string, session: { id: number }) {
      runCalls.push({ target, sessionId: session.id });
      return new Promise((resolve) => {
        releaseFirstRun = () => resolve({});
      });
    },
    async stopGui(session: { id: number }) {
      stopCalls.push(session.id);
    },
  };
  const runtime = new RuntimeService(runtimeBackend);
  const firstRun = runtime.runGuiPreview('/workspace/first.vo');
  await Promise.resolve();
  assert.deepEqual(runCalls, [{ target: '/workspace/first.vo', sessionId: 1 }]);

  const queuedRun = runtime.runGuiPreview('/workspace/queued.vo');
  await runtime.stopGui();
  assert.deepEqual(stopCalls, [1, 2]);
  const outcomesPromise = Promise.allSettled([firstRun, queuedRun]);
  assert.ok(releaseFirstRun);
  releaseFirstRun();
  const outcomes = await outcomesPromise;

  assert.deepEqual(outcomes.map(({ status }) => status), ['rejected', 'rejected']);
  for (const outcome of outcomes) {
    if (outcome.status === 'rejected') {
      assert.match(String(outcome.reason), /GUI session superseded/);
    }
  }
  assert.deepEqual(runCalls, [{ target: '/workspace/first.vo', sessionId: 1 }]);
  assert.deepEqual(stopCalls, [1, 2, 1]);
  assert.deepEqual(runtime.listGuiPreviews(), []);
} finally {
  await server.close();
  Reflect.deleteProperty(globalThis, 'localStorage');
}

console.log('project creation and runtime contracts: ok');
