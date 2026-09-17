import assert from 'node:assert/strict';

export async function checkWorkerUi(page, url) {
  await page.route('**/worker-contracts', route => route.fulfill({ contentType: 'text/html', body: '<!doctype html><title>Worker UI contracts</title>' }));
  await page.goto(`${url}/worker-contracts`);
  const result = await page.evaluate(async () => {
    const { createWorkerUi } = await import('/host/ui_next/worker-host.js');
    const { encodeBatch } = await import('/host/ui_next/generated/codec.js');
    const { WIRE_VERSION } = await import('/host/ui_next/generated/protocol.js');
    const require = (ok, message) => { if (!ok) throw new Error(message); };
    const script = URL.createObjectURL(new Blob([`
      self.onmessage = async event => {
        self.onmessage = null;
        const request = event.data;
        if (request.mode === 'loop') { for (;;) {} }
        if (request.mode === 'empty') {
          self.postMessage({ kind: 'ui-exchange', sequence: 1, bytes: new Uint8Array() });
          return;
        }
        if (request.mode === 'overlap') {
          self.postMessage({ kind: 'ui-exchange', sequence: 1, bytes: new Uint8Array() });
          self.postMessage({ kind: 'ui-exchange', sequence: 2, bytes: new Uint8Array() });
          return;
        }
        if (request.mode === 'turn-loop') {
          self.onmessage = () => { for (;;) {} };
          self.postMessage({ kind: 'ui-exchange', sequence: 1, bytes: request.bytes });
          return;
        }
        try {
          const runtime = await import('${location.origin}/wasm/vo_web.js');
          const { runWorkerVm } = await import('${location.origin}/host/ui_next/worker-vm.js');
          await runtime.default();
          const bytes = new Uint8Array(await (await fetch('${location.origin}/artifacts/interaction.vob')).arrayBuffer());
          const vm = new runtime.VoVmIsland(bytes);
          try { await runWorkerVm(vm, self); } finally { vm.free(); }
        } catch (error) { self.postMessage({ kind: 'ui-exit', error: String(error) }); }
      };
    `], { type: 'text/javascript' }));
    const sessions = [];
    const start = (mode, options = {}, bytes) => {
      const container = document.body.appendChild(document.createElement('section'));
      const worker = new Worker(script, { type: 'module' });
      let terminations = 0;
      const terminate = worker.terminate.bind(worker);
      worker.terminate = () => { terminations++; terminate(); };
      const app = createWorkerUi(container, worker, options);
      worker.postMessage({ mode, bytes });
      const result = { ...app, container, terminated: () => terminations };
      sessions.push(result);
      return result;
    };
    try {
      const a = start('vm'), b = start('vm');
      require(await a.ready && await b.ready, 'worker VM did not commit and become interactive');
      window.dispatchEvent(new PageTransitionEvent('pagehide', {persisted:true}));
      window.dispatchEvent(new PageTransitionEvent('pageshow', {persisted:true}));
      require(a.terminated() === 0 && b.terminated() === 0, 'persisted page event terminated a live worker');
      require(a.container.querySelector('[data-lifecycle]').textContent === 'Ready to explore', 'worker readiness preceded effects');
      const counter = a.container.querySelector('[data-counter=Alpha]');
      for (let i = 0; i < 25; i++) counter.click();
      await new Promise((resolve, reject) => {
        const timeout = setTimeout(() => { observer.disconnect(); reject(new Error('worker events did not settle')); }, 10000);
        const observer = new MutationObserver(() => {
          if (counter.textContent === 'Alpha: 25') { clearTimeout(timeout); observer.disconnect(); resolve(); }
        });
        observer.observe(counter, { subtree: true, characterData: true });
      });
      require(b.container.querySelector('[data-counter=Alpha]').textContent === 'Alpha: 0', 'worker roots shared state');
      // An idle UI remains live beyond its computation deadline.
      const idle = start('vm', { turnTimeoutMilliseconds: 1500 });
      require(await idle.ready, 'idle worker did not start');
      await new Promise(resolve => setTimeout(resolve, 1600));
      require(idle.terminated() === 0, 'idle input wait timed out');
      for (const app of [a, b, idle]) {
        app.close(); app.close(); await app.done;
        require(app.container.childNodes.length === 0 && app.terminated() === 1, 'worker close retained DOM or terminated twice');
      }
      for (const mode of ['loop', 'empty', 'overlap', 'turn-loop']) {
        const bytes = encodeBatch({ version: WIRE_VERSION, revision: 1, inputSequence: 0, mutations: [], commands: [] });
        const app = start(mode, { startupTimeoutMilliseconds: 1500, turnTimeoutMilliseconds: 100 }, bytes);
        require(!await app.ready, 'failed worker became interactive');
        let failure = '';
        try { await app.done; } catch (error) { failure = String(error); }
        require(mode === 'overlap' ? failure.includes('overlapping') : failure.includes('did not respond'), 'worker lost its failure diagnostic: ' + failure);
        require(app.terminated() === 1 && app.container.childNodes.length === 0, 'failed worker retained its owned resources');
      }
      const early = start('loop'); early.close();
      require(!await early.ready, 'cancelled startup became interactive');
      await early.done;
      return { passed: true, contracts: ['worker-vm-live-events', 'worker-post-commit-readiness', 'worker-root-isolation',
        'worker-idle-no-deadline', 'worker-startup-cancellation', 'worker-startup-loop-timeout', 'worker-uninitialized-idle-timeout',
        'worker-turn-loop-timeout', 'worker-overlap-rejection', 'worker-exactly-once-disposal'] };
    } finally {
      for (const session of sessions) { session.close(); session.container.remove(); }
      URL.revokeObjectURL(script);
    }
  });
  assert.equal(result.passed, true);
  return result;
}
