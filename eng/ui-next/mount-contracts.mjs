import assert from 'node:assert/strict';
import {checkStartup} from './startup-contracts.mjs';

export async function checkMount(page, url) {
  await page.route('**/__mount-contracts', route => route.fulfill({ contentType: 'text/html', body: '<!doctype html><body></body>' }));
  await page.goto(url + '/__mount-contracts');
  const result = await page.evaluate(async () => {
    const { mountUi } = await import('/host/ui_next/mount.js');
    const require = (value, message) => { if (!value) throw new Error(message); };
    const container = () => document.body.appendChild(document.createElement('div'));
    const missingRoot = container();
    const missing = mountUi(missingRoot, { backend: 'vm', artifact: '/missing-artifact.vob', loadVm: () => import('/wasm/vo_web.js') });
    require(!await missing.ready, 'failed fetch resolved interactive');
    let error;
    try { await missing.done; } catch (caught) { error = caught; }
    require(error?.message.includes('404') && missingRoot.childNodes.length === 0, 'failed artifact lost its HTTP diagnostic');
    missing.close(); missing.close();

    let initialized = 0, releaseLoad;
    const loading = new Promise(resolve => { releaseLoad = resolve; });
    let reachedLoad;
    const reached = new Promise(resolve => { reachedLoad = resolve; });
    const loadingRoot = container();
    const cancelled = mountUi(loadingRoot, { backend: 'vm', artifact: '/artifacts/interaction.vob', loadVm: () => { reachedLoad(); return loading; } });
    await reached;
    cancelled.close();
    require(!await cancelled.ready, 'close during runtime load resolved interactive');
    releaseLoad({ default() { initialized++; } });
    await cancelled.done;
    require(initialized === 0 && loadingRoot.childNodes.length === 0, 'cancelled loader initialized or mounted a late runtime');

    let finishInit, startedInit, sharedCalls = 0, sharedAllocations = 0, sharedReleases = 0;
    const initStarted = new Promise(resolve => { startedInit = resolve; });
    const initPending = new Promise(resolve => { finishInit = resolve; });
    const sharedRuntime = {
      default() { sharedCalls++; startedInit(); return initPending; },
      VoVmIsland: class {
        constructor() { sharedAllocations++; }
        run() { return 'completed'; }
        free() { sharedReleases++; }
      },
    };
    const sharedRoots = [container(), container()];
    const sharedApps = sharedRoots.map(root => mountUi(root, {backend: 'vm', artifact: '/artifacts/interaction.vob', loadVm: async () => sharedRuntime}));
    await initStarted;
    sharedApps[0].close();
    finishInit();
    await Promise.all(sharedApps.map(app => app.done));
    require(!await sharedApps[0].ready, 'cancelled root became interactive after shared initialization');
    require(sharedCalls === 1 && sharedAllocations === 1 && sharedReleases === 1, 'closing one root disrupted shared initialization or created a late Island');
    sharedApps.forEach(app => app.close()); sharedRoots.forEach(root => root.remove());

    let allocations = 0, releases = 0;
    const wasm = await import('/wasm/vo_web.js');
    const vmRuntime = { ...wasm, VoVmIsland: class extends wasm.VoVmIsland {
      constructor(bytes) { super(bytes); allocations++; }
      free() { releases++; super.free(); }
    } };
    for (const backend of ['vm']) {
      const root = container();
      const app = mountUi(root, { backend, artifact: `/artifacts/interaction.vob`, loadVm: async () => vmRuntime });
      require(await app.ready, `${backend} did not become interactive`);
      require(root.querySelector('[data-lifecycle]').textContent === 'Ready to explore', 'ready preceded initial effects');
      const counter = root.querySelector('[data-counter=Alpha]');
      for (let cycle = 1; cycle <= 2; cycle++) {
        window.dispatchEvent(new PageTransitionEvent('pagehide', { persisted: true }));
        window.dispatchEvent(new PageTransitionEvent('pageshow', { persisted: true }));
        require(root.querySelector('[data-counter=Alpha]') === counter, 'persisted pagehide discarded the application');
        await new Promise((resolve, reject) => {
          const observer = new MutationObserver(() => {
            if (counter.textContent === `Alpha: ${cycle}`) { clearTimeout(timer); observer.disconnect(); resolve(); }
          });
          const timer = setTimeout(() => { observer.disconnect(); reject(new Error('restored application stopped processing events')); }, 10000);
          observer.observe(counter, {subtree:true, characterData:true});
          counter.click();
        });
      }
      window.dispatchEvent(new PageTransitionEvent('pagehide', { persisted: false }));
      await app.done;
      require(root.childNodes.length === 0, 'discarded page retained its UI root');
      app.close(); app.close();
      await app.done;
      require(root.childNodes.length === 0, 'closed application retained its DOM');
      root.remove();
    }
    require(allocations === 1 && releases === 1, 'VM application did not release exactly one Island');
    return { passed: true, contracts: ['artifact-http-diagnostic', 'failed-start-readiness', 'close-during-runtime-load', 'late-loader-suppression', 'shared-runtime-initialization-and-cancellation', 'vm-readiness', 'idempotent-close', 'exactly-once-vm-release', 'persisted-page-event-keeps-state', 'discarded-page-closes-root'] };
  });
  assert.equal(result.passed, true);
  result.startup = await checkStartup(page, url);
  return result;
}
