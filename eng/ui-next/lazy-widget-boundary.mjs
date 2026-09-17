export async function checkLazyWidgetBoundary(page, url) {
  await page.route('**/lazy-widgets', route => route.fulfill({contentType:'text/html', body:'<!doctype html><title>Lazy widgets</title>'}));
  await page.goto(url + '/lazy-widgets');
  return page.evaluate(async () => {
    const {WidgetHost} = await import('/host/ui_next/widgets.js');
    const {createLazyWidget} = await import('/host/ui_next/lazy-widget.js');
    const require = (condition, message) => {if (!condition) throw new Error(message);};
    const tick = () => new Promise(resolve => setTimeout(resolve, 0));
    const deferred = () => {
      let resolve, reject;
      const promise = new Promise((yes, no) => {resolve = yes; reject = no;});
      return {promise, resolve, reject};
    };
    const root = document.body.appendChild(document.createElement('div'));
    const received = [], hosts = [], stats = {mounts:0, updates:0, disposals:0};
    const provider = context => {
      stats.mounts++;
      context.element.textContent = context.value;
      context.emit('ready:' + context.value);
      let disposed = false;
      return {
        update(value) {if (value === 'bad-update') throw new Error('update failed'); stats.updates++; context.element.textContent = value;},
        dispose() {if (disposed) return; disposed = true; stats.disposals++; context.element.replaceChildren();},
      };
    };
    const hostFor = widgets => {
      const host = new WidgetHost((id, value, error) => received.push({id, value, error}), widgets);
      hosts.push(host); return host;
    };
    try {
      for (const value of [0, -1, 60001, 1.5, NaN]) {
        let rejected = false;
        try {createLazyWidget(async () => provider, {timeoutMilliseconds:value});} catch {rejected = true;}
        require(rejected, 'invalid lazy deadline accepted');
      }
      const load = deferred(), signals = [];
      const host = hostFor({lazy:createLazyWidget((owner, signal) => {
        require(owner === document, 'loader received the wrong document'); signals.push(signal); return load.promise;
      }), sync:provider});
      host.apply(1, root, 'lazy', 'initial');
      for (let i = 0; i < 100; i++) host.apply(1, root, 'lazy', 'latest-' + i);
      require(!root.childNodes.length && stats.mounts === 0, 'pending loader blocked or mounted before resolution');
      load.resolve(provider); await tick();
      require(root.textContent === 'latest-99' && stats.mounts === 1 && stats.updates === 0, 'loader replayed intermediate values or lost the newest payload');
      host.apply(1, root, 'lazy', 'retained');
      require(root.textContent === 'retained' && stats.updates === 1, 'loaded widget stopped receiving updates');
      host.remove(1);
      require(signals[0].aborted && stats.disposals === 1 && !root.childNodes.length, 'loaded widget disposal did not abort and release its instance');
      // A settled promise remains usable for a later mount.
      host.apply(2, root, 'lazy', 'again'); await tick();
      require(root.textContent === 'again' && stats.mounts === 2, 'cached settled load never installed a new instance');
      host.apply(2, root, 'lazy', 'bad-update');
      require(received.at(-1).error === 'update failed' && stats.disposals === 2 && !root.childNodes.length, 'failed update retained the loaded instance');
      host.apply(3, root, 'sync', 'synchronous');
      require(root.textContent === 'synchronous', 'ordinary widget mounting became asynchronous'); host.remove(3);

      const abandoned = deferred(), then = abandoned.promise.then;
      let observers = 0, lateMounts = 0;
      abandoned.promise.then = function(...args) {observers++; return then.apply(this, args);};
      const late = hostFor({lazy:createLazyWidget(() => abandoned.promise)});
      for (let id = 1; id <= 100; id++) {late.apply(id, root, 'lazy', 'obsolete'); late.remove(id);}
      require(observers === 1, 'cancelled loads accumulated observers on one unresolved import');
      const eventCount = received.length;
      abandoned.resolve(() => {lateMounts++; return provider({element:root, value:'late', emit:()=>{}});}); await tick();
      require(lateMounts === 0 && received.length === eventCount && !root.childNodes.length, 'late load mounted or reported to removed instances');

      const shared = deferred(), sharedRoot = document.body.appendChild(document.createElement('div'));
      const sharedHost = hostFor({lazy:createLazyWidget(() => shared.promise)});
      try {
        sharedHost.apply(1, root, 'lazy', 'removed'); sharedHost.apply(2, sharedRoot, 'lazy', 'shared');
        sharedHost.remove(1); shared.resolve(provider); await tick();
        require(!root.childNodes.length && sharedRoot.textContent === 'shared', 'cancelling one subscriber cancelled another widget');
      } finally {sharedHost.close(); sharedRoot.remove();}

      const stale = deferred(); let calls = 0;
      const replacement = hostFor({lazy:createLazyWidget(() => ++calls === 1 ? stale.promise : Promise.resolve(provider))});
      replacement.apply(1, root, 'lazy', 'old'); replacement.remove(1);
      replacement.apply(1, root, 'lazy', 'new'); await tick();
      stale.resolve(provider); await tick();
      require(root.textContent === 'new', 'old completion replaced a newer binding with the same id'); replacement.close();

      let attempts = 0;
      const retry = hostFor({lazy:createLazyWidget(async () => {
        if (++attempts === 1) throw new Error('load failed'); return provider;
      })});
      retry.apply(1, root, 'lazy', 'first'); await tick();
      require(received.at(-1).error === 'load failed' && !root.childNodes.length, 'load failure was not local');
      retry.apply(1, root, 'lazy', 'retry'); await tick();
      require(root.textContent === 'retry', 'changed input could not retry a failed provider'); retry.close();

      let timedSignal;
      const timed = hostFor({lazy:createLazyWidget((_owner, signal) => {timedSignal = signal; return new Promise(() => {});}, {timeoutMilliseconds:25})});
      timed.apply(1, root, 'lazy', 'held'); await new Promise(resolve => setTimeout(resolve, 80));
      require(timedSignal.aborted && received.at(-1).error === 'widget loading timed out', 'deadline did not abort the loader and publish a local failure');

      let failedContext, failedDisposals = 0;
      const failing = hostFor({lazy:createLazyWidget(async () => context => {
        failedContext = context; context.fail('installation failed');
        return {update() {}, dispose() {failedDisposals++;}};
      })});
      failing.apply(1, root, 'lazy', 'fail'); await tick();
      require(received.at(-1).error === 'installation failed' && failedContext.signal.aborted && failedDisposals === 1, 'reentrant installation failure retained its returned instance');
      const afterFailure = received.length;
      failedContext.emit('late'); failedContext.fail('late');
      require(received.length === afterFailure, 'failed instance published another result');

      let reentrant;
      reentrant = hostFor({lazy:createLazyWidget(() => {reentrant.close(); return Promise.reject(new Error('closed during loading'));})});
      reentrant.apply(1, root, 'lazy', 'closed'); await tick();
      require(received.length === afterFailure && !root.childNodes.length, 'reentrant close left a loader subscription or failure');
      require(stats.mounts === stats.disposals, 'widget instance count did not return to its baseline');
      return {passed:true, contracts:['bounded-deadline', 'document-context', 'pending-latest-value', 'sync-fast-path',
        'settled-import-reuse', 'update-error-cleanup', 'shared-observer-cancellation', 'independent-shared-load-owners', 'late-load-no-mount', 'same-id-replacement',
        'load-error-retry', 'timeout-abort', 'reentrant-install-failure', 'late-event-rejection', 'reentrant-load-close', 'balanced-disposal']};
    } finally {for (const host of hosts) host.close(); root.remove();}
  });
}
