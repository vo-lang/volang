export async function checkMotionBoundary(page, url) {
  await page.route('**/motion-contracts', route => route.fulfill({ contentType: 'text/html', body: '<!doctype html><title>Motion contracts</title>' }));
  await page.goto(`${url}/motion-contracts`);
  return page.evaluate(async () => {
    const { motionTasks } = await import('/host/ui_next/motion.js');
    const { TaskHost } = await import('/host/ui_next/tasks.js');
    const require = (ok, message) => { if (!ok) throw new Error(message); };
    const container = document.createElement('div'), other = document.createElement('div');
    container.innerHTML = '<div id="moving:part"><i>Child motion</i></div>';
    other.innerHTML = '<div id="outside">Other root</div>';
    document.body.append(container, other);
    const element = container.firstElementChild;
    const wait = async (name, operation) => {
      let timer;
      try {
        return await Promise.race([operation, new Promise((_, reject) => {
          timer = setTimeout(() => reject(new Error(`Motion ${name} did not settle: ${JSON.stringify({
            visibility: document.visibilityState, focused: document.hasFocus(),
            animations: element.getAnimations().map(a => ({ state: a.playState, pending: a.pending, time: a.currentTime })),
          })}`)), 3000);
        })]);
      } finally { clearTimeout(timer); }
    };
    const request = motionTasks(container)['web.motion-finished'];
    const forever = element.firstElementChild.animate([{ opacity: 0 }, { opacity: 1 }], { duration: 1000, iterations: Infinity });
    const foreign = other.firstElementChild.animate([{ opacity: 0 }, { opacity: 1 }], { duration: 100000 });
    require(await request('outside', new AbortController().signal) === '', 'motion crossed root boundaries');
    require(await request('moving:part', new AbortController().signal) === '', 'child animation held its parent open');
    require(await request('missing', new AbortController().signal) === '', 'missing element did not settle');
    const infinite = element.animate([{ opacity: 0 }, { opacity: 1 }], { duration: 1000, iterations: Infinity });
    require(await request('moving:part', new AbortController().signal) === '', 'infinite wrapper animation held its exit open');
    infinite.cancel();
    const animate = () => element.animate([{ opacity: 0.4 }, { opacity: 1 }], { duration: 100000 });
    let animation = animate();
    let settled = false;
    const first = request('moving:part', new AbortController().signal).then(() => { settled = true; });
    await Promise.resolve();
    require(!settled, 'finite motion completed before its animation');
    animation.finish();
    await wait('finish', first);
    animation = animate();
    const cancelled = request('moving:part', new AbortController().signal);
    animation.cancel();
    await wait('cancel', cancelled);

    animation = animate(); animation.pause();
    const finished = animation.finished, then = finished.then;
    let continuations = 0;
    finished.then = function(...args) { continuations++; return then.apply(this, args); };
    for (let i = 0; i < 5; i++) {
      const controller = new AbortController(), signal = controller.signal;
      let listeners = 0;
      const add = signal.addEventListener, remove = signal.removeEventListener;
      signal.addEventListener = function(...args) { listeners++; return add.apply(this, args); };
      signal.removeEventListener = function(...args) { listeners--; return remove.apply(this, args); };
      const aborted = request('moving:part', signal).then(() => false, () => true);
      require(listeners === 1, 'motion request did not observe cancellation');
      controller.abort();
      require(await aborted && listeners === 0 && animation.playState === 'paused', 'cancellation retained listeners or changed application motion');
    }
    require(continuations === 1, 'repeated waits accumulated callbacks on a paused animation promise');
    const completion = new Promise(resolve => {
      const host = new TaskHost((id, value, error) => { host.close(); resolve({ id, error }); }, motionTasks(container));
      host.apply([{ op: 'start', id: 1, name: 'web.motion-finished', value: 'moving:part', timeoutMilliseconds: 25 }]);
    });
    const result = await completion;
    require(result.id === 1 && result.error.includes('timed out') && continuations === 1, 'deadline did not release paused motion observation');
    delete finished.then;
    animation.cancel(); forever.cancel(); foreign.cancel();
    container.remove(); other.remove();
    return { passed: true, contracts: ['motion-root-isolation', 'motion-missing-element', 'motion-excludes-children-and-infinite',
      'motion-finite-completion', 'motion-native-cancellation', 'motion-abort-releases-listeners', 'motion-timeout-releases-listeners', 'motion-shared-paused-completion'] };
  });
}
