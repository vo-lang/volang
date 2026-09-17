export async function checkScrollNavigation(page, url) {
  await page.route('**/navigation-scroll-contracts', route => route.fulfill({ contentType: 'text/html',
    body: '<!doctype html><style>body{margin:0}main{height:4000px}input{margin-top:600px}</style><div id="root"><main id="content" tabindex="-1"><input id="saved-a"></main></div>' }));
  await page.goto(`${url}/navigation-scroll-contracts`);
  return page.evaluate(async () => {
    const { createNavigationServices } = await import('/host/ui_next/navigation.js');
    const scrollCalls = [];
    const nativeScroll = window.scrollTo.bind(window);
    window.scrollTo = (...args) => { scrollCalls.push({ args, url: location.pathname + location.search + location.hash }); nativeScroll(...args); };
    const require = (ok, message) => { if (!ok) throw new Error(message); };
    const wait = async predicate => {
      const deadline = performance.now() + 5000;
      while (!predicate()) {
        if (performance.now() > deadline) throw new Error(`Navigation restoration did not settle: ${predicate}; scroll=${scrollY}; url=${location.href}; entries=${JSON.stringify(received)}; history=${JSON.stringify(history.state)}; scrolls=${JSON.stringify(scrollCalls.slice(-8))}; policy=${history.scrollRestoration}`);
        await new Promise(requestAnimationFrame);
      }
    };
    const root = document.querySelector('#root'), main = document.querySelector('main');
    const service = createNavigationServices(root), owner = new AbortController();
    const received = [];
    const last = () => received.at(-1);
    const commit = value => service.tasks['web.navigation-commit'](JSON.stringify(value), owner.signal);
    const navigate = (href, options = {}) => service.tasks['web.navigate'](JSON.stringify({ href, replace: false, ...options }), owner.signal);
    const originalRestoration = history.scrollRestoration;
    service.watches['web.location']('', owner.signal, () => {});
    const failedOwner = new AbortController();
    history.replaceState('external state', '');
    let unsupportedState = false;
    try { service.watches['web.navigation']('content', failedOwner.signal, () => {}); }
    catch { unsupportedState = true; }
    require(unsupportedState && history.state === 'external state' && history.scrollRestoration === originalRestoration,
      'incompatible history state was overwritten or leaked a partial installation');
    history.replaceState(null, '');
    document.querySelector('#saved-a').focus({ preventScroll: true });
    scrollTo(0, 650);
    service.watches['web.navigation']('content', owner.signal, value => received.push(JSON.parse(value)));
    failedOwner.abort();
    await commit(last());
    require(scrollY === 650 && document.activeElement.id === 'saved-a', 'initial subscription moved the viewport or focus');
    require(history.scrollRestoration === 'manual', 'viewport owner did not take manual restoration');
    let duplicate = false;
    try { service.watches['web.navigation']('content', new AbortController().signal, () => {}); }
    catch { duplicate = true; }
    require(duplicate, 'two roots were allowed to fight over one viewport');
    const initialURL = last().url;
    await navigate('/scroll-b');
    const second = last();
    require(scrollY === 650, 'history mutation scrolled before the guest committed');
    main.innerHTML = '<button id="saved-b">B action</button>';
    await commit(second);
    require(scrollY === 0 && document.activeElement.id === 'content', 'new route did not reset scroll and focus after commit');
    scrollTo(0, 900);
    document.querySelector('#saved-b').focus({ preventScroll: true });
    history.back();
    await wait(() => last().url === initialURL);
    main.innerHTML = '<input id="saved-a">';
    main.style.height = '100px';
    main.querySelector('input').style.marginTop = '0';
    await commit(last());
    require(document.activeElement.id === 'saved-a', 'back navigation did not restore a surviving focus target');
    main.style.height = '4000px';
    await wait(() => scrollY === 650);
    scrollTo(0, 123);
    await commit(last());
    await commit(second);
    require(scrollY === 123, 'duplicate or stale commit moved the current page');
    await navigate('/scroll-c');
    const stale = last();
    await navigate('/scroll-d');
    await commit(stale);
    require(scrollY === 123, 'rapid navigation applied an obsolete scroll intent');
    await commit(last());
    require(scrollY === 0, 'latest rapid navigation did not settle');
    history.replaceState({ ...history.state, applicationField: 'keep' }, '');
    scrollTo(0, 400);
    const length = history.length;
    await navigate('/scroll-d?filter=one', { replace: true, preserveScroll: true });
    await commit(last());
    require(scrollY === 400 && history.length === length && history.state.applicationField === 'keep',
      'replace/preserve-scroll discarded viewport or unrelated history state');
    main.innerHTML = '<input id="query-input">';
    const queryInput = main.querySelector('input');
    queryInput.value = '中文 edit'; queryInput.focus({preventScroll:true}); queryInput.setSelectionRange(2,5,'backward');
    const update = filter => service.tasks['web.query'](JSON.stringify({values:{filter:[filter]},replace:true,preserveScroll:true,preserveFocus:true}),owner.signal);
    await update('two'); await commit(last());
    require(document.activeElement === queryInput && queryInput.selectionStart === 2 && queryInput.selectionEnd === 5 && queryInput.selectionDirection === 'backward' && scrollY === 400,'query update moved focus, selection or scroll');
    queryInput.remove();
    await update('three'); await commit(last());
    require(document.activeElement === main,'removed preserved focus did not fall back to content');
    await navigate('/scroll-d?filter=one',{replace:true,preserveScroll:true}); await commit(last());
    main.innerHTML = '<a data-ui-route href="#reading">Read</a><h2 id="reading" tabindex="-1" style="margin-top:1800px">Reading</h2>';
    main.querySelector('a').click();
    await wait(() => last().url.endsWith('#reading'));
    const nativePosition = scrollY;
    await commit(last());
    require(nativePosition > 1000 && scrollY === nativePosition, `native fragment scrolling was overridden: ${nativePosition} -> ${scrollY}`);
    main.querySelector('a').click();
    history.back();
    await wait(() => !last().url.includes('#'));
    await commit(last());
    await wait(() => scrollY === 400);
    await navigate('/short');
    await commit(last());
    main.style.height = '100px';
    main.innerHTML = '';
    history.back();
    await wait(() => last().url.includes('filter=one'));
    await commit(last());
    window.dispatchEvent(new WheelEvent('wheel'));
    main.style.height = '4000px';
    await new Promise(requestAnimationFrame);
    require(scrollY === 0, 'layout retry ignored user scroll intent');
    await navigate('/cancelled-commit');
    scrollTo(0, 75);
    const cancelled = new AbortController();
    const queued = service.tasks['web.navigation-commit'](JSON.stringify(last()), cancelled.signal);
    cancelled.abort();
    await queued;
    require(scrollY === 75, 'cancelled posted restoration still moved the viewport');
    await commit(last());
    require(scrollY === 0, 'cancelling a notification consumed the live navigation revision');
    owner.abort();
    require(history.scrollRestoration === originalRestoration, 'disposing the owner did not restore browser policy');
    const count = received.length;
    await navigate('/released');
    require(received.length === count, 'disposed viewport owner kept observing history');
    return { passed: true, contracts: ['commit-before-scroll', 'initial-focus-preserved', 'single-viewport-owner',
      'back-focus-position', 'async-layout-restoration', 'duplicate-and-stale-commit', 'rapid-navigation',
      'replace-preserve-scroll', 'history-state-preservation', 'native-fragment', 'user-interruption', 'posted-commit-cancellation', 'owner-disposal'] };
  });
}
