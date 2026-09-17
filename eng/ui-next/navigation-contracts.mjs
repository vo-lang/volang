export async function checkNavigationContracts(page, url) {
  await page.route('**/navigation-contracts', route => route.fulfill({ contentType: 'text/html', body: '<!doctype html><title>Navigation contracts</title>' }));
  await page.goto(`${url}/navigation-contracts`);
  return page.evaluate(async () => {
    const { createNavigationServices } = await import('/host/ui_next/navigation.js');
    const require = (ok, message) => { if (!ok) throw new Error(message); };
    const roots = [document.createElement('div'), document.createElement('div')];
    document.body.append(...roots);
    const services = roots.map(createNavigationServices);
    const owners = [new AbortController(), new AbortController()];
    const received = [[], []];
    services.forEach((service, i) => service.watches['web.location']('', owners[i].signal, value => received[i].push(value)));
    const link = document.createElement('a');
    link.setAttribute('data-ui-route', '');
    link.href = '/studio/docs?topic=state#example';
    roots[0].append(link);
    let intercepted = false;
    // Observe whether the framework intercepted before suppressing the test's
    // external/new-tab/download default actions.
    const observe = event => { intercepted = event.defaultPrevented; event.preventDefault(); };
    document.addEventListener('click', observe);
    const click = fields => { intercepted = false; link.dispatchEvent(new MouseEvent('click', { bubbles: true, cancelable: true, ...fields })); return intercepted; };
    require(click({}), 'ordinary route link was not intercepted');
    require(received.every(values => values.length === 2 && values[1] === '/studio/docs?topic=state#example'), 'shared history did not reach both root subscriptions');
    const length = history.length;
    require(!click({}) && history.length === length && received[0].length === 2, 'same-page fragment did not retain native behavior');
    for (const fields of [{ ctrlKey: true }, { metaKey: true }, { shiftKey: true }, { altKey: true }, { button: 1 }]) {
      require(!click(fields), 'modified link lost its native behavior');
    }
    for (const [name, value] of [['target', '_blank'], ['download', 'sample'], ['rel', 'external']]) {
      link.setAttribute(name, value); require(!click({}), `native ${name} link was intercepted`); link.removeAttribute(name);
    }
    link.href = 'https://example.invalid/'; require(!click({}), 'external URL was intercepted');
    link.href = 'mailto:hello@example.invalid'; require(!click({}), 'non-HTTP URL was intercepted');
    link.href = 'http://'; require(!click({}), 'invalid URL was intercepted');
    await services[0].tasks['web.navigate'](JSON.stringify({ href: '/studio/gallery?q=a', replace: true }), owners[0].signal);
    require(history.length === length && received[1].at(-1) === '/studio/gallery?q=a', 'replace did not retain history length');
    let rejected = false;
    try { await services[0].tasks['web.navigate'](JSON.stringify({ href: 'https://example.invalid', replace: false }), owners[0].signal); }
    catch { rejected = true; }
    require(rejected && history.length === length, 'foreign imperative navigation changed history');
    await services[0].tasks['web.navigate'](JSON.stringify({href:'/studio/gallery?keep=old&page=3#anchor',replace:true}),owners[0].signal);
    const patch = values => services[0].tasks['web.query'](JSON.stringify({values,replace:true,preserveScroll:true,preserveFocus:true}),owners[0].signal);
    await Promise.all([patch({q:['设计'],tag:['first','last'],page:null}),patch({sort:['title']})]);
    const query = new URL(location.href);
    require(query.searchParams.get('q') === '设计' && query.searchParams.get('sort') === 'title' && query.searchParams.get('keep') === 'old' && !query.searchParams.has('page') && query.searchParams.getAll('tag').join(',') === 'first,last' && query.hash === '#anchor', 'rapid query patches lost fields, repeated values or fragment');
    await patch({tag:[]});
    require(!new URL(location.href).searchParams.has('tag'),'empty query values did not remove a field');
    const before = location.href;
    for(const values of [null,[],{q:'invalid'},{q:[1]},{q:['界'.repeat(4000)]}]) {
      let failed = false;
      try {await patch(values);} catch {failed=true;}
      require(failed && location.href === before,'invalid query update partially changed history');
    }
    owners[0].abort();
    const old = received[0].length;
    await services[1].tasks['web.navigate'](JSON.stringify({ href: '/studio/docs', replace: false }), owners[1].signal);
    require(received[0].length === old && received[1].at(-1) === '/studio/docs', 'disposed root retained its location observer');
    link.href = '/studio/gallery'; require(!click({}), 'disposed root retained link interception');
    document.removeEventListener('click', observe);
    const anchor = document.createElement('a'), target = document.createElement('h2');
    anchor.setAttribute('data-ui-route', '');
    anchor.href = '#reading'; anchor.textContent = 'Jump to reading';
    target.id = 'reading'; target.textContent = 'Reading'; target.tabIndex = -1; target.style.marginTop = '2000px';
    roots[1].append(anchor, target);
    const jumped = new Promise(resolve => window.addEventListener('hashchange', resolve, { once: true }));
    anchor.click(); await jumped;
    await new Promise(requestAnimationFrame);
    require(scrollY > 0 && received[1].at(-1).endsWith('#reading'), 'native fragment scrolling or location notification was lost');
    owners[1].abort();
    return { passed: true, contracts: ['root-isolation', 'history-notification', 'replace', 'atomic-query-patches', 'query-budget', 'same-page-fragment', 'native-modified-links', 'external-links', 'invalid-url', 'native-anchor-scrolling', 'subscription-release'] };
  });
}
