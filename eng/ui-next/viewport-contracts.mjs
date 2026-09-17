import assert from 'node:assert/strict';

export async function checkViewportBoundary(page, url) {
  await page.route('**/viewport-contracts', route => route.fulfill({ contentType: 'text/html', body: '<!doctype html><title>Viewport contracts</title>' }));
  await page.goto(`${url}/viewport-contracts`);
  const result = await page.evaluate(async () => {
    const { DomRenderer } = await import('/host/ui_next/renderer.js');
    const { WIRE_VERSION } = await import('/host/ui_next/generated/protocol.js');
    const { encodeBatch } = await import('/host/ui_next/generated/codec.js');
    const require = (ok, message) => { if (!ok) throw new Error(message); };
    const mutation = (op, id, fields = {}) => ({ op, id, parent: 0, before: 0, name: '', value: '', ...fields });
    const frame = () => new Promise(resolve => requestAnimationFrame(resolve));
    const settle = async () => { await frame(); await frame(); await frame(); };
    const container = document.createElement('div');
    document.body.append(container);
    const events = [];
    const root = new DomRenderer(container, event => events.push(event));
    const apply = (revision, mutations) => root.apply(encodeBatch({ version: WIRE_VERSION, revision, inputSequence: events.at(-1)?.sequence ?? 0, mutations, commands: null }));
    apply(1, [mutation('create', 1, { name: 'div' }), mutation('insert', 1),
      mutation('attr', 1, { name: 'style', value: 'height:100px;width:240px;overflow:auto;padding:0' }),
      mutation('listen', 1, { name: 'viewport', value: '0' }),
      mutation('create', 2, { name: 'div' }), mutation('insert', 2, { parent: 1 }),
      mutation('attr', 2, { name: 'style', value: 'height:600px' })]);
    await settle();
    const viewport = container.firstElementChild;
    require(events.length === 1 && events[0].kind === 'viewport', 'initial geometry did not coalesce');
    const first = JSON.parse(events[0].value);
    require(first.width === viewport.clientWidth && first.height === 100 && first.top === 0, 'initial geometry differs from native client box');
    for (let top = 1; top <= 30; top++) { viewport.scrollTop = top; viewport.dispatchEvent(new Event('scroll')); }
    await settle();
    require(events.length === 2 && JSON.parse(events.at(-1).value).top === 30, 'scroll burst did not coalesce to the latest geometry');
    viewport.dispatchEvent(new Event('scroll'));
    await settle();
    require(events.length === 2, 'unchanged geometry caused guest work');
    apply(2, [mutation('attr', 1, { name: 'style', value: 'height:160px;width:320px;overflow:auto;padding:0' })]);
    await settle();
    require(events.length === 3 && JSON.parse(events.at(-1).value).height === 160, 'resize observation did not reach the UI writer');
    let rejected = false;
    try { apply(3, [mutation('attr', 1, { name: 'title', value: 'partial' }), mutation('listen', 1, { name: 'viewport:capture', value: '1' })]); } catch { rejected = true; }
    require(rejected && !viewport.hasAttribute('title'), 'invalid observation options partially committed');
    viewport.scrollTop = 40;
    viewport.dispatchEvent(new Event('scroll'));
    apply(3, [mutation('unlisten', 1, { name: 'viewport' })]);
    await settle();
    require(events.length === 3, 'unlisten retained pending viewport delivery');
    apply(4, [mutation('listen', 1, { name: 'viewport', value: '0' })]);
    await settle();
    require(events.length === 4 && JSON.parse(events.at(-1).value).top === 40, 'resubscription omitted fresh geometry');
    viewport.scrollTop = 50;
    viewport.dispatchEvent(new Event('scroll'));
    apply(5, [mutation('remove', 1)]);
    await settle();
    require(events.length === 4, 'removed element retained viewport work');
    rejected = false;
    try { apply(6, [mutation('create', 3, { name: 'svg' }), mutation('insert', 3), mutation('listen', 3, { name: 'viewport', value: '0' })]); } catch { rejected = true; }
    require(rejected && container.children.length === 0, 'SVG viewport rejection partially committed');
    root.close();
    const hydrated = document.createElement('div');
    hydrated.innerHTML = '<div data-vo-id="1" data-vo-events="viewport=0" style="height:80px;width:220px;overflow:auto"><div data-vo-id="2" style="height:500px"></div></div>';
    document.body.append(hydrated);
    hydrated.firstElementChild.scrollTop = 70;
    const early = [];
    const adopted = new DomRenderer(hydrated, event => early.push(event), true);
    await settle();
    require(early.length === 1 && JSON.parse(early[0].value).top === 70, 'hydrated viewport lost pre-boot scroll position');
    adopted.close();
    await settle();
    require(early.length === 1, 'closed root retained an observer');
    const invalid = document.createElement('div');
    invalid.innerHTML = '<div data-vo-id="1" data-vo-events="viewport=0"></div><svg data-vo-id="2" data-vo-events="viewport=0"></svg>';
    document.body.append(invalid);
    const late = [];
    rejected = false;
    try { new DomRenderer(invalid, event => late.push(event), true); } catch { rejected = true; }
    await settle();
    require(rejected && late.length === 0, 'failed hydration retained an earlier viewport observer');
    invalid.remove();
    container.remove(); hydrated.remove();
    return { initial: true, resize: true, coalescedScroll: true, unchangedSkipped: true, unlisten: true, remove: true, earlyScroll: true, close: true, namespaceAtomicity: true, failedHydrationCleanup: true };
  });
  assert.equal(result.close, true);
  return result;
}
