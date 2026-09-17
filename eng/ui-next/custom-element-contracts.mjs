import assert from 'node:assert/strict';

export async function checkCustomElementBoundary(page, url) {
  await page.route('**/__custom_elements', route => route.fulfill({ contentType: 'text/html', body: '<!doctype html><body></body>' }));
  await page.goto(`${url}/__custom_elements`);
  return page.evaluate(async () => {
    const { createCustomElementWidget } = await import('/host/ui_next/custom-element.js');
    const { WidgetHost } = await import('/host/ui_next/widgets.js');
    const require = (value, message) => { if (!value) throw new Error(message); };
    const stats = { connected: 0, disconnected: 0, properties: 0 };
    const define = (view, stats) => view.customElements.define('test-ui-counter', class extends view.HTMLElement {
      #model;
      constructor() { super(); this.attachShadow({ mode: 'open' }).innerHTML = '<input aria-label="Retained local note">'; }
      set model(value) { if (value?.fail) throw new Error('setter failed'); this.#model = value; stats.properties++; }
      get model() { return this.#model; }
      connectedCallback() { stats.connected++; }
      disconnectedCallback() { stats.disconnected++; }
    });
    define(window, stats);
    const events = ['count-change'], properties = ['model'];
    const provider = createCustomElementWidget({ tag: 'test-ui-counter', properties, events });
    properties.push('unexpected'); events.push('unexpected');
    const received = [];
    const host = new WidgetHost((id, value, error) => received.push({ id, value, error }), { counter: provider,
      missing: createCustomElementWidget({ tag: 'test-not-defined' }) });
    const root = document.body.appendChild(document.createElement('div'));
    const payload = (attributes = {}, properties = {}) => JSON.stringify({ version: 1, attributes, properties });
    let iframe;
    try {
      host.apply(1, root, 'counter', payload({ title: 'First', constructor: 'authored' }, { model: { count: 2, items: ['a', 'b'] } }));
      const element = root.firstElementChild, input = element.shadowRoot.querySelector('input');
      input.value = 'A local draft';
      require(element.model.count === 2 && stats.connected === 1 && stats.properties === 1, 'initial object property or connection failed');
      host.apply(1, root, 'counter', payload({ title: 'Second' }, { model: { count: 2, items: ['a', 'b'] } }));
      require(root.firstElementChild === element && input.value === 'A local draft' && stats.properties === 1, 'attribute-only update reset the element or invoked an unchanged setter');
      require(element.title === 'Second' && !element.hasAttribute('constructor'), 'removed attributes remained through an inherited object property');
      host.apply(1, root, 'counter', payload({}, { model: { count: 3 } }));
      require(element.model.count === 3 && !element.hasAttribute('title') && stats.properties === 2, 'property update failed');
      element.dispatchEvent(new CustomEvent('count-change', { detail: { value: 4, labels: ['中文', 'ok'] } }));
      const event = JSON.parse(received.at(-1).value);
      require(event.type === 'count-change' && JSON.parse(event.detail).labels[0] === '中文', 'native event detail did not cross the boundary');
      const before = received.length;
      element.dispatchEvent(new CustomEvent('unexpected', { detail: 1 }));
      require(received.length === before, 'factory retained the caller event array');
      const cycle = {}; cycle.self = cycle;
      element.dispatchEvent(new CustomEvent('count-change', { detail: cycle }));
      require(JSON.parse(received.at(-1).value).error.length > 0, 'cyclic detail did not become a local event failure');
      element.dispatchEvent(new CustomEvent('count-change', { detail: 'x'.repeat(16384) }));
      require(JSON.parse(received.at(-1).value).error.includes('16 KiB'), 'oversized event detail was published');
      host.apply(1, root, 'counter', payload());
      require(element.model === undefined && stats.properties === 3, 'omitted property did not reset to undefined');
      host.remove(1); host.remove(1);
      const removedCount = received.length;
      element.dispatchEvent(new CustomEvent('count-change', { detail: 5 }));
      require(root.childNodes.length === 0 && stats.disconnected === 1 && received.length === removedCount, 'disposed custom element retained a listener or connection');

      for (const [name, value, expected] of [
        ['counter', payload({}, { unexpected: 1 }), 'undeclared'],
        ['counter', payload({ onclick: 'x' }), 'attributes'],
        ['counter', payload({}, { model: 'x'.repeat(65536) }), '64 KiB'],
        ['counter', payload({}, { model: { fail: true } }), 'setter failed'],
        ['missing', payload(), 'before mounting'],
      ]) {
        host.apply(2, root, name, value);
        require(received.at(-1).error.includes(expected) && root.childNodes.length === 0, 'failed initialization did not clean up: ' + expected);
      }
      host.apply(3, root, 'counter', payload({}, { model: { count: 1 } }));
      const failedElement = root.firstElementChild;
      host.apply(3, root, 'counter', payload({}, { model: { fail: true } }));
      require(received.at(-1).error === 'setter failed' && !failedElement.isConnected && root.childNodes.length === 0, 'failed setter retained a mounted widget');
      for (const options of [{ tag: 'div' }, { tag: 'font-face' }, { tag: 'test-ui-counter', properties: ['model', 'model'] },
        { tag: 'test-ui-counter', properties: ['__proto__'] }, { tag: 'test-ui-counter', events: ['has space'] }]) {
        let rejected = false;
        try { createCustomElementWidget(options); } catch { rejected = true; }
        require(rejected, 'invalid adapter declaration accepted');
      }
      const disabled = [];
      customElements.define('test-ui-form-field', class extends HTMLElement {
        static formAssociated = true;
        #internals = this.attachInternals();
        set value(value) { this.#internals.setFormValue(value); }
        formDisabledCallback(value) { disabled.push(value); }
        formResetCallback() { this.value = 'reset value'; }
      });
      const form = root.appendChild(document.createElement('form'));
      const fieldset = form.appendChild(document.createElement('fieldset'));
      const wrapper = fieldset.appendChild(document.createElement('div'));
      const formHost = new WidgetHost(() => {}, {
        field: createCustomElementWidget({ tag: 'test-ui-form-field', properties: ['value'] }),
      });
      try {
        formHost.apply(5, wrapper, 'field', payload({ name: 'custom-field' }, { value: 'A form value 中文' }));
        require(new FormData(form).get('custom-field') === 'A form value 中文', 'widget wrapper broke native form association');
        fieldset.disabled = true;
        require(new FormData(form).get('custom-field') === null && disabled.at(-1) === true, 'disabled fieldset did not reach custom form control');
        fieldset.disabled = false;
        form.reset();
        require(new FormData(form).get('custom-field') === 'reset value' && disabled.at(-1) === false, 'native custom field reset failed');
        formHost.close();
        require(new FormData(form).get('custom-field') === null, 'disposed custom field remained in native submission');
      } finally { formHost.close(); form.remove(); }
      iframe = document.createElement('iframe');
      const loaded = new Promise(resolve => { iframe.onload = resolve; });
      iframe.srcdoc = '<!doctype html><body></body>'; document.body.append(iframe); await loaded;
      const frameStats = { connected: 0, disconnected: 0, properties: 0 };
      define(iframe.contentWindow, frameStats);
      const frameRoot = iframe.contentDocument.body.appendChild(iframe.contentDocument.createElement('div'));
      host.apply(4, frameRoot, 'counter', payload({}, { model: { count: 7 } }));
      require(frameRoot.firstElementChild instanceof iframe.contentWindow.HTMLElement && frameStats.connected === 1, 'adapter used the wrong document registry');
      host.close();
      require(frameStats.disconnected === 1 && frameRoot.childNodes.length === 0, 'cross-document close left a connection');
      require(stats.connected === stats.disconnected, 'connected elements were not all disposed');
      return { passed: true, contracts: ['attributes-and-object-properties', 'unchanged-setter-skipped', 'omitted-property-reset',
        'shadow-dom-and-local-state-retention', 'frozen-adapter-declaration', 'json-event-detail', 'bounded-and-cyclic-events',
        'initialization-and-update-failure-cleanup', 'declared-properties', 'native-registry-per-document',
        'native-form-associated-values-disabled-reset-and-removal', 'idempotent-disposal'] };
    } finally { host.close(); root.remove(); iframe?.remove(); }
  });
}

export async function checkCustomElement(page) {
  const element = page.locator('vo-example-counter');
  await element.waitFor();
  assert.equal(await page.locator('[data-custom-error]').textContent(), '');
  await element.getByRole('textbox', { name: 'Component note' }).fill('Keep this thought.');
  await page.evaluate(() => { window.customElement = document.querySelector('vo-example-counter'); });
  await element.getByRole('button').click();
  await page.waitForFunction(() => document.querySelector('[data-custom-count]').textContent === '1 moments of encouragement');
  await page.locator('[data-custom-look]').click();
  await page.waitForFunction(() => document.querySelector('vo-example-counter').getAttribute('accent') === 'plum');
  assert.equal(await element.locator('h3').textContent(), 'A different perspective');
  assert(await page.evaluate(() => window.customElement === document.querySelector('vo-example-counter')));
  assert.equal(await element.getByRole('textbox', { name: 'Component note' }).inputValue(), 'Keep this thought.');
  await page.locator('[data-custom-look]').click();
  await page.waitForFunction(() => !document.querySelector('vo-example-counter').hasAttribute('accent'));
  await element.getByRole('button').evaluate(button => { for (let count = 0; count < 25; count++) button.click(); });
  await page.waitForFunction(() => document.querySelector('[data-custom-count]').textContent === '26 moments of encouragement');
  await page.locator('[data-custom-toggle]').click();
  await element.waitFor({ state: 'detached' });
  await page.locator('[data-custom-toggle]').click();
  await element.waitFor();
  assert.equal(await element.getByRole('textbox', { name: 'Component note' }).inputValue(), '');
  assert.equal(await page.locator('[data-custom-count]').textContent(), '26 moments of encouragement');
  assert.equal(await page.locator('[data-custom-error]').textContent(), '');
}
