import assert from 'node:assert/strict';

export async function checkShadowFocusBoundary(page, url) {
  await page.route('**/__shadow_focus', route => route.fulfill({ contentType: 'text/html', body: '<!doctype html><body></body>' }));
  await page.goto(`${url}/__shadow_focus`);
  const order = await page.evaluate(async () => {
    const { tabOrder, containTab, activeElement, composedContains } = await import('/host/ui_next/focus.js');
    const dialog = document.body.appendChild(document.createElement('dialog'));
    dialog.innerHTML = '<button id="first">First</button><div id="host"><button id="slotted">Slot</button></div><input id="outer-radio" type="radio" name="choice" checked><button id="last">Last</button><button id="outer-positive" tabindex="1">Priority</button>';
    const host = dialog.querySelector('#host');
    const shadow = host.attachShadow({ mode: 'open' });
    shadow.innerHTML = '<button id="inner-positive" tabindex="2">Inner priority</button><input id="inner"><slot></slot><div inert><input id="inert-input"></div><div id="nested"></div><input id="shadow-radio-a" type="radio" name="choice"><input id="shadow-radio-b" type="radio" name="choice" checked>';
    shadow.querySelector('#nested').attachShadow({ mode: 'open' }).innerHTML = '<input id="deep">';
    dialog.addEventListener('keydown', event => containTab(event, dialog));
    dialog.showModal();
    const order = tabOrder(dialog).map(element => element.id);
    const inner = shadow.querySelector('#inner'); inner.focus();
    if (activeElement(document) !== inner || !composedContains(dialog, inner)) throw new Error('open shadow focus was not resolved');
    window.shadowFocus = { dialog, host, active: () => activeElement(document)?.id };
    dialog.querySelector('#first').focus();
    return order;
  });
  assert.deepEqual(order, ['outer-positive', 'first', 'inner-positive', 'inner', 'slotted', 'deep', 'shadow-radio-b', 'outer-radio', 'last']);
  for (const id of ['inner-positive', 'inner', 'slotted', 'deep']) {
    await page.keyboard.press('Tab');
    assert.equal(await page.evaluate(() => window.shadowFocus.active()), id);
  }
  await page.keyboard.press('Shift+Tab');
  assert.equal(await page.evaluate(() => window.shadowFocus.active()), 'slotted');
  await page.evaluate(() => document.querySelector('#last').focus());
  await page.keyboard.press('Tab');
  assert.equal(await page.evaluate(() => window.shadowFocus.active()), 'outer-positive');
  const retained = await page.evaluate(async () => {
    const { DomRenderer } = await import('/host/ui_next/renderer.js');
    const { WIRE_VERSION } = await import('/host/ui_next/generated/protocol.js');
    const { activeElement } = await import('/host/ui_next/focus.js');
    const { createCustomElementWidget } = await import('/host/ui_next/custom-element.js');
    window.shadowFocus.dialog.close(); window.shadowFocus.dialog.remove();
    customElements.define('test-focus-note', class extends HTMLElement {
      constructor() { super(); this.attachShadow({ mode: 'open' }).innerHTML = '<input aria-label="Movable note">'; }
    });
    const root = document.body.appendChild(document.createElement('div'));
    const renderer = new DomRenderer(root, () => {}, false, { note: createCustomElementWidget({ tag: 'test-focus-note' }) });
    const mutation = (op, id, name = '', value = '', before = 0) => ({ op, id, name, value, parent: 0, before });
    const batch = (revision, mutations) => ({ version: WIRE_VERSION, revision, inputSequence: 0, mutations, commands: [] });
    try {
      renderer.applyBatch(batch(1, [mutation('create', 1, '#widget'), mutation('insert', 1),
        mutation('widget', 1, 'note', '{"version":1,"attributes":{},"properties":null}'),
        mutation('create', 2, 'button'), mutation('insert', 2)]));
      const element = root.querySelector('test-focus-note');
      const input = element.shadowRoot.querySelector('input');
      input.value = 'A movable thought'; input.focus(); input.setSelectionRange(2, 8, 'backward');
      renderer.applyBatch(batch(2, [mutation('insert', 1)]));
      return root.querySelector('test-focus-note') === element && activeElement(document) === input
        && input.selectionStart === 2 && input.selectionEnd === 8 && input.selectionDirection === 'backward';
    } finally { renderer.close(); root.remove(); }
  });
  assert(retained, 'moving a managed Custom Element lost its internal focus or selection');
  return { passed: true, contracts: ['nested-open-shadow-controls', 'assigned-slot-order', 'scoped-positive-tabindex',
    'inert-ancestor', 'radio-groups-per-tree', 'modal-tab-and-reverse-wrap', 'managed-shadow-focus-and-selection-retention'] };
}
