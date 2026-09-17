export async function checkKeyboardBoundary(page, url) {
  await page.route('**/keyboard-contracts', route => route.fulfill({ contentType: 'text/html', body: '<!doctype html><title>Keyboard contracts</title>' }));
  await page.goto(`${url}/keyboard-contracts`);
  return page.evaluate(async () => {
    const { DomRenderer } = await import('/host/ui_next/renderer.js');
    const { WIRE_VERSION, KEY_CTRL, KEY_META } = await import('/host/ui_next/generated/protocol.js');
    const { eventBinding } = await import('/host/ui_next/events.js');
    const require = (ok, message) => { if (!ok) throw new Error(message); };
    const mutation = (op, id, fields = {}) => ({ op, id, parent: 0, before: 0, name: '', value: '', ...fields });
    const batch = (revision, mutations) => ({ version: WIRE_VERSION, revision, inputSequence: 0, mutations, commands: [] });
    const container = document.createElement('div');
    document.body.append(container);
    const events = [];
    const renderer = new DomRenderer(container, event => events.push(event));
    renderer.applyBatch(batch(1, [mutation('create', 1, { name: 'button' }), mutation('insert', 1),
      mutation('listen', 1, { name: 'keydown', value: '2|["ArrowRight"]' })]));
    const button = container.querySelector('button');
    const press = (key, options = {}) => {
      const event = new KeyboardEvent('keydown', { key, bubbles: true, cancelable: true, ...options });
      button.dispatchEvent(event);
      return event.defaultPrevented;
    };
    require(!press('Tab') && !press('ArrowDown') && !press('ArrowRight', { isComposing: true }), 'filtered binding consumed unrelated native input');
    require(events.length === 0 && press('ArrowRight') && events.length === 1, 'matched key was not synchronously prevented and delivered once');
    renderer.applyBatch(batch(2, [mutation('listen', 1, { name: 'keydown', value: '2|["ArrowLeft"]' })]));
    require(!press('ArrowRight') && press('ArrowLeft') && events.length === 2, 'key filter update retained its old native listener');
    for (const value of ['2|[]', '2|[""]', '2|["x","x"]', '2|[1]', '2|null', '2|["\\ud800"]', `2|["${'a'.repeat(129)}"]`, '2|["x"']) {
      let rejected = false;
      try { renderer.applyBatch(batch(3, [mutation('attr', 1, { name: 'title', value: 'partial' }), mutation('listen', 1, { name: 'keydown', value })])); }
      catch { rejected = true; }
      require(rejected && !button.hasAttribute('title'), 'invalid key filter partially committed');
    }
    let rejected = false;
    try { eventBinding('click', '2|["Enter"]'); } catch { rejected = true; }
    require(rejected, 'non-keyboard listener accepted a key filter');
    renderer.close();
    require(!press('ArrowLeft') && events.length === 2, 'disposed listener retained keyboard interception');
    const binding = '2|["\\u0020","\\u003d","中","\\\""]';
    const server = document.createElement('button');
    server.setAttribute('data-vo-id', '1');
    server.setAttribute('data-vo-events', `keydown=${binding}`);
    container.innerHTML = server.outerHTML;
    const hydrated = new DomRenderer(container, event => events.push(event), true);
    const adopted = container.firstElementChild;
    hydrated.applyBatch(batch(1, [mutation('create', 1, { name: 'button' }), mutation('insert', 1), mutation('listen', 1, { name: 'keydown', value: binding })]));
    for (const key of [' ', '=', '中', '"']) adopted.dispatchEvent(new KeyboardEvent('keydown', { key, cancelable: true }));
    require(container.firstElementChild === adopted && events.slice(-4).map(event => event.key).join('') === ' =中"', 'SSR key filter lost token boundaries or DOM identity');
    hydrated.close();
    const modifierEvents = [];
    const modified = new DomRenderer(container, event => modifierEvents.push(event));
    const filtered = modifiers => `2|${JSON.stringify({ keys: ['Enter', '|'], modifiers })}`;
    modified.applyBatch(batch(1, [mutation('create', 1, { name: 'div' }), mutation('insert', 1),
      mutation('create', 2, { name: 'textarea' }), mutation('insert', 2, { parent: 1 }),
      mutation('listen', 1, { name: 'keydown', value: filtered([KEY_CTRL, KEY_META]) })]));
    const editor = container.querySelector('textarea');
    const shortcut = (options = {}, key = 'Enter') => {
      const event = new KeyboardEvent('keydown', { key, bubbles: true, cancelable: true, ...options });
      editor.dispatchEvent(event);
      return event.defaultPrevented;
    };
    for (const options of [{}, { shiftKey: true }, { ctrlKey: true, shiftKey: true },
      { ctrlKey: true, altKey: true }, { ctrlKey: true, metaKey: true }, { ctrlKey: true, isComposing: true }]) {
      require(!shortcut(options), 'modifier filter intercepted ordinary typing, extra modifiers or composition');
    }
    require(shortcut({ ctrlKey: true }) && shortcut({ metaKey: true }) && shortcut({ ctrlKey: true }, '|'), 'exact modifier alternatives or literal pipe key failed');
    require(modifierEvents.length === 3, 'modifier filtering dispatched unmatched events');
    editor.dispatchEvent(new CompositionEvent('compositionstart', { bubbles: true }));
    require(!shortcut({ ctrlKey: true }), 'ancestor shortcut ignored descendant composition state');
    editor.dispatchEvent(new CompositionEvent('compositionend', { bubbles: true }));
    require(shortcut({ metaKey: true }) && modifierEvents.length === 4, 'composition end did not restore shortcut');
    modified.applyBatch(batch(2, [mutation('listen', 1, { name: 'keydown', value: filtered([0]) })]));
    require(!shortcut({ ctrlKey: true }) && shortcut() && modifierEvents.length === 5, 'zero modifier rebinding failed');
    for (const filter of [{ modifiers: [] }, { modifiers: [0, 0] }, { modifiers: [16] },
      { modifiers: [-1] }, { modifiers: [1.5] }, { modifiers: ['2'] }, { modifiers: null },
      { modifiers: [0], extra: true }, { keys: ['Enter'] }, { modifiers: Array(17).fill(0) },
      { keys: 1, modifiers: [0] }]) {
      let rejected = false;
      try { modified.applyBatch(batch(3, [mutation('attr', 2, { name: 'title', value: 'partial' }),
        mutation('listen', 1, { name: 'keydown', value: `2|${JSON.stringify(filter)}` })])); }
      catch { rejected = true; }
      require(rejected && !editor.hasAttribute('title'), 'malformed modifier binding partially committed');
    }
    require(eventBinding('keyup', '0|{"modifiers":[0]}').modifiers.has(0), 'modifier-only filter was rejected');
    rejected = false;
    try { eventBinding('click', '2|{"modifiers":[0]}'); } catch { rejected = true; }
    require(rejected, 'non-keyboard listener accepted modifiers');
    modified.close();
    require(!shortcut() && modifierEvents.length === 5, 'disposed modifier listener intercepted input');
    const modifierBinding = `2|${JSON.stringify({ keys: ['|', ' ', '=', '"'], modifiers: [KEY_CTRL] }).replaceAll(' ', '\\u0020').replaceAll('=', '\\u003d')}`;
    server.setAttribute('data-vo-events', `keydown=${modifierBinding}`);
    container.innerHTML = server.outerHTML;
    const modifierHydrated = new DomRenderer(container, event => modifierEvents.push(event), true);
    const modifierAdopted = container.firstElementChild;
    modifierHydrated.applyBatch(batch(1, [mutation('create', 1, { name: 'button' }), mutation('insert', 1),
      mutation('listen', 1, { name: 'keydown', value: modifierBinding })]));
    for (const key of ['|', ' ', '=', '"']) {
      const event = new KeyboardEvent('keydown', { key, ctrlKey: true, cancelable: true });
      modifierAdopted.dispatchEvent(event);
      require(event.defaultPrevented, 'SSR modifier filter lost native prevention');
    }
    require(container.firstElementChild === modifierAdopted && modifierEvents.slice(-4).map(event => event.key).join('') === '| ="', 'SSR modifier filter lost escaping or identity');
    modifierHydrated.close();
    container.remove();
    return { passed: true, contracts: ['native-key-filter', 'unrelated-input-preserved', 'composition-bypass', 'key-rebinding', 'atomic-invalid-key-rejection', 'listener-disposal', 'ssr-escaped-keys',
      'exact-modifier-alternatives', 'altgr-extra-modifier-bypass', 'ancestor-composition-bypass', 'zero-modifier-rebinding', 'atomic-invalid-modifier-rejection', 'ssr-modifier-escaped-keys'] };
  });
}
