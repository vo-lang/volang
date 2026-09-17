import assert from 'node:assert/strict';

// Browser-boundary checks exercise protocol failures and native DOM identity
// independently of the guest reconciler that produces normal application input.
export async function checkDomContracts(page, url) {
  await page.route('**/boundary-contracts', route => route.fulfill({ contentType: 'text/html', body: '<!doctype html><title>DOM contracts</title>' }));
  await page.goto(`${url}/boundary-contracts`);
  const result = await page.evaluate(async () => {
    const { DomRenderer } = await import('/host/ui_next/renderer.js');
    const { WIRE_VERSION } = await import('/host/ui_next/generated/protocol.js');
    const { encodeBatch } = await import('/host/ui_next/generated/codec.js');
    const require = (ok, message) => { if (!ok) throw new Error(message); };
    const mutation = (op, id, fields = {}) => ({ op, id, parent: 0, before: 0, name: '', value: op === 'listen' ? '0' : '', ...fields });
    const bytes = (revision, mutations, inputSequence = 0) => encodeBatch({ version: WIRE_VERSION, revision, inputSequence, mutations, commands: null });
    const rejected = action => { let caught = false; try { action(); } catch { caught = true; } require(caught, 'malformed batch accepted'); };
    const containers = [document.createElement('div'), document.createElement('div')];
    document.body.append(...containers);
    const received = [[], []];
    const roots = containers.map((container, index) => new DomRenderer(container, event => received[index].push(event)));
    const initial = bytes(1, [
      mutation('create', 1, { name: '#fragment' }), mutation('insert', 1),
      mutation('create', 2, { name: 'input' }), mutation('insert', 2, { parent: 1 }),
      mutation('attr', 2, { name: 'value', value: 'alpha' }), mutation('listen', 2, { name: 'input' }),
      mutation('create', 3, { name: '#text', value: 'stable' }), mutation('insert', 3),
    ]);
    for (const root of roots) root.apply(initial);
    const input = containers[0].querySelector('input');
    rejected(() => roots[0].apply(bytes(2, [mutation('attr', 2, { name: 'title', value: 'partial' }), mutation('attr', 2, { name: 'TYPE', value: 'file' })])));
    require(input.value === 'alpha' && !input.hasAttribute('title'), 'uppercase file type partially committed');
    input.focus(); input.setSelectionRange(1, 3);
    rejected(() => roots[0].apply(bytes(2, [mutation('text', 3, { value: 'partial write' }), mutation('attr', 999, { name: 'title', value: 'invalid' })])));
    require(containers[0].textContent === 'stable' && input.value === 'alpha', 'failed transaction changed DOM');
    rejected(() => roots[0].apply(bytes(2, [mutation('text', 3, { value: 'partial' }), mutation('focus', 3)])));
    rejected(() => roots[0].apply(bytes(2, [mutation('focus', 2), mutation('remove', 1)])));
    rejected(() => roots[0].apply(bytes(2, [mutation('text', 3, { value: 'partial' }), mutation('scroll', 3)])));
    rejected(() => roots[0].apply(bytes(2, [mutation('scroll', 2), mutation('remove', 1)])));
    rejected(() => roots[0].apply(bytes(2, [mutation('scroll', 2, { name: 'unknown' })])));
    require(containers[0].textContent === 'stable' && input.isConnected, 'invalid focus command partially committed');
    rejected(() => roots[0].apply(bytes(2, [mutation('insert', 1, { parent: 1 })])));
    rejected(() => roots[0].apply(bytes(2, [mutation('insert', 3, { parent: 2 })])));
    rejected(() => roots[0].apply(bytes(2, [mutation('create', 2, { name: 'div' })])));
    roots[0].apply(bytes(2, [mutation('insert', 1)]));
    require(document.activeElement === input && input.selectionStart === 1 && input.selectionEnd === 3, 'range move lost focus or selection');
    require(containers[0].querySelector('input') === input, 'range move replaced input');
    input.value = 'new native edit'; input.dispatchEvent(new InputEvent('input', { bubbles: true }));
    roots[0].apply(bytes(3, [mutation('attr', 2, { name: 'value', value: 'guest value' })]));
    require(input.value === 'new native edit', 'older commit overwrote native input');
    roots[0].apply(bytes(4, [], 1));
    require(input.value === 'guest value', 'acknowledged rejected edit did not settle');
    require(received[0].length === 1 && received[1].length === 0, 'roots share input');
    roots[0].apply(bytes(5, [mutation('remove', 1)], 1));
    input.dispatchEvent(new InputEvent('input', { bubbles: true }));
    require(received[0].length === 1, 'removed input listener retained');
    roots[0].close(); roots[0].close();
    const other = containers[1].querySelector('input');
    other.value = 'still alive'; other.dispatchEvent(new InputEvent('input', { bubbles: true }));
    require(received[1].length === 1, 'closing one root closed another');
    roots[1].close();

    const scrolling = new DomRenderer(containers[0], () => {});
    scrolling.apply(bytes(1, [mutation('create', 1, { name: 'input' }), mutation('insert', 1),
      mutation('create', 2, { name: 'div' }), mutation('insert', 2),
      mutation('attr', 2, { name: 'style', value: 'height:60px;overflow:auto' }),
      mutation('create', 3, { name: 'div' }), mutation('insert', 3, { parent: 2 }),
      mutation('attr', 3, { name: 'style', value: 'height:300px' }),
      mutation('create', 4, { name: 'div' }), mutation('insert', 4, { parent: 2 }),
      mutation('attr', 4, { name: 'style', value: 'height:30px' })]));
    const stationary = containers[0].querySelector('input');
    stationary.focus();
    scrolling.apply(bytes(2, [mutation('scroll', 4)]));
    require(containers[0].querySelector('div').scrollTop > 200 && document.activeElement === stationary, 'scroll did not reveal its target while retaining focus');
    scrolling.close();

    const checkbox = new DomRenderer(containers[0], () => {});
    checkbox.apply(bytes(1, [mutation('create', 1, { name: 'input' }), mutation('insert', 1),
      mutation('attr', 1, { name: 'type', value: 'checkbox' }), mutation('attr', 1, { name: 'checked', value: 'false' }), mutation('listen', 1, { name: 'change' })]));
    const control = containers[0].querySelector('input');
    control.click();
    checkbox.apply(bytes(2, [mutation('attr', 1, { name: 'checked', value: 'false' })]));
    require(control.checked, 'older commit overwrote native checkbox');
    checkbox.apply(bytes(3, [], 1));
    require(!control.checked, 'controlled checkbox did not settle');
    checkbox.close();
    const file = new DomRenderer(containers[0], () => {});
    file.apply(bytes(1, [mutation('create', 1, { name: 'input' }), mutation('insert', 1),
      mutation('attr', 1, { name: 'value', value: 'kept' })]));
    rejected(() => file.apply(bytes(2, [mutation('attr', 1, { name: 'title', value: 'partial' }), mutation('attr', 1, { name: 'type', value: 'file' })])));
    require(containers[0].querySelector('input').value === 'kept' && !containers[0].querySelector('input').hasAttribute('title'), 'file value rejection partially committed');
    file.apply(bytes(2, [mutation('attr', 1, { name: 'type', value: 'file' }), mutation('attr', 1, { name: 'value', value: '' })]));
    require(containers[0].querySelector('input').type === 'file', 'empty file input rejected');
    file.close();
    const aliases = new DomRenderer(containers[0], () => {});
    aliases.apply(bytes(1, [mutation('create', 1, { name: 'input' }), mutation('insert', 1),
      mutation('attr', 1, { name: 'VaLuE', value: 'alias' }), mutation('attr', 1, { name: 'DISABLED', value: 'false' }),
      mutation('create', 2, { name: 'textarea' }), mutation('insert', 2), mutation('attr', 2, { name: 'VALUE', value: 'notes' }),
      mutation('create', 3, { name: 'input' }), mutation('insert', 3), mutation('attr', 3, { name: 'TYPE', value: 'checkbox' }),
      mutation('attr', 3, { name: 'CHECKED', value: 'false' })]));
    const aliasInput = containers[0].querySelector('input');
    require(aliasInput.value === 'alias' && !aliasInput.disabled && containers[0].querySelector('textarea').value === 'notes'
      && !containers[0].querySelector('input[type=checkbox]').checked, 'HTML attribute aliases changed control semantics');
    aliases.apply(bytes(2, [mutation('attr', 1, { name: 'VALUE', value: 'updated' })]));
    require(aliasInput.value === 'updated', 'HTML attribute alias did not update controlled state');
    aliases.close();
    const svg = new DomRenderer(containers[0], () => {});
    svg.apply(bytes(1, [mutation('create', 1, { name: 'svg' }), mutation('insert', 1),
      mutation('create', 2, { name: '#fragment' }), mutation('insert', 2, { parent: 1 }),
      mutation('create', 3, { name: 'circle' }), mutation('insert', 3, { parent: 2 }),
      mutation('attr', 3, { name: 'hidden', value: 'false' }),
      mutation('create', 4, { name: 'foreignObject' }), mutation('insert', 4, { parent: 1 }),
      mutation('create', 5, { name: 'textarea' }), mutation('insert', 5, { parent: 4 }),
      mutation('attr', 5, { name: 'value', value: 'hello' }),
    ]));
    require(containers[0].querySelector('circle').namespaceURI === 'http://www.w3.org/2000/svg', 'SVG namespace lost through a fragment');
    require(containers[0].querySelector('circle').getAttribute('hidden') === 'false', 'HTML boolean semantics applied to SVG');
    require(containers[0].querySelector('textarea').namespaceURI === 'http://www.w3.org/1999/xhtml', 'foreignObject children lost HTML namespace');
    rejected(() => svg.apply(bytes(2, [mutation('insert', 3, { parent: 0 })])));
    rejected(() => svg.apply(bytes(2, [mutation('attr', 5, { name: 'value', value: 'partial' }), mutation('create', 6, { name: '#text', value: 'child' }), mutation('insert', 6, { parent: 5 })])));
    require(containers[0].querySelector('textarea').value === 'hello', 'invalid raw text child partially changed textarea');
    svg.close();
    const phaseEvents = [];
    const phases = new DomRenderer(containers[0], event => phaseEvents.push(event));
    phases.apply(bytes(1, [mutation('create', 1, { name: 'div' }), mutation('insert', 1),
      mutation('listen', 1, { name: 'click:capture', value: '1' }), mutation('listen', 1, { name: 'click', value: '0' }),
      mutation('create', 2, { name: 'button' }), mutation('insert', 2, { parent: 1 }),
      mutation('listen', 2, { name: 'click', value: '2' }),
    ]));
    const button = containers[0].querySelector('button');
    const click = () => new MouseEvent('click', { bubbles: true, cancelable: true, ctrlKey: true, shiftKey: true, button: 1 });
    require(!button.dispatchEvent(click()), 'preventDefault waited for guest execution');
    require(phaseEvents.map(event => `${event.target}:${event.capture}`).join() === '1:true,2:false,1:false', 'native capture/bubble order changed');
    require(phaseEvents.every(event => event.ctrlKey && event.shiftKey && event.button === 1), 'native event payload lost modifiers');
    rejected(() => phases.apply(bytes(2, [mutation('attr', 2, { name: 'title', value: 'partial' }), mutation('listen', 2, { name: 'click', value: '10' })], 3)));
    require(!button.hasAttribute('title'), 'invalid passive/prevent options partially committed');
    phases.apply(bytes(2, [mutation('listen', 2, { name: 'click', value: '6' })], 3));
    button.dispatchEvent(click());
    require(phaseEvents.length === 5 && phaseEvents[4].target === 2, 'changed propagation options did not suppress the ancestor');
    phases.apply(bytes(3, [mutation('unlisten', 1, { name: 'click:capture' })], 5));
    button.dispatchEvent(click());
    require(phaseEvents.length === 6 && phaseEvents[5].target === 2, 'capture removal retained a native listener');
    phases.close();
    button.dispatchEvent(click());
    require(phaseEvents.length === 6, 'closed event owner retained a native listener');
    containers[0].innerHTML = '<input data-vo-id="1" type="number" value="invalid" data-vo-events="input=0">' +
      '<input data-vo-id="2" type="checkbox" data-vo-events="change=0">' +
      '<select data-vo-id="3" value="missing" data-vo-events="change=0"><option data-vo-id="4" value="first"></option></select>';
    const unbound = containers[0].querySelector('[type=checkbox]');
    unbound.checked = true;
    const initialEvents = [];
    const nativeDefaults = new DomRenderer(containers[0], event => initialEvents.push(event), true);
    require(initialEvents.length === 1 && initialEvents[0].target === 2 && initialEvents[0].checked, 'pre-boot unbound checkbox edit was lost or browser normalization generated extra input');
    nativeDefaults.apply(bytes(1, [mutation('create', 1, { name: 'input' }), mutation('insert', 1),
      mutation('attr', 1, { name: 'type', value: 'number' }), mutation('attr', 1, { name: 'value', value: 'invalid' }), mutation('listen', 1, { name: 'input' }),
      mutation('create', 2, { name: 'input' }), mutation('insert', 2), mutation('attr', 2, { name: 'type', value: 'checkbox' }), mutation('listen', 2, { name: 'change' }),
      mutation('create', 3, { name: 'select' }), mutation('insert', 3), mutation('attr', 3, { name: 'value', value: 'missing' }), mutation('listen', 3, { name: 'change' }),
      mutation('create', 4, { name: 'option' }), mutation('insert', 4, { parent: 3 }), mutation('attr', 4, { name: 'value', value: 'first' }),
    ]));
    require(unbound.checked, 'hydration controlled an unbound checkbox');
    require(initialEvents.length === 1 && containers[0].querySelector('select').value === '', 'unmatched select value became a synthetic user edit');
    nativeDefaults.close();
    const controlEvents = [];
    const formValues = new DomRenderer(containers[0], event => controlEvents.push(event));
    formValues.apply(bytes(1, [mutation('create', 1, { name: 'form' }), mutation('insert', 1),
      mutation('create', 2, { name: 'select' }), mutation('insert', 2, { parent: 1 }), mutation('attr', 2, { name: 'value', value: 'b' }),
      mutation('create', 3, { name: 'option' }), mutation('insert', 3, { parent: 2 }), mutation('attr', 3, { name: 'value', value: 'a' }),
      mutation('create', 4, { name: 'option' }), mutation('insert', 4, { parent: 2 }), mutation('attr', 4, { name: 'value', value: 'b' }),
      mutation('create', 5, { name: 'input' }), mutation('insert', 5, { parent: 1 }), mutation('attr', 5, { name: 'value', value: 'kept' }),
      mutation('create', 6, { name: 'input' }), mutation('insert', 6, { parent: 1 }), mutation('attr', 6, { name: 'type', value: 'checkbox' }),
      mutation('create', 7, { name: 'output' }), mutation('insert', 7, { parent: 1 }),
      mutation('create', 8, { name: '#fragment' }), mutation('insert', 8, { parent: 7 }),
      mutation('create', 9, { name: '#text' }), mutation('insert', 9, { parent: 8 }), mutation('text', 9, { value: 'Before reset' }),
    ]));
    const select = containers[0].querySelector('select');
    formValues.apply(bytes(2, [mutation('attr', 4, { name: 'value', value: 'c' })]));
    require(select.selectedIndex === -1, 'option mutation changed the controlled select value');
    formValues.apply(bytes(3, [mutation('attr', 3, { name: 'value', value: 'b' })]));
    require(select.selectedIndex === 0, 'new matching option did not settle the controlled select');
    const uncontrolled = containers[0].querySelector('[type=checkbox]');
    const output = containers[0].querySelector('output');
    const originalChildren = [...output.childNodes];
    uncontrolled.checked = true;
    containers[0].querySelector('form').reset();
    containers[0].querySelector('form').reset();
    require(controlEvents.length === 2 && controlEvents.every(event => event.kind === '@reset'), 'native reset did not schedule controlled settlement');
    formValues.apply(bytes(4, [mutation('text', 9, { value: 'Reset complete' })], 2));
    require(containers[0].querySelector('input').value === 'kept' && !uncontrolled.checked, 'reset mixed controlled and uncontrolled values');
    require(output.textContent === 'Reset complete' && originalChildren.every((child, index) => output.childNodes[index] === child), 'native output reset replaced managed text/range identities');
    formValues.apply(bytes(5, [mutation('text', 9, { value: 'Updated after reset' })], 2));
    require(output.textContent === 'Updated after reset', 'output lost subsequent guest updates');
    formValues.close();
    return { passed: true, contracts: ['atomic-rejection', 'cycles-and-void-children', 'stale-identities', 'moved-focus-selection', 'native-value-order', 'native-checked-order', 'listener-release', 'multiple-roots', 'svg-namespace-and-foreignObject', 'raw-text-child-rejection', 'native-event-phase-order', 'synchronous-prevention', 'option-rebinding', 'capture-listener-removal', 'native-reset-settlement', 'output-reset-preserves-managed-nodes'] };
  });
  assert.equal(result.passed, true);
  return result;
}
