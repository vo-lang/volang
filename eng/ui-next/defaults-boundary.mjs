export async function checkDefaultsBoundary(page, url) {
  await page.route('**/defaults-boundary', route => route.fulfill({ contentType: 'text/html', body: '<!doctype html><title>Native defaults</title>' }));
  await page.goto(`${url}/defaults-boundary`);
  return page.evaluate(async () => {
    const { DomRenderer } = await import('/host/ui_next/renderer.js');
    const { WIRE_VERSION } = await import('/host/ui_next/generated/protocol.js');
    const m = (op, id, fields = {}) => ({ op, id, parent: 0, before: 0, name: '', value: op === 'listen' ? '0' : '', ...fields });
    const batch = (revision, mutations, inputSequence = 0) => ({ version: WIRE_VERSION, revision, inputSequence, mutations, commands: null });
    const require = (ok, message) => { if (!ok) throw new Error(message); };
    const rejected = action => { let failed = false; try { action(); } catch { failed = true; } require(failed, 'invalid default binding was accepted'); };
    const container = document.createElement('div');
    document.body.append(container);
    const root = new DomRenderer(container, () => {});
    root.applyBatch(batch(1, [m('create', 1, { name: 'form' }), m('insert', 1),
      m('create', 2, { name: 'input' }), m('insert', 2, { parent: 1 }), m('default', 2, { name: 'value', value: 'Original' }),
      m('create', 3, { name: 'textarea' }), m('insert', 3, { parent: 1 }), m('default', 3, { name: 'value', value: '\nOriginal' }),
      m('create', 4, { name: 'input' }), m('insert', 4, { parent: 1 }), m('default', 4, { name: 'checked', value: 'true' }),
      m('attr', 4, { name: 'type', value: 'checkbox' })]));
    const [input, check] = container.querySelectorAll('input');
    const notes = container.querySelector('textarea');
    require(input.value === 'Original' && notes.value === '\nOriginal' && check.checked, 'defaults were not applied after element attributes');
    input.value = 'Edited'; notes.value = 'Edited notes'; check.checked = false;
    root.applyBatch(batch(2, [m('default', 2, { name: 'value', value: 'Updated' }),
      m('default', 3, { name: 'value', value: 'Updated notes' }), m('default', 4, { name: 'checked', value: 'false' })]));
    root.applyBatch(batch(3, [m('default', 2, { name: 'value', value: '' }), m('default', 4, { name: 'checked', value: 'true' })]));
    require(input.value === 'Edited' && notes.value === 'Edited notes' && !check.checked, 'reset baseline changed edited live values');
    for (const invalid of [m('default', 1, { name: 'value', value: 'invalid' }),
      m('default', 2, { name: 'unknown' }), m('default', 4, { name: 'checked', value: 'maybe' }),
      m('default', 3, { name: 'checked', value: 'true' })]) {
      rejected(() => root.applyBatch(batch(4, [m('attr', 2, { name: 'title', value: 'partial' }), invalid])));
    }
    rejected(() => root.applyBatch(batch(4, [m('default', 2, { name: 'value', value: 'removed' }), m('remove', 2)])));
    rejected(() => root.applyBatch(batch(4, [m('attr', 2, { name: 'type', value: 'file' }), m('default', 2, { name: 'value', value: 'unavailable.txt' })])));
    require(!input.hasAttribute('title') && input.type === 'text' && input.isConnected, 'invalid defaults partially committed');
    container.querySelector('form').reset();
    require(input.value === '' && notes.value === 'Updated notes' && check.checked, 'reset did not use the latest defaults');
    root.close();

    const html = '<form data-vo-id="1"><input data-vo-id="2" type="number" value="invalid" data-vo-default-value="" data-vo-events="input=0">'
      + '<input data-vo-id="3" type="radio" name="choice" checked data-vo-events="change=0">'
      + '<input data-vo-id="4" type="radio" name="choice" checked data-vo-events="change=0"></form>';
    container.innerHTML = html;
    const initialEvents = [];
    const initial = new DomRenderer(container, event => initialEvents.push(event), true);
    require(initialEvents.length === 0, 'native radio group resolution or number sanitization looked like an early edit');
    initial.close();
    container.innerHTML = html;
    const first = container.querySelector('[data-vo-id="3"]');
    first.checked = true;
    const edits = [];
    const adopted = new DomRenderer(container, event => edits.push(event), true);
    require(edits.length === 2 && edits[0].target === 3 && edits[0].checked && edits[1].target === 4 && !edits[1].checked, 'pre-boot radio selection was not captured');
    adopted.applyBatch(batch(1, [m('create', 1, { name: 'form' }), m('insert', 1),
      m('create', 2, { name: 'input' }), m('insert', 2, { parent: 1 }), m('attr', 2, { name: 'type', value: 'number' }),
      m('default', 2, { name: 'value', value: 'invalid' }), m('listen', 2, { name: 'input' }),
      m('create', 3, { name: 'input' }), m('insert', 3, { parent: 1 }), m('attr', 3, { name: 'type', value: 'radio' }),
      m('attr', 3, { name: 'name', value: 'choice' }), m('default', 3, { name: 'checked', value: 'true' }), m('listen', 3, { name: 'change' }),
      m('create', 4, { name: 'input' }), m('insert', 4, { parent: 1 }), m('attr', 4, { name: 'type', value: 'radio' }),
      m('attr', 4, { name: 'name', value: 'choice' }), m('default', 4, { name: 'checked', value: 'true' }), m('listen', 4, { name: 'change' })], 2));
    require(first.checked && !container.querySelector('[data-vo-id="4"]').checked, 'unchanged defaults overwrote early radio group selection');
    adopted.close();
    const acknowledgements = [];
    const fixed = new DomRenderer(container, event => acknowledgements.push(event));
    fixed.applyBatch(batch(1, [m('create', 1, { name: 'input' }), m('insert', 1),
      m('attr', 1, { name: 'value', value: 'Fixed text' }),
      m('create', 2, { name: 'input' }), m('insert', 2), m('attr', 2, { name: 'type', value: 'checkbox' }), m('attr', 2, { name: 'checked', value: 'false' }),
      m('create', 3, { name: 'input' }), m('insert', 3), m('default', 3, { name: 'value', value: 'Native text' })]));
    const [fixedText, fixedCheck, nativeText] = container.querySelectorAll('input');
    fixedText.value = 'Rejected edit'; fixedText.dispatchEvent(new InputEvent('input', { bubbles: true }));
    fixedCheck.click();
    nativeText.value = 'Accepted edit'; nativeText.dispatchEvent(new InputEvent('input', { bubbles: true }));
    require(acknowledgements.length > 0 && acknowledgements.every(event => event.kind === '@control' && event.target !== 3), 'unhandled controlled edits were not acknowledged or native edits generated work');
    fixed.applyBatch(batch(2, [], acknowledgements.at(-1).sequence));
    require(fixedText.value === 'Fixed text' && !fixedCheck.checked && nativeText.value === 'Accepted edit', 'controlled values without handlers did not settle independently from native defaults');
    fixed.close();
    const count = acknowledgements.length;
    fixedText.dispatchEvent(new InputEvent('input', { bubbles: true }));
    require(acknowledgements.length === count, 'closing retained control input ownership');
    container.remove();
    return ['native-defaults', 'edited-values-preserved', 'reset-baseline-updates', 'atomic-default-rejection',
      'radio-group-normalization', 'early-radio-selection-preserved', 'controlled-without-callback'];
  });
}
