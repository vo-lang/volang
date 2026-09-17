import assert from 'node:assert/strict';

export async function checkSelectionBoundary(page, url) {
  await page.route('**/__multiple_selection', route => route.fulfill({ contentType: 'text/html', body: '<!doctype html><body></body>' }));
  await page.goto(url + '/__multiple_selection');
  const result = await page.evaluate(async () => {
    const { DomRenderer } = await import('/host/ui_next/renderer.js');
    const { WIRE_VERSION } = await import('/host/ui_next/generated/protocol.js');
    const require = (value, message) => { if (!value) throw new Error(message); };
    const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);
    const m = (op, id, name = '', value = '', parent = 0) => ({ op, id, name, value, parent, before: 0 });
    for (const hydrate of [false, true]) {
      const form = document.body.appendChild(document.createElement('form'));
      const root = form.appendChild(document.createElement('div'));
      if (hydrate) {
        root.innerHTML = `<select data-vo-id="1" name="choice" multiple data-vo-selected='["a","b"]' data-vo-events="input=0 change=0"><option data-vo-id="2" value="a" selected></option><option data-vo-id="3" value="b" selected></option><option data-vo-id="4" value="c"></option></select>`;
        root.querySelector('option[value=b]').selected = false;
        root.querySelector('option[value=c]').selected = true;
      }
      const events = [], renderer = new DomRenderer(root, event => events.push(event), hydrate);
      let revision = 0;
      const batch = (mutations, inputSequence = events.at(-1)?.sequence ?? 0) => ({ version: WIRE_VERSION, revision: revision + 1, inputSequence, mutations, commands: [] });
      const apply = (mutations, inputSequence) => { renderer.applyBatch(batch(mutations, inputSequence)); revision++; };
      try {
        apply([m('create', 1, 'select'), m('insert', 1), m('attr', 1, 'name', 'choice'), m('attr', 1, 'multiple', 'true'),
          m('selection', 1, '', '["a","b"]'), m('listen', 1, 'input', '0'), m('listen', 1, 'change', '0'),
          ...['a', 'b', 'c'].flatMap((value, index) => [m('create', index + 2, 'option'), m('insert', index + 2, '', '', 1), m('attr', index + 2, 'value', value)]),
          m('attr', 3, 'selected', 'true')], 0);
        const select = root.querySelector('select'), current = () => [...select.selectedOptions].map(option => option.value);
        if (!hydrate) {
          require(same(current(), ['a', 'b']), 'controlled multiple selection did not mount');
          select.options[1].selected = false; select.options[2].selected = true;
          select.dispatchEvent(new Event('input', { bubbles: true }));
          select.dispatchEvent(new Event('change', { bubbles: true }));
        }
        require(same(current(), ['a', 'c']) && same(events.at(-1).selectedValues, ['a', 'c']) && events.at(-1).value === 'a',
          'a changed second selection was lost: ' + JSON.stringify({ hydrate, current: current(), event: events.at(-1) }));
        apply([], 0);
        require(same(current(), ['a', 'c']), 'an older commit replaced a newer native selection');
        apply([m('selection', 1, '', '["a","c"]')]);
        form.reset(); apply([]);
        require(same(current(), ['a', 'c']), 'native reset did not settle controlled selection');
        require(same(new FormData(form).getAll('choice'), ['a', 'c']), 'native repeated form values changed');
        for (const invalid of [m('selection', 1, '', 'null'), m('selection', 1, '', '[1]'), m('selection', 1, '', JSON.stringify(Array(4097).fill('a'))),
          m('selection', 2, '', '[]'), m('attr', 1, 'value', 'a'), m('removeAttr', 1, 'multiple')]) {
          const before = root.innerHTML;
          let rejected = false;
          try { renderer.applyBatch(batch([m('attr', 1, 'title', 'must not change'), invalid])); } catch { rejected = true; }
          require(rejected && root.innerHTML === before, 'invalid selection batch changed the live DOM');
        }
        apply([m('selection', 1, '', '["a","later"]')]);
        apply([m('create', 5, 'option'), m('insert', 5, '', '', 1), m('attr', 5, 'value', 'later')]);
        require(same(current(), ['a', 'later']), 'inserting an option did not reconcile controlled membership');
        apply([m('unlisten', 1, 'input'), m('unlisten', 1, 'change')]);
        select.options[0].selected = false;
        select.dispatchEvent(new Event('input', { bubbles: true }));
        require(events.at(-1).kind === '@control', 'read-only multiple selection did not request an acknowledgement');
        apply([]);
        require(same(current(), ['a', 'later']), 'read-only multiple selection retained an undeclared edit');
        apply([m('selection', 1, '', '')]);
        form.reset();
        require(same(current(), ['b']), `released selection did not use authored reset defaults (${hydrate ? 'SSR' : 'client'})`);
      } finally { renderer.close(); form.remove(); }
    }
    for (const hydrate of [false, true]) {
      const form = document.body.appendChild(document.createElement('form'));
      const root = form.appendChild(document.createElement('div'));
      if (hydrate) root.innerHTML = '<select data-vo-id="1" value="b"><option data-vo-id="2" value="a"></option><option data-vo-id="3" value="b" selected></option></select>';
      const renderer = new DomRenderer(root, () => {}, hydrate);
      try {
        renderer.applyBatch({ version: WIRE_VERSION, revision: 1, inputSequence: 0, commands: [], mutations: [
          m('create', 1, 'select'), m('insert', 1), m('attr', 1, 'value', 'b'),
          ...['a', 'b'].flatMap((value, index) => [m('create', index + 2, 'option'), m('insert', index + 2, '', '', 1), m('attr', index + 2, 'value', value)]),
          m('attr', 2, 'selected', 'true'),
        ] });
        const select = root.querySelector('select');
        require(select.value === 'b', 'authored defaults replaced the controlled scalar value');
        renderer.applyBatch({ version: WIRE_VERSION, revision: 2, inputSequence: 0, commands: [], mutations: [m('removeAttr', 1, 'value')] });
        form.reset();
        require(select.value === 'a', 'released scalar selection had a different SSR reset baseline');
      } finally { renderer.close(); form.remove(); }
    }
    return { passed: true, contracts: ['complete-input-and-change-values', 'second-value-pre-boot-edit', 'stale-commit-retention',
      'native-form-reset', 'repeated-form-data', 'atomic-selection-rejection', 'dynamic-option-membership', 'read-only-reconciliation', 'released-control-reset-parity', 'released-scalar-select-reset-parity'] };
  });
  assert(result.passed);
  return result;
}
