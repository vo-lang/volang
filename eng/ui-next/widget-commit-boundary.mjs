export async function checkWidgetCommitBoundary(page, url) {
  await page.route('**/widget-commit', route => route.fulfill({contentType:'text/html', body:'<!doctype html><title>Widget commit</title>'}));
  await page.goto(url + '/widget-commit');
  return page.evaluate(async () => {
    const {DomRenderer} = await import('/host/ui_next/renderer.js');
    const {WIRE_VERSION} = await import('/host/ui_next/generated/protocol.js');
    const require = (condition, message) => {if (!condition) throw new Error(message);};
    const container = document.body.appendChild(document.createElement('div'));
    const events = [], observations = [];
    let mounts = 0, disposals = 0, signal;
    const renderer = new DomRenderer(container, event => events.push(event), false, {
      recorder(context) {
        mounts++; signal = context.signal;
        const control = context.element.previousElementSibling;
        observations.push({phase:'mount', value:control.value});
        return {
          update() {observations.push({phase:'update', value:control.value});},
          afterCommit() {
            if (context.element.hasAttribute('data-fail')) throw new Error('commit failed');
            observations.push({phase:'commit', value:control.value, focused:document.activeElement === control,
              disabled:control.matches(':disabled'), parent:control.parentElement.id});
          },
          dispose() {disposals++;},
        };
      },
    });
    const mutation = (op, id, fields = {}) => ({op, id, parent:0, before:0, name:'', value:'', ...fields});
    let revision = 0;
    const apply = (mutations, inputSequence = 0) => renderer.applyBatch({version:WIRE_VERSION, revision:++revision, inputSequence, mutations, commands:[]});
    try {
      apply([mutation('create', 1, {name:'#fragment'}), mutation('insert', 1),
        mutation('create', 2, {name:'fieldset'}), mutation('insert', 2, {parent:1}),
        mutation('create', 3, {name:'textarea'}), mutation('insert', 3, {parent:2}),
        mutation('attr', 3, {name:'value', value:'initial'}), mutation('listen', 3, {name:'input', value:'0'}),
        mutation('create', 4, {name:'#widget'}), mutation('insert', 4, {parent:2}),
        mutation('widget', 4, {name:'recorder', value:'initial'}), mutation('focus', 3)]);
      require(observations[0].value === 'initial', 'widget mounted before initial controlled values applied');
      require(observations.at(-1).focused, 'afterCommit ran before focus requests');
      const control = container.querySelector('textarea');
      control.value = 'new local edit';
      control.dispatchEvent(new InputEvent('input', {bubbles:true}));
      const sequence = events.at(-1).sequence;
      apply([mutation('attr', 3, {name:'value', value:'controlled correction'}),
        mutation('widget', 4, {name:'recorder', value:'changed'})]);
      require(observations.at(-1).value === 'new local edit' && observations.at(-2).value === 'new local edit',
        'widget observed a stale desired value while user input was unacknowledged');
      apply([], sequence);
      require(observations.at(-1).value === 'controlled correction',
        'deferred native value changed without a widget commit notification');
      apply([mutation('create', 5, {name:'fieldset'}), mutation('insert', 5, {parent:1}),
        mutation('attr', 5, {name:'id', value:'disabled-owner'}), mutation('attr', 5, {name:'disabled', value:'true'}),
        mutation('insert', 3, {parent:5}), mutation('insert', 4, {parent:5}), mutation('remove', 2)], sequence);
      require(observations.at(-1).disabled && observations.at(-1).parent === 'disabled-owner' && mounts === 1,
        'a moved widget missed its new native ancestor state or remounted');
      apply([mutation('attr', 4, {name:'data-fail', value:'true'})], sequence);
      require(events.at(-1).error === 'commit failed' && signal.aborted && disposals === 1,
        'afterCommit failure did not abort/dispose and report locally');
      renderer.close();
      require(disposals === 1 && !container.childNodes.length, 'failed widget was disposed twice');
      return {passed:true, contracts:['native-values-before-widget-mount', 'post-focus-callback',
        'pending-input-acknowledgement', 'native-flush-without-widget-mutation', 'moved-ancestor-state',
        'local-commit-failure', 'exact-disposal']};
    } finally {renderer.close(); container.remove();}
  });
}
