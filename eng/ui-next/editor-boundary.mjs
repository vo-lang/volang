import assert from 'node:assert/strict';

export async function checkCodeEditorBoundary(page, url) {
  await page.route('**/editor-boundary', route => route.fulfill({contentType:'text/html',
    body:'<!doctype html><title>Code editor</title><style>textarea{width:600px;height:340px}body{font-family:system-ui;margin:32px}</style>'}));
  await page.goto(url + '/editor-boundary');
  await page.evaluate(async () => {
    const library = await import('/artifacts/editor-library.js');
    const {createCodeEditorWidget} = await import('/host/ui_next/editor.js');
    const {DomRenderer} = await import('/host/ui_next/renderer.js');
    const {WIRE_VERSION} = await import('/host/ui_next/generated/protocol.js');
    const container = document.body.appendChild(document.createElement('div'));
    const outside = document.body.appendChild(document.createElement('button'));
    outside.id = 'editor-outside'; outside.textContent = 'Another control';
    const events = [];
    const renderer = new DomRenderer(container, event => events.push(event), false, {editor:createCodeEditorWidget(library)});
    const mutation = (op, id, fields = {}) => ({op, id, parent:0, before:0, name:'', value:'', ...fields});
    let revision = 0, acknowledged = 0;
    const apply = (mutations, inputSequence = acknowledged) => {
      renderer.applyBatch({version:WIRE_VERSION, revision:++revision, inputSequence, mutations, commands:[]});
      acknowledged = inputSequence;
    };
    apply([mutation('create', 1, {name:'form'}), mutation('insert', 1),
      mutation('create', 2, {name:'fieldset'}), mutation('insert', 2, {parent:1}),
      mutation('listen', 2, {name:'keydown:capture', value:'3|{"keys":["Enter"],"modifiers":[2]}'}),
      mutation('create', 3, {name:'textarea'}), mutation('insert', 3, {parent:2}),
      mutation('attr', 3, {name:'id', value:'editor-source'}), mutation('attr', 3, {name:'name', value:'source'}),
      mutation('attr', 3, {name:'aria-label', value:'Source code'}),
      mutation('attr', 3, {name:'maxlength', value:'100000'}),
      mutation('attr', 3, {name:'value', value:'package main\n\nfunc main() {}\n'}),
      mutation('listen', 3, {name:'input', value:'0'})]);
    const input = container.querySelector('textarea');
    window.editorBoundary = {
      container, renderer, input, events, library,
      get acknowledged() {return acknowledged;},
      attach() {apply([mutation('create', 4, {name:'#widget'}), mutation('insert', 4, {parent:2}),
        mutation('widget', 4, {name:'editor', value:JSON.stringify({version:1, inputID:'editor-source', language:'vo'})})], 0);},
      source(value, sequence = events.at(-1)?.sequence ?? 0) {apply([mutation('attr', 3, {name:'value', value})], sequence);},
      acknowledge() {apply([mutation('attr', 3, {name:'value', value:input.value})], events.at(-1)?.sequence ?? 0);},
      attribute(id, name, value) {apply([mutation('attr', id, {name, value})]);},
      removeAttribute(id, name) {apply([mutation('removeAttr', id, {name})]);},
      text(id, value) {apply([mutation('text', id, {value})]);},
      labels() {apply([
        mutation('create', 5, {name:'label'}), mutation('insert', 5, {parent:1}),
        mutation('attr', 5, {name:'for', value:'editor-source'}),
        mutation('create', 6, {name:'#text', value:'Original label'}), mutation('insert', 6, {parent:5}),
        mutation('create', 7, {name:'label'}), mutation('insert', 7, {parent:1}),
        mutation('attr', 7, {name:'for', value:'another-control'}),
        mutation('create', 8, {name:'#text', value:'Other label'}), mutation('insert', 8, {parent:7}),
      ]);},
      move() {apply([mutation('insert', 1)]);},
      view() {return library.EditorView.findFromDOM(container.querySelector('.cm-editor'));},
      focus() {apply([mutation('focus', 3)]);},
      select(source,start,end,direction='forward') {apply([mutation('textSelection',3,
        {value:JSON.stringify({source,selection:{start,end,direction}})})]);},
      removeEditor() {apply([mutation('remove', 4)]);},
    };
  });
  try {
    await page.locator('#editor-source').fill('A native draft 中文');
    await page.evaluate(() => {editorBoundary.input.setSelectionRange(2, 8); editorBoundary.attach();});
    assert.equal(await page.locator('.cm-editor').count(), 0, 'enhancement interrupted an active native edit');
    assert.equal(await page.locator('#editor-source').inputValue(), 'A native draft 中文');
    await page.locator('#editor-outside').focus();
    await page.locator('.cm-editor').waitFor();
    const adopted = await page.evaluate(() => ({
      text:editorBoundary.view().state.doc.toString(),
      selection:[editorBoundary.view().state.selection.main.from, editorBoundary.view().state.selection.main.to],
      nativeHeight:editorBoundary.input.getBoundingClientRect().height,
      nativeTabIndex:editorBoundary.input.tabIndex,
      active:document.activeElement.id,
    }));
    assert.equal(adopted.text, 'A native draft 中文');
    assert.deepEqual(adopted.selection, [2, 8]);
    assert(adopted.nativeHeight <= 1.5, 'the enhanced editor left a second visible textarea');
    assert.equal(adopted.nativeTabIndex, -1);
    assert.equal(adopted.active, 'editor-outside', 'progressive handoff stole focus');

    await page.evaluate(() => {editorBoundary.source('seed'); editorBoundary.focus();});
    await page.waitForFunction(() => document.activeElement?.classList.contains('cm-content'));
    const content = page.locator('.cm-content');
    await page.evaluate(() => {editorBoundary.labels(); editorBoundary.removeAttribute(3, 'aria-label');});
    assert.equal(await content.getAttribute('aria-label'), 'Original label');
    await page.evaluate(() => editorBoundary.text(6, 'Updated label'));
    assert.equal(await content.getAttribute('aria-label'), 'Updated label', 'external label text was not synchronized');
    await page.evaluate(() => editorBoundary.attribute(5, 'for', 'another-control'));
    assert.equal(await content.getAttribute('aria-label'), 'Source code', 'removed native label association was not synchronized');
    await page.evaluate(() => editorBoundary.attribute(7, 'for', 'editor-source'));
    assert.equal(await content.getAttribute('aria-label'), 'Other label', 'new native label association was not synchronized');
    await page.evaluate(() => {editorBoundary.attribute(3, 'aria-label', 'Explicit label'); editorBoundary.text(8, 'External edit');});
    assert.equal(await content.getAttribute('aria-label'), 'Explicit label');
    await page.keyboard.press('End');
    await page.keyboard.type(' + edit');
    await page.waitForFunction(() => editorBoundary.input.value === 'seed + edit');
    await page.evaluate(() => editorBoundary.source('old frame', editorBoundary.acknowledged));
    assert.equal(await content.textContent(), 'seed + edit', 'a stale native frame overwrote editor typing');
    await page.evaluate(() => editorBoundary.acknowledge());
    await page.keyboard.press('ControlOrMeta+z');
    await page.waitForFunction(() => editorBoundary.input.value === 'seed');
    assert.equal(await page.evaluate(() => new FormData(editorBoundary.container.querySelector('form')).get('source')), 'seed');

    await page.evaluate(() => {editorBoundary.view().dispatch({selection:{anchor:1, head:3}}); editorBoundary.move();});
    assert.equal(await page.evaluate(() => editorBoundary.view().state.selection.main.from), 1);
    assert.equal(await page.evaluate(() => getSelection().toString()), 'ee', 'moving the managed range lost the native editor selection');
    await page.evaluate(() => editorBoundary.attribute(2, 'disabled', 'true'));
    assert.equal(await content.getAttribute('contenteditable'), 'false');
    assert.equal(await page.evaluate(() => new FormData(editorBoundary.container.querySelector('form')).has('source')), false);
    await page.evaluate(() => {editorBoundary.attribute(2, 'disabled', 'false'); editorBoundary.attribute(3, 'readonly', 'true'); editorBoundary.focus();});
    await page.keyboard.type('blocked');
    assert.equal(await page.locator('#editor-source').inputValue(), 'seed');
    await page.evaluate(() => {editorBoundary.attribute(3, 'readonly', 'false'); editorBoundary.attribute(3, 'maxlength', '5'); editorBoundary.focus();});
    await page.keyboard.press('End');
    await page.keyboard.insertText('too long');
    assert.equal(await page.locator('#editor-source').inputValue(), 'seed', 'the editor accepted an oversized insertion');
    await page.evaluate(() => {editorBoundary.attribute(3, 'maxlength', '2'); editorBoundary.focus();});
    await page.keyboard.press('Backspace');
    await page.waitForFunction(() => editorBoundary.input.value === 'see');
    await page.evaluate(() => {editorBoundary.acknowledge(); editorBoundary.attribute(3, 'maxlength', '100000');});

    // Native control bindings also accept programmatic input/reset-style edits.
    await page.evaluate(() => {
      editorBoundary.input.value = 'from the native control';
      editorBoundary.input.dispatchEvent(new InputEvent('input', {bubbles:true}));
      editorBoundary.acknowledge();
    });
    assert.equal(await content.textContent(), 'from the native control');
    await page.evaluate(() => {
      editorBoundary.source('中文🙂\nmissing()');
      document.getElementById('editor-outside').focus();
      editorBoundary.select('中文🙂\nmissing()',5,12,'backward');
    });
    assert.equal(await page.evaluate(() => document.activeElement === editorBoundary.view().contentDOM), true);
    assert.deepEqual(await page.evaluate(() => {
      const {input,view}=editorBoundary, selected=view().state.selection.main;
      return [input.selectionStart,input.selectionEnd,input.selectionDirection,selected.anchor,selected.head];
    }), [5,12,'backward',12,5], 'native source location was not projected into the enhanced editor');
    await page.evaluate(() => {
      document.getElementById('editor-outside').focus();
      editorBoundary.select('old source',0,3);
    });
    assert.equal(await page.evaluate(() => document.activeElement.id), 'editor-outside', 'old diagnostic stole editor focus');
    const composition = await page.evaluate(() => {
      const {view, source, input, events} = editorBoundary;
      const editor = view();
      editor.contentDOM.dispatchEvent(new CompositionEvent('compositionstart', {bubbles:true}));
      editor.dispatch({changes:{from:0, to:editor.state.doc.length, insert:'IME 中文'}});
      const event = new KeyboardEvent('keydown', {key:'Enter', ctrlKey:true, bubbles:true, cancelable:true});
      editor.contentDOM.dispatchEvent(event);
      const commands = events.filter(event => event.kind === 'keydown').length;
      source('pending application update');
      const native = input.value;
      editor.contentDOM.dispatchEvent(new CompositionEvent('compositionend', {bubbles:true}));
      return {commands, native};
    });
    assert.deepEqual(composition, {commands:0, native:'IME 中文'}, 'a captured command or render interrupted composition');
    await page.waitForFunction(() => editorBoundary.events.some(event => event.kind === 'input' && !event.isComposing && event.value === 'IME 中文'));
    await page.evaluate(() => editorBoundary.acknowledge());
    assert.equal(await content.textContent(), 'IME 中文');
    await page.evaluate(() => {
      const input = editorBoundary.input;
      input.defaultValue = 'native reset value';
      input.form.addEventListener('reset', event => event.preventDefault(), {once:true});
      input.form.reset();
    });
    assert.equal(await content.textContent(), 'IME 中文', 'cancelled native reset changed the editor');
    await page.evaluate(() => editorBoundary.input.form.reset());
    await page.waitForFunction(() => editorBoundary.view().state.doc.toString() === 'native reset value');
    await page.evaluate(() => editorBoundary.acknowledge());
    await page.evaluate(async () => {
      const {captureReloadInputs} = await import('/host/ui_next/reload-inputs.js');
      const {nativeFocusTarget} = await import('/host/ui_next/focus.js');
      const {input, container} = editorBoundary;
      editorBoundary.focus();
      editorBoundary.view().dispatch({selection:{anchor:8, head:2}});
      const restore = captureReloadInputs(container);
      const replacement = document.body.appendChild(document.createElement('div'));
      const next = replacement.appendChild(input.cloneNode());
      next.value = input.value; next.removeAttribute('aria-hidden'); next.removeAttribute('tabindex');
      restore(replacement);
      if (document.activeElement !== next || next.selectionStart !== 2 || next.selectionEnd !== 8 || next.selectionDirection !== 'backward') {
        throw new Error('enhanced editor reload lost its native focus/selection identity');
      }
      replacement.remove();
      const oldSurface = editorBoundary.view().contentDOM;
      editorBoundary.focus(); editorBoundary.removeEditor();
      if (nativeFocusTarget(oldSurface) !== oldSurface) throw new Error('disposed editor retained its focus projection');
    });
    assert.equal(await page.locator('.cm-editor').count(), 0);
    assert.equal(await page.locator('#editor-source').getAttribute('aria-hidden'), null);
    assert.equal(await page.locator('#editor-source').getAttribute('tabindex'), null);
    assert.equal(await page.evaluate(() => document.activeElement === editorBoundary.input), true);
    assert.equal(await page.locator('#editor-source').inputValue(), 'native reset value');
    assert.equal(await page.evaluate(() => editorBoundary.events.filter(event => event.error).length), 0);
    return {passed:true, contracts:['active-native-edit-preserved', 'progressive-selection-adoption',
      'single-visible-editor', 'native-ref-focus', 'input-projection', 'stale-frame-protection',
      'ack-preserves-undo', 'native-form-value', 'range-move-selection', 'fieldset-disabled', 'native-label-dependencies',
      'readonly', 'maximum-length', 'overlimit-deletion', 'native-input-synchronization',
      'source-selection-projection', 'stale-source-selection', 'composition-command-bypass', 'composition-projection', 'native-reset', 'cancelled-reset',
      'reload-focus-selection', 'focus-projection-disposal', 'fallback-disposal']};
  } finally {await page.evaluate(() => editorBoundary.renderer.close());}
}
