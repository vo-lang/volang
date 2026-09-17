export async function checkModalBoundary(page, url) {
  await page.route('**/modal-contracts', route => route.fulfill({ contentType: 'text/html', body: '<!doctype html><title>Modal contracts</title>' }));
  await page.goto(`${url}/modal-contracts`);
  return page.evaluate(async () => {
    const { DomRenderer } = await import('/host/ui_next/renderer.js');
    const { WIRE_VERSION } = await import('/host/ui_next/generated/protocol.js');
    const { tabOrder } = await import('/host/ui_next/focus.js');
    const require = (ok, message) => { if (!ok) throw new Error(message); };
    const mutation = (op, id, fields = {}) => ({ op, id, parent: 0, before: 0, name: '', value: '', ...fields });
    const batch = (revision, mutations) => ({ version: WIRE_VERSION, revision, inputSequence: 0, mutations, commands: [] });
    const containers = [document.createElement('div'), document.createElement('div')];
    document.body.append(...containers);
    document.documentElement.style.setProperty('overflow', 'scroll', 'important');
    const roots = containers.map(element => new DomRenderer(element, () => {}));
    const initial = batch(1, [mutation('create', 1, { name: '#fragment' }), mutation('insert', 1),
      mutation('create', 2, { name: 'dialog' }), mutation('insert', 2, { parent: 1 }),
      mutation('create', 3, { name: 'input' }), mutation('insert', 3, { parent: 2 }),
      mutation('attr', 3, { name: 'value', value: 'retained text' }), mutation('modal', 2, { value: 'true' })]);
    roots[0].applyBatch(initial); roots[1].applyBatch(initial);
    roots[0].close();
    require(document.documentElement.style.overflow === 'hidden', 'closing one root released another root’s scroll lock');
    const dialog = containers[1].querySelector('dialog'), input = dialog.querySelector('input');
    input.focus(); input.setSelectionRange(2, 5);
    roots[1].applyBatch(batch(2, [mutation('insert', 1)]));
    require(dialog.matches(':modal') && document.activeElement === input && input.selectionStart === 2 && input.selectionEnd === 5,
      `range move lost modal presentation or selection: ${JSON.stringify({ modal: dialog.matches(':modal'), active: document.activeElement?.outerHTML, start: input.selectionStart, end: input.selectionEnd })}`);
    dialog.close();
    roots[1].applyBatch(batch(3, []));
    require(!dialog.open, 'unrelated commit reopened a native-closed dialog before its close event');
    require(document.documentElement.style.overflow === 'scroll', 'native close kept a scroll lock after the dialog stopped being modal');
    roots[1].applyBatch(batch(4, [mutation('modal', 2, { value: 'false' })]));
    require(document.documentElement.style.overflow === 'scroll' && document.documentElement.style.getPropertyPriority('overflow') === 'important', 'last dialog did not restore the existing scroll policy');
    let rejected = false;
    try { roots[1].applyBatch(batch(5, [mutation('attr', 3, { name: 'title', value: 'partial' }), mutation('modal', 3, { value: 'true' })])); }
    catch { rejected = true; }
    require(rejected && !input.hasAttribute('title'), 'invalid modal command partially committed');
    roots[1].applyBatch(batch(5, [mutation('modal', 2, { value: 'true' })]));
    await new Promise(resolve => { dialog.addEventListener('close', resolve, { once: true }); dialog.close(); });
    require(document.documentElement.style.overflow === 'scroll', 'native close without a guest handler retained its scroll lock');
    roots[1].applyBatch(batch(6, [mutation('create', 4, { name: 'dialog' }), mutation('insert', 4, { parent: 2 }),
      mutation('modal', 2, { value: 'true' }), mutation('modal', 4, { value: 'true' })]));
    require(document.querySelectorAll('dialog:modal').length === 2, 'nested modal did not open');
    roots[1].applyBatch(batch(7, [mutation('modal', 2, { value: 'false' })]));
    require(!document.querySelector('dialog:modal') && document.documentElement.style.overflow === 'scroll', 'closing a parent kept its nested modal open');
    roots[1].applyBatch(batch(8, [mutation('modal', 2, { value: 'true' }), mutation('modal', 4, { value: 'true' })]));
    roots[1].applyBatch(batch(9, [mutation('remove', 1)]));
    require(!document.querySelector('dialog:modal') && document.documentElement.style.overflow === 'scroll', 'removed ancestor retained its modal layer or lock');
    roots[1].close();

    const focusRoot = new DomRenderer(containers[0], () => {});
    focusRoot.applyBatch(batch(1, [mutation('create', 1, { name: '#fragment' }), mutation('insert', 1),
      mutation('create', 2, { name: 'input' }), mutation('insert', 2, { parent: 1 }),
      mutation('attr', 2, { name: 'id', value: 'outside-modal' }),
      mutation('create', 3, { name: 'dialog' }), mutation('insert', 3, { parent: 1 }),
      mutation('create', 4, { name: 'input' }), mutation('insert', 4, { parent: 3 }),
      mutation('attr', 4, { name: 'id', value: 'inside-modal' }),
      mutation('attr', 4, { name: 'autofocus', value: 'true' }),
      mutation('create', 5, { name: 'dialog' }), mutation('insert', 5, { parent: 3 }),
      mutation('create', 6, { name: 'button' }), mutation('insert', 6, { parent: 5 }),
      mutation('attr', 6, { name: 'autofocus', value: 'true' }),
      mutation('attr', 6, { name: 'id', value: 'cancel-decision' }),
      mutation('modal', 3, { value: 'false' }), mutation('modal', 5, { value: 'false' })]));
    containers[0].querySelector('#outside-modal').focus();
    focusRoot.applyBatch(batch(2, [mutation('modal', 3, { value: 'true' })]));
    require(document.activeElement.id === 'inside-modal', 'retained page focus overrode modal autofocus');
    const retainedInput = document.activeElement, getRects = retainedInput.getClientRects;
    let layoutReads = 0;
    retainedInput.getClientRects = function() { layoutReads++; return getRects.call(this); };
    focusRoot.applyBatch(batch(3, [mutation('attr', 4, { name: 'title', value: 'updated' })]));
    require(layoutReads === 0, 'unchanged focus caused an unnecessary layout read on a normal update');
    delete retainedInput.getClientRects;
    focusRoot.applyBatch(batch(4, [mutation('modal', 5, { value: 'true' })]));
    require(document.activeElement.id === 'cancel-decision', 'retained parent focus overrode nested modal autofocus');
    focusRoot.applyBatch(batch(5, [mutation('modal', 5, { value: 'false' })]));
    require(document.activeElement.id === 'inside-modal', 'focus restoration targeted a hidden closed dialog');
    focusRoot.applyBatch(batch(6, [mutation('modal', 3, { value: 'false' })]));
    require(document.activeElement.id === 'outside-modal', 'programmatic modal closure lost its native return focus');
    focusRoot.close();
    const echoes=[], echoRoot=new DomRenderer(containers[0],event=>echoes.push(event));
    echoRoot.applyBatch(batch(1,[mutation('create',1,{name:'button'}),mutation('insert',1),
      mutation('listen',1,{name:'keydown',value:'0'}),mutation('create',2,{name:'dialog'}),mutation('insert',2),
      mutation('listen',2,{name:'close',value:'0'}),mutation('modal',2,{value:'true'})]));
    const controlledDialog=containers[0].querySelector('dialog');
    const closed=new Promise(resolve=>controlledDialog.addEventListener('close',resolve,{once:true}));
    echoRoot.applyBatch(batch(2,[mutation('modal',2,{value:'false'})]));
    containers[0].querySelector('button').dispatchEvent(new KeyboardEvent('keydown',{key:'Enter',bubbles:true}));
    await closed;
    require(echoes.length===1 && echoes[0].key==='Enter','controlled close echoed after a newer keyboard action');
    echoRoot.applyBatch(batch(3,[mutation('modal',2,{value:'true'})]));
    const dismissed=new Promise(resolve=>controlledDialog.addEventListener('close',resolve,{once:true}));
    controlledDialog.close();await dismissed;
    require(echoes.length===2 && echoes[1].kind==='close','native close was suppressed with controlled echoes');
    echoRoot.close();
    document.documentElement.style.removeProperty('overflow');
    const scope = document.createElement('section');
    scope.innerHTML = '<button id="second" tabindex="2">Second</button><button disabled>Disabled</button>' +
      '<button id="first" tabindex="1">First</button><fieldset disabled><legend><button id="legend">Legend</button></legend><input></fieldset>' +
      '<input id="radio-one" type="radio" name="choice"><input id="radio-two" type="radio" name="choice" checked>' +
      '<div id="editable" contenteditable>Text</div><button hidden>Hidden</button><div inert><button>Inert</button></div>' +
      '<button style="visibility:hidden">Invisible</button><button id="last">Last</button>';
    document.body.append(scope);
    require(tabOrder(scope).map(element => element.id).join(',') === 'first,second,legend,radio-two,editable,last', 'tab order lost native disabled/radio/visibility semantics');
    scope.remove();
    return { passed: true, contracts: ['shared-scroll-lock', 'prior-style-restoration', 'range-move-focus', 'native-close-order', 'controlled-close-echo', 'native-close-without-handler', 'nested-parent-close', 'atomic-rejection', 'ancestor-disposal', 'native-tab-order', 'modal-autofocus-over-retention', 'nested-autofocus-over-retention', 'programmatic-close-focus', 'unchanged-focus-no-layout-read'] };
  });
}
