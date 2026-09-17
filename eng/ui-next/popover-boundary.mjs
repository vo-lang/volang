export async function checkPopoverBoundary(page, url) {
  await page.route('**/popover-contracts', route => route.fulfill({ contentType: 'text/html', body: '<!doctype html><title>Popover contracts</title>' }));
  await page.goto(`${url}/popover-contracts`);
  return page.evaluate(async () => {
    const { DomRenderer } = await import('/host/ui_next/renderer.js');
    const { WIRE_VERSION } = await import('/host/ui_next/generated/protocol.js');
    const require = (ok, message) => { if (!ok) throw new Error(message); };
    const mutation = (op, id, fields = {}) => ({ op, id, parent: 0, before: 0, name: '', value: '', ...fields });
    const batch = (revision, mutations) => ({ version: WIRE_VERSION, revision, inputSequence: 0, mutations, commands: [] });
    const container = document.createElement('div');
    document.body.append(container);
    const received = [];
    const root = new DomRenderer(container, event => received.push(event));
    root.applyBatch(batch(1, [mutation('create', 1, { name: '#fragment' }), mutation('insert', 1),
      mutation('create', 2, { name: 'button' }), mutation('insert', 2, { parent: 1 }),
      mutation('attr', 2, { name: 'id', value: 'anchor' }),
      mutation('create', 3, { name: 'div' }), mutation('insert', 3, { parent: 1 }),
      mutation('attr', 3, { name: 'id', value: 'popup' }), mutation('attr', 3, { name: 'popover', value: 'auto' }),
      mutation('attr', 3, { name: 'style', value: 'width:240px;padding:20px;left:17px;' }),
      mutation('listen', 3, { name: 'toggle', value: '0' }),
      mutation('create', 4, { name: 'input' }), mutation('insert', 4, { parent: 3 }),
      mutation('attr', 4, { name: 'autofocus', value: 'true' }),
      mutation('attr', 4, { name: 'value', value: 'retained text' }),
      mutation('popover', 3, { parent: 2, name: 'bottom-start', value: 'true' })]));
    const popup = container.querySelector('#popup'), anchor = container.querySelector('#anchor'), input = popup.querySelector('input');
    require(popup.matches(':popover-open') && document.activeElement === input, 'popover did not open with native autofocus');
    input.setSelectionRange(2, 5);
    root.applyBatch(batch(2, [mutation('insert', 1)]));
    require(popup.matches(':popover-open') && document.activeElement === input && input.selectionStart === 2 && input.selectionEnd === 5,
      'moving a retained range lost popover presentation, focus or selection');
    let rejected = false;
    try { root.applyBatch(batch(3, [mutation('attr', 2, { name: 'title', value: 'partial' }), mutation('popover', 3, { parent: 4, name: 'bottom-start', value: 'true' })])); }
    catch { rejected = true; }
    require(rejected && !anchor.hasAttribute('title') && popup.matches(':popover-open'), 'invalid popup anchor partially committed');
    await new Promise(resolve => { popup.addEventListener('toggle', resolve, { once: true }); popup.hidePopover(); });
    require(received.at(-1).value === 'closed', 'native toggle did not publish its live closed state');
    root.applyBatch(batch(3, []));
    require(!popup.matches(':popover-open'), 'unrelated commit reopened a native-dismissed popover');
    require(popup.style.left === '17px' && !popup.style.getPropertyValue('--ui-popover-width')
      && !popup.style.getPropertyValue('--ui-popover-anchor-width'), 'native dismissal retained owned positioning styles');
    root.applyBatch(batch(4, [mutation('popover', 3, { parent: 2, name: 'bottom-end', value: 'true' })]));
    root.applyBatch(batch(5, [mutation('remove', 2), mutation('popover', 3, { name: 'bottom-end', value: 'false' })]));
    require(!popup.matches(':popover-open'), 'removing the anchor retained its open layer');
    root.close();
    require(!document.querySelector(':popover-open') && !container.childNodes.length, 'closing the root leaked a native layer');

    // The layer remains part of the modal's DOM subtree and keyboard scope.
    const nested = new DomRenderer(container, () => {});
    nested.applyBatch(batch(1, [mutation('create', 1, { name: 'dialog' }), mutation('insert', 1),
      mutation('create', 2, { name: 'button' }), mutation('insert', 2, { parent: 1 }),
      mutation('create', 3, { name: 'div' }), mutation('insert', 3, { parent: 1 }),
      mutation('attr', 3, { name: 'popover', value: 'auto' }),
      mutation('create', 4, { name: 'input' }), mutation('insert', 4, { parent: 3 }),
      mutation('attr', 4, { name: 'autofocus', value: 'true' }),
      mutation('modal', 1, { value: 'true' }), mutation('popover', 3, { parent: 2, name: 'bottom-start', value: 'true' })]));
    require(container.querySelector('dialog').matches(':modal') && container.querySelector('div').matches(':popover-open')
      && document.activeElement === container.querySelector('input'), 'popover could not receive focus inside a modal');
    nested.applyBatch(batch(2, [mutation('modal', 1, { value: 'false' })]));
    require(!container.querySelector('div').matches(':popover-open'), 'closing the modal retained a popup whose anchor became hidden');
    nested.close();
    require(!document.querySelector(':popover-open') && !document.querySelector('dialog:modal'), 'nested modal/popover disposal leaked a layer');

    // Hints keep the current focus and coexist with an auto layer. Mode changes
    // and moving retained ranges must preserve their live presentation.
    const hints = new DomRenderer(container, () => {});
    hints.applyBatch(batch(1, [mutation('create', 1, { name: '#fragment' }), mutation('insert', 1),
      mutation('create', 2, { name: 'input' }), mutation('insert', 2, { parent: 1 }),
      mutation('create', 3, { name: 'button' }), mutation('insert', 3, { parent: 1 }),
      mutation('create', 4, { name: 'div' }), mutation('insert', 4, { parent: 1 }),
      mutation('attr', 4, { name: 'popover', value: 'hint' }),
      mutation('popover', 4, { parent: 3, name: 'bottom-start', value: 'false' })]));
    const retainedInput = container.querySelector('input'), hint = container.querySelector('div');
    retainedInput.focus();
    hints.applyBatch(batch(2, [mutation('popover', 4, { parent: 3, name: 'bottom-start', value: 'true' })]));
    require(hint.matches(':popover-open') && document.activeElement === retainedInput, 'hint opening moved focus');
    hints.applyBatch(batch(3, [mutation('insert', 1)]));
    require(hint.matches(':popover-open') && document.activeElement === retainedInput, 'hint range move lost presentation or focus');
    hints.applyBatch(batch(4, [mutation('attr', 4, { name: 'popover', value: 'auto' })]));
    require(hint.matches(':popover-open') && hint.popover === 'auto', 'live hint-to-auto change lost presentation');
    hints.applyBatch(batch(5, [mutation('attr', 4, { name: 'popover', value: 'hint' })]));
    require(hint.matches(':popover-open') && hint.popover === 'hint', 'live auto-to-hint change lost presentation');
    rejected = false;
    try { hints.applyBatch(batch(6, [mutation('attr', 3, { name: 'title', value: 'partial' }),
      mutation('attr', 4, { name: 'popover', value: 'manual' }),
      mutation('popover', 4, { parent: 3, name: 'bottom-start', value: 'true' })])); }
    catch { rejected = true; }
    require(rejected && hint.popover === 'hint' && !container.querySelector('button').hasAttribute('title'), 'invalid hint mode partially committed');
    hints.applyBatch(batch(6, [mutation('create', 5, { name: 'div' }), mutation('insert', 5, { parent: 1 }),
      mutation('attr', 5, { name: 'popover', value: 'hint' }),
      mutation('popover', 5, { parent: 3, name: 'top-start', value: 'true' })]));
    const secondHint = container.querySelectorAll('div')[1];
    require(!hint.matches(':popover-open') && secondHint.matches(':popover-open'), 'a new hint did not dismiss the previous hint');
    hints.applyBatch(batch(7, []));
    require(!hint.matches(':popover-open') && secondHint.matches(':popover-open'), 'an unrelated commit reopened a replaced hint');
    hints.close();
    require(!document.querySelector(':popover-open') && !container.childNodes.length, 'hint disposal leaked a layer');
    const echoes = [], controlled = new DomRenderer(container, event => echoes.push(event));
    controlled.applyBatch(batch(1, [mutation('create', 1, {name:'button'}), mutation('insert', 1),
      mutation('listen', 1, {name:'keydown',value:'0'}),
      mutation('create', 2, {name:'div'}), mutation('insert', 2),
      mutation('attr', 2, {name:'popover',value:'auto'}), mutation('listen', 2, {name:'toggle',value:'0'}),
      mutation('popover', 2, {parent:1,name:'bottom-start',value:'true'})]));
    const controlledPopup=container.querySelector('div'), controlledTrigger=container.querySelector('button');
    await new Promise(resolve=>controlledPopup.addEventListener('toggle',resolve,{once:true}));
    require(echoes.length===0,'controlled opening echoed into the guest event queue');
    const closing=new Promise(resolve=>controlledPopup.addEventListener('toggle',resolve,{once:true}));
    controlled.applyBatch(batch(2,[mutation('popover',2,{parent:1,name:'bottom-start',value:'false'})]));
    controlledTrigger.dispatchEvent(new KeyboardEvent('keydown',{key:'ArrowUp',bubbles:true}));
    await closing;
    require(echoes.length===1 && echoes[0].key==='ArrowUp','delayed controlled close overwrote a newer keyboard intent');
    controlled.applyBatch(batch(3,[mutation('popover',2,{parent:1,name:'bottom-start',value:'true'})]));
    await new Promise(resolve=>controlledPopup.addEventListener('toggle',resolve,{once:true}));
    const dismissed=new Promise(resolve=>controlledPopup.addEventListener('toggle',resolve,{once:true}));
    controlledPopup.hidePopover();await dismissed;
    require(echoes.length===2 && echoes[1].kind==='toggle' && echoes[1].value==='closed','native dismissal was lost with controlled echoes');
    controlled.close();
    container.remove();
    return { passed: true, contracts: ['native-autofocus', 'range-move-selection', 'atomic-anchor-validation', 'live-toggle-state',
      'native-dismissal', 'controlled-toggle-echo', 'position-style-release', 'anchor-disposal', 'modal-composition', 'root-disposal',
      'hint-retains-focus', 'hint-range-move', 'live-hint-mode-change', 'atomic-hint-mode', 'hint-replacement', 'hint-disposal'] };
  });
}
