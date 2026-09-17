/** Native close stays immediate; finite CSS motion only retains positioning. */
export async function checkPopoverMotion(page, url) {
  await page.route('**/popover-motion', route => route.fulfill({contentType:'text/html', body:'<!doctype html><title>Popover motion</title>'}));
  await page.goto(url + '/popover-motion');
  return page.evaluate(async () => {
    const {DomRenderer} = await import('/host/ui_next/renderer.js');
    const {WIRE_VERSION} = await import('/host/ui_next/generated/protocol.js');
    const {tabOrder} = await import('/host/ui_next/focus.js');
    const require = (value, message) => {if (!value) throw new Error(message);};
    const style = document.createElement('style');
    style.textContent = `@supports (overlay:auto) {
      [popover] {opacity:0;transition:opacity 180ms linear,display 180ms allow-discrete,overlay 180ms allow-discrete;}
      [popover]:popover-open {opacity:1;}
      @starting-style {[popover]:popover-open {opacity:0;}}
    }`;
    document.head.append(style);
    const container = document.createElement('div'); document.body.append(container);
    const renderer = new DomRenderer(container, () => {});
    const mutation = (op, id, fields = {}) => ({op, id, parent:0, before:0, name:'', value:'', ...fields});
    let revision = 0;
    const apply = mutations => renderer.applyBatch({version:WIRE_VERSION, revision:++revision, inputSequence:0, mutations, commands:[]});
    const settle = element => Promise.all(element.getAnimations().map(animation => animation.finished.catch(() => {})));
    const frame = () => new Promise(resolve => requestAnimationFrame(resolve));
    try {
      apply([mutation('create', 1, {name:'#fragment'}), mutation('insert', 1),
        mutation('create', 2, {name:'button'}), mutation('insert', 2, {parent:1}),
        mutation('attr', 2, {name:'id', value:'motion-trigger'}), mutation('attr', 2, {name:'style', value:'margin:100px 0 0 250px'}),
        mutation('create', 3, {name:'div'}), mutation('insert', 3, {parent:1}),
        mutation('attr', 3, {name:'id', value:'motion-popover'}), mutation('attr', 3, {name:'popover', value:'auto'}),
        mutation('attr', 3, {name:'style', value:'width:200px;padding:20px;left:17px'}),
        mutation('create', 4, {name:'input'}), mutation('insert', 4, {parent:3}),
        mutation('attr', 4, {name:'autofocus', value:'true'}), mutation('attr', 4, {name:'value', value:'Keep this edit'}),
        mutation('popover', 3, {parent:2, name:'bottom-start', value:'true'})]);
      const popup = container.querySelector('#motion-popover'), trigger = container.querySelector('button'), input = popup.querySelector('input');
      await settle(popup);
      const positioned = popup.style.left;
      require(positioned !== '17px', 'the popup was not anchored');
      apply([mutation('popover', 3, {parent:2, name:'bottom-start', value:'false'})]);
      require(!popup.matches(':popover-open'), 'closing motion reopened native presentation');
      require(document.activeElement === trigger, 'range focus restoration focused a closed popup');
      require(!tabOrder(container).includes(input), 'closed popup remained in the managed tab order');
      const animated = popup.getAnimations().length > 0;
      if (animated) require(popup.style.left === positioned, 'closing animation lost its anchored position');
      else require(popup.style.left === '17px', 'a nonanimated close retained positioning');
      await settle(popup); await frame();
      require(popup.style.left === '17px', 'completed exit did not release positioning');

      // Reopening retires the old wait without restoring styles over the new open.
      apply([mutation('popover', 3, {parent:2, name:'bottom-start', value:'true'})]); await settle(popup);
      apply([mutation('popover', 3, {parent:2, name:'bottom-start', value:'false'})]);
      apply([mutation('popover', 3, {parent:2, name:'bottom-end', value:'true'})]);
      const reopened = popup.style.left;
      await settle(popup); await frame();
      require(popup.matches(':popover-open') && popup.style.left === reopened && reopened !== '17px', 'old exit cleanup changed a reopened popup');
      require(input.value === 'Keep this edit' && document.activeElement === input, 'reopening lost content or native autofocus');
      const nativeClosed = new Promise(resolve => popup.addEventListener('toggle', event => {if (event.newState === 'closed') resolve();}, {once:true}));
      popup.hidePopover(); await nativeClosed;
      if (popup.getAnimations().length) require(popup.style.left === reopened, 'native dismissal released positioning during exit');
      await settle(popup); await frame();
      require(popup.style.left === '17px', 'native dismissal did not release positioning');

      // A paused finite transition cannot keep a detached cleanup owner forever.
      if (animated) {
        apply([mutation('popover', 3, {parent:2, name:'bottom-start', value:'true'})]); await settle(popup);
        apply([mutation('popover', 3, {parent:2, name:'bottom-start', value:'false'})]);
        const paused = popup.getAnimations();
        for (const animation of paused) animation.pause();
        const deadline = performance.now() + 6500;
        while (popup.style.left !== '17px' && performance.now() < deadline) await new Promise(resolve => setTimeout(resolve, 50));
        require(popup.style.left === '17px', 'paused exit exceeded its cleanup deadline');
        for (const animation of paused) animation.cancel();
      }

      apply([mutation('popover', 3, {parent:2, name:'bottom-start', value:'true'})]); await settle(popup);
      apply([mutation('popover', 3, {parent:2, name:'bottom-start', value:'false'})]);
      const orphaned = popup.getAnimations();
      for (const animation of orphaned) animation.pause();
      apply([mutation('remove', 2), mutation('popover', 3, {name:'bottom-start', value:'false'})]);
      require(!popup.matches(':popover-open') && popup.style.left === '17px', 'anchor disposal retained a paused positioning owner');
      for (const animation of orphaned) animation.cancel();
      apply([mutation('create', 5, {name:'button'}), mutation('insert', 5, {parent:1}),
        mutation('popover', 3, {parent:5, name:'bottom-start', value:'true'})]); await settle(popup);
      apply([mutation('popover', 3, {parent:5, name:'bottom-start', value:'false'})]);
      for (const animation of popup.getAnimations()) animation.pause();
      renderer.close();
      require(!container.childNodes.length && popup.style.left === '17px', 'root disposal retained a paused exit owner');
      for (const animation of popup.getAnimations()) animation.cancel();
      return {passed:true, nativeExitAnimation:animated, overlaySupported:CSS.supports('overlay','auto'),
        contracts:['anchored-exit', 'immediate-logical-close', 'native-focus-return', 'closed-tab-order',
          'completed-style-release', 'reopen-cancels-old-cleanup', 'retained-input', 'native-dismissal', 'paused-exit-deadline', 'paused-exit-anchor-disposal', 'paused-exit-disposal', 'nonanimated-fallback']};
    } finally {renderer.close(); container.remove(); style.remove();}
  });
}
