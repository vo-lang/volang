// Record the user-visible interval in the browser. Trace serialization and
// transport back to the test driver must not extend an already completed open.
export async function beginEditorOpenTiming(page, buttonName, sourceText) {
  return page.evaluateHandle(({ buttonName, sourceText }) => {
    const state = { started: null, ready: null, frame: 0, dispose: () => {} };
    const clicked = event => {
      if (state.started === null && event.isTrusted
          && event.target instanceof Element
          && event.target.closest('button')?.getAttribute('aria-label') === buttonName) {
        state.started = performance.now();
      }
    };
    state.dispose = () => {
      window.removeEventListener('click', clicked, true);
      cancelAnimationFrame(state.frame);
    };
    const sample = () => {
      const source = document.querySelector('[data-testid="volang-code-editor"]')?.value ?? '';
      if (state.started !== null && source.includes(sourceText)
          && !document.querySelector('[role="treeitem"][aria-label="Open vo.lock"]')
          && !document.querySelector('[role="treeitem"][aria-label="Open vo.work"]')
          && (document.getElementById('volang-diagnostic')?.textContent ?? '') === '') {
        state.ready = performance.now();
        state.dispose();
        return;
      }
      state.frame = requestAnimationFrame(sample);
    };
    window.addEventListener('click', clicked, true);
    state.frame = requestAnimationFrame(sample);
    return state;
  }, { buttonName, sourceText });
}

export async function editorOpenDuration(timing) {
  const { started, ready } = await timing.evaluate(state => ({ started: state.started, ready: state.ready }));
  if (!Number.isFinite(started) || !Number.isFinite(ready) || ready < started) {
    throw new Error('Editor timing lacks a trusted click and a ready frame');
  }
  return ready - started;
}

export async function disposeEditorOpenTiming(timing) {
  await timing.evaluate(state => state.dispose());
  await timing.dispose();
}
