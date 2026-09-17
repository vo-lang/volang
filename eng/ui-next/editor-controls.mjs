// Exercise the visible surface while inspecting the canonical native value.
// The active textarea can stay native until a natural blur after lazy loading.
export function sourceEditor(page, id = 'playground-source') {
  const input = page.locator('#' + id);
  const surface = page.locator(`#${id}:not([aria-hidden="true"]), #${id} + div .cm-content`);
  return {
    input, surface,
    // Stabilize lazy handoff before Playwright selects/replaces text. A focused
    // native control stays native; re-resolve if enhancement already occurred.
    fill: async value => {await surface.focus(); await surface.fill(value);},
    press: key => surface.press(key),
    inputValue: () => input.inputValue(),
    evaluate: (fn, argument) => input.evaluate(fn, argument),
    getAttribute: name => input.getAttribute(name),
    waitFor: () => input.waitFor({state:'attached'}),
  };
}
