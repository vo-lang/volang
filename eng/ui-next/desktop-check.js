// System WebView conformance. Timers also progress with the macOS display
// locked; this establishes DOM/state behavior, not paint or physical input.
(async () => {
  const host = window.__volangDesktop;
  const wait = async predicate => {
    const deadline = performance.now() + 15_000;
    while (!predicate()) {
      if (performance.now() >= deadline) throw new Error('Desktop interaction deadline exceeded');
      await new Promise(resolve => setTimeout(resolve, 16));
    }
  };
  const button = selector => document.querySelector(selector);
  try {
    if (!await host.ready) throw new Error('Desktop closed before becoming interactive');
    if (document.getElementById('volang-desktop-config')) throw new Error('Desktop bootstrap was retained');
    await window.__volangShellCheck?.();
    for (let index = 0; index < 12; index++) button('[data-counter="Alpha"]').click();
    await wait(() => button('[data-counter="Alpha"]').textContent === 'Alpha: 12');
    const input = document.querySelector('#name');
    input.focus(); input.value = '桌面 🌿 Volang';
    input.dispatchEvent(new InputEvent('input', {bubbles: true})); input.form.requestSubmit();
    await wait(() => document.querySelector('[data-submitted]').textContent === input.value);
    const alpha = button('[data-counter="Alpha"]');
    button('[data-reverse]').click();
    await wait(() => button('[data-counters] button').dataset.counter === 'Beta');
    if (button('[data-counter="Alpha"]') !== alpha || alpha.textContent !== 'Alpha: 12') throw new Error('Desktop move lost identity');
    button('[data-break]').click(); await wait(() => !!document.querySelector('[data-recover]'));
    button('[data-recover]').click(); await wait(() => !!document.querySelector('[data-break]'));
    host.close();
  } catch (error) { host.fail(String(error)); }
})();
