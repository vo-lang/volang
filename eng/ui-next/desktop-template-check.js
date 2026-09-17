// Real system WebView DOM/media contracts. No physical input or paint claim.
const template = document.currentScript.dataset.template;
(async () => {
  let host;
  const wait = async (predicate, stage = 'application readiness') => {
    const deadline = performance.now() + 20_000;
    while (!predicate()) {
      if (performance.now() >= deadline) throw new Error(`Desktop ${template} timed out at ${stage}: ${JSON.stringify({
        readyState: document.readyState,
        canvases: document.querySelectorAll('canvas').length,
        media: [...document.querySelectorAll('audio,video')].map(media => ({
          readyState: media.readyState, networkState: media.networkState,
          currentTime: media.currentTime, duration: media.duration, seeking: media.seeking,
          error: media.error?.message,
          seekable: Array.from({length: media.seekable.length}, (_, i) => [media.seekable.start(i), media.seekable.end(i)]),
        })),
        text: document.querySelector('#root')?.textContent.slice(0, 2000),
      })}`);
      await new Promise(resolve => setTimeout(resolve, 25));
    }
  };
  const button = name => [...document.querySelectorAll('button')].find(value => value.textContent.trim() === name);
  const require = (condition, message) => { if (!condition) throw new Error(message); };
  try {
    await wait(() => host = window.__volangDesktop);
    require(await host.ready, 'Application closed before becoming ready');
    require(Intl.getCanonicalLocales(navigator.language).length === 1, 'Invalid host language');
    require(document.querySelector('#root').textContent.trim(), 'Empty application');
    if (template === 'canvas') {
      await wait(() => document.querySelector('canvas'), 'canvas mounting');
      const canvas = document.querySelector('canvas');
      const pixels = canvas.toDataURL();
      require(canvas.width === 96 && canvas.height === 64, 'Incorrect canvas backing size');
      button('Change palette').click();
      await wait(() => canvas.toDataURL() !== pixels, 'canvas palette update');
      require(canvas === document.querySelector('canvas'), 'Palette update replaced canvas');
      button('Show or hide').click();
      await wait(() => !document.querySelector('canvas'), 'canvas disposal');
      require(canvas.width === 0 && canvas.height === 0, 'Canvas backing was retained');
    } else if (template === 'plot') {
      require(!document.querySelector('canvas'), 'Chart mounted before it was requested');
      button('Show chart').click();
      await wait(() => document.querySelector('canvas'), 'chart loading');
      const canvas = document.querySelector('canvas');
      button('Change week').click();
      await wait(() => [...document.querySelectorAll('td')].some(cell => cell.textContent === '20 cm'), 'chart data update');
      require(canvas === document.querySelector('canvas'), 'Data update replaced chart');
      button('Hide chart').click();
      await wait(() => !document.querySelector('canvas'), 'chart disposal');
      require(!document.querySelector('style[data-ui-plot]'), 'Removed chart retained stylesheet');
    } else if (template === 'listening') {
      const audio = document.querySelector('audio');
      require(audio, 'Missing native audio');
      audio.preload = 'auto'; audio.load();
      await wait(() => audio.readyState >= 1 || audio.error, 'media metadata');
      require(!audio.error && audio.duration > 6, 'Native media metadata failed: ' + JSON.stringify({
        code: audio.error?.code, message: audio.error?.message,
        readyState: audio.readyState, duration: audio.duration,
        source: audio.currentSrc.slice(0, 120), wav: audio.canPlayType('audio/wav'),
      }));
      // Metadata can arrive before the seekable ranges. Seeking outside them
      // may clamp the requested position, even after the resource gains data.
      await wait(() => Array.from({length: audio.seekable.length}, (_, i) =>
        audio.seekable.start(i) <= 5 && audio.seekable.end(i) >= 5).some(Boolean), 'media seekable range');
      for (const position of [5, 1, 6]) {
        audio.currentTime = position;
        await wait(() => !audio.seeking && Math.abs(audio.currentTime - position) < .05, 'media seeking to ' + position);
      }
      button('Put the player away').click();
      await wait(() => !document.querySelector('audio'), 'media disposal');
      require(audio.paused, 'Removed media retained playback');
      button('Bring the player back').click();
      await wait(() => document.querySelector('audio'), 'media remounting');
      require(audio !== document.querySelector('audio'), 'Media remount retained old instance');
    } else if (template === 'variable-list') {
      await wait(() => document.querySelectorAll('[data-row]').length > 0, 'list mounting');
      const list = document.querySelector('#variable-list');
      button('Jump ahead').click();
      await wait(() => list.scrollTop > 4000, 'list scrolling');
      require(document.querySelectorAll('[data-row]').length < 40, 'List mounted its whole data set');
      button('Five notes').click();
      await wait(() => document.querySelector('[data-range]').textContent.includes('of 5'), 'list shrinking');
      require(document.querySelectorAll('[data-row]').length <= 5, 'Shrunk list retained excess rows');
    }
    host.close();
  } catch (error) {
    if (host) host.fail(String(error));
    else throw error;
  }
})();
