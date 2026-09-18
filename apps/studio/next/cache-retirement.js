// A direct visit to a new page can bypass the former cached shell. Request its
// retirement update without delaying startup or registering a new worker.
export async function retireAssetCache(window) {
  if (!window.navigator.serviceWorker) return;
  const registration = await window.navigator.serviceWorker.getRegistration('/');
  const worker = registration?.active ?? registration?.waiting ?? registration?.installing;
  if (registration?.scope === window.location.origin + '/' && worker?.scriptURL === window.location.origin + '/service-worker.js') {
    await registration.update();
    const replacement = registration.installing ?? registration.waiting;
    if (!replacement || replacement.scriptURL !== worker.scriptURL) return;
    // Installation can overlap an automatic browser update. Request activation
    // again once installed; open tabs keep their current document and input.
    const installed = () => {
      if (replacement.state === 'installed') replacement.postMessage({type:'volang.studio.retire'});
      if (['installed', 'activated', 'redundant'].includes(replacement.state)) replacement.removeEventListener('statechange', installed);
    };
    replacement.addEventListener('statechange', installed);
    installed();
  }
}
