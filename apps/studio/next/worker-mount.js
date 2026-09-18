import {createWorkerUi} from '/host/ui_next/worker-host.js';

// Keep DOM and browser services on the page; the Studio guest owns a dedicated
// worker so compilation-independent UI work cannot monopolize input/painting.
export function mountUi(container, options) {
  const worker = new Worker(new URL('/studio-assets/studio-worker.js', location.href), {type:'module', name:'volang-studio'});
  let application;
  try {
    const services = typeof options.services === 'function' ? options.services(container) : options.services;
    application = createWorkerUi(container, worker, {
      hydrate: options.hydrate ?? container.hasChildNodes(), services,
      startupTimeoutMilliseconds: 60000,
    });
    worker.postMessage({artifact:new URL(options.artifact, location.href).href});
    return application;
  } catch (error) {
    if (application) application.close(); else worker.terminate();
    throw error;
  }
}
