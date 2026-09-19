import {runWorkerVm} from '/host/ui_next/worker-vm.js';
import {loadStudioRuntime} from './startup.js';

self.onmessage = async event => {
  self.onmessage = null;
  try {
    const artifact = new URL(event.data.artifact);
    if (artifact.origin !== self.location.origin) throw new Error('Invalid Studio artifact origin.');
    const {runtime, bytes} = await loadStudioRuntime(artifact, {
      compressed: typeof STUDIO_COMPRESSED !== 'undefined' && STUDIO_COMPRESSED,
    });
    const vm = new runtime.VoVmIsland(bytes);
    try {await runWorkerVm(vm, self);} finally {vm.free();}
  } catch (error) {
    self.postMessage({kind:'ui-exit',error:String(error?.message ?? error).slice(0,65536)});
  }
};
