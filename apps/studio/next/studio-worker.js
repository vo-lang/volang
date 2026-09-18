import {runWorkerVm} from '/host/ui_next/worker-vm.js';
import {initializeUiVm} from '/host/ui_next/vm.js';

self.onmessage = async event => {
  self.onmessage = null;
  try {
    const artifact = new URL(event.data.artifact);
    if (artifact.origin !== self.location.origin) throw new Error('Invalid Studio artifact origin.');
    const [runtime, bytes] = await Promise.all([
      import('/wasm/vo_web.js').then(async runtime => {await initializeUiVm(runtime); return runtime;}),
      fetch(artifact).then(async response => {
        if (!response.ok) throw new Error(`Could not load Studio (${response.status}).`);
        return new Uint8Array(await response.arrayBuffer());
      }),
    ]);
    const vm = new runtime.VoVmIsland(bytes);
    try {await runWorkerVm(vm, self);} finally {vm.free();}
  } catch (error) {
    self.postMessage({kind:'ui-exit',error:String(error?.message ?? error).slice(0,65536)});
  }
};
