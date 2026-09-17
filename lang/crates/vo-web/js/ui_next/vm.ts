/** The execution capabilities consumed by the UI exchange driver. */
export interface UiVm {
  run(): string;
  runScheduled(): string;
  takeHostOutput(): Uint8Array | undefined;
  takePendingHostEvents(): readonly { key: string }[];
  wakeHostEventWithData(key: string, data: Uint8Array): boolean;
}

/** A loadable execution module. Compilation and legacy UI APIs are optional. */
export interface UiVmRuntime {
  default(): Promise<unknown>;
  VoVmIsland: new (bytecode: Uint8Array) => UiVm & { free(): void };
}

const initializations = new WeakMap<UiVmRuntime['default'], Promise<unknown>>();

/** wasm-bindgen caches completed initialization, but concurrent calls can still
 * replace its shared exports while another root already owns a live Island.
 * Use the initializer identity so wrappers around the same module also join. */
export function initializeUiVm(runtime: UiVmRuntime): Promise<unknown> {
  const initialize = runtime.default;
  let pending = initializations.get(initialize);
  if (!pending) {
    pending = Promise.resolve().then(() => initialize.call(runtime)).catch(error => {
      initializations.delete(initialize);
      throw error;
    });
    initializations.set(initialize, pending);
  }
  return pending;
}

/** The VM owns execution; its caller owns the Island and exchange lifetime. */
export async function runVmUi(vm: UiVm, exchange: (bytes: Uint8Array) => Promise<Uint8Array>): Promise<void> {
  let status = vm.run();
  while (status === 'suspended_for_host_events') {
    const batch = vm.takeHostOutput();
    const pending = vm.takePendingHostEvents();
    if (pending.length !== 1) throw new Error('UI host expects one exchange wait');
    const response = await exchange(batch ?? new Uint8Array());
    if (!vm.wakeHostEventWithData(pending[0].key, response)) throw new Error('UI exchange could not resume');
    status = vm.runScheduled();
  }
  if (status !== 'completed') throw new Error(`UI VM stopped: ${status}`);
}
