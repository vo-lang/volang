import { MAX_FRAME_BYTES } from './generated/protocol.js';
import { runVmUi, type UiVm } from './vm.js';

export interface UiWorkerPort {
  postMessage(message: unknown, transfer: Transferable[]): void;
  addEventListener(type: 'message', listener: (event: MessageEvent) => void): void;
  removeEventListener(type: 'message', listener: (event: MessageEvent) => void): void;
}

/** Run inside a dedicated Worker. The caller frees the Island after settlement.
 * Initial compilation/loading failures should send ui-exit with a readable error. */
export async function runWorkerVm(vm: UiVm, port: UiWorkerPort): Promise<void> {
  let sequence = 0;
  let pending: { resolve: (bytes: Uint8Array) => void; reject: (error: Error) => void } | undefined;
  const receive = (event: MessageEvent): void => {
    const current = pending, message = event.data;
    if (!current) return;
    pending = undefined;
    if (message?.kind !== 'ui-input' || message.sequence !== sequence || !(message.bytes instanceof Uint8Array)
      || message.bytes.length > MAX_FRAME_BYTES) {
      current.reject(new Error('Invalid UI worker input'));
    } else current.resolve(message.bytes);
  };
  port.addEventListener('message', receive);
  try {
    await runVmUi(vm, bytes => new Promise((resolve, reject) => {
      if (pending) { reject(new Error('Overlapping UI worker request')); return; }
      pending = { resolve, reject };
      port.postMessage({ kind: 'ui-exchange', sequence: ++sequence, bytes }, [bytes.buffer]);
    }));
    port.postMessage({ kind: 'ui-exit', error: '' }, []);
  } catch (error) {
    port.postMessage({ kind: 'ui-exit', error: String((error as Error)?.message ?? error).slice(0, 65536) }, []);
  } finally {
    pending = undefined;
    port.removeEventListener('message', receive);
  }
}
