/** Give timers, I/O and rendering a task turn without a timer's minimum delay.
 * Each outstanding turn owns its ports; delivery closes them before resuming
 * guest work, including when several VM/AOT instances share the host.
 * @returns {Promise<void>}
 */
export function yieldHost() {
  if (typeof MessageChannel === 'undefined') {
    return new Promise(resolve => setTimeout(resolve, 0));
  }
  return new Promise(resolve => {
    const channel = new MessageChannel();
    channel.port1.onmessage = () => {
      channel.port1.close();
      channel.port2.close();
      resolve();
    };
    channel.port2.postMessage(0);
  });
}
