// Native directory reads and module imports cannot always be cancelled. Release
// the awaiting operation promptly and observe their eventual rejection safely.
export function recoveryRead(operation, signal) {
  signal.throwIfAborted();
  return new Promise((resolve, reject) => {
    const abort = () => {signal.removeEventListener('abort', abort); reject(signal.reason);};
    signal.addEventListener('abort', abort, {once:true});
    Promise.resolve().then(() => {signal.throwIfAborted(); return operation();}).then(value => {
      signal.removeEventListener('abort', abort);
      if (signal.aborted) reject(signal.reason); else resolve(value);
    }, error => {signal.removeEventListener('abort', abort); reject(error);});
  });
}
