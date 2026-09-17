/** Optional string storage for owned services. A write resolves only when its
 * IndexedDB transaction commits; cancellation aborts uncommitted work. */
export function createPersistentStorage(name: string, factory: IDBFactory = globalThis.indexedDB) {
  function run<T>(mode: IDBTransactionMode, signal: AbortSignal | undefined,
    body: (store: IDBObjectStore, result: (value: T) => void, fail: (error: unknown) => void) => void): Promise<T> {
    return new Promise((resolve, reject) => {
      if (signal?.aborted) { reject(signal.reason); return; }
      if (!factory) { reject(new Error('Persistent storage is unavailable.')); return; }
      let database: IDBDatabase | undefined, transaction: IDBTransaction | undefined;
      let settled = false, value: T;
      const finish = (failure?: {cause: unknown}) => {
        if (settled) return;
        settled = true;
        signal?.removeEventListener('abort', abort);
        database?.close();
        if (failure) reject(failure.cause); else resolve(value);
      };
      const fail = (error: unknown) => {
        try { transaction?.abort(); } catch { /* Already completed or aborted. */ }
        finish({cause: error});
      };
      const abort = () => fail(signal!.reason);
      signal?.addEventListener('abort', abort, {once: true});
      let opening: IDBOpenDBRequest;
      try { opening = factory.open(name, 1); } catch (error) { fail(error); return; }
      opening.onblocked = () => fail(new Error('Close the older application using this storage and try again.'));
      opening.onerror = () => fail(opening.error ?? new Error('Could not open persistent storage.'));
      opening.onupgradeneeded = () => {
        if (settled) { opening.transaction?.abort(); return; }
        opening.result.createObjectStore('values');
      };
      opening.onsuccess = () => {
        database = opening.result;
        if (settled) { database.close(); return; }
        database.onversionchange = () => database!.close();
        try {
          transaction = database.transaction('values', mode, {durability: 'strict'});
          transaction.oncomplete = () => finish();
          transaction.onabort = () => finish({cause: transaction!.error ?? new Error('Persistent storage transaction aborted.')});
          body(transaction.objectStore('values'), result => { value = result; }, fail);
        } catch (error) { fail(error); }
      };
    });
  }

  return {
    /** A synchronous legacy reader is imported only when this key is absent.
     * The check and import share one transaction; existing values always win. */
    get(key: string, signal?: AbortSignal, migrate?: () => string | null): Promise<string | null> {
      return run(migrate ? 'readwrite' : 'readonly', signal, (store, result, fail) => {
        const request = store.get(key);
        request.onsuccess = () => {
          try {
            let value: unknown = request.result;
            if (value === undefined && migrate) {
              value = migrate();
              if (value !== null) store.put(value, key);
            }
            if (value === undefined || value === null) result(null);
            else if (typeof value === 'string') result(value);
            else fail(new Error('Invalid persistent storage value.'));
          } catch (error) { fail(error); }
        };
      });
    },
    set(key: string, value: string, signal?: AbortSignal): Promise<void> {
      return run('readwrite', signal, store => { store.put(value, key); });
    },
    remove(key: string, signal?: AbortSignal): Promise<void> {
      return run('readwrite', signal, store => { store.delete(key); });
    },
  };
}
