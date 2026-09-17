// Previous Studio used sha256("Volang Studio\0/")[:16] as its asset-cache
// namespace. OPFS projects and localStorage drafts are outside this cache.
const prefix = 'volang-ui-d7559a3ca207bb8c-';
self.addEventListener('install', event => event.waitUntil(self.skipWaiting()));
self.addEventListener('message', event => {
  if (event.data?.type === 'volang.studio.retire') event.waitUntil(self.skipWaiting());
});
self.addEventListener('activate', event => event.waitUntil((async () => {
  await self.clients.claim();
  try {
    for (const key of await caches.keys()) if (key.startsWith(prefix)) await caches.delete(key);
  } finally {
    await self.registration.unregister();
  }
  for (const client of await self.clients.matchAll()) client.postMessage({type:'volang.studio.retired'});
})()));
// No fetch handler or forced navigation. Open tabs keep their current work;
// their next navigation uses the network. No replacement cache is registered.
