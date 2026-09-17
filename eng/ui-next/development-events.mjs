/** Shared development stream for local hosts. Late connections receive the
 * current compiler diagnostic; successful builds retire that diagnostic. */
export function developmentEvents() {
  const subscribers = new Set();
  let diagnostic, closed = false;
  return {
    get hasError() { return diagnostic !== undefined; },
    handle(pathname, request, response) {
      if (pathname !== '/__ui-next/events') return false;
      if (closed) {response.writeHead(503, {connection:'close'}).end(); return true;}
      if (request.method !== 'GET') {response.writeHead(405, {allow:'GET'}).end(); return true;}
      if (subscribers.size >= 64) {response.writeHead(503, {'retry-after':'1'}).end(); return true;}
      response.writeHead(200, {'content-type':'text/event-stream', 'cache-control':'no-cache'});
      response.write(': connected\n\n');
      if (diagnostic) response.write(`data: ${JSON.stringify(diagnostic)}\n\n`);
      subscribers.add(response);
      response.once('close', () => subscribers.delete(response));
      return true;
    },
    broadcast(event) {
      if (event.type === 'error') diagnostic = event;
      if (event.type === 'reload' || event.type === 'guest') diagnostic = undefined;
      for (const response of subscribers) {
        if (!response.write(`data: ${JSON.stringify(event)}\n\n`)) {subscribers.delete(response); response.end();}
      }
    },
    close() {closed = true; diagnostic = undefined; for (const response of subscribers) response.end(); subscribers.clear();},
  };
}
