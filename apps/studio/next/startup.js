// Fetch the Wasm image before importing its JS bindings: their dependencies
// must not delay the large download. Packaged bytecode uses an explicit gzip
// file so delivery does not depend on a static host's encoding negotiation.
export async function loadStudioRuntime(artifact, {compressed = false, request = fetch,
  loadRuntime = () => import('/wasm/vo_web.js')} = {}) {
  const lifetime = new AbortController();
  const get = async url => {
    const response = await request(url, {signal:lifetime.signal});
    if (!response.ok) throw new Error(`Could not load Studio (${response.status}).`);
    return response;
  };
  try {
    const image = get('/wasm/vo_web_bg.wasm');
    const bytecodeURL = new URL(artifact);
    if (compressed) bytecodeURL.pathname += '.gz';
    const bytes = get(bytecodeURL).then(async response => {
      const buffer = new Uint8Array(await response.arrayBuffer());
      // Some hosts decode Content-Encoding themselves. Inspect the body to
      // avoid decoding twice while still accepting an ordinary .gz file.
      if (compressed && buffer[0] === 0x1f && buffer[1] === 0x8b) {
        return new Uint8Array(await new Response(new Blob([buffer]).stream()
          .pipeThrough(new DecompressionStream('gzip'))).arrayBuffer());
      }
      return buffer;
    });
    const initialized = Promise.all([Promise.resolve().then(loadRuntime), image]).then(async ([runtime, response]) => {
      await runtime.default({module_or_path:response});
      return runtime;
    });
    const [runtime, bytecode] = await Promise.all([initialized, bytes]);
    return {runtime, bytes:bytecode};
  } catch (error) {
    lifetime.abort();
    throw error;
  }
}
