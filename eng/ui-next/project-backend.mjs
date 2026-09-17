export const backendMarker = '<!--ui-next:backend-->';

export function projectBackend(config) {
  const backend = config.defaultBackend;
  if (backend !== undefined && backend !== 'vm') {
    throw new Error('defaultBackend must be "vm"; Wasm AOT has been removed.');
  }
  return backend;
}

// Resolve this build choice before either static rendering or native-server
// packaging. It never belongs to request data or a page entry's state.
export function prepareBackend(template, config, {development = false} = {}) {
  const configured = projectBackend(config);
  const markers = template.split(backendMarker).length - 1;
  if (markers > 1) throw new Error(`Use at most one ${backendMarker} in web/index.html.`);
  if (markers === 1) return template.replace(backendMarker, development ? 'vm' : configured ?? 'vm');
  if (configured !== undefined) {
    throw new Error(`defaultBackend requires <meta name="ui-next-backend" content="${backendMarker}"> in web/index.html.`);
  }
  if (!development) return template;
  // Older templates have no backend slot. Development must select its VM-only
  // artifact even when the authored document has a production backend meta.
  const head = /<head\b[^>]*>/i;
  if (!head.test(template)) throw new Error('Development requires a <head> in web/index.html.');
  return template.replace(head, tag => tag + '<meta name="ui-next-backend" content="vm">');
}
