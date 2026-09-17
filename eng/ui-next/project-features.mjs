import {readFile} from 'node:fs/promises';
import {resolve} from 'node:path';
import {toolchain} from './toolchain.mjs';

// Optional host roots are shared by application bundling and portable delivery.
// Ordinary applications do not import them through the default mount entry.
const packs = {
  plot: {entry:'plot-provider.ts', symbol:'createPlotProvider', provider:'uplot',
    create:() => `createPlotProvider(() => import(${JSON.stringify(toolchain.plot)}))`},
  canvas: {entry:'canvas-bitmap.ts', symbol:'canvasBitmap', provider:'canvas-bitmap', create:() => 'canvasBitmap'},
  editor: {entry:'editor-provider.ts', symbol:'createCodeEditorProvider', provider:'code-editor',
    create:() => `createCodeEditorProvider(() => import(${JSON.stringify(toolchain.editor)}))`},
};
export const optionalHostEntries = Object.freeze(Object.values(packs).map(pack => pack.entry));

export function projectFeatures(config) {
  if (config.features === undefined) return [];
  const features = config.features;
  if (!Array.isArray(features) || features.some(name => typeof name !== 'string' || !Object.hasOwn(packs,name)) || new Set(features).size !== features.length) {
    throw new Error('features must be an array of unique supported names: ' + Object.keys(packs).join(', ') + '.');
  }
  return [...features].sort();
}

// Bind services to the actual root, including a candidate during source reload.
export function projectHost({features, development = false, desktop = false}) {
  const entry = resolve(toolchain.host, `${desktop ? 'desktop-mount' : development ? 'development-mount' : 'mount'}.ts`);
  const selected = features.map(name => packs[name]);
  return {
    name:'ui-project-host',
    setup(build) {
      // Scope CSS-as-text to the pinned chart package; application CSS keeps
      // esbuild's ordinary stylesheet behavior and license/input provenance.
      if (features.includes('plot')) build.onLoad({filter:/[/\\]uplot[/\\]dist[/\\]uPlot\.min\.css$/}, async ({path}) => ({
        contents:await readFile(path, 'utf8'), loader:'text',
      }));
      build.onResolve({filter:/^@volang\/ui-next$/}, () => selected.length
        ? {path:'entry', namespace:'ui-project-host'} : {path:entry});
      build.onLoad({filter:/^entry$/, namespace:'ui-project-host'}, () => ({
        loader:'js', resolveDir:toolchain.root,
        contents:`export * from ${JSON.stringify(entry)};
import {mountUi as mount} from ${JSON.stringify(entry)};
${selected.map(pack => `import {${pack.symbol}} from ${JSON.stringify(resolve(toolchain.host,pack.entry))};`).join('\n')}
export function mountUi(container, options) {
  const original = options.services;
  return mount(container, {...options, services:target => {
    const supplied = typeof original === 'function' ? original(target) : original;
    return {...supplied, widgets:{
      ${selected.map(pack => `${JSON.stringify(pack.provider)}:${pack.create()},`).join('\n      ')}
      ...supplied?.widgets,
    }};
  }});
}
`,
      }));
    },
  };
}
