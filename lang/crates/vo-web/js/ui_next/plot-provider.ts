import {createLazyWidget} from './lazy-widget.js';
import {createUPlotWidget} from './uplot.js';
import type {WidgetInstance, WidgetProvider} from './widgets.js';

type PlotLibrary = {default: unknown; css: string};
type StyleRoot = Document | ShadowRoot;
type StyleRecord = {element: HTMLStyleElement; css: string; references: number};
const styles = new WeakMap<StyleRoot, StyleRecord>();

// A chart may belong to a shadow tree. Keep the library stylesheet with that
// actual rendering root and share it only while at least one chart is mounted.
function retainStyles(element: Element, css: string): () => void {
  const node = element.getRootNode();
  const root = node.nodeType === 11 && 'host' in node ? node as ShadowRoot : element.ownerDocument;
  let record = styles.get(root);
  if (record && record.css !== css) throw new Error('Chart roots require one matching stylesheet.');
  if (!record) {
    const sheet = element.ownerDocument.createElement('style');
    sheet.dataset.uiPlot = '';
    sheet.textContent = css;
    if (root.nodeType === 9) (root as Document).head.append(sheet);
    else root.appendChild(sheet);
    record = {element: sheet, css, references: 0};
    styles.set(root, record);
  }
  const owned = record;
  owned.references++;
  let released = false;
  return () => {
    if (released) return;
    released = true;
    if (--owned.references === 0) {
      owned.element.remove();
      styles.delete(root);
    }
  };
}

/** The matching toolkit supplies its pinned library. No chart means no import. */
export function createPlotProvider(load: () => Promise<PlotLibrary>): WidgetProvider {
  return createLazyWidget(async (_document, signal) => {
    const library = await load();
    signal.throwIfAborted();
    if (typeof library.default !== 'function' || typeof library.css !== 'string' || !library.css) {
      throw new Error('The chart library or stylesheet is unavailable.');
    }
    const provider = createUPlotWidget(library.default);
    return context => {
      const release = retainStyles(context.element, library.css);
      let instance: WidgetInstance;
      try { instance = provider(context); }
      catch (error) { release(); throw error; }
      let disposed = false;
      return {
        update(value) { if (!disposed) instance.update(value); },
        afterCommit() { if (!disposed) instance.afterCommit?.(); },
        commitTargets() { return instance.afterCommit ? instance.commitTargets?.() : []; },
        dispose() {
          if (disposed) return;
          disposed = true;
          try { instance.dispose(); }
          finally { release(); }
        },
      };
    };
  });
}
