import { createLazyWidget } from './lazy-widget.js';
import type { WidgetProvider } from './widgets.js';
import type {EditorLanguageServiceFactory} from './editor-service.js';

/** The toolchain supplies the optional library location; each widget retains
 * the normal loading, cancellation and native editing fallback contract. */
export function createCodeEditorProvider(loadLibrary: () => Promise<unknown>, languageService?:EditorLanguageServiceFactory): WidgetProvider {
  return createLazyWidget(async (_document, signal) => {
    const [library, { createCodeEditorWidget }] = await Promise.all([
      loadLibrary(), import('./editor.js'),
    ]);
    signal.throwIfAborted();
    return createCodeEditorWidget(library,languageService);
  });
}
