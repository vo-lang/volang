/**
 * CodeMirror 6 ExternalWidget plugin.
 *
 * Sends `{"type":"change","content":"..."}` payloads to the IDE host VM
 * whenever the editor content changes.
 */

import { registerWidget } from '@vogui/index';
import type { WidgetInstance } from '@vogui/renderer';

function createCodeMirrorWidget(
  container: HTMLElement,
  props: any,
  onEvent: (payload: string) => void,
): WidgetInstance {
  container.style.height = '100%';
  container.style.overflow = 'hidden';
  container.style.display = 'flex';
  container.style.flexDirection = 'column';

  let editorView: any = null;
  let currentContent: string = props?.content ?? '';

  // Placeholder while CodeMirror loads
  const placeholder = document.createElement('div');
  placeholder.style.cssText = 'flex:1;background:#1e1e2e;color:#ccc;font:13px monospace;padding:12px;white-space:pre-wrap;overflow:auto';
  placeholder.textContent = currentContent;
  container.appendChild(placeholder);

  // Async CodeMirror init
  (async () => {
    try {
      const { EditorView, basicSetup } = await import('codemirror');
      const { keymap } = await import('@codemirror/view');
      const { indentWithTab } = await import('@codemirror/commands');
      const { EditorState } = await import('@codemirror/state');
      const { oneDark } = await import('@codemirror/theme-one-dark');

      const state = EditorState.create({
        doc: currentContent,
        extensions: [
          basicSetup,
          keymap.of([indentWithTab]),
          oneDark,
          EditorView.updateListener.of((update) => {
            if (update.docChanged) {
              const newContent = update.state.doc.toString();
              if (newContent !== currentContent) {
                currentContent = newContent;
                onEvent(newContent);
              }
            }
          }),
          EditorView.theme({
            '&': { height: '100%', fontSize: '13px' },
            '.cm-scroller': { overflow: 'auto', fontFamily: "'JetBrains Mono', 'Fira Code', monospace" },
          }),
        ],
      });

      editorView = new EditorView({ state, parent: container });
      container.removeChild(placeholder);
    } catch (e) {
      console.error('[CodeMirror] Failed to initialize:', e);
    }
  })();

  return {
    element: container,
    update(newProps: any) {
      const newContent = newProps?.content ?? '';
      if (editorView && newContent !== currentContent) {
        const doc = editorView.state.doc.toString();
        if (doc !== newContent) {
          editorView.dispatch({
            changes: { from: 0, to: doc.length, insert: newContent },
          });
          currentContent = newContent;
        }
      } else if (!editorView) {
        currentContent = newContent;
        placeholder.textContent = newContent;
      }
    },
    destroy() {
      if (editorView) {
        editorView.destroy();
        editorView = null;
      }
    },
  };
}

export function registerCodeMirrorWidget() {
  registerWidget('codemirror', {
    create: createCodeMirrorWidget,
  });
}
