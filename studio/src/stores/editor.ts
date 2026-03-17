import { writable } from 'svelte/store';

export interface EditorState {
  activeFilePath: string;
  code: string;
  dirty: boolean;
}

export const editor = writable<EditorState>({
  activeFilePath: '',
  code: '',
  dirty: false,
});

export function editorOpen(path: string, content: string): void {
  editor.set({ activeFilePath: path, code: content, dirty: false });
}

export function editorMarkDirty(): void {
  editor.update((s) => ({ ...s, dirty: true }));
}

export function editorMarkSaved(): void {
  editor.update((s) => ({ ...s, dirty: false }));
}

export function editorSetCode(code: string): void {
  editor.update((s) => ({ ...s, code, dirty: true }));
}
