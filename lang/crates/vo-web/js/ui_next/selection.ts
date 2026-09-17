import { MAX_SELECTED_VALUES, MAX_FRAME_BYTES } from './generated/protocol.js';

export function parseSelection(value: string): string[] {
  if (value.length > MAX_FRAME_BYTES) throw new Error('UI selection exceeds the frame limit');
  const values: unknown = JSON.parse(value);
  if (!Array.isArray(values) || values.length > MAX_SELECTED_VALUES || values.some(item => typeof item !== 'string')) {
    throw new Error('UI selection requires at most 4096 string values');
  }
  return values;
}

export function selectedValues(select: HTMLSelectElement): string[] {
  return [...select.selectedOptions].map(option => option.value);
}

export function sameSelection(first: readonly string[], second: readonly string[]): boolean {
  return first.length === second.length && first.every((value, index) => value === second[index]);
}

export function applySelection(select: HTMLSelectElement, values: readonly string[]): void {
  const selected = new Set(values);
  for (const option of select.options) {
    const desired = selected.has(option.value);
    if (option.selected !== desired) option.selected = desired;
  }
}
