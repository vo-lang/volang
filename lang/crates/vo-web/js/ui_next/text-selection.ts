import {MAX_FRAME_BYTES} from './generated/protocol.js';

export interface TextSelectionRequest {
  selection: {start:number; end:number; direction:'forward' | 'backward'};
  source:string;
}

export const textSelectionInputTypes = new Set(['text', 'search', 'url', 'tel', 'password']);

export function parseTextSelection(value:string):TextSelectionRequest {
  if (value.length > MAX_FRAME_BYTES) throw new Error('UI text selection exceeds the frame limit');
  const request = JSON.parse(value) as TextSelectionRequest;
  const selection = request?.selection;
  if (typeof request?.source !== 'string' || !selection || !Number.isSafeInteger(selection.start)
    || !Number.isSafeInteger(selection.end) || selection.start < 0 || selection.end < selection.start
    || selection.end > request.source.length || !['forward', 'backward'].includes(selection.direction)) {
    throw new Error('Invalid UI text selection');
  }
  // A source coordinate must never split a Unicode scalar value.
  for (const offset of [selection.start, selection.end]) {
    const previous = request.source.charCodeAt(offset - 1), current = request.source.charCodeAt(offset);
    if (previous >= 0xd800 && previous <= 0xdbff && current >= 0xdc00 && current <= 0xdfff) throw new Error('UI text selection splits a Unicode character');
  }
  return request;
}

// Optional text editors subscribe through this owned projection. The renderer
// keeps no library dependency and the native textarea remains authoritative.
const projections = new WeakMap<HTMLTextAreaElement | HTMLInputElement, () => void>();

export function registerTextSelectionProjection(input:HTMLTextAreaElement, project:() => void):() => void {
  if (projections.has(input)) throw new Error('A text control already has a selection projection');
  projections.set(input, project);
  return () => {if (projections.get(input) === project) projections.delete(input);};
}

export function applyTextSelection(input:HTMLTextAreaElement | HTMLInputElement, request:TextSelectionRequest):void {
  if (input.value !== request.source || input.matches(':disabled')) return;
  const {start,end,direction} = request.selection;
  input.setSelectionRange(start,end,direction);
  input.focus({preventScroll:true});
  projections.get(input)?.();
}
