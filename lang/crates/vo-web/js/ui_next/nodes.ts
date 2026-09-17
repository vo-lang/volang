import type { NativeListener } from './events.js';

export interface NodeEntry {
  start: Node;
  end: Node;
  kind: string;
  namespace: string;
  parent: number | null;
  children: number[];
  listeners: Map<string, NativeListener>;
  desiredValue?: string;
  desiredChecked?: boolean;
  desiredIndeterminate?: boolean;
  desiredSelection?: readonly string[];
  /** Last committed presentation request; matching native toggles are echoes. */
  desiredPopoverOpen?: boolean;
  desiredModalOpen?: boolean;
  editedAt: number;
  composing: boolean;
}
export interface NodeShape { kind: string; namespace: string; parent: number | null; children: number[]; text?: string }

export function nodeAt(nodes: ReadonlyMap<number, NodeEntry>, id: number): NodeEntry {
  const entry = nodes.get(id);
  if (!entry) throw new Error(`unknown UI node ${id}`);
  return entry;
}
