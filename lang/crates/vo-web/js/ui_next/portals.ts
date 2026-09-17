import {nodeAt, type NodeEntry} from './nodes.js';

interface Placement {target: number; placeholder: Comment}

/** Physical placement only. Node identities and logical ownership stay in the
 * renderer tree; a comment retains the original insertion position. */
export class PortalHost {
  private readonly placements = new Map<number, Placement>();
  readonly targets = new Map<number, number>();

  constructor(private readonly nodes: ReadonlyMap<number, NodeEntry>) {}

  anchor(id: number): Node {
    return this.placements.get(id)?.placeholder ?? nodeAt(this.nodes, id).start;
  }

  private restore(id: number): void {
    const placement = this.placements.get(id);
    if (!placement) return;
    const node = nodeAt(this.nodes, id).start;
    placement.placeholder.replaceWith(node);
    this.placements.delete(id);
    this.targets.delete(id);
  }

  // A target's DOM may contain nodes with a different logical owner. Evacuate
  // surviving placements before removing it; dispose every removed source even
  // when its element now sits outside its owner's physical subtree.
  removing(removed: ReadonlySet<number>): void {
    for (const [id, placement] of this.placements) {
      if (removed.has(id)) {
        nodeAt(this.nodes, id).start.parentNode?.removeChild(nodeAt(this.nodes, id).start);
        placement.placeholder.remove();
        this.placements.delete(id); this.targets.delete(id);
      } else if (removed.has(placement.target)) this.restore(id);
    }
  }

  settle(targets: ReadonlyMap<number, number> | undefined): void {
    if (!targets) return;
    for (const id of this.placements.keys()) if (!targets.has(id)) this.restore(id);
    const groups = new Map<number, number[]>();
    for (const [id, target] of targets) {
      let placement = this.placements.get(id);
      if (!placement) {
        const node = nodeAt(this.nodes, id).start;
        const placeholder = node.ownerDocument!.createComment(`ui:portal:${id}`);
        node.parentNode!.insertBefore(placeholder, node);
        placement = {target, placeholder};
        this.placements.set(id, placement);
      }
      placement.target = target; this.targets.set(id, target);
      const group = groups.get(target) ?? [];
      group.push(id); groups.set(target, group);
    }
    // Managed ordinary children precede portal content. Portals sharing a
    // target retain creation order independently of source sibling reordering.
    for (const [target, ids] of groups) {
      const destination = nodeAt(this.nodes, target).start;
      let before: Node | null = null;
      ids.sort((left, right) => left - right);
      for (let index = ids.length - 1; index >= 0; index--) {
        const node = nodeAt(this.nodes, ids[index]).start;
        if (node.parentNode !== destination || node.nextSibling !== before) destination.insertBefore(node, before);
        before = node;
      }
    }
  }

  close(): void {
    for (const id of this.placements.keys()) this.restore(id);
  }
}
