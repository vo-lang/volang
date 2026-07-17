export declare class PortablePathTrie {
  constructor(maxNodes: number, maxPathKeyBytes?: number);
  get nodeCount(): number;
  get pathKeyBytes(): number;
  insert(relative: string, isDirectory?: boolean, label?: string): void;
}
export declare function portableCaseKey(value: string): string;
export declare function portablePathCollisionKey(value: string): string;
