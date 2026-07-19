export function portableCaseKey(value: string): string {
  return value.normalize('NFKC').toLowerCase();
}

export function portablePathCollisionKey(value: string): string {
  return value
    .replace(/\\/g, '/')
    .split('/')
    .map(portableCaseKey)
    .join('/');
}

interface PortablePathEntry {
  path: string;
  isDirectory: boolean;
}

const utf8Encoder = new TextEncoder();

export class PortablePathTrie {
  private readonly entries = new Map<string, PortablePathEntry>();
  private readonly nodes = new Set<string>();
  private keyBytes = 0;

  constructor(
    private readonly maxNodes: number,
    private readonly maxKeyBytes = Number.MAX_SAFE_INTEGER,
  ) {
    if (!Number.isSafeInteger(maxNodes) || maxNodes <= 0) {
      throw new Error('portable path registry limit must be a positive integer');
    }
    if (!Number.isSafeInteger(maxKeyBytes) || maxKeyBytes <= 0) {
      throw new Error('portable path registry byte limit must be a positive integer');
    }
  }

  get nodeCount(): number {
    return this.nodes.size;
  }

  insert(path: string, isDirectory: boolean, label: string): void {
    const key = portablePathCollisionKey(path);
    const duplicate = this.entries.get(key);
    if (duplicate) {
      throw new Error(`${label} collides with ${duplicate.path}: ${path}`);
    }

    const components = key.split('/');
    for (let length = 1; length < components.length; length += 1) {
      const ancestor = this.entries.get(components.slice(0, length).join('/'));
      if (ancestor && !ancestor.isDirectory) {
        throw new Error(`${label} descends from file ${ancestor.path}: ${path}`);
      }
    }
    if (!isDirectory) {
      const prefix = `${key}/`;
      for (const [existingKey, existing] of this.entries) {
        if (existingKey.startsWith(prefix)) {
          throw new Error(`${label} is an ancestor of ${existing.path}: ${path}`);
        }
      }
    }

    const prefixes = components.map((_, index) => components.slice(0, index + 1).join('/'));
    const newNodes = prefixes.filter((prefix) => !this.nodes.has(prefix));
    const nextNodeCount = this.nodes.size + newNodes.length;
    if (!Number.isSafeInteger(nextNodeCount) || nextNodeCount > this.maxNodes) {
      throw new Error(`${label} exceeds the ${this.maxNodes}-node limit`);
    }
    const addedKeyBytes = newNodes.reduce(
      (total, prefix) => total + utf8Encoder.encode(prefix).byteLength,
      0,
    );
    const nextKeyBytes = this.keyBytes + addedKeyBytes;
    if (!Number.isSafeInteger(nextKeyBytes) || nextKeyBytes > this.maxKeyBytes) {
      throw new Error(`${label} exceeds the ${this.maxKeyBytes}-byte key limit`);
    }

    this.entries.set(key, { path, isDirectory });
    for (const prefix of newNodes) this.nodes.add(prefix);
    this.keyBytes = nextKeyBytes;
  }
}
