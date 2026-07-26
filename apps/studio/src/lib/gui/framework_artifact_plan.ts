import type { FrameworkContract } from '../types.ts';

export type FrameworkArtifactPlan = Readonly<{
  frameworks: readonly FrameworkContract[];
  hostBridgePaths: readonly string[];
  rendererPaths: readonly string[];
  needsVfs: boolean;
}>;

/**
 * Builds the Studio-side artifact load plan exclusively from resolved provider
 * contracts. Exact duplicate contracts collapse; two different contracts that
 * claim the same framework owner are rejected before any module is loaded.
 */
export function planFrameworkArtifacts(
  primary: FrameworkContract | null,
  providers: readonly FrameworkContract[],
): FrameworkArtifactPlan {
  const frameworks: FrameworkContract[] = [];
  const ownerContracts = new Map<string, string>();
  const exactContracts = new Set<string>();
  const ordered = primary ? [primary, ...providers] : [...providers];
  for (const framework of ordered) {
    const key = contractKey(framework);
    const ownerKey = ownerContracts.get(framework.name);
    if (ownerKey !== undefined && ownerKey !== key) {
      throw new Error(`framework owner '${framework.name}' has conflicting runtime contracts`);
    }
    ownerContracts.set(framework.name, key);
    if (exactContracts.has(key)) continue;
    exactContracts.add(key);
    frameworks.push(framework);
  }

  const hostBridgePaths = uniqueModulePaths(frameworks, 'host_bridge');
  const rendererPaths = uniqueModulePaths(frameworks, 'renderer');
  return Object.freeze({
    frameworks: Object.freeze(frameworks),
    hostBridgePaths: Object.freeze(hostBridgePaths),
    rendererPaths: Object.freeze(rendererPaths),
    needsVfs: hostBridgePaths.length > 0 || rendererPaths.length > 0,
  });
}

function uniqueModulePaths(frameworks: readonly FrameworkContract[], name: string): string[] {
  const seen = new Set<string>();
  const paths: string[] = [];
  for (const framework of frameworks) {
    const path = framework.jsModules[name] ?? null;
    if (!path || seen.has(path)) continue;
    seen.add(path);
    paths.push(path);
  }
  return paths;
}

function contractKey(framework: FrameworkContract): string {
  const capabilities = [...framework.capabilities].sort(compareUtf8);
  const roles = [...framework.roles].sort(compareUtf8);
  const jsModules = Object.entries(framework.jsModules)
    .sort(([left], [right]) => compareUtf8(left, right))
    .flatMap(([name, path]) => [name, path]);
  return [
    framework.name,
    framework.moduleKey,
    framework.entry ?? '',
    framework.providerRole ?? '',
    ...framework.providerRoles,
    ...capabilities,
    ...roles,
    ...jsModules,
  ].join('\0');
}

function compareUtf8(left: string, right: string): number {
  const encoder = new TextEncoder();
  const leftBytes = encoder.encode(left);
  const rightBytes = encoder.encode(right);
  const common = Math.min(leftBytes.length, rightBytes.length);
  for (let index = 0; index < common; index += 1) {
    const order = leftBytes[index]! - rightBytes[index]!;
    if (order !== 0) return order;
  }
  return leftBytes.length - rightBytes.length;
}
