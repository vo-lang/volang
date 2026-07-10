const ownerNames = new Set([
  'RaceSession',
  'KartRig',
  'TrackRuntime',
  'HUDPresenter',
  'PerfReporter',
  'AssetRuntimeCache',
]);

const stateGroups = new Map([
  ['BlockKartRuntimeCore', 'core'],
  ['BlockKartRuntimeInputState', 'input'],
  ['BlockKartRuntimeRaceState', 'race'],
  ['BlockKartRuntimeKartState', 'kart'],
  ['BlockKartRuntimeHudState', 'hud'],
]);

function lineAt(source, index) {
  return source.slice(0, index).split(/\r?\n/).length;
}

function typeFacts(sources) {
  const aliases = new Map();
  const structs = new Map();
  for (const entry of sources) {
    for (const match of entry.source.matchAll(/^type\s+([A-Za-z_][A-Za-z0-9_]*)\s*=\s*\*?([A-Za-z_][A-Za-z0-9_]*)\s*$/gm)) {
      aliases.set(match[1], match[2]);
    }
    for (const match of entry.source.matchAll(/^type\s+([A-Za-z_][A-Za-z0-9_]*)\s+struct\s*\{([\s\S]*?)^\}/gm)) {
      const references = [];
      for (const line of match[2].split(/\r?\n/)) {
        const field = line.trim().match(/^(?:[A-Za-z_][A-Za-z0-9_]*\s+)?\*?([A-Za-z_][A-Za-z0-9_]*)$/);
        if (field) references.push(field[1]);
      }
      structs.set(match[1], references);
    }
  }
  return { aliases, structs };
}

function collectGroups(typeName, facts, seen = new Set()) {
  if (seen.has(typeName)) return new Set();
  seen.add(typeName);
  if (typeName === 'BlockKartRuntimeContext') {
    return new Set(stateGroups.values());
  }
  if (stateGroups.has(typeName)) {
    return new Set([stateGroups.get(typeName)]);
  }
  if (facts.aliases.has(typeName)) {
    return collectGroups(facts.aliases.get(typeName), facts, seen);
  }
  const groups = new Set();
  for (const reference of facts.structs.get(typeName) ?? []) {
    for (const group of collectGroups(reference, facts, new Set(seen))) groups.add(group);
  }
  return groups;
}

function reachesRuntimeContext(typeName, facts, seen = new Set()) {
  if (typeName === 'BlockKartRuntimeContext') return true;
  if (seen.has(typeName)) return false;
  seen.add(typeName);
  if (facts.aliases.has(typeName)) {
    return reachesRuntimeContext(facts.aliases.get(typeName), facts, seen);
  }
  return (facts.structs.get(typeName) ?? [])
    .some((reference) => reachesRuntimeContext(reference, facts, new Set(seen)));
}

export function analyzeBlockKartBoundary(sources, { maximumOwnerStateGroups = 3 } = {}) {
  const facts = typeFacts(sources);
  const wideOwnerParameters = [];
  for (const entry of sources) {
    const methodPattern = /^func\s+\([^)]*\*([A-Za-z_][A-Za-z0-9_]*)\)\s+([A-Za-z_][A-Za-z0-9_]*)\(([^)]*)\)/gm;
    for (const match of entry.source.matchAll(methodPattern)) {
      if (!ownerNames.has(match[1])) continue;
      const methodGroups = new Set();
      const parameterTypes = [];
      const contextParameters = [];
      for (const parameter of match[3].matchAll(/\*([A-Za-z_][A-Za-z0-9_]*)/g)) {
        const parameterType = parameter[1];
        const groups = [...collectGroups(parameterType, facts)].sort();
        const contextReachable = reachesRuntimeContext(parameterType, facts);
        parameterTypes.push(parameterType);
        for (const group of groups) methodGroups.add(group);
        if (contextReachable) contextParameters.push(parameterType);
      }
      const groups = [...methodGroups].sort();
      if (contextParameters.length === 0 && groups.length <= maximumOwnerStateGroups) continue;
      wideOwnerParameters.push({
        path: entry.rel ?? entry.file,
        line: lineAt(entry.source, match.index),
        owner: match[1],
        method: match[2],
        parameterType: parameterTypes.join(', '),
        parameterTypes,
        groups,
        reason: contextParameters.length > 0
          ? `method parameter reaches BlockKartRuntimeContext through an alias or wrapper: ${contextParameters.join(', ')}`
          : `method parameters jointly expose ${groups.length} runtime state groups`,
      });
    }
  }
  return {
    aliases: Object.fromEntries(facts.aliases),
    stateGroupsByType: Object.fromEntries(
      [...new Set([...facts.aliases.keys(), ...facts.structs.keys()])]
        .sort()
        .map((typeName) => [typeName, [...collectGroups(typeName, facts)].sort()]),
    ),
    wideOwnerParameters,
  };
}
