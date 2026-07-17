export interface LocalProjectSourcePlan {
  roots: string[];
  files: string[];
}

export interface LocalProjectSourcePlanOptions {
  voBin?: string;
  environment?: Record<string, string | undefined>;
  maxOutputBytes?: number;
  maxRoots?: number;
  timeoutMs?: number;
}

export function planLocalProjectSources(
  projectRoot: string,
  options?: LocalProjectSourcePlanOptions,
): LocalProjectSourcePlan;
