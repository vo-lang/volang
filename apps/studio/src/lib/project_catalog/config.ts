import type { SessionInfo } from '../types';
import type { ManagedProject, ManagedProjectConfig } from './types';

const PROJECT_CONFIG_STORAGE_KEY = 'vo_studio_project_config_v1';

type ProjectConfigMap = Record<string, ManagedProjectConfig>;
type ProjectConfigInput = Partial<ManagedProjectConfig>;

export function defaultProjectConfig(): ManagedProjectConfig {
  return { hasGui: false };
}

export function normalizeProjectConfig(config: ProjectConfigInput | null | undefined): ManagedProjectConfig {
  return {
    hasGui: config?.hasGui ?? false,
  };
}

export function projectConfigKey(project: Pick<ManagedProject, 'type' | 'localPath' | 'entryPath'>): string | null {
  if (project.type === 'module') {
    return project.localPath ? `module:${normalizePath(project.localPath)}` : null;
  }
  const filePath = project.entryPath ?? project.localPath;
  return filePath ? `single:${normalizePath(filePath)}` : null;
}

export function sessionProjectConfigKey(session: Pick<SessionInfo, 'projectMode' | 'root' | 'entryPath'>): string {
  if (session.projectMode === 'module') {
    return `module:${normalizePath(session.root)}`;
  }
  return `single:${normalizePath(session.entryPath ?? session.root)}`;
}

export function readStoredProjectConfig(key: string | null): ManagedProjectConfig | null {
  if (!key) {
    return null;
  }
  return loadStoredProjectConfigs()[key] ?? null;
}

export function writeStoredProjectConfig(key: string | null, config: Partial<ManagedProjectConfig>): void {
  if (!key) {
    return;
  }
  const next = loadStoredProjectConfigs();
  next[key] = normalizeProjectConfig(config);
  saveStoredProjectConfigs(next);
}

export function deleteStoredProjectConfig(key: string | null): void {
  if (!key) {
    return;
  }
  const next = loadStoredProjectConfigs();
  if (!(key in next)) {
    return;
  }
  delete next[key];
  saveStoredProjectConfigs(next);
}

export function moveStoredProjectConfig(fromKey: string | null, toKey: string | null): void {
  if (!fromKey || !toKey || fromKey === toKey) {
    return;
  }
  const next = loadStoredProjectConfigs();
  const config = next[fromKey];
  if (!config) {
    return;
  }
  delete next[fromKey];
  next[toKey] = config;
  saveStoredProjectConfigs(next);
}

function loadStoredProjectConfigs(): ProjectConfigMap {
  if (typeof localStorage === 'undefined') {
    return {};
  }
  const raw = localStorage.getItem(PROJECT_CONFIG_STORAGE_KEY);
  if (!raw) {
    return {};
  }
  try {
    const parsed = JSON.parse(raw) as Record<string, ProjectConfigInput>;
    return Object.fromEntries(
      Object.entries(parsed).map(([key, config]) => [key, normalizeProjectConfig(config)]),
    );
  } catch {
    return {};
  }
}

function saveStoredProjectConfigs(configs: ProjectConfigMap): void {
  if (typeof localStorage === 'undefined') {
    return;
  }
  const keys = Object.keys(configs);
  if (keys.length === 0) {
    localStorage.removeItem(PROJECT_CONFIG_STORAGE_KEY);
    return;
  }
  localStorage.setItem(PROJECT_CONFIG_STORAGE_KEY, JSON.stringify(configs));
}

function normalizePath(path: string): string {
  const trimmed = path.trim();
  if (!trimmed) {
    return '/';
  }
  const absolute = trimmed.startsWith('/') ? trimmed : `/${trimmed}`;
  return absolute.endsWith('/') && absolute.length > 1 ? absolute.slice(0, -1) : absolute;
}
