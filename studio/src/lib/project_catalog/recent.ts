import type { ManagedProjectType } from './types';

const STORAGE_KEY = 'vo_studio_recent_projects_v1';
const MAX_RECENT = 30;

export interface RecentProject {
  name: string;
  type: ManagedProjectType;
  localPath: string;
  entryPath: string | null;
  openedAt: number;
}

export function recentProjectIdentity(project: Pick<RecentProject, 'type' | 'localPath' | 'entryPath'>): string {
  return project.type === 'module'
    ? project.localPath
    : (project.entryPath ?? project.localPath);
}

export function loadRecentProjects(): RecentProject[] {
  if (typeof localStorage === 'undefined') {
    return [];
  }
  const raw = localStorage.getItem(STORAGE_KEY);
  if (!raw) {
    return [];
  }
  try {
    const parsed = JSON.parse(raw) as RecentProject[];
    if (!Array.isArray(parsed)) {
      return [];
    }
    return parsed.filter(
      (p) => typeof p.name === 'string' && typeof p.localPath === 'string' && typeof p.type === 'string',
    );
  } catch {
    return [];
  }
}

export function addRecentProject(project: Omit<RecentProject, 'openedAt'>): void {
  const list = loadRecentProjects();
  const identity = recentProjectIdentity(project);
  const filtered = list.filter((p) => recentProjectIdentity(p) !== identity);
  filtered.unshift({ ...project, openedAt: Date.now() });
  if (filtered.length > MAX_RECENT) {
    filtered.length = MAX_RECENT;
  }
  saveRecentProjects(filtered);
}

export function removeRecentProject(localPath: string): void {
  const list = loadRecentProjects();
  const filtered = list.filter((p) => recentProjectIdentity(p) !== localPath);
  saveRecentProjects(filtered);
}

function saveRecentProjects(projects: RecentProject[]): void {
  if (typeof localStorage === 'undefined') {
    return;
  }
  if (projects.length === 0) {
    localStorage.removeItem(STORAGE_KEY);
    return;
  }
  localStorage.setItem(STORAGE_KEY, JSON.stringify(projects));
}
