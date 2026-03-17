import type { Backend } from '../backend/backend';
import type { DiscoveredProject, FsEntry, FsStat, GrepMatch, GrepOpts, ReadManyResult, SessionInfo } from '../types';

export class WorkspaceService {
  private sessionInfo: SessionInfo | null = null;

  constructor(private readonly backend: Backend) {}

  get root(): string {
    return this.sessionInfo?.root ?? '/';
  }

  get session(): SessionInfo | null {
    return this.sessionInfo;
  }

  bindSession(sessionInfo: SessionInfo): void {
    this.sessionInfo = sessionInfo;
  }

  async discoverProjects(root: string): Promise<DiscoveredProject[]> {
    return this.backend.discoverProjects(root);
  }

  async list(path: string): Promise<FsEntry[]> {
    return this.backend.listDir(path);
  }

  async stat(path: string): Promise<FsStat> {
    return this.backend.statPath(path);
  }

  async readFile(path: string): Promise<string> {
    return this.backend.readFile(path);
  }

  async readMany(paths: string[]): Promise<ReadManyResult[]> {
    return this.backend.readMany(paths);
  }

  async writeFile(path: string, content: string): Promise<void> {
    return this.backend.writeFile(path, content);
  }

  async mkdir(path: string): Promise<void> {
    return this.backend.mkdir(path);
  }

  async remove(path: string, recursive = false): Promise<void> {
    return this.backend.removeEntry(path, recursive);
  }

  async rename(oldPath: string, newPath: string): Promise<void> {
    return this.backend.renameEntry(oldPath, newPath);
  }

  async copy(src: string, dst: string): Promise<void> {
    return this.backend.copyEntry(src, dst);
  }

  async grep(path: string, pattern: string, opts?: GrepOpts): Promise<GrepMatch[]> {
    return this.backend.grep(path, pattern, opts);
  }
}
