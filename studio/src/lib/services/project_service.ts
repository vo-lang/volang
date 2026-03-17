import type { Backend } from '../backend/backend';
import type { BootstrapContext, SessionInfo } from '../types';
import { WorkspaceService } from './workspace_service';

export class ProjectService {
  private bootstrap: BootstrapContext | null = null;
  private session: SessionInfo | null = null;

  constructor(
    private readonly backend: Backend,
    private readonly workspace: WorkspaceService,
  ) {}

  get bootstrapContext(): BootstrapContext {
    if (!this.bootstrap) {
      throw new Error('Bootstrap context has not been loaded');
    }
    return this.bootstrap;
  }

  get sessionInfo(): SessionInfo | null {
    return this.session;
  }

  async initialize(): Promise<BootstrapContext> {
    this.bootstrap = await this.backend.getBootstrapContext();
    return this.bootstrap;
  }

  async openWorkspace(): Promise<SessionInfo> {
    const session = await this.backend.openWorkspaceSession();
    this.bindSession(session);
    return session;
  }

  async openRunSession(path: string): Promise<SessionInfo> {
    const session = await this.backend.openRunSession(path);
    this.bindSession(session);
    return session;
  }

  async openUrl(url: string): Promise<SessionInfo> {
    const session = await this.backend.openUrlSession(url);
    this.bindSession(session);
    return session;
  }

  private bindSession(session: SessionInfo): void {
    this.session = session;
    this.workspace.bindSession(session);
  }
}
