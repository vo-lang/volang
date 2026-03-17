import type { Backend } from '../backend/backend';
import { NativeBackend } from '../backend/native_backend';
import { WebBackend } from '../backend/web_backend';
import { CompilerService } from './compiler_service';
import { ExtensionService } from './extension_service';
import { ProjectCatalogService } from './project_catalog_service';
import { ProjectService } from './project_service';
import { RuntimeService } from './runtime_service';
import { TerminalService } from './terminal_service';
import { WorkspaceService } from './workspace_service';

export interface ServiceRegistry {
  backend: Backend;
  compiler: CompilerService;
  extension: ExtensionService;
  projectCatalog: ProjectCatalogService;
  project: ProjectService;
  runtime: RuntimeService;
  terminal: TerminalService;
  workspace: WorkspaceService;
}

export async function createServiceRegistry(): Promise<ServiceRegistry> {
  const backend = createBackend();
  const compiler = new CompilerService(backend);
  const extension = new ExtensionService(backend);
  const workspace = new WorkspaceService(backend);
  const project = new ProjectService(backend, workspace);
  const projectCatalog = new ProjectCatalogService(backend, workspace);
  const runtime = new RuntimeService(backend);
  const terminal = new TerminalService(workspace, compiler, runtime, backend);
  await project.initialize();
  return {
    backend,
    compiler,
    extension,
    projectCatalog,
    project,
    runtime,
    terminal,
    workspace,
  };
}

function createBackend(): Backend {
  if (isTauriRuntime()) {
    return new NativeBackend();
  }
  return new WebBackend();
}

function isTauriRuntime(): boolean {
  const runtime = window as typeof window & {
    __TAURI__?: unknown;
    __TAURI_INTERNALS__?: unknown;
  };
  return Boolean(runtime.__TAURI__ || runtime.__TAURI_INTERNALS__);
}
