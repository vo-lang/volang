import type { Backend } from '../backend/backend';
import { NativeBackend } from '../backend/native_backend';
import { WebBackend } from '../backend/web_backend';
import { ProjectCatalogService } from './project_catalog_service';
import { ProjectService } from './project_service';
import { RuntimeService } from './runtime_service';

export interface ServiceRegistry {
  backend: Backend;
  projectCatalog: ProjectCatalogService;
  project: ProjectService;
  runtime: RuntimeService;
}

export async function createServiceRegistry(): Promise<ServiceRegistry> {
  const backend = createBackend();
  const project = new ProjectService(backend);
  const projectCatalog = new ProjectCatalogService(backend);
  const runtime = new RuntimeService(backend);
  await project.initialize();
  return {
    backend,
    projectCatalog,
    project,
    runtime,
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
