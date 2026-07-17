import type { Backend } from '../backend/backend';

export class ExtensionService {
  constructor(private readonly backend: Backend) {}

  async voVersion(): Promise<string> {
    return this.backend.voVersion();
  }

  async voInit(path: string, module: string, mainContent: string): Promise<string> {
    return this.backend.voInit(path, module, mainContent);
  }
}
