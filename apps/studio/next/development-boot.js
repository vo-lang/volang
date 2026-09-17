import { mountUi } from '/host/ui_next/development-mount.js';
import { startStudio } from './application.js';

await startStudio(mountUi, { artifact: 'studio-dev', defaultBackend: 'vm' });
