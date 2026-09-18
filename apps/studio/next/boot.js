import { mountUi } from './worker-mount.js';
import { startStudio } from './application.js';

await startStudio(mountUi);
