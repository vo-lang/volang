import { mountUi } from '/host/ui_next/mount.js';
import { startStudio } from './application.js';

await startStudio(mountUi);
