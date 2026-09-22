import { mountUi } from './worker-mount.js';
import { startStudio } from './application.js';

// Root aliases serve the complete document; normalize without another request.
const alias = document.documentElement.dataset.studioAlias;
if (alias) history.replaceState(history.state, '', alias + location.search + location.hash);
await startStudio(mountUi);
