/** Default adapter fixture entry; packaged projects supply their own boot. */
import {mountUi} from './desktop-mount.js';
const root = document.getElementById('root');
if (!root) throw new Error('Missing desktop document root');
mountUi(root);
