// PostCSS config for Vibe Studio.
//
// Studio does not use Tailwind directly, but it imports vogui.css?inline which
// must be processed by Tailwind (with vogui's content paths) so that utility
// classes emitted by vogui's renderer are included in the final CSS bundle.
//
// We explicitly load tailwindcss and autoprefixer from vogui's node_modules
// (where they are installed) rather than relying on vite silently using
// vogui's postcss.config.js. This makes the dependency surface visible.
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';
import { createRequire } from 'module';

const _dir   = dirname(fileURLToPath(import.meta.url));
const vogui  = resolve(_dir, '../libs/vogui/js');
const req    = createRequire(import.meta.url);

const tailwindcss  = req(resolve(vogui, 'node_modules/tailwindcss'));
const autoprefixer = req(resolve(vogui, 'node_modules/autoprefixer'));

export default {
  plugins: [
    tailwindcss({ config: resolve(vogui, 'tailwind.config.js') }),
    autoprefixer(),
  ],
};
