import { fileURLToPath } from 'url';
import { dirname, resolve } from 'path';

const _dir = dirname(fileURLToPath(import.meta.url));

export default {
  plugins: {
    tailwindcss: { config: resolve(_dir, 'tailwind.config.js') },
    autoprefixer: {},
  },
};
