import { fileURLToPath } from 'url';
import { dirname, resolve } from 'path';

const _dir = dirname(fileURLToPath(import.meta.url));

/** @type {import('tailwindcss').Config} */
export default {
  content: [resolve(_dir, 'src/**/*.{ts,tsx}')],
  darkMode: 'class',
  theme: {
    extend: {
      colors: {
        primary: {
          DEFAULT: 'var(--vo-primary, #3b82f6)',
          foreground: 'var(--vo-primary-foreground, #ffffff)',
        },
        secondary: {
          DEFAULT: 'var(--vo-secondary, #6b7280)',
          foreground: 'var(--vo-secondary-foreground, #ffffff)',
        },
        success: {
          DEFAULT: 'var(--vo-success, #22c55e)',
          foreground: 'var(--vo-success-foreground, #ffffff)',
        },
        danger: {
          DEFAULT: 'var(--vo-danger, #ef4444)',
          foreground: 'var(--vo-danger-foreground, #ffffff)',
        },
        warning: {
          DEFAULT: 'var(--vo-warning, #f59e0b)',
          foreground: 'var(--vo-warning-foreground, #ffffff)',
        },
        info: {
          DEFAULT: 'var(--vo-info, #06b6d4)',
          foreground: 'var(--vo-info-foreground, #ffffff)',
        },
        background: 'var(--vo-background, #ffffff)',
        foreground: 'var(--vo-text, #0f172a)',
        muted: {
          DEFAULT: 'var(--vo-muted, #f1f5f9)',
          foreground: 'var(--vo-text-muted, #64748b)',
        },
        surface: 'var(--vo-surface, #f8fafc)',
        card: {
          DEFAULT: 'var(--vo-card, #ffffff)',
          foreground: 'var(--vo-card-foreground, #0f172a)',
        },
        popover: {
          DEFAULT: 'var(--vo-popover, #ffffff)',
          foreground: 'var(--vo-popover-foreground, #0f172a)',
        },
        border: 'var(--vo-border, #e2e8f0)',
        input: 'var(--vo-input-border, #e2e8f0)',
        ring: 'var(--vo-ring, #3b82f6)',
        accent: {
          DEFAULT: 'var(--vo-accent, #f1f5f9)',
          foreground: 'var(--vo-accent-foreground, #0f172a)',
        },
      },
      borderRadius: {
        lg: 'var(--vo-radius-lg, 8px)',
        md: 'var(--vo-radius, 6px)',
        sm: 'var(--vo-radius-sm, 4px)',
      },
      fontFamily: {
        sans: ['var(--vo-font-family, system-ui, -apple-system, sans-serif)'],
      },
      keyframes: {
        'fade-in': {
          from: { opacity: '0' },
          to: { opacity: '1' },
        },
        'fade-out': {
          from: { opacity: '1' },
          to: { opacity: '0' },
        },
        'slide-in-from-top': {
          from: { transform: 'translateY(-100%)' },
          to: { transform: 'translateY(0)' },
        },
        'slide-in-from-bottom': {
          from: { transform: 'translateY(100%)' },
          to: { transform: 'translateY(0)' },
        },
        'slide-in-from-left': {
          from: { transform: 'translateX(-100%)' },
          to: { transform: 'translateX(0)' },
        },
        'slide-in-from-right': {
          from: { transform: 'translateX(100%)' },
          to: { transform: 'translateX(0)' },
        },
        'scale-in': {
          from: { opacity: '0', transform: 'scale(0.95)' },
          to: { opacity: '1', transform: 'scale(1)' },
        },
        'spin': {
          from: { transform: 'rotate(0deg)' },
          to: { transform: 'rotate(360deg)' },
        },
      },
      animation: {
        'fade-in': 'fade-in 0.2s ease-out',
        'fade-out': 'fade-out 0.2s ease-out',
        'slide-in-from-top': 'slide-in-from-top 0.2s ease-out',
        'slide-in-from-bottom': 'slide-in-from-bottom 0.2s ease-out',
        'slide-in-from-left': 'slide-in-from-left 0.2s ease-out',
        'slide-in-from-right': 'slide-in-from-right 0.2s ease-out',
        'scale-in': 'scale-in 0.2s ease-out',
        'spin': 'spin 1s linear infinite',
      },
    },
  },
  plugins: [],
};
