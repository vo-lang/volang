// Style injection and theme application for VoGUI v4.

import type { StylePropertyMap, toCssValue } from './types';
import voguiCss from './vogui.css?inline';

let stylesInjected = false;
let styleEl: HTMLStyleElement | null = null;

/** Inject the base VoGUI Tailwind stylesheet. Called once before first render. */
export function injectStyles(): void {
    if (stylesInjected) return;
    stylesInjected = true;
    const style = document.createElement('style');
    style.id = 'vogui-base';
    style.textContent = voguiCss;
    document.head.appendChild(style);
}

/** Inject dynamic CSS strings from Vo's Style combinator system. */
export function injectDynamicStyles(styles: string[]): void {
    if (!styles || styles.length === 0) return;
    if (!styleEl) {
        styleEl = document.createElement('style');
        styleEl.id = 'vogui-dynamic';
        document.head.appendChild(styleEl);
    }
    for (const css of styles) {
        styleEl.sheet?.insertRule(css, styleEl.sheet.cssRules.length);
    }
}

/** Default theme values. */
const DEFAULT_THEME: Record<string, string> = {
    '--vo-primary': '#3b82f6',
    '--vo-primary-foreground': '#ffffff',
    '--vo-secondary': '#6b7280',
    '--vo-secondary-foreground': '#ffffff',
    '--vo-success': '#22c55e',
    '--vo-success-foreground': '#ffffff',
    '--vo-danger': '#ef4444',
    '--vo-danger-foreground': '#ffffff',
    '--vo-warning': '#f59e0b',
    '--vo-warning-foreground': '#ffffff',
    '--vo-info': '#06b6d4',
    '--vo-info-foreground': '#ffffff',
    '--vo-background': '#ffffff',
    '--vo-text': '#0f172a',
    '--vo-text-muted': '#64748b',
    '--vo-surface': '#f8fafc',
    '--vo-card': '#ffffff',
    '--vo-card-foreground': '#0f172a',
    '--vo-popover': '#ffffff',
    '--vo-popover-foreground': '#0f172a',
    '--vo-border': '#e2e8f0',
    '--vo-input-border': '#e2e8f0',
    '--vo-ring': '#3b82f6',
    '--vo-accent': '#f1f5f9',
    '--vo-accent-foreground': '#0f172a',
    '--vo-muted': '#f1f5f9',
    '--vo-radius': '6px',
    '--vo-radius-sm': '4px',
    '--vo-radius-lg': '8px',
    '--vo-font-family': 'system-ui, -apple-system, sans-serif',
};

// =========================================================================
// Dark mode
// =========================================================================

let darkModeContainer: HTMLElement | null = null;

/** Toggle dark mode on the render container. Returns the new state. */
export function toggleDarkMode(): boolean {
    if (!darkModeContainer) return false;
    const isDark = darkModeContainer.classList.toggle('dark');
    return isDark;
}

/** Explicitly set dark mode on or off. */
export function setDarkMode(enabled: boolean): void {
    if (!darkModeContainer) return;
    if (enabled) {
        darkModeContainer.classList.add('dark');
    } else {
        darkModeContainer.classList.remove('dark');
    }
}

/** Check if dark mode is currently active. */
export function isDarkMode(): boolean {
    return darkModeContainer?.classList.contains('dark') ?? false;
}

/** Apply theme CSS custom properties to a container element. */
export function applyTheme(container: HTMLElement, theme?: Record<string, string>): void {
    darkModeContainer = container;
    const vars = { ...DEFAULT_THEME, ...theme };
    for (const [key, value] of Object.entries(vars)) {
        container.style.setProperty(key, value);
    }
    container.style.color = vars['--vo-text'];
    container.style.fontFamily = vars['--vo-font-family'];
    container.style.fontSize = '14px';
}
