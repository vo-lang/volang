// VoGUI v2 Default Styles
// Includes component styles and utility classes.

const voguiStyles = `
/* =============================================================================
   CSS Custom Properties (Theme Defaults)
   ============================================================================= */

:root {
    --vo-primary: #3b82f6;
    --vo-secondary: #6b7280;
    --vo-success: #22c55e;
    --vo-danger: #ef4444;
    --vo-warning: #f59e0b;
    --vo-info: #06b6d4;
    --vo-bg: #ffffff;
    --vo-surface: #f8fafc;
    --vo-text: #0f172a;
    --vo-text-muted: #64748b;
    --vo-border: #e2e8f0;
    --vo-radius: 6px;
    --vo-font-family: system-ui, -apple-system, sans-serif;
}

/* =============================================================================
   Base Reset
   ============================================================================= */

*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

body {
    font-family: var(--vo-font-family);
    color: var(--vo-text);
    background: var(--vo-bg);
    line-height: 1.5;
}

/* =============================================================================
   Layout Components
   ============================================================================= */

.vo-row { display: flex; flex-direction: row; align-items: center; }
.vo-column { display: flex; flex-direction: column; }
.vo-center { display: flex; align-items: center; justify-content: center; }
.vo-stack { position: relative; }
.vo-stack > * { position: relative; }
.vo-grid { display: grid; }
.vo-spacer { flex: 1; }
.vo-divider { border: none; border-top: 1px solid var(--vo-border); margin: 8px 0; width: 100%; }
.vo-scroll { overflow: auto; }
.vo-wrap { display: flex; flex-wrap: wrap; }

/* =============================================================================
   Text & Display
   ============================================================================= */

.vo-text { }
.vo-badge {
    display: inline-flex; align-items: center; padding: 2px 8px;
    font-size: 0.75rem; font-weight: 500; border-radius: 9999px;
    background: var(--vo-surface); border: 1px solid var(--vo-border);
}
.vo-badge[data-variant="success"] { background: #dcfce7; color: #166534; border-color: #bbf7d0; }
.vo-badge[data-variant="danger"] { background: #fee2e2; color: #991b1b; border-color: #fecaca; }
.vo-badge[data-variant="warning"] { background: #fef3c7; color: #92400e; border-color: #fde68a; }
.vo-badge[data-variant="info"] { background: #cffafe; color: #155e75; border-color: #a5f3fc; }

.vo-tag {
    display: inline-flex; align-items: center; padding: 2px 10px;
    font-size: 0.8rem; border-radius: var(--vo-radius);
    background: var(--vo-surface); border: 1px solid var(--vo-border);
}

.vo-progress {
    width: 100%; height: 8px; background: var(--vo-surface);
    border-radius: 4px; overflow: hidden;
}
.vo-progress-bar {
    height: 100%; background: var(--vo-primary);
    border-radius: 4px; transition: width 0.3s ease;
}

.vo-spinner {
    width: 24px; height: 24px; border: 3px solid var(--vo-border);
    border-top-color: var(--vo-primary); border-radius: 50%;
    animation: vo-spin 0.8s linear infinite;
}
@keyframes vo-spin { to { transform: rotate(360deg); } }

.vo-alert {
    padding: 12px 16px; border-radius: var(--vo-radius);
    border: 1px solid var(--vo-border); background: var(--vo-surface);
}
.vo-alert[data-type="success"] { background: #f0fdf4; border-color: #bbf7d0; color: #166534; }
.vo-alert[data-type="danger"],
.vo-alert[data-type="error"] { background: #fef2f2; border-color: #fecaca; color: #991b1b; }
.vo-alert[data-type="warning"] { background: #fffbeb; border-color: #fde68a; color: #92400e; }
.vo-alert[data-type="info"] { background: #ecfeff; border-color: #a5f3fc; color: #155e75; }

.vo-avatar {
    width: 40px; height: 40px; border-radius: 50%; overflow: hidden;
    display: inline-flex; align-items: center; justify-content: center;
    background: var(--vo-surface);
}
.vo-avatar img { width: 100%; height: 100%; object-fit: cover; }

.vo-icon { display: inline-flex; align-items: center; justify-content: center; }

/* =============================================================================
   Button
   ============================================================================= */

button {
    display: inline-flex; align-items: center; justify-content: center; gap: 6px;
    padding: 8px 16px; border-radius: var(--vo-radius);
    font-size: 0.875rem; font-weight: 500; cursor: pointer;
    border: 1px solid var(--vo-border); background: var(--vo-bg); color: var(--vo-text);
    transition: all 0.15s ease;
}
button:hover { background: var(--vo-surface); }
button:active { transform: scale(0.98); }
button:disabled { opacity: 0.5; cursor: not-allowed; }

button[data-variant="primary"] {
    background: var(--vo-primary); color: white; border-color: var(--vo-primary);
}
button[data-variant="primary"]:hover { filter: brightness(1.1); }

button[data-variant="danger"] {
    background: var(--vo-danger); color: white; border-color: var(--vo-danger);
}

button[data-variant="outline"] {
    background: transparent; border-color: var(--vo-border); color: var(--vo-text);
}
button[data-variant="outline"]:hover { background: var(--vo-surface); }

button[data-variant="ghost"] {
    background: transparent; border-color: transparent; color: var(--vo-text);
}
button[data-variant="ghost"]:hover { background: var(--vo-surface); }

.vo-icon-btn { padding: 8px; min-width: 36px; min-height: 36px; }

/* =============================================================================
   Inputs
   ============================================================================= */

input, textarea, select {
    padding: 8px 12px; border: 1px solid var(--vo-border); border-radius: var(--vo-radius);
    font-size: 0.875rem; color: var(--vo-text); background: var(--vo-bg);
    font-family: inherit; width: 100%;
    transition: border-color 0.15s ease;
}
input:focus, textarea:focus, select:focus {
    outline: none; border-color: var(--vo-primary);
    box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
}
input:disabled, textarea:disabled, select:disabled {
    opacity: 0.5; cursor: not-allowed; background: var(--vo-surface);
}

textarea { resize: vertical; min-height: 80px; }

/* Checkbox & Switch */
.vo-checkbox, .vo-switch {
    display: inline-flex; align-items: center; gap: 8px; cursor: pointer;
    font-size: 0.875rem; user-select: none;
}

.vo-radio {
    display: inline-flex; align-items: center; gap: 8px; cursor: pointer;
    font-size: 0.875rem;
}

/* Slider */
.vo-slider { display: flex; align-items: center; gap: 8px; }
.vo-slider input[type="range"] { flex: 1; }

/* =============================================================================
   Containers
   ============================================================================= */

.vo-card {
    background: var(--vo-bg); border: 1px solid var(--vo-border);
    border-radius: var(--vo-radius); padding: 16px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.05);
}
.vo-panel {
    background: var(--vo-bg); border: 1px solid var(--vo-border);
    border-radius: var(--vo-radius); padding: 16px;
}

/* =============================================================================
   Dialog
   ============================================================================= */

.vo-dialog {
    border: none; border-radius: var(--vo-radius);
    padding: 0; max-width: 500px; width: 90%;
    box-shadow: 0 20px 60px rgba(0,0,0,0.3);
}
.vo-dialog::backdrop { background: rgba(0,0,0,0.5); }
.vo-dialog-title {
    font-size: 1.125rem; font-weight: 600; padding: 16px 20px;
    border-bottom: 1px solid var(--vo-border);
}
.vo-dialog-content { padding: 20px; }
.vo-dialog-actions {
    display: flex; justify-content: flex-end; gap: 8px;
    padding: 12px 20px; border-top: 1px solid var(--vo-border);
}

/* =============================================================================
   Drawer
   ============================================================================= */

.vo-drawer {
    position: fixed; top: 0; bottom: 0; background: var(--vo-bg);
    box-shadow: -4px 0 20px rgba(0,0,0,0.15); z-index: 1000;
    transition: transform 0.3s ease; overflow-y: auto;
    width: 320px; padding: 16px;
}
.vo-drawer[data-side="right"] { right: 0; transform: translateX(100%); }
.vo-drawer[data-side="left"] { left: 0; transform: translateX(-100%); }
.vo-drawer.open { transform: translateX(0); }

/* =============================================================================
   Tooltip
   ============================================================================= */

.vo-tooltip { position: relative; display: inline-flex; }

/* =============================================================================
   Dropdown Menu
   ============================================================================= */

.vo-dropdown-menu { position: relative; display: inline-flex; }
.vo-menu-item {
    padding: 8px 12px; cursor: pointer; font-size: 0.875rem;
    display: flex; align-items: center; gap: 8px;
    transition: background 0.1s;
}
.vo-menu-item:hover { background: var(--vo-surface); }
.vo-menu-item.disabled { opacity: 0.5; cursor: not-allowed; }
.vo-menu-divider { border: none; border-top: 1px solid var(--vo-border); margin: 4px 0; }

/* =============================================================================
   Tabs
   ============================================================================= */

.vo-tabs { display: flex; flex-direction: column; }

/* =============================================================================
   Accordion
   ============================================================================= */

.vo-accordion { display: flex; flex-direction: column; }

/* =============================================================================
   Form
   ============================================================================= */

form { display: flex; flex-direction: column; gap: 16px; }
.vo-form-field { display: flex; flex-direction: column; gap: 4px; }
.vo-form-field > label { font-size: 0.875rem; font-weight: 500; }
.vo-form-error { color: var(--vo-danger); font-size: 0.8rem; }
.vo-form-help { color: var(--vo-text-muted); font-size: 0.8rem; }
.vo-form-section { display: flex; flex-direction: column; gap: 12px; }
.vo-form-section > h3 { font-size: 1rem; font-weight: 600; }

/* =============================================================================
   Navigation
   ============================================================================= */

.vo-nav-item, .vo-nav-link {
    padding: 8px 12px; text-decoration: none; color: var(--vo-text-muted);
    font-size: 0.875rem; border-radius: var(--vo-radius);
    transition: all 0.15s; cursor: pointer;
}
.vo-nav-item:hover, .vo-nav-link:hover { background: var(--vo-surface); color: var(--vo-text); }
.vo-nav-item.active { color: var(--vo-primary); background: rgba(59,130,246,0.08); }

.vo-sidebar {
    display: flex; flex-direction: column; gap: 2px;
    width: 240px; padding: 8px;
}
.vo-sidebar-item {
    display: flex; align-items: center; gap: 8px;
    padding: 8px 12px; border-radius: var(--vo-radius);
    text-decoration: none; color: var(--vo-text-muted);
    font-size: 0.875rem; cursor: pointer; transition: all 0.15s;
}
.vo-sidebar-item:hover { background: var(--vo-surface); color: var(--vo-text); }
.vo-sidebar-item.active { background: rgba(59,130,246,0.08); color: var(--vo-primary); }
.vo-sidebar-section {
    padding: 16px 12px 4px;
    font-size: 0.75rem; font-weight: 600; text-transform: uppercase;
    color: var(--vo-text-muted); letter-spacing: 0.05em;
}

/* =============================================================================
   Breadcrumb, Pagination, Steps
   ============================================================================= */

.vo-breadcrumb { display: flex; align-items: center; gap: 4px; font-size: 0.875rem; }
.vo-pagination { display: flex; align-items: center; gap: 4px; }
.vo-steps { display: flex; align-items: center; gap: 8px; }

/* =============================================================================
   Portal Containers
   ============================================================================= */

.vo-portal { position: fixed; z-index: 9999; pointer-events: none; }
.vo-portal > * { pointer-events: auto; }
.vo-portal-toast { top: 16px; right: 16px; display: flex; flex-direction: column; gap: 8px; }
.vo-portal-notifications { top: 16px; right: 16px; display: flex; flex-direction: column; gap: 8px; }

/* =============================================================================
   Collapsible
   ============================================================================= */

.vo-collapsible { }
.vo-hover-card { position: relative; display: inline-flex; }
.vo-combobox { position: relative; }
.vo-context-menu { position: relative; }

/* =============================================================================
   Transition Classes (built-in)
   ============================================================================= */

.vo-enter-from-fade { opacity: 0; }
.vo-enter-active-fade { transition: opacity 0.2s ease; }
.vo-leave-active-fade { transition: opacity 0.15s ease; }
.vo-leave-to-fade { opacity: 0; }

.vo-enter-from-slide-down { opacity: 0; transform: translateY(-10px); }
.vo-enter-active-slide-down { transition: all 0.2s ease; }
.vo-leave-active-slide-down { transition: all 0.15s ease; }
.vo-leave-to-slide-down { opacity: 0; transform: translateY(-10px); }

.vo-enter-from-slide-up { opacity: 0; transform: translateY(10px); }
.vo-enter-active-slide-up { transition: all 0.2s ease; }
.vo-leave-active-slide-up { transition: all 0.15s ease; }
.vo-leave-to-slide-up { opacity: 0; transform: translateY(10px); }

.vo-enter-from-scale { opacity: 0; transform: scale(0.95); }
.vo-enter-active-scale { transition: all 0.2s ease; }
.vo-leave-active-scale { transition: all 0.15s ease; }
.vo-leave-to-scale { opacity: 0; transform: scale(0.95); }

/* =============================================================================
   Utility Classes
   ============================================================================= */

/* Spacing: p-0 through p-12 (0-48px, step 4px) */
.p-0 { padding: 0; } .p-1 { padding: 4px; } .p-2 { padding: 8px; }
.p-3 { padding: 12px; } .p-4 { padding: 16px; } .p-5 { padding: 20px; }
.p-6 { padding: 24px; } .p-8 { padding: 32px; } .p-10 { padding: 40px; }
.p-12 { padding: 48px; }

.px-0 { padding-left: 0; padding-right: 0; }
.px-1 { padding-left: 4px; padding-right: 4px; }
.px-2 { padding-left: 8px; padding-right: 8px; }
.px-3 { padding-left: 12px; padding-right: 12px; }
.px-4 { padding-left: 16px; padding-right: 16px; }
.px-6 { padding-left: 24px; padding-right: 24px; }
.px-8 { padding-left: 32px; padding-right: 32px; }

.py-0 { padding-top: 0; padding-bottom: 0; }
.py-1 { padding-top: 4px; padding-bottom: 4px; }
.py-2 { padding-top: 8px; padding-bottom: 8px; }
.py-3 { padding-top: 12px; padding-bottom: 12px; }
.py-4 { padding-top: 16px; padding-bottom: 16px; }
.py-6 { padding-top: 24px; padding-bottom: 24px; }

.m-0 { margin: 0; } .m-1 { margin: 4px; } .m-2 { margin: 8px; }
.m-4 { margin: 16px; } .m-auto { margin: auto; }
.mx-auto { margin-left: auto; margin-right: auto; }

.gap-0 { gap: 0; } .gap-1 { gap: 4px; } .gap-2 { gap: 8px; }
.gap-3 { gap: 12px; } .gap-4 { gap: 16px; } .gap-6 { gap: 24px; }
.gap-8 { gap: 32px; }

/* Layout */
.flex { display: flex; }
.flex-col { flex-direction: column; }
.flex-row { flex-direction: row; }
.flex-wrap { flex-wrap: wrap; }
.flex-1 { flex: 1; }
.flex-none { flex: none; }
.items-center { align-items: center; }
.items-start { align-items: flex-start; }
.items-end { align-items: flex-end; }
.items-stretch { align-items: stretch; }
.justify-center { justify-content: center; }
.justify-between { justify-content: space-between; }
.justify-end { justify-content: flex-end; }
.justify-start { justify-content: flex-start; }

/* Sizing */
.w-full { width: 100%; }
.h-full { height: 100%; }
.min-h-screen { min-height: 100vh; }
.w-1\\/2 { width: 50%; }
.w-1\\/3 { width: 33.333%; }
.w-2\\/3 { width: 66.667%; }

/* Text */
.text-xs { font-size: 0.75rem; }
.text-sm { font-size: 0.875rem; }
.text-base { font-size: 1rem; }
.text-lg { font-size: 1.125rem; }
.text-xl { font-size: 1.25rem; }
.text-2xl { font-size: 1.5rem; }
.text-3xl { font-size: 1.875rem; }
.font-bold { font-weight: 700; }
.font-semibold { font-weight: 600; }
.font-medium { font-weight: 500; }
.font-normal { font-weight: 400; }
.text-center { text-align: center; }
.text-right { text-align: right; }
.text-left { text-align: left; }
.truncate { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.line-clamp-2 { display: -webkit-box; -webkit-line-clamp: 2; -webkit-box-orient: vertical; overflow: hidden; }
.line-clamp-3 { display: -webkit-box; -webkit-line-clamp: 3; -webkit-box-orient: vertical; overflow: hidden; }

/* Display */
.hidden { display: none; }
.block { display: block; }
.inline { display: inline; }
.inline-block { display: inline-block; }
.inline-flex { display: inline-flex; }

/* Overflow */
.overflow-hidden { overflow: hidden; }
.overflow-auto { overflow: auto; }
.overflow-scroll { overflow: scroll; }

/* Position */
.relative { position: relative; }
.absolute { position: absolute; }
.fixed { position: fixed; }
.sticky { position: sticky; }
.top-0 { top: 0; } .right-0 { right: 0; }
.bottom-0 { bottom: 0; } .left-0 { left: 0; }
.inset-0 { top: 0; right: 0; bottom: 0; left: 0; }

/* Border */
.border { border: 1px solid var(--vo-border); }
.border-t { border-top: 1px solid var(--vo-border); }
.border-b { border-bottom: 1px solid var(--vo-border); }
.border-none { border: none; }
.rounded { border-radius: var(--vo-radius); }
.rounded-lg { border-radius: 8px; }
.rounded-xl { border-radius: 12px; }
.rounded-full { border-radius: 9999px; }

/* Cursor */
.cursor-pointer { cursor: pointer; }
.cursor-default { cursor: default; }
.cursor-not-allowed { cursor: not-allowed; }

/* Z-index */
.z-10 { z-index: 10; }
.z-20 { z-index: 20; }
.z-50 { z-index: 50; }

/* Responsive (sm: 640px, md: 768px, lg: 1024px) */
@media (min-width: 640px) {
    .sm\\:hidden { display: none; }
    .sm\\:block { display: block; }
    .sm\\:flex { display: flex; }
    .sm\\:flex-row { flex-direction: row; }
}
@media (min-width: 768px) {
    .md\\:hidden { display: none; }
    .md\\:block { display: block; }
    .md\\:flex { display: flex; }
    .md\\:flex-row { flex-direction: row; }
    .md\\:w-64 { width: 16rem; }
}
@media (min-width: 1024px) {
    .lg\\:hidden { display: none; }
    .lg\\:block { display: block; }
    .lg\\:flex { display: flex; }
    .lg\\:grid-cols-2 { grid-template-columns: repeat(2, 1fr); }
    .lg\\:grid-cols-3 { grid-template-columns: repeat(3, 1fr); }
    .lg\\:grid-cols-4 { grid-template-columns: repeat(4, 1fr); }
}

/* =============================================================================
   Canvas
   ============================================================================= */

.vo-canvas { display: block; }

/* =============================================================================
   External Widget
   ============================================================================= */

.vo-external-widget { display: block; }

/* =============================================================================
   Table
   ============================================================================= */

table { width: 100%; border-collapse: collapse; font-size: 0.875rem; }
th { text-align: left; padding: 8px 12px; font-weight: 600; border-bottom: 2px solid var(--vo-border); }
td { padding: 8px 12px; border-bottom: 1px solid var(--vo-border); }
tbody tr:hover { background: var(--vo-surface); }
`;

let stylesInjected = false;

/** Inject VoGUI default styles and managed component styles into the document head. */
export function injectStyles(): void {
    if (stylesInjected) return;
    const style = document.createElement('style');
    style.id = 'vogui-styles';
    style.textContent = voguiStyles;
    document.head.appendChild(style);
    injectManagedStyles();
    stylesInjected = true;
}

/** Apply a theme by setting CSS custom properties on :root. */
export function applyTheme(theme: Record<string, string>): void {
    const root = document.documentElement;
    const map: Record<string, string> = {
        Primary: '--vo-primary', Secondary: '--vo-secondary',
        Success: '--vo-success', Danger: '--vo-danger',
        Warning: '--vo-warning', Info: '--vo-info',
        Background: '--vo-bg', Surface: '--vo-surface',
        Text: '--vo-text', TextMuted: '--vo-text-muted',
        Border: '--vo-border', Radius: '--vo-radius',
        FontFamily: '--vo-font-family',
    };
    for (const [key, value] of Object.entries(theme)) {
        const prop = map[key];
        if (prop && value) {
            root.style.setProperty(prop, value);
        }
    }
}

export { voguiStyles };

const managedStyles = `
/* =============================================================================
   Managed Component Styles
   ============================================================================= */

/* Tabs */
.vo-tabs { display: flex; flex-direction: column; }
.vo-tabs-header {
    display: flex; flex-direction: row; border-bottom: 2px solid var(--vo-border);
    gap: 0;
}
.vo-tab {
    background: none; border: none; cursor: pointer;
    padding: 8px 16px; font-size: 0.9rem; color: var(--vo-text-muted);
    border-bottom: 2px solid transparent; margin-bottom: -2px;
    transition: color 0.15s, border-color 0.15s;
}
.vo-tab:hover { color: var(--vo-text); }
.vo-tab.active { color: var(--vo-primary); border-bottom-color: var(--vo-primary); font-weight: 500; }
.vo-tabs-content { padding: 16px 0; }
.vo-tab-panel { }

/* Accordion */
.vo-accordion { display: flex; flex-direction: column; border: 1px solid var(--vo-border); border-radius: var(--vo-radius); overflow: hidden; }
.vo-accordion-item { border-bottom: 1px solid var(--vo-border); }
.vo-accordion-item:last-child { border-bottom: none; }
.vo-accordion-header {
    display: flex; justify-content: space-between; align-items: center;
    width: 100%; background: none; border: none; cursor: pointer;
    padding: 12px 16px; font-size: 0.9rem; font-weight: 500;
    color: var(--vo-text); text-align: left;
    transition: background 0.15s;
}
.vo-accordion-header:hover { background: var(--vo-surface); }
.vo-accordion-item.open .vo-accordion-header { color: var(--vo-primary); }
.vo-accordion-chevron { font-size: 0.7rem; color: var(--vo-text-muted); }
.vo-accordion-panel { padding: 0 16px 12px; }

/* Breadcrumb */
.vo-breadcrumb ol { display: flex; align-items: center; list-style: none; flex-wrap: wrap; gap: 4px; }
.vo-breadcrumb-item a { color: var(--vo-primary); text-decoration: none; font-size: 0.875rem; }
.vo-breadcrumb-item a:hover { text-decoration: underline; }
.vo-breadcrumb-item.current { color: var(--vo-text-muted); font-size: 0.875rem; }
.vo-breadcrumb-sep { color: var(--vo-text-muted); font-size: 0.75rem; user-select: none; }

/* Pagination */
.vo-pagination { display: flex; align-items: center; gap: 4px; }
.vo-page-btn {
    background: none; border: 1px solid var(--vo-border); cursor: pointer;
    padding: 6px 10px; border-radius: var(--vo-radius); font-size: 0.875rem;
    color: var(--vo-text); transition: background 0.15s, border-color 0.15s;
    min-width: 36px;
}
.vo-page-btn:hover:not(:disabled) { background: var(--vo-surface); border-color: var(--vo-primary); }
.vo-page-btn.active { background: var(--vo-primary); border-color: var(--vo-primary); color: #fff; }
.vo-page-btn:disabled { opacity: 0.4; cursor: not-allowed; }
.vo-page-ellipsis { padding: 6px 4px; color: var(--vo-text-muted); font-size: 0.875rem; }

/* Steps */
.vo-steps { display: flex; gap: 0; }
.vo-step {
    display: flex; flex-direction: column; align-items: center;
    flex: 1; gap: 6px; position: relative;
}
.vo-step:not(:last-child)::after {
    content: ''; position: absolute; top: 16px; left: 50%; width: 100%;
    height: 2px; background: var(--vo-border); z-index: 0;
}
.vo-step.completed::after { background: var(--vo-primary); }
.vo-step-indicator {
    width: 32px; height: 32px; border-radius: 50%; border: 2px solid var(--vo-border);
    display: flex; align-items: center; justify-content: center;
    font-size: 0.8rem; font-weight: 600; background: var(--vo-bg);
    position: relative; z-index: 1; color: var(--vo-text-muted);
}
.vo-step.completed .vo-step-indicator { background: var(--vo-primary); border-color: var(--vo-primary); color: #fff; }
.vo-step.active .vo-step-indicator { border-color: var(--vo-primary); color: var(--vo-primary); }
.vo-step-label { font-size: 0.8rem; font-weight: 500; color: var(--vo-text-muted); text-align: center; }
.vo-step.active .vo-step-label, .vo-step.completed .vo-step-label { color: var(--vo-text); }
.vo-step-description { font-size: 0.75rem; color: var(--vo-text-muted); text-align: center; }

/* Checkbox */
.vo-checkbox {
    display: inline-flex; align-items: center; gap: 8px;
    cursor: pointer; user-select: none; font-size: 0.9rem;
}
.vo-checkbox input[type="checkbox"] {
    width: 16px; height: 16px; accent-color: var(--vo-primary); cursor: pointer;
}
.vo-checkbox input:disabled { cursor: not-allowed; opacity: 0.6; }

/* Switch */
.vo-switch {
    display: inline-flex; align-items: center; gap: 10px;
    cursor: pointer; user-select: none; font-size: 0.9rem;
}
.vo-switch input[type="checkbox"] { display: none; }
.vo-switch-track {
    position: relative; width: 40px; height: 22px; border-radius: 11px;
    background: var(--vo-border); transition: background 0.2s;
}
.vo-switch input:checked + .vo-switch-track { background: var(--vo-primary); }
.vo-switch-thumb {
    position: absolute; top: 3px; left: 3px;
    width: 16px; height: 16px; border-radius: 50%;
    background: #fff; transition: transform 0.2s;
    box-shadow: 0 1px 3px rgba(0,0,0,0.2);
}
.vo-switch input:checked + .vo-switch-track .vo-switch-thumb { transform: translateX(18px); }
.vo-switch-label { }

/* Radio */
.vo-radio {
    display: inline-flex; align-items: center; gap: 8px;
    cursor: pointer; user-select: none; font-size: 0.9rem;
}
.vo-radio input[type="radio"] {
    width: 16px; height: 16px; accent-color: var(--vo-primary); cursor: pointer;
}
.vo-radio input:disabled { cursor: not-allowed; opacity: 0.6; }

/* Slider */
.vo-slider { display: flex; align-items: center; width: 100%; }
.vo-slider input[type="range"] {
    width: 100%; accent-color: var(--vo-primary); cursor: pointer; height: 4px;
}

/* Tooltip (CSS hover-driven) */
.vo-tooltip { position: relative; display: inline-block; }
.vo-tooltip-content {
    position: absolute; bottom: calc(100% + 6px); left: 50%; transform: translateX(-50%);
    background: #1e293b; color: #fff; padding: 4px 10px; border-radius: 4px;
    font-size: 0.8rem; white-space: nowrap; pointer-events: none;
    opacity: 0; transition: opacity 0.15s; z-index: 1000;
}
.vo-tooltip:hover .vo-tooltip-content { opacity: 1; }

/* HoverCard (CSS hover-driven) */
.vo-hover-card { position: relative; display: inline-block; }
.vo-hover-card-trigger { display: inline-block; }
.vo-hover-card-content {
    position: absolute; top: calc(100% + 8px); left: 0; z-index: 1000;
    background: var(--vo-bg); border: 1px solid var(--vo-border); border-radius: var(--vo-radius);
    box-shadow: 0 4px 20px rgba(0,0,0,0.12); padding: 12px;
    min-width: 200px; opacity: 0; pointer-events: none;
    transform: translateY(-4px); transition: opacity 0.15s, transform 0.15s;
}
.vo-hover-card:hover .vo-hover-card-content { opacity: 1; pointer-events: auto; transform: translateY(0); }

/* Popover */
.vo-popover { position: relative; display: inline-block; }
.vo-popover-trigger { display: inline-block; cursor: pointer; }
.vo-popover-content {
    position: absolute; top: calc(100% + 6px); left: 0; z-index: 1000;
    background: var(--vo-bg); border: 1px solid var(--vo-border); border-radius: var(--vo-radius);
    box-shadow: 0 4px 20px rgba(0,0,0,0.12); padding: 12px; min-width: 180px;
    display: none;
}
.vo-popover.open .vo-popover-content { display: block; }

/* Dropdown Menu */
.vo-dropdown-menu { position: relative; display: inline-block; }
.vo-dropdown-trigger { display: inline-block; cursor: pointer; }
.vo-dropdown-list {
    position: absolute; top: calc(100% + 4px); left: 0; z-index: 1000;
    background: var(--vo-bg); border: 1px solid var(--vo-border); border-radius: var(--vo-radius);
    box-shadow: 0 4px 20px rgba(0,0,0,0.12); min-width: 160px;
    display: none; padding: 4px 0;
}
.vo-dropdown-menu.open .vo-dropdown-list { display: block; }
.vo-menu-item {
    padding: 8px 14px; cursor: pointer; font-size: 0.875rem;
    display: flex; align-items: center; gap: 8px;
    transition: background 0.1s;
}
.vo-menu-item:hover:not(.disabled) { background: var(--vo-surface); }
.vo-menu-item.disabled { opacity: 0.5; cursor: not-allowed; }
.vo-menu-divider { border: none; border-top: 1px solid var(--vo-border); margin: 4px 0; }

/* Collapsible */
.vo-collapsible { }
.vo-collapsible-trigger { cursor: pointer; }
.vo-collapsible-panel { }

/* Combobox */
.vo-combobox { position: relative; display: inline-block; }
.vo-combobox-input {
    width: 100%; padding: 8px 12px; border: 1px solid var(--vo-border);
    border-radius: var(--vo-radius); font-size: 0.875rem; outline: none;
    background: var(--vo-bg);
}
.vo-combobox-input:focus { border-color: var(--vo-primary); box-shadow: 0 0 0 2px rgba(59,130,246,0.15); }
.vo-combobox-listbox {
    position: absolute; top: calc(100% + 4px); left: 0; right: 0; z-index: 1000;
    background: var(--vo-bg); border: 1px solid var(--vo-border); border-radius: var(--vo-radius);
    box-shadow: 0 4px 20px rgba(0,0,0,0.12); max-height: 200px; overflow-y: auto;
}
.vo-combobox-option {
    padding: 8px 12px; cursor: pointer; font-size: 0.875rem;
    transition: background 0.1s;
}
.vo-combobox-option:hover { background: var(--vo-surface); }

/* Context Menu */
.vo-context-menu { }
.vo-context-menu-trigger { }
.vo-context-menu-list {
    position: fixed; z-index: 9999;
    background: var(--vo-bg); border: 1px solid var(--vo-border); border-radius: var(--vo-radius);
    box-shadow: 0 4px 20px rgba(0,0,0,0.15); min-width: 150px; padding: 4px 0;
    display: none;
}
.vo-context-menu.open .vo-context-menu-list { display: block; }
`;

export function injectManagedStyles(): void {
    if (document.getElementById('vogui-managed-styles')) return;
    const el = document.createElement('style');
    el.id = 'vogui-managed-styles';
    el.textContent = managedStyles;
    document.head.appendChild(el);
}
