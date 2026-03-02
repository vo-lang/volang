// VoNode type → HTML tag + Tailwind class mapping for layout and simple elements.

import { StylePropertyMap, toCssValue } from './types';

/** Map VoNode type to HTML tag name. */
export function typeToTag(type: string): string {
    switch (type) {
        // Standard HTML
        case 'div': return 'div';
        case 'span': return 'span';
        case 'button': return 'button';
        case 'input': return 'input';
        case 'textarea': return 'textarea';
        case 'select': return 'select';
        case 'form': return 'form';
        case 'a': return 'a';
        case 'img': return 'img';
        case 'video': return 'video';
        case 'h1': return 'h1';
        case 'h2': return 'h2';
        case 'h3': return 'h3';
        case 'h4': return 'h4';
        case 'h5': return 'h5';
        case 'h6': return 'h6';
        case 'p': return 'p';
        case 'code': return 'code';
        case 'pre': return 'pre';
        case 'strong': return 'strong';
        case 'em': return 'em';
        case 'ul': return 'ul';
        case 'ol': return 'ol';
        case 'li': return 'li';
        case 'table': return 'table';
        case 'thead': return 'thead';
        case 'tbody': return 'tbody';
        case 'tr': return 'tr';
        case 'td': return 'td';
        case 'th': return 'th';
        case 'nav': return 'nav';
        case 'hr': return 'hr';

        // VoGUI layout
        case 'vo-text': return 'span';
        case 'vo-row': return 'div';
        case 'vo-column': return 'div';
        case 'vo-center': return 'div';
        case 'vo-stack': return 'div';
        case 'vo-grid': return 'div';
        case 'vo-spacer': return 'div';
        case 'vo-divider': return 'hr';
        case 'vo-scroll': return 'div';
        case 'vo-wrap': return 'div';

        // VoGUI display
        case 'vo-badge': return 'span';
        case 'vo-tag': return 'span';
        case 'vo-progress': return 'div';
        case 'vo-spinner': return 'div';
        case 'vo-alert': return 'div';
        case 'vo-avatar': return 'div';
        case 'vo-icon': return 'span';
        case 'vo-card': return 'div';
        case 'vo-card-header': return 'div';
        case 'vo-card-body': return 'div';
        case 'vo-card-footer': return 'div';
        case 'vo-panel': return 'div';

        // VoGUI form
        case 'vo-form-field': return 'div';
        case 'vo-form-error': return 'div';
        case 'vo-form-help': return 'div';
        case 'vo-form-section': return 'div';

        // VoGUI nav
        case 'vo-nav-item': return 'a';
        case 'vo-nav-link': return 'a';
        case 'vo-nav-divider': return 'hr';
        case 'vo-nav-group': return 'div';
        case 'vo-sidebar': return 'aside';
        case 'vo-sidebar-item': return 'a';
        case 'vo-sidebar-section': return 'div';

        // VoGUI dialog / overlay sub-parts
        case 'vo-dialog-title': return 'h2';
        case 'vo-dialog-content': return 'div';
        case 'vo-dialog-actions': return 'div';

        // VoGUI menu sub-parts (used inside dropdown/context menus)
        case 'vo-menu-item': return 'div';
        case 'vo-menu-divider': return 'hr';

        // VoGUI combobox sub-parts
        case 'vo-combobox-option': return 'div';

        // VoGUI transitions
        case 'vo-transition': return 'div';
        case 'vo-transition-group': return 'div';

        default: return 'div';
    }
}

/** Map VoNode type to base Tailwind class string. */
export function typeToBaseClass(type: string): string {
    switch (type) {
        // Layout
        case 'vo-row': return 'flex flex-row';
        case 'vo-column': return 'flex flex-col';
        case 'vo-center': return 'flex items-center justify-center';
        case 'vo-stack': return 'relative';
        case 'vo-grid': return 'grid';
        case 'vo-spacer': return 'flex-1';
        case 'vo-divider': return 'border-t border-border my-2';
        case 'vo-scroll': return 'overflow-auto';
        case 'vo-wrap': return 'flex flex-wrap';

        // Display
        case 'vo-badge': return 'inline-flex items-center rounded-md border px-2 py-0.5 text-xs font-medium';
        case 'vo-tag': return 'inline-flex items-center rounded-full bg-muted px-2.5 py-0.5 text-xs font-medium text-muted-foreground';
        case 'vo-spinner': return 'inline-block h-5 w-5 animate-spin rounded-full border-2 border-current border-t-transparent';
        case 'vo-alert': return 'relative w-full rounded-lg border p-4';
        case 'vo-avatar': return 'relative flex h-10 w-10 shrink-0 overflow-hidden rounded-full';
        case 'vo-icon': return 'inline-flex items-center justify-center';
        case 'vo-card': return 'rounded-lg border border-border bg-card text-card-foreground shadow-sm';
        case 'vo-card-header': return 'flex flex-col gap-1.5 p-6 pb-4';
        case 'vo-card-body': return 'px-6 pb-4';
        case 'vo-card-footer': return 'flex items-center px-6 py-4 border-t border-border';
        case 'vo-panel': return 'rounded-lg border border-border bg-surface p-4';

        // Form
        case 'vo-form-field': return 'flex flex-col gap-1.5';
        case 'vo-form-error': return 'text-sm text-danger';
        case 'vo-form-help': return 'text-sm text-muted-foreground';
        case 'vo-form-section': return 'flex flex-col gap-4';

        // Nav
        case 'vo-nav-item': return 'inline-flex items-center px-3 py-2 text-sm font-medium rounded-md hover:bg-accent hover:text-accent-foreground';
        case 'vo-nav-link': return 'inline-flex items-center px-3 py-2 text-sm font-medium rounded-md hover:bg-accent hover:text-accent-foreground cursor-pointer';
        case 'vo-nav-divider': return 'border-t border-border my-1';
        case 'vo-nav-group': return 'flex flex-col gap-1';
        case 'vo-sidebar': return 'flex flex-col w-64 border-r border-border bg-surface h-full';
        case 'vo-sidebar-item': return 'flex items-center gap-2 px-3 py-2 text-sm rounded-md hover:bg-accent cursor-pointer';
        case 'vo-sidebar-section': return 'flex flex-col gap-0.5 px-2 py-2';

        // Dialog / overlay sub-parts
        case 'vo-dialog-title': return 'text-lg font-semibold leading-none tracking-tight';
        case 'vo-dialog-content': return 'mt-2 text-sm text-muted-foreground';
        case 'vo-dialog-actions': return 'mt-4 flex justify-end gap-2';

        // Menu sub-parts
        case 'vo-menu-item': return 'relative flex cursor-default select-none items-center rounded-sm px-2 py-1.5 text-sm outline-none hover:bg-accent hover:text-accent-foreground';
        case 'vo-menu-divider': return 'my-1 h-px bg-border';

        // Combobox option
        case 'vo-combobox-option': return 'relative flex cursor-default select-none items-center rounded-sm px-2 py-1.5 text-sm outline-none hover:bg-accent';

        // Transitions (CSS-based)
        case 'vo-transition': return 'transition-all';
        case 'vo-transition-group': return '';

        default: return '';
    }
}

/** Build variant-specific classes for components. */
export function variantClass(type: string, variant?: string, size?: string): string {
    // Alert variants
    if (type === 'vo-alert') {
        switch (variant) {
            case 'success': return 'border-success/50 text-success bg-success/10';
            case 'warning': return 'border-warning/50 text-warning bg-warning/10';
            case 'danger': case 'error': return 'border-danger/50 text-danger bg-danger/10';
            case 'info': return 'border-info/50 text-info bg-info/10';
            default: return 'border-border text-foreground';
        }
    }
    // Badge variants
    if (type === 'vo-badge') {
        switch (variant) {
            case 'primary': return 'border-transparent bg-primary text-primary-foreground';
            case 'secondary': return 'border-transparent bg-secondary text-secondary-foreground';
            case 'success': return 'border-transparent bg-success text-success-foreground';
            case 'warning': return 'border-transparent bg-warning text-warning-foreground';
            case 'danger': case 'destructive': case 'error': return 'border-transparent bg-danger text-danger-foreground';
            case 'info': return 'border-transparent bg-info text-info-foreground';
            case 'outline': return 'text-foreground border-border';
            default: return 'border-transparent bg-muted text-muted-foreground';
        }
    }
    // Sidebar item active
    if (type === 'vo-sidebar-item') {
        if (variant === 'active') return 'bg-accent text-accent-foreground font-medium';
    }
    // Nav item active
    if (type === 'vo-nav-item') {
        if (variant === 'active') return 'bg-accent text-accent-foreground';
    }
    return '';
}

/** Convert VoNode props.style map into a CSSProperties object for Preact. */
export function propsToStyle(props: Record<string, any>): Record<string, string> | undefined {
    const style = props.style;
    if (!style || typeof style !== 'object') return undefined;

    const css: Record<string, string> = {};
    for (const [key, val] of Object.entries(style)) {
        const cssProp = StylePropertyMap[key] || key;
        css[camelToCssProp(cssProp)] = toCssValue(val, cssProp);
    }
    return css;
}

/** Convert a CSS property like 'font-size' to camelCase 'fontSize' for inline style. */
function camelToCssProp(prop: string): string {
    // Preact accepts camelCase for style properties
    if (!prop.includes('-')) return prop;
    return prop.replace(/-([a-z])/g, (_, c) => c.toUpperCase());
}
