// VoGUI v2 Renderer - Converts VoNode tree to DOM using morphdom.

import morphdom from 'morphdom';
import { VoNode, VoHandler, RenderMessage, RendererConfig, StylePropertyMap, toCssValue, WidgetFactory, WidgetInstance } from './types';

// =============================================================================
// State
// =============================================================================

let currentGen = 0;
let currentHandlers: VoHandler[] = [];
let currentConfig: RendererConfig | null = null;

// Event emit helper — guards optional onEvent
function emit(handlerId: number, payload: string): void {
    currentConfig?.onEvent?.(handlerId, payload);
}

// Ref registry: named DOM references
const refRegistry = new Map<string, HTMLElement>();

// Widget registry and instances
const widgetRegistry = new Map<string, WidgetFactory>();
const widgetInstances = new Map<string, WidgetInstance>();

// Portal containers
const portalContainers = new Map<string, HTMLElement>();

// =============================================================================
// Public API
// =============================================================================

/** Register an external widget factory. */
export function registerWidget(name: string, factory: WidgetFactory): void {
    widgetRegistry.set(name, factory);
}

/** Get a ref element by name. */
export function getRef(name: string): HTMLElement | null {
    return refRegistry.get(name) ?? null;
}

/** Destroy all widget instances. */
export function destroyAllWidgets(): void {
    widgetInstances.forEach((instance) => instance.destroy?.());
    widgetInstances.clear();
}

/** Main render function. Called with container, parsed RenderMessage, and RendererConfig. */
export function render(
    container: HTMLElement,
    msg: RenderMessage | null,
    config: RendererConfig
): void {
    if (!msg || !msg.tree) {
        container.innerHTML = '';
        return;
    }
    _renderTree(container, msg.tree, msg.gen, msg.handlers || [], config);
}

/** Setup a global keyboard handler that forwards key events to the event callback. */
export function setupKeyHandler(config: RendererConfig): () => void {
    const handler = (event: KeyboardEvent) => {
        const target = event.target as HTMLElement;
        const tag = target.tagName;
        if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT') return;
        if (!config.onEvent) return;
        // Prevent browser default (scrolling, etc.) for navigation/game keys
        if (event.key === 'ArrowUp' || event.key === 'ArrowDown' ||
            event.key === 'ArrowLeft' || event.key === 'ArrowRight' ||
            event.key === ' ' || event.key === 'PageUp' || event.key === 'PageDown') {
            event.preventDefault();
        }
        emit(-2, JSON.stringify({ key: event.key }));
    };
    document.addEventListener('keydown', handler, { capture: true });
    return () => document.removeEventListener('keydown', handler, { capture: true });
}

function _renderTree(
    container: HTMLElement,
    tree: VoNode,
    gen: number,
    handlers: VoHandler[],
    config: RendererConfig
): void {
    currentGen = gen;
    currentHandlers = handlers;
    currentConfig = config;

    const newEl = renderNode(tree);
    if (!newEl) return;

    if (!container.firstElementChild) {
        container.appendChild(newEl);
    } else {
        morphdom(container.firstElementChild, newEl, {
            onBeforeElUpdated(fromEl, toEl) {
                // Preserve external widget containers
                if (fromEl.hasAttribute('data-widget-id')) {
                    const widgetId = fromEl.getAttribute('data-widget-id')!;
                    const instance = widgetInstances.get(widgetId);
                    if (instance?.update) {
                        const propsStr = toEl.getAttribute('data-widget-props');
                        if (propsStr) {
                            try { instance.update(JSON.parse(propsStr)); } catch {}
                        }
                    }
                    return false; // don't touch widget DOM
                }

                // Preserve canvas elements
                if (fromEl.tagName === 'CANVAS' && toEl.tagName === 'CANVAS') {
                    // Update attributes but preserve canvas context
                    const w = toEl.getAttribute('width');
                    const h = toEl.getAttribute('height');
                    if (w) fromEl.setAttribute('width', w);
                    if (h) fromEl.setAttribute('height', h);
                    return false;
                }

                return true;
            },
            onNodeDiscarded(node) {
                if (node instanceof HTMLElement) {
                    // Clean up refs
                    const refName = node.dataset.ref;
                    if (refName) refRegistry.delete(refName);

                    // Clean up widgets
                    const widgetId = node.getAttribute('data-widget-id');
                    if (widgetId) {
                        widgetInstances.get(widgetId)?.destroy?.();
                        widgetInstances.delete(widgetId);
                    }
                }
            },
            getNodeKey(node) {
                if (node instanceof HTMLElement) {
                    return node.getAttribute('data-key') || undefined;
                }
                return undefined;
            },
        });
    }
}

// =============================================================================
// Node Rendering
// =============================================================================

function renderNode(node: VoNode): HTMLElement | Text | null {
    if (!node || !node.type) return null;

    const type = node.type;
    const props = node.props || {};
    const children = node.children || [];

    // Fragment: render children only
    if (type === 'Fragment') {
        const frag = document.createElement('div');
        frag.style.display = 'contents';
        for (const child of children) {
            const el = renderNode(child);
            if (el) frag.appendChild(el);
        }
        return frag;
    }

    // Portal: render into named container
    if (type === 'vo-portal') {
        const portalName = props['portalName'] as string;
        const container = getOrCreatePortal(portalName);
        // Create a placeholder in the normal tree
        const placeholder = document.createElement('div');
        placeholder.style.display = 'none';
        placeholder.setAttribute('data-portal', portalName);
        // Render children into portal container
        container.innerHTML = '';
        for (const child of children) {
            const el = renderNode(child);
            if (el) container.appendChild(el);
        }
        return placeholder;
    }

    // UnsafeHTML
    if (type === 'vo-unsafe-html') {
        const div = document.createElement('div');
        div.innerHTML = props['html'] as string || '';
        applyCommonProps(div, props);
        return div;
    }

    // External Widget
    if (type === 'vo-external-widget') {
        return renderExternalWidget(props);
    }

    // Canvas
    if (type === 'Canvas') {
        return renderCanvas(props);
    }

    // Create the DOM element
    const el = createElementForType(type, props);
    if (!el) return null;

    // Apply properties
    applyProps(el, type, props);

    // Apply common props (class, style, ref, key, attrs, events)
    applyCommonProps(el, props);

    // Render children
    for (const child of children) {
        const childEl = renderNode(child);
        if (childEl) el.appendChild(childEl);
    }

    return el;
}

// =============================================================================
// Element Creation
// =============================================================================

function createElementForType(type: string, props: Record<string, any>): HTMLElement | null {
    switch (type) {
        // Standard HTML elements
        case 'div': return document.createElement('div');
        case 'button': return document.createElement('button');
        case 'input': return document.createElement('input');
        case 'textarea': return document.createElement('textarea');
        case 'select': return document.createElement('select');
        case 'form': return document.createElement('form');
        case 'a': return document.createElement('a');
        case 'img': return document.createElement('img');
        case 'video': return document.createElement('video');
        case 'h1': return document.createElement('h1');
        case 'h2': return document.createElement('h2');
        case 'h3': return document.createElement('h3');
        case 'h4': return document.createElement('h4');
        case 'h5': return document.createElement('h5');
        case 'h6': return document.createElement('h6');
        case 'p': return document.createElement('p');
        case 'code': return document.createElement('code');
        case 'pre': return document.createElement('pre');
        case 'strong': return document.createElement('strong');
        case 'em': return document.createElement('em');
        case 'ul': return document.createElement('ul');
        case 'ol': return document.createElement('ol');
        case 'li': return document.createElement('li');
        case 'table': return document.createElement('table');
        case 'thead': return document.createElement('thead');
        case 'tbody': return document.createElement('tbody');
        case 'tr': return document.createElement('tr');
        case 'td': return document.createElement('td');
        case 'th': return document.createElement('th');
        case 'nav': return document.createElement('nav');

        // VoGUI components (rendered as div with class)
        case 'vo-text': return createVoElement('span', 'vo-text');
        case 'vo-row': return createVoElement('div', 'vo-row');
        case 'vo-column': return createVoElement('div', 'vo-column');
        case 'vo-center': return createVoElement('div', 'vo-center');
        case 'vo-stack': return createVoElement('div', 'vo-stack');
        case 'vo-grid': return createVoElement('div', 'vo-grid');
        case 'vo-spacer': return createVoElement('div', 'vo-spacer');
        case 'vo-divider': return createVoElement('hr', 'vo-divider');
        case 'vo-scroll': return createVoElement('div', 'vo-scroll');
        case 'vo-wrap': return createVoElement('div', 'vo-wrap');
        case 'vo-badge': return createVoElement('span', 'vo-badge');
        case 'vo-tag': return createVoElement('span', 'vo-tag');
        case 'vo-progress': return createVoElement('div', 'vo-progress');
        case 'vo-spinner': return createVoElement('div', 'vo-spinner');
        case 'vo-alert': return createVoElement('div', 'vo-alert');
        case 'vo-avatar': return createVoElement('div', 'vo-avatar');
        case 'vo-icon': return createVoElement('span', 'vo-icon');
        case 'vo-card': return createVoElement('div', 'vo-card');
        case 'vo-panel': return createVoElement('div', 'vo-panel');
        case 'vo-slider': return createVoElement('div', 'vo-slider');
        case 'vo-checkbox': return createVoElement('label', 'vo-checkbox');
        case 'vo-switch': return createVoElement('label', 'vo-switch');
        case 'vo-radio': return createVoElement('label', 'vo-radio');
        case 'vo-form-field': return createVoElement('div', 'vo-form-field');
        case 'vo-form-error': return createVoElement('div', 'vo-form-error');
        case 'vo-form-help': return createVoElement('div', 'vo-form-help');
        case 'vo-form-section': return createVoElement('div', 'vo-form-section');
        case 'vo-nav-item': return createVoElement('a', 'vo-nav-item');
        case 'vo-nav-link': return createVoElement('a', 'vo-nav-link');
        case 'vo-nav-divider': return createVoElement('hr', 'vo-nav-divider');
        case 'vo-nav-group': return createVoElement('div', 'vo-nav-group');
        case 'vo-sidebar': return createVoElement('aside', 'vo-sidebar');
        case 'vo-sidebar-item': return createVoElement('a', 'vo-sidebar-item');
        case 'vo-sidebar-section': return createVoElement('div', 'vo-sidebar-section');

        // Managed components
        case 'vo-dialog': return createVoElement('dialog', 'vo-dialog');
        case 'vo-dialog-title': return createVoElement('h2', 'vo-dialog-title');
        case 'vo-dialog-content': return createVoElement('div', 'vo-dialog-content');
        case 'vo-dialog-actions': return createVoElement('div', 'vo-dialog-actions');
        case 'vo-drawer': return createVoElement('div', 'vo-drawer');
        case 'vo-tooltip': return createVoElement('div', 'vo-tooltip');
        case 'vo-popover': return createVoElement('div', 'vo-popover');
        case 'vo-dropdown-menu': return createVoElement('div', 'vo-dropdown-menu');
        case 'vo-menu-item': return createVoElement('div', 'vo-menu-item');
        case 'vo-menu-divider': return createVoElement('hr', 'vo-menu-divider');
        case 'vo-hover-card': return createVoElement('div', 'vo-hover-card');
        case 'vo-collapsible': return createVoElement('div', 'vo-collapsible');
        case 'vo-combobox': return createVoElement('div', 'vo-combobox');
        case 'vo-combobox-option': return createVoElement('div', 'vo-combobox-option');
        case 'vo-context-menu': return createVoElement('div', 'vo-context-menu');
        case 'vo-tabs': return createVoElement('div', 'vo-tabs');
        case 'vo-accordion': return createVoElement('div', 'vo-accordion');
        case 'vo-breadcrumb': return createVoElement('nav', 'vo-breadcrumb');
        case 'vo-pagination': return createVoElement('nav', 'vo-pagination');
        case 'vo-steps': return createVoElement('div', 'vo-steps');
        case 'vo-transition': return createVoElement('div', 'vo-transition');
        case 'vo-transition-group': return createVoElement('div', 'vo-transition-group');

        default:
            console.warn(`VoGUI: unknown node type "${type}"`);
            return document.createElement('div');
    }
}

function createVoElement(tag: string, className: string): HTMLElement {
    const el = document.createElement(tag);
    el.classList.add(className);
    return el;
}

// =============================================================================
// Property Application
// =============================================================================

function applyProps(el: HTMLElement, type: string, props: Record<string, any>): void {
    // Text content
    if (props['textContent'] != null) {
        el.textContent = String(props['textContent']);
    }

    // Input-specific props
    if (el instanceof HTMLInputElement) {
        if (props['type']) el.type = props['type'];
        if (props['value'] != null) el.value = String(props['value']);
        if (props['placeholder']) el.placeholder = props['placeholder'];
        if (props['disabled']) el.disabled = true;
        if (props['readOnly']) el.readOnly = true;
    }
    if (el instanceof HTMLTextAreaElement) {
        if (props['value'] != null) el.value = String(props['value']);
        if (props['placeholder']) el.placeholder = props['placeholder'];
        if (props['rows']) el.rows = Number(props['rows']);
        if (props['disabled']) el.disabled = true;
    }

    // Select element
    if (el instanceof HTMLSelectElement && props['options']) {
        const options = props['options'] as Array<{Label: string, Value: string}>;
        for (const opt of options) {
            const optEl = document.createElement('option');
            optEl.value = opt.Value;
            optEl.textContent = opt.Label;
            if (opt.Value === props['value']) optEl.selected = true;
            el.appendChild(optEl);
        }
    }

    // Link props
    if (el instanceof HTMLAnchorElement) {
        if (props['href']) el.href = props['href'];
    }

    // Image props
    if (el instanceof HTMLImageElement) {
        if (props['src']) el.src = props['src'];
        if (props['alt']) el.alt = props['alt'];
    }

    // Video props
    if (el instanceof HTMLVideoElement) {
        if (props['src']) el.src = props['src'];
    }

    // Grid cols
    if (type === 'vo-grid' && props['cols']) {
        el.style.gridTemplateColumns = `repeat(${props['cols']}, 1fr)`;
    }

    // Progress bar
    if (type === 'vo-progress') {
        const value = props['value'] || 0;
        const max = props['max'] || 100;
        const pct = Math.round((value / max) * 100);
        el.setAttribute('data-value', String(value));
        el.setAttribute('data-max', String(max));
        const bar = document.createElement('div');
        bar.classList.add('vo-progress-bar');
        bar.style.width = `${pct}%`;
        el.appendChild(bar);
    }

    // Alert type
    if (type === 'vo-alert' && props['alertType']) {
        el.setAttribute('data-type', props['alertType']);
    }

    // Avatar
    if (type === 'vo-avatar' && props['src']) {
        const img = document.createElement('img');
        img.src = props['src'];
        el.appendChild(img);
    }

    // Variant
    if (props['variant']) {
        el.setAttribute('data-variant', props['variant']);
    }

    // Label (form fields)
    if (type === 'vo-form-field' && props['label']) {
        const label = document.createElement('label');
        label.textContent = props['label'];
        el.insertBefore(label, el.firstChild);
    }

    // Title (form section)
    if (type === 'vo-form-section' && props['title']) {
        const h = document.createElement('h3');
        h.textContent = props['title'];
        el.insertBefore(h, el.firstChild);
    }

    // Active state
    if (props['active']) {
        el.classList.add('active');
    }

    // Disabled state
    if (props['disabled'] && !(el instanceof HTMLInputElement) && !(el instanceof HTMLTextAreaElement)) {
        el.classList.add('disabled');
        el.setAttribute('aria-disabled', 'true');
    }

    // Icon
    if (props['icon'] && type !== 'vo-icon') {
        const icon = document.createElement('span');
        icon.classList.add('vo-icon');
        icon.setAttribute('data-icon', props['icon']);
        el.insertBefore(icon, el.firstChild);
    }
    if (type === 'vo-icon' && props['name']) {
        el.setAttribute('data-icon', props['name']);
    }

    // Dialog management (P6: Vo props authoritative)
    if (type === 'vo-dialog' && el instanceof HTMLDialogElement) {
        if (props['open']) {
            if (!el.open) el.showModal();
        } else {
            if (el.open) el.close();
        }
    }

    // Drawer
    if (type === 'vo-drawer') {
        if (props['side']) el.setAttribute('data-side', props['side']);
        if (props['open']) el.classList.add('open');
    }

    // Collapsible
    if (type === 'vo-collapsible' && props['defaultOpen']) {
        el.classList.add('open');
    }

    // Attach event handlers
    attachHandlers(el, props);
}

// =============================================================================
// Common Props (class, style, ref, key, attrs, events)
// =============================================================================

function applyCommonProps(el: HTMLElement, props: Record<string, any>): void {
    // CSS classes
    if (props['class']) {
        const classes = (props['class'] as string).split(' ').filter(c => c);
        for (const cls of classes) {
            el.classList.add(cls);
        }
    }

    // Inline styles
    if (props['style'] && typeof props['style'] === 'object') {
        const style = props['style'] as Record<string, any>;
        for (const [key, val] of Object.entries(style)) {
            const cssProp = StylePropertyMap[key] || key;
            el.style.setProperty(cssProp, toCssValue(val, cssProp));
        }
    }

    // Ref
    if (props['ref']) {
        const refName = props['ref'] as string;
        el.dataset.ref = refName;
        refRegistry.set(refName, el);
    }

    // Key
    if (props['key']) {
        el.setAttribute('data-key', String(props['key']));
    }

    // HTML attributes (ARIA, data-*, role, etc.)
    if (props['attrs'] && typeof props['attrs'] === 'object') {
        const attrs = props['attrs'] as Record<string, any>;
        for (const [key, val] of Object.entries(attrs)) {
            if (typeof val === 'boolean') {
                if (val) el.setAttribute(key, '');
                else el.removeAttribute(key);
            } else {
                el.setAttribute(key, String(val));
            }
        }
    }

    // Arbitrary DOM events via node.On(event, handler)
    if (props['events'] && typeof props['events'] === 'object') {
        const events = props['events'] as Record<string, any>;
        for (const [eventName, handler] of Object.entries(events)) {
            if (handler && typeof handler === 'object' && 'iD' in handler) {
                const h = handler as VoHandler;
                attachEventListener(el, eventName, h);
            }
        }
    }

    // Transition
    if (props['transition']) {
        el.setAttribute('data-transition', props['transition']);
    }
}

// =============================================================================
// Event Handling
// =============================================================================

function attachHandlers(el: HTMLElement, props: Record<string, any>): void {
    const config = currentConfig;
    if (!config) return;

    // onClick
    if (props['onClick'] != null) {
        const handlerId = props['onClick'] as number;
        el.addEventListener('click', (e) => {
            emit(handlerId, '{}');
        });
    }

    // onChange (for inputs)
    if (props['onChange'] != null) {
        const handlerId = props['onChange'] as number;
        const handler = currentHandlers[handlerId];

        if (el instanceof HTMLInputElement) {
            if (el.type === 'checkbox') {
                el.addEventListener('change', () => {
                    emit(handlerId, JSON.stringify({ Checked: el.checked }));
                });
            } else {
                el.addEventListener('input', () => {
                    emit(handlerId, JSON.stringify({ Value: el.value }));
                });
            }
        } else if (el instanceof HTMLTextAreaElement) {
            el.addEventListener('input', () => {
                emit(handlerId, JSON.stringify({ Value: el.value }));
            });
        } else if (el instanceof HTMLSelectElement) {
            el.addEventListener('change', () => {
                emit(handlerId, JSON.stringify({ Value: el.value }));
            });
        }
    }

    // onSubmit (for forms)
    if (props['onSubmit'] != null) {
        const handlerId = props['onSubmit'] as number;
        el.addEventListener('submit', (e) => {
            e.preventDefault();
            emit(handlerId, '{}');
        });
    }

    // onClose (for dialog)
    if (props['onClose'] != null) {
        const handlerId = props['onClose'] as number;
        if (el instanceof HTMLDialogElement) {
            el.addEventListener('close', () => {
                emit(handlerId, '{}');
            });
            // Backdrop click
            el.addEventListener('click', (e) => {
                if (e.target === el) {
                    emit(handlerId, '{}');
                }
            });
        }
    }

    // onFiles
    if (props['onFiles'] != null) {
        const handlerId = props['onFiles'] as number;
        if (el instanceof HTMLInputElement && el.type === 'file') {
            el.addEventListener('change', () => {
                const files: any[] = [];
                if (el.files) {
                    for (let i = 0; i < el.files.length; i++) {
                        const f = el.files[i];
                        files.push({ Name: f.name, Size: f.size, Type: f.type, Data: '' });
                    }
                }
                emit(handlerId, JSON.stringify({ Files: files }));
            });
        }
    }

    // onSelect (for select change)
    if (props['onSelect'] != null && !(el instanceof HTMLSelectElement)) {
        const handlerId = props['onSelect'] as number;
        // Used by Combobox and similar managed components
        el.setAttribute('data-on-select', String(handlerId));
    }
}

function attachEventListener(el: HTMLElement, eventName: string, handler: VoHandler): void {
    const config = currentConfig;
    if (!config) return;

    let listener = (e: Event) => {
        // Key filter
        if (handler.keyFilter && e instanceof KeyboardEvent) {
            if (e.key !== handler.keyFilter) return;
        }

        // Modifiers
        if (handler.modifiers) {
            for (const mod of handler.modifiers) {
                if (mod === 'prevent') e.preventDefault();
                if (mod === 'stop') e.stopPropagation();
            }
        }

        emit(handler.iD, '{}');
    };

    // Apply debounce/throttle modifiers
    if (handler.modifiers) {
        for (const mod of handler.modifiers) {
            if (mod.startsWith('debounce:')) {
                const ms = parseInt(mod.split(':')[1]);
                listener = debounce(listener, ms);
            }
            if (mod.startsWith('throttle:')) {
                const ms = parseInt(mod.split(':')[1]);
                listener = throttle(listener, ms);
            }
        }
    }

    const options: AddEventListenerOptions = {};
    if (handler.modifiers?.includes('once')) {
        options.once = true;
    }

    el.addEventListener(eventName, listener, options);
}

// =============================================================================
// External Widget Rendering
// =============================================================================

function renderExternalWidget(props: Record<string, any>): HTMLElement {
    const widgetType = props['widgetType'] as string;
    const widgetId = `widget-${widgetType}-${Math.random().toString(36).slice(2, 8)}`;

    const container = document.createElement('div');
    container.classList.add('vo-external-widget');
    container.setAttribute('data-widget-id', widgetId);
    container.setAttribute('data-widget-type', widgetType);
    container.setAttribute('data-widget-props', JSON.stringify(props));
    // Apply style, class, key, etc. from the Vo node (e.g. .Flex(1), .H("100%"))
    applyCommonProps(container, props);

    // Build the onEvent callback the widget can call to emit events back to Vo
    const onWidgetEvent = (payload: string) => {
        if (props['onWidget'] != null) {
            emit(props['onWidget'] as number, payload);
        }
    };
    (container as any).__voWidgetCallback = onWidgetEvent;

    const factory = widgetRegistry.get(widgetType);
    if (factory) {
        const instance = factory.create(container, props, onWidgetEvent);
        widgetInstances.set(widgetId, instance);
    }

    return container;
}

// =============================================================================
// Canvas Rendering
// =============================================================================

function renderCanvas(props: Record<string, any>): HTMLElement {
    const canvas = document.createElement('canvas');
    canvas.classList.add('vo-canvas');
    if (props['width']) canvas.width = props['width'];
    if (props['height']) canvas.height = props['height'];
    if (props['fullscreen']) {
        canvas.style.width = '100%';
        canvas.style.height = '100%';
    }
    applyCommonProps(canvas, props);
    return canvas;
}

// =============================================================================
// Portal Manager
// =============================================================================

function getOrCreatePortal(name: string): HTMLElement {
    let container = portalContainers.get(name);
    if (!container) {
        container = document.createElement('div');
        container.id = `vo-portal-${name}`;
        container.className = `vo-portal vo-portal-${name}`;
        document.body.appendChild(container);
        portalContainers.set(name, container);
    }
    return container;
}

// =============================================================================
// Utility Functions
// =============================================================================

function debounce(fn: Function, ms: number): any {
    let timer: any;
    return (...args: any[]) => {
        clearTimeout(timer);
        timer = setTimeout(() => fn(...args), ms);
    };
}

function throttle(fn: Function, ms: number): any {
    let last = 0;
    return (...args: any[]) => {
        const now = Date.now();
        if (now - last >= ms) {
            last = now;
            fn(...args);
        }
    };
}

export { renderNode };
