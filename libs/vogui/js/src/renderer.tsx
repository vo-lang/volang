// VoGUI v4 Preact Renderer — core rendering engine.
// Converts VoNode tree → Preact VNode tree → DOM via Preact reconciler.

import { h, render as preactRender, type ComponentChildren } from 'preact';
import { useRef, useEffect } from 'preact/hooks';

import type { VoNode, RenderMessage, RendererConfig, CanvasBatch, WidgetFactory, WidgetInstance } from './types';
import { setRenderContext, propsToHandlers, emit } from './events';
import { typeToTag, typeToBaseClass, variantClass, propsToStyle } from './mapping';
import { refCallback, refRegistry } from './refs';
import { applyTheme, injectDynamicStyles } from './styles';
import { executeCanvasBatch } from './canvas';
import { componentMap } from './components/index';

// =============================================================================
// Component memo cache for __comp__ / __cached__ optimization
// =============================================================================

const compCache = new Map<number, any>();

// =============================================================================
// External Widget Registry
// =============================================================================

const widgetRegistry = new Map<string, WidgetFactory>();
const widgetInstances = new Map<string, WidgetInstance>();

/** Register an external widget factory. */
export function registerWidget(type: string, factory: WidgetFactory): void {
    widgetRegistry.set(type, factory);
}

/** Unregister and destroy all widget instances. */
export function destroyWidgets(): void {
    for (const [, instance] of widgetInstances) {
        instance.destroy?.();
    }
    widgetInstances.clear();
}

// =============================================================================
// Public API
// =============================================================================

/** Render a decoded VoNode tree into a container element. */
export function render(container: HTMLElement, msg: RenderMessage, config: RendererConfig): void {
    if (!msg?.tree) {
        preactRender(null, container);
        return;
    }

    setRenderContext(msg.gen, msg.handlers, config);
    applyTheme(container, msg.theme);

    if (msg.styles) {
        injectDynamicStyles(msg.styles);
    }

    preactRender(
        h(VoTreeRoot, { tree: msg.tree, canvas: msg.canvas }),
        container,
    );
}

// =============================================================================
// Root component
// =============================================================================

function VoTreeRoot({ tree, canvas }: { tree: VoNode; canvas?: CanvasBatch[] }): any {
    const containerRef = useRef<HTMLDivElement>(null);

    useEffect(() => {
        if (canvas && containerRef.current) {
            for (const batch of canvas) {
                executeCanvasBatch(batch, refRegistry);
            }
        }
    }, [canvas]);

    return h('div', { ref: containerRef, style: { display: 'contents' } }, voNodeToVNode(tree));
}

// =============================================================================
// VoNode → Preact VNode conversion
// =============================================================================

export function voNodeToVNode(node: VoNode | null | undefined): any {
    if (!node || !node.type) return null;

    const { type, props = {}, children = [] } = node;

    // Text node
    if (type === '#text') {
        return props.text != null ? String(props.text) : null;
    }

    // Fragment
    if (type === 'Fragment') {
        return h('div', { style: { display: 'contents' } }, childrenToVNodes(children));
    }

    // Component subtree markers
    if (type === '__comp__') {
        const cid = props._cid as number;
        const childVNode = children[0] ? voNodeToVNode(children[0]) : null;
        // Cache the rendered subtree
        const vnode = h('div', { 'data-vcid': cid, style: { display: 'contents' } }, childVNode);
        compCache.set(cid, vnode);
        return vnode;
    }
    if (type === '__cached__') {
        const cid = props._cid as number;
        const cached = compCache.get(cid);
        if (cached) return cached;
        return h('div', { 'data-vcid': cid });
    }

    // UnsafeHTML
    if (type === 'vo-unsafe-html') {
        const html = props.html as string || '';
        return h('div', {
            dangerouslySetInnerHTML: { __html: html },
            ...buildCommonProps(props),
        });
    }

    // Portal
    if (type === 'vo-portal') {
        // For now, render inline. Full createPortal support can be added later.
        return h('div', { style: { display: 'contents' } }, childrenToVNodes(children));
    }

    // Canvas
    if (type === 'Canvas') {
        return renderCanvas(props);
    }

    // External widget
    if (type === 'vo-external-widget') {
        return renderExternalWidget(props);
    }

    // Check component registry for managed components
    const Component = componentMap[type];
    if (Component) {
        return h(Component, {
            ...props,
            voChildren: children,
        });
    }

    // Generic element rendering
    return renderGenericElement(type, props, children);
}

function childrenToVNodes(children: VoNode[]): ComponentChildren[] {
    return children.map(voNodeToVNode);
}

// =============================================================================
// Generic element rendering
// =============================================================================

function renderGenericElement(type: string, props: Record<string, any>, children: VoNode[]): any {
    const tag = typeToTag(type);
    const baseClass = typeToBaseClass(type);
    const vClass = variantClass(type, props.variant, props.size);
    const userClass = props.class || '';

    // Active state classes
    const activeClass = props.active ? getActiveClass(type) : '';
    // Disabled state classes (non-input elements)
    const disabledClass = props.disabled && tag !== 'input' && tag !== 'textarea' && tag !== 'select'
        ? 'opacity-50 pointer-events-none' : '';

    const className = [baseClass, vClass, activeClass, disabledClass, userClass].filter(Boolean).join(' ') || undefined;
    const style = propsToStyle(props);
    const eventHandlers = propsToHandlers(props);
    const commonProps = buildCommonProps(props);

    // Grid cols
    const gridStyle = type === 'vo-grid' && props.cols
        ? { ...style, gridTemplateColumns: `repeat(${props.cols}, 1fr)` }
        : style;

    const elementProps: Record<string, any> = {
        ...commonProps,
        ...eventHandlers,
        className,
        style: gridStyle,
    };

    // Disabled ARIA on non-form elements
    if (props.disabled && tag !== 'input' && tag !== 'textarea' && tag !== 'select') {
        elementProps['aria-disabled'] = 'true';
    }

    // Text content — render inline when no children
    if (props.textContent != null && children.length === 0) {
        // Prepend icon if present
        const iconEl = renderIconProp(props, type);
        if (iconEl) {
            return h(tag, elementProps, iconEl, String(props.textContent));
        }
        return h(tag, elementProps, String(props.textContent));
    }

    // HTML-specific attributes
    if (tag === 'input') {
        applyInputProps(elementProps, props);
    }
    if (tag === 'textarea') {
        applyTextareaProps(elementProps, props);
    }
    if (tag === 'a') {
        if (props.href) elementProps.href = props.href;
    }
    if (tag === 'img') {
        if (props.src) elementProps.src = props.src;
        if (props.alt) elementProps.alt = props.alt;
    }
    if (tag === 'video') {
        if (props.src) elementProps.src = props.src;
    }

    // Form submit
    if (tag === 'form' && props.onSubmit != null) {
        elementProps.onSubmit = (e: Event) => {
            e.preventDefault();
            emit(props.onSubmit, '{}');
        };
    }

    // Label for form fields
    const labelContent = type === 'vo-form-field' && props.label
        ? [h('label', { className: 'text-sm font-medium text-foreground' }, props.label), ...childrenToVNodes(children)]
        : childrenToVNodes(children);

    // Section title
    const sectionContent = type === 'vo-form-section' && props.title
        ? [h('h3', { className: 'text-lg font-semibold' }, props.title), ...childrenToVNodes(children)]
        : null;

    let content = sectionContent || labelContent;

    // Prepend icon if present and has children
    const iconEl = renderIconProp(props, type);
    if (iconEl && Array.isArray(content)) {
        content = [iconEl, ...content];
    }

    // Progress bar special rendering
    if (type === 'vo-progress') {
        return renderProgress(elementProps, props);
    }

    // Avatar special rendering
    if (type === 'vo-avatar' && props.src) {
        return h(tag, elementProps,
            h('img', { src: props.src, className: 'aspect-square h-full w-full object-cover' }),
        );
    }

    return h(tag, elementProps, ...content);
}

/** Get the active-state class for a given node type. */
function getActiveClass(type: string): string {
    switch (type) {
        case 'vo-nav-item': case 'vo-sidebar-item':
            return 'bg-accent text-accent-foreground font-medium';
        default:
            return 'active';
    }
}

/** Render an icon element from the `icon` or `name` prop. */
function renderIconProp(props: Record<string, any>, type: string): any {
    if (type === 'vo-icon' && props.name) {
        return h('span', { className: 'vo-icon', 'data-icon': props.name });
    }
    if (props.icon && type !== 'vo-icon') {
        return h('span', { className: 'vo-icon mr-1.5 inline-flex items-center', 'data-icon': props.icon });
    }
    return null;
}

// =============================================================================
// Specialized renderers
// =============================================================================

function renderCanvas(props: Record<string, any>): any {
    const refName = props.ref as string;
    const w = props.width || 300;
    const hVal = props.height || 150;
    const common = buildCommonProps(props);
    const fullscreen = props.fullscreen;
    const pointerId = props.onPointer as number | undefined;
    const resizeId = props.onResize as number | undefined;

    const canvasStyle: Record<string, string> = {};
    if (fullscreen) {
        canvasStyle.width = '100%';
        canvasStyle.height = '100%';
    }

    const pointerHandlers: Record<string, any> = {};
    if (pointerId != null) {
        const emitPointer = (kind: string, e: PointerEvent) => {
            const rect = (e.currentTarget as HTMLCanvasElement).getBoundingClientRect();
            emit(pointerId, JSON.stringify({
                Kind: kind,
                X: e.clientX - rect.left,
                Y: e.clientY - rect.top,
                Button: e.button,
                Buttons: e.buttons,
            }));
        };
        pointerHandlers.onPointerDown = (e: PointerEvent) => emitPointer('down', e);
        pointerHandlers.onPointerUp = (e: PointerEvent) => emitPointer('up', e);
        pointerHandlers.onPointerMove = (e: PointerEvent) => emitPointer('move', e);
        pointerHandlers.onPointerEnter = (e: PointerEvent) => emitPointer('enter', e);
        pointerHandlers.onPointerLeave = (e: PointerEvent) => emitPointer('leave', e);
    }

    // ResizeObserver callback bound via ref
    const combinedRef = (el: HTMLCanvasElement | null) => {
        if (refName) refCallback(refName)(el);
        if (el && resizeId != null) {
            const observer = new ResizeObserver((entries) => {
                for (const entry of entries) {
                    const cr = entry.contentRect;
                    emit(resizeId, JSON.stringify({
                        Width: Math.round(cr.width),
                        Height: Math.round(cr.height),
                    }));
                }
            });
            observer.observe(el);
            (el as any).__voResizeObserver = observer;
        } else if (!el) {
            // Cleanup on unmount — el is null
        }
    };

    return h('canvas', {
        ...common,
        ...pointerHandlers,
        width: w,
        height: hVal,
        style: Object.keys(canvasStyle).length > 0 ? canvasStyle : undefined,
        ref: combinedRef,
    });
}

function renderExternalWidget(props: Record<string, any>): any {
    const widgetType = props.widgetType as string;
    const refName = `widget-${widgetType}-${Math.random().toString(36).slice(2, 8)}`;
    const common = buildCommonProps(props);
    const style = propsToStyle(props);

    return h('div', {
        ...common,
        className: 'vo-external-widget',
        style,
        'data-widget-type': widgetType,
        ref: (el: HTMLElement | null) => {
            if (!el) {
                const instance = widgetInstances.get(refName);
                if (instance) {
                    instance.destroy?.();
                    widgetInstances.delete(refName);
                }
                return;
            }
            // Create widget on mount
            const factory = widgetRegistry.get(widgetType);
            if (factory && !widgetInstances.has(refName)) {
                const onWidgetEvent = (payload: string) => {
                    if (props.onWidget != null) emit(props.onWidget as number, payload);
                };
                const instance = factory.create(el, props, onWidgetEvent);
                widgetInstances.set(refName, instance);
            }
        },
    });
}

function renderProgress(elementProps: Record<string, any>, props: Record<string, any>): any {
    const value = props.value || 0;
    const max = props.max || 100;
    const pct = Math.round((value / max) * 100);

    return h('div', {
        ...elementProps,
        className: [elementProps.className, 'relative h-2 w-full overflow-hidden rounded-full bg-muted'].filter(Boolean).join(' '),
    },
        h('div', {
            className: 'h-full bg-primary transition-all',
            style: { width: `${pct}%` },
        }),
    );
}

// =============================================================================
// HTML attribute helpers
// =============================================================================

function applyInputProps(elementProps: Record<string, any>, props: Record<string, any>): void {
    if (props.type) elementProps.type = props.type;
    if (props.value != null) elementProps.value = String(props.value);
    if (props.placeholder) elementProps.placeholder = props.placeholder;
    if (props.disabled) elementProps.disabled = true;
    if (props.readOnly) elementProps.readOnly = true;

    // Tailwind input styling
    elementProps.className = [
        elementProps.className,
        'flex h-9 w-full rounded-md border border-input bg-transparent px-3 py-1 text-sm shadow-sm',
        'transition-colors placeholder:text-muted-foreground',
        'focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring',
        'disabled:cursor-not-allowed disabled:opacity-50',
    ].filter(Boolean).join(' ');
}

function applyTextareaProps(elementProps: Record<string, any>, props: Record<string, any>): void {
    if (props.value != null) elementProps.value = String(props.value);
    if (props.placeholder) elementProps.placeholder = props.placeholder;
    if (props.rows) elementProps.rows = Number(props.rows);
    if (props.disabled) elementProps.disabled = true;

    elementProps.className = [
        elementProps.className,
        'flex min-h-[60px] w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm',
        'placeholder:text-muted-foreground',
        'focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring',
        'disabled:cursor-not-allowed disabled:opacity-50',
    ].filter(Boolean).join(' ');
}

// =============================================================================
// Common props (ref, key, data-attributes)
// =============================================================================

function buildCommonProps(props: Record<string, any>): Record<string, any> {
    const common: Record<string, any> = {};

    // Ref
    if (props.ref) {
        common.ref = refCallback(props.ref);
    }

    // Key
    if (props.key) {
        common.key = props.key;
        common['data-key'] = props.key;
    }

    // Variant
    if (props.variant) {
        common['data-variant'] = props.variant;
    }

    // Transition
    if (props.transition) {
        common['data-transition'] = props.transition;
    }

    // HTML attributes (ARIA, data-*, role, etc.) from node.Attrs()
    if (props.attrs && typeof props.attrs === 'object') {
        const attrs = props.attrs as Record<string, any>;
        for (const [key, val] of Object.entries(attrs)) {
            if (typeof val === 'boolean') {
                if (val) common[key] = '';
                // false → omit the attribute (Preact won't render it)
            } else {
                common[key] = String(val);
            }
        }
    }

    return common;
}
