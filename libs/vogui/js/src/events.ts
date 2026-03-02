// Event bridge: connects Preact component events to Vo handler dispatch.

import type { VoHandler, RendererConfig } from './types';

let currentGen = 0;
let currentHandlers: VoHandler[] = [];
let currentConfig: RendererConfig | null = null;

/** Update the render context for event dispatch. Called on each render(). */
export function setRenderContext(gen: number, handlers: VoHandler[], config: RendererConfig): void {
    currentGen = gen;
    currentHandlers = handlers;
    currentConfig = config;
}

/** Emit an event to Vo. */
export function emit(handlerId: number, payload: string): void {
    currentConfig?.onEvent?.(handlerId, payload);
}

/** Get handler metadata by ID. */
export function getHandler(id: number): VoHandler | undefined {
    if (id < 0 || id >= currentHandlers.length) return undefined;
    return currentHandlers[id];
}

/** Get current render generation. */
export function getRenderGen(): number {
    return currentGen;
}

// =========================================================================
// Debounce / Throttle utilities
// =========================================================================

function debounce<T extends (...args: any[]) => void>(fn: T, ms: number): T {
    let timer: any;
    return ((...args: any[]) => {
        clearTimeout(timer);
        timer = setTimeout(() => fn(...args), ms);
    }) as any;
}

function throttle<T extends (...args: any[]) => void>(fn: T, ms: number): T {
    let last = 0;
    return ((...args: any[]) => {
        const now = Date.now();
        if (now - last >= ms) {
            last = now;
            fn(...args);
        }
    }) as any;
}

// =========================================================================
// Wrap a raw listener with VoHandler modifiers
// =========================================================================

function wrapWithModifiers(fn: (e: Event) => void, handler: VoHandler): (e: Event) => void {
    const hasOnce = handler.modifiers?.includes('once');
    let fired = false;

    let wrapped = (e: Event) => {
        // Once guard
        if (hasOnce && fired) return;

        // Key filter
        if (handler.keyFilter && e instanceof KeyboardEvent) {
            if (e.key !== handler.keyFilter) return;
        }
        // Modifier side-effects
        if (handler.modifiers) {
            for (const mod of handler.modifiers) {
                if (mod === 'prevent') e.preventDefault();
                if (mod === 'stop') e.stopPropagation();
            }
        }
        fn(e);
        if (hasOnce) fired = true;
    };

    // Debounce / throttle
    if (handler.modifiers) {
        for (const mod of handler.modifiers) {
            if (mod.startsWith('debounce:')) {
                const ms = parseInt(mod.split(':')[1], 10);
                wrapped = debounce(wrapped, ms);
            }
            if (mod.startsWith('throttle:')) {
                const ms = parseInt(mod.split(':')[1], 10);
                wrapped = throttle(wrapped, ms);
            }
        }
    }

    return wrapped;
}

/**
 * Setup a global keyboard handler that forwards key events to Vo's
 * global key handler (handler ID -2). Skips events from form inputs.
 * Returns a cleanup function.
 */
export function setupKeyHandler(config: RendererConfig): () => void {
    const handler = (event: KeyboardEvent) => {
        const target = event.target as HTMLElement;
        const tag = target.tagName;
        if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT') return;
        if (!config.onEvent) return;
        // Prevent browser default for navigation/game keys
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

/**
 * Extract a generic payload from a DOM event for OnEvent handlers.
 * Captures commonly useful properties based on the event type.
 *
 * Payload schema (all fields optional, only present when relevant):
 *   Type:     string  — DOM event type ("scroll", "focus", "mousedown", etc.)
 *   Value:    string  — target.value for form elements
 *   Checked:  bool    — target.checked for checkboxes/radios
 *   Key:      string  — key name for keyboard events
 *   ClientX:  float64 — mouse/pointer X relative to viewport
 *   ClientY:  float64 — mouse/pointer Y relative to viewport
 *   Detail:   int     — click count or custom detail
 */
function extractGenericPayload(e: Event): string {
    const payload: Record<string, any> = { Type: e.type };
    const target = e.target as any;
    if (target) {
        if ('value' in target && target.value !== undefined) payload.Value = String(target.value);
        if ('checked' in target && typeof target.checked === 'boolean') payload.Checked = target.checked;
    }
    if (e instanceof KeyboardEvent) {
        payload.Key = e.key;
    }
    if (e instanceof MouseEvent) {
        payload.ClientX = e.clientX;
        payload.ClientY = e.clientY;
    }
    if ('detail' in e && typeof (e as any).detail === 'number') {
        payload.Detail = (e as any).detail;
    }
    return JSON.stringify(payload);
}

/**
 * Convert VoNode event props (onClick, onChange, onSubmit, etc.) into
 * Preact-compatible event handler functions.
 *
 * Also processes the `events` prop from node.On("eventName", handler)
 * which contains VoHandler objects with keyFilter, modifiers, etc.
 */
export function propsToHandlers(props: Record<string, any>): Record<string, any> {
    const handlers: Record<string, any> = {};

    // onClick
    if (props.onClick != null) {
        const id = props.onClick;
        handlers.onClick = (e: Event) => {
            e.stopPropagation();
            emit(id, '{}');
        };
    }

    // onChange — for input/textarea emits Value, for select emits Value too
    if (props.onChange != null) {
        const id = props.onChange;
        handlers.onInput = (e: Event) => {
            const target = e.target as HTMLInputElement;
            // Checkbox/radio use 'change' not 'input', but those are handled by
            // their Radix components. For text inputs we use onInput.
            emit(id, JSON.stringify({ Value: target.value ?? '' }));
        };
        // Also bind actual 'change' for <select> elements (Preact maps onChange → onInput
        // for inputs but select uses onChange)
        handlers.onChange = (e: Event) => {
            const target = e.target as HTMLSelectElement;
            if (target.tagName === 'SELECT') {
                emit(id, JSON.stringify({ Value: target.value ?? '' }));
            }
        };
    }

    // onSubmit
    if (props.onSubmit != null) {
        const id = props.onSubmit;
        handlers.onSubmit = (e: Event) => {
            e.preventDefault();
            emit(id, '{}');
        };
    }

    // onClose (dialog/drawer)
    if (props.onClose != null) {
        // Handled by VgDialog/VgDrawer components, but store for generic access
        handlers['data-vo-close'] = props.onClose;
    }

    // onFiles
    if (props.onFiles != null) {
        const id = props.onFiles;
        // For file inputs, bind to onChange (not onInput)
        const prevOnChange = handlers.onChange;
        handlers.onChange = (e: Event) => {
            const input = e.target as HTMLInputElement;
            if (input.type === 'file' && input.files) {
                const arr: any[] = [];
                for (let i = 0; i < input.files.length; i++) {
                    const f = input.files[i];
                    arr.push({ Name: f.name, Size: f.size, Type: f.type, Data: '' });
                }
                emit(id, JSON.stringify({ Files: arr }));
                return;
            }
            // Fall through to previous onChange if not a file input
            if (prevOnChange) prevOnChange(e);
        };
    }

    // onSelect (combobox)
    if (props.onSelect != null) {
        handlers['data-vo-select'] = props.onSelect;
    }

    // Generic DOM event bindings via node.On("eventName", handler)
    // props.events = { eventName: VoHandler, ... }
    if (props.events && typeof props.events === 'object') {
        const events = props.events as Record<string, any>;
        for (const [eventName, handler] of Object.entries(events)) {
            if (handler && typeof handler === 'object' && 'iD' in handler) {
                const h = handler as VoHandler;
                const preactName = 'on' + eventName.charAt(0).toUpperCase() + eventName.slice(1);
                const baseFn = (e: Event) => {
                    emit(h.iD, extractGenericPayload(e));
                };
                handlers[preactName] = wrapWithModifiers(baseFn, h);
            }
        }
    }

    return handlers;
}
