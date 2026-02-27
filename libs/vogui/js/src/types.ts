// VoGUI v2 TypeScript type definitions.

/** VoNode is the JSON representation of a Vo Node struct. */
export interface VoNode {
    type: string;
    props?: Record<string, any>;
    children?: VoNode[];
}

/** Handler metadata from Vo. */
export interface VoHandler {
    iD: number;
    gen: number;
    type: number;
    intVal: number;
    modifiers?: string[];
    keyFilter?: string;
}

/** Canvas draw command from Vo. */
export interface CanvasCommand {
    c: string;
    a?: any[];
}

/** Canvas command batch targeting a specific canvas ref. */
export interface CanvasBatch {
    ref: string;
    cmds: CanvasCommand[];
}

/** Render message from Vo. */
export interface RenderMessage {
    type: 'render';
    gen: number;
    tree: VoNode;
    handlers: VoHandler[];
    styles?: string[];
    canvas?: CanvasBatch[];
}

/** Event callback signature. */
export type EventCallback = (handlerId: number, payload: string) => void;

/** Renderer configuration. */
export interface RendererConfig {
    onEvent?: EventCallback;
}

/** External widget factory interface. */
export interface WidgetFactory {
    create(container: HTMLElement, props: Record<string, any>, onEvent: (payload: string) => void): WidgetInstance;
}

/** External widget instance interface. */
export interface WidgetInstance {
    update?(props: Record<string, any>): void;
    destroy?(): void;
}

/** Style property mapping from Vo camelCase to CSS property names. */
export const StylePropertyMap: Record<string, string> = {
    width: 'width',
    height: 'height',
    minWidth: 'min-width',
    maxWidth: 'max-width',
    minHeight: 'min-height',
    maxHeight: 'max-height',
    padding: 'padding',
    paddingTop: 'padding-top',
    paddingBottom: 'padding-bottom',
    paddingLeft: 'padding-left',
    paddingRight: 'padding-right',
    margin: 'margin',
    marginTop: 'margin-top',
    marginBottom: 'margin-bottom',
    marginLeft: 'margin-left',
    marginRight: 'margin-right',
    gap: 'gap',
    background: 'background',
    color: 'color',
    fontSize: 'font-size',
    fontWeight: 'font-weight',
    fontFamily: 'font-family',
    borderRadius: 'border-radius',
    border: 'border',
    boxShadow: 'box-shadow',
    opacity: 'opacity',
    overflow: 'overflow',
    cursor: 'cursor',
    flex: 'flex',
    display: 'display',
    position: 'position',
    top: 'top',
    right: 'right',
    bottom: 'bottom',
    left: 'left',
    zIndex: 'z-index',
    textAlign: 'text-align',
    textDecoration: 'text-decoration',
    letterSpacing: 'letter-spacing',
    lineHeight: 'line-height',
    whiteSpace: 'white-space',
    wordBreak: 'word-break',
    objectFit: 'object-fit',
    transition: 'transition',
    transform: 'transform',
    animation: 'animation',
    gridTemplateColumns: 'grid-template-columns',
    gridColumn: 'grid-column',
    gridRow: 'grid-row',
    alignItems: 'align-items',
    justifyContent: 'justify-content',
    flexDirection: 'flex-direction',
    flexWrap: 'flex-wrap',
    flexGrow: 'flex-grow',
    flexShrink: 'flex-shrink',
    maxLines: '-webkit-line-clamp',
};

/** CSS properties that must remain unitless (no 'px' suffix). */
const UNITLESS_PROPERTIES = new Set([
    'flex', 'flex-grow', 'flex-shrink', 'opacity', 'z-index', 'order',
    'line-height', 'font-weight', 'orphans', 'widows', 'columns',
    'column-count', 'tab-size', 'counter-increment', 'counter-reset',
]);

/** Convert a value to CSS string. Numbers get 'px' unless the property is unitless. */
export function toCssValue(val: any, cssProp?: string): string {
    if (typeof val === 'number') {
        if (cssProp && UNITLESS_PROPERTIES.has(cssProp)) return String(val);
        return `${val}px`;
    }
    return String(val);
}
