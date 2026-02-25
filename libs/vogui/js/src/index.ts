// VoGUI JavaScript/TypeScript runtime
// Provides DOM rendering for VoGUI virtual node trees

export type { VoNode, EventCallback, RendererConfig } from './types';
export { StylePropertyMap } from './types';
export { voguiStyles, injectStyles } from './styles';
export { render, renderNode, styleToString, setupKeyHandler, registerWidget, destroyAllWidgets } from './renderer';
export type { WidgetFactory, WidgetInstance } from './renderer';
