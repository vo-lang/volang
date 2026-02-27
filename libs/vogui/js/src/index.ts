// VoGUI v2 JavaScript Runtime - Public API

export type {
    VoNode,
    VoHandler,
    RenderMessage,
    EventCallback,
    RendererConfig,
    WidgetFactory,
    WidgetInstance,
    CanvasCommand,
    CanvasBatch,
} from './types';

export { render, renderNode, setupKeyHandler, registerWidget, getRef, destroyAllWidgets } from './renderer';
export { injectStyles, applyTheme, voguiStyles } from './styles';
export { executeCanvasBatch } from './canvas';
export { decodeBinaryRender } from './decoder';
