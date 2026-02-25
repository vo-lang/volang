// VoGUI DOM Renderer
// Framework-agnostic rendering of VoNode tree to DOM with morphdom

import morphdom from 'morphdom';
import { VoNode, RendererConfig, StylePropertyMap } from './types';

// =============================================================================
// Widget Registry
// =============================================================================

export interface WidgetFactory {
  create(container: HTMLElement, props: any, onEvent: (payload: string) => void): WidgetInstance;
}

export interface WidgetInstance {
  element: HTMLElement;
  update(props: any): void;
  destroy(): void;
}

const widgetRegistry = new Map<string, WidgetFactory>();
const widgetInstances = new Map<string, WidgetInstance>();

export function registerWidget(type: string, factory: WidgetFactory): void {
  widgetRegistry.set(type, factory);
}

export function destroyAllWidgets(): void {
  widgetInstances.forEach(inst => inst.destroy());
  widgetInstances.clear();
}

/** Convert Vo style object to CSS string */
export function styleToString(style: Record<string, any> | undefined): string {
  if (!style || typeof style !== 'object') return '';
  
  return Object.entries(style)
    .map(([key, value]) => {
      const cssProp = StylePropertyMap[key] || key;
      const cssValue = typeof value === 'number' ? `${value}px` : value;
      return `${cssProp}: ${cssValue}`;
    })
    .join('; ');
}

/** Create event handlers bound to config */
function createEventHandlers(config: RendererConfig) {
  const { interactive, onEvent } = config;
  
  return {
    handleClick(handlerId: number | undefined) {
      if (interactive && onEvent && handlerId !== undefined) {
        onEvent(handlerId, '{}');
      }
    },
    
    handleInput(handlerId: number | undefined, value: string) {
      if (interactive && onEvent && handlerId !== undefined) {
        onEvent(handlerId, JSON.stringify({ value }));
      }
    },
    
    handleChecked(handlerId: number | undefined, checked: boolean) {
      if (interactive && onEvent && handlerId !== undefined) {
        onEvent(handlerId, JSON.stringify({ checked }));
      }
    },
    
    handleSlider(handlerId: number | undefined, value: number) {
      if (interactive && onEvent && handlerId !== undefined) {
        onEvent(handlerId, JSON.stringify({ value }));
      }
    },
  };
}

/** Render a VoNode tree to DOM element */
export function renderNode(node: VoNode, config: RendererConfig, handlers?: ReturnType<typeof createEventHandlers>): HTMLElement | Text | null {
  const { interactive } = config;
  if (!handlers) handlers = createEventHandlers(config);
  const style = styleToString(node.props?.style);
  
  function renderChildren(parent: HTMLElement, children?: VoNode[]) {
    if (!children) return;
    for (const child of children) {
      const el = renderNode(child, config, handlers);
      if (el) parent.appendChild(el);
    }
  }
  
  switch (node.type) {
    // Layout
    case 'Column': {
      const el = document.createElement('div');
      el.className = 'vo-column';
      if (style) el.style.cssText = style;
      renderChildren(el, node.children);
      return el;
    }
    
    case 'Row': {
      const el = document.createElement('div');
      el.className = 'vo-row';
      if (style) el.style.cssText = style;
      renderChildren(el, node.children);
      return el;
    }
    
    case 'Center': {
      const el = document.createElement('div');
      el.className = 'vo-center';
      if (style) el.style.cssText = style;
      if (node.children?.[0]) {
        const child = renderNode(node.children[0], config);
        if (child) el.appendChild(child);
      }
      return el;
    }
    
    case 'Wrap': {
      const el = document.createElement('div');
      el.className = 'vo-wrap';
      if (style) el.style.cssText = style;
      renderChildren(el, node.children);
      return el;
    }
    
    case 'Grid': {
      const el = document.createElement('div');
      el.className = 'vo-grid';
      const cols = node.props?.cols ?? 2;
      el.style.cssText = `grid-template-columns: repeat(${cols}, 1fr); ${style}`;
      renderChildren(el, node.children);
      return el;
    }
    
    case 'GridItem': {
      const el = document.createElement('div');
      el.className = 'vo-grid-item';
      const span = node.props?.span ?? 1;
      el.style.cssText = `grid-column: span ${span}; ${style}`;
      if (node.children?.[0]) {
        const child = renderNode(node.children[0], config, handlers);
        if (child) el.appendChild(child);
      }
      return el;
    }
    
    case 'Stack': {
      const el = document.createElement('div');
      el.className = 'vo-stack';
      if (style) el.style.cssText = style;
      renderChildren(el, node.children);
      return el;
    }
    
    case 'Scroll': {
      const el = document.createElement('div');
      el.className = 'vo-scroll';
      if (style) el.style.cssText = style;
      if (node.children?.[0]) {
        const child = renderNode(node.children[0], config);
        if (child) el.appendChild(child);
      }
      return el;
    }
    
    case 'Fragment': {
      const el = document.createElement('div');
      el.style.display = 'contents';
      renderChildren(el, node.children);
      return el;
    }
    
    case 'Show': {
      const el = document.createElement('div');
      el.className = 'vo-show';
      el.style.cssText = `display: ${node.props?.visible ? 'contents' : 'none'}; ${style}`;
      if (node.children?.[0]) {
        const child = renderNode(node.children[0], config);
        if (child) el.appendChild(child);
      }
      return el;
    }
    
    case 'Block': {
      const el = document.createElement('div');
      el.className = 'vo-block';
      if (style) el.style.cssText = style;
      renderChildren(el, node.children);
      return el;
    }
    
    // Text
    case 'Text': {
      const el = document.createElement('span');
      el.className = 'vo-text';
      if (style) el.style.cssText = style;
      el.textContent = node.props?.content ?? '';
      return el;
    }
    
    case 'H1': {
      const el = document.createElement('h1');
      el.className = 'vo-h1';
      el.textContent = node.props?.text ?? '';
      return el;
    }
    
    case 'H2': {
      const el = document.createElement('h2');
      el.className = 'vo-h2';
      el.textContent = node.props?.text ?? '';
      return el;
    }
    
    case 'H3': {
      const el = document.createElement('h3');
      el.className = 'vo-h3';
      el.textContent = node.props?.text ?? '';
      return el;
    }
    
    case 'H4': {
      const el = document.createElement('h4');
      el.className = 'vo-h4';
      el.textContent = node.props?.text ?? '';
      return el;
    }
    
    case 'H5': {
      const el = document.createElement('h5');
      el.className = 'vo-h5';
      el.textContent = node.props?.text ?? '';
      return el;
    }
    
    case 'H6': {
      const el = document.createElement('h6');
      el.className = 'vo-h6';
      el.textContent = node.props?.text ?? '';
      return el;
    }
    
    case 'P': {
      const el = document.createElement('p');
      el.className = 'vo-p';
      el.textContent = node.props?.text ?? '';
      return el;
    }
    
    case 'Code': {
      const el = document.createElement('code');
      el.className = 'vo-code';
      el.textContent = node.props?.code ?? '';
      return el;
    }
    
    case 'Pre': {
      const el = document.createElement('pre');
      el.className = 'vo-pre';
      el.textContent = node.props?.code ?? '';
      return el;
    }
    
    case 'Strong': {
      const el = document.createElement('strong');
      el.textContent = node.props?.text ?? '';
      return el;
    }
    
    case 'Em': {
      const el = document.createElement('em');
      el.textContent = node.props?.text ?? '';
      return el;
    }
    
    case 'Link': {
      const el = document.createElement('a');
      el.className = 'vo-link';
      el.href = node.props?.href ?? '#';
      el.textContent = node.props?.text ?? '';
      return el;
    }
    
    // Display
    case 'Badge': {
      const el = document.createElement('span');
      el.className = 'vo-badge';
      el.textContent = node.props?.text ?? '';
      return el;
    }
    
    case 'Tag': {
      const el = document.createElement('span');
      el.className = 'vo-tag';
      el.textContent = node.props?.text ?? '';
      return el;
    }
    
    case 'Progress': {
      const el = document.createElement('div');
      el.className = 'vo-progress';
      const bar = document.createElement('div');
      bar.className = 'vo-progress-bar';
      bar.style.width = `${(node.props?.value ?? 0) * 100}%`;
      el.appendChild(bar);
      return el;
    }
    
    case 'Spinner': {
      const el = document.createElement('div');
      el.className = 'vo-spinner';
      return el;
    }
    
    case 'Alert': {
      const el = document.createElement('div');
      el.className = `vo-alert vo-alert-${node.props?.kind ?? 'info'}`;
      el.textContent = node.props?.message ?? '';
      return el;
    }
    
    case 'Image': {
      const el = document.createElement('img');
      el.className = 'vo-image';
      el.src = node.props?.src ?? '';
      el.alt = '';
      return el;
    }
    
    case 'Avatar': {
      const el = document.createElement('img');
      el.className = 'vo-avatar';
      el.src = node.props?.src ?? '';
      el.alt = '';
      return el;
    }
    
    case 'Video': {
      const el = document.createElement('video');
      el.className = 'vo-video';
      el.src = node.props?.src ?? '';
      el.controls = true;
      return el;
    }
    
    case 'Icon': {
      const el = document.createElement('span');
      el.className = 'vo-icon';
      el.textContent = node.props?.name ?? '?';
      return el;
    }
    
    case 'Divider': {
      const el = document.createElement('hr');
      el.className = 'vo-divider';
      return el;
    }
    
    // Interactive
    case 'Button': {
      const el = document.createElement('button');
      el.className = 'vo-button' + (interactive ? ' interactive' : '');
      el.disabled = !interactive;
      el.textContent = node.props?.text ?? 'Button';
      if (style) el.style.cssText = style;
      el.onclick = () => handlers.handleClick(node.props?.onClick);
      return el;
    }
    
    case 'IconButton': {
      const el = document.createElement('button');
      el.className = 'vo-icon-button' + (interactive ? ' interactive' : '');
      el.disabled = !interactive;
      el.textContent = node.props?.icon ?? '?';
      el.onclick = () => handlers.handleClick(node.props?.onClick);
      return el;
    }
    
    case 'Input': {
      const el = document.createElement('input');
      el.className = 'vo-input';
      el.type = 'text';
      el.value = node.props?.value ?? '';
      el.disabled = !interactive;
      if (style) el.style.cssText = style;
      el.oninput = () => handlers.handleInput(node.props?.onChange, el.value);
      return el;
    }
    
    case 'Password': {
      const el = document.createElement('input');
      el.className = 'vo-input';
      el.type = 'password';
      el.value = node.props?.value ?? '';
      el.disabled = !interactive;
      el.oninput = () => handlers.handleInput(node.props?.onChange, el.value);
      return el;
    }
    
    case 'TextArea': {
      const el = document.createElement('textarea');
      el.className = 'vo-textarea';
      el.value = node.props?.value ?? '';
      el.disabled = !interactive;
      if (style) el.style.cssText = style;
      el.oninput = () => handlers.handleInput(node.props?.onChange, el.value);
      return el;
    }
    
    case 'Checkbox': {
      const label = document.createElement('label');
      label.className = 'vo-checkbox';
      const input = document.createElement('input');
      input.type = 'checkbox';
      input.checked = node.props?.checked ?? false;
      input.disabled = !interactive;
      input.onchange = () => handlers.handleChecked(node.props?.onChange, input.checked);
      label.appendChild(input);
      return label;
    }
    
    case 'CheckboxLabel': {
      const label = document.createElement('label');
      label.className = 'vo-checkbox-label';
      const input = document.createElement('input');
      input.type = 'checkbox';
      input.checked = node.props?.checked ?? false;
      input.disabled = !interactive;
      input.onchange = () => handlers.handleChecked(node.props?.onChange, input.checked);
      const span = document.createElement('span');
      span.textContent = node.props?.label ?? '';
      label.appendChild(input);
      label.appendChild(span);
      return label;
    }
    
    case 'Switch': {
      const label = document.createElement('label');
      label.className = 'vo-switch';
      const input = document.createElement('input');
      input.type = 'checkbox';
      input.checked = node.props?.on ?? false;
      input.disabled = !interactive;
      input.onchange = () => handlers.handleChecked(node.props?.onChange, input.checked);
      const slider = document.createElement('span');
      slider.className = 'vo-switch-slider';
      label.appendChild(input);
      label.appendChild(slider);
      return label;
    }
    
    case 'Select': {
      const el = document.createElement('select');
      el.className = 'vo-select';
      el.disabled = !interactive;
      for (const opt of (node.props?.options ?? [])) {
        const option = document.createElement('option');
        option.value = opt;
        option.textContent = opt;
        option.selected = opt === node.props?.value;
        el.appendChild(option);
      }
      el.onchange = () => handlers.handleInput(node.props?.onChange, el.value);
      return el;
    }
    
    case 'SelectLabeled': {
      const el = document.createElement('select');
      el.className = 'vo-select';
      el.disabled = !interactive;
      for (const opt of (node.props?.options ?? [])) {
        const option = document.createElement('option');
        option.value = opt.Value ?? '';
        option.textContent = opt.Label ?? opt.Value ?? '';
        option.selected = opt.Value === node.props?.value;
        el.appendChild(option);
      }
      el.onchange = () => handlers.handleInput(node.props?.onChange, el.value);
      return el;
    }
    
    case 'Radio': {
      const label = document.createElement('label');
      label.className = 'vo-radio';
      const input = document.createElement('input');
      input.type = 'radio';
      input.name = node.props?.group ?? '';
      input.value = node.props?.value ?? '';
      input.checked = node.props?.value === node.props?.selected;
      input.disabled = !interactive;
      input.onchange = () => handlers.handleInput(node.props?.onChange, input.value);
      const span = document.createElement('span');
      span.textContent = node.props?.value ?? '';
      label.appendChild(input);
      label.appendChild(span);
      return label;
    }
    
    case 'Slider': {
      const el = document.createElement('input');
      el.className = 'vo-slider';
      el.type = 'range';
      el.min = String(node.props?.min ?? 0);
      el.max = String(node.props?.max ?? 100);
      el.value = String(node.props?.value ?? 0);
      el.disabled = !interactive;
      el.oninput = () => handlers.handleSlider(node.props?.onChange, parseInt(el.value));
      return el;
    }
    
    case 'NumberInput': {
      const el = document.createElement('input');
      el.className = 'vo-input vo-number-input';
      el.type = 'number';
      el.value = String(node.props?.value ?? 0);
      el.disabled = !interactive;
      el.oninput = () => handlers.handleSlider(node.props?.onChange, parseInt(el.value) || 0);
      return el;
    }
    
    case 'DateInput': {
      const el = document.createElement('input');
      el.className = 'vo-input vo-date-input';
      el.type = 'date';
      el.value = node.props?.value ?? '';
      el.disabled = !interactive;
      el.oninput = () => handlers.handleInput(node.props?.onChange, el.value);
      return el;
    }
    
    case 'TimeInput': {
      const el = document.createElement('input');
      el.className = 'vo-input vo-time-input';
      el.type = 'time';
      el.value = node.props?.value ?? '';
      el.disabled = !interactive;
      el.oninput = () => handlers.handleInput(node.props?.onChange, el.value);
      return el;
    }
    
    case 'ColorInput': {
      const el = document.createElement('input');
      el.className = 'vo-color-input';
      el.type = 'color';
      el.value = node.props?.value ?? '#000000';
      el.disabled = !interactive;
      el.oninput = () => handlers.handleInput(node.props?.onChange, el.value);
      return el;
    }
    
    case 'FileInput': {
      const el = document.createElement('input');
      el.className = 'vo-file-input';
      el.type = 'file';
      el.accept = node.props?.accept ?? '';
      el.disabled = !interactive;
      el.onchange = () => {
        if (interactive && config.onEvent && node.props?.onFiles !== undefined) {
          const files = Array.from(el.files || []).map(f => ({
            name: f.name,
            size: f.size,
            type: f.type,
          }));
          config.onEvent(node.props.onFiles, JSON.stringify({ files }));
        }
      };
      return el;
    }
    
    case 'SearchInput': {
      const container = document.createElement('div');
      container.className = 'vo-search-input';
      const el = document.createElement('input');
      el.type = 'search';
      el.className = 'vo-input';
      el.value = node.props?.value ?? '';
      el.disabled = !interactive;
      el.oninput = () => handlers.handleInput(node.props?.onChange, el.value);
      el.onkeydown = (e) => {
        if (e.key === 'Enter') handlers.handleClick(node.props?.onSubmit);
      };
      container.appendChild(el);
      return container;
    }
    
    // Container
    case 'Card': {
      const el = document.createElement('div');
      el.className = 'vo-card';
      if (style) el.style.cssText = style;
      renderChildren(el, node.children);
      return el;
    }
    
    case 'CardHeader': {
      const el = document.createElement('div');
      el.className = 'vo-card-header';
      el.textContent = node.props?.title ?? '';
      return el;
    }
    
    case 'CardBody': {
      const el = document.createElement('div');
      el.className = 'vo-card-body';
      renderChildren(el, node.children);
      return el;
    }
    
    case 'CardFooter': {
      const el = document.createElement('div');
      el.className = 'vo-card-footer';
      renderChildren(el, node.children);
      return el;
    }
    
    case 'Panel': {
      const el = document.createElement('div');
      el.className = 'vo-panel';
      if (style) el.style.cssText = style;
      const header = document.createElement('div');
      header.className = 'vo-panel-header';
      header.textContent = node.props?.title ?? '';
      el.appendChild(header);
      const body = document.createElement('div');
      body.className = 'vo-panel-body';
      renderChildren(body, node.children);
      el.appendChild(body);
      return el;
    }
    
    case 'Accordion': {
      const el = document.createElement('div');
      el.className = 'vo-accordion';
      const items = node.props?.items ?? [];
      items.forEach((item: any, idx: number) => {
        const panel = document.createElement('div');
        panel.className = 'vo-accordion-item';
        const header = document.createElement('div');
        header.className = 'vo-accordion-header';
        header.textContent = item.title ?? '';
        if (interactive) {
          header.onclick = () => {
            if (config.onEvent && node.props?.onChange !== undefined) {
              config.onEvent(node.props.onChange, JSON.stringify({ value: idx }));
            }
          };
        }
        const content = document.createElement('div');
        content.className = 'vo-accordion-content';
        content.style.display = item.open ? 'block' : 'none';
        if (item.content) {
          const child = renderNode(item.content, config, handlers);
          if (child) content.appendChild(child);
        }
        panel.appendChild(header);
        panel.appendChild(content);
        el.appendChild(panel);
      });
      return el;
    }
    
    case 'Tabs': {
      const el = document.createElement('div');
      el.className = 'vo-tabs';
      const tabList = document.createElement('div');
      tabList.className = 'vo-tab-list';
      const tabs = node.props?.tabs ?? [];
      const active = node.props?.active ?? 0;
      tabs.forEach((tab: any, idx: number) => {
        const tabBtn = document.createElement('button');
        tabBtn.className = 'vo-tab' + (idx === active ? ' active' : '');
        tabBtn.textContent = tab.label ?? '';
        tabBtn.disabled = !interactive;
        if (interactive) {
          tabBtn.onclick = () => {
            if (config.onEvent && node.props?.onChange !== undefined) {
              config.onEvent(node.props.onChange, JSON.stringify({ value: idx }));
            }
          };
        }
        tabList.appendChild(tabBtn);
      });
      el.appendChild(tabList);
      const tabContent = document.createElement('div');
      tabContent.className = 'vo-tab-content';
      if (tabs[active]?.content) {
        const child = renderNode(tabs[active].content, config, handlers);
        if (child) tabContent.appendChild(child);
      }
      el.appendChild(tabContent);
      return el;
    }
    
    // Overlay
    case 'Modal': {
      const overlay = document.createElement('div');
      overlay.className = 'vo-modal-overlay';
      if (!node.props?.open) {
        overlay.style.display = 'none';
        return overlay;
      }
      if (interactive) {
        overlay.onclick = (e) => {
          if (e.target === overlay) handlers.handleClick(node.props?.onClose);
        };
      }
      const modal = document.createElement('div');
      modal.className = 'vo-modal';
      renderChildren(modal, node.children);
      overlay.appendChild(modal);
      return overlay;
    }
    
    case 'ModalHeader': {
      const el = document.createElement('div');
      el.className = 'vo-modal-header';
      el.textContent = node.props?.title ?? '';
      return el;
    }
    
    case 'ModalBody': {
      const el = document.createElement('div');
      el.className = 'vo-modal-body';
      renderChildren(el, node.children);
      return el;
    }
    
    case 'ModalFooter': {
      const el = document.createElement('div');
      el.className = 'vo-modal-footer';
      renderChildren(el, node.children);
      return el;
    }
    
    case 'Drawer': {
      const overlay = document.createElement('div');
      overlay.className = 'vo-drawer-overlay';
      if (!node.props?.open) {
        overlay.style.display = 'none';
        return overlay;
      }
      if (interactive) {
        overlay.onclick = (e) => {
          if (e.target === overlay) handlers.handleClick(node.props?.onClose);
        };
      }
      const drawer = document.createElement('div');
      drawer.className = `vo-drawer vo-drawer-${node.props?.side ?? 'left'}`;
      renderChildren(drawer, node.children);
      overlay.appendChild(drawer);
      return overlay;
    }
    
    case 'Tooltip': {
      const el = document.createElement('div');
      el.className = 'vo-tooltip-wrapper';
      if (node.children?.[0]) {
        const child = renderNode(node.children[0], config, handlers);
        if (child) el.appendChild(child);
      }
      const tip = document.createElement('div');
      tip.className = 'vo-tooltip';
      tip.textContent = node.props?.content ?? '';
      el.appendChild(tip);
      return el;
    }
    
    case 'Popover': {
      const el = document.createElement('div');
      el.className = 'vo-popover-wrapper';
      if (node.children?.[0]) {
        const trigger = renderNode(node.children[0], config, handlers);
        if (trigger) el.appendChild(trigger);
      }
      const pop = document.createElement('div');
      pop.className = 'vo-popover';
      if (node.props?.content) {
        const content = renderNode(node.props.content, config, handlers);
        if (content) pop.appendChild(content);
      }
      el.appendChild(pop);
      return el;
    }
    
    case 'Dropdown': {
      const el = document.createElement('div');
      el.className = 'vo-dropdown-wrapper';
      if (node.children?.[0]) {
        const trigger = renderNode(node.children[0], config, handlers);
        if (trigger) el.appendChild(trigger);
      }
      const menu = document.createElement('div');
      menu.className = 'vo-dropdown-menu';
      const items = node.props?.items ?? [];
      items.forEach((item: any) => {
        const menuItem = document.createElement('div');
        menuItem.className = 'vo-dropdown-item';
        menuItem.textContent = item.label ?? '';
        if (interactive && item.onClick !== undefined) {
          menuItem.onclick = () => handlers.handleClick(item.onClick);
        }
        menu.appendChild(menuItem);
      });
      el.appendChild(menu);
      return el;
    }
    
    case 'ContextMenu': {
      const el = document.createElement('div');
      el.className = 'vo-context-menu-wrapper';
      if (node.children?.[0]) {
        const child = renderNode(node.children[0], config, handlers);
        if (child) el.appendChild(child);
      }
      return el;
    }
    
    // Form
    case 'Form': {
      const el = document.createElement('form');
      el.className = 'vo-form';
      el.onsubmit = (e) => {
        e.preventDefault();
        handlers.handleClick(node.props?.onSubmit);
      };
      renderChildren(el, node.children);
      return el;
    }
    
    case 'FormField': {
      const el = document.createElement('div');
      el.className = 'vo-form-field';
      const label = document.createElement('label');
      label.className = 'vo-form-label';
      label.textContent = node.props?.label ?? '';
      el.appendChild(label);
      if (node.children?.[0]) {
        const child = renderNode(node.children[0], config, handlers);
        if (child) el.appendChild(child);
      }
      return el;
    }
    
    case 'FormError': {
      const el = document.createElement('div');
      el.className = 'vo-form-error';
      el.textContent = node.props?.message ?? '';
      return el;
    }
    
    case 'FormHelp': {
      const el = document.createElement('div');
      el.className = 'vo-form-help';
      el.textContent = node.props?.message ?? '';
      return el;
    }
    
    case 'FormSection': {
      const el = document.createElement('fieldset');
      el.className = 'vo-form-section';
      const legend = document.createElement('legend');
      legend.textContent = node.props?.title ?? '';
      el.appendChild(legend);
      renderChildren(el, node.children);
      return el;
    }
    
    // List & Table
    case 'List': {
      const el = document.createElement('ul');
      el.className = 'vo-list';
      renderChildren(el, node.children);
      return el;
    }
    
    case 'ListItem': {
      const el = document.createElement('li');
      el.className = 'vo-list-item';
      renderChildren(el, node.children);
      return el;
    }
    
    case 'OrderedList': {
      const el = document.createElement('ol');
      el.className = 'vo-ordered-list';
      renderChildren(el, node.children);
      return el;
    }
    
    case 'Table': {
      const el = document.createElement('table');
      el.className = 'vo-table';
      renderChildren(el, node.children);
      return el;
    }
    
    case 'TableHead': {
      const el = document.createElement('thead');
      el.className = 'vo-table-head';
      renderChildren(el, node.children);
      return el;
    }
    
    case 'TableBody': {
      const el = document.createElement('tbody');
      el.className = 'vo-table-body';
      renderChildren(el, node.children);
      return el;
    }
    
    case 'TableRow': {
      const el = document.createElement('tr');
      el.className = 'vo-table-row';
      renderChildren(el, node.children);
      return el;
    }
    
    case 'TableCell': {
      const el = document.createElement('td');
      el.className = 'vo-table-cell';
      if (style) el.style.cssText = style;
      renderChildren(el, node.children);
      return el;
    }
    
    case 'TableHeaderCell': {
      const el = document.createElement('th');
      el.className = 'vo-table-header-cell';
      el.textContent = node.props?.content ?? '';
      return el;
    }
    
    // Navigation
    case 'Nav': {
      const el = document.createElement('nav');
      el.className = 'vo-nav';
      renderChildren(el, node.children);
      return el;
    }
    
    case 'NavItem': {
      const el = document.createElement('button');
      el.className = 'vo-nav-item' + (node.props?.active ? ' active' : '');
      el.textContent = node.props?.text ?? '';
      el.disabled = !interactive;
      el.onclick = () => handlers.handleClick(node.props?.onClick);
      return el;
    }
    
    case 'NavLink': {
      const el = document.createElement('a');
      el.className = 'vo-nav-link';
      el.href = node.props?.to ?? '#';
      el.textContent = node.props?.text ?? '';
      if (interactive) {
        el.onclick = (e) => {
          e.preventDefault();
          if (config.onEvent) {
            config.onEvent(-3, JSON.stringify({ path: node.props?.to }));
          }
        };
      }
      return el;
    }
    
    case 'Breadcrumb': {
      const el = document.createElement('nav');
      el.className = 'vo-breadcrumb';
      const items = node.props?.items ?? [];
      items.forEach((item: any, idx: number) => {
        if (idx > 0) {
          const sep = document.createElement('span');
          sep.className = 'vo-breadcrumb-sep';
          sep.textContent = '/';
          el.appendChild(sep);
        }
        const link = document.createElement('a');
        link.className = 'vo-breadcrumb-item';
        link.href = item.href ?? '#';
        link.textContent = item.label ?? '';
        el.appendChild(link);
      });
      return el;
    }
    
    case 'Pagination': {
      const el = document.createElement('div');
      el.className = 'vo-pagination';
      const current = node.props?.current ?? 1;
      const total = node.props?.total ?? 1;
      for (let i = 1; i <= total; i++) {
        const btn = document.createElement('button');
        btn.className = 'vo-pagination-btn' + (i === current ? ' active' : '');
        btn.textContent = String(i);
        btn.disabled = !interactive;
        if (interactive) {
          btn.onclick = () => {
            if (config.onEvent && node.props?.onChange !== undefined) {
              config.onEvent(node.props.onChange, JSON.stringify({ value: i }));
            }
          };
        }
        el.appendChild(btn);
      }
      return el;
    }
    
    case 'Steps': {
      const el = document.createElement('div');
      el.className = 'vo-steps';
      const current = node.props?.current ?? 0;
      const items = node.props?.items ?? [];
      items.forEach((item: string, idx: number) => {
        const step = document.createElement('div');
        step.className = 'vo-step' + (idx < current ? ' completed' : '') + (idx === current ? ' active' : '');
        const num = document.createElement('span');
        num.className = 'vo-step-num';
        num.textContent = String(idx + 1);
        const label = document.createElement('span');
        label.className = 'vo-step-label';
        label.textContent = item;
        step.appendChild(num);
        step.appendChild(label);
        el.appendChild(step);
      });
      return el;
    }
    
    // ExternalWidget — delegates to JS-native widget plugins
    case 'ExternalWidget': {
      const id = node.props?.id ?? 'widget-0';
      const widgetType = node.props?.widgetType ?? '';
      const domId = `vo-widget-${id}`;

      // Return existing instance element if already mounted (morphdom preserves it)
      let instance = widgetInstances.get(id);
      if (instance) {
        instance.update(node.props);
        return instance.element;
      }

      const factory = widgetRegistry.get(widgetType);
      if (!factory) {
        const el = document.createElement('div');
        el.className = 'vo-widget vo-widget-missing';
        el.textContent = `Unknown widget: ${widgetType}`;
        return el;
      }

      const container = document.createElement('div');
      container.className = `vo-widget vo-widget-${widgetType}`;
      container.id = domId;
      container.dataset.widgetId = id;

      const onEvent = (rawPayload: string) => {
        if (interactive && config.onEvent && node.props?.onEvent !== undefined) {
          config.onEvent(node.props.onEvent as number, rawPayload);
        }
      };

      instance = factory.create(container, node.props, onEvent);
      // factory.create sets instance.element = container or a child;
      // ensure the outer element has the widget identity attrs
      instance.element.className = `vo-widget vo-widget-${widgetType}`;
      instance.element.id = domId;
      instance.element.dataset.widgetId = id;
      widgetInstances.set(id, instance);
      return instance.element;
    }

    // Canvas (GPU rendering)
    case 'Canvas': {
      const canvasId = node.props?.id ?? 'vo-canvas-0';
      const width = node.props?.width ?? 300;
      const height = node.props?.height ?? 150;

      const el = document.createElement('canvas') as HTMLCanvasElement;
      el.id = `vo-canvas-${canvasId}`;
      el.className = 'vo-canvas';
      el.width = width;
      el.height = height;
      if (style) el.style.cssText = style;
      // Prevent canvas from being a drag target / interfering with touch scroll
      el.style.touchAction = 'none';

      // Register in platform canvas registry immediately.
      // Canvas contexts (WebGL, 2D) can be obtained before DOM insertion,
      // so extensions can access the canvas in the same render cycle.
      const win = window as any;
      if (typeof win.voRegisterCanvas === 'function') {
        win.voRegisterCanvas(canvasId, el);
      }

      // Pointer events (unified mouse + touch)
      if (interactive && config.onEvent && node.props?.onPointer !== undefined) {
        const ptrHandler = node.props.onPointer as number;
        const ptrEvents = ['pointerdown', 'pointerup', 'pointermove', 'pointerenter', 'pointerleave'];
        for (const evtType of ptrEvents) {
          el.addEventListener(evtType, (e: PointerEvent) => {
            const rect = el.getBoundingClientRect();
            const payload = JSON.stringify({
              kind: evtType.replace('pointer', ''),
              x: e.clientX - rect.left,
              y: e.clientY - rect.top,
              button: e.button,
              buttons: e.buttons,
            });
            config.onEvent!(ptrHandler, payload);
          });
        }
        // Prevent context menu on right-click inside canvas
        el.addEventListener('contextmenu', (e) => e.preventDefault());
      }

      // Resize observer → onResize handler
      if (interactive && config.onEvent && node.props?.onResize !== undefined) {
        const resizeHandler = node.props.onResize as number;
        const observer = new ResizeObserver((entries) => {
          for (const entry of entries) {
            const { width: w, height: h } = entry.contentRect;
            config.onEvent!(resizeHandler, JSON.stringify({ width: Math.round(w), height: Math.round(h) }));
          }
        });
        observer.observe(el);
        // Store observer for cleanup
        (el as any).__voResizeObserver = observer;
      }

      // Fullscreen support
      if (node.props?.fullscreen) {
        // requestFullscreen must be called from a user gesture
        // Queue on next click if not already fullscreen
        if (!document.fullscreenElement) {
          el.addEventListener('click', () => {
            el.requestFullscreen().catch(() => {});
          }, { once: true });
        }
      }

      // Listen for fullscreen exit → fire resize event
      if (interactive && config.onEvent && node.props?.onResize !== undefined) {
        const resizeHandler = node.props.onResize as number;
        el.addEventListener('fullscreenchange', () => {
          config.onEvent!(resizeHandler, JSON.stringify({
            width: el.clientWidth,
            height: el.clientHeight,
          }));
        });
      }

      return el;
    }

    // Utility
    case 'Spacer': {
      const el = document.createElement('div');
      el.className = 'vo-spacer';
      return el;
    }
    
    case 'Empty':
      return null;
    
    default: {
      const el = document.createElement('div');
      el.className = 'vo-unknown';
      el.textContent = `[${node.type}]`;
      return el;
    }
  }
}

/** Render VoNode tree into a container element using morphdom */
export function render(container: HTMLElement, tree: VoNode | null, config: RendererConfig): void {
  if (!tree) {
    container.innerHTML = '';
    return;
  }
  
  const handlers = createEventHandlers(config);
  const newEl = renderNode(tree, config, handlers);
  if (!newEl || !(newEl instanceof HTMLElement)) {
    container.innerHTML = '';
    if (newEl) container.appendChild(newEl);
    return;
  }
  
  const oldEl = container.firstElementChild as HTMLElement | null;
  if (!oldEl) {
    container.appendChild(newEl);
    return;
  }
  
  // Use morphdom for efficient DOM updates
  morphdom(oldEl, newEl, {
    onBeforeElUpdated(fromEl, toEl) {
      // Preserve ExternalWidget elements — update via widget instance instead
      if (fromEl.dataset?.widgetId && toEl.dataset?.widgetId
          && fromEl.dataset.widgetId === toEl.dataset.widgetId) {
        const instance = widgetInstances.get(fromEl.dataset.widgetId);
        // props update already happened in renderNode (ExternalWidget case)
        // just prevent DOM replacement
        if (instance) return false;
      }

      // Preserve canvas GPU state — never replace canvas elements
      if (fromEl.tagName === 'CANVAS' && toEl.tagName === 'CANVAS') {
        const fromCanvas = fromEl as HTMLCanvasElement;
        const toCanvas = toEl as HTMLCanvasElement;
        // Update dimensions if changed
        if (fromCanvas.width !== toCanvas.width) fromCanvas.width = toCanvas.width;
        if (fromCanvas.height !== toCanvas.height) fromCanvas.height = toCanvas.height;
        // Update CSS
        if (toCanvas.style.cssText) fromCanvas.style.cssText = toCanvas.style.cssText;
        return false; // Do NOT replace the element — preserve WebGL/GPU context
      }

      // Preserve focused input values
      if (document.activeElement === fromEl) {
        if ('value' in fromEl && 'value' in toEl) {
          (toEl as HTMLInputElement).value = (fromEl as HTMLInputElement).value;
        }
        if ('checked' in fromEl && 'checked' in toEl) {
          (toEl as HTMLInputElement).checked = (fromEl as HTMLInputElement).checked;
        }
      }
      // Copy event handlers (morphdom doesn't handle these)
      const events = ['onclick', 'oninput', 'onchange', 'onsubmit', 'onkeydown'];
      for (const evt of events) {
        if ((toEl as any)[evt]) {
          (fromEl as any)[evt] = (toEl as any)[evt];
        }
      }
      return true;
    },
    onNodeDiscarded(node: Node) {
      // Cleanup ExternalWidget instances when removed from DOM
      if (node instanceof HTMLElement && node.dataset?.widgetId) {
        const instance = widgetInstances.get(node.dataset.widgetId);
        if (instance) {
          instance.destroy();
          widgetInstances.delete(node.dataset.widgetId);
        }
      }
      // Cleanup canvas registry and resize observers when canvas is removed
      if (node instanceof HTMLCanvasElement) {
        const canvasId = node.id.replace('vo-canvas-', '');
        const win = window as any;
        if (typeof win.voUnregisterCanvas === 'function') {
          win.voUnregisterCanvas(canvasId);
        }
        // Disconnect resize observer
        if ((node as any).__voResizeObserver) {
          (node as any).__voResizeObserver.disconnect();
        }
      }
    }
  });
}

/** Setup global key handler */
export function setupKeyHandler(config: RendererConfig): () => void {
  const handler = (event: KeyboardEvent) => {
    // Skip global key events when focused on input elements
    const target = event.target as HTMLElement;
    if (target.tagName === 'INPUT' || target.tagName === 'TEXTAREA' || target.isContentEditable) {
      return;
    }
    if (config.interactive && config.onEvent) {
      config.onEvent(-2, JSON.stringify({ key: event.key }));
    }
  };
  
  window.addEventListener('keydown', handler);
  return () => window.removeEventListener('keydown', handler);
}
