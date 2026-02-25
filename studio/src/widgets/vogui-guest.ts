/**
 * Nested vogui guest renderer ExternalWidget plugin.
 *
 * Receives a `renderJson` prop (the JSON emitted by the guest VM's emitRender),
 * renders it inside a shadow container using the vogui DOM renderer, and
 * forwards user interactions back to the IDE host VM as widget events.
 *
 * Event payload format: JSON {"h":<handlerId>,"p":<payload>}
 * This matches what actions.vo `sendGuestEvent` expects.
 */

import { registerWidget, render } from '@vogui/index';
import { injectStyles } from '@vogui/styles';
import type { WidgetInstance, RenderMessage } from '@vogui/types';

function createVoguiGuestWidget(
  container: HTMLElement,
  props: any,
  onEvent: (payload: string) => void,
): WidgetInstance {
  container.style.cssText = 'flex:1;overflow:auto;background:#fff;position:relative';

  // Ensure vogui styles are injected
  injectStyles();

  // Inner mount point for the guest render tree
  const inner = document.createElement('div');
  inner.style.cssText = 'min-height:100%;';
  container.appendChild(inner);

  let lastRenderJson = '';

  function applyRender(renderJson: string) {
    if (!renderJson || renderJson === lastRenderJson) return;
    lastRenderJson = renderJson;

    let msg: RenderMessage;
    try {
      msg = JSON.parse(renderJson) as RenderMessage;
    } catch {
      return;
    }
    if (msg.type !== 'render') return;

    render(inner, msg, {
      onEvent(handlerId: number, payload: string) {
        onEvent(JSON.stringify({ h: handlerId, p: payload }));
      },
    });
  }

  // Initial render
  if (props?.renderJson) {
    applyRender(props.renderJson);
  }

  return {
    update(newProps: any) {
      if (newProps?.renderJson && newProps.renderJson !== lastRenderJson) {
        applyRender(newProps.renderJson);
      }
    },
    destroy() {
      inner.innerHTML = '';
    },
  };
}

export function registerVoguiGuestWidget() {
  registerWidget('vogui-guest', {
    create: createVoguiGuestWidget,
  });
}
