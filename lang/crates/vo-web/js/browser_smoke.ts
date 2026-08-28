import { UiDomAdapter } from './ui_dom.js';
import { decodeUiEvent, type UiIdentity, type UiMutation, type UiValue } from './ui_protocol.js';

interface BrowserSmokeReport {
  readonly complete: true;
  readonly passed: boolean;
  readonly checks: readonly string[];
  readonly revision: string;
  readonly error?: string;
}

declare global {
  interface Window {
    __volangUiBrowserSmoke: Promise<BrowserSmokeReport>;
  }
}

const identity = (index: number): UiIdentity => ({ index, generation: 1 });

function property(index: number, code: number, value: UiValue): UiMutation {
  return { type: 'set-property', id: identity(index), property: code, value };
}

function insert(parent: number, child: number): UiMutation {
  return { type: 'insert-before', parent: identity(parent), child: identity(child) };
}

function requireCheck(value: unknown, label: string, checks: string[]): void {
  if (!value) throw new Error(`browser conformance failed: ${label}`);
  checks.push(label);
}

async function runBrowserSmoke(): Promise<BrowserSmokeReport> {
  const checks: string[] = [];
  const root = document.querySelector<HTMLElement>('#volang-root');
  if (root === null) throw new Error('browser conformance root is missing');
  const adapter = new UiDomAdapter(root);
  try {
    adapter.applyBatch({
      sessionEpoch: 81n,
      revision: 1n,
      mutations: [
        { type: 'create-element', id: identity(1), primitive: 4 },
        { type: 'create-element', id: identity(2), primitive: 10 },
        { type: 'create-element', id: identity(3), primitive: 9 },
        { type: 'create-text', id: identity(4) },
        property(1, 8, { type: 'i64', value: 12n }),
        property(1, 10, { type: 'color', value: 0xff315efb }),
        property(2, 16, { type: 'text', value: 'initial' }),
        property(2, 20, { type: 'text', value: 'Display name' }),
        property(2, 28, { type: 'bool', value: true }),
        property(2, 29, { type: 'bool', value: true }),
        property(2, 30, { type: 'text', value: 'Name is required' }),
        property(2, 31, { type: 'i64', value: 0n }),
        property(2, 32, { type: 'i64', value: 0n }),
        {
          type: 'listen', id: identity(2), listener: {
            event: 2, handler: identity(20), capture: false, passive: false, once: false,
          },
        },
        { type: 'set-text', id: identity(4), text: 'Open dialog' },
        insert(0, 1),
        insert(1, 2),
        insert(1, 3),
        insert(3, 4),
      ],
    });
    const column = root.children[0] as HTMLElement;
    const input = column.children[0] as HTMLInputElement;
    const launcher = column.children[1] as HTMLButtonElement;
    requireCheck(
      column.style.display === 'flex'
        && column.style.flexDirection === 'column'
        && column.style.gap === '12px'
        && getComputedStyle(column).backgroundColor === 'rgb(49, 94, 251)',
      'primitive and visual style mapping',
      checks,
    );
    requireCheck(
      input.getAttribute('aria-label') === 'Display name'
        && input.required
        && input.getAttribute('aria-invalid') === 'true'
        && input.getAttribute('aria-description') === 'Name is required',
      'form accessibility mapping',
      checks,
    );

    input.value = 'typed';
    input.dispatchEvent(new InputEvent('input', { bubbles: true, data: 'd' }));
    const inputFrame = adapter.shiftEventFrame();
    const inputEvent = inputFrame === undefined ? undefined : decodeUiEvent(inputFrame);
    requireCheck(
      inputEvent?.handler.index === 20
        && inputEvent.event === 2
        && inputEvent.payload.type === 'text'
        && inputEvent.payload.value === 'typed',
      'typed browser input event encoding',
      checks,
    );

    input.dispatchEvent(new CompositionEvent('compositionstart', { bubbles: true }));
    input.value = '拼音';
    input.setSelectionRange(2, 2);
    adapter.applyBatch({
      sessionEpoch: 81n,
      revision: 2n,
      mutations: [
        property(2, 16, { type: 'text', value: '应用状态' }),
        property(2, 31, { type: 'i64', value: 1n }),
        property(2, 32, { type: 'i64', value: 2n }),
      ],
    });
    requireCheck(
      input.value === '拼音' && input.selectionStart === 2 && input.selectionEnd === 2,
      'IME composition ownership',
      checks,
    );
    input.dispatchEvent(new CompositionEvent('compositionend', { bubbles: true, data: '拼音' }));
    adapter.applyBatch({ sessionEpoch: 81n, revision: 3n, mutations: [] });
    requireCheck(
      input.value === '应用状态' && input.selectionStart === 1 && input.selectionEnd === 3,
      'controlled UTF-16 synchronization',
      checks,
    );

    launcher.focus();
    adapter.applyBatch({
      sessionEpoch: 81n,
      revision: 4n,
      mutations: [
        { type: 'create-element', id: identity(5), primitive: 5 },
        { type: 'create-element', id: identity(6), primitive: 9 },
        { type: 'create-element', id: identity(7), primitive: 9 },
        property(5, 19, { type: 'text', value: 'dialog' }),
        property(5, 20, { type: 'text', value: 'Confirm' }),
        property(5, 35, { type: 'bool', value: true }),
        property(7, 36, { type: 'bool', value: true }),
        insert(5, 6),
        insert(5, 7),
        insert(0, 5),
      ],
    });
    const modal = root.children[1] as HTMLElement;
    const first = modal.children[0] as HTMLButtonElement;
    const second = modal.children[1] as HTMLButtonElement;
    requireCheck(
      modal.getAttribute('role') === 'dialog'
        && modal.hasAttribute('aria-modal')
        && modal.style.display === 'grid'
        && first.style.gridArea === '1 / 1'
        && second.style.gridArea === '1 / 1'
        && document.activeElement === second,
      'modal focus and Stack projection',
      checks,
    );
    second.dispatchEvent(new KeyboardEvent('keydown', {
      bubbles: true, cancelable: true, key: 'Tab',
    }));
    requireCheck(document.activeElement === first, 'modal forward focus wrapping', checks);
    const blocked = new PointerEvent('pointerdown', { bubbles: true, cancelable: true });
    launcher.dispatchEvent(blocked);
    requireCheck(blocked.defaultPrevented, 'modal background pointer isolation', checks);

    adapter.applyBatch({
      sessionEpoch: 81n,
      revision: 5n,
      mutations: [{ type: 'remove-property', id: identity(5), property: 35 }],
    });
    requireCheck(document.activeElement === launcher, 'modal focus restoration', checks);

    let rejected = false;
    try {
      adapter.applyBatch({
        sessionEpoch: 81n,
        revision: 6n,
        mutations: [10, 11].flatMap((index): UiMutation[] => [
          { type: 'create-element', id: identity(index), primitive: 2 },
          property(index, 35, { type: 'bool', value: true }),
          insert(0, index),
        ]),
      });
    } catch {
      rejected = true;
    }
    requireCheck(
      rejected && adapter.currentRevision === 5n && root.children.length === 2,
      'atomic rejection of ambiguous modal trees',
      checks,
    );
    return { complete: true, passed: true, checks, revision: adapter.currentRevision.toString() };
  } catch (error) {
    return {
      passed: false,
      complete: true,
      checks,
      revision: adapter.currentRevision.toString(),
      error: error instanceof Error ? error.stack ?? error.message : String(error),
    };
  }
}

window.__volangUiBrowserSmoke = runBrowserSmoke();
