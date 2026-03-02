import { h } from 'preact';
import * as ContextMenu from '@radix-ui/react-context-menu';
import { emit } from '../events';
import { voNodeToVNode } from '../renderer';
import type { VoNode } from '../types';

export function VgContextMenu(props: any) {
    const { voChildren } = props;
    const children = (voChildren || []) as VoNode[];
    // First child is the trigger area, rest are menu items
    const trigger = children[0];
    const items = children.slice(1);

    return h(ContextMenu.Root, null,
        h(ContextMenu.Trigger, { asChild: true },
            trigger ? voNodeToVNode(trigger) : h('div', null),
        ),
        h(ContextMenu.Portal, null,
            h(ContextMenu.Content, {
                className: [
                    'z-50 min-w-[8rem] overflow-hidden rounded-md border bg-popover p-1 text-popover-foreground shadow-md',
                    'animate-scale-in',
                ].join(' '),
            },
                ...items.map((item) => renderContextMenuItem(item)),
            ),
        ),
    );
}

function renderContextMenuItem(node: VoNode): any {
    if (!node) return null;
    const { type, props = {} } = node;

    if (type === 'vo-menu-divider') {
        return h(ContextMenu.Separator, { className: '-mx-1 my-1 h-px bg-border' });
    }

    const label = props.textContent || '';
    const onClick = props.onClick;
    const disabled = props.disabled;

    return h(ContextMenu.Item, {
        className: [
            'relative flex cursor-default select-none items-center rounded-sm px-2 py-1.5 text-sm outline-none',
            'transition-colors focus:bg-accent focus:text-accent-foreground',
            'data-[disabled]:pointer-events-none data-[disabled]:opacity-50',
        ].join(' '),
        disabled: disabled || false,
        onSelect: () => {
            if (onClick != null) emit(onClick, '{}');
        },
    }, label);
}
