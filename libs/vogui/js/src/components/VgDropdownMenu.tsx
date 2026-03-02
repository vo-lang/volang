import { h } from 'preact';
import * as DropdownMenu from '@radix-ui/react-dropdown-menu';
import { emit } from '../events';
import { voNodeToVNode } from '../renderer';
import type { VoNode } from '../types';

export function VgDropdownMenu(props: any) {
    const { voChildren } = props;
    const children = (voChildren || []) as VoNode[];
    // First child is the trigger, rest are menu items
    const trigger = children[0];
    const items = children.slice(1);

    return h(DropdownMenu.Root, null,
        h(DropdownMenu.Trigger, { asChild: true },
            trigger ? voNodeToVNode(trigger) : h('button', null, '...'),
        ),
        h(DropdownMenu.Portal, null,
            h(DropdownMenu.Content, {
                className: [
                    'z-50 min-w-[8rem] overflow-hidden rounded-md border bg-popover p-1 text-popover-foreground shadow-md',
                    'animate-scale-in',
                ].join(' '),
                sideOffset: 4,
                align: 'start',
            },
                ...items.map((item) => renderMenuItem(item)),
            ),
        ),
    );
}

function renderMenuItem(node: VoNode): any {
    if (!node) return null;
    const { type, props = {} } = node;

    if (type === 'vo-menu-divider') {
        return h(DropdownMenu.Separator, { className: '-mx-1 my-1 h-px bg-border' });
    }

    // Regular menu item
    const label = props.textContent || '';
    const onClick = props.onClick;
    const disabled = props.disabled;

    return h(DropdownMenu.Item, {
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
