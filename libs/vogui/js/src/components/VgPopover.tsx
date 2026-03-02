import { h } from 'preact';
import * as Popover from '@radix-ui/react-popover';
import { voNodeToVNode } from '../renderer';
import type { VoNode } from '../types';

export function VgPopover(props: any) {
    const { open, onOpenChange, side, voChildren } = props;
    const children = (voChildren || []) as VoNode[];
    // First child is trigger, rest is content
    const trigger = children[0];
    const content = children.slice(1);

    return h(Popover.Root, {
        open: open != null ? Boolean(open) : undefined,
    },
        h(Popover.Trigger, { asChild: true },
            trigger ? voNodeToVNode(trigger) : h('span', null),
        ),
        h(Popover.Portal, null,
            h(Popover.Content, {
                className: [
                    'z-50 w-72 rounded-md border bg-popover p-4 text-popover-foreground shadow-md outline-none',
                    'animate-scale-in',
                ].join(' '),
                side: side || 'bottom',
                sideOffset: 4,
                align: 'center',
            },
                ...content.map(voNodeToVNode),
                h(Popover.Arrow, { className: 'fill-popover' }),
            ),
        ),
    );
}
