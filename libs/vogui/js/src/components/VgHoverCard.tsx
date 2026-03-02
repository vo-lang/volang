import { h } from 'preact';
import * as HoverCard from '@radix-ui/react-hover-card';
import { voNodeToVNode } from '../renderer';
import type { VoNode } from '../types';

export function VgHoverCard(props: any) {
    const { voChildren } = props;
    const children = (voChildren || []) as VoNode[];
    // First child is trigger, rest is card content
    const trigger = children[0];
    const content = children.slice(1);

    return h(HoverCard.Root, { openDelay: 200, closeDelay: 100 },
        h(HoverCard.Trigger, { asChild: true },
            trigger ? voNodeToVNode(trigger) : h('span', null),
        ),
        h(HoverCard.Portal, null,
            h(HoverCard.Content, {
                className: [
                    'z-50 w-64 rounded-md border bg-popover p-4 text-popover-foreground shadow-md outline-none',
                    'animate-scale-in',
                ].join(' '),
                sideOffset: 4,
            },
                ...content.map(voNodeToVNode),
                h(HoverCard.Arrow, { className: 'fill-popover' }),
            ),
        ),
    );
}
