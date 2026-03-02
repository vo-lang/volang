import { h } from 'preact';
import * as Tooltip from '@radix-ui/react-tooltip';
import { voNodeToVNode } from '../renderer';
import type { VoNode } from '../types';

export function VgTooltip(props: any) {
    const { textContent, side, voChildren } = props;
    const children = (voChildren || []) as VoNode[];
    const trigger = children[0];

    return h(Tooltip.Provider as any, { delayDuration: 200 },
        h(Tooltip.Root as any, null,
            h(Tooltip.Trigger, { asChild: true },
                trigger ? voNodeToVNode(trigger) : h('span', null),
            ),
            h(Tooltip.Portal, null,
                h(Tooltip.Content, {
                    className: [
                        'z-50 overflow-hidden rounded-md bg-foreground px-3 py-1.5 text-xs text-background shadow-md',
                        'animate-scale-in',
                    ].join(' '),
                    side: side || 'top',
                    sideOffset: 4,
                },
                    textContent || '',
                    h(Tooltip.Arrow, { className: 'fill-foreground' }),
                ),
            ),
        ),
    );
}
