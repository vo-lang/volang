import { h } from 'preact';
import * as Accordion from '@radix-ui/react-accordion';
import { emit } from '../events';
import { propsToStyle } from '../mapping';
import { voNodeToVNode } from '../renderer';
import type { VoNode } from '../types';

export function VgAccordion(props: any) {
    const { items, openIndex, onChange } = props;
    const accItems = (items || []) as Array<{ title: string; content: VoNode }>;
    const activeValue = openIndex != null && openIndex >= 0 ? String(openIndex) : undefined;
    const userClass = props.class || '';
    const userStyle = propsToStyle(props);

    return h(Accordion.Root, {
        type: 'single',
        collapsible: true,
        value: activeValue,
        className: userClass || undefined,
        style: userStyle,
        onValueChange: (val: string) => {
            if (onChange != null) {
                const idx = val === '' ? -1 : parseInt(val, 10);
                emit(onChange, JSON.stringify({ Value: idx }));
            }
        },
    },
        ...accItems.map((item, i) =>
            h(Accordion.Item, {
                key: String(i),
                value: String(i),
                className: 'border-b border-border',
            },
                h(Accordion.Header, { className: 'flex' },
                    h(Accordion.Trigger, {
                        className: [
                            'flex flex-1 items-center justify-between py-4 text-sm font-medium transition-all',
                            'hover:underline [&[data-state=open]>svg]:rotate-180',
                        ].join(' '),
                    },
                        item.title,
                        h('svg', {
                            width: 16, height: 16, viewBox: '0 0 24 24', fill: 'none',
                            stroke: 'currentColor', strokeWidth: 2,
                            className: 'h-4 w-4 shrink-0 text-muted-foreground transition-transform duration-200',
                        },
                            h('path', { d: 'm6 9 6 6 6-6' }),
                        ),
                    ),
                ),
                h(Accordion.Content, {
                    className: 'overflow-hidden text-sm data-[state=closed]:animate-fade-out data-[state=open]:animate-fade-in',
                },
                    h('div', { className: 'pb-4 pt-0' },
                        item.content ? voNodeToVNode(item.content) : null,
                    ),
                ),
            ),
        ),
    );
}
