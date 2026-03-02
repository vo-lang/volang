import { h } from 'preact';
import * as Tabs from '@radix-ui/react-tabs';
import { emit } from '../events';
import { propsToStyle } from '../mapping';
import { voNodeToVNode } from '../renderer';
import type { VoNode } from '../types';

export function VgTabs(props: any) {
    const { activeIndex, items, onChange } = props;
    const tabItems = (items || []) as Array<{ label: string; content: VoNode }>;
    const activeValue = String(activeIndex ?? 0);
    const userClass = props.class || '';
    const userStyle = propsToStyle(props);

    return h(Tabs.Root, {
        value: activeValue,
        className: userClass || undefined,
        style: userStyle,
        onValueChange: (val: string) => {
            if (onChange != null) emit(onChange, JSON.stringify({ Value: parseInt(val, 10) }));
        },
    },
        h(Tabs.List, {
            className: [
                'inline-flex h-9 items-center justify-center rounded-lg bg-muted p-1 text-muted-foreground',
            ].join(' '),
        },
            ...tabItems.map((item, i) =>
                h(Tabs.Trigger, {
                    key: String(i),
                    value: String(i),
                    className: [
                        'inline-flex items-center justify-center whitespace-nowrap rounded-md px-3 py-1 text-sm font-medium',
                        'ring-offset-background transition-all focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring',
                        'disabled:pointer-events-none disabled:opacity-50',
                        'data-[state=active]:bg-background data-[state=active]:text-foreground data-[state=active]:shadow',
                    ].join(' '),
                }, item.label),
            ),
        ),
        ...tabItems.map((item, i) =>
            h(Tabs.Content, {
                key: String(i),
                value: String(i),
                className: 'mt-2 ring-offset-background focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring',
            },
                item.content ? voNodeToVNode(item.content) : null,
            ),
        ),
    );
}
