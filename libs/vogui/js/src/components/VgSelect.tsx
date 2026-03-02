import { h } from 'preact';
import * as Select from '@radix-ui/react-select';
import { emit } from '../events';
import { propsToStyle } from '../mapping';

export function VgSelect(props: any): any {
    const { value, options, onChange, placeholder, disabled } = props;
    const items = (options || []) as Array<{ label: string; value: string }>;
    const userClass = props.class || '';
    const userStyle = propsToStyle(props);

    return h(Select.Root, {
        value: value != null ? String(value) : undefined,
        onValueChange: (v: string) => {
            if (onChange != null) emit(onChange, JSON.stringify({ Value: v }));
        },
        disabled: disabled || false,
    },
        h(Select.Trigger, {
            className: [
                'flex h-9 w-full items-center justify-between rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm',
                'placeholder:text-muted-foreground focus:outline-none focus:ring-1 focus:ring-ring',
                'disabled:cursor-not-allowed disabled:opacity-50',
                userClass,
            ].filter(Boolean).join(' '),
            style: userStyle,
        },
            h(Select.Value, { placeholder: placeholder || 'Select...' }),
            h(Select.Icon, { className: 'ml-2 opacity-50' },
                h('svg', { width: 12, height: 12, viewBox: '0 0 24 24', fill: 'none', stroke: 'currentColor', strokeWidth: 2 },
                    h('path', { d: 'm6 9 6 6 6-6' }),
                ),
            ),
        ),
        h(Select.Portal, null,
            h(Select.Content, {
                className: [
                    'relative z-50 max-h-96 min-w-[8rem] overflow-hidden rounded-md border bg-popover text-popover-foreground shadow-md',
                    'animate-scale-in',
                ].join(' '),
                position: 'popper',
                sideOffset: 4,
            },
                h(Select.Viewport, { className: 'p-1' },
                    items.map((item) =>
                        h(Select.Item, {
                            key: item.value,
                            value: item.value,
                            className: [
                                'relative flex w-full cursor-default select-none items-center rounded-sm py-1.5 pl-2 pr-8 text-sm outline-none',
                                'focus:bg-accent focus:text-accent-foreground',
                                'data-[disabled]:pointer-events-none data-[disabled]:opacity-50',
                            ].join(' '),
                        },
                            h(Select.ItemText, null, item.label),
                            h(Select.ItemIndicator, { className: 'absolute right-2 flex h-3.5 w-3.5 items-center justify-center' },
                                h('svg', { width: 12, height: 12, viewBox: '0 0 24 24', fill: 'none', stroke: 'currentColor', strokeWidth: 2 },
                                    h('polyline', { points: '20 6 9 17 4 12' }),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ),
    );
}
