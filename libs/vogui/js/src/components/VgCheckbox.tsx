import { h } from 'preact';
import * as Checkbox from '@radix-ui/react-checkbox';
import { emit } from '../events';
import { propsToStyle } from '../mapping';

export function VgCheckbox(props: any) {
    const { textContent, checked, onChange, disabled } = props;

    const userClass = props.class || '';
    const userStyle = propsToStyle(props);

    return h('label', {
        className: ['flex items-center gap-2 text-sm cursor-pointer', userClass].filter(Boolean).join(' '),
        style: userStyle,
    },
        h(Checkbox.Root, {
            className: [
                'peer h-4 w-4 shrink-0 rounded-sm border border-input shadow',
                'focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring',
                'disabled:cursor-not-allowed disabled:opacity-50',
                'data-[state=checked]:bg-primary data-[state=checked]:text-primary-foreground data-[state=checked]:border-primary',
            ].join(' '),
            checked: Boolean(checked),
            disabled: disabled || false,
            onCheckedChange: (val: boolean) => {
                if (onChange != null) emit(onChange, JSON.stringify({ Checked: val }));
            },
        },
            h(Checkbox.Indicator, { className: 'flex items-center justify-center text-current' },
                h('svg', { width: 12, height: 12, viewBox: '0 0 24 24', fill: 'none', stroke: 'currentColor', strokeWidth: 3, strokeLinecap: 'round', strokeLinejoin: 'round' },
                    h('polyline', { points: '20 6 9 17 4 12' }),
                ),
            ),
        ),
        textContent ? h('span', null, textContent) : null,
    );
}
