import { h } from 'preact';
import * as Switch from '@radix-ui/react-switch';
import { emit } from '../events';
import { propsToStyle } from '../mapping';

export function VgSwitch(props: any) {
    const { textContent, checked, onChange, disabled } = props;
    const userClass = props.class || '';
    const userStyle = propsToStyle(props);

    return h('label', {
        className: ['flex items-center gap-2 text-sm cursor-pointer', userClass].filter(Boolean).join(' '),
        style: userStyle,
    },
        h(Switch.Root, {
            className: [
                'peer inline-flex h-5 w-9 shrink-0 cursor-pointer items-center rounded-full border-2 border-transparent',
                'shadow-sm transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring',
                'disabled:cursor-not-allowed disabled:opacity-50',
                'data-[state=checked]:bg-primary data-[state=unchecked]:bg-muted',
            ].join(' '),
            checked: Boolean(checked),
            disabled: disabled || false,
            onCheckedChange: (val: boolean) => {
                if (onChange != null) emit(onChange, JSON.stringify({ Checked: val }));
            },
        },
            h(Switch.Thumb, {
                className: [
                    'pointer-events-none block h-4 w-4 rounded-full bg-background shadow-lg ring-0 transition-transform',
                    'data-[state=checked]:translate-x-4 data-[state=unchecked]:translate-x-0',
                ].join(' '),
            }),
        ),
        textContent ? h('span', null, textContent) : null,
    );
}
