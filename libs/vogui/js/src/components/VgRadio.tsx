import { h } from 'preact';
import { emit } from '../events';
import { propsToStyle } from '../mapping';

export function VgRadio(props: any) {
    const { textContent, value, selected, onChange, disabled, name } = props;
    const isChecked = String(value ?? '') === String(selected ?? '');
    const userClass = props.class || '';
    const userStyle = propsToStyle(props);

    return h('label', {
        className: [
            'flex items-center gap-2 text-sm cursor-pointer',
            disabled ? 'opacity-50 cursor-not-allowed' : '',
            userClass,
        ].filter(Boolean).join(' '),
        style: userStyle,
    },
        h('input', {
            type: 'radio',
            className: [
                'h-4 w-4 shrink-0 rounded-full border border-input text-primary',
                'focus:outline-none focus:ring-1 focus:ring-ring',
                'disabled:cursor-not-allowed',
            ].join(' '),
            name: name || undefined,
            value: value ?? '',
            checked: isChecked,
            disabled: disabled || false,
            onChange: () => {
                if (onChange != null) emit(onChange, JSON.stringify({ Value: String(value ?? '') }));
            },
        }),
        textContent ? h('span', null, textContent) : null,
    );
}
