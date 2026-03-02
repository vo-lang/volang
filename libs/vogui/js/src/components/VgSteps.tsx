import { h } from 'preact';
import { propsToStyle } from '../mapping';

export function VgSteps(props: any) {
    const { current, items } = props;
    const curStep = (current as number) ?? 0;
    const steps = (items || []) as Array<{ label: string; description?: string }>;
    const userClass = props.class || '';
    const userStyle = propsToStyle(props);

    return h('div', {
        className: ['flex items-start gap-4', userClass].filter(Boolean).join(' '),
        style: userStyle,
    },
        ...steps.map((step, i) => {
            const state = i < curStep ? 'completed' : i === curStep ? 'active' : 'pending';

            const indicatorClass = [
                'flex items-center justify-center h-8 w-8 rounded-full text-sm font-medium shrink-0',
                state === 'completed' ? 'bg-primary text-primary-foreground' : '',
                state === 'active' ? 'border-2 border-primary text-primary' : '',
                state === 'pending' ? 'border-2 border-muted text-muted-foreground' : '',
            ].filter(Boolean).join(' ');

            const labelClass = [
                'text-sm font-medium',
                state === 'completed' ? 'text-foreground' : '',
                state === 'active' ? 'text-foreground' : '',
                state === 'pending' ? 'text-muted-foreground' : '',
            ].filter(Boolean).join(' ');

            return h('div', { key: String(i), className: 'flex flex-col items-center gap-1.5 flex-1' },
                h('div', { className: indicatorClass },
                    state === 'completed'
                        ? h('svg', { width: 14, height: 14, viewBox: '0 0 24 24', fill: 'none', stroke: 'currentColor', strokeWidth: 2.5 },
                            h('polyline', { points: '20 6 9 17 4 12' }),
                        )
                        : String(i + 1),
                ),
                h('div', { className: labelClass }, step.label),
                step.description
                    ? h('div', { className: 'text-xs text-muted-foreground text-center' }, step.description)
                    : null,
            );
        }),
    );
}
