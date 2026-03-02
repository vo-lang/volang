import { h } from 'preact';
import * as Slider from '@radix-ui/react-slider';
import { emit } from '../events';
import { propsToStyle } from '../mapping';

export function VgSlider(props: any) {
    const { value, min, max, onChange, disabled } = props;
    const minVal = min ?? 0;
    const maxVal = max ?? 100;
    const curVal = value ?? 0;
    const userClass = props.class || '';
    const userStyle = propsToStyle(props);

    return h(Slider.Root, {
        className: ['relative flex w-full touch-none select-none items-center', userClass].filter(Boolean).join(' '),
        style: userStyle,
        value: [curVal],
        min: minVal,
        max: maxVal,
        disabled: disabled || false,
        onValueChange: (vals: number[]) => {
            if (onChange != null) emit(onChange, JSON.stringify({ Value: vals[0] }));
        },
    },
        h(Slider.Track, {
            className: 'relative h-1.5 w-full grow overflow-hidden rounded-full bg-muted',
        },
            h(Slider.Range, {
                className: 'absolute h-full bg-primary',
            }),
        ),
        h(Slider.Thumb, {
            className: [
                'block h-4 w-4 rounded-full border border-primary/50 bg-background shadow',
                'transition-colors focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring',
                'disabled:pointer-events-none disabled:opacity-50',
            ].join(' '),
        }),
    );
}
