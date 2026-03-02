import { h } from 'preact';
import * as Collapsible from '@radix-ui/react-collapsible';
import { emit } from '../events';
import { propsToStyle } from '../mapping';
import { voNodeToVNode } from '../renderer';
import type { VoNode } from '../types';

export function VgCollapsible(props: any) {
    const { open, defaultOpen, onChange, voChildren } = props;
    const children = (voChildren || []) as VoNode[];
    // First child is the trigger/header, rest is collapsible content
    const trigger = children[0];
    const content = children.slice(1);

    const userClass = props.class || '';
    const userStyle = propsToStyle(props);

    // Controlled mode: open is explicitly set (not undefined)
    // Uncontrolled mode: use defaultOpen, let Radix manage state
    const rootProps: Record<string, any> = {
        className: userClass || undefined,
        style: userStyle,
        onOpenChange: (val: boolean) => {
            if (onChange != null) emit(onChange, JSON.stringify({ Checked: val }));
        },
    };
    if (open != null) {
        rootProps.open = Boolean(open);
    } else if (defaultOpen) {
        rootProps.defaultOpen = true;
    }

    return h(Collapsible.Root, rootProps,
        h(Collapsible.Trigger, { asChild: true },
            trigger ? voNodeToVNode(trigger) : h('button', null, 'Toggle'),
        ),
        h(Collapsible.Content, {
            className: 'overflow-hidden data-[state=closed]:animate-fade-out data-[state=open]:animate-fade-in',
        },
            ...content.map(voNodeToVNode),
        ),
    );
}
