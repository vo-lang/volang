import { h } from 'preact';
import * as Dialog from '@radix-ui/react-dialog';
import { emit } from '../events';
import { propsToStyle } from '../mapping';
import { voNodeToVNode } from '../renderer';
import type { VoNode } from '../types';

export function VgDialog(props: any) {
    const { open, onClose, voChildren } = props;
    const children = (voChildren || []) as VoNode[];
    const userClass = props.class || '';
    const userStyle = propsToStyle(props);

    // Resolve title: prefer props.title, else extract from vo-dialog-title child
    let title: string | undefined = props.title;
    let bodyChildren = children;
    if (!title) {
        const idx = children.findIndex(c => c?.type === 'vo-dialog-title');
        if (idx >= 0) {
            title = children[idx].props?.textContent as string;
            bodyChildren = children.filter((_, i) => i !== idx);
        }
    }

    return h(Dialog.Root, {
        open: Boolean(open),
        onOpenChange: (val: boolean) => {
            if (!val && onClose != null) emit(onClose, '{}');
        },
    },
        h(Dialog.Portal, null,
            h(Dialog.Overlay, {
                className: 'fixed inset-0 z-50 bg-black/50 data-[state=open]:animate-fade-in data-[state=closed]:animate-fade-out',
            }),
            h(Dialog.Content, {
                className: [
                    'fixed left-1/2 top-1/2 z-50 w-full max-w-lg -translate-x-1/2 -translate-y-1/2',
                    'rounded-lg border bg-background p-6 shadow-lg',
                    'data-[state=open]:animate-scale-in',
                    userClass,
                ].filter(Boolean).join(' '),
                style: userStyle,
            },
                title ? h(Dialog.Title, { className: 'text-lg font-semibold leading-none tracking-tight' }, title) : null,
                h('div', { className: 'mt-4' }, ...bodyChildren.map(voNodeToVNode)),
                h(Dialog.Close, {
                    className: 'absolute right-4 top-4 rounded-sm opacity-70 hover:opacity-100 focus:outline-none focus:ring-1 focus:ring-ring',
                },
                    h('svg', { width: 16, height: 16, viewBox: '0 0 24 24', fill: 'none', stroke: 'currentColor', strokeWidth: 2 },
                        h('path', { d: 'M18 6 6 18' }),
                        h('path', { d: 'm6 6 12 12' }),
                    ),
                ),
            ),
        ),
    );
}
