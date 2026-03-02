import { h } from 'preact';
import * as Dialog from '@radix-ui/react-dialog';
import { emit } from '../events';
import { propsToStyle } from '../mapping';
import { voNodeToVNode } from '../renderer';
import type { VoNode } from '../types';

export function VgDrawer(props: any) {
    const { open, onClose, side, voChildren } = props;
    const children = (voChildren || []) as VoNode[];
    const drawerSide = side || 'right';
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

    const sideClasses: Record<string, string> = {
        left: 'inset-y-0 left-0 w-3/4 max-w-sm data-[state=open]:animate-slide-in-from-left',
        right: 'inset-y-0 right-0 w-3/4 max-w-sm data-[state=open]:animate-slide-in-from-right',
        top: 'inset-x-0 top-0 h-auto data-[state=open]:animate-slide-in-from-top',
        bottom: 'inset-x-0 bottom-0 h-auto data-[state=open]:animate-slide-in-from-bottom',
    };

    return h(Dialog.Root, {
        open: Boolean(open),
        onOpenChange: (val: boolean) => {
            if (!val && onClose != null) emit(onClose, '{}');
        },
    },
        h(Dialog.Portal, null,
            h(Dialog.Overlay, {
                className: 'fixed inset-0 z-50 bg-black/50 data-[state=open]:animate-fade-in',
            }),
            h(Dialog.Content, {
                className: [
                    'fixed z-50 border bg-background p-6 shadow-lg',
                    sideClasses[drawerSide] || sideClasses.right,
                    userClass,
                ].filter(Boolean).join(' '),
                style: userStyle,
            },
                title ? h(Dialog.Title, { className: 'text-lg font-semibold' }, title) : null,
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
