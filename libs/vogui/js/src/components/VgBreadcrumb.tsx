import { h } from 'preact';
import { propsToStyle } from '../mapping';

export function VgBreadcrumb(props: any) {
    const { items } = props;
    const crumbs = (items || []) as Array<{ label: string; href?: string }>;
    const userClass = props.class || '';
    const userStyle = propsToStyle(props);

    return h('nav', {
        className: ['flex items-center text-sm text-muted-foreground', userClass].filter(Boolean).join(' '),
        style: userStyle,
        'aria-label': 'breadcrumb',
    },
        h('ol', { className: 'flex items-center gap-1.5' },
            ...crumbs.map((item, i) => {
                const isLast = i === crumbs.length - 1;
                const parts: any[] = [];

                if (i > 0) {
                    parts.push(h('li', {
                        key: `sep-${i}`,
                        className: 'text-muted-foreground/50 select-none',
                        'aria-hidden': 'true',
                    }, '/'));
                }

                if (isLast) {
                    parts.push(h('li', {
                        key: `item-${i}`,
                        className: 'font-medium text-foreground',
                        'aria-current': 'page',
                    }, item.label));
                } else {
                    parts.push(h('li', { key: `item-${i}` },
                        h('a', {
                            href: item.href || '#',
                            className: 'transition-colors hover:text-foreground',
                        }, item.label),
                    ));
                }

                return parts;
            }).flat(),
        ),
    );
}
