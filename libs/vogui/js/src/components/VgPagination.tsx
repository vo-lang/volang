import { h } from 'preact';
import { emit } from '../events';
import { propsToStyle } from '../mapping';

export function VgPagination(props: any) {
    const { current, total, onChange } = props;
    const cur = (current as number) ?? 1;
    const tot = (total as number) ?? 1;
    const userClass = props.class || '';
    const userStyle = propsToStyle(props);

    const btnClass = (active: boolean, disabled: boolean) => [
        'inline-flex items-center justify-center h-8 min-w-[2rem] px-2 text-sm rounded-md',
        'transition-colors focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring',
        active ? 'bg-primary text-primary-foreground' : 'hover:bg-accent hover:text-accent-foreground',
        disabled ? 'pointer-events-none opacity-50' : 'cursor-pointer',
    ].filter(Boolean).join(' ');

    const onPage = (page: number) => {
        if (onChange != null) emit(onChange, JSON.stringify({ Value: page }));
    };

    const buttons: any[] = [];

    // Previous
    buttons.push(h('button', {
        key: 'prev',
        className: btnClass(false, cur <= 1),
        disabled: cur <= 1,
        onClick: () => onPage(cur - 1),
    }, '\u00AB'));

    // Page range
    const start = Math.max(1, cur - 2);
    const end = Math.min(tot, cur + 2);

    if (start > 1) {
        buttons.push(h('button', { key: 'p1', className: btnClass(cur === 1, false), onClick: () => onPage(1) }, '1'));
        if (start > 2) {
            buttons.push(h('span', { key: 'ell1', className: 'px-1 text-muted-foreground' }, '\u2026'));
        }
    }

    for (let p = start; p <= end; p++) {
        buttons.push(h('button', {
            key: `p${p}`,
            className: btnClass(p === cur, false),
            onClick: () => onPage(p),
        }, String(p)));
    }

    if (end < tot) {
        if (end < tot - 1) {
            buttons.push(h('span', { key: 'ell2', className: 'px-1 text-muted-foreground' }, '\u2026'));
        }
        buttons.push(h('button', { key: `p${tot}`, className: btnClass(cur === tot, false), onClick: () => onPage(tot) }, String(tot)));
    }

    // Next
    buttons.push(h('button', {
        key: 'next',
        className: btnClass(false, cur >= tot),
        disabled: cur >= tot,
        onClick: () => onPage(cur + 1),
    }, '\u00BB'));

    return h('nav', {
        className: ['flex items-center gap-1', userClass].filter(Boolean).join(' '),
        style: userStyle,
        'aria-label': 'pagination',
    }, ...buttons);
}
