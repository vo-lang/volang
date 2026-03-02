import { h } from 'preact';
import { useState, useRef, useEffect } from 'preact/hooks';
import { emit } from '../events';
import { propsToStyle } from '../mapping';
import { voNodeToVNode } from '../renderer';
import type { VoNode } from '../types';

// Combobox: a filterable select. Radix doesn't ship one yet, so this is custom
// with proper keyboard navigation and ARIA.
export function VgCombobox(props: any) {
    const { value, placeholder, voChildren } = props;
    const onChange = props.onSelect ?? props.onChange;
    const children = (voChildren || []) as VoNode[];

    // Extract options from vo-combobox-option children
    const options: Array<{ label: string; value: string }> = children
        .filter(c => c.type === 'vo-combobox-option')
        .map(c => ({
            label: (c.props?.textContent || c.props?.label || '') as string,
            value: (c.props?.value || c.props?.textContent || '') as string,
        }));

    const [open, setOpen] = useState(false);
    const [search, setSearch] = useState('');
    const [highlightIdx, setHighlightIdx] = useState(0);
    const inputRef = useRef<HTMLInputElement>(null);
    const listRef = useRef<HTMLDivElement>(null);

    const filtered = search
        ? options.filter(o => o.label.toLowerCase().includes(search.toLowerCase()))
        : options;

    useEffect(() => {
        setHighlightIdx(0);
    }, [search]);

    const selectItem = (val: string) => {
        setOpen(false);
        setSearch('');
        if (onChange != null) emit(onChange, JSON.stringify({ Value: val }));
    };

    const selectedLabel = options.find(o => o.value === value)?.label || '';

    const onKeyDown = (e: KeyboardEvent) => {
        if (e.key === 'ArrowDown') {
            e.preventDefault();
            setHighlightIdx(i => Math.min(i + 1, filtered.length - 1));
        } else if (e.key === 'ArrowUp') {
            e.preventDefault();
            setHighlightIdx(i => Math.max(i - 1, 0));
        } else if (e.key === 'Enter') {
            e.preventDefault();
            if (filtered[highlightIdx]) selectItem(filtered[highlightIdx].value);
        } else if (e.key === 'Escape') {
            setOpen(false);
        }
    };

    const userClass = props.class || '';
    const userStyle = propsToStyle(props);

    return h('div', {
        className: ['relative w-full', userClass].filter(Boolean).join(' '),
        style: userStyle,
    },
        h('button', {
            className: [
                'flex h-9 w-full items-center justify-between rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm',
                'hover:bg-accent focus:outline-none focus:ring-1 focus:ring-ring',
            ].join(' '),
            onClick: () => {
                setOpen(!open);
                setTimeout(() => inputRef.current?.focus(), 0);
            },
            type: 'button',
        },
            h('span', { className: value ? '' : 'text-muted-foreground' },
                selectedLabel || placeholder || 'Select...',
            ),
            h('svg', { width: 12, height: 12, viewBox: '0 0 24 24', fill: 'none', stroke: 'currentColor', strokeWidth: 2, className: 'ml-2 opacity-50' },
                h('path', { d: 'm6 9 6 6 6-6' }),
            ),
        ),
        open ? h('div', {
            className: [
                'absolute z-50 mt-1 w-full rounded-md border bg-popover text-popover-foreground shadow-md',
                'animate-scale-in',
            ].join(' '),
        },
            h('div', { className: 'flex items-center border-b px-3' },
                h('svg', { width: 14, height: 14, viewBox: '0 0 24 24', fill: 'none', stroke: 'currentColor', strokeWidth: 2, className: 'mr-2 shrink-0 opacity-50' },
                    h('circle', { cx: 11, cy: 11, r: 8 }),
                    h('path', { d: 'm21 21-4.3-4.3' }),
                ),
                h('input', {
                    ref: inputRef,
                    className: 'flex h-9 w-full bg-transparent py-2 text-sm outline-none placeholder:text-muted-foreground',
                    placeholder: 'Search...',
                    value: search,
                    onInput: (e: Event) => setSearch((e.target as HTMLInputElement).value),
                    onKeyDown,
                }),
            ),
            h('div', {
                ref: listRef,
                className: 'max-h-60 overflow-auto p-1',
                role: 'listbox',
            },
                filtered.length === 0
                    ? h('div', { className: 'py-6 text-center text-sm text-muted-foreground' }, 'No results.')
                    : filtered.map((item, i) =>
                        h('div', {
                            key: item.value,
                            className: [
                                'relative flex cursor-default select-none items-center rounded-sm px-2 py-1.5 text-sm outline-none',
                                i === highlightIdx ? 'bg-accent text-accent-foreground' : '',
                                item.value === value ? 'font-medium' : '',
                            ].join(' '),
                            role: 'option',
                            'aria-selected': item.value === value,
                            onMouseEnter: () => setHighlightIdx(i),
                            onClick: () => selectItem(item.value),
                        },
                            item.value === value
                                ? h('svg', { width: 12, height: 12, viewBox: '0 0 24 24', fill: 'none', stroke: 'currentColor', strokeWidth: 2, className: 'mr-2' },
                                    h('polyline', { points: '20 6 9 17 4 12' }),
                                )
                                : h('span', { className: 'mr-2 w-3' }),
                            item.label,
                        ),
                    ),
            ),
        ) : null,
    );
}
