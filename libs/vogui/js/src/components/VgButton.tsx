import { h } from 'preact';
import { emit } from '../events';
import { propsToStyle } from '../mapping';

const variantClasses: Record<string, string> = {
    default:     'bg-muted text-foreground hover:bg-muted/80',
    primary:     'bg-primary text-primary-foreground hover:bg-primary/90',
    secondary:   'bg-secondary text-secondary-foreground hover:bg-secondary/90',
    destructive: 'bg-danger text-danger-foreground hover:bg-danger/90',
    danger:      'bg-danger text-danger-foreground hover:bg-danger/90',
    error:       'bg-danger text-danger-foreground hover:bg-danger/90',
    outline:     'border border-input bg-transparent hover:bg-accent hover:text-accent-foreground',
    ghost:       'bg-transparent hover:bg-accent hover:text-accent-foreground',
    link:        'text-primary underline-offset-4 hover:underline bg-transparent',
};

const sizeClasses: Record<string, string> = {
    xs: 'h-7 px-2 text-xs rounded-sm',
    sm: 'h-8 px-3 text-xs rounded-sm',
    md: 'h-9 px-4 text-sm rounded-md',
    lg: 'h-10 px-6 text-sm rounded-md',
    xl: 'h-12 px-8 text-base rounded-lg',
    icon: 'h-9 w-9 rounded-md',
};

export function VgButton(props: any) {
    const { textContent, onClick, variant, size, disabled, icon } = props;
    const userClass = props.class || '';
    const userStyle = propsToStyle(props);

    const cls = [
        'inline-flex items-center justify-center font-medium',
        'transition-colors focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring',
        'disabled:pointer-events-none disabled:opacity-50',
        variantClasses[variant || 'default'],
        sizeClasses[size || 'md'],
        userClass,
    ].filter(Boolean).join(' ');

    return h('button', {
        className: cls,
        style: userStyle,
        disabled: disabled || false,
        onClick: onClick != null ? () => emit(onClick, '{}') : undefined,
    }, icon ? h('span', { className: 'vo-icon' }, icon) : null, textContent != null ? String(textContent) : null);
}
