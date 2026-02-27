// VoGUI Canvas 2D command executor.
// Receives batched draw commands from Vo and executes them on canvas contexts.

import { CanvasBatch, CanvasCommand } from './types';

// Image cache for DrawImage commands
const imageCache = new Map<string, HTMLImageElement>();
const imagePending = new Set<string>();

function getImage(url: string): HTMLImageElement | null {
    const cached = imageCache.get(url);
    if (cached) return cached;
    if (imagePending.has(url)) return null;

    imagePending.add(url);
    const img = new Image();
    img.onload = () => {
        imageCache.set(url, img);
        imagePending.delete(url);
    };
    img.onerror = () => {
        imagePending.delete(url);
        console.warn(`VoGUI: Failed to load image: ${url}`);
    };
    img.src = url;
    return null;
}

/** Execute a batch of canvas commands on the target canvas element. */
export function executeCanvasBatch(batch: CanvasBatch, refRegistry: Map<string, HTMLElement>): void {
    const el = refRegistry.get(batch.ref);
    if (!el || !(el instanceof HTMLCanvasElement)) {
        console.warn(`VoGUI: Canvas ref "${batch.ref}" not found or not a canvas element`);
        return;
    }
    const ctx = el.getContext('2d');
    if (!ctx) return;

    for (const cmd of batch.cmds) {
        executeCommand(ctx, el, cmd);
    }
}

function executeCommand(ctx: CanvasRenderingContext2D, canvas: HTMLCanvasElement, cmd: CanvasCommand): void {
    const a = cmd.a || [];
    switch (cmd.c) {
        // State
        case 'clear':
            ctx.clearRect(0, 0, canvas.width, canvas.height);
            break;
        case 'fill':
            ctx.fillStyle = a[0] as string;
            break;
        case 'stroke':
            ctx.strokeStyle = a[0] as string;
            break;
        case 'lw':
            ctx.lineWidth = a[0] as number;
            break;
        case 'font':
            ctx.font = a[0] as string;
            break;
        case 'alpha':
            ctx.globalAlpha = a[0] as number;
            break;
        case 'ta':
            ctx.textAlign = a[0] as CanvasTextAlign;
            break;
        case 'tb':
            ctx.textBaseline = a[0] as CanvasTextBaseline;
            break;
        case 'lc':
            ctx.lineCap = a[0] as CanvasLineCap;
            break;
        case 'lj':
            ctx.lineJoin = a[0] as CanvasLineJoin;
            break;
        case 'shadow':
            ctx.shadowOffsetX = a[0] as number;
            ctx.shadowOffsetY = a[1] as number;
            ctx.shadowBlur = a[2] as number;
            ctx.shadowColor = a[3] as string;
            break;

        // Shapes
        case 'fr':
            ctx.fillRect(a[0], a[1], a[2], a[3]);
            break;
        case 'sr':
            ctx.strokeRect(a[0], a[1], a[2], a[3]);
            break;
        case 'cr':
            ctx.clearRect(a[0], a[1], a[2], a[3]);
            break;
        case 'fc':
            ctx.beginPath();
            ctx.arc(a[0], a[1], a[2], 0, Math.PI * 2);
            ctx.fill();
            break;
        case 'sc':
            ctx.beginPath();
            ctx.arc(a[0], a[1], a[2], 0, Math.PI * 2);
            ctx.stroke();
            break;
        case 'frr':
            roundRect(ctx, a[0], a[1], a[2], a[3], a[4]);
            ctx.fill();
            break;
        case 'srr':
            roundRect(ctx, a[0], a[1], a[2], a[3], a[4]);
            ctx.stroke();
            break;
        case 'fe':
            ctx.beginPath();
            ctx.ellipse(a[0], a[1], a[2], a[3], 0, 0, Math.PI * 2);
            ctx.fill();
            break;
        case 'se':
            ctx.beginPath();
            ctx.ellipse(a[0], a[1], a[2], a[3], 0, 0, Math.PI * 2);
            ctx.stroke();
            break;

        // Path
        case 'bp':
            ctx.beginPath();
            break;
        case 'mt':
            ctx.moveTo(a[0], a[1]);
            break;
        case 'lt':
            ctx.lineTo(a[0], a[1]);
            break;
        case 'at':
            ctx.arcTo(a[0], a[1], a[2], a[3], a[4]);
            break;
        case 'arc':
            ctx.arc(a[0], a[1], a[2], a[3], a[4]);
            break;
        case 'qct':
            ctx.quadraticCurveTo(a[0], a[1], a[2], a[3]);
            break;
        case 'bct':
            ctx.bezierCurveTo(a[0], a[1], a[2], a[3], a[4], a[5]);
            break;
        case 'cp':
            ctx.closePath();
            break;
        case 'f':
            ctx.fill();
            break;
        case 's':
            ctx.stroke();
            break;
        case 'clip':
            ctx.clip();
            break;

        // Text
        case 'ft':
            ctx.fillText(a[0] as string, a[1], a[2]);
            break;
        case 'st':
            ctx.strokeText(a[0] as string, a[1], a[2]);
            break;

        // Images
        case 'di': {
            const img = getImage(a[0] as string);
            if (img) ctx.drawImage(img, a[1], a[2]);
            break;
        }
        case 'dis': {
            const img = getImage(a[0] as string);
            if (img) ctx.drawImage(img, a[1], a[2], a[3], a[4]);
            break;
        }
        case 'disub': {
            const img = getImage(a[0] as string);
            if (img) ctx.drawImage(img, a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
            break;
        }

        // Transform
        case 'save':
            ctx.save();
            break;
        case 'rest':
            ctx.restore();
            break;
        case 'tr':
            ctx.translate(a[0], a[1]);
            break;
        case 'rot':
            ctx.rotate(a[0]);
            break;
        case 'scl':
            ctx.scale(a[0], a[1]);
            break;
        case 'rt':
            ctx.resetTransform();
            break;

        // Gradients
        case 'lg': {
            const grad = ctx.createLinearGradient(a[0], a[1], a[2], a[3]);
            const stops = a[4] as Array<{ offset: number; color: string }>;
            for (const stop of stops) {
                grad.addColorStop(stop.offset, stop.color);
            }
            ctx.fillStyle = grad;
            break;
        }
        case 'rg': {
            const grad = ctx.createRadialGradient(a[0], a[1], a[2], a[3], a[4], a[5]);
            const stops = a[6] as Array<{ offset: number; color: string }>;
            for (const stop of stops) {
                grad.addColorStop(stop.offset, stop.color);
            }
            ctx.fillStyle = grad;
            break;
        }

        default:
            console.warn(`VoGUI: Unknown canvas command: ${cmd.c}`);
    }
}

function roundRect(ctx: CanvasRenderingContext2D, x: number, y: number, w: number, h: number, r: number): void {
    ctx.beginPath();
    ctx.moveTo(x + r, y);
    ctx.lineTo(x + w - r, y);
    ctx.arcTo(x + w, y, x + w, y + r, r);
    ctx.lineTo(x + w, y + h - r);
    ctx.arcTo(x + w, y + h, x + w - r, y + h, r);
    ctx.lineTo(x + r, y + h);
    ctx.arcTo(x, y + h, x, y + h - r, r);
    ctx.lineTo(x, y + r);
    ctx.arcTo(x, y, x + r, y, r);
    ctx.closePath();
}
