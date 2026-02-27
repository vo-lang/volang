// Binary render protocol decoder.
// Mirrors the format produced by libs/vogui/encode.vo (little-endian).
//
// Node tags: 0=null 1=element 2=text 3=fragment
// Value tags: 0=null 1=bool 2=int 3=float64 4=string 5=map 6=array 7=node

import type { VoNode, VoHandler, RenderMessage, CanvasBatch, CanvasCommand } from './types';

const utf8Dec = new TextDecoder('utf-8');

class BinReader {
    private view: DataView;
    private bytes: Uint8Array;
    pos = 0;

    constructor(data: Uint8Array) {
        this.bytes = data;
        this.view = new DataView(data.buffer, data.byteOffset, data.byteLength);
    }

    u8(): number {
        return this.view.getUint8(this.pos++);
    }

    u16(): number {
        const v = this.view.getUint16(this.pos, true);
        this.pos += 2;
        return v;
    }

    u32(): number {
        const v = this.view.getUint32(this.pos, true);
        this.pos += 4;
        return v;
    }

    i32(): number {
        const v = this.view.getInt32(this.pos, true);
        this.pos += 4;
        return v;
    }

    f64(): number {
        const v = this.view.getFloat64(this.pos, true);
        this.pos += 8;
        return v;
    }

    str(): string {
        const len = this.u16();
        const slice = this.bytes.subarray(this.pos, this.pos + len);
        this.pos += len;
        return utf8Dec.decode(slice);
    }

    value(): any {
        const tag = this.u8();
        switch (tag) {
            case 0: return null;
            case 1: return this.u8() !== 0;
            case 2: return this.i32();
            case 3: return this.f64();
            case 4: return this.str();
            case 5: {
                const n = this.u16();
                const m: Record<string, any> = {};
                for (let i = 0; i < n; i++) {
                    const k = this.str();
                    m[k] = this.value();
                }
                return m;
            }
            case 6: {
                const n = this.u32();
                const arr: any[] = new Array(n);
                for (let i = 0; i < n; i++) arr[i] = this.value();
                return arr;
            }
            case 7: return this.node();
            default: return null;
        }
    }

    node(): VoNode | null {
        const tag = this.u8();
        if (tag === 0) return null;
        if (tag === 2) {
            return { type: '#text', props: { text: this.str() }, children: [] };
        }
        if (tag === 3) {
            const n = this.u16();
            const children: VoNode[] = [];
            for (let i = 0; i < n; i++) {
                const child = this.node();
                if (child) children.push(child);
            }
            return { type: 'Fragment', props: {}, children };
        }
        if (tag === 4) {
            // binNodeComponent: u32 componentID + subtree node
            const cid = this.u32();
            const child = this.node();
            return { type: '__comp__', props: { _cid: cid }, children: child ? [child] : [] };
        }
        if (tag === 5) {
            // binNodeCached: u32 componentID; JS reuses stored DOM subtree
            const cid = this.u32();
            return { type: '__cached__', props: { _cid: cid }, children: [] };
        }
        // tag === 1: element
        const type = this.str();
        const propCount = this.u16();
        const props: Record<string, any> = {};
        for (let i = 0; i < propCount; i++) {
            const k = this.str();
            props[k] = this.value();
        }
        const childCount = this.u32();
        const children: VoNode[] = [];
        for (let i = 0; i < childCount; i++) {
            const child = this.node();
            if (child) children.push(child);
        }
        return { type, props, children };
    }

    handler(): VoHandler {
        const iD = this.u16();
        const gen = this.u16();
        const type = this.u8();
        const intVal = this.i32();
        const modCount = this.u8();
        const modifiers: string[] = [];
        for (let i = 0; i < modCount; i++) modifiers.push(this.str());
        const keyFilter = this.str();
        return {
            iD,
            gen,
            type,
            intVal,
            modifiers: modifiers.length > 0 ? modifiers : undefined,
            keyFilter: keyFilter || undefined,
        };
    }
}

/** Decode a binary render message produced by VoGUI's encode.vo. */
export function decodeBinaryRender(data: Uint8Array): RenderMessage {
    const r = new BinReader(data);

    const gen = r.u32();
    const flags = r.u8();

    const tree = r.node()!;

    const handlerCount = r.u16();
    const handlers: VoHandler[] = new Array(handlerCount);
    for (let i = 0; i < handlerCount; i++) {
        handlers[i] = r.handler();
    }

    let styles: string[] | undefined;
    if (flags & 1) {
        const n = r.u16();
        styles = new Array(n);
        for (let i = 0; i < n; i++) styles[i] = r.str();
    }

    let canvas: CanvasBatch[] | undefined;
    if (flags & 2) {
        const n = r.u16();
        canvas = new Array(n);
        for (let i = 0; i < n; i++) {
            const ref = r.str();
            const cmdCount = r.u32();
            const cmds: CanvasCommand[] = new Array(cmdCount);
            for (let j = 0; j < cmdCount; j++) {
                const c = r.str();
                const argCount = r.u8();
                if (argCount > 0) {
                    const a: any[] = new Array(argCount);
                    for (let k = 0; k < argCount; k++) a[k] = r.value();
                    cmds[j] = { c, a };
                } else {
                    cmds[j] = { c };
                }
            }
            canvas[i] = { ref, cmds };
        }
    }

    return { type: 'render', gen, tree, handlers, styles, canvas };
}
