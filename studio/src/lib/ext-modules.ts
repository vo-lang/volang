// =============================================================================
// WASM Extension Module Bridge
//
// Manages loading, calling, and replaying external WASM modules (.wasm
// extensions like zip, vox, vogui).  Provides three window globals that the
// Rust ext_bridge calls into:
//
//   voSetupExtModule  — instantiate a .wasm extension (bindgen or standalone)
//   voCallExt         — call an extern function on a loaded extension
//   voCallExtReplay   — replay a suspended host-event call with resume data
// =============================================================================

// ── State ────────────────────────────────────────────────────────────────────

const extInstances = new Map<string, WebAssembly.Instance>();
const extBindgenModules = new Map<string, any>();

// ── Host import builder ──────────────────────────────────────────────────────

/** Build the WebAssembly.Imports object for a standalone C-ABI ext module. */
function buildHostImports(
  getInstance: () => WebAssembly.Instance | undefined,
  getMem: () => WebAssembly.Memory,
): WebAssembly.Imports {
  const w = window as any;
  const decoder = new TextDecoder();
  const encoder = new TextEncoder();

  const memStr = (mem: WebAssembly.Memory, ptr: number, len: number): string =>
    decoder.decode(new Uint8Array(mem.buffer, ptr, len));

  return {
    env: {
      host_start_timeout:     (id: number, ms: number) => { w.startTimeout(id, ms); },
      host_clear_timeout:     (id: number) => { w.clearTimeout(id); },
      host_start_interval:    (id: number, ms: number) => { w.startInterval(id, ms); },
      host_clear_interval:    (id: number) => { w.clearInterval(id); },
      host_navigate:          (ptr: number, len: number) => { w.navigate(memStr(getMem(), ptr, len)); },
      host_get_current_path:  (outLenPtr: number): number => {
        const path = String(w.getCurrentPath?.() ?? '/');
        const enc = encoder.encode(path);
        const exp = getInstance()!.exports as any;
        const dst: number = exp.vo_alloc(enc.length);
        new Uint8Array(getMem().buffer).set(enc, dst);
        new DataView(getMem().buffer).setUint32(outLenPtr, enc.length, true);
        return dst;
      },
      host_focus:             (ptr: number, len: number) => { w.voguiFocus(memStr(getMem(), ptr, len)); },
      host_blur:              (ptr: number, len: number) => { w.voguiBlur(memStr(getMem(), ptr, len)); },
      host_scroll_to:         (ptr: number, len: number, top: number) => { w.voguiScrollTo(memStr(getMem(), ptr, len), top); },
      host_scroll_into_view:  (ptr: number, len: number) => { w.voguiScrollIntoView(memStr(getMem(), ptr, len)); },
      host_select_text:       (ptr: number, len: number) => { w.voguiSelectText(memStr(getMem(), ptr, len)); },
      host_set_title:         (ptr: number, len: number) => { w.voguiSetTitle(memStr(getMem(), ptr, len)); },
      host_set_meta:          (np: number, nl: number, cp: number, cl: number) => { w.voguiSetMeta(memStr(getMem(), np, nl), memStr(getMem(), cp, cl)); },
      host_toast:             (mp: number, ml: number, tp: number, tl: number, dur: number) => { w.voguiToast(memStr(getMem(), mp, ml), memStr(getMem(), tp, tl), dur); },
      host_start_anim_frame:  (id: number) => { w.voguiStartAnimFrame(id); },
      host_cancel_anim_frame: (id: number) => { w.voguiCancelAnimFrame(id); },
      host_start_game_loop:   (id: number) => { w.voguiStartGameLoop(id); },
      host_stop_game_loop:    (id: number) => { w.voguiStopGameLoop(id); },

      // ── vox host imports ──────────────────────────────────────────
      host_compile: (pathPtr: number, pathLen: number, okPtr: number, outLenPtr: number): number => {
        const exp = getInstance()!.exports as any;
        const mem = getMem();
        const dv = new DataView(mem.buffer);
        const path = memStr(mem, pathPtr, pathLen);
        try {
          const bytecode: Uint8Array = w.voHostCompileFile(path);
          const dst: number = exp.vo_alloc(bytecode.length);
          new Uint8Array(mem.buffer).set(bytecode, dst);
          dv.setUint32(okPtr, 1, true);
          dv.setUint32(outLenPtr, bytecode.length, true);
          return dst;
        } catch (e: any) {
          const msg = encoder.encode(String(e?.message ?? e));
          const dst: number = exp.vo_alloc(msg.length);
          new Uint8Array(mem.buffer).set(msg, dst);
          dv.setUint32(okPtr, 0, true);
          dv.setUint32(outLenPtr, msg.length, true);
          return dst;
        }
      },

      host_compile_string: (codePtr: number, codeLen: number, okPtr: number, outLenPtr: number): number => {
        const exp = getInstance()!.exports as any;
        const mem = getMem();
        const dv = new DataView(mem.buffer);
        const code = memStr(mem, codePtr, codeLen);
        try {
          const bytecode: Uint8Array = w.voHostCompileString(code);
          const dst: number = exp.vo_alloc(bytecode.length);
          new Uint8Array(mem.buffer).set(bytecode, dst);
          dv.setUint32(okPtr, 1, true);
          dv.setUint32(outLenPtr, bytecode.length, true);
          return dst;
        } catch (e: any) {
          const msg = encoder.encode(String(e?.message ?? e));
          const dst: number = exp.vo_alloc(msg.length);
          new Uint8Array(mem.buffer).set(msg, dst);
          dv.setUint32(okPtr, 0, true);
          dv.setUint32(outLenPtr, msg.length, true);
          return dst;
        }
      },

      host_compile_check: (codePtr: number, codeLen: number, outLenPtr: number): number => {
        const exp = getInstance()!.exports as any;
        const mem = getMem();
        const code = memStr(mem, codePtr, codeLen);
        const result: string = w.voHostCompileCheck(code);
        const msg = encoder.encode(result);
        const dst: number = exp.vo_alloc(msg.length);
        new Uint8Array(mem.buffer).set(msg, dst);
        new DataView(mem.buffer).setUint32(outLenPtr, msg.length, true);
        return dst;
      },

      host_run_bytecode: (bcPtr: number, bcLen: number, errLenPtr: number): number => {
        const exp = getInstance()!.exports as any;
        const mem = getMem();
        const bc = new Uint8Array(mem.buffer, bcPtr, bcLen).slice();
        try {
          w.voHostRunBytecode(bc);
          new DataView(mem.buffer).setUint32(errLenPtr, 0, true);
          return 0;
        } catch (e: any) {
          const msg = encoder.encode(String(e?.message ?? e));
          const dst: number = exp.vo_alloc(msg.length);
          new Uint8Array(mem.buffer).set(msg, dst);
          new DataView(mem.buffer).setUint32(errLenPtr, msg.length, true);
          return dst;
        }
      },

      host_run_bytecode_capture: (bcPtr: number, bcLen: number, okPtr: number, outLenPtr: number): number => {
        const exp = getInstance()!.exports as any;
        const mem = getMem();
        const dv = new DataView(mem.buffer);
        const bc = new Uint8Array(mem.buffer, bcPtr, bcLen).slice();
        try {
          const output: string = w.voHostRunBytecodeCapture(bc);
          const msg = encoder.encode(output);
          const dst: number = exp.vo_alloc(msg.length);
          new Uint8Array(mem.buffer).set(msg, dst);
          dv.setUint32(okPtr, 1, true);
          dv.setUint32(outLenPtr, msg.length, true);
          return dst;
        } catch (e: any) {
          const msg = encoder.encode(String(e?.message ?? e));
          const dst: number = exp.vo_alloc(msg.length);
          new Uint8Array(mem.buffer).set(msg, dst);
          dv.setUint32(okPtr, 0, true);
          dv.setUint32(outLenPtr, msg.length, true);
          return dst;
        }
      },

      host_vfs_read: (pathPtr: number, pathLen: number, okPtr: number, outLenPtr: number): number => {
        const exp = getInstance()!.exports as any;
        const mem = getMem();
        const dv = new DataView(mem.buffer);
        const path = memStr(mem, pathPtr, pathLen);
        try {
          const data: Uint8Array = w.voHostVfsRead(path);
          const dst: number = exp.vo_alloc(data.length);
          new Uint8Array(mem.buffer).set(data, dst);
          dv.setUint32(okPtr, 1, true);
          dv.setUint32(outLenPtr, data.length, true);
          return dst;
        } catch {
          dv.setUint32(okPtr, 0, true);
          dv.setUint32(outLenPtr, 0, true);
          return 0;
        }
      },

      host_vfs_write: (pathPtr: number, pathLen: number, dataPtr: number, dataLen: number): number => {
        const mem = getMem();
        const path = memStr(mem, pathPtr, pathLen);
        const data = new Uint8Array(mem.buffer, dataPtr, dataLen).slice();
        try {
          w.voHostVfsWrite(path, data);
          return 1;
        } catch {
          return 0;
        }
      },

      host_vfs_exists: (pathPtr: number, pathLen: number): number => {
        const path = memStr(getMem(), pathPtr, pathLen);
        return w.voHostVfsExists(path) ? 1 : 0;
      },
    },
  };
}

// ── Public API ───────────────────────────────────────────────────────────────

/** Register voSetupExtModule, voCallExt, voCallExtReplay on `window`. */
export function registerExtModuleGlobals(): void {
  const w = window as any;

  w.voSetupExtModule = async (key: string, bytes: Uint8Array, jsGlueUrl?: string): Promise<void> => {
    if (jsGlueUrl) {
      const resp = await fetch(jsGlueUrl);
      if (!resp.ok) throw new Error(`Failed to fetch JS glue: HTTP ${resp.status}`);
      const jsText = await resp.text();
      const blob = new Blob([jsText], { type: 'application/javascript' });
      const blobUrl = URL.createObjectURL(blob);
      try {
        const glue = await import(/* @vite-ignore */ blobUrl);
        await glue.default({ module_or_path: bytes.slice() });
        if (typeof glue.__voInit === 'function') await glue.__voInit();
        extBindgenModules.set(key, glue);
      } finally {
        URL.revokeObjectURL(blobUrl);
      }
    } else {
      let instance: WebAssembly.Instance;
      let mem: WebAssembly.Memory;

      const importObject = buildHostImports(
        () => instance,
        () => mem,
      );

      const result = await WebAssembly.instantiate(bytes.slice(), importObject);
      instance = result.instance;
      mem = instance.exports.memory as WebAssembly.Memory;
      extInstances.set(key, instance);
    }
  };

  w.voCallExt = (externName: string, input: Uint8Array): Uint8Array => {
    // 1) Try bindgen modules first (longest prefix match)
    let bindgenModule: any;
    let bindgenKey = '';
    for (const [key, mod] of extBindgenModules) {
      if (externName.startsWith(key) && key.length > bindgenKey.length) { bindgenKey = key; bindgenModule = mod; }
    }
    if (bindgenModule) {
      const fn = bindgenModule[externName.substring(bindgenKey.length + 1)];
      if (typeof fn !== 'function') return new Uint8Array(0);
      try { const r = fn(input); if (r instanceof Uint8Array) return r; } catch { }
      return new Uint8Array(0);
    }

    // 2) Try standalone C-ABI instances (longest prefix match)
    let instance: WebAssembly.Instance | undefined;
    let matchedKey = '';
    for (const [key, inst] of extInstances) {
      if (externName.startsWith(key) && key.length > matchedKey.length) { matchedKey = key; instance = inst; }
    }
    if (!instance) return new Uint8Array(0);
    const exp = instance.exports as any;
    const funcName = externName.substring(matchedKey.length + 1);
    const extFunc: Function | undefined = exp[funcName] ?? exp[externName];
    if (!extFunc) return new Uint8Array(0);
    const shortMod = matchedKey.split('_').slice(-1)[0];
    const allocFn: Function = exp.vo_alloc ?? exp[`${shortMod}_alloc`];
    const deallocFn: Function = exp.vo_dealloc ?? exp[`${shortMod}_dealloc`];
    if (!allocFn || !deallocFn) throw new Error(`ext module '${matchedKey}' missing alloc/dealloc exports`);
    const inputPtr: number = allocFn(input.length);
    new Uint8Array(exp.memory.buffer).set(input, inputPtr);
    const outLenPtr: number = allocFn(4);
    const outPtr: number = extFunc(inputPtr, input.length, outLenPtr);
    deallocFn(inputPtr, input.length);
    if (outPtr === 0) { deallocFn(outLenPtr, 4); return new Uint8Array(0); }
    const outLen: number = new Uint32Array(exp.memory.buffer, outLenPtr, 1)[0];
    const result = new Uint8Array(exp.memory.buffer, outPtr, outLen).slice();
    deallocFn(outPtr, outLen);
    deallocFn(outLenPtr, 4);
    return result;
  };

  // Replay path for ext bridge HostEventWaitAndReplay.
  // The standalone .wasm module's waitForEvent export always returns TAG_SUSPEND.
  // On replay, the ext bridge calls voCallExtReplay with resume data.
  // We decode it here into tagged output format.
  w.voCallExtReplay = (externName: string, resumeData: Uint8Array): Uint8Array => {
    // waitForEvent replay: resumeData = [i32 handler_id LE][UTF-8 payload]
    // Return: [TAG_VALUE(handler_id as u64)][TAG_BYTES(payload)]
    if (externName.endsWith('_waitForEvent')) {
      const handlerId = new DataView(resumeData.buffer, resumeData.byteOffset, 4).getInt32(0, true);
      const payloadEnc = resumeData.subarray(4);
      const result = new Uint8Array(9 + 5 + payloadEnc.length);
      const dv = new DataView(result.buffer);
      result[0] = 0xE2; // TAG_VALUE
      dv.setInt32(1, handlerId, true);
      dv.setInt32(5, handlerId < 0 ? -1 : 0, true);
      result[9] = 0xE3; // TAG_BYTES
      dv.setUint32(10, payloadEnc.length, true);
      result.set(payloadEnc, 14);
      return result;
    }
    return new Uint8Array(0);
  };
}
