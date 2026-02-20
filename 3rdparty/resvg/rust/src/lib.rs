//! Vo wrapper for resvg — SVG rendering to PNG.
//!
//! # Features
//! - `native` (default): dynamic library for dlopen  
//! - `wasm`: compiled into the playground WASM binary
//! - `wasm-standalone`: pure C-ABI cdylib for dynamic WASM loading

use resvg::{usvg, tiny_skia};

fn render_svg_to_png(svg_str: &str) -> Result<Vec<u8>, String> {
    let opt = usvg::Options::default();
    let tree = usvg::Tree::from_str(svg_str, &opt)
        .map_err(|e| e.to_string())?;
    let int_size = tree.size().to_int_size();
    let mut pixmap = tiny_skia::Pixmap::new(int_size.width(), int_size.height())
        .ok_or_else(|| "failed to allocate pixmap: SVG has zero size".to_string())?;
    resvg::render(&tree, tiny_skia::Transform::default(), &mut pixmap.as_mut());
    pixmap.encode_png().map_err(|e| e.to_string())
}

// ── Vo extension ABI (native + wasm-integrated) ───────────────────────────────

#[cfg(any(feature = "native", feature = "wasm"))]
mod vo_ext_impl {
    use super::render_svg_to_png;
    use vo_ext::prelude::*;
    use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};

    #[vo_fn("github.com/vo-lang/resvg", "Render")]
    pub fn render(call: &mut ExternCallContext) -> ExternResult {
        let svg = call.arg_str(0).to_string();
        match render_svg_to_png(&svg) {
            Ok(png_bytes) => {
                let slice_ref = call.alloc_bytes(&png_bytes);
                call.ret_ref(0, slice_ref);
                write_nil_error(call, 1);
            }
            Err(msg) => {
                call.ret_nil(0);
                write_error_to(call, 1, &msg);
            }
        }
        ExternResult::Ok
    }
}

#[cfg(feature = "native")]
vo_ext::export_extensions!();

#[cfg(feature = "wasm")]
vo_ext::export_extensions!(__STDLIB_github_com_vo_lang_resvg_Render);

/// Register resvg extern functions (wasm-integrated feature only).
#[cfg(feature = "wasm")]
pub fn register_externs(
    registry: &mut vo_runtime::ffi::ExternRegistry,
    externs: &[vo_runtime::bytecode::ExternDef],
) {
    for entry in VO_EXT_ENTRIES {
        for (id, def) in externs.iter().enumerate() {
            if def.name == entry.name {
                registry.register(id as u32, entry.func);
                break;
            }
        }
    }
}

// ── Standalone C-ABI WASM exports (wasm-standalone feature) ──────────────────
//
// Used when the .wasm binary is pre-built and dynamically loaded by the browser.
// JS calls: resvg_alloc / resvg_render / resvg_dealloc via WebAssembly.instantiate.

#[cfg(feature = "wasm-standalone")]
mod standalone {
    use super::render_svg_to_png;

    #[no_mangle]
    pub extern "C" fn resvg_alloc(size: u32) -> *mut u8 {
        let mut buf = Vec::<u8>::with_capacity(size as usize);
        let ptr = buf.as_mut_ptr();
        std::mem::forget(buf);
        ptr
    }

    #[no_mangle]
    pub extern "C" fn resvg_dealloc(ptr: *mut u8, size: u32) {
        unsafe { drop(Vec::from_raw_parts(ptr, 0, size as usize)) };
    }

    /// Render SVG bytes at `svg_ptr[..svg_len]` → PNG.
    ///
    /// Writes the PNG byte count to `*out_len` and returns a pointer to the
    /// PNG bytes (caller must free with `resvg_dealloc`).
    /// Returns null on error (and writes 0 to `*out_len`).
    #[no_mangle]
    pub extern "C" fn resvg_render(
        svg_ptr: *const u8,
        svg_len: u32,
        out_len: *mut u32,
    ) -> *mut u8 {
        let svg = unsafe {
            let bytes = std::slice::from_raw_parts(svg_ptr, svg_len as usize);
            match std::str::from_utf8(bytes) {
                Ok(s) => s.to_string(),
                Err(_) => { *out_len = 0; return std::ptr::null_mut(); }
            }
        };
        match render_svg_to_png(&svg) {
            Ok(mut png) => {
                png.shrink_to_fit();
                let len = png.len() as u32;
                let ptr = png.as_mut_ptr();
                std::mem::forget(png);
                unsafe { *out_len = len; }
                ptr
            }
            Err(_) => {
                unsafe { *out_len = 0; }
                std::ptr::null_mut()
            }
        }
    }
}
