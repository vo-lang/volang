//! Vo wrapper for resvg — SVG rendering to PNG.
//!
//! Exposes a single extern function `Render(svg string) ([]byte, error)`
//! to Vo code via the native extension ABI (native) or as a registered
//! StdlibEntry (WASM, compiled directly into the runtime binary).

use resvg::{usvg, tiny_skia};
use vo_ext::prelude::*;
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};

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

#[vo_fn("github.com/vo-lang/resvg", "Render")]
fn render(call: &mut ExternCallContext) -> ExternResult {
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

#[cfg(not(target_arch = "wasm32"))]
vo_ext::export_extensions!();

#[cfg(target_arch = "wasm32")]
vo_ext::export_extensions!(__STDLIB_github_com_vo_lang_resvg_Render);

/// Register resvg extern functions into a `ExternRegistry`.
///
/// On WASM this matches each `StdlibEntry` against the bytecode extern list by name.
/// On native this is a no-op — native extensions register via the dlopen ABI.
pub fn register_externs(
    registry: &mut vo_runtime::ffi::ExternRegistry,
    externs: &[vo_runtime::bytecode::ExternDef],
) {
    #[cfg(target_arch = "wasm32")]
    {
        for entry in VO_EXT_ENTRIES {
            for (id, def) in externs.iter().enumerate() {
                if def.name == entry.name {
                    registry.register(id as u32, entry.func);
                    break;
                }
            }
        }
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = (registry, externs);
    }
}
