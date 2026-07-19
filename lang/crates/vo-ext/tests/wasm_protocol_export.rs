//! Cross-crate compile contract for the browser protocol export helper.
//!
//! WASM target checks compile tests, so this exercises macro expansion in
//! a downstream crate, where hidden SDK dependencies cannot be assumed.

vo_ext::export_wasm_extension_protocol!();

#[cfg(target_arch = "wasm32")]
#[test]
fn exported_protocol_epoch_is_v3() {
    assert_eq!(vo_ext_protocol_version(), 3);
}
