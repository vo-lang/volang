#[path = "../../../eng/build-identity.rs"]
mod build_identity;

fn main() {
    build_identity::emit(
        std::path::Path::new(&std::env::var("CARGO_MANIFEST_DIR").unwrap()),
        "VO_UI_BUILD_ID",
    );
}
