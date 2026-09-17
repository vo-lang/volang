#[path = "../../../eng/build-identity.rs"]
mod build_identity;

fn main() {
    let target = std::env::var("TARGET").expect("Cargo target triple");
    println!("cargo:rustc-env=VO_TARGET_TRIPLE={target}");
    println!("cargo:rerun-if-env-changed=TARGET");
    build_identity::emit(
        std::path::Path::new(&std::env::var("CARGO_MANIFEST_DIR").unwrap()),
        "VO_COMPILER_BUILD_ID",
    );
}
