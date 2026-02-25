fn main() {
    // Expose the Cargo build profile name as a compile-time env so ext_manifest.rs
    // can resolve {profile} in vo.ext.toml paths to the correct target/ subdirectory.
    //   debug          → "debug"
    //   --release      → "release"
    //   --profile release-native → "release-native"
    let profile = std::env::var("PROFILE").unwrap_or_else(|_| "debug".to_string());
    println!("cargo:rustc-env=VO_BUILD_PROFILE={}", profile);
    println!("cargo:rerun-if-env-changed=PROFILE");
}
