fn main() {
    // Keep malformed or adversarial regexp requests inside a hard WebAssembly
    // memory boundary. The support module is a separate instance, so this cap
    // is independent of the guest program's admitted memory limit.
    if std::env::var_os("CARGO_CFG_TARGET_ARCH").as_deref() == Some(std::ffi::OsStr::new("wasm32"))
    {
        println!("cargo:rustc-link-arg=--max-memory=268435456");
    }
    println!("cargo:rerun-if-changed=build.rs");
}
