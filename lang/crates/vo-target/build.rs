fn main() {
    let target = std::env::var("TARGET").unwrap_or_else(|_| "unknown-target".to_string());
    println!("cargo:rustc-env=VO_HOST_TARGET_TRIPLE={target}");
    println!("cargo:rerun-if-env-changed=TARGET");
}
