fn main() {
    let profile = std::env::var("PROFILE").unwrap_or_else(|_| "debug".to_string());
    println!("cargo:rustc-env=VO_BUILD_PROFILE={}", profile);
    println!("cargo:rerun-if-env-changed=PROFILE");
}
