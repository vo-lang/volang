//! Explicit local archive rehearsal using real, already built inputs. Publication
//! still requires the normal clean-source and protected-CI release commands.
use super::*;

#[test]
#[ignore = "requires a complete local UI toolkit and native release runtimes"]
fn local_installation_archive() {
    let output = PathBuf::from(
        std::env::var_os("VO_RELEASE_INSTALL_PROBE")
            .expect("set VO_RELEASE_INSTALL_PROBE to a new output directory"),
    );
    let kit = PathBuf::from(
        std::env::var_os("VO_RELEASE_UI_TOOLCHAIN")
            .expect("set VO_RELEASE_UI_TOOLCHAIN to the verified platform toolkit"),
    );
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let target = match (std::env::consts::OS, std::env::consts::ARCH) {
        ("macos", "aarch64") => "aarch64-apple-darwin",
        ("macos", "x86_64") => "x86_64-apple-darwin",
        ("linux", "aarch64") => "aarch64-unknown-linux-gnu",
        ("linux", "x86_64") => "x86_64-unknown-linux-gnu",
        ("windows", "x86_64") => "x86_64-pc-windows-msvc",
        _ => panic!("unsupported release host"),
    };
    let binary_name = if cfg!(windows) { "vo.exe" } else { "vo" };
    let paths = [
        binary_name,
        release_aot_runtime_name(target),
        release_ui_aot_runtime_name(target),
    ]
    .map(|name| root.join("target/release").join(name));
    let binary = binary_record(&paths[0], binary_name).unwrap();
    let runtime = binary_record(&paths[1], release_aot_runtime_name(target)).unwrap();
    let ui_runtime = binary_record(&paths[2], release_ui_aot_runtime_name(target)).unwrap();
    let compatibility = ui_web_runtime_records(&root).unwrap();
    let toolkit = toolchain::records_from(&kit, target, &binary).unwrap();
    fs::create_dir(&output).expect("archive rehearsal output must be a new directory");
    let archive = output.join("installation.tar.gz");
    let epoch = 1_700_000_000;
    create_deterministic_tarball(
        &archive,
        ArchiveBinaryInput {
            path: &paths[0],
            name: &binary.path,
        },
        ArchiveBinaryInput {
            path: &paths[1],
            name: &runtime.path,
        },
        ArchiveBinaryInput {
            path: &paths[2],
            name: &ui_runtime.path,
        },
        UiWebRuntimeInput {
            root: &root,
            records: &compatibility,
        },
        toolchain::Input {
            directory: &kit,
            records: &toolkit,
        },
        epoch,
    )
    .unwrap();
    verify_deterministic_tarball(
        &archive,
        &binary,
        &runtime,
        &ui_runtime,
        &compatibility,
        &toolkit,
        epoch,
    )
    .unwrap();
    let installed = output.join("installed");
    tar::Archive::new(GzDecoder::new(BufReader::new(
        File::open(&archive).unwrap(),
    )))
    .unpack(&installed)
    .unwrap();
    write_json_atomic(&output.join("packaging-report.json"), &serde_json::json!({
        "schema":"volang.local-release-installation.v1", "target":target,
        "archiveSha256":sha256_file(&archive).unwrap(), "archiveBytes":regular_file_size(&archive).unwrap(),
        "binary":binary, "aotRuntime":runtime, "uiAotRuntime":ui_runtime,
        "compatibility":compatibility, "toolkit":toolkit,
        "archiveVerified":true, "productCertified":false, "published":false
    })).unwrap();
    println!(
        "Installation archive verified and extracted to {}",
        installed.display()
    );
}
