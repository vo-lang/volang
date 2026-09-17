//! Actual multi-package Engine compilation with exclusive phase attribution.
//! Requires the non-default compiler-profile feature. These instrumented
//! timings describe compiler work; ordinary compilation is measured separately.

use std::alloc::{GlobalAlloc, Layout, System};
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Instant, SystemTime, UNIX_EPOCH};

use serde_json::{json, Value};
use sha2::{Digest, Sha256};
use vo_common::compiler_profile::{capture, Phase};
use vo_engine::{compile_with_cache, CaptureSink, CompileOutput, RunMode};

const COUNT_ALLOCATIONS: bool = option_env!("VO_PIPELINE_ALLOCATION_DIAGNOSTICS").is_some();

struct DiagnosticAllocator;
unsafe impl GlobalAlloc for DiagnosticAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc(layout) };
        if COUNT_ALLOCATIONS && !pointer.is_null() {
            vo_common::compiler_profile::record_allocation(layout.size());
        }
        pointer
    }
    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc_zeroed(layout) };
        if COUNT_ALLOCATIONS && !pointer.is_null() {
            vo_common::compiler_profile::record_allocation(layout.size());
        }
        pointer
    }
    unsafe fn realloc(&self, pointer: *mut u8, layout: Layout, bytes: usize) -> *mut u8 {
        let result = unsafe { System.realloc(pointer, layout, bytes) };
        if COUNT_ALLOCATIONS && !result.is_null() {
            vo_common::compiler_profile::record_reallocation(bytes);
        }
        result
    }
    unsafe fn dealloc(&self, pointer: *mut u8, layout: Layout) {
        unsafe { System.dealloc(pointer, layout) };
    }
}
#[global_allocator]
static ALLOCATOR: DiagnosticAllocator = DiagnosticAllocator;

fn allocations(value: &vo_common::compiler_profile::Allocations) -> Value {
    json!({"allocations": value.allocations, "reallocations": value.reallocations,
        "requested_bytes": value.requested_bytes})
}

struct Scratch(PathBuf);

impl Scratch {
    fn new() -> Self {
        let parent = std::env::temp_dir().canonicalize().unwrap();
        // Never let cache-root discovery reach a containing repository.
        assert!(!parent.ancestors().any(|p| p.join("Cargo.toml").exists()));
        for attempt in 0..100 {
            let stamp = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_nanos();
            let root = parent.join(format!(
                "vo-compiler-phases-{}-{stamp}-{attempt}",
                std::process::id()
            ));
            match fs::create_dir(&root) {
                Ok(()) => return Self(root),
                Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => continue,
                Err(error) => panic!("cannot create compiler probe directory: {error}"),
            }
        }
        panic!("cannot reserve unique compiler probe directory");
    }
}

impl Drop for Scratch {
    fn drop(&mut self) {
        // Own only the directory admitted with create_dir, including its caches.
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn digest(bytes: &[u8]) -> String {
    format!("{:x}", Sha256::digest(bytes))
}

fn files(root: &Path) -> BTreeMap<String, (String, u128)> {
    fn visit(root: &Path, current: &Path, result: &mut BTreeMap<String, (String, u128)>) {
        if !current.exists() {
            return;
        }
        for entry in fs::read_dir(current).unwrap() {
            let path = entry.unwrap().path();
            let metadata = fs::symlink_metadata(&path).unwrap();
            assert!(!metadata.file_type().is_symlink());
            if metadata.is_dir() {
                visit(root, &path, result);
            } else {
                let modified = metadata
                    .modified()
                    .unwrap()
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .as_nanos();
                result.insert(
                    path.strip_prefix(root)
                        .unwrap()
                        .to_string_lossy()
                        .into_owned(),
                    (digest(&fs::read(path).unwrap()), modified),
                );
            }
        }
    }
    let mut result = BTreeMap::new();
    visit(root, root, &mut result);
    result
}

fn project(
    root: &Path,
    name: &str,
    packages: usize,
    parts: usize,
    functions: usize,
) -> (PathBuf, i64) {
    let path = root.join(name);
    fs::create_dir(&path).unwrap();
    let identity = format!("example.com/volang/compile-{name}");
    fs::write(
        path.join("vo.mod"),
        format!("format = 1\nmodule = \"{identity}\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n"),
    )
    .unwrap();
    let mut main = String::from("package main\nimport (\n");
    for package in 0..packages {
        main.push_str(&format!("\"{identity}/p{package}\"\n"));
    }
    main.push_str(")\nfunc main() { sum := 0\n");
    for package in 0..packages {
        main.push_str(&format!("sum += p{package}.F0({package})\n"));
    }
    main.push_str("println(sum)\n}\n");
    fs::write(path.join("main.vo"), main).unwrap();
    for package in 0..packages {
        let directory = path.join(format!("p{package}"));
        fs::create_dir(&directory).unwrap();
        for part in 0..parts {
            let mut source = format!("package p{package}\n");
            for offset in 0..functions {
                let function = part * functions + offset;
                let elements = (function..function + 8)
                    .map(|i| format!("x + {i},"))
                    .collect::<Vec<_>>()
                    .join(" ");
                source.push_str(&format!("func F{function}(x int) int {{ values := [8]int{{{elements}}}; sum := 0; for i := 0; i < 8; i++ {{ sum += values[i] }}; return sum }}\n"));
            }
            fs::write(directory.join(format!("part{part}.vo")), source).unwrap();
        }
    }
    let expected = (0..packages).map(|p| 8 * p as i64 + 28).sum();
    (path, expected)
}

fn check_output(output: CompileOutput, expected: i64) -> Vec<u8> {
    let bytes = output.module.serialize().unwrap();
    let sink = CaptureSink::new();
    vo_engine::run_with_output(output, RunMode::Vm, Vec::new(), sink.clone()).unwrap();
    assert_eq!(sink.take(), format!("{expected}\n"));
    bytes
}

fn main() {
    assert_eq!(
        std::env::var("VOWORK").as_deref(),
        Ok("off"),
        "run this repository probe with VOWORK=off"
    );
    let mut args = std::env::args().skip(1);
    let mut allocation_flag = false;
    let mut reference_compiler = None;
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--allocations" if !allocation_flag => allocation_flag = true,
            "--reference-compiler" if reference_compiler.is_none() => {
                let path =
                    PathBuf::from(args.next().expect("--reference-compiler requires a path"))
                        .canonicalize()
                        .unwrap();
                let sha256 = digest(&fs::read(&path).unwrap());
                reference_compiler = Some((path, sha256));
            }
            _ => panic!("compiler_pipeline accepts --allocations and --reference-compiler PATH"),
        }
    }
    assert_eq!(
        allocation_flag, COUNT_ALLOCATIONS,
        "allocation diagnostics require their own VO_PIPELINE_ALLOCATION_DIAGNOSTICS build"
    );
    let scratch = Scratch::new();
    let mut samples = Vec::new();
    for (name, packages, parts, functions) in
        [("small", 1, 1, 4), ("medium", 8, 4, 4), ("large", 32, 4, 8)]
    {
        let (path, expected) = project(&scratch.0, name, packages, parts, functions);
        let source_manifest = files(&path);
        let cache = path.join(".volang/cache/vo/compile/native");
        let leaf = path.join("p0/part0.vo");
        let original = fs::read_to_string(&leaf).unwrap();
        for scenario in ["cache-miss", "cache-hit", "changed-file"] {
            for iteration in 0..5 {
                fs::write(&leaf, &original).unwrap();
                if cache.exists() {
                    fs::remove_dir_all(&cache).unwrap();
                }
                if scenario != "cache-miss" {
                    check_output(
                        compile_with_cache(path.to_str().unwrap()).unwrap(),
                        expected,
                    );
                }
                let expected = if scenario == "changed-file" {
                    let generation = iteration + 101;
                    let changed = original.replacen("x + 0,", &format!("x + {generation},"), 1);
                    assert_ne!(changed, original);
                    fs::write(&leaf, changed).unwrap();
                    expected + generation
                } else {
                    expected
                };
                let mut measured_sources = source_manifest.clone();
                let metadata = fs::metadata(&leaf).unwrap();
                measured_sources.insert(
                    "p0/part0.vo".into(),
                    (
                        digest(&fs::read(&leaf).unwrap()),
                        metadata
                            .modified()
                            .unwrap()
                            .duration_since(UNIX_EPOCH)
                            .unwrap()
                            .as_nanos(),
                    ),
                );
                let before = files(&cache);
                let started = Instant::now();
                let (output, report) =
                    capture(|| compile_with_cache(path.to_str().unwrap())).unwrap();
                let elapsed = started.elapsed();
                assert!(
                    !report.overflowed,
                    "phase storage overflow invalidates attribution"
                );
                let after = files(&cache);
                assert!(
                    !after.is_empty(),
                    "cache was not published under the isolated project"
                );
                assert_eq!(before == after, scenario == "cache-hit");
                let output = output.unwrap();
                let module_functions = output.module.functions.len();
                let instructions: usize =
                    output.module.functions.iter().map(|f| f.code.len()).sum();
                let frame_slots: usize = output
                    .module
                    .functions
                    .iter()
                    .map(|f| usize::from(f.local_slots))
                    .sum();
                let bytes = check_output(output, expected);
                // Capture-disabled compilation must produce identical bytecode.
                // Remove our own cache so this also repeats semantic analysis.
                fs::remove_dir_all(&cache).unwrap();
                let reference = check_output(
                    compile_with_cache(path.to_str().unwrap()).unwrap(),
                    expected,
                );
                assert_eq!(bytes, reference);
                if let Some((compiler, _)) = &reference_compiler {
                    // This separate equivalence mode never participates in a
                    // diagnostic timing comparison. --no-cache forces the
                    // feature-off compiler to analyze these exact sources.
                    let reference_path = scratch.0.join("reference.vob");
                    let result = std::process::Command::new(compiler)
                        .arg("build")
                        .arg(&path)
                        .args(["--kind=bytecode", "--no-cache", "-o"])
                        .arg(&reference_path)
                        .env("VOWORK", "off")
                        .output()
                        .unwrap();
                    assert!(
                        result.status.success(),
                        "reference compiler failed: {}",
                        String::from_utf8_lossy(&result.stderr)
                    );
                    assert_eq!(
                        bytes,
                        fs::read(&reference_path).unwrap(),
                        "feature-on/off bytecode diverged"
                    );
                    fs::remove_file(reference_path).unwrap();
                }
                let has_analysis = report.phases[Phase::TypeCheck as usize].calls != 0;
                assert_eq!(has_analysis, scenario != "cache-hit");
                for phase in [Phase::LexParse, Phase::Codegen] {
                    assert_eq!(report.phases[phase as usize].calls != 0, has_analysis);
                }
                let attributed = report.phases.iter().map(|p| p.exclusive).sum();
                let unattributed = elapsed
                    .checked_sub(attributed)
                    .expect("exclusive phases exceed total compile interval");
                let allocated = &report.allocations;
                let sum_allocations: u64 = report
                    .phases
                    .iter()
                    .map(|p| p.allocations.allocations)
                    .sum();
                let sum_reallocations: u64 = report
                    .phases
                    .iter()
                    .map(|p| p.allocations.reallocations)
                    .sum();
                let sum_bytes: u64 = report
                    .phases
                    .iter()
                    .map(|p| p.allocations.requested_bytes)
                    .sum();
                let unattributed_allocator = vo_common::compiler_profile::Allocations {
                    allocations: allocated.allocations.checked_sub(sum_allocations).unwrap(),
                    reallocations: allocated
                        .reallocations
                        .checked_sub(sum_reallocations)
                        .unwrap(),
                    requested_bytes: allocated.requested_bytes.checked_sub(sum_bytes).unwrap(),
                };
                let phases: BTreeMap<_, Value> = Phase::ALL.into_iter().map(|phase| {
                    let value = &report.phases[phase as usize];
                    (phase.name(), json!({"calls": value.calls, "exclusive_ns": value.exclusive.as_nanos(), "allocator": allocations(&value.allocations)}))
                }).collect();
                samples.push(json!({"case": name, "scenario": scenario, "iteration": iteration,
                    "project_packages": packages + 1, "project_source_files": packages * parts + 1,
                    "project_functions": packages * parts * functions + 1,
                    "parsed_files": report.source_files, "parsed_bytes": report.source_bytes,
                    "allocator": allocations(&report.allocations), "unattributed_allocator": allocations(&unattributed_allocator), "missed_allocator_events": report.missed_allocation_events,
                    "elapsed_ns": elapsed.as_nanos(), "unattributed_ns": unattributed.as_nanos(),
                    "phases": phases, "module_functions": module_functions, "instructions": instructions,
                    "frame_slots": frame_slots, "vob_bytes": bytes.len(), "vob_sha256": digest(&bytes),
                    "output": format!("{expected}\n"), "source_manifest": measured_sources}));
            }
        }
        fs::write(leaf, original).unwrap();
    }
    if let Some((path, sha256)) = &reference_compiler {
        assert_eq!(
            &digest(&fs::read(path).unwrap()),
            sha256,
            "reference compiler changed during equivalence check"
        );
    }
    let reference_identity = reference_compiler
        .as_ref()
        .map(|(path, sha256)| json!({"path":path,"sha256":sha256}));
    println!(
        "{}",
        json!({"compiler_profile": true, "compiler_profile_schema": vo_common::compiler_profile::SCHEMA, "allocation_diagnostics": COUNT_ALLOCATIONS,
        "reference_compiler": reference_identity,
        "scope": "Instrumented exclusive compiler phases in one process; each operation uses the real Engine capture/cache pipeline. Setup, output execution and reference compilation are outside the interval. Cache-hit reuses the matching product and requires zero analysis/codegen. Imported stdlib work is included in parsed/module counts. These diagnostic intervals are not ordinary compiler performance.",
        "samples": samples})
    );
}
