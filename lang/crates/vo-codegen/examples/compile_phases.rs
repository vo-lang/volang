//! Reproducible in-memory compiler workloads. Run an optimized build serially;
//! Build with VO_COMPILER_ALLOCATION_DIAGNOSTICS=1 for `--allocations`;
//! the ordinary timing build removes counter loads. Diagnostics run separately
//! from ordinary timing. Filesystem capture and persistent caches are excluded.
//! Allocation bytes charge the full successful realloc request, without
//! claiming that those bytes moved or were resident simultaneously.

use std::alloc::{GlobalAlloc, Layout, System};
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering::Relaxed};
use std::time::Instant;

use vo_analysis::vfs::{ModSource, PackageResolver, StdSource};
use vo_analysis::{analyze_project_with_identity, PackageIdentity};
use vo_common::vfs::{FileSet, MemoryFs};
use vo_common_core::{verifier::verify_module, Module};

const COUNT_ALLOCATIONS: bool = option_env!("VO_COMPILER_ALLOCATION_DIAGNOSTICS").is_some();
struct Allocator;
static COUNT: AtomicBool = AtomicBool::new(false);
static CALLS: AtomicU64 = AtomicU64::new(0);
static BYTES: AtomicU64 = AtomicU64::new(0);

fn allocated(pointer: *mut u8, bytes: usize) {
    if COUNT_ALLOCATIONS && !pointer.is_null() && COUNT.load(Relaxed) {
        CALLS.fetch_add(1, Relaxed);
        BYTES.fetch_add(bytes as u64, Relaxed);
    }
}

unsafe impl GlobalAlloc for Allocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc(layout) };
        allocated(pointer, layout.size());
        pointer
    }
    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc_zeroed(layout) };
        allocated(pointer, layout.size());
        pointer
    }
    unsafe fn realloc(&self, pointer: *mut u8, layout: Layout, size: usize) -> *mut u8 {
        let result = unsafe { System.realloc(pointer, layout, size) };
        allocated(result, size);
        result
    }
    unsafe fn dealloc(&self, pointer: *mut u8, layout: Layout) {
        unsafe { System.dealloc(pointer, layout) };
    }
}

#[global_allocator]
static ALLOCATOR: Allocator = Allocator;

fn measure<T>(stage: &str, operation: impl FnOnce() -> T) -> (T, String) {
    let calls = CALLS.load(Relaxed);
    let bytes = BYTES.load(Relaxed);
    let started = Instant::now();
    let value = operation();
    let seconds = started.elapsed().as_secs_f64();
    let calls = CALLS.load(Relaxed) - calls;
    let bytes = BYTES.load(Relaxed) - bytes;
    let report = format!(
        "\"{stage}\":{{\"seconds\":{seconds},\"allocations\":{calls},\"allocated_bytes\":{bytes}}}"
    );
    (value, report)
}

fn main() {
    let diagnostics = std::env::args().any(|arg| arg == "--allocations");
    assert_eq!(diagnostics, COUNT_ALLOCATIONS, "--allocations requires a separate VO_COMPILER_ALLOCATION_DIAGNOSTICS build; timing uses the ordinary build");
    let resolver = PackageResolver {
        std: StdSource::with_fs(MemoryFs::new().with_file(
            "errors/errors.vo",
            include_str!("../../../stdlib/errors/errors.vo"),
        )),
        r#mod: ModSource::with_fs(MemoryFs::new()),
    };
    println!("{{\"allocation_diagnostics\":{diagnostics},\"samples\":[");
    let mut first = true;
    for (name, functions, width) in [("small", 1, 4), ("medium", 64, 32), ("wide", 256, 128)] {
        let mut source = format!("package main\ntype Big struct {{ values [{width}]int; tag string; other [4]float64 }}\n");
        for index in 0..functions {
            source.push_str(&format!(
                "func f{index}(a Big) Big {{ b := a; b.values[0] += 1; return b }}\n"
            ));
        }
        source.push_str("func main() {}\n");
        for iteration in 0..7 {
            let mut files = FileSet::new(PathBuf::from("compile-phases"));
            files.files.insert(PathBuf::from("main.vo"), source.clone());
            COUNT.store(diagnostics, Relaxed);
            let (project, analysis) = measure("analysis", || {
                analyze_project_with_identity(
                    files,
                    &resolver,
                    PackageIdentity::new("local/compile-phases").unwrap(),
                )
                .unwrap()
            });
            let (module, codegen) =
                measure("codegen", || vo_codegen::compile_project(&project).unwrap());
            let (_, verification) = measure("verification", || verify_module(&module).unwrap());
            let (bytes, serialization) = measure("serialization", || module.serialize().unwrap());
            let (decoded, decoding) = measure("decoding", || Module::deserialize(&bytes).unwrap());
            let (_, load_verification) =
                measure("load_verification", || verify_module(&decoded).unwrap());
            COUNT.store(false, Relaxed);
            assert_eq!(
                decoded.serialize().unwrap(),
                bytes,
                "compiler diagnostic VOB roundtrip changed bytes"
            );
            if iteration < 2 {
                continue;
            }
            if !first {
                println!(",");
            }
            first = false;
            let instructions: usize = module.functions.iter().map(|f| f.code.len()).sum();
            let slots: usize = module
                .functions
                .iter()
                .map(|f| usize::from(f.local_slots))
                .sum();
            print!("{{\"case\":\"{name}\",\"iteration\":{iteration},\"source_bytes\":{},\"instructions\":{instructions},\"frame_slots\":{slots},\"vob_bytes\":{},\"stages\":{{{analysis},{codegen},{verification},{serialization},{decoding},{load_verification}}}}}", source.len(), bytes.len());
        }
    }
    println!("\n]}}");
}
