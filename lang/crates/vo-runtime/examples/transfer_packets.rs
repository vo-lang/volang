//! Transfer encoder diagnostic. Compile both variants from these exact bytes.
//! Allocation counting is opt-in and runs separately from formal timing. Bytes
//! count full successful realloc requests, not peak occupancy or bytes copied.
//! Build allocation probes with VO_TRANSFER_ALLOCATION_DIAGNOSTICS=1 and run
//! them with --allocations. Formal timing uses a separate build without that
//! variable, so the optimizer removes counter loads from every allocation.
use std::alloc::{GlobalAlloc, Layout, System};
use std::hint::black_box;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering::Relaxed};
use std::time::Instant;
use vo_runtime::bytecode::TransferType;
use vo_runtime::gc::Gc;
use vo_runtime::island_msg::{
    decode_spawn_header, encode_spawn_payload_from_capture_descriptors, unpack_spawn_payload,
    SpawnCaptureStorage, SpawnCaptureValue,
};
use vo_runtime::{RuntimeType, ValueKind, ValueMeta, ValueRttid};

const COUNT_ALLOCATIONS: bool = option_env!("VO_TRANSFER_ALLOCATION_DIAGNOSTICS").is_some();
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

fn main() {
    let arguments = std::env::args().skip(1).collect::<Vec<_>>();
    let diagnostics = arguments.iter().any(|arg| arg == "--allocations");
    assert_eq!(diagnostics, COUNT_ALLOCATIONS,
        "--allocations requires the separately built VO_TRANSFER_ALLOCATION_DIAGNOSTICS probe; formal timing requires the ordinary build");
    let option = |name: &str| {
        arguments
            .windows(2)
            .find(|pair| pair[0] == name)
            .map(|pair| pair[1].as_str())
    };
    let iterations = option("--iterations")
        .map(|n| n.parse::<usize>().unwrap())
        .unwrap_or(2000);
    assert!(iterations > 0 && iterations <= 1_000_000);
    let wire = option("--wire-dir").map(PathBuf::from);
    if let Some(directory) = &wire {
        std::fs::create_dir_all(directory).unwrap();
    }
    println!("{{\"allocation_diagnostics\":{diagnostics},\"iterations\":{iterations},\"cases\":[");
    let mut first = true;
    for (name, captures, parameters, width) in [
        ("empty", 0, 0, 1),
        ("one-int", 1, 0, 1),
        ("captures16", 16, 0, 1),
        ("captures128", 128, 0, 1),
        ("arguments128", 0, 128, 1),
        ("mixed64", 32, 32, 1),
        ("array64-one", 1, 0, 64),
        ("array64-many", 16, 16, 64),
    ] {
        if option("--case").is_some_and(|selected| selected != name) {
            continue;
        }
        let gc = Gc::new();
        let mut types = vec![RuntimeType::Basic(ValueKind::Int64)];
        let (rttid, meta) = if width == 1 {
            (
                ValueRttid::new(0, ValueKind::Int64),
                ValueMeta::new(0, ValueKind::Int64),
            )
        } else {
            types.push(RuntimeType::Array {
                len: width as u64,
                elem: ValueRttid::new(0, ValueKind::Int64),
            });
            (
                ValueRttid::new(1, ValueKind::Array),
                ValueMeta::new(1, ValueKind::Array),
            )
        };
        let transfer = TransferType {
            meta_raw: meta.to_raw(),
            rttid_raw: rttid.to_raw(),
            slots: width as u16,
        };
        let slots = (0..width).map(|i| i as u64 + 11).collect::<Vec<_>>();
        let descriptors = (0..captures)
            .map(|_| SpawnCaptureValue {
                slots: Some(slots.as_slice()),
                storage: SpawnCaptureStorage::ValueSlots,
            })
            .collect::<Vec<_>>();
        let capture_types = vec![transfer; captures];
        let parameter_types = vec![transfer; parameters];
        let args = slots.repeat(parameters);
        let encode = || {
            encode_spawn_payload_from_capture_descriptors(
                &gc,
                7,
                &descriptors,
                &capture_types,
                &args,
                &parameter_types,
                &[],
                &[],
                &types,
            )
            .unwrap()
        };
        let reference = encode();
        let header = decode_spawn_header(&reference).unwrap();
        let mut destination = Gc::new();
        let (received_captures, received_args) = unpack_spawn_payload(
            &mut destination,
            &reference,
            &header,
            &capture_types,
            &parameter_types,
            &[],
            &[],
            &types,
            |_, _| panic!("scalar/array fixture has no queue handles"),
        )
        .unwrap();
        assert_eq!(received_captures.len(), captures);
        for capture in &received_captures {
            let restored = (0..width)
                .map(|i| unsafe { Gc::read_slot(*capture as vo_runtime::gc::GcRef, i) })
                .collect::<Vec<_>>();
            assert_eq!(restored, slots);
        }

        assert_eq!(received_args, args);
        if let Some(directory) = &wire {
            std::fs::write(directory.join(format!("{name}.bin")), &reference).unwrap();
        }
        for _ in 0..32 {
            black_box(encode());
        }
        CALLS.store(0, Relaxed);
        BYTES.store(0, Relaxed);
        COUNT.store(diagnostics, Relaxed);
        let started = Instant::now();
        let mut checksum = 0usize;
        for _ in 0..iterations {
            let value = encode();
            assert_eq!(value.len(), reference.len());
            checksum = checksum.wrapping_add(black_box(value.as_slice()).len());
        }
        let seconds = started.elapsed().as_secs_f64();
        COUNT.store(false, Relaxed);
        let calls = CALLS.load(Relaxed);
        let bytes = BYTES.load(Relaxed);
        if !first {
            println!(",");
        }
        first = false;
        print!("{{\"name\":\"{name}\",\"seconds\":{seconds},\"allocations\":{calls},\"allocated_bytes\":{bytes},\"wire_bytes\":{},\"checksum\":{checksum}}}",reference.len());
    }
    println!("]}}");
}
