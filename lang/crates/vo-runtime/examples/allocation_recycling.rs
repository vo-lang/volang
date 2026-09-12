//! Repeated small-block allocation and collection. Build before measuring.
//! VO_RECYCLING_ALLOCATION_DIAGNOSTICS=1 enables a separate allocation-count
//! build; ordinary timing binaries compile out every allocator counter.
use std::alloc::{GlobalAlloc, Layout, System};
use std::hint::black_box;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering::Relaxed};
use std::time::Instant;
use vo_runtime::gc::{
    Gc, GcMode, GcObjectScanChunk, GcRootScanChunk, GcRootState, GcState, VmMemoryConfig,
};
use vo_runtime::{ValueKind, ValueMeta};

const DIAGNOSTICS: bool = option_env!("VO_RECYCLING_ALLOCATION_DIAGNOSTICS").is_some();
static COUNT: AtomicBool = AtomicBool::new(false);
static ALLOCS: AtomicU64 = AtomicU64::new(0);
static ALLOC_BYTES: AtomicU64 = AtomicU64::new(0);
static FREES: AtomicU64 = AtomicU64::new(0);
static FREE_BYTES: AtomicU64 = AtomicU64::new(0);
struct Allocator;

fn allocated(pointer: *mut u8, bytes: usize) {
    if DIAGNOSTICS && !pointer.is_null() && COUNT.load(Relaxed) {
        ALLOCS.fetch_add(1, Relaxed);
        ALLOC_BYTES.fetch_add(bytes as u64, Relaxed);
    }
}

fn freed(bytes: usize) {
    if DIAGNOSTICS && COUNT.load(Relaxed) {
        FREES.fetch_add(1, Relaxed);
        FREE_BYTES.fetch_add(bytes as u64, Relaxed);
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
        if !result.is_null() {
            freed(layout.size());
            allocated(result, size);
        }
        result
    }
    unsafe fn dealloc(&self, pointer: *mut u8, layout: Layout) {
        freed(layout.size());
        unsafe { System.dealloc(pointer, layout) };
    }
}

#[global_allocator]
static ALLOCATOR: Allocator = Allocator;

fn counts() -> [u64; 4] {
    [
        ALLOCS.load(Relaxed),
        ALLOC_BYTES.load(Relaxed),
        FREES.load(Relaxed),
        FREE_BYTES.load(Relaxed),
    ]
}

fn cycle(gc: &mut Gc, shape: &str) -> u64 {
    let meta = ValueMeta::new(0, ValueKind::Int64);
    let mut sum = 0_u64;
    let count: usize = if shape == "two-blocks" { 4096 } else { 64 };
    for i in 0..count {
        let slots = match shape {
            "class32" | "two-blocks" | "lane" => 2,
            "class1024" => 120,
            "classes" => (1usize << (1 + i % 12)) - 1,
            _ => unreachable!(),
        } as u16;
        // Include every class, including one-slot scalar objects in class16.
        let slots = slots.max(1);
        let object = if shape == "lane" {
            gc.try_alloc_value_slots_in_region(meta, slots)
        } else {
            gc.try_alloc_value_slots(meta, slots)
        }
        .unwrap();
        unsafe {
            object.write(i as u64);
            sum += black_box(object).read();
        }
    }
    gc.close_value_slot_allocation_region_for_boundary();
    gc.gc_request_cycle();
    let mut steps = 0;
    while gc.state() != GcState::Pause || steps == 0 {
        // The fixture has no live roots after its allocation loop. Every
        // object contains scalar slots and has no native finalizer.
        let work = unsafe {
            gc.step_with_scanners_budget(
                GcRootState::StableSinceLastScan,
                256,
                |_, _, _| GcRootScanChunk::complete(0),
                |_, _, _, _| GcObjectScanChunk::complete(0),
                |_| {},
            )
        };
        assert!(work <= 256 * 8);
        steps += 1;
        assert!(steps < 100_000, "collection failed to make progress");
    }
    assert_eq!(gc.memory_stats().object_count, 0);
    assert_eq!(sum, (count * (count - 1) / 2) as u64);
    sum
}

fn main() {
    let args: Vec<_> = std::env::args().collect();
    assert_eq!(args.len(), 5, "usage: allocation_recycling class32|class1024|classes|two-blocks|lane generational|incremental iterations timing|allocations");
    let shape = args[1].as_str();
    assert!(matches!(
        shape,
        "class32" | "class1024" | "classes" | "two-blocks" | "lane"
    ));
    let mode = match args[2].as_str() {
        "generational" => GcMode::Generational,
        "incremental" => GcMode::Incremental,
        _ => panic!("invalid GC mode"),
    };
    let iterations: usize = args[3].parse().unwrap();
    assert!((1..=1_000_000).contains(&iterations));
    assert!(matches!(args[4].as_str(), "timing" | "allocations"));
    assert_eq!(
        args[4] == "allocations",
        DIAGNOSTICS,
        "use the separately built counter binary"
    );
    COUNT.store(true, Relaxed);
    let mut gc = Gc::with_memory_config(VmMemoryConfig {
        gc_mode: mode,
        automatic_gc: false,
        initial_reserve_bytes: 2 * 1024 * 1024,
        ..Default::default()
    })
    .unwrap();
    for _ in 0..16 {
        black_box(cycle(&mut gc, shape));
    }
    let warm_counts = counts();
    let before = gc.memory_stats();
    let started = Instant::now();
    let mut sum = 0;
    for _ in 0..iterations {
        sum += black_box(cycle(&mut gc, shape));
    }
    let elapsed_ns = started.elapsed().as_nanos();
    let after = gc.memory_stats();
    let measured_counts = counts();
    drop(gc);
    let final_counts = counts();
    COUNT.store(false, Relaxed);
    if DIAGNOSTICS {
        assert_eq!(
            final_counts[1], final_counts[3],
            "collector teardown leaked requested host bytes"
        );
    }
    assert_eq!(after.allocation_failures, 0);
    println!("{{\"case\":{shape:?},\"gc_mode\":{:?},\"allocation_diagnostics\":{DIAGNOSTICS},\"iterations\":{iterations},\"elapsed_ns\":{elapsed_ns},\"checksum\":{sum},\"managed_allocation_bytes\":{},\"managed_committed_bytes\":{},\"gc_work_units\":{},\"host_allocation_calls\":{},\"host_requested_bytes\":{},\"host_free_calls\":{},\"host_freed_bytes\":{},\"warm_retained_requested_bytes_excluding_pages\":{},\"teardown_requested_bytes_balance\":{}}}",
        args[2], after.allocation_bytes_total-before.allocation_bytes_total,
        after.managed_committed_bytes, after.work_units_total-before.work_units_total,
        measured_counts[0]-warm_counts[0], measured_counts[1]-warm_counts[1],
        measured_counts[2]-warm_counts[2], measured_counts[3]-warm_counts[3],
        if DIAGNOSTICS { warm_counts[1] as i128-warm_counts[3] as i128-before.managed_committed_bytes as i128 } else { 0 },
        final_counts[1] as i128-final_counts[3] as i128);
}
