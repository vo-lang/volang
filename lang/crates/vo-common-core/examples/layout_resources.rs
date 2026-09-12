//! Verify/load and public layout-view resources for one frozen bytecode image.
//! Build with VO_LAYOUT_ALLOCATION_DIAGNOSTICS=1 for allocation accounting;
//! the ordinary timing build compiles out every allocation counter.
use std::alloc::{GlobalAlloc, Layout, System};
use std::hint::black_box;
use std::sync::atomic::{AtomicU64, Ordering::Relaxed};
use std::time::Instant;
use vo_common_core::execution_layouts::{ElementLayoutMaps, PointerLayoutMaps};
use vo_common_core::{bytecode::LoadedModule, verifier::verify_loaded_module, Module, SlotType};

const DIAGNOSTICS: bool = option_env!("VO_LAYOUT_ALLOCATION_DIAGNOSTICS").is_some();
static CALLS: AtomicU64 = AtomicU64::new(0);
static BYTES: AtomicU64 = AtomicU64::new(0);
static LIVE: AtomicU64 = AtomicU64::new(0);
static PEAK: AtomicU64 = AtomicU64::new(0);
struct Allocator;

fn allocated(pointer: *mut u8, old: usize, new: usize) {
    if DIAGNOSTICS && !pointer.is_null() {
        CALLS.fetch_add(1, Relaxed);
        BYTES.fetch_add(new as u64, Relaxed);
        // Successful realloc replaces the old logical request. This measures
        // requested live bytes, not allocator-internal overlapping storage.
        let live = if new >= old {
            LIVE.fetch_add((new - old) as u64, Relaxed) + (new - old) as u64
        } else {
            LIVE.fetch_sub((old - new) as u64, Relaxed) - (old - new) as u64
        };
        PEAK.fetch_max(live, Relaxed);
    }
}

unsafe impl GlobalAlloc for Allocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc(layout) };
        allocated(pointer, 0, layout.size());
        pointer
    }
    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc_zeroed(layout) };
        allocated(pointer, 0, layout.size());
        pointer
    }
    unsafe fn realloc(&self, pointer: *mut u8, layout: Layout, size: usize) -> *mut u8 {
        let result = unsafe { System.realloc(pointer, layout, size) };
        allocated(result, layout.size(), size);
        result
    }
    unsafe fn dealloc(&self, pointer: *mut u8, layout: Layout) {
        if DIAGNOSTICS {
            LIVE.fetch_sub(layout.size() as u64, Relaxed);
        }
        unsafe { System.dealloc(pointer, layout) };
    }
}

#[global_allocator]
static ALLOCATOR: Allocator = Allocator;

#[derive(Clone, Copy)]
struct Counts {
    calls: u64,
    bytes: u64,
    live: u64,
}
fn counts() -> Counts {
    Counts {
        calls: CALLS.load(Relaxed),
        bytes: BYTES.load(Relaxed),
        live: LIVE.load(Relaxed),
    }
}
#[derive(Clone, Copy)]
struct Cost {
    ns: u128,
    calls: u64,
    bytes: u64,
    live_delta: i128,
    peak_extra: u64,
}
fn measure<T>(operation: impl FnOnce() -> T) -> (T, Cost) {
    let before = counts();
    PEAK.store(before.live, Relaxed);
    let started = Instant::now();
    let result = operation();
    let ns = started.elapsed().as_nanos();
    let after = counts();
    let peak_extra = PEAK.load(Relaxed) - before.live;
    (
        result,
        Cost {
            ns,
            calls: after.calls - before.calls,
            bytes: after.bytes - before.bytes,
            live_delta: after.live as i128 - before.live as i128,
            peak_extra,
        },
    )
}
impl Cost {
    fn json(self) -> String {
        format!("{{\"elapsed_ns\":{},\"allocation_requests\":{},\"requested_bytes\":{},\"live_delta_bytes\":{},\"peak_extra_bytes\":{}}}",self.ns,self.calls,self.bytes,self.live_delta,self.peak_extra)
    }
}

enum Views {
    Pointers(PointerLayoutMaps),
    Elements(ElementLayoutMaps),
    Both(PointerLayoutMaps, ElementLayoutMaps),
}
fn mix(hash: &mut u64, value: u64) {
    *hash = (*hash ^ value).wrapping_mul(0x100000001b3);
}
fn verify_facts(loaded: &LoadedModule) -> u64 {
    let mut hash = 0xcbf29ce484222325;
    for (id, function) in loaded.functions.iter().enumerate() {
        let pointers = loaded.pointer_layout_maps().function(id as u32).unwrap();
        let elements = loaded.element_layout_maps().function(id as u32).unwrap();
        let bases = loaded.exact_base_maps().function(id as u32).unwrap();
        assert_eq!(elements.len(), function.instruction_metadata.len());
        for (pc, metadata) in function.instruction_metadata.iter().enumerate() {
            mix(&mut hash, id as u64);
            mix(&mut hash, pc as u64);
            match (metadata.ptr_value_layout(), pointers.get(pc)) {
                (None, None) => mix(&mut hash, 0),
                (Some(layout), Some(fact)) => {
                    assert_eq!(fact.value_slots as usize, layout.len());
                    assert_eq!(
                        fact.needs_write_barrier,
                        layout
                            .first()
                            .is_some_and(|slot| slot.needs_write_barrier())
                    );
                    assert_eq!(
                        fact.supports_exact_barrier,
                        matches!(layout.first(), Some(SlotType::GcBase | SlotType::GcRef))
                    );
                    assert_eq!(fact.base_provenance, bases.write_barrier(pc));
                    mix(&mut hash, 1);
                    mix(&mut hash, fact.value_slots as u64);
                    mix(&mut hash, fact.needs_write_barrier as u64);
                    mix(&mut hash, fact.supports_exact_barrier as u64);
                    mix(&mut hash, fact.base_provenance.parent_is_exact() as u64);
                    mix(&mut hash, fact.base_provenance.child_is_exact() as u64);
                }
                other => panic!("pointer fact mismatch {other:?}"),
            }
            match (metadata.elem_layout(), elements.get(pc)) {
                (None, None) => mix(&mut hash, 0),
                (Some(expected), Some(actual)) => {
                    assert_eq!(actual.bytes, expected.bytes);
                    assert_eq!(actual.slots, expected.slots);
                    assert_eq!(actual.needs_sign_extend, expected.needs_sign_extend);
                    mix(&mut hash, 1);
                    mix(&mut hash, actual.bytes as u64);
                    mix(&mut hash, actual.slots as u64);
                    mix(&mut hash, actual.needs_sign_extend as u64);
                }
                other => panic!("element fact mismatch {other:?}"),
            }
        }
        assert_eq!(pointers.get(function.instruction_metadata.len()), None);
        assert!(elements.get(function.instruction_metadata.len()).is_none());
    }
    hash
}

struct Record {
    iteration: usize,
    load: Cost,
    clone: Cost,
    retained: u64,
    digest: u64,
}
fn main() {
    let args: Vec<_> = std::env::args().collect();
    assert_eq!(
        args.len(),
        5,
        "usage: layout_resources INPUT.vob pointers|elements|both SAMPLES timing|allocations"
    );
    assert_eq!(DIAGNOSTICS, args[4] == "allocations");
    assert!(matches!(args[4].as_str(), "timing" | "allocations"));
    assert!(matches!(args[2].as_str(), "pointers" | "elements" | "both"));
    let samples: usize = args[3].parse().unwrap();
    assert!((1..=1000).contains(&samples));
    let bytes = vo_common_core::serialize::read_vob_file(std::path::Path::new(&args[1])).unwrap();
    let mut records = Vec::with_capacity(samples);
    let mut previous = None;
    for iteration in 0..samples + 2 {
        let baseline = counts().live;
        let module = Module::deserialize(&bytes).unwrap();
        let (loaded, load) = measure(|| verify_loaded_module(module).unwrap());
        let digest = verify_facts(&loaded);
        if let Some(expected) = previous {
            assert_eq!(digest, expected);
        }
        previous = Some(digest);
        let (views, clone) = measure(|| match args[2].as_str() {
            "pointers" => Views::Pointers(black_box(loaded.pointer_layout_maps()).clone()),
            "elements" => Views::Elements(black_box(loaded.element_layout_maps()).clone()),
            "both" => Views::Both(
                black_box(loaded.pointer_layout_maps()).clone(),
                black_box(loaded.element_layout_maps()).clone(),
            ),
            _ => unreachable!(),
        });
        match &views {
            Views::Pointers(p) => assert_eq!(p, loaded.pointer_layout_maps()),
            Views::Elements(e) => assert_eq!(e, loaded.element_layout_maps()),
            Views::Both(p, e) => {
                assert_eq!(p, loaded.pointer_layout_maps());
                assert_eq!(e, loaded.element_layout_maps());
            }
        }
        drop(loaded);
        let retained = counts().live.checked_sub(baseline).unwrap();
        // The view remains usable after module destruction. Equality and all
        // metadata facts were checked outside the measured load/clone spans.
        match black_box(&views) {
            Views::Pointers(p) => {
                black_box(p.function(0));
            }
            Views::Elements(e) => {
                black_box(e.function(0));
            }
            Views::Both(p, e) => {
                black_box(p.function(0));
                black_box(e.function(0));
            }
        }
        drop(views);
        assert_eq!(
            counts().live,
            baseline,
            "loaded module or view allocation retained after final drop"
        );
        if iteration >= 2 {
            records.push(Record {
                iteration: iteration - 2,
                load,
                clone,
                retained,
                digest,
            });
        }
    }
    print!("{{\"schema\":\"volang.layout-resources.v1\",\"allocation_diagnostics\":{DIAGNOSTICS},\"view\":\"{}\",\"vob_bytes\":{},\"samples\":[", args[2],bytes.len());
    for (i, row) in records.into_iter().enumerate() {
        if i != 0 {
            print!(",");
        }
        print!("{{\"iteration\":{},\"load\":{},\"clone\":{},\"view_retained_bytes\":{},\"layout_digest\":{}}}",row.iteration,row.load.json(),row.clone.json(),row.retained,row.digest);
    }
    println!("]}}");
}
