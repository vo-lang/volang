//! Reproducible region-shape diagnostic. Build separately from measurement:
//! cargo build -p vo-runtime --example allocation_regions --profile release-native --locked
//! target/release-native/examples/allocation_regions
//! Output is JSON Lines; iterations 0 and 1 are warmups. Initialization and
//! destruction are outside each timed interval. GC is disabled to isolate
//! allocation work; the language catalog covers end-to-end execution with GC.

use std::hint::black_box;
use std::time::Instant;
use vo_runtime::gc::{Gc, VmMemoryConfig};
use vo_runtime::{ValueKind, ValueMeta};

fn measure(region: bool, batch: usize, mixed_sizes: bool, iteration: usize) {
    const N: usize = 262_144;
    let mut gc = Gc::with_memory_config(VmMemoryConfig {
        automatic_gc: false,
        initial_reserve_bytes: 16 * 1024 * 1024,
        ..VmMemoryConfig::default()
    })
    .unwrap();
    let metas = [
        ValueMeta::new(0, ValueKind::Int),
        ValueMeta::new(0, ValueKind::Uint),
    ];
    let started = Instant::now();
    let mut sum = 0_u64;
    for i in 0..N {
        let meta = metas[(i / black_box(batch)) & 1];
        let slots = if mixed_sizes {
            1 + ((i / black_box(batch)) & 1) as u16
        } else {
            1
        };
        let object = if region {
            gc.try_alloc_value_slots_in_region(meta, slots)
        } else {
            gc.try_alloc_value_slots(meta, slots)
        }
        .unwrap();
        unsafe {
            *object = i as u64;
            sum += *black_box(object);
        }
    }
    gc.close_value_slot_allocation_region_for_boundary();
    let ns = started.elapsed().as_nanos();
    let stats = gc.memory_stats();
    assert_eq!(sum, (N as u64) * (N as u64 - 1) / 2);
    assert_eq!(stats.object_count, N);
    assert_eq!(stats.allocation_failures, 0);
    println!("{{\"region\":{region},\"batch\":{batch},\"mixed_sizes\":{mixed_sizes},\"iteration\":{iteration},\"n\":{N},\"elapsed_ns\":{ns},\"allocation_bytes\":{},\"objects\":{},\"checksum\":{sum}}}", stats.allocation_bytes_total, stats.object_count);
}

fn main() {
    const N: usize = 262_144;
    for iteration in 0..7 {
        let cases = [
            (true, 1),
            (true, 2),
            (true, 64),
            (true, N),
            (false, 1),
            (false, 2),
            (false, 64),
            (false, N),
        ];
        if iteration % 2 == 0 {
            for (region, batch) in cases {
                measure(region, batch, false, iteration);
                measure(region, batch, true, iteration);
            }
        } else {
            for (region, batch) in cases.into_iter().rev() {
                measure(region, batch, false, iteration);
                measure(region, batch, true, iteration);
            }
        }
    }
}
