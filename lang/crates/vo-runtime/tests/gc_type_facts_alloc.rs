use std::alloc::{GlobalAlloc, Layout, System};
use std::cell::Cell;
use std::collections::BTreeMap;

use vo_runtime::bytecode::{
    InterfaceMeta, InterfaceMethodMeta, Itab, MethodInfo, Module, NamedTypeMeta, RuntimeTypeFacts,
};
use vo_runtime::gc::{Gc, GcObjectScanChunk, GcTraceCursor};
use vo_runtime::gc_types::{
    scan_object_chunk_with_context, typed_write_barrier_range_by_meta, ClosureScanLayout,
    GcScanContext,
};
use vo_runtime::itab::{check_interface_satisfaction, validate_interface_itab, ItabCache};
use vo_runtime::objects::string;
use vo_runtime::slot::SLOT_BYTES;
use vo_runtime::{RuntimeType, ValueKind, ValueMeta, ValueRttid};

struct ThreadCountingAllocator;

thread_local! {
    static COUNTING: Cell<bool> = const { Cell::new(false) };
    static ALLOCATIONS: Cell<usize> = const { Cell::new(0) };
}

unsafe impl GlobalAlloc for ThreadCountingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let ptr = unsafe { System.alloc(layout) };
        if !ptr.is_null() {
            record_allocation();
        }
        ptr
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let ptr = unsafe { System.alloc_zeroed(layout) };
        if !ptr.is_null() {
            record_allocation();
        }
        ptr
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        unsafe { System.dealloc(ptr, layout) };
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        let ptr = unsafe { System.realloc(ptr, layout, new_size) };
        if !ptr.is_null() {
            record_allocation();
        }
        ptr
    }
}

#[global_allocator]
static ALLOCATOR: ThreadCountingAllocator = ThreadCountingAllocator;

fn record_allocation() {
    COUNTING.with(|counting| {
        if counting.get() {
            ALLOCATIONS.with(|allocations| allocations.set(allocations.get() + 1));
        }
    });
}

fn count_allocations(run: impl FnOnce()) -> usize {
    ALLOCATIONS.with(|allocations| allocations.set(0));
    COUNTING.with(|counting| counting.set(true));
    run();
    COUNTING.with(|counting| counting.set(false));
    ALLOCATIONS.with(Cell::get)
}

#[test]
fn cached_array_scan_and_range_barrier_allocate_nothing() {
    const WIDTH: usize = 4_096;

    let runtime_types = [
        RuntimeType::Basic(ValueKind::String),
        RuntimeType::Array {
            len: WIDTH as u64,
            elem: ValueRttid::new(0, ValueKind::String),
        },
    ];
    let facts = RuntimeTypeFacts::from_module_parts(&[], &[], &runtime_types)
        .expect("valid runtime type facts");
    let mut gc = Gc::new();
    let leaf = string::create(&mut gc, b"allocation-probe");
    gc.mark_gray(leaf);
    let value = gc.alloc_value_slots(ValueMeta::new(1, ValueKind::Array), WIDTH as u16);
    for slot in 0..WIDTH {
        unsafe { Gc::write_slot(value, slot, leaf as u64) };
    }
    let mut cursor = GcTraceCursor::default();

    let scan_allocations = count_allocations(|| {
        let chunk = unsafe {
            scan_object_chunk_with_context(
                &mut gc,
                value,
                GcScanContext::with_runtime_type_facts(&[], &facts),
                &|_| ClosureScanLayout::default(),
                &mut cursor,
                WIDTH * SLOT_BYTES,
            )
        };
        assert_eq!(chunk, GcObjectScanChunk::complete(WIDTH * SLOT_BYTES));
    });
    assert_eq!(scan_allocations, 0, "cached array scan allocated");

    let values = vec![leaf as u64; WIDTH];
    let barrier_allocations = count_allocations(|| unsafe {
        typed_write_barrier_range_by_meta(
            &mut gc,
            value,
            values.as_ptr().cast(),
            WIDTH,
            SLOT_BYTES,
            ValueMeta::new(0, ValueKind::String),
            None,
        );
    });
    assert_eq!(barrier_allocations, 0, "range barrier allocated");
}

#[test]
fn interface_satisfaction_and_itab_validation_allocate_nothing() {
    let mut module = Module::new("interface-allocation-probe".to_string());
    module.runtime_types.extend([
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Named {
            id: 0,
            struct_meta_id: None,
        },
    ]);
    let mut methods = BTreeMap::new();
    methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 7,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 2,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "T".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
        methods,
    });
    module.interface_metas.push(InterfaceMeta {
        name: "I".to_string(),
        method_names: vec!["M".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "M".to_string(),
            signature_rttid: 2,
        }],
    });
    let cache = ItabCache::from_module_itabs(vec![
        Itab::default(),
        Itab {
            iface_meta_id: 0,
            methods: vec![7],
        },
    ]);
    let value_rttid = ValueRttid::new(1, ValueKind::Int64);

    let allocations = count_allocations(|| {
        for _ in 0..100 {
            assert!(check_interface_satisfaction(
                value_rttid.rttid(),
                value_rttid.value_kind(),
                0,
                &module,
            ));
            assert_eq!(
                validate_interface_itab(&module, &cache, 0, 1, Some(value_rttid)),
                Ok(())
            );
        }
    });
    assert_eq!(allocations, 0, "interface contract validation allocated");
}
