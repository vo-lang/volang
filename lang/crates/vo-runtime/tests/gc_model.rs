use std::collections::{HashSet, VecDeque};

use vo_runtime::bytecode::StructMeta;
use vo_runtime::gc::{Gc, GcRef, GcState};
use vo_runtime::gc_types::{scan_object, typed_write_barrier, ClosureScanLayout};
use vo_runtime::objects::{array, interface};
use vo_runtime::{SlotType, ValueKind, ValueMeta};

#[derive(Clone)]
struct ModelObject {
    ptr: GcRef,
    slot_types: Vec<SlotType>,
    edges: Vec<Option<GcRef>>,
}

struct GcModel {
    gc: Gc,
    struct_metas: Vec<StructMeta>,
    objects: Vec<ModelObject>,
    roots: Vec<GcRef>,
}

impl GcModel {
    fn new() -> Self {
        Self {
            gc: Gc::new(),
            struct_metas: Vec::new(),
            objects: Vec::new(),
            roots: Vec::new(),
        }
    }

    fn alloc_struct(&mut self, slot_types: Vec<SlotType>) -> GcRef {
        let slot_count = slot_types.len();
        let meta_id = self.struct_metas.len() as u32;
        self.struct_metas.push(StructMeta {
            slot_types: slot_types.clone(),
            fields: Vec::new(),
            field_index: Default::default(),
        });
        let ptr = self.gc.alloc(
            ValueMeta::new(meta_id, ValueKind::Struct),
            slot_types.len() as u16,
        );
        self.objects.push(ModelObject {
            ptr,
            slot_types,
            edges: vec![None; slot_count],
        });
        ptr
    }

    fn root(&mut self, ptr: GcRef) {
        self.roots.push(ptr);
    }

    fn clear_roots(&mut self) {
        self.roots.clear();
    }

    fn write_ref(&mut self, parent: GcRef, slot: usize, child: GcRef) {
        unsafe { Gc::write_slot(parent, slot, child as u64) };
        typed_write_barrier(&mut self.gc, parent, &[child as u64], &[SlotType::GcRef]);
        self.object_mut(parent).edges[slot] = Some(child);
    }

    fn write_value(&mut self, parent: GcRef, slot: usize, value: u64) {
        unsafe { Gc::write_slot(parent, slot, value) };
        self.object_mut(parent).edges[slot] = None;
    }

    fn write_interface(&mut self, parent: GcRef, slot: usize, iface: interface::InterfaceSlot) {
        unsafe {
            Gc::write_slot(parent, slot, iface.slot0);
            Gc::write_slot(parent, slot + 1, iface.slot1);
        }
        typed_write_barrier(
            &mut self.gc,
            parent,
            &[iface.slot0, iface.slot1],
            &[SlotType::Interface0, SlotType::Interface1],
        );
        self.object_mut(parent).edges[slot] = None;
        self.object_mut(parent).edges[slot + 1] = if iface.is_ref_type() {
            Some(iface.as_ref())
        } else {
            None
        };
    }

    fn full_gc(&mut self) {
        let roots = self.roots.clone();
        let struct_metas = &self.struct_metas;
        let previous_stress = self.gc.stress_every_step();
        self.gc.set_stress_every_step(true);
        for _ in 0..10_000 {
            gc_step(
                &mut self.gc,
                |gc| {
                    for &root in &roots {
                        gc.mark_gray(root);
                    }
                },
                |gc, obj| unsafe { scan_object(gc, obj, struct_metas, &empty_closure_layout) },
                |_| {},
            );
            if self.gc.last_step_stats().cycle_finished {
                self.gc.set_stress_every_step(previous_stress);
                return;
            }
        }
        self.gc.set_stress_every_step(previous_stress);
        panic!(
            "GC did not reach pause; state={:?} roots={}",
            self.gc.state(),
            self.roots.len()
        );
    }

    fn assert_survivors_match_model(&self) {
        let reachable = self.expected_reachable();
        for object in &self.objects {
            let expected_live = reachable.contains(&(object.ptr as usize));
            let actual_live = self.gc.canonicalize_ref(object.ptr).is_some();
            assert_eq!(
                actual_live, expected_live,
                "object {:?} survivor mismatch",
                object.ptr
            );
        }
    }

    fn assert_reachable_survive(&self) {
        let reachable = self.expected_reachable();
        for object in &self.objects {
            if reachable.contains(&(object.ptr as usize)) {
                assert_eq!(
                    self.gc.canonicalize_ref(object.ptr),
                    Some(object.ptr),
                    "reachable object {:?} was not retained",
                    object.ptr
                );
            }
        }
    }

    fn expected_reachable(&self) -> HashSet<usize> {
        let mut reachable = HashSet::new();
        let mut queue: VecDeque<GcRef> = self.roots.iter().copied().collect();
        while let Some(ptr) = queue.pop_front() {
            if ptr.is_null() || !reachable.insert(ptr as usize) {
                continue;
            }
            if let Some(object) = self.objects.iter().find(|object| object.ptr == ptr) {
                for child in object.edges.iter().flatten() {
                    queue.push_back(*child);
                }
            }
        }
        reachable
    }

    fn live_object_indices(&self) -> Vec<usize> {
        self.objects
            .iter()
            .enumerate()
            .filter_map(|(idx, object)| self.gc.canonicalize_ref(object.ptr).map(|_| idx))
            .collect()
    }

    fn object_mut(&mut self, ptr: GcRef) -> &mut ModelObject {
        self.objects
            .iter_mut()
            .find(|object| object.ptr == ptr)
            .expect("model object")
    }
}

fn empty_closure_layout(_: u32) -> ClosureScanLayout<'static> {
    ClosureScanLayout::default()
}

fn gc_step<R, S, F>(gc: &mut Gc, scan_roots: R, scan_object: S, finalize_object: F) -> usize
where
    R: FnMut(&mut Gc),
    S: FnMut(&mut Gc, GcRef),
    F: FnMut(GcRef),
{
    unsafe { gc.step(scan_roots, scan_object, finalize_object) }
}

struct Lcg {
    state: u64,
}

impl Lcg {
    fn new(seed: u64) -> Self {
        Self { state: seed }
    }

    fn next(&mut self) -> u64 {
        self.state = self
            .state
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        self.state
    }

    fn range(&mut self, upper: usize) -> usize {
        debug_assert!(upper > 0);
        (self.next() as usize) % upper
    }

    fn chance(&mut self, divisor: usize) -> bool {
        self.range(divisor) == 0
    }
}

fn fuzz_slot_layout(rng: &mut Lcg) -> Vec<SlotType> {
    let target_len = 1 + rng.range(6);
    let mut slots = Vec::new();
    while slots.len() < target_len {
        match rng.range(5) {
            0 => slots.push(SlotType::Value),
            1 => slots.push(SlotType::GcRef),
            2 => slots.push(SlotType::Float),
            3 if slots.len() + 1 < target_len => {
                slots.push(SlotType::Interface0);
                slots.push(SlotType::Interface1);
            }
            _ => slots.push(SlotType::Value),
        }
    }
    slots
}

fn choose_slot(
    model: &GcModel,
    rng: &mut Lcg,
    candidates: &[(usize, usize)],
) -> Option<(GcRef, usize)> {
    if candidates.is_empty() {
        return None;
    }
    let (object_idx, slot) = candidates[rng.range(candidates.len())];
    Some((model.objects[object_idx].ptr, slot))
}

#[test]
fn gc_model_simple_chain_survives_from_root() {
    let mut model = GcModel::new();
    let parent = model.alloc_struct(vec![SlotType::GcRef]);
    let child = model.alloc_struct(Vec::new());
    model.write_ref(parent, 0, child);
    model.root(parent);

    model.full_gc();

    model.assert_survivors_match_model();
}

#[test]
fn gc_model_disconnected_graph_is_swept() {
    let mut model = GcModel::new();
    let live = model.alloc_struct(Vec::new());
    let dead_parent = model.alloc_struct(vec![SlotType::GcRef]);
    let dead_child = model.alloc_struct(Vec::new());
    model.write_ref(dead_parent, 0, dead_child);
    model.root(live);

    model.full_gc();

    model.assert_survivors_match_model();
}

#[test]
fn gc_model_root_removal_collects_graph() {
    let mut model = GcModel::new();
    let parent = model.alloc_struct(vec![SlotType::GcRef]);
    let child = model.alloc_struct(Vec::new());
    model.write_ref(parent, 0, child);
    model.root(parent);
    model.full_gc();

    model.clear_roots();
    model.full_gc();

    model.assert_survivors_match_model();
}

#[test]
fn gc_model_mixed_slots_scan_only_refs() {
    let mut model = GcModel::new();
    let parent = model.alloc_struct(vec![SlotType::Value, SlotType::GcRef]);
    let child = model.alloc_struct(Vec::new());
    model.write_value(parent, 0, 0xfeed_face_cafe_beef);
    model.write_ref(parent, 1, child);
    model.root(parent);

    model.full_gc();

    model.assert_survivors_match_model();
}

#[test]
fn gc_model_interface_pair_scans_data_only_when_metadata_is_ref() {
    let mut model = GcModel::new();
    let immediate_parent = model.alloc_struct(vec![SlotType::Interface0, SlotType::Interface1]);
    let immediate_data = model.alloc_struct(Vec::new());
    model.write_interface(
        immediate_parent,
        0,
        interface::InterfaceSlot::from_i64(immediate_data as i64),
    );
    model.root(immediate_parent);

    let ref_parent = model.alloc_struct(vec![SlotType::Interface0, SlotType::Interface1]);
    let ref_child = model.alloc_struct(Vec::new());
    model.write_interface(
        ref_parent,
        0,
        interface::InterfaceSlot::from_ref(ref_child, 0, ValueKind::Struct),
    );
    model.root(ref_parent);

    model.full_gc();

    model.assert_survivors_match_model();
}

#[test]
fn gc_model_array_ref_elements_survive_from_root() {
    let mut gc = Gc::new();
    let struct_metas = vec![StructMeta {
        slot_types: Vec::new(),
        fields: Vec::new(),
        field_index: Default::default(),
    }];
    let child = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 0);
    let arr = array::create(&mut gc, ValueMeta::new(0, ValueKind::Pointer), 8, 1);
    unsafe { array::set(arr, 0, child as u64, 8) };
    typed_write_barrier(&mut gc, arr, &[child as u64], &[SlotType::GcRef]);

    gc.set_stress_every_step(true);
    for _ in 0..10_000 {
        gc_step(
            &mut gc,
            |gc| gc.mark_gray(arr),
            |gc, obj| unsafe { scan_object(gc, obj, &struct_metas, &empty_closure_layout) },
            |_| {},
        );
        if gc.last_step_stats().cycle_finished {
            assert_eq!(gc.canonicalize_ref(arr), Some(arr));
            assert_eq!(gc.canonicalize_ref(child), Some(child));
            return;
        }
    }
    panic!("GC did not reach pause");
}

#[test]
fn gc_model_black_parent_write_barrier_rescues_white_child() {
    let mut model = GcModel::new();
    let mut slot_types = vec![SlotType::GcRef, SlotType::GcRef];
    slot_types.extend(std::iter::repeat_n(SlotType::Value, 2048));
    let parent = model.alloc_struct(slot_types);
    let filler = model.alloc_struct(Vec::new());
    let child = model.alloc_struct(Vec::new());
    model.write_ref(parent, 0, filler);
    model.root(parent);

    let roots = vec![parent];
    let struct_metas = &model.struct_metas;
    for _ in 0..10_000 {
        gc_step(
            &mut model.gc,
            |gc| {
                for &root in &roots {
                    gc.mark_gray(root);
                }
            },
            |gc, obj| unsafe { scan_object(gc, obj, struct_metas, &empty_closure_layout) },
            |_| {},
        );
        if model.gc.is_black(parent) && model.gc.state() == GcState::Propagate {
            break;
        }
    }
    assert!(
        model.gc.is_black(parent),
        "parent should be black before late write"
    );
    assert_eq!(model.gc.state(), GcState::Propagate);

    model.write_ref(parent, 1, child);
    model.full_gc();

    model.assert_survivors_match_model();
}

#[test]
fn gc_model_deterministic_fuzz_smoke_preserves_reachable_roots() {
    let mut rng = Lcg::new(0x766f_6763_6675_7a7a);
    let mut model = GcModel::new();
    let root = model.alloc_struct(vec![
        SlotType::Value,
        SlotType::GcRef,
        SlotType::Interface0,
        SlotType::Interface1,
    ]);
    model.root(root);

    for step in 0..256 {
        let live = model.live_object_indices();
        if live.is_empty() || rng.chance(4) {
            let ptr = model.alloc_struct(fuzz_slot_layout(&mut rng));
            model.root(ptr);
            continue;
        }

        match rng.range(6) {
            0 => {
                let ptr = model.objects[live[rng.range(live.len())]].ptr;
                model.root(ptr);
            }
            1 => model.full_gc(),
            2 => {
                let mut ref_slots = Vec::new();
                for &object_idx in &live {
                    let object = &model.objects[object_idx];
                    for (slot, slot_type) in object.slot_types.iter().enumerate() {
                        if *slot_type == SlotType::GcRef && object.edges[slot].is_none() {
                            ref_slots.push((object_idx, slot));
                        }
                    }
                }
                if let Some((parent, slot)) = choose_slot(&model, &mut rng, &ref_slots) {
                    let child = model.objects[live[rng.range(live.len())]].ptr;
                    model.write_ref(parent, slot, child);
                }
            }
            3 => {
                let mut value_slots = Vec::new();
                for &object_idx in &live {
                    let object = &model.objects[object_idx];
                    for (slot, slot_type) in object.slot_types.iter().enumerate() {
                        if *slot_type == SlotType::Value {
                            value_slots.push((object_idx, slot));
                        }
                    }
                }
                if let Some((parent, slot)) = choose_slot(&model, &mut rng, &value_slots) {
                    model.write_value(parent, slot, rng.next());
                }
            }
            4 => {
                let mut interface_slots = Vec::new();
                for &object_idx in &live {
                    let object = &model.objects[object_idx];
                    for (slot, pair) in object.slot_types.windows(2).enumerate() {
                        if pair == [SlotType::Interface0, SlotType::Interface1]
                            && object.edges[slot + 1].is_none()
                        {
                            interface_slots.push((object_idx, slot));
                        }
                    }
                }
                if let Some((parent, slot)) = choose_slot(&model, &mut rng, &interface_slots) {
                    let iface = if rng.chance(2) {
                        let child = model.objects[live[rng.range(live.len())]].ptr;
                        interface::InterfaceSlot::from_ref(child, 0, ValueKind::Struct)
                    } else {
                        interface::InterfaceSlot::from_i64(rng.next() as i64)
                    };
                    model.write_interface(parent, slot, iface);
                }
            }
            _ => model.full_gc(),
        }

        if step % 17 == 0 {
            model.full_gc();
            model.assert_reachable_survive();
        }
    }

    model.full_gc();
    model.assert_reachable_survive();
}
