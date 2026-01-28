//! Island thread execution - runs a VM instance for an island.

use std::collections::HashMap;
use std::sync::Arc;
use std::sync::mpsc::{Receiver, Sender};

use vo_runtime::gc::GcRef;
use vo_runtime::island::IslandCommand;
use vo_runtime::island_msg::{self, PortWire};

use crate::bytecode::Module;
use crate::fiber::Fiber;
use super::Vm;

/// Shared registry of island command senders.
/// Key 0 = main island.
pub type IslandRegistry = Arc<std::sync::Mutex<HashMap<u32, Sender<IslandCommand>>>>;

/// Run an island thread - processes commands and executes fibers.
pub fn run_island_thread(
    island_id: u32,
    module: Arc<Module>,
    cmd_rx: Receiver<IslandCommand>,
    island_registry: IslandRegistry,
) {
    let mut vm = Vm::new();
    vm.load((*module).clone());
    
    // Register our ability to send to other islands
    vm.state.island_registry = Some(island_registry);
    vm.state.current_island_id = island_id;
    
    loop {
        // Process pending commands
        match cmd_rx.try_recv() {
            Ok(cmd) => {
                if handle_command(&mut vm, cmd) { break; }
            }
            Err(std::sync::mpsc::TryRecvError::Empty) => {
                if vm.scheduler.has_runnable() {
                    let _ = vm.run_scheduled();
                } else {
                    // Block waiting for command
                    match cmd_rx.recv() {
                        Ok(cmd) => { if handle_command(&mut vm, cmd) { break; } }
                        Err(_) => break,
                    }
                }
            }
            Err(std::sync::mpsc::TryRecvError::Disconnected) => break,
        }
    }
    
}

/// Returns true if should exit loop.
fn handle_command(vm: &mut Vm, cmd: IslandCommand) -> bool {
    match cmd {
        IslandCommand::Shutdown => true,
        IslandCommand::SpawnFiber { closure_data, .. } => {
            handle_spawn_fiber(vm, closure_data.data());
            false
        }
        IslandCommand::WakeFiber { fiber_id } => {
            vm.scheduler.wake_fiber(crate::scheduler::FiberId::from_raw(fiber_id));
            let _ = vm.run_scheduled();
            false
        }
    }
}

fn handle_spawn_fiber(vm: &mut Vm, data: &[u8]) {
    // Decode spawn payload
    let payload = island_msg::decode_spawn_header(data);
    
    // Build port maps from decoded wires
    let (capture_port_map, arg_port_map) = build_port_maps(vm, &payload.port_wires);
    
    // Get function info from module
    let (local_slots, capture_types, param_types, struct_metas, runtime_types) = {
        let module = vm.module().expect("module loaded");
        let func_idx = payload.func_id as usize;
        assert!(func_idx < module.functions.len(), "island spawn: invalid func_id {}", payload.func_id);
        let func_def = &module.functions[func_idx];
        (
            func_def.local_slots,
            func_def.capture_types.clone(),
            func_def.param_types.clone(),
            module.struct_metas.clone(),
            module.runtime_types.clone(),
        )
    };
    
    // Unpack captures (proper deep copy)
    let (unpacked_captures, args_data_offset) = island_msg::unpack_captures(
        &mut vm.state.gc,
        data,
        payload.data_offset,
        payload.num_captures,
        &capture_types,
        &struct_metas,
        &runtime_types,
    );
    
    // Unpack args (proper deep copy)
    let unpacked_args = island_msg::unpack_args(
        &mut vm.state.gc,
        data,
        args_data_offset,
        payload.num_args,
        &param_types,
        &struct_metas,
        &runtime_types,
    );
    
    // Create closure with captures
    let closure_ref = vo_runtime::objects::closure::create(
        &mut vm.state.gc, payload.func_id, payload.num_captures as usize
    );
    
    // Set captures, replacing ports with recreated ones
    for (i, &cap_ref) in unpacked_captures.iter().enumerate() {
        if let Some(&(new_port, is_boxed)) = capture_port_map.get(&(i as u16)) {
            if is_boxed {
                // Boxed port - create new box containing new port
                let new_box = vm.state.gc.alloc(
                    vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Struct),
                    1
                );
                unsafe { vo_runtime::gc::Gc::write_slot(new_box, 0, new_port as u64); }
                vo_runtime::objects::closure::set_capture(closure_ref, i, new_box as u64);
            } else {
                // Direct port capture
                vo_runtime::objects::closure::set_capture(closure_ref, i, new_port as u64);
            }
        } else {
            // Use unpacked capture (properly deep-copied)
            vo_runtime::objects::closure::set_capture(closure_ref, i, cap_ref as u64);
        }
    }
    
    // Create fiber
    let mut fiber = Fiber::new(0);
    fiber.push_frame(payload.func_id, local_slots, 0, 0);
    fiber.stack[0] = closure_ref as u64;
    
    // Copy unpacked args to fiber stack, replacing ports with recreated ones
    let mut arg_slot_idx = 0;
    for (param_idx, &(_meta_raw, slots)) in param_types.iter().enumerate() {
        if let Some(&(new_port, is_boxed)) = arg_port_map.get(&(param_idx as u16)) {
            if is_boxed {
                let new_box = vm.state.gc.alloc(
                    vo_runtime::ValueMeta::new(0, vo_runtime::ValueKind::Struct),
                    1
                );
                unsafe { vo_runtime::gc::Gc::write_slot(new_box, 0, new_port as u64); }
                fiber.stack[1 + arg_slot_idx] = new_box as u64;
            } else {
                fiber.stack[1 + arg_slot_idx] = new_port as u64;
            }
            arg_slot_idx += slots as usize;
        } else {
            // Copy unpacked arg slots
            for j in 0..slots as usize {
                if arg_slot_idx + j < unpacked_args.len() {
                    fiber.stack[1 + arg_slot_idx + j] = unpacked_args[arg_slot_idx + j];
                }
            }
            arg_slot_idx += slots as usize;
        }
    }
    
    vm.scheduler.spawn(fiber);
    
    let _ = vm.run_scheduled();
}

/// Build port maps from decoded PortWire list.
/// Each map: idx -> (new_port_gcref, is_boxed)
fn build_port_maps(
    vm: &mut Vm, 
    port_wires: &[PortWire],
) -> (HashMap<u16, (GcRef, bool)>, HashMap<u16, (GcRef, bool)>) {
    let mut capture_port_map = HashMap::new();
    let mut arg_port_map = HashMap::new();
    
    for wire in port_wires {
        let elem_meta = vo_runtime::ValueMeta::from_raw(wire.meta_raw);
        let new_port = vo_runtime::objects::port::create_from_raw(
            &mut vm.state.gc, wire.state_ptr, wire.cap, elem_meta, wire.elem_slots
        );
        
        if wire.is_arg {
            arg_port_map.insert(wire.idx, (new_port, wire.is_boxed));
        } else {
            capture_port_map.insert(wire.idx, (new_port, wire.is_boxed));
        }
    }
    
    (capture_port_map, arg_port_map)
}

