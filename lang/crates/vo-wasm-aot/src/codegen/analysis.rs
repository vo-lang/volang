//! Closed-world reachability, effects, and ABI planning.
use super::*;

/// Derived facts belong to one immutable compilation input. Lowering and
/// reachability share these caches; no process-global invalidation is needed.
pub(super) struct ModuleAnalysis<'a> {
    module: &'a VoModule,
    signatures: std::sync::OnceLock<Vec<DynamicFunctionSignature>>,
    closures: std::sync::OnceLock<BTreeMap<u32, BTreeSet<u16>>>,
    interfaces: std::cell::RefCell<BTreeMap<u32, Result<Vec<(u32, Vec<u32>)>, WasmAotError>>>,
}

impl<'a> ModuleAnalysis<'a> {
    pub(super) fn new(module: &'a VoModule) -> Self {
        Self {
            module,
            signatures: Default::default(),
            closures: Default::default(),
            interfaces: Default::default(),
        }
    }
}
impl core::ops::Deref for ModuleAnalysis<'_> {
    type Target = VoModule;
    fn deref(&self) -> &Self::Target {
        self.module
    }
}

// Portable Wasm engines admit at least this many parameters/results. Keep
// aggregate expansion below the validator limits; wider values use slots.
const MAX_FAST_ABI_PARAMS: usize = 1000;
const MAX_FAST_ABI_RESULTS: usize = 1000;

pub(super) fn closure_target_ids(module: &ModuleAnalysis<'_>) -> BTreeSet<u32> {
    let mut targets: BTreeSet<u32> = module
        .functions
        .iter()
        .flat_map(|function| function.code.iter())
        .filter_map(|instruction| {
            (instruction.opcode() == Opcode::ClosureNew)
                .then_some(instruction.closure_new_func_id())
        })
        .collect();
    targets.extend(
        module
            .named_type_metas
            .iter()
            .flat_map(|named| named.methods.values().map(|method| method.func_id)),
    );
    targets
}

/// Closure bodies that the bytecode constructs explicitly. Named methods are
/// added to `closure_target_ids` for reflective dynamic method lookup, but
/// they are speculative until such a lookup succeeds at runtime.
pub(super) fn explicit_closure_target_ids(module: &ModuleAnalysis<'_>) -> BTreeSet<u32> {
    module
        .functions
        .iter()
        .flat_map(|function| function.code.iter())
        .filter_map(|instruction| {
            (instruction.opcode() == Opcode::ClosureNew)
                .then_some(instruction.closure_new_func_id())
        })
        .collect()
}

pub(super) fn dynamic_function_signatures<'a>(
    module: &'a ModuleAnalysis<'_>,
) -> &'a [DynamicFunctionSignature] {
    module
        .signatures
        .get_or_init(|| compute_dynamic_function_signatures(module))
}

fn compute_dynamic_function_signatures(
    module: &ModuleAnalysis<'_>,
) -> Vec<DynamicFunctionSignature> {
    (0..module.runtime_types.len() as u32)
        .filter_map(|rttid| {
            let value_rttid = module.value_rttid_for_rttid(rttid)?;
            if value_rttid.value_kind() != ValueKind::Closure {
                return None;
            }
            let (
                _,
                RuntimeType::Func {
                    params,
                    results,
                    variadic,
                },
            ) = module
                .runtime_type_resolver()
                .resolve_value_rttid(value_rttid)?
            else {
                return None;
            };
            Some(DynamicFunctionSignature {
                value_rttid,
                params: params.clone(),
                results: results.clone(),
                variadic: *variadic,
            })
        })
        .collect()
}

pub(super) fn dynamic_function_signature(
    module: &ModuleAnalysis<'_>,
    signature_rttid: u32,
) -> Result<DynamicFunctionSignature, WasmAotError> {
    let missing = || {
        WasmAotError::InvalidModule(format!(
            "dynamic function signature RTTID {signature_rttid} is missing"
        ))
    };
    let value_rttid = module
        .value_rttid_for_rttid(signature_rttid)
        .ok_or_else(missing)?;
    let (
        _,
        RuntimeType::Func {
            params,
            results,
            variadic,
        },
    ) = module
        .runtime_type_resolver()
        .resolve_value_rttid(value_rttid)
        .ok_or_else(missing)?
    else {
        return Err(missing());
    };
    Ok(DynamicFunctionSignature {
        value_rttid,
        params: params.clone(),
        results: results.clone(),
        variadic: *variadic,
    })
}

pub(super) fn flattened_value_layout(
    module: &ModuleAnalysis<'_>,
    values: &[ValueRttid],
) -> Result<Vec<vo_common_core::SlotType>, WasmAotError> {
    let mut layout = Vec::new();
    for value in values {
        layout.extend(module.slot_layout_for_value_rttid(*value).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "dynamic function signature type {} has no physical layout",
                value.rttid()
            ))
        })?);
    }
    Ok(layout)
}

pub(super) fn dynamic_signature_matches_target(
    module: &ModuleAnalysis<'_>,
    signature: &DynamicFunctionSignature,
    target: ClosureCallTarget,
) -> Result<bool, WasmAotError> {
    let function = module
        .functions
        .get(target.function_id as usize)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "dynamic closure references missing function {}",
                target.function_id
            ))
        })?;
    let explicit_layout = function
        .slot_types
        .get(usize::from(target.abi.arg_offset)..usize::from(function.param_slots))
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "dynamic closure {} has a truncated parameter layout",
                function.name
            ))
        })?;
    Ok(
        explicit_layout == flattened_value_layout(module, &signature.params)?
            && function.ret_slot_types == flattened_value_layout(module, &signature.results)?,
    )
}

pub(super) fn dynamic_closure_targets_for_signature(
    module: &ModuleAnalysis<'_>,
    signature: &DynamicFunctionSignature,
    instantiations: &BTreeMap<u32, BTreeSet<u16>>,
) -> Result<Vec<ClosureCallTarget>, WasmAotError> {
    let mut targets = Vec::new();
    for (&function_id, capture_counts) in instantiations {
        let function = module.functions.get(function_id as usize).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "closure instantiation references missing function {function_id}"
            ))
        })?;
        for &capture_slots in capture_counts {
            let target = ClosureCallTarget {
                function_id,
                capture_slots,
                abi: closure_call_abi(function, capture_slots)?,
            };
            if dynamic_signature_matches_target(module, signature, target)? {
                targets.push(target);
            }
        }
    }
    Ok(targets)
}

pub(super) fn closure_instantiations<'a>(
    module: &'a ModuleAnalysis<'_>,
) -> &'a BTreeMap<u32, BTreeSet<u16>> {
    module
        .closures
        .get_or_init(|| compute_closure_instantiations(module))
}

fn compute_closure_instantiations(module: &ModuleAnalysis<'_>) -> BTreeMap<u32, BTreeSet<u16>> {
    let mut instantiations = BTreeMap::<u32, BTreeSet<u16>>::new();
    for function in &module.functions {
        for instruction in &function.code {
            if instruction.opcode() == Opcode::ClosureNew {
                instantiations
                    .entry(instruction.closure_new_func_id())
                    .or_default()
                    .insert(instruction.c);
            }
        }
    }
    for method in module
        .named_type_metas
        .iter()
        .flat_map(|named| named.methods.values())
    {
        if let Some(target) = module.functions.get(method.func_id as usize) {
            instantiations
                .entry(method.func_id)
                .or_default()
                .insert(target.recv_slots);
        }
    }
    instantiations
}

pub(super) fn closure_call_abi(
    target: &FunctionDef,
    capture_slots: u16,
) -> Result<ClosureCallAbi, WasmAotError> {
    let abi = if target.recv_slots > 0 && capture_slots > 0 {
        if target.recv_slots != capture_slots {
            return Err(WasmAotError::InvalidModule(format!(
                "method closure {} has recv_slots={} but capture_slots={capture_slots}",
                target.name, target.recv_slots
            )));
        }
        ClosureCallAbi {
            arg_offset: target.recv_slots,
            prefix: ClosureArgumentPrefix::ReceiverCaptures(target.recv_slots),
        }
    } else if capture_slots > 0 || target.is_closure {
        ClosureCallAbi {
            arg_offset: 1,
            prefix: ClosureArgumentPrefix::ClosureRef,
        }
    } else {
        ClosureCallAbi {
            arg_offset: 0,
            prefix: ClosureArgumentPrefix::None,
        }
    };
    if abi.arg_offset > target.param_slots {
        return Err(WasmAotError::InvalidModule(format!(
            "closure target {} has arg_offset={} beyond param_slots={}",
            target.name, abi.arg_offset, target.param_slots
        )));
    }
    Ok(abi)
}

pub(super) fn closure_prefix_code(prefix: ClosureArgumentPrefix) -> u32 {
    match prefix {
        ClosureArgumentPrefix::None => 0,
        ClosureArgumentPrefix::ClosureRef => 1,
        ClosureArgumentPrefix::ReceiverCaptures(slots) => u32::from(slots) + 2,
    }
}

pub(super) fn closure_callsite_targets(
    module: &ModuleAnalysis<'_>,
    caller: &FunctionDef,
    pc: usize,
    result_use: ClosureResultUse,
) -> Result<Vec<ClosureCallTarget>, WasmAotError> {
    let (arg_layout, ret_layout) = caller
        .instruction_metadata
        .get(pc)
        .and_then(InstructionMetadata::call_layout_slices)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "{} pc {pc} is missing closure CallLayout metadata",
                caller.name
            ))
        })?;
    let instantiations = closure_instantiations(module);
    let mut candidates = Vec::new();
    for (&target_id, capture_counts) in instantiations {
        let target = module.functions.get(target_id as usize).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "closure instantiation references missing function {target_id}"
            ))
        })?;
        for &capture_slots in capture_counts {
            let abi = closure_call_abi(target, capture_slots)?;
            let user_args =
                &target.slot_types[usize::from(abi.arg_offset)..usize::from(target.param_slots)];
            let returns_match = result_use == ClosureResultUse::Discarded
                || (target.ret_slot_types.as_slice() == ret_layout
                    && target.ret_slots as usize == ret_layout.len());
            if user_args == arg_layout && returns_match {
                candidates.push(ClosureCallTarget {
                    function_id: target_id,
                    capture_slots,
                    abi,
                });
            }
        }
    }
    Ok(candidates)
}

pub(super) fn closure_callsite_candidates(
    module: &ModuleAnalysis<'_>,
    caller: &FunctionDef,
    pc: usize,
    function_indices: &BTreeMap<u32, u32>,
    result_use: ClosureResultUse,
) -> Result<Vec<ClosureCallCandidate>, WasmAotError> {
    Ok(closure_callsite_targets(module, caller, pc, result_use)?
        .into_iter()
        .filter_map(|target| {
            function_indices
                .get(&target.function_id)
                .copied()
                .map(|wasm_index| ClosureCallCandidate { target, wasm_index })
        })
        .collect())
}

/// Return every concrete runtime type that implements an interface, together
/// with its method targets in interface order. Core Wasm dispatch specializes
/// on the concrete ValueRttid carried in the interface value, so it remains
/// complete even when the bytecode module never materialized a particular
/// concrete/interface itab pair at a static assignment site.
pub(super) fn interface_implementations(
    module: &ModuleAnalysis<'_>,
    iface_meta_id: u32,
) -> Result<Vec<(u32, Vec<u32>)>, WasmAotError> {
    if let Some(result) = module.interfaces.borrow().get(&iface_meta_id) {
        return result.clone();
    }
    let result = compute_interface_implementations(module, iface_meta_id);
    module
        .interfaces
        .borrow_mut()
        .insert(iface_meta_id, result.clone());
    result
}

fn compute_interface_implementations(
    module: &ModuleAnalysis<'_>,
    iface_meta_id: u32,
) -> Result<Vec<(u32, Vec<u32>)>, WasmAotError> {
    let target_iface = module
        .interface_metas
        .get(iface_meta_id as usize)
        .ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "interface metadata {iface_meta_id} is outside the module table"
            ))
        })?;
    let mut implementations = BTreeMap::new();
    for rttid in 0..module.runtime_types.len() as u32 {
        let Some(value_rttid) = module.value_rttid_for_rttid(rttid) else {
            continue;
        };
        let Some(named_id) = module.named_type_id_for_rttid(rttid) else {
            continue;
        };
        let Some(named) = module.named_type_metas.get(named_id as usize) else {
            continue;
        };
        let source_is_pointer = value_rttid.value_kind() == ValueKind::Pointer;
        let methods: Option<Vec<u32>> = target_iface
            .methods
            .iter()
            .map(|required| {
                named
                    .methods
                    .get(&required.name)
                    .and_then(|implementation| {
                        (implementation.signature_rttid == required.signature_rttid
                            && (!implementation.is_pointer_receiver || source_is_pointer))
                            .then_some(implementation.func_id)
                    })
            })
            .collect();
        if let Some(methods) = methods {
            implementations.insert(value_rttid.to_raw(), methods);
        }
    }
    Ok(implementations.into_iter().collect())
}

pub(super) fn reachable_functions(
    module: &ModuleAnalysis<'_>,
    resolved_externs: &ResolvedExternTable,
) -> Result<Vec<u32>, WasmAotError> {
    let mut reachable = BTreeSet::from([module.entry_func]);
    let mut pending = vec![module.entry_func];
    for target in module
        .named_type_metas
        .iter()
        .flat_map(|named| named.methods.values().map(|method| method.func_id))
    {
        if target as usize >= module.functions.len() {
            return Err(WasmAotError::InvalidModule(format!(
                "dynamic method metadata references missing function {target}"
            )));
        }
        if reachable.insert(target) {
            pending.push(target);
        }
    }
    while let Some(function_id) = pending.pop() {
        let function = module.functions.get(function_id as usize).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "reachability analysis found missing function {function_id}"
            ))
        })?;
        let mut discovered = Vec::new();
        for (pc, instruction) in function.code.iter().enumerate() {
            match instruction.opcode() {
                Opcode::Call => discovered.push(instruction.static_call_func_id()),
                Opcode::ClosureNew => discovered.push(instruction.closure_new_func_id()),
                Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush
                    if !instruction.call_shape_is_closure() =>
                {
                    discovered.push(instruction.call_shape_static_func_id());
                }
                Opcode::GoStart | Opcode::GoIsland | Opcode::DeferPush | Opcode::ErrDeferPush => {
                    discovered.extend(closure_target_ids(module));
                }
                Opcode::IslandNew => discovered.push(module.island_init_func),
                Opcode::CallIface => {
                    let Some(InstructionMetadata::CallIfaceLayout {
                        iface_meta_id,
                        method_idx,
                        ..
                    }) = function.instruction_metadata.get(pc)
                    else {
                        return Err(WasmAotError::InvalidModule(format!(
                            "function {function_id} pc {pc} is missing CallIfaceLayout metadata"
                        )));
                    };
                    discovered.extend(
                        interface_implementations(module, *iface_meta_id)?
                            .into_iter()
                            .filter_map(|(_, methods)| methods.get(*method_idx as usize).copied()),
                    );
                }
                Opcode::CallClosure => {
                    discovered.extend(closure_target_ids(module));
                }
                Opcode::CallExtern => {
                    let protocol =
                        match core_runtime_extern(resolved_externs, u32::from(instruction.b)) {
                            Some(
                                CoreRuntimeExtern::DynField
                                | CoreRuntimeExtern::DynGetAttr
                                | CoreRuntimeExtern::DynMethod,
                            ) => module.well_known.attr_object_iface_id,
                            Some(CoreRuntimeExtern::DynIndex | CoreRuntimeExtern::DynGetIndex) => {
                                module.well_known.index_object_iface_id
                            }
                            Some(
                                CoreRuntimeExtern::DynSetField | CoreRuntimeExtern::DynSetAttr,
                            ) => module.well_known.set_attr_object_iface_id,
                            Some(
                                CoreRuntimeExtern::DynSetIndex | CoreRuntimeExtern::DynSetIndexApi,
                            ) => module.well_known.set_index_object_iface_id,
                            Some(CoreRuntimeExtern::DynCall) => {
                                module.well_known.call_object_iface_id
                            }
                            _ => None,
                        };
                    if let Some(protocol) = protocol {
                        discovered.extend(
                            interface_implementations(module, protocol)?
                                .into_iter()
                                .filter_map(|(_, methods)| methods.first().copied()),
                        );
                    }
                }
                _ => {}
            }
        }
        for target in discovered {
            if target as usize >= module.functions.len() {
                return Err(WasmAotError::InvalidModule(format!(
                    "function {function_id} references missing function {target}"
                )));
            }
            if reachable.insert(target) {
                pending.push(target);
            }
        }
    }
    Ok(reachable.into_iter().collect())
}

/// Functions reached without crossing a reflective dynamic-dispatch edge.
///
/// The full AOT image still contains every target admitted by closed-world
/// dynamic dispatch. Host externs referenced solely by those speculative
/// targets stay lazy: preflight does not reject an image that never selects
/// them, while an actual call continues to fail closed in the dispatcher when
/// its provider is unavailable.
pub(super) fn statically_reachable_functions(
    module: &ModuleAnalysis<'_>,
) -> Result<BTreeSet<u32>, WasmAotError> {
    let explicit_closures = explicit_closure_target_ids(module);
    let mut reachable = BTreeSet::from([module.entry_func]);
    let mut pending = vec![module.entry_func];
    while let Some(function_id) = pending.pop() {
        let function = module.functions.get(function_id as usize).ok_or_else(|| {
            WasmAotError::InvalidModule(format!(
                "static reachability found missing function {function_id}"
            ))
        })?;
        let mut discovered = Vec::new();
        for (pc, instruction) in function.code.iter().enumerate() {
            match instruction.opcode() {
                Opcode::Call => discovered.push(instruction.static_call_func_id()),
                Opcode::ClosureNew => discovered.push(instruction.closure_new_func_id()),
                Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush
                    if !instruction.call_shape_is_closure() =>
                {
                    discovered.push(instruction.call_shape_static_func_id());
                }
                Opcode::GoStart | Opcode::GoIsland | Opcode::DeferPush | Opcode::ErrDeferPush => {
                    discovered.extend(explicit_closures.iter().copied());
                }
                Opcode::IslandNew => discovered.push(module.island_init_func),
                Opcode::CallIface => {
                    let Some(InstructionMetadata::CallIfaceLayout {
                        iface_meta_id,
                        method_idx,
                        ..
                    }) = function.instruction_metadata.get(pc)
                    else {
                        return Err(WasmAotError::InvalidModule(format!(
                            "function {function_id} pc {pc} is missing CallIfaceLayout metadata"
                        )));
                    };
                    discovered.extend(
                        interface_implementations(module, *iface_meta_id)?
                            .into_iter()
                            .filter_map(|(_, methods)| methods.get(*method_idx as usize).copied()),
                    );
                }
                Opcode::CallClosure => {
                    discovered.extend(
                        closure_callsite_targets(module, function, pc, ClosureResultUse::Consumed)?
                            .into_iter()
                            .map(|target| target.function_id)
                            .filter(|target| explicit_closures.contains(target)),
                    );
                }
                _ => {}
            }
        }
        for target in discovered {
            if target as usize >= module.functions.len() {
                return Err(WasmAotError::InvalidModule(format!(
                    "function {function_id} references missing function {target}"
                )));
            }
            if reachable.insert(target) {
                pending.push(target);
            }
        }
    }
    Ok(reachable)
}

pub(super) fn instruction_calls_materialized(
    module: &ModuleAnalysis<'_>,
    function: &FunctionDef,
    pc: usize,
    instruction: &vo_common_core::instruction::Instruction,
    materialized: &BTreeSet<u32>,
) -> Result<bool, WasmAotError> {
    match instruction.opcode() {
        Opcode::Call => Ok(materialized.contains(&instruction.static_call_func_id())),
        Opcode::CallClosure => {
            Ok(
                closure_callsite_targets(module, function, pc, ClosureResultUse::Consumed)?
                    .into_iter()
                    .any(|target| materialized.contains(&target.function_id)),
            )
        }
        Opcode::CallIface => {
            let Some(InstructionMetadata::CallIfaceLayout {
                iface_meta_id,
                method_idx,
                ..
            }) = function.instruction_metadata.get(pc)
            else {
                return Err(WasmAotError::InvalidModule(format!(
                    "{} pc {pc} is missing CallIfaceLayout metadata",
                    function.name
                )));
            };
            Ok(interface_implementations(module, *iface_meta_id)?
                .into_iter()
                .filter_map(|(_, methods)| methods.get(*method_idx as usize).copied())
                .any(|target| materialized.contains(&target)))
        }
        _ => Ok(false),
    }
}

/// Local lowering costs. Static calls propagate callee effects in the fixed
/// point; StrNew refers to image-owned static data. Scheduler/defer records
/// allocate in this backend, and closure loads can reject a nil environment.
pub(super) fn wasm_local_effects(opcode: Opcode) -> (bool, bool) {
    let effects = vo_common_core::execution_effects::opcode_effect_contract(opcode);
    let allocates = match opcode {
        Opcode::Call | Opcode::CallClosure | Opcode::CallIface | Opcode::StrNew => false,
        Opcode::GoStart | Opcode::GoIsland | Opcode::DeferPush | Opcode::ErrDeferPush => true,
        _ => effects.may_alloc,
    };
    let unwinds = match opcode {
        Opcode::Call | Opcode::CallClosure | Opcode::CallIface => false,
        Opcode::ClosureGet => true,
        _ => effects.may_panic || effects.may_unwind,
    };
    (allocates, unwinds)
}

pub(super) fn extern_may_suspend(resolved_externs: &ResolvedExternTable, extern_id: u32) -> bool {
    const SUSPENDING_EFFECTS: ExternEffects = ExternEffects::MAY_YIELD
        .union(ExternEffects::MAY_QUEUE_BLOCK)
        .union(ExternEffects::MAY_WAIT_IO_REPLAY)
        .union(ExternEffects::MAY_HOST_WAIT)
        .union(ExternEffects::MAY_HOST_REPLAY)
        .union(ExternEffects::MAY_CALL_CLOSURE_REPLAY)
        .union(ExternEffects::UNKNOWN_CONTROL);
    resolved_externs
        .get(extern_id)
        .is_none_or(|resolved| resolved.effective_effects.intersects(SUSPENDING_EFFECTS))
}

pub(super) fn instruction_callees(
    module: &ModuleAnalysis<'_>,
    function: &FunctionDef,
    pc: usize,
    instruction: &vo_common_core::instruction::Instruction,
) -> Result<Vec<u32>, WasmAotError> {
    match instruction.opcode() {
        Opcode::Call => Ok(vec![instruction.static_call_func_id()]),
        Opcode::CallClosure => {
            Ok(
                closure_callsite_targets(module, function, pc, ClosureResultUse::Consumed)?
                    .into_iter()
                    .map(|target| target.function_id)
                    .collect(),
            )
        }
        Opcode::CallIface => {
            let Some(InstructionMetadata::CallIfaceLayout {
                iface_meta_id,
                method_idx,
                ..
            }) = function.instruction_metadata.get(pc)
            else {
                return Err(WasmAotError::InvalidModule(format!(
                    "{} pc {pc} is missing CallIfaceLayout metadata",
                    function.name
                )));
            };
            Ok(interface_implementations(module, *iface_meta_id)?
                .into_iter()
                .filter_map(|(_, methods)| methods.get(*method_idx as usize).copied())
                .collect())
        }
        _ => Ok(Vec::new()),
    }
}

pub(super) fn analyze_function_capabilities(
    module: &ModuleAnalysis<'_>,
    resolved_externs: &ResolvedExternTable,
    reachable: &[u32],
) -> Result<BTreeMap<u32, FunctionCapabilities>, WasmAotError> {
    // A program with concurrent guest work or asynchronous host operations must
    // be able to suspend every unbounded call chain. Pure synchronous images
    // retain their direct numeric ABI and are still constrained by total fuel.
    let cooperative = reachable.iter().any(|id| {
        module.functions[*id as usize].code.iter().any(|i| {
            matches!(
                i.opcode(),
                Opcode::GoStart
                    | Opcode::GoIsland
                    | Opcode::QueueSend
                    | Opcode::QueueRecv
                    | Opcode::SelectExec
            ) || (i.opcode() == Opcode::CallExtern
                && extern_may_suspend(resolved_externs, u32::from(i.b)))
        })
    });
    let recursive = if cooperative {
        recursive_functions(module, reachable)?
    } else {
        BTreeSet::new()
    };
    let mut capabilities = BTreeMap::new();
    for function_id in reachable {
        let function = module.functions.get(*function_id as usize).ok_or_else(|| {
            WasmAotError::InvalidModule(format!("reachable function {function_id} is missing"))
        })?;
        let mut local = FunctionCapabilities {
            may_suspend: (cooperative
                && (recursive.contains(function_id)
                    || function.code.iter().enumerate().any(|(pc, i)| {
                        matches!(
                            i.opcode(),
                            Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot | Opcode::ForLoop
                        ) && branch_target(pc, i) <= pc
                    })))
                || function.has_defer
                || function.code.iter().any(|instruction| {
                    matches!(
                        instruction.opcode(),
                        Opcode::QueueSend
                            | Opcode::QueueRecv
                            | Opcode::SelectExec
                            | Opcode::GoIsland
                    ) || (instruction.opcode() == Opcode::CallExtern
                        && extern_may_suspend(resolved_externs, u32::from(instruction.b)))
                }),
            may_allocate: function
                .code
                .iter()
                .any(|instruction| wasm_local_effects(instruction.opcode()).0),
            may_unwind: function.has_defer
                || function
                    .code
                    .iter()
                    .any(|instruction| wasm_local_effects(instruction.opcode()).1),
            has_host_effect: function.code.iter().enumerate().any(|(pc, instruction)| {
                match instruction.opcode() {
                    Opcode::CallExtern => {
                        extern_requires_host(resolved_externs, function, pc, instruction)
                    }
                    Opcode::QueueSend
                    | Opcode::QueueRecv
                    | Opcode::SelectExec
                    | Opcode::GoStart
                    | Opcode::GoIsland
                    | Opcode::IslandNew => true,
                    _ => false,
                }
            }),
            has_gc_roots: function
                .slot_types
                .iter()
                .any(|slot_type| !matches!(slot_type, SlotType::Value | SlotType::Float)),
            // Keep wide aggregates on the durable memory ABI. The two owner/budget
            // parameters and status result count against the Wasm type limits.
            direct_local_supported: usize::from(function.param_slots) + 2 <= MAX_FAST_ABI_PARAMS
                && usize::from(function.ret_slots) + 1 <= MAX_FAST_ABI_RESULTS
                && is_direct_local_candidate(module, resolved_externs, function),
            observes_call_stack: function.code.iter().any(|instruction| {
                if instruction.opcode() != Opcode::CallExtern {
                    return false;
                }
                resolved_externs
                    .get(u32::from(instruction.b))
                    .and_then(|resolved| {
                        vo_common_core::extern_key::decode_extern_name(&resolved.name).ok()
                    })
                    .is_some_and(|key| key.package() == "runtime" && key.function() == "Caller")
            }),
        };
        local.may_suspend |= local.may_allocate
            || function.code.iter().any(|instruction| {
                instruction.opcode() == Opcode::CallExtern
                    && resolved_externs
                        .get(u32::from(instruction.b))
                        .and_then(|resolved| {
                            vo_common_core::extern_key::decode_extern_name(&resolved.name).ok()
                        })
                        .is_some_and(|key| {
                            key.package() == "runtime/mem"
                                && matches!(key.function(), "GCStep" | "GCCollect")
                        })
            });
        if function.has_defer {
            local.may_allocate = true;
        }
        capabilities.insert(*function_id, local);
    }

    loop {
        let previous = capabilities.clone();
        let mut changed = false;
        for function_id in reachable {
            let function = &module.functions[*function_id as usize];
            let current = capabilities
                .get_mut(function_id)
                .expect("reachable capability initialized above");
            for (pc, instruction) in function.code.iter().enumerate() {
                for target in instruction_callees(module, function, pc, instruction)? {
                    let callee = previous.get(&target).copied().ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} calls function {target} outside the reachable image",
                            function.name
                        ))
                    })?;
                    changed |= current.merge_callee(callee);
                }
            }
        }
        if !changed {
            return Ok(capabilities);
        }
    }
}

pub(super) fn recursive_functions(
    module: &ModuleAnalysis<'_>,
    reachable: &[u32],
) -> Result<BTreeSet<u32>, WasmAotError> {
    let mut graph = BTreeMap::<u32, BTreeSet<u32>>::new();
    for function_id in reachable {
        let function = &module.functions[*function_id as usize];
        let targets = graph.entry(*function_id).or_default();
        for (pc, instruction) in function.code.iter().enumerate() {
            targets.extend(instruction_callees(module, function, pc, instruction)?);
        }
    }
    let mut recursive = BTreeSet::new();
    for function_id in reachable {
        let mut pending: Vec<u32> = graph
            .get(function_id)
            .into_iter()
            .flatten()
            .copied()
            .collect();
        let mut visited = BTreeSet::new();
        while let Some(current) = pending.pop() {
            if current == *function_id {
                recursive.insert(*function_id);
                break;
            }
            if visited.insert(current) {
                pending.extend(graph.get(&current).into_iter().flatten().copied());
            }
        }
    }
    Ok(recursive)
}

pub(super) fn retry_safe_scalar_recursive_functions(
    module: &ModuleAnalysis<'_>,
    resolved_externs: &ResolvedExternTable,
    reachable: &[u32],
    capabilities: &BTreeMap<u32, FunctionCapabilities>,
) -> Result<BTreeSet<u32>, WasmAotError> {
    fn instruction_is_retry_safe(
        module: &ModuleAnalysis<'_>,
        resolved_externs: &ResolvedExternTable,
        function: &FunctionDef,
        pc: usize,
        instruction: &vo_common_core::instruction::Instruction,
    ) -> bool {
        match instruction.opcode() {
            Opcode::LoadConst => matches!(
                module.constants.get(instruction.b as usize),
                Some(Constant::Nil | Constant::Bool(_) | Constant::Int(_) | Constant::Float(_))
            ),
            Opcode::CallExtern => {
                direct_intrinsic(resolved_externs, function, pc, instruction).is_some()
            }
            Opcode::Hint
            | Opcode::LoadInt
            | Opcode::Copy
            | Opcode::PtrGet
            | Opcode::PtrGetN
            | Opcode::PtrAdd
            | Opcode::ArrayGet
            | Opcode::SliceGet
            | Opcode::ArrayAddr
            | Opcode::SliceAddr
            | Opcode::SliceLen
            | Opcode::SliceCap
            | Opcode::ClosureGet
            | Opcode::AddI
            | Opcode::SubI
            | Opcode::MulI
            | Opcode::DivI
            | Opcode::DivU
            | Opcode::ModI
            | Opcode::ModU
            | Opcode::And
            | Opcode::Or
            | Opcode::Xor
            | Opcode::AndNot
            | Opcode::Shl
            | Opcode::ShrS
            | Opcode::ShrU
            | Opcode::NegI
            | Opcode::Not
            | Opcode::BoolNot
            | Opcode::EqI
            | Opcode::NeI
            | Opcode::LtI
            | Opcode::LeI
            | Opcode::GtI
            | Opcode::GeI
            | Opcode::LtU
            | Opcode::LeU
            | Opcode::GtU
            | Opcode::GeU
            | Opcode::AddF
            | Opcode::AddF32
            | Opcode::SubF
            | Opcode::SubF32
            | Opcode::MulF
            | Opcode::MulF32
            | Opcode::DivF
            | Opcode::DivF32
            | Opcode::NegF
            | Opcode::NegF32
            | Opcode::EqF
            | Opcode::EqF32
            | Opcode::NeF
            | Opcode::NeF32
            | Opcode::LtF
            | Opcode::LtF32
            | Opcode::LeF
            | Opcode::LeF32
            | Opcode::GtF
            | Opcode::GtF32
            | Opcode::GeF
            | Opcode::GeF32
            | Opcode::ConvI2F
            | Opcode::ConvF2I
            | Opcode::ConvF64F32
            | Opcode::ConvF32F64
            | Opcode::Trunc
            | Opcode::IndexCheck
            | Opcode::Jump
            | Opcode::JumpIf
            | Opcode::JumpIfNot
            | Opcode::ForLoop
            | Opcode::Call
            | Opcode::Return => true,
            _ => false,
        }
    }

    let recursive = recursive_functions(module, reachable)?;
    let mut pure: BTreeSet<u32> = reachable
        .iter()
        .copied()
        .filter(|function_id| {
            capabilities
                .get(function_id)
                .is_some_and(|capability| capability.typed_fast_abi())
                && module.functions[*function_id as usize]
                    .code
                    .iter()
                    .enumerate()
                    .all(|(pc, instruction)| {
                        instruction_is_retry_safe(
                            module,
                            resolved_externs,
                            &module.functions[*function_id as usize],
                            pc,
                            instruction,
                        )
                    })
        })
        .collect();
    loop {
        let rejected: Vec<u32> = pure
            .iter()
            .copied()
            .filter(|function_id| {
                module.functions[*function_id as usize]
                    .code
                    .iter()
                    .filter(|instruction| instruction.opcode() == Opcode::Call)
                    .any(|instruction| !pure.contains(&instruction.static_call_func_id()))
            })
            .collect();
        if rejected.is_empty() {
            break;
        }
        for function_id in rejected {
            pure.remove(&function_id);
        }
    }
    Ok(recursive.intersection(&pure).copied().collect())
}

pub(super) fn materialized_functions(
    module: &ModuleAnalysis<'_>,
    reachable: &[u32],
    capabilities: &BTreeMap<u32, FunctionCapabilities>,
) -> Result<BTreeSet<u32>, WasmAotError> {
    // The root entry is the one function whose frame is owned for the whole
    // scheduler lifetime. Other dispatcher targets may use the direct ABI
    // when their instruction set proves that they cannot suspend.
    // Closed functions can share a pre-sized slot span because they have no
    // safe point; an owning-frame parameter preserves precise panic unwinding.
    // Deferred callees retain their dispatcher-owned frame identity so
    // recover can prove that it is executing in the directly invoked defer.
    // Allocating functions retain durable state for bounded GC progress.
    // Recursive SCCs retain a scheduler entry for Wasm stack limits; separately
    // proven retry-safe scalar recursion may also receive a direct adapter.
    let recursive = recursive_functions(module, reachable)?;
    let mut deferred = BTreeSet::new();
    let mut fiber_entries = BTreeSet::new();
    if reachable.contains(&module.island_init_func) {
        fiber_entries.insert(module.island_init_func);
    }
    for function_id in reachable {
        let function = &module.functions[*function_id as usize];
        for (pc, instruction) in function.code.iter().enumerate() {
            match instruction.opcode() {
                Opcode::DeferPush | Opcode::ErrDeferPush => {
                    if instruction.call_shape_is_closure() {
                        deferred.extend(
                            closure_callsite_targets(
                                module,
                                function,
                                pc,
                                ClosureResultUse::Discarded,
                            )?
                            .into_iter()
                            .map(|target| target.function_id),
                        );
                    } else {
                        deferred.insert(instruction.call_shape_static_func_id());
                    }
                }
                Opcode::GoStart if !instruction.call_shape_is_closure() => {
                    fiber_entries.insert(instruction.call_shape_static_func_id());
                }
                Opcode::GoStart | Opcode::GoIsland => {
                    fiber_entries.extend(
                        closure_callsite_targets(
                            module,
                            function,
                            pc,
                            ClosureResultUse::Discarded,
                        )?
                        .into_iter()
                        .map(|target| target.function_id),
                    );
                }
                _ => {}
            }
        }
    }
    let mut materialized: BTreeSet<u32> = reachable
        .iter()
        .copied()
        .filter(|function_id| {
            *function_id == module.entry_func
                || recursive.contains(function_id)
                || deferred.contains(function_id)
                // runtime.Caller makes every transitively active logical Vo
                // frame observable. Keep that subgraph on scheduler-owned
                // frames so fast/rooted ABI choices and inlining cannot erase
                // caller identities or source locations.
                || capabilities
                    .get(function_id)
                    .is_some_and(|capabilities| capabilities.observes_call_stack || capabilities.may_allocate)
                || (fiber_entries.contains(function_id)
                    && capabilities
                        .get(function_id)
                        .is_none_or(|capabilities| capabilities.may_unwind))
                || module.functions[*function_id as usize].code.is_empty()
                || capabilities.get(function_id).is_none_or(|capabilities| {
                    !capabilities.typed_fast_abi()
                })
        })
        .collect();

    loop {
        let mut changed = false;
        for function_id in reachable {
            if materialized.contains(function_id) {
                continue;
            }
            let function = &module.functions[*function_id as usize];
            let calls_materialized =
                function
                    .code
                    .iter()
                    .enumerate()
                    .try_fold(false, |found, (pc, instruction)| {
                        if found {
                            Ok(true)
                        } else {
                            instruction_calls_materialized(
                                module,
                                function,
                                pc,
                                instruction,
                                &materialized,
                            )
                        }
                    })?;
            if calls_materialized {
                changed |= materialized.insert(*function_id);
            }
        }
        if !changed {
            return Ok(materialized);
        }
    }
}

pub(super) fn required_shared_frame_slots(
    module: &ModuleAnalysis<'_>,
    function_id: u32,
    materialized: &BTreeSet<u32>,
) -> Result<u32, WasmAotError> {
    fn direct_scratch_slots(
        module: &ModuleAnalysis<'_>,
        function_id: u32,
        materialized: &BTreeSet<u32>,
        visiting: &mut BTreeSet<u32>,
        cache: &mut BTreeMap<u32, u32>,
    ) -> Result<u32, WasmAotError> {
        if let Some(slots) = cache.get(&function_id) {
            return Ok(*slots);
        }
        if !visiting.insert(function_id) {
            return Ok(0);
        }
        let function = module.functions.get(function_id as usize).ok_or_else(|| {
            WasmAotError::InvalidModule(format!("function {function_id} is missing"))
        })?;
        let mut required = u32::from(function.local_slots);
        for instruction in &function.code {
            if instruction.opcode() != Opcode::Call {
                continue;
            }
            let target = instruction.static_call_func_id();
            if materialized.contains(&target) {
                continue;
            }
            required = required.max(direct_scratch_slots(
                module,
                target,
                materialized,
                visiting,
                cache,
            )?);
        }
        visiting.remove(&function_id);
        cache.insert(function_id, required);
        Ok(required)
    }

    let function = module
        .functions
        .get(function_id as usize)
        .ok_or_else(|| WasmAotError::InvalidModule(format!("function {function_id} is missing")))?;
    let mut required = u32::from(function.local_slots);
    let mut scratch_cache = BTreeMap::new();
    for (pc, instruction) in function.code.iter().enumerate() {
        let mut callees = Vec::new();
        match instruction.opcode() {
            Opcode::Call => {
                let target = instruction.static_call_func_id();
                if !materialized.contains(&target) {
                    callees.push((target, u32::from(instruction.b)));
                }
            }
            Opcode::CallClosure => {
                for target in
                    closure_callsite_targets(module, function, pc, ClosureResultUse::Consumed)?
                {
                    if materialized.contains(&target.function_id) {
                        continue;
                    }
                    let base = instruction
                        .b
                        .checked_sub(target.abi.arg_offset)
                        .ok_or_else(|| {
                            WasmAotError::InvalidModule(format!(
                                "{} pc {pc} closure argument prefix {} underflows its call frame",
                                function.name, target.abi.arg_offset
                            ))
                        })?;
                    callees.push((target.function_id, u32::from(base)));
                }
            }
            Opcode::CallIface => {
                let Some(InstructionMetadata::CallIfaceLayout {
                    iface_meta_id,
                    method_idx,
                    ..
                }) = function.instruction_metadata.get(pc)
                else {
                    return Err(WasmAotError::InvalidModule(format!(
                        "function {function_id} pc {pc} is missing CallIfaceLayout metadata"
                    )));
                };
                for target in interface_implementations(module, *iface_meta_id)?
                    .into_iter()
                    .filter_map(|(_, methods)| methods.get(*method_idx as usize).copied())
                {
                    if materialized.contains(&target) {
                        continue;
                    }
                    let receiver_slots = module.functions[target as usize].recv_slots;
                    let base = instruction.b.checked_sub(receiver_slots).ok_or_else(|| {
                        WasmAotError::InvalidModule(format!(
                            "{} pc {pc} interface receiver underflows the call frame",
                            function.name
                        ))
                    })?;
                    callees.push((target, u32::from(base)));
                }
            }
            _ => {}
        }
        for (target, base) in callees {
            let child = direct_scratch_slots(
                module,
                target,
                materialized,
                &mut BTreeSet::new(),
                &mut scratch_cache,
            )?;
            required = required.max(base.checked_add(child).ok_or_else(|| {
                WasmAotError::InvalidModule(format!(
                    "{} pc {pc} call-frame span overflows wasm32",
                    function.name
                ))
            })?);
        }
    }
    Ok(required)
}

pub(super) fn branch_target(
    pc: usize,
    instruction: &vo_common_core::instruction::Instruction,
) -> usize {
    match instruction.opcode() {
        Opcode::ForLoop => instruction.forloop_target(pc),
        _ => (pc as i64 + instruction.imm32() as i64) as usize,
    }
}

pub(super) fn basic_blocks(
    function: &FunctionDef,
) -> Result<(Vec<BasicBlock>, BTreeMap<usize, u32>), WasmAotError> {
    if function.code.is_empty() {
        return Err(WasmAotError::InvalidModule(format!(
            "function {} has an empty instruction stream",
            function.name
        )));
    }
    let mut leaders = BTreeSet::from([0usize]);
    for (pc, instruction) in function.code.iter().enumerate() {
        match instruction.opcode() {
            Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot | Opcode::ForLoop => {
                leaders.insert(branch_target(pc, instruction));
                if !matches!(instruction.opcode(), Opcode::Jump) && pc + 1 < function.code.len() {
                    leaders.insert(pc + 1);
                }
            }
            Opcode::Return | Opcode::Panic => {
                if pc + 1 < function.code.len() {
                    leaders.insert(pc + 1);
                }
            }
            Opcode::Call
            | Opcode::CallExtern
            | Opcode::CallClosure
            | Opcode::CallIface
            | Opcode::QueueSend
            | Opcode::QueueRecv
            | Opcode::SelectExec
            | Opcode::GoIsland => {
                leaders.insert(pc);
                if pc + 1 < function.code.len() {
                    leaders.insert(pc + 1);
                }
            }
            _ => {}
        }
    }
    if leaders.iter().any(|leader| *leader >= function.code.len()) {
        return Err(WasmAotError::InvalidModule(format!(
            "function {} contains an out-of-range branch target",
            function.name
        )));
    }
    let starts: Vec<_> = leaders.into_iter().collect();
    let mut blocks = Vec::with_capacity(starts.len());
    let mut by_pc = BTreeMap::new();
    for (index, start) in starts.iter().copied().enumerate() {
        let end = starts
            .get(index + 1)
            .copied()
            .unwrap_or(function.code.len());
        by_pc.insert(start, index as u32);
        blocks.push(BasicBlock { start, end });
    }
    Ok((blocks, by_pc))
}

pub(super) fn is_direct_local_candidate(
    module: &ModuleAnalysis<'_>,
    resolved_externs: &ResolvedExternTable,
    function: &FunctionDef,
) -> bool {
    if function.has_defer || function.code.is_empty() {
        return false;
    }
    function
        .code
        .iter()
        .enumerate()
        .all(|(pc, instruction)| match instruction.opcode() {
            Opcode::LoadConst => matches!(
                module.constants.get(instruction.b as usize),
                Some(Constant::Nil | Constant::Bool(_) | Constant::Int(_) | Constant::Float(_))
            ),
            Opcode::CopyN => super::direct::aggregate::copy_slots(instruction).is_some(),
            Opcode::SlotGet | Opcode::SlotGetN | Opcode::SlotSet | Opcode::SlotSetN => {
                super::direct::aggregate::projection_shape(function, pc).is_some()
            }
            Opcode::CallExtern => {
                direct_intrinsic(resolved_externs, function, pc, instruction).is_some()
            }
            Opcode::ArrayGet | Opcode::ArraySet | Opcode::SliceGet | Opcode::SliceSet => function
                .instruction_metadata
                .get(pc)
                .and_then(InstructionMetadata::elem_layout)
                .is_some_and(|layout| {
                    matches!(layout.bytes, 1 | 2 | 4 | 8) || layout.bytes % 8 == 0
                }),
            // Literal descriptors and bytes already belong to immutable image
            // data. This lowering does not allocate or introduce a safe point.
            Opcode::StrNew
            | Opcode::Hint
            | Opcode::LoadInt
            | Opcode::Copy
            | Opcode::AddI
            | Opcode::SubI
            | Opcode::MulI
            | Opcode::DivI
            | Opcode::DivU
            | Opcode::ModI
            | Opcode::ModU
            | Opcode::And
            | Opcode::Or
            | Opcode::Xor
            | Opcode::AndNot
            | Opcode::Shl
            | Opcode::ShrS
            | Opcode::ShrU
            | Opcode::NegI
            | Opcode::Not
            | Opcode::BoolNot
            | Opcode::EqI
            | Opcode::NeI
            | Opcode::LtI
            | Opcode::LeI
            | Opcode::GtI
            | Opcode::GeI
            | Opcode::LtU
            | Opcode::LeU
            | Opcode::GtU
            | Opcode::GeU
            | Opcode::AddF
            | Opcode::AddF32
            | Opcode::SubF
            | Opcode::SubF32
            | Opcode::MulF
            | Opcode::MulF32
            | Opcode::DivF
            | Opcode::DivF32
            | Opcode::NegF
            | Opcode::NegF32
            | Opcode::EqF
            | Opcode::EqF32
            | Opcode::NeF
            | Opcode::NeF32
            | Opcode::LtF
            | Opcode::LtF32
            | Opcode::LeF
            | Opcode::LeF32
            | Opcode::GtF
            | Opcode::GtF32
            | Opcode::GeF
            | Opcode::GeF32
            | Opcode::PtrGet
            | Opcode::PtrSet
            | Opcode::PtrGetN
            | Opcode::PtrSetN
            | Opcode::PtrAdd
            | Opcode::ArrayAddr
            | Opcode::SliceAddr
            | Opcode::SliceLen
            | Opcode::SliceCap
            | Opcode::ClosureGet
            | Opcode::ConvI2F
            | Opcode::ConvF2I
            | Opcode::ConvF64F32
            | Opcode::ConvF32F64
            | Opcode::Trunc
            | Opcode::IndexCheck
            | Opcode::Jump
            | Opcode::JumpIf
            | Opcode::JumpIfNot
            | Opcode::ForLoop
            | Opcode::Call
            | Opcode::Return => true,
            _ => false,
        })
}

pub(super) fn inline_instruction_cost(
    module: &ModuleAnalysis<'_>,
    instruction: &vo_common_core::instruction::Instruction,
) -> Option<u32> {
    match instruction.opcode() {
        Opcode::Hint | Opcode::LoadInt | Opcode::Copy => Some(1),
        Opcode::LoadConst
            if matches!(
                module.constants.get(instruction.b as usize),
                Some(Constant::Nil | Constant::Bool(_) | Constant::Int(_) | Constant::Float(_))
            ) =>
        {
            Some(1)
        }
        Opcode::AddI
        | Opcode::SubI
        | Opcode::MulI
        | Opcode::And
        | Opcode::Or
        | Opcode::Xor
        | Opcode::AndNot
        | Opcode::NegI
        | Opcode::Not
        | Opcode::BoolNot
        | Opcode::EqI
        | Opcode::NeI
        | Opcode::LtI
        | Opcode::LeI
        | Opcode::GtI
        | Opcode::GeI
        | Opcode::LtU
        | Opcode::LeU
        | Opcode::GtU
        | Opcode::GeU
        | Opcode::AddF
        | Opcode::AddF32
        | Opcode::SubF
        | Opcode::SubF32
        | Opcode::MulF
        | Opcode::MulF32
        | Opcode::NegF
        | Opcode::NegF32
        | Opcode::EqF
        | Opcode::EqF32
        | Opcode::NeF
        | Opcode::NeF32
        | Opcode::LtF
        | Opcode::LtF32
        | Opcode::LeF
        | Opcode::LeF32
        | Opcode::GtF
        | Opcode::GtF32
        | Opcode::GeF
        | Opcode::GeF32 => Some(1),
        Opcode::DivF
        | Opcode::DivF32
        | Opcode::ConvI2F
        | Opcode::ConvF2I
        | Opcode::ConvF64F32
        | Opcode::ConvF32F64
        | Opcode::Trunc => Some(2),
        _ => None,
    }
}

pub(super) fn inline_candidate_cost(
    module: &ModuleAnalysis<'_>,
    function: &FunctionDef,
) -> Option<u32> {
    const MAX_INLINE_INSTRUCTIONS: usize = 12;
    const MAX_INLINE_SLOTS: u16 = 16;
    if function.local_slots > MAX_INLINE_SLOTS
        || function.code.is_empty()
        || function.code.len() > MAX_INLINE_INSTRUCTIONS + 1
    {
        return None;
    }
    let (return_instruction, body) = function.code.split_last()?;
    if return_instruction.opcode() != Opcode::Return
        || return_instruction.b != function.ret_slots
        || body
            .iter()
            .any(|instruction| instruction.opcode() == Opcode::Return)
    {
        return None;
    }
    body.iter().try_fold(0u32, |cost, instruction| {
        cost.checked_add(inline_instruction_cost(module, instruction)?)
    })
}

pub(super) fn plan_typed_inlining(
    module: &ModuleAnalysis<'_>,
    function: &FunctionDef,
    fast_functions: &BTreeMap<u32, FastAbiFunction>,
    first_extra_local: u32,
) -> FunctionInlinePlan {
    const MAX_INLINE_COST_PER_CALLER: u32 = 64;
    let mut plan = FunctionInlinePlan::default();
    let mut total_cost = 0u32;
    for (pc, instruction) in function.code.iter().enumerate() {
        if instruction.opcode() != Opcode::Call {
            continue;
        }
        let target = instruction.static_call_func_id();
        if !fast_functions.contains_key(&target) {
            continue;
        }
        let Some(callee) = module.functions.get(target as usize) else {
            continue;
        };
        let Some(cost) = inline_candidate_cost(module, callee) else {
            continue;
        };
        let Some(next_cost) = total_cost.checked_add(cost) else {
            continue;
        };
        if next_cost > MAX_INLINE_COST_PER_CALLER {
            continue;
        }
        let first_local = first_extra_local + plan.extra_locals;
        plan.calls.insert(
            pc,
            InlineCallPlan {
                callee: target,
                first_local,
            },
        );
        plan.extra_locals += u32::from(callee.local_slots);
        total_cost = next_cost;
    }
    plan
}

pub(super) fn direct_function_may_panic(
    module: &ModuleAnalysis<'_>,
    function_id: u32,
    materialized: &BTreeSet<u32>,
    visiting: &mut BTreeSet<u32>,
) -> bool {
    if materialized.contains(&function_id) || !visiting.insert(function_id) {
        return false;
    }
    let Some(function) = module.functions.get(function_id as usize) else {
        return true;
    };
    let result = function
        .code
        .iter()
        .any(|instruction| match instruction.opcode() {
            Opcode::PtrGet
            | Opcode::PtrSet
            | Opcode::PtrGetN
            | Opcode::PtrSetN
            | Opcode::ArrayGet
            | Opcode::ArraySet
            | Opcode::ArrayAddr
            | Opcode::SliceGet
            | Opcode::SliceSet
            | Opcode::SliceAddr
            | Opcode::ClosureGet
            | Opcode::DivI
            | Opcode::DivU
            | Opcode::ModI
            | Opcode::ModU
            | Opcode::Shl
            | Opcode::ShrS
            | Opcode::ShrU
            | Opcode::IndexCheck
            | Opcode::CallExtern => true,
            Opcode::Call => direct_function_may_panic(
                module,
                instruction.static_call_func_id(),
                materialized,
                visiting,
            ),
            _ => false,
        });
    visiting.remove(&function_id);
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn local_effects_retain_common_safety_and_document_backend_refinements() {
        // Interface boxing needs GC visibility; map lookup can panic on an
        // unhashable dynamic key even though a plain scalar lookup cannot.
        assert!(wasm_local_effects(Opcode::IfaceAssign).0);
        assert!(wasm_local_effects(Opcode::MapGet).1);
        assert!(wasm_local_effects(Opcode::DivI).1);
        assert!(wasm_local_effects(Opcode::ClosureGet).1);
        // Image-owned strings need no runtime allocation. Call effects are
        // propagated from resolved callees, and deferred frames allocate here.
        assert!(!wasm_local_effects(Opcode::StrNew).0);
        assert_eq!(wasm_local_effects(Opcode::Call), (false, false));
        assert!(wasm_local_effects(Opcode::DeferPush).0);
    }
}
