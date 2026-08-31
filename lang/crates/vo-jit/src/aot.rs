//! Native ahead-of-time object generation built on the shared JIT lowering.

use std::str::FromStr;
use std::sync::Arc;

use cranelift_codegen::ir::{types, AbiParam, InstBuilder, UserFuncName};
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataDescription, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use target_lexicon::Triple;
use vo_runtime::bytecode::{LoadedModule, ResolvedExternTable};
use vo_runtime::jit_api::{JitNativeFrame, JitTier};

use crate::analysis::FunctionAnalysis;
use crate::call_graph::ModuleCallGraph;
use crate::func_compiler::FunctionCompiler;
use crate::helpers::{self, HelperRefs};
use crate::native_stack_map::JitArtifactMetadata;
use crate::optimizer::{ModuleInlinePlan, ModuleOptimizationPlan, OptimizedFunction};
use crate::{
    abi, encode_native_aot_metadata, function_needs_native_root_frame, JitBackendCaps,
    JitCompileEnv, JitError, JitFrameEntryEligibility, MAX_JIT_ANALYSIS_BYTES,
    MAX_JIT_COMPILE_WORK_BYTES, MAX_JIT_NATIVE_FRAME_BYTES,
};

pub const NATIVE_AOT_MODULE_BYTES_SYMBOL: &str = "vo_aot_module_bytes";
pub const NATIVE_AOT_MODULE_LEN_SYMBOL: &str = "vo_aot_module_len";
pub const NATIVE_AOT_METADATA_BYTES_SYMBOL: &str = "vo_aot_metadata_bytes";
pub const NATIVE_AOT_METADATA_LEN_SYMBOL: &str = "vo_aot_metadata_len";
pub const NATIVE_AOT_FUNCTION_TABLE_SYMBOL: &str = "vo_aot_function_table";
pub const NATIVE_AOT_FUNCTION_COUNT_SYMBOL: &str = "vo_aot_function_count";
pub const NATIVE_AOT_START_SYMBOL: &str = "vo_aot_start";

/// Immutable options that participate in the native AOT build identity.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeAotOptions {
    pub target_triple: String,
    pub debug_ir: bool,
}

impl NativeAotOptions {
    pub fn new(target_triple: impl Into<String>) -> Self {
        Self {
            target_triple: target_triple.into(),
            debug_ir: false,
        }
    }
}

/// Runtime metadata and stable linker symbol for one compiled Vo function.
#[derive(Debug, Clone)]
pub struct NativeAotFunction {
    pub func_id: u32,
    pub symbol: String,
    pub metadata: Arc<JitArtifactMetadata>,
    pub entry_eligibility: JitFrameEntryEligibility,
}

/// Relocatable native object plus the exact metadata needed to publish it.
#[derive(Debug, Clone)]
pub struct NativeAotObject {
    pub target_triple: String,
    pub bytes: Vec<u8>,
    pub functions: Vec<NativeAotFunction>,
}

fn build_isa(target: &str) -> Result<cranelift_codegen::isa::OwnedTargetIsa, JitError> {
    let triple = Triple::from_str(target)
        .map_err(|error| JitError::Internal(format!("invalid AOT target {target}: {error}")))?;
    if triple.pointer_width().map(|width| width.bits()) != Ok(64) {
        return Err(JitError::Internal(format!(
            "native AOT target {target} must use the supported 64-bit runtime ABI"
        )));
    }
    if triple.endianness() != Ok(target_lexicon::Endianness::Little) {
        return Err(JitError::Internal(format!(
            "native AOT target {target} must use the supported little-endian runtime ABI"
        )));
    }

    let mut flags = settings::builder();
    flags
        .set("opt_level", "speed")
        .map_err(|error| JitError::Internal(error.to_string()))?;
    flags
        .set("is_pic", "true")
        .map_err(|error| JitError::Internal(error.to_string()))?;
    flags
        .set("enable_verifier", "true")
        .map_err(|error| JitError::Internal(error.to_string()))?;
    super::configure_required_stack_probes(
        &mut flags,
        triple.operating_system == target_lexicon::OperatingSystem::Windows,
    )?;

    let builder = cranelift_codegen::isa::lookup(triple).map_err(|error| {
        JitError::Internal(format!(
            "Cranelift does not support AOT target {target}: {error:?}"
        ))
    })?;
    builder
        .finish(settings::Flags::new(flags))
        .map_err(|error| JitError::Internal(format!("invalid AOT ISA for {target}: {error}")))
}

fn verify_compile_work_budget(module: &LoadedModule) -> Result<(), JitError> {
    let requested_bytes = module.functions.iter().fold(0usize, |total, function| {
        total
            .saturating_add(function.code.len().saturating_mul(512))
            .saturating_add(function.instruction_metadata.len().saturating_mul(32))
            .saturating_add(usize::from(function.local_slots).saturating_mul(64))
    });
    if requested_bytes > MAX_JIT_COMPILE_WORK_BYTES {
        return Err(JitError::CompileWorkLimitExceeded {
            limit_bytes: MAX_JIT_COMPILE_WORK_BYTES,
            requested_bytes,
        });
    }
    Ok(())
}

fn verify_native_frame_budget(context: &cranelift_codegen::Context) -> Result<(), JitError> {
    let requested_bytes = context
        .func
        .sized_stack_slots
        .values()
        .try_fold(0usize, |total, slot| total.checked_add(slot.size as usize))
        .unwrap_or(usize::MAX);
    if requested_bytes > MAX_JIT_NATIVE_FRAME_BYTES {
        return Err(JitError::NativeFrameLimitExceeded {
            limit_bytes: MAX_JIT_NATIVE_FRAME_BYTES,
            requested_bytes,
        });
    }
    Ok(())
}

fn compiled_metadata(
    context: &cranelift_codegen::Context,
    function_name: &str,
    deopt_states: Vec<crate::DeoptFrameState>,
) -> Result<JitArtifactMetadata, JitError> {
    let compiled = context.compiled_code().ok_or_else(|| {
        JitError::Internal(format!("missing compiled AOT code for {function_name}"))
    })?;
    let code_size = compiled.code_info().total_size as usize;
    let stack_maps = compiled.buffer.user_stack_maps();
    let source_locs = compiled.buffer.get_srclocs_sorted();
    let mut source_index = 0usize;
    let mut native_stack_maps = Vec::with_capacity(stack_maps.len());
    for (return_address, frame_size, map) in stack_maps {
        while source_locs
            .get(source_index)
            .is_some_and(|source| source.end < *return_address)
        {
            source_index += 1;
        }
        let source = source_locs
            .get(source_index)
            .filter(|source| source.start < *return_address && *return_address <= source.end)
            .ok_or_else(|| {
                JitError::Internal(format!(
                    "native AOT stack map for {function_name} has no safepoint source location"
                ))
            })?;
        let safepoint_id = source.loc.bits().checked_sub(1).ok_or_else(|| {
            JitError::Internal(format!(
                "native AOT stack map for {function_name} has an invalid source location"
            ))
        })?;
        native_stack_maps.push((
            safepoint_id,
            *return_address,
            *frame_size,
            map.entries().collect::<Vec<_>>(),
        ));
    }

    JitArtifactMetadata::from_entries(code_size, native_stack_maps, function_name)?
        .with_deopt_states(deopt_states, function_name)
}

fn define_exported_bytes(
    module: &mut ObjectModule,
    symbol: &str,
    bytes: Vec<u8>,
) -> Result<(), JitError> {
    let id = module.declare_data(symbol, Linkage::Export, false, false)?;
    let mut description = DataDescription::new();
    description.define(bytes.into_boxed_slice());
    description.set_align(8);
    description.set_used(true);
    module.define_data(id, &description)?;
    Ok(())
}

fn define_exported_u64(
    module: &mut ObjectModule,
    symbol: &str,
    value: u64,
) -> Result<(), JitError> {
    define_exported_bytes(module, symbol, value.to_le_bytes().to_vec())
}

fn define_function_table(
    module: &mut ObjectModule,
    functions: &[cranelift_module::FuncId],
) -> Result<(), JitError> {
    let byte_len = functions
        .len()
        .checked_mul(8)
        .ok_or_else(|| JitError::Internal("AOT function table size overflow".to_string()))?;
    let id = module.declare_data(
        NATIVE_AOT_FUNCTION_TABLE_SYMBOL,
        Linkage::Export,
        false,
        false,
    )?;
    let mut description = DataDescription::new();
    // A function-address table carries linker relocations, so it must have
    // file-backed storage. Placing relocations in a zero-fill/BSS section is
    // rejected by ELF/COFF linkers and can crash some ld64 releases.
    description.define(vec![0; byte_len].into_boxed_slice());
    description.set_align(8);
    description.set_used(true);
    for (index, function) in functions.iter().copied().enumerate() {
        let offset =
            u32::try_from(index.checked_mul(8).ok_or_else(|| {
                JitError::Internal("AOT function table offset overflow".to_string())
            })?)
            .map_err(|_| JitError::Internal("AOT function table exceeds u32".to_string()))?;
        let reference = module.declare_func_in_data(function, &mut description);
        description.write_function_addr(offset, reference);
    }
    module.define_data(id, &description)?;
    define_exported_u64(
        module,
        NATIVE_AOT_FUNCTION_COUNT_SYMBOL,
        functions.len() as u64,
    )
}

fn define_main(module: &mut ObjectModule) -> Result<(), JitError> {
    let pointer_type = module.target_config().pointer_type();
    let call_conv = module.target_config().default_call_conv;
    let mut signature = cranelift_codegen::ir::Signature::new(call_conv);
    signature.params.push(AbiParam::new(types::I32));
    signature.params.push(AbiParam::new(pointer_type));
    signature.returns.push(AbiParam::new(types::I32));
    let start = module.declare_function(NATIVE_AOT_START_SYMBOL, Linkage::Import, &signature)?;
    let main = module.declare_function("main", Linkage::Export, &signature)?;

    let mut context = module.make_context();
    context.func.signature = signature;
    context.func.name = UserFuncName::user(1, 0);
    let start_ref = module.declare_func_in_func(start, &mut context.func);
    let mut frontend = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut context.func, &mut frontend);
    let entry = builder.create_block();
    builder.append_block_params_for_function_params(entry);
    builder.switch_to_block(entry);
    builder.seal_block(entry);
    let params = builder.block_params(entry).to_vec();
    let call = builder.ins().call(start_ref, &params);
    let result = builder.inst_results(call)[0];
    builder.ins().return_(&[result]);
    builder.finalize(module.target_config());
    module.define_function(main, &mut context)?;
    Ok(())
}

/// Compile every verified Vo function into one deterministic relocatable
/// object. Runtime helper calls remain ordinary linker imports, while all Vo
/// function entries are exported under stable `vo_aot_fn_<id>` symbols.
pub fn compile_native_object(
    loaded: Arc<LoadedModule>,
    externs: &ResolvedExternTable,
    options: &NativeAotOptions,
) -> Result<NativeAotObject, JitError> {
    verify_compile_work_budget(&loaded)?;
    let isa = build_isa(&options.target_triple)?;
    let mut object_builder = ObjectBuilder::new(
        isa,
        b"volang-aot".to_vec(),
        cranelift_module::default_libcall_names(),
    )?;
    object_builder.per_function_section(true);
    object_builder.per_data_object_section(true);
    let mut object_module = ObjectModule::new(object_builder);
    let target_config = object_module.target_config();
    let pointer_type = target_config.pointer_type();
    let helper_funcs = helpers::declare_helpers(&mut object_module, pointer_type)?;

    let signature = abi::native_signature(target_config.default_call_conv, pointer_type);
    let mut declared = Vec::with_capacity(loaded.functions.len());
    for func_id in 0..loaded.functions.len() {
        let symbol = format!("vo_aot_fn_{func_id}");
        let id = object_module.declare_function(&symbol, Linkage::Export, &signature)?;
        declared.push((id, symbol));
    }

    let env = JitCompileEnv {
        externs,
        backend_caps: JitBackendCaps {
            extern_suspend: true,
        },
    };
    let graph = Arc::new(ModuleCallGraph::build_with_limit(
        loaded.module(),
        MAX_JIT_ANALYSIS_BYTES,
    )?);
    let module_analysis =
        super::ModuleJitAnalysis::build(loaded.module(), env, graph, MAX_JIT_ANALYSIS_BYTES)?;
    let inline_plan: &ModuleInlinePlan = &module_analysis.inline_plan;
    let optimization_plan = ModuleOptimizationPlan::build_with_inline_plan(
        loaded.module(),
        Arc::clone(&module_analysis.inline_plan),
        MAX_JIT_ANALYSIS_BYTES,
    )?;
    let mut context = object_module.make_context();
    let mut frontend_context = FunctionBuilderContext::new();
    let mut functions = Vec::with_capacity(loaded.functions.len());

    for (func_index, ((func_id, symbol), function)) in
        declared.iter().zip(loaded.functions.iter()).enumerate()
    {
        let func_id = *func_id;
        let func_id_u32 = u32::try_from(func_index)
            .map_err(|_| JitError::Internal("AOT function id overflow".to_string()))?;
        let analysis = FunctionAnalysis::for_function_with_return_summaries(
            function,
            loaded.module(),
            loaded.exact_base_maps().exact_base_returns(),
            MAX_JIT_ANALYSIS_BYTES,
        )?;
        let optimization = OptimizedFunction::analyze_with_module(
            analysis.ir(),
            function,
            &optimization_plan,
            func_id_u32,
        );

        object_module.clear_context(&mut context);
        context.func.signature = signature.clone();
        context.func.name = UserFuncName::user(JitTier::Optimizing as u32, func_id_u32);
        let self_native_ref = object_module.declare_func_in_func(func_id, &mut context.func);
        let helpers = HelperRefs::new(&mut object_module, helper_funcs);
        FunctionCompiler::new(
            &mut context.func,
            &mut frontend_context,
            func_id_u32,
            function,
            loaded.module(),
            env,
            &module_analysis.entry_eligibility,
            helpers,
            &analysis,
            JitTier::Optimizing,
            inline_plan,
            Some(&optimization_plan),
            Some(&optimization),
            Some(self_native_ref),
        )
        .compile(target_config)
        .map_err(|error| {
            JitError::Internal(format!(
                "AOT function {func_index} ({}) lowering failed: {error}",
                function.name
            ))
        })?;

        if function_needs_native_root_frame(function) {
            crate::native_frame::instrument_function(
                &mut context.func,
                pointer_type,
                func_id_u32,
                JitNativeFrame::ARTIFACT_FUNCTION,
                u32::MAX,
                JitTier::Optimizing as u32,
            )?;
        }
        verify_native_frame_budget(&context)?;
        cranelift_codegen::verifier::verify_function(&context.func, object_module.isa().flags())
            .map_err(|errors| {
                JitError::Internal(format!(
                    "Cranelift AOT IR verification failed for {symbol}: {errors}"
                ))
            })?;
        if options.debug_ir {
            eprintln!("=== AOT IR for {symbol} {} ===", function.name);
            eprintln!("{}", context.func.display());
        }

        object_module.define_function(func_id, &mut context)?;
        let metadata = Arc::new(compiled_metadata(
            &context,
            symbol,
            analysis.ir().deopt_metadata(0..function.code.len()),
        )?);
        functions.push(NativeAotFunction {
            func_id: func_id_u32,
            symbol: symbol.clone(),
            metadata,
            entry_eligibility: module_analysis.entry_eligibility[func_index],
        });
    }

    let module_bytes = loaded
        .module()
        .serialize()
        .map_err(|error| JitError::Internal(format!("failed to serialize AOT module: {error}")))?;
    let metadata_bytes = encode_native_aot_metadata(&options.target_triple, &functions)?;
    let function_ids = declared.iter().map(|(id, _)| *id).collect::<Vec<_>>();
    define_exported_u64(
        &mut object_module,
        NATIVE_AOT_MODULE_LEN_SYMBOL,
        module_bytes.len() as u64,
    )?;
    define_exported_bytes(
        &mut object_module,
        NATIVE_AOT_MODULE_BYTES_SYMBOL,
        module_bytes,
    )?;
    define_exported_u64(
        &mut object_module,
        NATIVE_AOT_METADATA_LEN_SYMBOL,
        metadata_bytes.len() as u64,
    )?;
    define_exported_bytes(
        &mut object_module,
        NATIVE_AOT_METADATA_BYTES_SYMBOL,
        metadata_bytes,
    )?;
    define_function_table(&mut object_module, &function_ids)?;
    define_main(&mut object_module)?;

    let bytes = object_module
        .finish()
        .emit()
        .map_err(|error| JitError::Internal(format!("failed to emit AOT object: {error}")))?;
    Ok(NativeAotObject {
        target_triple: options.target_triple.clone(),
        bytes,
        functions,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use object::{Object, ObjectSection, ObjectSymbol, SectionKind};
    use vo_runtime::bytecode::{Module as VoModule, ResolvedExternTable};
    use vo_runtime::instruction::{Instruction, Opcode};

    fn test_module() -> Arc<LoadedModule> {
        let mut module = VoModule::new("native-aot-test".to_string());
        module.functions.push(crate::test_fixtures::function(
            vec![Instruction::new(Opcode::Return, 0, 0, 0)],
            1,
        ));
        module.set_artifact(vo_common_core::ModuleArtifact::new(
            "volang.ui.component-bundle",
            1,
            vec![0x56, 0x55, 0x42, 0x31],
        ));
        Arc::new(
            vo_common_core::verifier::verify_loaded_module(module)
                .expect("common verifier accepts AOT fixture"),
        )
    }

    #[test]
    fn native_object_contains_every_function_and_is_deterministic() {
        let options = NativeAotOptions::new(target_lexicon::HOST.to_string());
        let externs = ResolvedExternTable::empty();
        let first = compile_native_object(test_module(), &externs, &options)
            .expect("compile native AOT object");
        let second = compile_native_object(test_module(), &externs, &options)
            .expect("repeat native AOT object");
        assert!(!first.bytes.is_empty());
        assert_eq!(first.bytes, second.bytes);
        assert_eq!(first.functions.len(), 1);
        assert_eq!(first.functions[0].symbol, "vo_aot_fn_0");

        let object = object::File::parse(first.bytes.as_slice()).expect("parse emitted object");
        let table = object
            .symbols()
            .find(|symbol| {
                symbol.name().ok().is_some_and(|name| {
                    name.trim_start_matches('_') == NATIVE_AOT_FUNCTION_TABLE_SYMBOL
                })
            })
            .expect("function table symbol");
        let section = object
            .section_by_index(table.section_index().expect("function table section"))
            .expect("read function table section");
        assert_ne!(
            section.kind(),
            SectionKind::UninitializedData,
            "function-pointer relocations require a file-backed section"
        );

        let module_len_symbol = object
            .symbols()
            .find(|symbol| {
                symbol.name().ok().is_some_and(|name| {
                    name.trim_start_matches('_') == NATIVE_AOT_MODULE_LEN_SYMBOL
                })
            })
            .expect("embedded module length symbol");
        let module_len_section = object
            .section_by_index(
                module_len_symbol
                    .section_index()
                    .expect("module length symbol section"),
            )
            .expect("embedded module length section");
        let module_len_data = module_len_section
            .data()
            .expect("embedded module length section data");
        let module_len_offset =
            usize::try_from(module_len_symbol.address() - module_len_section.address()).unwrap();
        let embedded_len = usize::try_from(u64::from_le_bytes(
            module_len_data[module_len_offset..module_len_offset + 8]
                .try_into()
                .unwrap(),
        ))
        .unwrap();

        let module_symbol = object
            .symbols()
            .find(|symbol| {
                symbol.name().ok().is_some_and(|name| {
                    name.trim_start_matches('_') == NATIVE_AOT_MODULE_BYTES_SYMBOL
                })
            })
            .expect("embedded module symbol");
        let module_section = object
            .section_by_index(
                module_symbol
                    .section_index()
                    .expect("module symbol section"),
            )
            .expect("embedded module section");
        let section_data = module_section.data().expect("embedded module section data");
        let offset = usize::try_from(module_symbol.address() - module_section.address()).unwrap();
        let embedded = VoModule::deserialize(&section_data[offset..offset + embedded_len])
            .expect("decode embedded native AOT module");
        assert_eq!(
            embedded
                .artifact("volang.ui.component-bundle")
                .map(|artifact| artifact.payload.as_slice()),
            Some([0x56, 0x55, 0x42, 0x31].as_slice())
        );
    }

    #[test]
    fn windows_native_aot_uses_inline_stack_probes() {
        let isa = build_isa("x86_64-pc-windows-msvc").expect("build Windows AOT ISA");
        let flags = isa.flags().to_string();
        assert!(flags.contains("enable_probestack = true"));
        assert!(flags.contains("probestack_strategy = \"inline\""));
    }
}
