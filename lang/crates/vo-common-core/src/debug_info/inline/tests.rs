use super::*;
use crate::instruction::{Instruction, Opcode};
use crate::{InstructionMetadata, Module};
#[cfg(not(feature = "std"))]
use alloc::{format, vec};

fn module() -> Module {
    let mut module = Module::new("logical-sources".into());
    for id in 0..3 {
        module.functions.push(FunctionDef {
            name: format!("f{id}"),
            param_count: 0,
            param_slots: 0,
            local_slots: 0,
            ret_slots: 0,
            ret_slot_types: vec![],
            recv_slots: 0,
            heap_ret_gcref_count: 0,
            heap_ret_gcref_start: 0,
            heap_ret_slots: vec![],
            is_closure: false,
            error_ret_slot: -1,
            has_defer: false,
            has_calls: false,
            has_call_extern: false,
            code: vec![Instruction::new(Opcode::Return, 0, 0, 0); 4],
            instruction_metadata: vec![InstructionMetadata::None; 4],
            slot_types: vec![],
            capture_types: vec![],
            capture_slot_types: vec![],
            param_types: vec![],
        });
        module.debug_info.add_loc(id, 0, "physical.vo", 7, 1, 1);
    }
    let debug = &mut module.debug_info;
    let leaf_file = debug.get_or_add_file("leaf.vo");
    debug.inline_sources.frames = vec![
        InlineSourceFrame {
            parent: InlineSourceFrame::NO_PARENT,
            function_id: 2,
            span: Some(SourceSpan {
                file_id: 0,
                line: 30,
                col: 2,
                len: 3,
            }),
        },
        InlineSourceFrame {
            parent: 0,
            function_id: 1,
            span: None,
        },
        InlineSourceFrame {
            parent: 1,
            function_id: 0,
            span: Some(SourceSpan {
                file_id: leaf_file,
                line: 10,
                col: 70_000,
                len: 80_000,
            }),
        },
    ];
    debug.inline_sources.functions = vec![InlineFunctionSources {
        function_id: 2,
        entries: vec![InlineSourceEntry { pc: 1, frame: 2 }],
    }];
    module
}

#[test]
fn exact_leaf_to_caller_sources_survive_roundtrip_and_original_function_rewrites() {
    let mut module = module();
    crate::verifier::verify_module(&module).unwrap();
    // The copied origin is independent of the old leaf's code length and PC.
    module.functions[0].code.truncate(1);
    module.functions[0].instruction_metadata.truncate(1);
    let bytes = module.serialize().unwrap();
    let decoded = Module::deserialize(&bytes).unwrap();
    crate::verifier::verify_module(&decoded).unwrap();
    assert_eq!(
        decoded.debug_info.inline_sources,
        module.debug_info.inline_sources
    );
    let frames: Vec<_> = decoded.debug_info.logical_frames(2, 1).collect();
    assert_eq!(
        frames
            .iter()
            .map(|frame| frame.function_id)
            .collect::<Vec<_>>(),
        [0, 1, 2]
    );
    assert!(frames[1].span.is_none());
    let location = decoded.debug_info.lookup(2, 1).unwrap();
    assert_eq!(
        (
            location.file.as_str(),
            location.line,
            location.col,
            location.len
        ),
        ("leaf.vo", 10, 70_000, 80_000)
    );
    for pc in [0, 2, 3] {
        assert_eq!(decoded.debug_info.logical_frames(2, pc).count(), 1);
        assert_eq!(
            decoded.debug_info.lookup(2, pc).unwrap().file,
            "physical.vo"
        );
    }
}

#[test]
fn verifier_rejects_cycles_foreign_roots_missing_references_and_ambiguous_pcs() {
    let mutations: &[fn(&mut InlineSources)] = &[
        |s| s.frames[1].parent = 1,
        |s| s.frames[0].parent = 2,
        |s| s.frames[0].parent = 99,
        |s| s.frames[0].function_id = 0,
        |s| s.frames[2].function_id = 3,
        |s| s.frames[2].span.as_mut().unwrap().file_id = 2,
        |s| s.frames[2].span.as_mut().unwrap().line = 0,
        |s| s.frames[2].span.as_mut().unwrap().col = 0,
        |s| s.frames[2].span.as_mut().unwrap().len = 0,
        |s| s.functions[0].function_id = 3,
        |s| s.functions[0].entries[0].pc = 4,
        |s| s.functions[0].entries[0].frame = 3,
        |s| {
            let entry = s.functions[0].entries[0];
            s.functions[0].entries.push(entry);
        },
        |s| s.functions.push(s.functions[0].clone()),
    ];
    for mutate in mutations {
        let mut module = module();
        mutate(&mut module.debug_info.inline_sources);
        assert!(crate::verifier::verify_module(&module).is_err());
    }
}

#[test]
fn ancestry_depth_is_bounded_even_when_debug_data_has_not_been_verified() {
    let mut module = module();
    let sources = &mut module.debug_info.inline_sources;
    for index in sources.frames.len()..MAX_INLINE_SOURCE_DEPTH {
        sources.frames.push(InlineSourceFrame {
            parent: index as u32 - 1,
            function_id: 0,
            span: None,
        });
    }
    sources.functions[0].entries[0].frame = MAX_INLINE_SOURCE_DEPTH as u32 - 1;
    crate::verifier::verify_module(&module).unwrap();
    assert_eq!(
        module.debug_info.logical_frames(2, 1).count(),
        MAX_INLINE_SOURCE_DEPTH
    );
    module
        .debug_info
        .inline_sources
        .frames
        .push(InlineSourceFrame {
            parent: MAX_INLINE_SOURCE_DEPTH as u32 - 1,
            function_id: 0,
            span: None,
        });
    assert!(crate::verifier::verify_module(&module).is_err());
    module.debug_info.inline_sources.frames[2].parent = 2;
    module.debug_info.inline_sources.functions[0].entries[0].frame = 2;
    assert_eq!(
        module.debug_info.logical_frames(2, 1).count(),
        MAX_INLINE_SOURCE_DEPTH
    );
}

#[test]
fn native_diagnostic_anchors_compose_bytecode_ancestry_without_deduplicating_recursion() {
    use crate::debug_info::{DiagnosticSource, InstructionSource};
    let module = module();
    let leaf = InstructionSource::from_parts(2, 1).unwrap();
    let physical = InstructionSource::from_parts(1, 0).unwrap();
    let source = DiagnosticSource::from_instruction(leaf, Some(physical));
    let frames: Vec<_> = source.logical_frames(&module.debug_info).collect();
    assert_eq!(
        frames
            .iter()
            .map(|frame| frame.function_id)
            .collect::<Vec<_>>(),
        [0, 1, 2, 1]
    );
    assert_eq!(frames[0].span.unwrap().line, 10);
    assert!(frames[1].span.is_none());
    assert_eq!(frames[2].span.unwrap().line, 30);
    assert_eq!(frames[3].span.unwrap().line, 7);
    // Identical anchors refer to the same source chain, not a second activation.
    let same = DiagnosticSource::from_instruction(leaf, Some(leaf));
    assert!(same.inlined_in().is_none());
    assert_eq!(same.logical_frames(&module.debug_info).count(), 3);
    let stripped = DebugInfo::default();
    let unknown: Vec<_> = source.logical_frames(&stripped).collect();
    assert_eq!(
        unknown
            .iter()
            .map(|frame| frame.function_id)
            .collect::<Vec<_>>(),
        [2, 1]
    );
    assert!(unknown.iter().all(|frame| frame.span.is_none()));
    assert_eq!(core::mem::size_of::<DiagnosticSource>(), 16);
    assert_eq!(core::mem::size_of::<Option<DiagnosticSource>>(), 16);
}

#[test]
fn resolved_source_frames_preserve_unknown_identity_and_owned_wide_coordinates() {
    let frame = {
        let module = module();
        module.debug_info.resolve_frame(
            module.debug_info.logical_frames(2, 1).next().unwrap(),
            &module.functions,
        )
    };
    assert_eq!(frame.function_id, 0);
    assert_eq!(frame.location.as_ref().unwrap().col, 70_000);
    assert_eq!(frame.location.as_ref().unwrap().len, 80_000);
    assert!(frame.to_string().contains(":10:70000"));
    let missing = DebugInfo::default().resolve_frame(
        LogicalSourceFrame {
            function_id: 99,
            span: None,
        },
        &[],
    );
    assert_eq!(missing.function_id, 99);
    assert!(missing.function_name.is_none() && missing.location.is_none());
    assert_eq!(missing.to_string(), "function #99");
}
