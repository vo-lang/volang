//! Transport-only adapter for the replacement UI. All component semantics
//! execute in the guest; there is no thread-local UI state in this adapter.

#![no_std]

extern crate alloc;

use alloc::format;
use vo_common_core::extern_key::decode_extern_name;
use vo_runtime::bytecode::{ExternDef, ExternEffects};
use vo_runtime::ffi::{
    unique_extern_providers, ExternCallContext, ExternContractError, ExternRegistry, ExternResult,
    HostEventReplaySource,
};

pub const PACKAGE: &str = "github.com/vo-lang/ui/next/host";
include!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../../ui/next/wire/limits.rs"
));

/// Register the transport requested by a module before its provider table freezes.
/// Component state and renderer ownership stay with the guest and its host.
pub fn register_externs(
    registry: &mut ExternRegistry,
    externs: &[ExternDef],
) -> Result<(), ExternContractError> {
    for (id, definition) in unique_extern_providers(externs) {
        let Ok(key) = decode_extern_name(&definition.name) else {
            continue;
        };
        if key.package() != PACKAGE {
            continue;
        }
        if key.function() != "Exchange" {
            return Err(ExternContractError::new(format!(
                "unknown UI transport provider: {}.{}",
                key.package(),
                key.function()
            )));
        }
        registry.try_register_named_with_effects(
            id as u32,
            definition.name.clone(),
            exchange,
            ExternEffects::MAY_HOST_REPLAY,
        )?;
    }
    Ok(())
}

fn exchange(call: &mut ExternCallContext<'_>) -> ExternResult {
    if call.take_resume_host_event_token().is_some() {
        let Some(bytes) = call.take_resume_host_event_data() else {
            return ExternResult::Panic("UI exchange resumed without input".into());
        };
        if bytes.len() > MAX_FRAME_BYTES {
            return ExternResult::Panic("UI exchange input exceeds the frame limit".into());
        }
        call.ret_bytes(0, &bytes);
        return ExternResult::Ok;
    }
    let bytes = call.arg_bytes(0);
    if bytes.len() > MAX_FRAME_BYTES {
        return ExternResult::Panic("UI exchange output exceeds the frame limit".into());
    }
    let bytes = bytes.to_vec();
    let Some(token) = call.try_next_host_event_token() else {
        return ExternResult::Panic("UI exchange event identity space exhausted".into());
    };
    call.set_host_output(bytes);
    ExternResult::HostEventWaitAndReplay {
        token,
        source: HostEventReplaySource::GuiEvent,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec::Vec;
    use vo_common_core::extern_key::ExternKeyRef;
    use vo_runtime::bytecode::{ParamShape, ReturnShape};

    fn definition(package: &str, function: &str) -> ExternDef {
        ExternDef::new(
            ExternKeyRef::new(package, function).encode().unwrap(),
            ParamShape::CallSiteVariadic,
            ReturnShape::slots(1),
            ExternEffects::MAY_HOST_REPLAY,
            Vec::new(),
        )
    }

    #[test]
    fn repeated_calls_share_one_canonical_replay_provider() {
        let exchange = definition(PACKAGE, "Exchange");
        let externs = [exchange.clone(), exchange.clone()];
        let mut registry = ExternRegistry::new();
        register_externs(&mut registry, &externs).unwrap();
        assert!(registry.registered(0).is_some());
        assert!(registry.registered(1).is_none());
        assert_eq!(
            registry
                .registered_by_name(&exchange.name)
                .unwrap()
                .provider_effects(),
            ExternEffects::MAY_HOST_REPLAY
        );
        registry.resolve_module_externs(&externs).unwrap();
        let mut incompatible = exchange;
        incompatible.allowed_effects = ExternEffects::NONE;
        assert!(registry.resolve_module_externs(&[incompatible]).is_err());
    }

    #[test]
    fn transport_registration_preserves_other_owners_and_rejects_unknown_operations() {
        let other = definition("example.test/application", "Exchange");
        let unknown = definition(PACKAGE, "Missing");
        let mut registry = ExternRegistry::new();
        register_externs(&mut registry, core::slice::from_ref(&other)).unwrap();
        assert!(registry.registered_by_name(&other.name).is_none());
        assert!(register_externs(&mut registry, core::slice::from_ref(&unknown)).is_err());
        assert!(registry.registered_by_name(&unknown.name).is_none());
        register_externs(&mut registry, &[definition(PACKAGE, "Exchange")]).unwrap();
    }
}
