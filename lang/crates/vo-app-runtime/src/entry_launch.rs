use alloc::collections::BTreeSet;
use alloc::string::String;
use alloc::vec::Vec;

use crate::{
    CapabilityId, MaterializedRuntimeArtifact, ResolvedAppRuntimePlan, RuntimeArtifactRole,
};

pub const CAPABILITY_VOGUI_RUN_ENTRY: &str = "vogui.run-entry";
pub const CAPABILITY_VOPLAY_RUN_ENTRY: &str = "voplay.run-entry";
pub const CAPABILITY_VOGUI_TARGET_INIT: &str = "vogui.target-init";
pub const CAPABILITY_VOGUI_TARGET_NEXT_TURN: &str = "vogui.target-next-turn";
pub const CAPABILITY_VOGUI_TARGET_COMMIT: &str = "vogui.target-commit";
pub const CAPABILITY_VOPLAY_TARGET_START: &str = "voplay.target-start";
pub const CAPABILITY_VOPLAY_TARGET_NEXT_TICKS: &str = "voplay.target-next-ticks";
pub const CAPABILITY_VOPLAY_TARGET_COMMIT_TICKS: &str = "voplay.target-commit-ticks";
pub const CAPABILITY_VOPLAY_NEW_ENGINE: &str = "voplay.new-engine";
pub const CAPABILITY_VOPLAY_INSTALL_ENTRY: &str = "voplay.install-entry";
pub const CAPABILITY_VOPLAY_ENGINE_START: &str = "voplay.engine-start";
pub const CAPABILITY_VOPLAY_ENGINE_STEP: &str = "voplay.engine-step";
pub const CAPABILITY_VOPLAY_ENGINE_PAUSE: &str = "voplay.engine-pause";
pub const CAPABILITY_VOPLAY_ENGINE_RESUME: &str = "voplay.engine-resume";
pub const CAPABILITY_VOPLAY_ENGINE_SHUTDOWN: &str = "voplay.engine-shutdown";
pub const MAX_ENTRY_INIT_BYTES: usize = 16 * 1024 * 1024;
pub const MAX_TARGET_STARTUP_BYTES: usize = 16 * 1024 * 1024;

const ENTRY_LAUNCH_MAGIC: &[u8] = b"vo-entry-launch-v1\0";
const VOGUI_TARGET_INIT_MAGIC: &[u8] = b"vogui-target-init-v1\0";
const VOGUI_TARGET_COMMIT_MAGIC: &[u8] = b"vogui-target-commit-v1\0";
const VOPLAY_TARGET_START_MAGIC: &[u8] = b"voplay-target-start-v3\0";
const VOPLAY_TARGET_COMMIT_TICKS_MAGIC: &[u8] = b"voplay-target-commit-ticks-v1\0";
const VOGUI_DESCRIPTOR_BYTES: usize = 172;
const VOPLAY_DESCRIPTOR_BYTES: usize = 104;
const VOPLAY_NEW_ENGINE_MAGIC: &[u8] = b"voplay-new-engine-v1\0";
const VOPLAY_INSTALL_ENTRY_MAGIC: &[u8] = b"voplay-install-entry-v1\0";
const VOPLAY_ENGINE_CONTROL_MAGIC: &[u8] = b"voplay-engine-control-v1\0";

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EntryFramework {
    Vogui,
    Voplay,
}

impl EntryFramework {
    pub const fn capability(self) -> &'static str {
        match self {
            Self::Vogui => CAPABILITY_VOGUI_RUN_ENTRY,
            Self::Voplay => CAPABILITY_VOPLAY_RUN_ENTRY,
        }
    }

    pub fn capability_id(self) -> CapabilityId {
        stable_capability_id(self.capability().as_bytes())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct VoguiEntryDescriptor {
    pub artifact_identity: [u8; 32],
    pub factory_id: u64,
    pub app_build_identity: [u8; 32],
    pub model_fingerprint: [u8; 32],
    pub message_fingerprint: [u8; 32],
    pub role_artifact_set_fingerprint: [u8; 32],
    pub transaction_mode: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct VoplayEntryDescriptor {
    pub artifact_identity: [u8; 32],
    pub factory_id: u64,
    pub schema_fingerprint: [u8; 32],
    pub role_artifact_set_fingerprint: [u8; 32],
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EntryDescriptor {
    Vogui(VoguiEntryDescriptor),
    Voplay(VoplayEntryDescriptor),
}

impl EntryDescriptor {
    pub const fn framework(self) -> EntryFramework {
        match self {
            Self::Vogui(_) => EntryFramework::Vogui,
            Self::Voplay(_) => EntryFramework::Voplay,
        }
    }

    pub const fn artifact_identity(self) -> [u8; 32] {
        match self {
            Self::Vogui(descriptor) => descriptor.artifact_identity,
            Self::Voplay(descriptor) => descriptor.artifact_identity,
        }
    }

    pub const fn factory_id(self) -> u64 {
        match self {
            Self::Vogui(descriptor) => descriptor.factory_id,
            Self::Voplay(descriptor) => descriptor.factory_id,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EntryLaunch {
    pub descriptor: EntryDescriptor,
    pub init: Vec<u8>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CertifiedEntryLaunch {
    pub launch: EntryLaunch,
    pub factory: crate::CertifiedEntryFactory,
    pub entry_artifact: MaterializedRuntimeArtifact,
    pub plan_identity: [u8; 32],
    pub plan_generation: u64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EntryLaunchError {
    UnsupportedCapability,
    MalformedEnvelope,
    DescriptorLength,
    InitCapacity,
    InvalidDescriptor,
    InvalidTransactionMode,
    InvalidRuntimePlan,
    CapabilityNotGranted,
    EntryFactoryNotCertified,
    EntryArtifactNotFound,
    EntryArtifactAmbiguous,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct VoplayPublicEngineRef {
    pub session_index: u32,
    pub session_generation: u32,
    pub session_epoch: u64,
    pub engine_index: u32,
    pub engine_generation: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VoplayPublicEngineDesc {
    pub profile: String,
    pub fixed_tick_nanos: u64,
    pub max_catch_up_ticks: u32,
    pub max_world_entities: u32,
    pub headless: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum VoplayEngineCommand {
    New {
        session_index: u32,
        session_generation: u32,
        session_epoch: u64,
        descriptor: VoplayPublicEngineDesc,
    },
    Install {
        engine: VoplayPublicEngineRef,
        entry: EntryLaunch,
    },
    Start(VoplayPublicEngineRef),
    Step {
        engine: VoplayPublicEngineRef,
        count: u64,
    },
    Pause(VoplayPublicEngineRef),
    Resume(VoplayPublicEngineRef),
    Shutdown(VoplayPublicEngineRef),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum VoplayEngineCommandError {
    UnsupportedCapability,
    MalformedEnvelope,
    InvalidIdentity,
    InvalidDescriptor,
    Capacity,
}

pub fn decode_voplay_engine_command(
    capability: &[u8],
    payload: &[u8],
) -> Result<VoplayEngineCommand, VoplayEngineCommandError> {
    match capability {
        value if value == CAPABILITY_VOPLAY_NEW_ENGINE.as_bytes() => {
            decode_voplay_new_engine(payload)
        }
        value if value == CAPABILITY_VOPLAY_INSTALL_ENTRY.as_bytes() => {
            decode_voplay_install_entry(payload)
        }
        value if value == CAPABILITY_VOPLAY_ENGINE_START.as_bytes() => {
            decode_voplay_engine_control(payload, false).map(VoplayEngineCommand::Start)
        }
        value if value == CAPABILITY_VOPLAY_ENGINE_STEP.as_bytes() => {
            let header = VOPLAY_ENGINE_CONTROL_MAGIC.len() + 24;
            if payload.len() != header + 8 {
                return Err(VoplayEngineCommandError::MalformedEnvelope);
            }
            let engine = decode_public_engine_ref(payload, VOPLAY_ENGINE_CONTROL_MAGIC.len())?;
            let count = read_target_u64(payload, header)
                .map_err(|_| VoplayEngineCommandError::MalformedEnvelope)?;
            if count == 0 {
                return Err(VoplayEngineCommandError::InvalidDescriptor);
            }
            Ok(VoplayEngineCommand::Step { engine, count })
        }
        value if value == CAPABILITY_VOPLAY_ENGINE_PAUSE.as_bytes() => {
            decode_voplay_engine_control(payload, false).map(VoplayEngineCommand::Pause)
        }
        value if value == CAPABILITY_VOPLAY_ENGINE_RESUME.as_bytes() => {
            decode_voplay_engine_control(payload, false).map(VoplayEngineCommand::Resume)
        }
        value if value == CAPABILITY_VOPLAY_ENGINE_SHUTDOWN.as_bytes() => {
            decode_voplay_engine_control(payload, false).map(VoplayEngineCommand::Shutdown)
        }
        _ => Err(VoplayEngineCommandError::UnsupportedCapability),
    }
}

fn decode_voplay_new_engine(
    payload: &[u8],
) -> Result<VoplayEngineCommand, VoplayEngineCommandError> {
    let offset = VOPLAY_NEW_ENGINE_MAGIC.len();
    let prefix = offset + 37;
    if payload.len() < prefix || !payload.starts_with(VOPLAY_NEW_ENGINE_MAGIC) {
        return Err(VoplayEngineCommandError::MalformedEnvelope);
    }
    let session_index = read_target_u32(payload, offset)
        .map_err(|_| VoplayEngineCommandError::MalformedEnvelope)?;
    let session_generation = read_target_u32(payload, offset + 4)
        .map_err(|_| VoplayEngineCommandError::MalformedEnvelope)?;
    let session_epoch = read_target_u64(payload, offset + 8)
        .map_err(|_| VoplayEngineCommandError::MalformedEnvelope)?;
    let fixed_tick_nanos = read_target_u64(payload, offset + 16)
        .map_err(|_| VoplayEngineCommandError::MalformedEnvelope)?;
    let max_catch_up_ticks = read_target_u32(payload, offset + 24)
        .map_err(|_| VoplayEngineCommandError::MalformedEnvelope)?;
    let max_world_entities = read_target_u32(payload, offset + 28)
        .map_err(|_| VoplayEngineCommandError::MalformedEnvelope)?;
    let headless = match payload[offset + 32] {
        0 => false,
        1 => true,
        _ => return Err(VoplayEngineCommandError::InvalidDescriptor),
    };
    let profile_len = read_target_u32(payload, offset + 33)
        .map_err(|_| VoplayEngineCommandError::MalformedEnvelope)? as usize;
    if session_index == u32::MAX
        || session_generation == 0
        || session_epoch == 0
        || fixed_tick_nanos == 0
        || max_catch_up_ticks == 0
        || max_world_entities == 0
        || profile_len == 0
        || profile_len > 64
        || prefix.checked_add(profile_len) != Some(payload.len())
    {
        return Err(VoplayEngineCommandError::InvalidDescriptor);
    }
    let profile = core::str::from_utf8(&payload[prefix..])
        .map_err(|_| VoplayEngineCommandError::InvalidDescriptor)?;
    if !matches!(profile, "core" | "2d" | "3d" | "full" | "editor") {
        return Err(VoplayEngineCommandError::InvalidDescriptor);
    }
    Ok(VoplayEngineCommand::New {
        session_index,
        session_generation,
        session_epoch,
        descriptor: VoplayPublicEngineDesc {
            profile: profile.into(),
            fixed_tick_nanos,
            max_catch_up_ticks,
            max_world_entities,
            headless,
        },
    })
}

fn decode_voplay_install_entry(
    payload: &[u8],
) -> Result<VoplayEngineCommand, VoplayEngineCommandError> {
    let offset = VOPLAY_INSTALL_ENTRY_MAGIC.len();
    let prefix = offset + 32;
    if payload.len() < prefix || !payload.starts_with(VOPLAY_INSTALL_ENTRY_MAGIC) {
        return Err(VoplayEngineCommandError::MalformedEnvelope);
    }
    let engine = decode_public_engine_ref(payload, offset)?;
    let descriptor_len = read_target_u32(payload, offset + 24)
        .map_err(|_| VoplayEngineCommandError::MalformedEnvelope)?
        as usize;
    let init_len = read_target_u32(payload, offset + 28)
        .map_err(|_| VoplayEngineCommandError::MalformedEnvelope)? as usize;
    let descriptor_end = prefix
        .checked_add(descriptor_len)
        .ok_or(VoplayEngineCommandError::Capacity)?;
    if descriptor_len != VOPLAY_DESCRIPTOR_BYTES
        || init_len > MAX_ENTRY_INIT_BYTES
        || descriptor_end.checked_add(init_len) != Some(payload.len())
    {
        return Err(VoplayEngineCommandError::InvalidDescriptor);
    }
    let descriptor = decode_voplay_descriptor(&payload[prefix..descriptor_end])
        .map_err(|_| VoplayEngineCommandError::InvalidDescriptor)?;
    Ok(VoplayEngineCommand::Install {
        engine,
        entry: EntryLaunch {
            descriptor: EntryDescriptor::Voplay(descriptor),
            init: payload[descriptor_end..].to_vec(),
        },
    })
}

fn decode_voplay_engine_control(
    payload: &[u8],
    _allow_count: bool,
) -> Result<VoplayPublicEngineRef, VoplayEngineCommandError> {
    if payload.len() != VOPLAY_ENGINE_CONTROL_MAGIC.len() + 24
        || !payload.starts_with(VOPLAY_ENGINE_CONTROL_MAGIC)
    {
        return Err(VoplayEngineCommandError::MalformedEnvelope);
    }
    decode_public_engine_ref(payload, VOPLAY_ENGINE_CONTROL_MAGIC.len())
}

fn decode_public_engine_ref(
    payload: &[u8],
    offset: usize,
) -> Result<VoplayPublicEngineRef, VoplayEngineCommandError> {
    let value = VoplayPublicEngineRef {
        session_index: read_target_u32(payload, offset)
            .map_err(|_| VoplayEngineCommandError::MalformedEnvelope)?,
        session_generation: read_target_u32(payload, offset + 4)
            .map_err(|_| VoplayEngineCommandError::MalformedEnvelope)?,
        session_epoch: read_target_u64(payload, offset + 8)
            .map_err(|_| VoplayEngineCommandError::MalformedEnvelope)?,
        engine_index: read_target_u32(payload, offset + 16)
            .map_err(|_| VoplayEngineCommandError::MalformedEnvelope)?,
        engine_generation: read_target_u32(payload, offset + 20)
            .map_err(|_| VoplayEngineCommandError::MalformedEnvelope)?,
    };
    if value.session_index == u32::MAX
        || value.session_generation == 0
        || value.session_epoch == 0
        || value.engine_index == u32::MAX
        || value.engine_generation == 0
    {
        return Err(VoplayEngineCommandError::InvalidIdentity);
    }
    Ok(value)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TargetStartup {
    Vogui {
        model: Vec<u8>,
        effects: Vec<u8>,
        presentation: Vec<u8>,
        subscriptions: Vec<u8>,
    },
    Voplay {
        configuration: Vec<u8>,
        schedule_hash: u64,
        operations: Vec<VoplayStartupOperation>,
        fixed_tick_nanos: u64,
        max_catch_up_ticks: u32,
    },
}

impl TargetStartup {
    pub const fn framework(&self) -> EntryFramework {
        match self {
            Self::Vogui { .. } => EntryFramework::Vogui,
            Self::Voplay { .. } => EntryFramework::Voplay,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum VoplayStartupOperation {
    RegisterComponent(Vec<u8>),
    RegisterSystem {
        stage: u32,
        system_id: u64,
        descriptor: Vec<u8>,
    },
    RegisterPlugin(Vec<u8>),
    RegisterAssetLoader(Vec<u8>),
    RegisterRenderFeature(Vec<u8>),
    SetFixedTick {
        nanos: u64,
        max_catch_up: u32,
    },
    Spawn(Vec<u8>),
    RequestAsset(Vec<u8>),
    CreateRenderView(Vec<u8>),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TargetStartupError {
    UnsupportedCapability,
    Capacity,
    MalformedEnvelope,
    InvalidOperation,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VoguiTargetCommit {
    pub model: Vec<u8>,
    pub update_result: Vec<u8>,
    pub effects: Vec<u8>,
    pub presentation: Vec<u8>,
    pub subscriptions: Vec<u8>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct VoplayTickCommit {
    pub first_tick: u64,
    pub count: u64,
    pub result: Vec<u8>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EntryFactoryMetadataError {
    MalformedMarker,
    UnknownFramework,
    InvalidFactoryId,
    InvalidDigest,
}

pub fn decode_entry_launch(
    capability: &[u8],
    payload: &[u8],
) -> Result<EntryLaunch, EntryLaunchError> {
    let framework = match capability {
        value if value == CAPABILITY_VOGUI_RUN_ENTRY.as_bytes() => EntryFramework::Vogui,
        value if value == CAPABILITY_VOPLAY_RUN_ENTRY.as_bytes() => EntryFramework::Voplay,
        _ => return Err(EntryLaunchError::UnsupportedCapability),
    };
    let header_len = ENTRY_LAUNCH_MAGIC
        .len()
        .checked_add(8)
        .ok_or(EntryLaunchError::MalformedEnvelope)?;
    if payload.len() < header_len || !payload.starts_with(ENTRY_LAUNCH_MAGIC) {
        return Err(EntryLaunchError::MalformedEnvelope);
    }
    let descriptor_len = read_u32(payload, ENTRY_LAUNCH_MAGIC.len())? as usize;
    let init_len = read_u32(payload, ENTRY_LAUNCH_MAGIC.len() + 4)? as usize;
    if init_len > MAX_ENTRY_INIT_BYTES {
        return Err(EntryLaunchError::InitCapacity);
    }
    let descriptor_end = header_len
        .checked_add(descriptor_len)
        .ok_or(EntryLaunchError::MalformedEnvelope)?;
    let payload_end = descriptor_end
        .checked_add(init_len)
        .ok_or(EntryLaunchError::MalformedEnvelope)?;
    if payload_end != payload.len() {
        return Err(EntryLaunchError::MalformedEnvelope);
    }
    let descriptor_bytes = &payload[header_len..descriptor_end];
    let descriptor = match framework {
        EntryFramework::Vogui => EntryDescriptor::Vogui(decode_vogui_descriptor(descriptor_bytes)?),
        EntryFramework::Voplay => {
            EntryDescriptor::Voplay(decode_voplay_descriptor(descriptor_bytes)?)
        }
    };
    Ok(EntryLaunch {
        descriptor,
        init: payload[descriptor_end..].to_vec(),
    })
}

pub fn decode_target_startup(
    capability: &[u8],
    payload: &[u8],
) -> Result<TargetStartup, TargetStartupError> {
    if payload.len() > MAX_TARGET_STARTUP_BYTES {
        return Err(TargetStartupError::Capacity);
    }
    match capability {
        value if value == CAPABILITY_VOGUI_TARGET_INIT.as_bytes() => {
            decode_vogui_target_init(payload)
        }
        value if value == CAPABILITY_VOPLAY_TARGET_START.as_bytes() => {
            decode_voplay_target_start(payload)
        }
        _ => Err(TargetStartupError::UnsupportedCapability),
    }
}

pub fn decode_vogui_target_commit(payload: &[u8]) -> Result<VoguiTargetCommit, TargetStartupError> {
    if payload.len() > MAX_TARGET_STARTUP_BYTES {
        return Err(TargetStartupError::Capacity);
    }
    let header_len = VOGUI_TARGET_COMMIT_MAGIC
        .len()
        .checked_add(20)
        .ok_or(TargetStartupError::MalformedEnvelope)?;
    if payload.len() < header_len || !payload.starts_with(VOGUI_TARGET_COMMIT_MAGIC) {
        return Err(TargetStartupError::MalformedEnvelope);
    }
    let model_len = read_target_u32(payload, VOGUI_TARGET_COMMIT_MAGIC.len())? as usize;
    let update_len = read_target_u32(payload, VOGUI_TARGET_COMMIT_MAGIC.len() + 4)? as usize;
    let effects_len = read_target_u32(payload, VOGUI_TARGET_COMMIT_MAGIC.len() + 8)? as usize;
    let presentation_len = read_target_u32(payload, VOGUI_TARGET_COMMIT_MAGIC.len() + 12)? as usize;
    let subscriptions_len =
        read_target_u32(payload, VOGUI_TARGET_COMMIT_MAGIC.len() + 16)? as usize;
    let model_end = header_len
        .checked_add(model_len)
        .ok_or(TargetStartupError::MalformedEnvelope)?;
    let update_end = model_end
        .checked_add(update_len)
        .ok_or(TargetStartupError::MalformedEnvelope)?;
    let effects_end = update_end
        .checked_add(effects_len)
        .ok_or(TargetStartupError::MalformedEnvelope)?;
    let presentation_end = effects_end
        .checked_add(presentation_len)
        .ok_or(TargetStartupError::MalformedEnvelope)?;
    let payload_end = presentation_end
        .checked_add(subscriptions_len)
        .ok_or(TargetStartupError::MalformedEnvelope)?;
    if payload_end != payload.len() {
        return Err(TargetStartupError::MalformedEnvelope);
    }
    validate_vogui_presentation(&payload[effects_end..presentation_end])?;
    validate_vogui_subscriptions(&payload[presentation_end..])?;
    Ok(VoguiTargetCommit {
        model: payload[header_len..model_end].to_vec(),
        update_result: payload[model_end..update_end].to_vec(),
        effects: payload[update_end..effects_end].to_vec(),
        presentation: payload[effects_end..presentation_end].to_vec(),
        subscriptions: payload[presentation_end..].to_vec(),
    })
}

pub fn decode_voplay_tick_commit(payload: &[u8]) -> Result<VoplayTickCommit, TargetStartupError> {
    let header_len = VOPLAY_TARGET_COMMIT_TICKS_MAGIC
        .len()
        .checked_add(20)
        .ok_or(TargetStartupError::MalformedEnvelope)?;
    if payload.len() < header_len || !payload.starts_with(VOPLAY_TARGET_COMMIT_TICKS_MAGIC) {
        return Err(TargetStartupError::MalformedEnvelope);
    }
    let offset = VOPLAY_TARGET_COMMIT_TICKS_MAGIC.len();
    let first_tick = read_target_u64(payload, offset)?;
    let count = read_target_u64(payload, offset + 8)?;
    let result_len = read_target_u32(payload, offset + 16)? as usize;
    let end = header_len
        .checked_add(result_len)
        .ok_or(TargetStartupError::MalformedEnvelope)?;
    if first_tick == 0
        || count == 0
        || end != payload.len()
        || result_len > MAX_TARGET_STARTUP_BYTES
    {
        return Err(TargetStartupError::InvalidOperation);
    }
    Ok(VoplayTickCommit {
        first_tick,
        count,
        result: payload[header_len..end].to_vec(),
    })
}

fn decode_vogui_target_init(payload: &[u8]) -> Result<TargetStartup, TargetStartupError> {
    let header_len = VOGUI_TARGET_INIT_MAGIC
        .len()
        .checked_add(16)
        .ok_or(TargetStartupError::MalformedEnvelope)?;
    if payload.len() < header_len || !payload.starts_with(VOGUI_TARGET_INIT_MAGIC) {
        return Err(TargetStartupError::MalformedEnvelope);
    }
    let model_len = read_target_u32(payload, VOGUI_TARGET_INIT_MAGIC.len())? as usize;
    let effects_len = read_target_u32(payload, VOGUI_TARGET_INIT_MAGIC.len() + 4)? as usize;
    let presentation_len = read_target_u32(payload, VOGUI_TARGET_INIT_MAGIC.len() + 8)? as usize;
    let subscriptions_len = read_target_u32(payload, VOGUI_TARGET_INIT_MAGIC.len() + 12)? as usize;
    let model_end = header_len
        .checked_add(model_len)
        .ok_or(TargetStartupError::MalformedEnvelope)?;
    let effects_end = model_end
        .checked_add(effects_len)
        .ok_or(TargetStartupError::MalformedEnvelope)?;
    let presentation_end = effects_end
        .checked_add(presentation_len)
        .ok_or(TargetStartupError::MalformedEnvelope)?;
    let payload_end = presentation_end
        .checked_add(subscriptions_len)
        .ok_or(TargetStartupError::MalformedEnvelope)?;
    if payload_end != payload.len() {
        return Err(TargetStartupError::MalformedEnvelope);
    }
    validate_vogui_presentation(&payload[effects_end..presentation_end])?;
    validate_vogui_subscriptions(&payload[presentation_end..])?;
    Ok(TargetStartup::Vogui {
        model: payload[header_len..model_end].to_vec(),
        effects: payload[model_end..effects_end].to_vec(),
        presentation: payload[effects_end..presentation_end].to_vec(),
        subscriptions: payload[presentation_end..].to_vec(),
    })
}

fn validate_vogui_presentation(payload: &[u8]) -> Result<(), TargetStartupError> {
    if payload.starts_with(b"VGR1") {
        if payload.len() < 8 {
            return Err(TargetStartupError::MalformedEnvelope);
        }
        let count = u16::from_le_bytes(payload[4..6].try_into().unwrap()) as usize;
        if count == 0 || payload[6..8] != [0, 0] {
            return Err(TargetStartupError::MalformedEnvelope);
        }
        let mut cursor = 8_usize;
        let mut roots = BTreeSet::new();
        for _ in 0..count {
            let logical_root = read_target_u64(payload, cursor)?;
            let length = read_target_u32(payload, cursor + 8)? as usize;
            cursor = cursor
                .checked_add(12)
                .ok_or(TargetStartupError::MalformedEnvelope)?;
            let end = cursor
                .checked_add(length)
                .filter(|end| *end <= payload.len())
                .ok_or(TargetStartupError::MalformedEnvelope)?;
            if logical_root == 0 || !roots.insert(logical_root) {
                return Err(TargetStartupError::InvalidOperation);
            }
            validate_vogui_presentation(&payload[cursor..end])?;
            cursor = end;
        }
        return (cursor == payload.len())
            .then_some(())
            .ok_or(TargetStartupError::MalformedEnvelope);
    }
    let mut cursor = 0usize;
    let mut identities = BTreeSet::new();
    let mut references = Vec::new();
    let mut root = None;
    while cursor < payload.len() {
        let tag = payload[cursor];
        cursor = cursor
            .checked_add(1)
            .ok_or(TargetStartupError::MalformedEnvelope)?;
        match tag {
            1 => {
                let id = read_target_u64(payload, cursor)?;
                if id == 0 {
                    return Err(TargetStartupError::InvalidOperation);
                }
                if !identities.insert(id) {
                    return Err(TargetStartupError::InvalidOperation);
                }
                let key_len = read_target_u32(payload, cursor + 16)? as usize;
                let properties_len = read_target_u32(payload, cursor + 20)? as usize;
                let child_count = read_target_u32(payload, cursor + 24)? as usize;
                let children_offset = cursor
                    .checked_add(28)
                    .and_then(|value| value.checked_add(key_len))
                    .and_then(|value| value.checked_add(properties_len))
                    .ok_or(TargetStartupError::MalformedEnvelope)?;
                for index in 0..child_count {
                    references.push(read_target_u64(
                        payload,
                        children_offset.saturating_add(index.saturating_mul(8)),
                    )?);
                }
                cursor = cursor
                    .checked_add(28)
                    .and_then(|value| value.checked_add(key_len))
                    .and_then(|value| value.checked_add(properties_len))
                    .and_then(|value| value.checked_add(child_count.saturating_mul(8)))
                    .ok_or(TargetStartupError::MalformedEnvelope)?;
            }
            2 => {
                let id = read_target_u64(payload, cursor)?;
                if id == 0 {
                    return Err(TargetStartupError::InvalidOperation);
                }
                if !identities.insert(id) {
                    return Err(TargetStartupError::InvalidOperation);
                }
                references.push(read_target_u64(payload, cursor + 16)?);
                let key_len = read_target_u32(payload, cursor + 24)? as usize;
                cursor = cursor
                    .checked_add(28)
                    .and_then(|value| value.checked_add(key_len))
                    .ok_or(TargetStartupError::MalformedEnvelope)?;
            }
            3 => {
                references.push(read_target_u64(payload, cursor)?);
                cursor = cursor
                    .checked_add(24)
                    .ok_or(TargetStartupError::MalformedEnvelope)?;
            }
            4 => {
                references.push(read_target_u64(payload, cursor)?);
                cursor = cursor
                    .checked_add(16)
                    .ok_or(TargetStartupError::MalformedEnvelope)?;
            }
            5 => {
                let identity = read_target_u64(payload, cursor)?;
                if identity == 0 || root.replace(identity).is_some() {
                    return Err(TargetStartupError::InvalidOperation);
                }
                cursor = cursor
                    .checked_add(8)
                    .ok_or(TargetStartupError::MalformedEnvelope)?;
            }
            _ => return Err(TargetStartupError::InvalidOperation),
        }
        if cursor > payload.len() {
            return Err(TargetStartupError::MalformedEnvelope);
        }
    }
    let root = root.ok_or(TargetStartupError::InvalidOperation)?;
    if !identities.contains(&root)
        || references
            .iter()
            .any(|identity| *identity == 0 || !identities.contains(identity))
    {
        return Err(TargetStartupError::InvalidOperation);
    }
    Ok(())
}

fn validate_vogui_subscriptions(payload: &[u8]) -> Result<(), TargetStartupError> {
    let mut cursor = 0usize;
    while cursor < payload.len() {
        if payload[cursor] != 1 {
            return Err(TargetStartupError::InvalidOperation);
        }
        cursor = cursor
            .checked_add(1)
            .ok_or(TargetStartupError::MalformedEnvelope)?;
        let kind_len = read_target_u32(payload, cursor)? as usize;
        let descriptor_len = read_target_u32(payload, cursor + 4)? as usize;
        cursor = cursor
            .checked_add(16)
            .and_then(|value| value.checked_add(kind_len))
            .and_then(|value| value.checked_add(descriptor_len))
            .ok_or(TargetStartupError::MalformedEnvelope)?;
        if cursor > payload.len() {
            return Err(TargetStartupError::MalformedEnvelope);
        }
    }
    Ok(())
}

fn decode_voplay_target_start(payload: &[u8]) -> Result<TargetStartup, TargetStartupError> {
    let header_len = VOPLAY_TARGET_START_MAGIC
        .len()
        .checked_add(12)
        .ok_or(TargetStartupError::MalformedEnvelope)?;
    if payload.len() < header_len || !payload.starts_with(VOPLAY_TARGET_START_MAGIC) {
        return Err(TargetStartupError::MalformedEnvelope);
    }
    let schedule_hash = read_target_u64(payload, VOPLAY_TARGET_START_MAGIC.len())?;
    if schedule_hash == 0 {
        return Err(TargetStartupError::InvalidOperation);
    }
    let operation_count = read_target_u32(payload, VOPLAY_TARGET_START_MAGIC.len() + 8)? as usize;
    if operation_count > 65_536 {
        return Err(TargetStartupError::Capacity);
    }
    let mut cursor = header_len;
    let mut operations = Vec::with_capacity(operation_count);
    let mut fixed_tick_nanos = 0;
    let mut max_catch_up_ticks = 0;
    let mut start_phase = false;
    for _ in 0..operation_count {
        let tag = *payload
            .get(cursor)
            .ok_or(TargetStartupError::MalformedEnvelope)?;
        if !(1..=6).contains(&tag) && !(16..=18).contains(&tag) {
            return Err(TargetStartupError::InvalidOperation);
        }
        cursor = cursor
            .checked_add(1)
            .ok_or(TargetStartupError::MalformedEnvelope)?;
        let first_len = read_target_u32(payload, cursor)? as usize;
        let second_len = read_target_u32(payload, cursor + 4)? as usize;
        let data_offset = cursor
            .checked_add(8)
            .ok_or(TargetStartupError::MalformedEnvelope)?;
        let first_end = data_offset
            .checked_add(first_len)
            .ok_or(TargetStartupError::MalformedEnvelope)?;
        let second_end = first_end
            .checked_add(second_len)
            .ok_or(TargetStartupError::MalformedEnvelope)?;
        if second_end > payload.len() || first_len > 1024 * 1024 || second_len > 1024 * 1024 {
            return Err(TargetStartupError::Capacity);
        }
        if (tag == 2 && first_len != 12) || (tag == 6 && (first_len != 8 || second_len != 4)) {
            return Err(TargetStartupError::InvalidOperation);
        }
        if tag >= 16 {
            start_phase = true;
        } else if start_phase {
            return Err(TargetStartupError::InvalidOperation);
        }
        if tag != 2 && tag != 6 && second_len != 0 {
            return Err(TargetStartupError::InvalidOperation);
        }
        let first = &payload[data_offset..first_end];
        let second = &payload[first_end..second_end];
        let operation = match tag {
            1 => VoplayStartupOperation::RegisterComponent(first.to_vec()),
            2 => VoplayStartupOperation::RegisterSystem {
                stage: {
                    let stage = u32::from_le_bytes(first[..4].try_into().unwrap());
                    if !(1..=11).contains(&stage) {
                        return Err(TargetStartupError::InvalidOperation);
                    }
                    stage
                },
                system_id: {
                    let id = u64::from_le_bytes(first[4..12].try_into().unwrap());
                    if id == 0 {
                        return Err(TargetStartupError::InvalidOperation);
                    }
                    id
                },
                descriptor: second.to_vec(),
            },
            3 => VoplayStartupOperation::RegisterPlugin(first.to_vec()),
            4 => VoplayStartupOperation::RegisterAssetLoader(first.to_vec()),
            5 => VoplayStartupOperation::RegisterRenderFeature(first.to_vec()),
            6 => {
                let nanos = u64::from_le_bytes(first.try_into().unwrap());
                let max_catch_up = u32::from_le_bytes(second.try_into().unwrap());
                if nanos == 0 || max_catch_up == 0 || fixed_tick_nanos != 0 {
                    return Err(TargetStartupError::InvalidOperation);
                }
                fixed_tick_nanos = nanos;
                max_catch_up_ticks = max_catch_up;
                VoplayStartupOperation::SetFixedTick {
                    nanos,
                    max_catch_up,
                }
            }
            16 => VoplayStartupOperation::Spawn(first.to_vec()),
            17 => VoplayStartupOperation::RequestAsset(first.to_vec()),
            18 => VoplayStartupOperation::CreateRenderView(first.to_vec()),
            _ => return Err(TargetStartupError::InvalidOperation),
        };
        operations.push(operation);
        cursor = second_end;
    }
    if cursor != payload.len() {
        return Err(TargetStartupError::MalformedEnvelope);
    }
    Ok(TargetStartup::Voplay {
        configuration: payload.to_vec(),
        schedule_hash,
        operations,
        fixed_tick_nanos,
        max_catch_up_ticks,
    })
}

fn read_target_u32(payload: &[u8], offset: usize) -> Result<u32, TargetStartupError> {
    let bytes = payload
        .get(offset..offset.saturating_add(4))
        .and_then(|bytes| bytes.try_into().ok())
        .ok_or(TargetStartupError::MalformedEnvelope)?;
    Ok(u32::from_le_bytes(bytes))
}

fn read_target_u64(payload: &[u8], offset: usize) -> Result<u64, TargetStartupError> {
    let bytes = payload
        .get(offset..offset.saturating_add(8))
        .and_then(|bytes| bytes.try_into().ok())
        .ok_or(TargetStartupError::MalformedEnvelope)?;
    Ok(u64::from_le_bytes(bytes))
}

pub fn certify_entry_launch(
    plan: &ResolvedAppRuntimePlan,
    launch: EntryLaunch,
) -> Result<CertifiedEntryLaunch, EntryLaunchError> {
    plan.validate()
        .map_err(|_| EntryLaunchError::InvalidRuntimePlan)?;
    if !plan
        .granted_capabilities
        .contains(&launch.descriptor.framework().capability_id())
    {
        return Err(EntryLaunchError::CapabilityNotGranted);
    }
    let (binding_fingerprint, role_artifact_set_fingerprint) = match launch.descriptor {
        EntryDescriptor::Vogui(descriptor) => (
            descriptor.app_build_identity,
            descriptor.role_artifact_set_fingerprint,
        ),
        EntryDescriptor::Voplay(descriptor) => (
            descriptor.schema_fingerprint,
            descriptor.role_artifact_set_fingerprint,
        ),
    };
    let factory = plan
        .entry_factories
        .iter()
        .find(|factory| {
            factory.framework == launch.descriptor.framework()
                && factory.factory_id == launch.descriptor.factory_id()
                && factory.artifact_identity == launch.descriptor.artifact_identity()
                && factory.binding_fingerprint == binding_fingerprint
                && factory.role_artifact_set_fingerprint == role_artifact_set_fingerprint
        })
        .copied()
        .ok_or(EntryLaunchError::EntryFactoryNotCertified)?;
    let identity = launch.descriptor.artifact_identity();
    let mut artifacts = plan.artifacts.iter().filter(|artifact| {
        artifact.role == RuntimeArtifactRole::EntryCode && artifact.artifact_identity == identity
    });
    let entry_artifact = *artifacts
        .next()
        .ok_or(EntryLaunchError::EntryArtifactNotFound)?;
    if artifacts.next().is_some() {
        return Err(EntryLaunchError::EntryArtifactAmbiguous);
    }
    Ok(CertifiedEntryLaunch {
        launch,
        factory,
        entry_artifact,
        plan_identity: plan.plan_identity,
        plan_generation: plan.plan_generation,
    })
}

pub fn scan_module_entry_factories(
    module: &vo_vm::bytecode::Module,
) -> Result<Vec<crate::CertifiedEntryFactory>, EntryFactoryMetadataError> {
    const MARKER: &str = "__vo_entry_meta_v1_";
    let mut factories = Vec::new();
    for (function_id, function) in module.functions.iter().enumerate() {
        let Some(marker_offset) = function.name.find(MARKER) else {
            continue;
        };
        let marker = &function.name[marker_offset + MARKER.len()..];
        let marker = marker
            .split(['$', '·'])
            .next()
            .ok_or(EntryFactoryMetadataError::MalformedMarker)?;
        let fields = marker.split('_').collect::<Vec<_>>();
        if fields.len() != 5 {
            return Err(EntryFactoryMetadataError::MalformedMarker);
        }
        let framework = match fields[0] {
            "vogui" => EntryFramework::Vogui,
            "voplay" => EntryFramework::Voplay,
            _ => return Err(EntryFactoryMetadataError::UnknownFramework),
        };
        let factory_id = fields[1]
            .parse::<u64>()
            .ok()
            .filter(|id| *id != 0)
            .ok_or(EntryFactoryMetadataError::InvalidFactoryId)?;
        factories.push(crate::CertifiedEntryFactory {
            framework,
            factory_id,
            function_id: u32::try_from(function_id)
                .map_err(|_| EntryFactoryMetadataError::InvalidFactoryId)?,
            artifact_identity: parse_hex_digest(fields[2])?,
            binding_fingerprint: parse_hex_digest(fields[3])?,
            role_artifact_set_fingerprint: parse_hex_digest(fields[4])?,
        });
    }
    Ok(factories)
}

fn decode_vogui_descriptor(bytes: &[u8]) -> Result<VoguiEntryDescriptor, EntryLaunchError> {
    if bytes.len() != VOGUI_DESCRIPTOR_BYTES {
        return Err(EntryLaunchError::DescriptorLength);
    }
    let descriptor = VoguiEntryDescriptor {
        artifact_identity: read_digest(bytes, 0)?,
        factory_id: read_u64(bytes, 32)?,
        app_build_identity: read_digest(bytes, 40)?,
        model_fingerprint: read_digest(bytes, 72)?,
        message_fingerprint: read_digest(bytes, 104)?,
        role_artifact_set_fingerprint: read_digest(bytes, 136)?,
        transaction_mode: read_u32(bytes, 168)?,
    };
    if descriptor.factory_id == 0
        || is_zero_digest(descriptor.artifact_identity)
        || is_zero_digest(descriptor.app_build_identity)
        || is_zero_digest(descriptor.model_fingerprint)
        || is_zero_digest(descriptor.message_fingerprint)
        || is_zero_digest(descriptor.role_artifact_set_fingerprint)
    {
        return Err(EntryLaunchError::InvalidDescriptor);
    }
    if descriptor.transaction_mode > 2 {
        return Err(EntryLaunchError::InvalidTransactionMode);
    }
    Ok(descriptor)
}

fn decode_voplay_descriptor(bytes: &[u8]) -> Result<VoplayEntryDescriptor, EntryLaunchError> {
    if bytes.len() != VOPLAY_DESCRIPTOR_BYTES {
        return Err(EntryLaunchError::DescriptorLength);
    }
    let descriptor = VoplayEntryDescriptor {
        artifact_identity: read_digest(bytes, 0)?,
        factory_id: read_u64(bytes, 32)?,
        schema_fingerprint: read_digest(bytes, 40)?,
        role_artifact_set_fingerprint: read_digest(bytes, 72)?,
    };
    if descriptor.factory_id == 0
        || is_zero_digest(descriptor.artifact_identity)
        || is_zero_digest(descriptor.schema_fingerprint)
        || is_zero_digest(descriptor.role_artifact_set_fingerprint)
    {
        return Err(EntryLaunchError::InvalidDescriptor);
    }
    Ok(descriptor)
}

fn read_digest(bytes: &[u8], offset: usize) -> Result<[u8; 32], EntryLaunchError> {
    bytes
        .get(offset..offset + 32)
        .and_then(|value| value.try_into().ok())
        .ok_or(EntryLaunchError::DescriptorLength)
}

fn read_u32(bytes: &[u8], offset: usize) -> Result<u32, EntryLaunchError> {
    bytes
        .get(offset..offset + 4)
        .and_then(|value| value.try_into().ok())
        .map(u32::from_le_bytes)
        .ok_or(EntryLaunchError::MalformedEnvelope)
}

fn read_u64(bytes: &[u8], offset: usize) -> Result<u64, EntryLaunchError> {
    bytes
        .get(offset..offset + 8)
        .and_then(|value| value.try_into().ok())
        .map(u64::from_le_bytes)
        .ok_or(EntryLaunchError::DescriptorLength)
}

fn is_zero_digest(digest: [u8; 32]) -> bool {
    digest.iter().all(|byte| *byte == 0)
}

fn parse_hex_digest(value: &str) -> Result<[u8; 32], EntryFactoryMetadataError> {
    if value.len() != 64 {
        return Err(EntryFactoryMetadataError::InvalidDigest);
    }
    let mut digest = [0u8; 32];
    for (index, pair) in value.as_bytes().chunks_exact(2).enumerate() {
        digest[index] = (hex_nibble(pair[0])? << 4) | hex_nibble(pair[1])?;
    }
    if is_zero_digest(digest) {
        return Err(EntryFactoryMetadataError::InvalidDigest);
    }
    Ok(digest)
}

fn hex_nibble(value: u8) -> Result<u8, EntryFactoryMetadataError> {
    match value {
        b'0'..=b'9' => Ok(value - b'0'),
        b'a'..=b'f' => Ok(value - b'a' + 10),
        b'A'..=b'F' => Ok(value - b'A' + 10),
        _ => Err(EntryFactoryMetadataError::InvalidDigest),
    }
}

fn stable_capability_id(name: &[u8]) -> CapabilityId {
    let mut hash = 0xcbf2_9ce4_8422_2325_u64;
    for &byte in name {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    CapabilityId(hash)
}
