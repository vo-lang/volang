//! Canonical target descriptions and target-specific module validation.
//!
//! Source analysis and bytecode generation remain target independent. Build
//! backends consume a [`TargetSpec`] only after common bytecode verification,
//! which keeps one verified module portable across VM, JIT, Native AOT, and
//! WebAssembly AOT.

use core::fmt;
use core::str::FromStr;

use target_lexicon::{Architecture, BinaryFormat as LexiconBinaryFormat, Triple};
use vo_common_core::{Module, RuntimeType};

pub const WASM32_UNKNOWN_UNKNOWN: &str = "wasm32-unknown-unknown";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TargetFamily {
    Native,
    WebAssembly,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HostSurface {
    Native,
    BareWasm,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Endianness {
    Little,
    Big,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PointerWidth {
    U32,
    U64,
}

impl PointerWidth {
    #[inline]
    pub const fn bits(self) -> u8 {
        match self {
            Self::U32 => 32,
            Self::U64 => 64,
        }
    }

    #[inline]
    pub const fn bytes(self) -> u8 {
        self.bits() / 8
    }

    #[inline]
    pub const fn max_address(self) -> u64 {
        match self {
            Self::U32 => u32::MAX as u64,
            Self::U64 => u64::MAX,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ObjectFormat {
    Elf,
    Coff,
    MachO,
    Wasm,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArtifactKind {
    Executable,
    StaticLibrary,
    DynamicLibrary,
    Web,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct WasmFeatureSet(u32);

impl WasmFeatureSet {
    pub const MUTABLE_GLOBALS: Self = Self(1 << 0);
    pub const MULTI_VALUE: Self = Self(1 << 1);
    pub const REFERENCE_TYPES: Self = Self(1 << 2);
    pub const SIGN_EXTENSION: Self = Self(1 << 3);
    pub const BULK_MEMORY: Self = Self(1 << 4);

    /// Stable Volang WebAssembly baseline. Adding a feature is a target-contract
    /// change and must update build identities and the cross-engine matrix.
    pub const VOLANG_BASELINE: Self = Self(
        Self::MUTABLE_GLOBALS.0
            | Self::MULTI_VALUE.0
            | Self::REFERENCE_TYPES.0
            | Self::SIGN_EXTENSION.0
            | Self::BULK_MEMORY.0,
    );

    #[inline]
    pub const fn contains(self, feature: Self) -> bool {
        self.0 & feature.0 == feature.0
    }

    #[inline]
    pub const fn bits(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TargetSpec {
    triple: String,
    family: TargetFamily,
    host_surface: HostSurface,
    pointer_width: PointerWidth,
    endianness: Endianness,
    object_format: ObjectFormat,
    wasm_features: WasmFeatureSet,
}

impl TargetSpec {
    pub fn host() -> Result<Self, TargetSpecError> {
        Self::parse(env!("VO_HOST_TARGET_TRIPLE"))
    }

    pub fn parse(value: &str) -> Result<Self, TargetSpecError> {
        let triple = Triple::from_str(value)
            .map_err(|error| TargetSpecError::invalid(value, error.to_string()))?;
        let canonical = triple.to_string();
        if canonical != value {
            return Err(TargetSpecError::invalid(
                value,
                format!("target spelling is not canonical; use {canonical}"),
            ));
        }

        let family = match triple.architecture {
            Architecture::Wasm32 | Architecture::Wasm64 => TargetFamily::WebAssembly,
            Architecture::Unknown => {
                return Err(TargetSpecError::unsupported(value, "unknown architecture"));
            }
            _ => TargetFamily::Native,
        };
        let pointer_width = match triple.pointer_width().map(|width| width.bits()) {
            Ok(32) => PointerWidth::U32,
            Ok(64) => PointerWidth::U64,
            Ok(bits) => {
                return Err(TargetSpecError::unsupported(
                    value,
                    format!("{bits}-bit pointers"),
                ));
            }
            Err(()) => {
                return Err(TargetSpecError::unsupported(value, "unknown pointer width"));
            }
        };
        let endianness = match triple.endianness() {
            Ok(target_lexicon::Endianness::Little) => Endianness::Little,
            Ok(target_lexicon::Endianness::Big) => Endianness::Big,
            Err(()) => {
                return Err(TargetSpecError::unsupported(value, "unknown endianness"));
            }
        };
        let object_format = match triple.binary_format {
            LexiconBinaryFormat::Elf => ObjectFormat::Elf,
            LexiconBinaryFormat::Coff => ObjectFormat::Coff,
            LexiconBinaryFormat::Macho => ObjectFormat::MachO,
            LexiconBinaryFormat::Wasm => ObjectFormat::Wasm,
            other => {
                return Err(TargetSpecError::unsupported(
                    value,
                    format!("{} object format", other.into_str()),
                ));
            }
        };
        let host_surface = match family {
            TargetFamily::Native => HostSurface::Native,
            TargetFamily::WebAssembly => match value {
                WASM32_UNKNOWN_UNKNOWN => HostSurface::BareWasm,
                _ => {
                    return Err(TargetSpecError::unsupported(
                        value,
                        "WebAssembly host surface",
                    ));
                }
            },
        };

        Ok(Self {
            triple: value.to_string(),
            family,
            host_surface,
            pointer_width,
            endianness,
            object_format,
            wasm_features: if family == TargetFamily::WebAssembly {
                WasmFeatureSet::VOLANG_BASELINE
            } else {
                WasmFeatureSet::default()
            },
        })
    }

    #[inline]
    pub fn triple(&self) -> &str {
        &self.triple
    }

    #[inline]
    pub const fn family(&self) -> TargetFamily {
        self.family
    }

    #[inline]
    pub const fn host_surface(&self) -> HostSurface {
        self.host_surface
    }

    #[inline]
    pub const fn pointer_width(&self) -> PointerWidth {
        self.pointer_width
    }

    #[inline]
    pub const fn endianness(&self) -> Endianness {
        self.endianness
    }

    #[inline]
    pub const fn object_format(&self) -> ObjectFormat {
        self.object_format
    }

    #[inline]
    pub const fn wasm_features(&self) -> WasmFeatureSet {
        self.wasm_features
    }

    pub const fn supports_artifact(&self, kind: ArtifactKind) -> bool {
        match (self.host_surface, kind) {
            (HostSurface::Native, ArtifactKind::Executable)
                if matches!(self.pointer_width, PointerWidth::U64)
                    && matches!(self.endianness, Endianness::Little) =>
            {
                true
            }
            (HostSurface::BareWasm, ArtifactKind::Web) => true,
            _ => false,
        }
    }
}

impl fmt::Display for TargetSpec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.triple())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TargetSpecError {
    target: String,
    detail: String,
}

impl TargetSpecError {
    fn invalid(target: &str, detail: impl Into<String>) -> Self {
        Self {
            target: target.to_string(),
            detail: format!("invalid target: {}", detail.into()),
        }
    }

    fn unsupported(target: &str, detail: impl Into<String>) -> Self {
        Self {
            target: target.to_string(),
            detail: format!("unsupported target capability: {}", detail.into()),
        }
    }

    #[inline]
    pub fn target(&self) -> &str {
        &self.target
    }

    #[inline]
    pub fn detail(&self) -> &str {
        &self.detail
    }
}

impl fmt::Display for TargetSpecError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "target {}: {}", self.target, self.detail)
    }
}

impl std::error::Error for TargetSpecError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TargetVerificationError {
    target: String,
    item: String,
    detail: String,
}

impl TargetVerificationError {
    fn new(target: &TargetSpec, item: impl Into<String>, detail: impl Into<String>) -> Self {
        Self {
            target: target.triple().to_string(),
            item: item.into(),
            detail: detail.into(),
        }
    }

    #[inline]
    pub fn target(&self) -> &str {
        &self.target
    }

    #[inline]
    pub fn item(&self) -> &str {
        &self.item
    }

    #[inline]
    pub fn detail(&self) -> &str {
        &self.detail
    }
}

impl fmt::Display for TargetVerificationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "target {} rejects {}: {}",
            self.target, self.item, self.detail
        )
    }
}

impl std::error::Error for TargetVerificationError {}

/// Validate target-address and target-surface invariants after the common
/// verifier has accepted the module.
pub fn verify_module_for_target(
    module: &Module,
    target: &TargetSpec,
) -> Result<(), TargetVerificationError> {
    let max_address = target.pointer_width().max_address();
    for (index, runtime_type) in module.runtime_types.iter().enumerate() {
        if let RuntimeType::Array { len, .. } = runtime_type {
            if *len > max_address {
                return Err(TargetVerificationError::new(
                    target,
                    format!("runtime_types[{index}]"),
                    format!(
                        "target limit: array length {len} exceeds the {}-bit target address width",
                        target.pointer_width().bits()
                    ),
                ));
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_common_core::{types::ValueRttid, RuntimeType, ValueKind};

    #[test]
    fn parses_canonical_native_target() {
        let target = TargetSpec::parse("x86_64-unknown-linux-gnu").unwrap();
        assert_eq!(target.family(), TargetFamily::Native);
        assert_eq!(target.pointer_width(), PointerWidth::U64);
        assert_eq!(target.object_format(), ObjectFormat::Elf);
        assert!(target.supports_artifact(ArtifactKind::Executable));
        assert!(!target.supports_artifact(ArtifactKind::Web));
    }

    #[test]
    fn parses_published_core_wasm_surface() {
        let bare = TargetSpec::parse(WASM32_UNKNOWN_UNKNOWN).unwrap();
        assert_eq!(bare.host_surface(), HostSurface::BareWasm);
        assert!(bare.supports_artifact(ArtifactKind::Web));
    }

    #[test]
    fn rejects_noncanonical_or_unknown_targets() {
        assert!(TargetSpec::parse("x86_64-linux").is_err());
        assert!(TargetSpec::parse("unknown-unknown-unknown").is_err());
        assert!(TargetSpec::parse("wasm32-unknown-emscripten").is_err());
        assert!(TargetSpec::parse("wasm32-wasip1").is_err());
    }

    #[test]
    fn wasm_baseline_is_explicit() {
        let features = TargetSpec::parse(WASM32_UNKNOWN_UNKNOWN)
            .unwrap()
            .wasm_features();
        assert!(features.contains(WasmFeatureSet::MUTABLE_GLOBALS));
        assert!(features.contains(WasmFeatureSet::BULK_MEMORY));
        assert!(features.contains(WasmFeatureSet::MULTI_VALUE));
    }

    #[test]
    fn target_verifier_rejects_arrays_outside_wasm32_address_domain() {
        let mut module = Module::new("target-test".to_string());
        module.runtime_types.push(RuntimeType::Array {
            len: u32::MAX as u64 + 1,
            elem: ValueRttid::new(0, ValueKind::Uint8),
        });
        let target = TargetSpec::parse(WASM32_UNKNOWN_UNKNOWN).unwrap();
        let error = verify_module_for_target(&module, &target).unwrap_err();
        assert!(error.to_string().contains("32-bit target address width"));
    }

    #[test]
    fn target_verifier_accepts_same_array_on_64_bit_target() {
        let mut module = Module::new("target-test".to_string());
        module.runtime_types.push(RuntimeType::Array {
            len: u32::MAX as u64 + 1,
            elem: ValueRttid::new(0, ValueKind::Uint8),
        });
        let target = TargetSpec::parse("x86_64-unknown-linux-gnu").unwrap();
        verify_module_for_target(&module, &target).unwrap();
    }
}
