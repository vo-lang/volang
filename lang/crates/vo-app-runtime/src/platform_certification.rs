use alloc::collections::{BTreeMap, BTreeSet};
use alloc::string::String;
use alloc::vec::Vec;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum PlatformCapability {
    Window,
    Input,
    Present,
    Recovery,
    Close,
    Gpu,
    Ime,
    Accessibility,
    Audio,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum PlatformWorkload {
    Ui,
    Voplay,
    Audio,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DeclaredPlatformTarget {
    pub target: String,
    pub runtime_target: crate::RuntimeTarget,
    pub topology: crate::HostTopology,
    pub workloads: BTreeSet<PlatformWorkload>,
    pub certification_digest: [u8; 32],
    pub required: BTreeSet<PlatformCapability>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlatformCapabilityStatus {
    Passed,
    Failed,
    Unavailable,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlatformCapabilityObservation {
    pub capability: PlatformCapability,
    pub status: PlatformCapabilityStatus,
    pub evidence: String,
    pub evidence_digest: [u8; 32],
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlatformCertificationReport {
    pub format: u16,
    pub target: String,
    pub runtime_target: crate::RuntimeTarget,
    pub topology: crate::HostTopology,
    pub workloads: BTreeSet<PlatformWorkload>,
    pub certification_digest: [u8; 32],
    pub observations: BTreeMap<PlatformCapability, PlatformCapabilityObservation>,
    pub missing: Vec<PlatformCapability>,
    pub failed: Vec<PlatformCapability>,
    pub unavailable: Vec<PlatformCapability>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CertifiedPlatformProbe {
    pub report: PlatformCertificationReport,
    pub probe: crate::TrustedHostProbe,
}

impl PlatformCertificationReport {
    pub fn passed(&self) -> bool {
        self.missing.is_empty() && self.failed.is_empty() && self.unavailable.is_empty()
    }

    pub fn require_passed(&self) -> Result<(), PlatformCertificationError> {
        if self.passed() {
            Ok(())
        } else {
            Err(PlatformCertificationError::CertificationFailed {
                missing: self.missing.clone(),
                failed: self.failed.clone(),
                unavailable: self.unavailable.clone(),
            })
        }
    }

    pub fn trusted_probe(
        &self,
        selected_variant_identity: [u8; 32],
        probe_evidence_digest: [u8; 32],
    ) -> Result<crate::TrustedHostProbe, PlatformCertificationError> {
        self.require_passed()?;
        if selected_variant_identity.iter().all(|byte| *byte == 0) {
            return Err(PlatformCertificationError::InvalidVariantIdentity);
        }
        if probe_evidence_digest.iter().all(|byte| *byte == 0) {
            return Err(PlatformCertificationError::InvalidProbeEvidenceDigest);
        }
        Ok(crate::TrustedHostProbe {
            target: self.runtime_target,
            topology: self.topology,
            selected_variant_identity,
            available: required_host_probes(self.topology, &self.workloads),
            platform_certification_digest: self.certification_digest,
            probe_evidence_digest,
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PlatformCertificationError {
    InvalidTarget,
    InvalidTargetTopology,
    InvalidCertificationDigest,
    InvalidVariantIdentity,
    InvalidProbeEvidenceDigest,
    VariantMismatch,
    ProbeRequirementMismatch,
    EmptyRequirements,
    InvalidRequirements {
        missing: Vec<PlatformCapability>,
        unexpected: Vec<PlatformCapability>,
    },
    InvalidEvidence(PlatformCapability),
    DuplicateObservation(PlatformCapability),
    UnexpectedObservation(PlatformCapability),
    CertificationFailed {
        missing: Vec<PlatformCapability>,
        failed: Vec<PlatformCapability>,
        unavailable: Vec<PlatformCapability>,
    },
}

pub fn certify_platform_target(
    declaration: &DeclaredPlatformTarget,
    observations: Vec<PlatformCapabilityObservation>,
) -> Result<PlatformCertificationReport, PlatformCertificationError> {
    if declaration.target.is_empty()
        || declaration.target.len() > 256
        || !declaration
            .target
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'.' | b'_' | b'-'))
    {
        return Err(PlatformCertificationError::InvalidTarget);
    }
    if declaration
        .certification_digest
        .iter()
        .all(|byte| *byte == 0)
    {
        return Err(PlatformCertificationError::InvalidCertificationDigest);
    }
    if !valid_target_topology(declaration.runtime_target, declaration.topology) {
        return Err(PlatformCertificationError::InvalidTargetTopology);
    }
    if declaration.required.is_empty() {
        return Err(PlatformCertificationError::EmptyRequirements);
    }
    let canonical = required_platform_capabilities(declaration.topology, &declaration.workloads);
    if declaration.required != canonical {
        return Err(PlatformCertificationError::InvalidRequirements {
            missing: canonical
                .difference(&declaration.required)
                .copied()
                .collect(),
            unexpected: declaration
                .required
                .difference(&canonical)
                .copied()
                .collect(),
        });
    }
    let mut by_capability = BTreeMap::new();
    for observation in observations {
        if !declaration.required.contains(&observation.capability) {
            return Err(PlatformCertificationError::UnexpectedObservation(
                observation.capability,
            ));
        }
        if observation.evidence.is_empty()
            || observation.evidence.len() > 4096
            || observation.evidence_digest.iter().all(|byte| *byte == 0)
        {
            return Err(PlatformCertificationError::InvalidEvidence(
                observation.capability,
            ));
        }
        let capability = observation.capability;
        if by_capability.insert(capability, observation).is_some() {
            return Err(PlatformCertificationError::DuplicateObservation(capability));
        }
    }
    let missing = declaration
        .required
        .iter()
        .filter(|capability| !by_capability.contains_key(capability))
        .copied()
        .collect();
    let failed = by_capability
        .iter()
        .filter_map(|(capability, observation)| {
            (observation.status == PlatformCapabilityStatus::Failed).then_some(*capability)
        })
        .collect();
    let unavailable = by_capability
        .iter()
        .filter_map(|(capability, observation)| {
            (observation.status == PlatformCapabilityStatus::Unavailable).then_some(*capability)
        })
        .collect();
    Ok(PlatformCertificationReport {
        format: 2,
        target: declaration.target.clone(),
        runtime_target: declaration.runtime_target,
        topology: declaration.topology,
        workloads: declaration.workloads.clone(),
        certification_digest: declaration.certification_digest,
        observations: by_capability,
        missing,
        failed,
        unavailable,
    })
}

pub fn certify_runtime_variant(
    declaration: &DeclaredPlatformTarget,
    observations: Vec<PlatformCapabilityObservation>,
    variant: &crate::CertifiedAppRuntimeVariant,
    probe_evidence_digest: [u8; 32],
) -> Result<CertifiedPlatformProbe, PlatformCertificationError> {
    let report = certify_platform_target(declaration, observations)?;
    report.require_passed()?;
    if variant.target != declaration.runtime_target
        || variant.topology != declaration.topology
        || variant.platform_certification_digest != declaration.certification_digest
    {
        return Err(PlatformCertificationError::VariantMismatch);
    }
    if variant.required_probes != required_host_probes(declaration.topology, &declaration.workloads)
    {
        return Err(PlatformCertificationError::ProbeRequirementMismatch);
    }
    let probe = report.trusted_probe(variant.variant_identity, probe_evidence_digest)?;
    Ok(CertifiedPlatformProbe { report, probe })
}

fn valid_target_topology(target: crate::RuntimeTarget, topology: crate::HostTopology) -> bool {
    matches!(
        (target, topology),
        (
            crate::RuntimeTarget::BrowserWasm,
            crate::HostTopology::BrowserMain
        ) | (
            crate::RuntimeTarget::NativeMacOs
                | crate::RuntimeTarget::NativeLinux
                | crate::RuntimeTarget::NativeWindows,
            crate::HostTopology::WebviewNativeHost | crate::HostTopology::GpuNativeHost
        ) | (
            crate::RuntimeTarget::Headless,
            crate::HostTopology::Headless
        ) | (crate::RuntimeTarget::Manual, crate::HostTopology::Manual)
    )
}

pub fn required_host_probes(
    topology: crate::HostTopology,
    workloads: &BTreeSet<PlatformWorkload>,
) -> crate::HostProbeRequirements {
    let mut bits = 0_u64;
    match topology {
        crate::HostTopology::BrowserMain => {
            if workloads.contains(&PlatformWorkload::Voplay) {
                bits |= crate::HostProbeRequirements::WEB_GPU.0;
                bits |= crate::HostProbeRequirements::OFFSCREEN_CANVAS.0;
            }
        }
        crate::HostTopology::WebviewNativeHost => {
            bits |= crate::HostProbeRequirements::WEBVIEW_PROCESS.0;
            if workloads.contains(&PlatformWorkload::Voplay) {
                bits |= crate::HostProbeRequirements::WEB_GPU.0;
            }
        }
        crate::HostTopology::GpuNativeHost => {
            bits |= crate::HostProbeRequirements::NATIVE_GPU_SURFACE.0;
        }
        crate::HostTopology::Headless | crate::HostTopology::Manual => {}
    }
    if workloads.contains(&PlatformWorkload::Ui) {
        bits |= crate::HostProbeRequirements::ACCESSIBILITY_BRIDGE.0;
    }
    if workloads.contains(&PlatformWorkload::Audio) {
        bits |= crate::HostProbeRequirements::AUDIO_DEVICE.0;
    }
    crate::HostProbeRequirements(bits)
}

pub fn required_platform_capabilities(
    topology: crate::HostTopology,
    workloads: &BTreeSet<PlatformWorkload>,
) -> BTreeSet<PlatformCapability> {
    let mut required = match topology {
        crate::HostTopology::Headless => {
            BTreeSet::from([PlatformCapability::Recovery, PlatformCapability::Close])
        }
        crate::HostTopology::BrowserMain
        | crate::HostTopology::WebviewNativeHost
        | crate::HostTopology::GpuNativeHost => BTreeSet::from([
            PlatformCapability::Window,
            PlatformCapability::Input,
            PlatformCapability::Present,
            PlatformCapability::Recovery,
            PlatformCapability::Close,
        ]),
        crate::HostTopology::Manual => BTreeSet::new(),
    };
    if workloads.contains(&PlatformWorkload::Ui) {
        required.extend([PlatformCapability::Ime, PlatformCapability::Accessibility]);
    }
    if workloads.contains(&PlatformWorkload::Voplay) {
        required.insert(PlatformCapability::Gpu);
    }
    if workloads.contains(&PlatformWorkload::Audio) {
        required.insert(PlatformCapability::Audio);
    }
    required
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;

    fn declaration() -> DeclaredPlatformTarget {
        DeclaredPlatformTarget {
            target: String::from("aarch64-apple-darwin.gpu-native-host"),
            runtime_target: crate::RuntimeTarget::NativeMacOs,
            topology: crate::HostTopology::GpuNativeHost,
            workloads: BTreeSet::from([
                PlatformWorkload::Ui,
                PlatformWorkload::Voplay,
                PlatformWorkload::Audio,
            ]),
            certification_digest: [7; 32],
            required: BTreeSet::from([
                PlatformCapability::Window,
                PlatformCapability::Input,
                PlatformCapability::Present,
                PlatformCapability::Recovery,
                PlatformCapability::Close,
                PlatformCapability::Gpu,
                PlatformCapability::Ime,
                PlatformCapability::Accessibility,
                PlatformCapability::Audio,
            ]),
        }
    }

    #[test]
    fn report_preserves_explicit_missing_and_failed_capabilities() {
        let report = certify_platform_target(
            &declaration(),
            vec![
                PlatformCapabilityObservation {
                    capability: PlatformCapability::Window,
                    status: PlatformCapabilityStatus::Passed,
                    evidence: String::from("window-smoke.json"),
                    evidence_digest: [1; 32],
                },
                PlatformCapabilityObservation {
                    capability: PlatformCapability::Input,
                    status: PlatformCapabilityStatus::Failed,
                    evidence: String::from("input-smoke.json"),
                    evidence_digest: [2; 32],
                },
            ],
        )
        .unwrap();

        let missing = vec![
            PlatformCapability::Present,
            PlatformCapability::Recovery,
            PlatformCapability::Close,
            PlatformCapability::Gpu,
            PlatformCapability::Ime,
            PlatformCapability::Accessibility,
            PlatformCapability::Audio,
        ];
        assert_eq!(report.missing, missing);
        assert_eq!(report.failed, vec![PlatformCapability::Input]);
        assert!(report.unavailable.is_empty());
        assert!(!report.passed());
        assert_eq!(
            report.require_passed(),
            Err(PlatformCertificationError::CertificationFailed {
                missing,
                failed: vec![PlatformCapability::Input],
                unavailable: vec![],
            })
        );
    }

    #[test]
    fn observations_are_exact_unique_and_evidence_bound() {
        let duplicate = PlatformCapabilityObservation {
            capability: PlatformCapability::Window,
            status: PlatformCapabilityStatus::Passed,
            evidence: String::from("window-smoke.json"),
            evidence_digest: [1; 32],
        };
        assert_eq!(
            certify_platform_target(&declaration(), vec![duplicate.clone(), duplicate]),
            Err(PlatformCertificationError::DuplicateObservation(
                PlatformCapability::Window
            ))
        );
        let mut declaration_without_voplay = declaration();
        declaration_without_voplay
            .workloads
            .remove(&PlatformWorkload::Voplay);
        declaration_without_voplay
            .required
            .remove(&PlatformCapability::Gpu);
        assert_eq!(
            certify_platform_target(
                &declaration_without_voplay,
                vec![PlatformCapabilityObservation {
                    capability: PlatformCapability::Gpu,
                    status: PlatformCapabilityStatus::Passed,
                    evidence: String::from("gpu-smoke.json"),
                    evidence_digest: [3; 32],
                }]
            ),
            Err(PlatformCertificationError::UnexpectedObservation(
                PlatformCapability::Gpu
            ))
        );
        assert_eq!(
            certify_platform_target(
                &declaration(),
                vec![PlatformCapabilityObservation {
                    capability: PlatformCapability::Window,
                    status: PlatformCapabilityStatus::Passed,
                    evidence: String::new(),
                    evidence_digest: [4; 32],
                }]
            ),
            Err(PlatformCertificationError::InvalidEvidence(
                PlatformCapability::Window
            ))
        );
    }
}
