use std::collections::BTreeMap;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use sha2::{Digest, Sha256};
use vo_app_runtime::{
    capability_id, default_hosted_session_limits, AppBuildPlan, CertifiedAppRuntimeVariant,
    HostProbeRequirements, HostTopology, InitialInstanceGroupPlan, InitialProviderInstancePlan,
    IsolationClass, LoadedProviderFactory, MaterializedRuntimeArtifact,
    OptionalProviderDisablePolicy, PlacementDomain, ProviderCatalogEntry,
    ProviderDeferredActivationPolicy, ProviderDependencySet, ProviderFactoryManifest,
    ProviderFactoryRequirement, ProviderLoaderKind, ProviderRestartPolicy, ProviderRole,
    ProviderTemplate, ProviderTrustEvidence, ProviderTrustPolicy, ResolvedAppRuntimePlan,
    RuntimeArtifactRole, RuntimeTarget, StaticInitializerPolicy, TerminalFailureScope,
    TrustedHostProbe, APP_PROTOCOL_EXACT_FINGERPRINT, CAPABILITY_APP_TIMER_ONCE,
    HOST_SERVICES_V2_LAYOUT_FINGERPRINT, MAX_RUNTIME_PLAN_ARTIFACTS,
};
use vo_module::profile::{ArtifactRole, CapabilitySet};
use vo_module::schema::lockfile::LockedModule;
use vo_runtime::ext_loader::NativeExtensionSpec;
use vo_web::{MaterializedBrowserArtifact, MaterializedBrowserArtifactRole};

const SESSION_VM_TEMPLATE_ID: u32 = 1;
const SESSION_VM_FACTORY_ID: u32 = 1;
const MAX_HOST_EXECUTABLE_BYTES: u64 = 1024 * 1024 * 1024;

#[derive(Clone)]
pub(crate) struct FrameworkProviderBinding {
    pub capabilities: Vec<vo_app_runtime::CapabilityId>,
    pub providers: Vec<FrameworkProviderTemplateBinding>,
}

#[derive(Clone, Copy)]
pub(crate) struct FrameworkProviderTemplateBinding {
    pub template_id: u32,
    pub loaded: LoadedProviderFactory,
}

pub(crate) fn framework_provider_bindings(
    runtime: &vo_web::BrowserRuntimePlan,
    plan: &ResolvedAppRuntimePlan,
) -> Result<BTreeMap<String, FrameworkProviderBinding>, String> {
    let mut frameworks = runtime.graph.frameworks.iter().collect::<Vec<_>>();
    frameworks.sort_by(|left, right| {
        left.module_key
            .as_bytes()
            .cmp(right.module_key.as_bytes())
            .then_with(|| {
                left.id
                    .extension_name
                    .as_bytes()
                    .cmp(right.id.extension_name.as_bytes())
            })
    });
    frameworks
        .into_iter()
        .map(|framework| {
            if framework.contract.provider_role.is_none() {
                return Err(format!(
                    "framework {} is missing its provider role",
                    framework.module_key
                ));
            }
            let mut providers = plan
                .providers
                .iter()
                .filter(|provider| {
                    provider.template.role != ProviderRole::SessionVm
                        && provider.template.factory.factory_id
                            == stable_provider_factory_id(
                                &framework.module_key,
                                provider.template.role,
                            )
                })
                .collect::<Vec<_>>();
            providers.sort_by_key(|provider| provider.template.template_id);
            if providers.is_empty() {
                return Err(format!(
                    "framework {} provider role set is absent from resolved plan",
                    framework.module_key
                ));
            }
            let mut capabilities = framework
                .contract
                .capabilities
                .iter()
                .map(|capability| capability_id(capability.as_bytes()))
                .collect::<Vec<_>>();
            capabilities.sort();
            capabilities.dedup();
            Ok((
                framework.module_key.clone(),
                FrameworkProviderBinding {
                    capabilities,
                    providers: providers
                        .into_iter()
                        .map(|provider| FrameworkProviderTemplateBinding {
                            template_id: provider.template.template_id,
                            loaded: LoadedProviderFactory {
                                factory_id: provider.template.factory.factory_id,
                                artifact_digest: provider.template.factory.artifact_digest,
                                role: provider.template.role,
                                abi_fingerprint: provider.template.factory.abi_fingerprint,
                                schema_fingerprint: provider.template.factory.schema_fingerprint,
                            },
                        })
                        .collect(),
                },
            ))
        })
        .collect()
}

pub(crate) fn materialize_native_studio_plan(
    module_bytes: &[u8],
    runtime: &vo_web::BrowserRuntimePlan,
    browser_artifacts: &[MaterializedBrowserArtifact],
    native_extensions: &[NativeExtensionSpec],
    locked_modules: &[LockedModule],
    plan_generation: u64,
    available_host_probes: HostProbeRequirements,
) -> Result<ResolvedAppRuntimePlan, String> {
    if module_bytes.is_empty() || plan_generation == 0 {
        return Err(String::from(
            "native Studio AppBuildPlan requires entry bytes and a non-zero generation",
        ));
    }
    if browser_artifacts
        .len()
        .saturating_add(runtime.graph.frameworks.len())
        > MAX_RUNTIME_PLAN_ARTIFACTS.saturating_sub(3)
    {
        return Err(format!(
            "native Studio materialized {} browser artifacts, exceeding the AppBuildPlan budget",
            browser_artifacts.len()
        ));
    }
    let target = native_runtime_target();
    let topology = HostTopology::WebviewNativeHost;
    let required_probes = HostProbeRequirements::WEBVIEW_PROCESS;
    if !available_host_probes.contains(required_probes) {
        return Err(String::from(
            "native Studio planned startup requires an observed live WebView process",
        ));
    }
    let host_path = std::env::current_exe()
        .map_err(|error| format!("resolve native Studio host executable: {error}"))?;
    let host_digest = sha256_file(&host_path)?;
    let entry_digest = sha256_bytes(module_bytes);
    let entry_module = vo_engine::Module::deserialize(module_bytes)
        .map_err(|error| format!("decode native Studio entry metadata: {error:?}"))?;
    let entry_factories = vo_app_runtime::scan_module_entry_factories(&entry_module)
        .map_err(|error| format!("scan native Studio entry metadata: {error:?}"))?;
    let entry_schema_record = canonical_record(
        b"vo.studio.entry-schema.v1",
        &[&entry_digest, &APP_PROTOCOL_EXACT_FINGERPRINT],
    )?;
    let entry_schema_fingerprint = sha256_bytes(&entry_schema_record);

    let mut capabilities = vec![
        capability_id(CAPABILITY_APP_TIMER_ONCE.as_bytes()),
        capability_id(b"render_island_host"),
    ];
    for framework in &runtime.graph.frameworks {
        capabilities.extend(
            framework
                .contract
                .capabilities
                .iter()
                .map(|capability| capability_id(capability.as_bytes())),
        );
    }
    capabilities.sort();
    capabilities.dedup();
    let capability_digest = digest_capabilities(&capabilities);
    let abi_fingerprint = sha256_parts(
        b"vo.session-vm.host-services-v2.v1",
        &[
            &HOST_SERVICES_V2_LAYOUT_FINGERPRINT.to_le_bytes(),
            &APP_PROTOCOL_EXACT_FINGERPRINT,
        ],
    );
    let factory = ProviderFactoryRequirement {
        factory_id: SESSION_VM_FACTORY_ID,
        artifact_digest: host_digest,
        abi_fingerprint,
        schema_fingerprint: APP_PROTOCOL_EXACT_FINGERPRINT,
        capability_digest,
        loader: ProviderLoaderKind::BuiltInStatic,
    };
    let template = ProviderTemplate {
        template_id: SESSION_VM_TEMPLATE_ID,
        role: ProviderRole::SessionVm,
        placement: PlacementDomain::HostedActor,
        isolation: IsolationClass::CooperativeInProcess,
        failure_scope: TerminalFailureScope::AppRuntime,
        required: true,
        optional_disable_policy: OptionalProviderDisablePolicy::Forbidden,
        deferred_activation_policy: ProviderDeferredActivationPolicy::Immediate,
        restart_policy: ProviderRestartPolicy::Forbidden,
        max_groups_per_session: 1,
        prepare_deadline_ticks: 10_000,
        start_deadline_ticks: 10_000,
        close_deadline_ticks: 10_000,
        factory,
        dependencies: ProviderDependencySet::EMPTY,
    };
    let session_vm_provider = ProviderCatalogEntry {
        template,
        manifest: ProviderFactoryManifest {
            format_version: 1,
            factory,
            role: template.role,
            placement: template.placement,
            isolation: template.isolation,
            static_initializer_policy: StaticInitializerPolicy::ProvenAbsent,
            safe_unload: false,
        },
        evidence: ProviderTrustEvidence::BuiltIn,
        loaded: Some(LoadedProviderFactory {
            factory_id: factory.factory_id,
            artifact_digest: factory.artifact_digest,
            role: template.role,
            abi_fingerprint: factory.abi_fingerprint,
            schema_fingerprint: factory.schema_fingerprint,
        }),
    };

    let mut artifacts = Vec::with_capacity(
        browser_artifacts
            .len()
            .saturating_add(entry_factories.len())
            .saturating_add(3),
    );
    if entry_factories.is_empty() {
        artifacts.push(development_artifact(
            b"vo.studio.entry-code.v1",
            RuntimeArtifactRole::EntryCode,
            entry_digest,
            &[&entry_digest],
        ));
    } else {
        artifacts.extend(
            entry_factories
                .iter()
                .map(|factory| generated_entry_artifact(factory, entry_digest)),
        );
    }
    artifacts.push(development_artifact(
        b"vo.studio.entry-schema-artifact.v1",
        RuntimeArtifactRole::EntrySchema,
        entry_schema_fingerprint,
        &[&entry_digest, &APP_PROTOCOL_EXACT_FINGERPRINT],
    ));
    artifacts.push(builtin_host_artifact(target, topology, host_digest));
    for browser in browser_artifacts {
        artifacts.push(MaterializedRuntimeArtifact {
            artifact_identity: browser.artifact_identity,
            role: match browser.role {
                MaterializedBrowserArtifactRole::WasmModule => RuntimeArtifactRole::WasmModule,
                MaterializedBrowserArtifactRole::JavaScriptGlue => {
                    RuntimeArtifactRole::JavaScriptModule
                }
                MaterializedBrowserArtifactRole::JavaScriptModule => {
                    RuntimeArtifactRole::JavaScriptModule
                }
            },
            content_digest: browser.content_digest,
            detached_manifest_digest: browser.detached_manifest_digest,
            trust: ProviderTrustEvidence::DevelopmentAttestation {
                attestation_digest: browser.development_attestation_digest,
            },
        });
    }
    let (framework_providers, framework_factory_artifacts) =
        native_framework_provider_catalog(runtime, native_extensions, locked_modules)?;
    artifacts.extend(framework_factory_artifacts);
    artifacts.sort_by_key(|artifact| artifact.artifact_identity);

    let certification_digest = sha256_parts(
        b"vo.studio.native-platform-certification.v1",
        &[
            &[target_tag(target)],
            &[topology_tag(topology)],
            &host_digest,
            &required_probes.0.to_le_bytes(),
        ],
    );
    let effective_limits_digest = default_hosted_session_limits().fingerprint();
    let providers = core::iter::once(session_vm_provider)
        .chain(framework_providers)
        .collect::<Vec<_>>();
    let variant_identity = digest_variant(
        target,
        topology,
        certification_digest,
        effective_limits_digest,
        &artifacts,
        &capabilities,
        &providers,
    );
    let variant = CertifiedAppRuntimeVariant {
        variant_identity,
        target,
        topology,
        platform_certification_digest: certification_digest,
        required_probes,
        artifacts,
        entry_factories,
        providers,
        initial_groups: vec![InitialInstanceGroupPlan {
            instances: vec![InitialProviderInstancePlan {
                template_id: SESSION_VM_TEMPLATE_ID,
                capabilities: capabilities.clone(),
            }],
        }],
        requested_capabilities: capabilities,
        effective_limits_digest,
    };
    let build_identity = sha256_parts(
        b"vo.studio.app-build-plan.v1",
        &[
            &entry_digest,
            &entry_schema_fingerprint,
            &APP_PROTOCOL_EXACT_FINGERPRINT,
            &variant_identity,
        ],
    );
    let build_plan = AppBuildPlan {
        build_identity,
        entry_code_fingerprint: entry_digest,
        entry_schema_fingerprint,
        app_protocol_fingerprint: APP_PROTOCOL_EXACT_FINGERPRINT,
        trust_policy: ProviderTrustPolicy::Development,
        variants: vec![variant],
    };
    build_plan
        .validate()
        .map_err(|error| format!("validate native Studio AppBuildPlan: {error:?}"))?;

    let probe_evidence_digest = sha256_parts(
        b"vo.studio.native-host-probe.v1",
        &[
            &host_digest,
            &[target_tag(target)],
            &[topology_tag(topology)],
            &available_host_probes.0.to_le_bytes(),
        ],
    );
    build_plan
        .resolve(
            TrustedHostProbe {
                target,
                topology,
                selected_variant_identity: variant_identity,
                available: available_host_probes,
                platform_certification_digest: certification_digest,
                probe_evidence_digest,
            },
            plan_generation,
        )
        .map_err(|error| format!("resolve native Studio AppBuildPlan: {error:?}"))
}

fn native_framework_provider_catalog(
    runtime: &vo_web::BrowserRuntimePlan,
    native_extensions: &[NativeExtensionSpec],
    locked_modules: &[LockedModule],
) -> Result<(Vec<ProviderCatalogEntry>, Vec<MaterializedRuntimeArtifact>), String> {
    let mut frameworks = runtime.graph.frameworks.iter().collect::<Vec<_>>();
    frameworks.sort_by(|left, right| {
        left.module_key
            .as_bytes()
            .cmp(right.module_key.as_bytes())
            .then_with(|| {
                left.id
                    .extension_name
                    .as_bytes()
                    .cmp(right.id.extension_name.as_bytes())
            })
    });
    let mut providers = Vec::new();
    let mut factory_artifacts = Vec::new();
    let mut next_template_id = 2_u32;
    for framework in frameworks {
        let extension = native_extensions
            .iter()
            .find(|extension| extension.module_owner == framework.module_key)
            .ok_or_else(|| {
                format!(
                    "framework {} has no materialized native extension",
                    framework.module_key
                )
            })?;
        let locked = locked_modules
            .iter()
            .find(|module| module.path.as_str() == framework.module_key)
            .ok_or_else(|| format!("framework {} is absent from vo.lock", framework.module_key))?;
        let selection = locked.selection.as_ref().ok_or_else(|| {
            format!(
                "framework {} has no frozen capability selection",
                framework.module_key
            )
        })?;
        let mut roles = if selection.role_artifacts.is_empty() {
            selection
                .source_outputs
                .iter()
                .map(|output| output.role.clone())
                .collect::<Vec<_>>()
        } else {
            selection
                .role_artifacts
                .iter()
                .map(|artifact| artifact.role.clone())
                .collect::<Vec<_>>()
        };
        roles.sort();
        roles.dedup();
        if roles.is_empty() {
            return Err(format!(
                "framework {} frozen selection has no role artifacts",
                framework.module_key
            ));
        }
        let artifact_digest = sha256_file(&extension.native_path)?;
        let factory_artifact = development_artifact(
            b"vo.studio.framework-native-provider-artifact.v1",
            RuntimeArtifactRole::ProviderFactory,
            artifact_digest,
            &[
                framework.module_key.as_bytes(),
                framework.id.extension_name.as_bytes(),
                &artifact_digest,
            ],
        );
        let abi_fingerprint = digest_bytes(&selection.abi)?;
        let schema_fingerprint = digest_bytes(&selection.schema)?;
        let capability_set = CapabilitySet::normalize(
            selection.capabilities.iter().map(String::as_str),
            "native provider capability selection",
        )
        .map_err(|error| format!("normalize provider capabilities: {error}"))?;
        let capability_digest = digest_bytes(&capability_set.digest())?;
        for artifact_role in roles {
            let provider_role = map_native_provider_role(&framework.module_key, &artifact_role)?;
            let factory = ProviderFactoryRequirement {
                factory_id: stable_provider_factory_id(&framework.module_key, provider_role),
                artifact_digest,
                abi_fingerprint,
                schema_fingerprint,
                capability_digest,
                loader: ProviderLoaderKind::NativeDynamicLibrary,
            };
            let template = ProviderTemplate {
                template_id: next_template_id,
                role: provider_role,
                placement: PlacementDomain::NativeThread,
                isolation: IsolationClass::CooperativeInProcess,
                failure_scope: TerminalFailureScope::InstanceGroup,
                required: true,
                optional_disable_policy: OptionalProviderDisablePolicy::Forbidden,
                deferred_activation_policy: ProviderDeferredActivationPolicy::Immediate,
                restart_policy: ProviderRestartPolicy::Forbidden,
                max_groups_per_session: 1,
                prepare_deadline_ticks: 10_000,
                start_deadline_ticks: 10_000,
                close_deadline_ticks: 10_000,
                factory,
                dependencies: ProviderDependencySet {
                    ids: [SESSION_VM_TEMPLATE_ID, 0, 0, 0, 0, 0, 0, 0],
                    len: 1,
                },
            };
            providers.push(ProviderCatalogEntry {
                template,
                manifest: ProviderFactoryManifest {
                    format_version: 1,
                    factory,
                    role: template.role,
                    placement: template.placement,
                    isolation: template.isolation,
                    static_initializer_policy: StaticInitializerPolicy::ProvenAbsent,
                    safe_unload: true,
                },
                evidence: factory_artifact.trust,
                loaded: None,
            });
            next_template_id = next_template_id
                .checked_add(1)
                .ok_or_else(|| String::from("framework provider template identity exhausted"))?;
        }
        factory_artifacts.push(factory_artifact);
    }
    Ok((providers, factory_artifacts))
}

fn map_native_provider_role(module_key: &str, role: &ArtifactRole) -> Result<ProviderRole, String> {
    let provider = match role {
        ArtifactRole::Logic if module_key.ends_with("/voplay") => ProviderRole::GameLogic,
        ArtifactRole::Asset if module_key.ends_with("/voplay") => ProviderRole::GameAsset,
        ArtifactRole::Render if module_key.ends_with("/voplay") => ProviderRole::GameRenderer,
        ArtifactRole::Audio if module_key.ends_with("/voplay") => ProviderRole::GameAudio,
        ArtifactRole::UiLogic => ProviderRole::UiLogic,
        ArtifactRole::UiRenderer => ProviderRole::UiRenderer,
        ArtifactRole::SurfaceHost => ProviderRole::SurfaceHost,
        ArtifactRole::Accessibility => ProviderRole::Accessibility,
        ArtifactRole::Diagnostics => ProviderRole::Diagnostics,
        _ => {
            return Err(format!(
                "framework {module_key} has unsupported native role {}",
                role.as_str()
            ))
        }
    };
    Ok(provider)
}

fn stable_provider_factory_id(module_key: &str, role: ProviderRole) -> u32 {
    let role = match role {
        ProviderRole::GameLogic => "logic",
        ProviderRole::GameAsset => "asset",
        ProviderRole::GameRenderer => "render",
        ProviderRole::GameAudio => "audio",
        ProviderRole::UiLogic => "ui-logic",
        ProviderRole::UiRenderer => "ui-renderer",
        ProviderRole::SurfaceHost => "surface-host",
        ProviderRole::Accessibility => "accessibility",
        ProviderRole::Diagnostics => "diagnostics",
        ProviderRole::SessionVm => "session-vm",
    };
    let mut hash = 0x811c_9dc5_u32;
    for byte in module_key
        .as_bytes()
        .iter()
        .chain([0].iter())
        .chain(role.as_bytes())
    {
        hash ^= u32::from(*byte);
        hash = hash.wrapping_mul(0x0100_0193);
    }
    hash.max(1)
}

fn digest_bytes(digest: &vo_module::digest::Digest) -> Result<[u8; 32], String> {
    let bytes = digest.hex().as_bytes();
    let mut output = [0_u8; 32];
    for (index, pair) in bytes.chunks_exact(2).enumerate() {
        output[index] = (hex_nibble(pair[0])? << 4) | hex_nibble(pair[1])?;
    }
    Ok(output)
}

fn hex_nibble(value: u8) -> Result<u8, String> {
    match value {
        b'0'..=b'9' => Ok(value - b'0'),
        b'a'..=b'f' => Ok(value - b'a' + 10),
        _ => Err(String::from("provider digest contains invalid hex")),
    }
}

const fn provider_role_tag(role: ProviderRole) -> u8 {
    match role {
        ProviderRole::SessionVm => 1,
        ProviderRole::UiLogic => 2,
        ProviderRole::UiRenderer => 3,
        ProviderRole::GameLogic => 4,
        ProviderRole::GameAsset => 5,
        ProviderRole::GameRenderer => 6,
        ProviderRole::GameAudio => 7,
        ProviderRole::SurfaceHost => 8,
        ProviderRole::Accessibility => 9,
        ProviderRole::Diagnostics => 10,
    }
}

fn development_artifact(
    domain: &[u8],
    role: RuntimeArtifactRole,
    content_digest: [u8; 32],
    identity_parts: &[&[u8]],
) -> MaterializedRuntimeArtifact {
    let artifact_identity = sha256_parts(domain, identity_parts);
    let detached_manifest_digest = sha256_parts(
        b"vo.studio.detached-artifact-manifest.v1",
        &[
            &artifact_identity,
            &content_digest,
            &[runtime_artifact_tag(role)],
        ],
    );
    let attestation_digest = sha256_parts(
        b"vo.studio.development-materialization-attestation.v1",
        &[
            &artifact_identity,
            &content_digest,
            &detached_manifest_digest,
        ],
    );
    MaterializedRuntimeArtifact {
        artifact_identity,
        role,
        content_digest,
        detached_manifest_digest,
        trust: ProviderTrustEvidence::DevelopmentAttestation { attestation_digest },
    }
}

fn generated_entry_artifact(
    factory: &vo_app_runtime::CertifiedEntryFactory,
    content_digest: [u8; 32],
) -> MaterializedRuntimeArtifact {
    let detached_manifest_digest = sha256_parts(
        b"vo.studio.generated-entry-manifest.v1",
        &[
            &factory.artifact_identity,
            &factory.factory_id.to_le_bytes(),
            &factory.binding_fingerprint,
            &factory.role_artifact_set_fingerprint,
            &content_digest,
        ],
    );
    let attestation_digest = sha256_parts(
        b"vo.studio.generated-entry-attestation.v1",
        &[
            &factory.artifact_identity,
            &content_digest,
            &detached_manifest_digest,
        ],
    );
    MaterializedRuntimeArtifact {
        artifact_identity: factory.artifact_identity,
        role: RuntimeArtifactRole::EntryCode,
        content_digest,
        detached_manifest_digest,
        trust: ProviderTrustEvidence::DevelopmentAttestation { attestation_digest },
    }
}

fn builtin_host_artifact(
    target: RuntimeTarget,
    topology: HostTopology,
    content_digest: [u8; 32],
) -> MaterializedRuntimeArtifact {
    let artifact_identity = sha256_parts(
        b"vo.studio.builtin-session-vm.v1",
        &[
            &[target_tag(target)],
            &[topology_tag(topology)],
            &content_digest,
        ],
    );
    let detached_manifest_digest = sha256_parts(
        b"vo.studio.builtin-session-vm-manifest.v1",
        &[&artifact_identity, &content_digest],
    );
    MaterializedRuntimeArtifact {
        artifact_identity,
        role: RuntimeArtifactRole::ProviderFactory,
        content_digest,
        detached_manifest_digest,
        trust: ProviderTrustEvidence::BuiltIn,
    }
}

fn digest_variant(
    target: RuntimeTarget,
    topology: HostTopology,
    certification: [u8; 32],
    limits: [u8; 32],
    artifacts: &[MaterializedRuntimeArtifact],
    capabilities: &[vo_app_runtime::CapabilityId],
    providers: &[ProviderCatalogEntry],
) -> [u8; 32] {
    let mut record = Vec::new();
    record.extend_from_slice(b"vo.studio.certified-app-runtime-variant.v1");
    record.push(target_tag(target));
    record.push(topology_tag(topology));
    record.extend_from_slice(&certification);
    record.extend_from_slice(&limits);
    for artifact in artifacts {
        record.extend_from_slice(&artifact.artifact_identity);
        record.extend_from_slice(&artifact.content_digest);
        record.extend_from_slice(&artifact.detached_manifest_digest);
        record.push(runtime_artifact_tag(artifact.role));
    }
    for capability in capabilities {
        record.extend_from_slice(&capability.0.to_le_bytes());
    }
    for provider in providers {
        append_provider_record(&mut record, provider);
    }
    sha256_bytes(&record)
}

fn append_provider_record(record: &mut Vec<u8>, provider: &ProviderCatalogEntry) {
    let template = provider.template;
    record.extend_from_slice(&template.template_id.to_le_bytes());
    record.push(provider_role_tag(template.role));
    record.push(placement_tag(template.placement));
    record.push(isolation_tag(template.isolation));
    record.push(failure_scope_tag(template.failure_scope));
    record.push(u8::from(template.required));
    record.push(match template.optional_disable_policy {
        OptionalProviderDisablePolicy::Forbidden => 1,
        OptionalProviderDisablePolicy::DisableCapability => 2,
    });
    record.push(match template.deferred_activation_policy {
        ProviderDeferredActivationPolicy::Immediate => 1,
        ProviderDeferredActivationPolicy::ReadyLockedAllowed => 2,
    });
    match template.restart_policy {
        ProviderRestartPolicy::Forbidden => record.extend_from_slice(&[1, 0]),
        ProviderRestartPolicy::OnFailure { max_restarts } => {
            record.extend_from_slice(&[2, max_restarts])
        }
    }
    record.extend_from_slice(&template.max_groups_per_session.to_le_bytes());
    record.extend_from_slice(&template.prepare_deadline_ticks.to_le_bytes());
    record.extend_from_slice(&template.start_deadline_ticks.to_le_bytes());
    record.extend_from_slice(&template.close_deadline_ticks.to_le_bytes());
    append_factory_record(record, template.factory);
    record.push(template.dependencies.len);
    for dependency in template.dependencies.iter() {
        record.extend_from_slice(&dependency.to_le_bytes());
    }
    record.extend_from_slice(&provider.manifest.format_version.to_le_bytes());
    record.push(static_initializer_tag(
        provider.manifest.static_initializer_policy,
    ));
    record.push(u8::from(provider.manifest.safe_unload));
    append_trust_record(record, provider.evidence);
    if let Some(loaded) = provider.loaded {
        record.push(1);
        record.extend_from_slice(&loaded.factory_id.to_le_bytes());
        record.extend_from_slice(&loaded.artifact_digest);
        record.push(provider_role_tag(loaded.role));
        record.extend_from_slice(&loaded.abi_fingerprint);
        record.extend_from_slice(&loaded.schema_fingerprint);
    } else {
        record.push(0);
    }
}

fn append_factory_record(record: &mut Vec<u8>, factory: ProviderFactoryRequirement) {
    record.extend_from_slice(&factory.factory_id.to_le_bytes());
    record.extend_from_slice(&factory.artifact_digest);
    record.extend_from_slice(&factory.abi_fingerprint);
    record.extend_from_slice(&factory.schema_fingerprint);
    record.extend_from_slice(&factory.capability_digest);
    record.push(match factory.loader {
        ProviderLoaderKind::BuiltInStatic => 1,
        ProviderLoaderKind::NativeDynamicLibrary => 2,
        ProviderLoaderKind::WasmModule => 3,
        ProviderLoaderKind::BrowserJsModule => 4,
    });
}

fn append_trust_record(record: &mut Vec<u8>, trust: ProviderTrustEvidence) {
    match trust {
        ProviderTrustEvidence::BuiltIn => record.push(1),
        ProviderTrustEvidence::DevelopmentAttestation { attestation_digest } => {
            record.push(2);
            record.extend_from_slice(&attestation_digest);
        }
        ProviderTrustEvidence::ReleaseProvenance {
            signature_verification_digest,
            provenance_digest,
            sbom_digest,
        } => {
            record.push(3);
            record.extend_from_slice(&signature_verification_digest);
            record.extend_from_slice(&provenance_digest);
            record.extend_from_slice(&sbom_digest);
        }
    }
}

const fn placement_tag(placement: PlacementDomain) -> u8 {
    match placement {
        PlacementDomain::NativeMain => 1,
        PlacementDomain::NativeThread => 2,
        PlacementDomain::HostedActor => 3,
        PlacementDomain::WasmMain => 4,
        PlacementDomain::WebWorker => 5,
        PlacementDomain::WebView => 6,
        PlacementDomain::ChildProcess => 7,
    }
}

const fn isolation_tag(isolation: IsolationClass) -> u8 {
    match isolation {
        IsolationClass::CooperativeInProcess => 1,
        IsolationClass::TerminableWorker => 2,
        IsolationClass::ChildProcess => 3,
    }
}

const fn failure_scope_tag(scope: TerminalFailureScope) -> u8 {
    match scope {
        TerminalFailureScope::InstanceGroup => 1,
        TerminalFailureScope::Session => 2,
        TerminalFailureScope::AppRuntime => 3,
    }
}

const fn static_initializer_tag(policy: StaticInitializerPolicy) -> u8 {
    match policy {
        StaticInitializerPolicy::ProvenAbsent => 1,
        StaticInitializerPolicy::CertifiedSideEffectFreeHostAdapter => 2,
        StaticInitializerPolicy::IsolatedByWorkerOrProcess => 3,
    }
}

fn digest_capabilities(capabilities: &[vo_app_runtime::CapabilityId]) -> [u8; 32] {
    let mut record = Vec::with_capacity(capabilities.len().saturating_mul(8));
    for capability in capabilities {
        record.extend_from_slice(&capability.0.to_le_bytes());
    }
    sha256_parts(b"vo.provider-capabilities.v1", &[&record])
}

fn canonical_record(domain: &[u8], parts: &[&[u8]]) -> Result<Vec<u8>, String> {
    let mut record = Vec::new();
    record.extend_from_slice(domain);
    for part in parts {
        let length =
            u32::try_from(part.len()).map_err(|_| String::from("plan record field too large"))?;
        record.extend_from_slice(&length.to_le_bytes());
        record.extend_from_slice(part);
    }
    Ok(record)
}

fn sha256_parts(domain: &[u8], parts: &[&[u8]]) -> [u8; 32] {
    let mut hasher = Sha256::new();
    hasher.update((domain.len() as u64).to_le_bytes());
    hasher.update(domain);
    for part in parts {
        hasher.update((part.len() as u64).to_le_bytes());
        hasher.update(part);
    }
    hasher.finalize().into()
}

fn sha256_bytes(bytes: &[u8]) -> [u8; 32] {
    Sha256::digest(bytes).into()
}

fn sha256_file(path: &Path) -> Result<[u8; 32], String> {
    let metadata =
        std::fs::metadata(path).map_err(|error| format!("{}: {error}", path.display()))?;
    if !metadata.is_file() || metadata.len() == 0 || metadata.len() > MAX_HOST_EXECUTABLE_BYTES {
        return Err(format!(
            "native Studio executable must be a non-empty regular file within {} bytes: {}",
            MAX_HOST_EXECUTABLE_BYTES,
            path.display()
        ));
    }
    let mut file = File::open(path).map_err(|error| format!("{}: {error}", path.display()))?;
    let mut hasher = Sha256::new();
    let mut buffer = [0u8; 64 * 1024];
    loop {
        let read = file
            .read(&mut buffer)
            .map_err(|error| format!("{}: {error}", path.display()))?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
    }
    Ok(hasher.finalize().into())
}

const fn native_runtime_target() -> RuntimeTarget {
    #[cfg(target_os = "macos")]
    {
        RuntimeTarget::NativeMacOs
    }
    #[cfg(target_os = "linux")]
    {
        RuntimeTarget::NativeLinux
    }
    #[cfg(target_os = "windows")]
    {
        RuntimeTarget::NativeWindows
    }
}

const fn target_tag(target: RuntimeTarget) -> u8 {
    match target {
        RuntimeTarget::BrowserWasm => 1,
        RuntimeTarget::NativeMacOs => 2,
        RuntimeTarget::NativeLinux => 3,
        RuntimeTarget::NativeWindows => 4,
        RuntimeTarget::Headless => 5,
        RuntimeTarget::Manual => 6,
    }
}

const fn topology_tag(topology: HostTopology) -> u8 {
    match topology {
        HostTopology::BrowserMain => 1,
        HostTopology::WebviewNativeHost => 2,
        HostTopology::GpuNativeHost => 3,
        HostTopology::Headless => 4,
        HostTopology::Manual => 5,
    }
}

const fn runtime_artifact_tag(role: RuntimeArtifactRole) -> u8 {
    match role {
        RuntimeArtifactRole::EntryCode => 1,
        RuntimeArtifactRole::EntrySchema => 2,
        RuntimeArtifactRole::ProviderFactory => 3,
        RuntimeArtifactRole::RendererModule => 4,
        RuntimeArtifactRole::ShaderBundle => 5,
        RuntimeArtifactRole::NativeAdapter => 6,
        RuntimeArtifactRole::WasmModule => 7,
        RuntimeArtifactRole::JavaScriptModule => 8,
    }
}
