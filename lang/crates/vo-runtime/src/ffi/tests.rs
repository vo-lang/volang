use super::*;

#[test]
fn process_identity_allocator_exhausts_without_wrapping_or_reuse() {
    let counter = AtomicU64::new(u64::MAX - 1);

    assert_eq!(allocate_process_identity(&counter), Some(u64::MAX - 1));
    assert_eq!(allocate_process_identity(&counter), Some(u64::MAX));
    assert_eq!(allocate_process_identity(&counter), None);
    assert_eq!(counter.load(Ordering::Relaxed), 0);
}

#[cfg(feature = "std")]
#[test]
fn native_abi_fingerprint_covers_entry_and_table_layouts_independently() {
    const SEED: u64 = 0xcbf2_9ce4_8422_2325;

    let entry_words = extern_entry_layout_words();
    let entry_fingerprint = hash_abi_words(SEED, &entry_words);
    let mut tampered_entry = entry_words;
    tampered_entry[0] ^= 1;
    assert_ne!(
        hash_abi_words(SEED, &tampered_entry),
        entry_fingerprint,
        "ExternEntry size drift must change its ABI layout fingerprint"
    );

    let table_words = extension_table_layout_words();
    let table_fingerprint = hash_abi_words(SEED, &table_words);
    let mut tampered_table = table_words;
    tampered_table[4] ^= 1;
    assert_ne!(
        hash_abi_words(SEED, &tampered_table),
        table_fingerprint,
        "ExtensionTable entries offset drift must change its ABI layout fingerprint"
    );

    assert_ne!(entry_fingerprint, table_fingerprint);
}

#[cfg(feature = "std")]
#[test]
fn linkme_table_validation_builds_a_unique_canonical_index() {
    extern "C" fn linkme_dummy(_ctx: *mut ExtAbiContextV9) -> u32 {
        ext_abi::RESULT_OK
    }

    fn entry(name: &str, module_owner: &str) -> ExternEntry {
        ExternEntry {
            name_ptr: name.as_ptr(),
            name_len: name.len() as u32,
            module_owner_ptr: module_owner.as_ptr(),
            module_owner_len: module_owner.len() as u32,
            func: Some(linkme_dummy),
            effects_bits: ExternEffects::NONE.bits(),
        }
    }

    fn owner_entry(module_owner: &str) -> ExternModuleOwnerEntry {
        ExternModuleOwnerEntry {
            module_owner_ptr: module_owner.as_ptr(),
            module_owner_len: module_owner.len() as u32,
        }
    }

    let ascii =
        vo_common_core::extern_key::ExternKeyRef::new("github.com/volang/stdlib/math", "Sqrt")
            .encode()
            .unwrap();
    let unicode =
        vo_common_core::extern_key::ExternKeyRef::new("github.com/acme/demo/render", "方法")
            .encode()
            .unwrap();
    let unicode_package =
        vo_common_core::extern_key::ExternKeyRef::new("github.com/acme/demo/图形/é", "Run")
            .encode()
            .unwrap();
    let entries = [
        entry(&ascii, "github.com/volang/stdlib"),
        entry(&unicode, "github.com/acme/demo"),
        entry(&unicode_package, "github.com/acme/demo"),
    ];
    let owners = [
        owner_entry("github.com/volang/stdlib"),
        owner_entry("github.com/acme/demo"),
    ];
    let index = validate_linkme_extern_entries(&owners, &entries).expect("canonical unique table");
    assert_eq!(index.entries.len(), 3);
    assert!(index
        .entries
        .contains_key(&("github.com/volang/stdlib", ascii.as_str())));
    assert!(index
        .entries
        .contains_key(&("github.com/acme/demo", unicode.as_str())));
    assert!(index
        .entries
        .contains_key(&("github.com/acme/demo", unicode_package.as_str())));

    let parent_owner = "github.com/acme/mono";
    let child_owner = "github.com/acme/mono/graphics";
    let overlapping = vo_common_core::extern_key::ExternKeyRef::new(
        "github.com/acme/mono/graphics/render",
        "Draw",
    )
    .encode()
    .unwrap();
    for entries in [
        [
            entry(&overlapping, parent_owner),
            entry(&overlapping, child_owner),
        ],
        [
            entry(&overlapping, child_owner),
            entry(&overlapping, parent_owner),
        ],
    ] {
        let owners = [owner_entry(parent_owner), owner_entry(child_owner)];
        let index = validate_linkme_extern_entries(&owners, &entries)
            .expect("nested owners may carry the same canonical extern name");
        let (selected_owner, _) = lookup_linkme_entry_in_index(&index, &overlapping)
            .expect("nested owner provides the selected extern");
        assert_eq!(selected_owner, child_owner);
    }

    let missing = vo_common_core::extern_key::ExternKeyRef::new(
        "github.com/acme/mono/graphics/render",
        "Missing",
    )
    .encode()
    .unwrap();
    let child_only_other = vo_common_core::extern_key::ExternKeyRef::new(
        "github.com/acme/mono/graphics/render",
        "Other",
    )
    .encode()
    .unwrap();
    let entries = [
        entry(&missing, parent_owner),
        entry(&child_only_other, child_owner),
    ];
    let owners = [owner_entry(parent_owner), owner_entry(child_owner)];
    let index = validate_linkme_extern_entries(&owners, &entries).expect("valid nested catalogs");
    assert!(
        lookup_linkme_entry_in_index(&index, &missing).is_none(),
        "a missing child entry must not fall back to the parent provider"
    );

    let duplicate = [
        entry(&ascii, "github.com/volang/stdlib"),
        entry(&ascii, "github.com/volang/stdlib"),
    ];
    assert!(
        validate_linkme_extern_entries(&[owner_entry("github.com/volang/stdlib")], &duplicate,)
            .err()
            .expect("duplicate must fail")
            .to_string()
            .contains("duplicate extern name")
    );

    let legacy = concat!("math_", "Sqrt");
    assert!(validate_linkme_extern_entries(
        &[owner_entry("github.com/volang/stdlib")],
        &[entry(legacy, "github.com/volang/stdlib")],
    )
    .err()
    .expect("legacy name must fail")
    .to_string()
    .contains("not canonical"));

    let mut invalid_effects = entry(&ascii, "github.com/volang/stdlib");
    invalid_effects.effects_bits = ExternEffects::ALLOWED_BITS << 1;
    assert!(validate_linkme_extern_entries(
        &[owner_entry("github.com/volang/stdlib")],
        &[invalid_effects],
    )
    .err()
    .expect("invalid effects must fail")
    .to_string()
    .contains("invalid effects bits"));

    let traversal =
        vo_common_core::extern_key::ExternKeyRef::new("github.com/acme/demo/../evil", "Run")
            .encode()
            .expect("codec remains injective for invalid owner paths");
    let error = validate_linkme_extern_entries(
        &[owner_entry("github.com/acme/demo")],
        &[entry(&traversal, "github.com/acme/demo")],
    )
    .err()
    .expect("linkme owner validation must reject traversal");
    assert!(
        error.to_string().contains("invalid language identity"),
        "{error}"
    );

    let error = validate_linkme_extern_entries(
        &[owner_entry("github.com/acme/other")],
        &[entry(&unicode, "github.com/acme/other")],
    )
    .err()
    .expect("linkme entry must belong to its explicit owner");
    assert!(
        error.to_string().contains("outside declared module owner"),
        "{error}"
    );

    for (package, function) in [
        ("github.com/acme/demo/e\u{301}", "Run"),
        ("github.com/acme/demo", "for"),
        ("github.com/acme/demo", "_"),
        ("github.com/acme/demo", "bad-name"),
        ("github.com/acme/demo", "\u{301}Bad"),
    ] {
        let encoded = vo_common_core::extern_key::ExternKeyRef::new(package, function)
            .encode()
            .expect("wire codec preserves semantic-invalid linkme vector");
        let entries = [entry(&encoded, "github.com/acme/demo")];
        let error =
            validate_linkme_extern_entries(&[owner_entry("github.com/acme/demo")], &entries)
                .err()
                .expect("semantic-invalid linkme identity must fail");
        assert!(
            error.to_string().contains("invalid language identity"),
            "{error}"
        );
    }

    let zero_entry_child_owners = [owner_entry(parent_owner), owner_entry(child_owner)];
    let parent_only_entries = [entry(&missing, parent_owner)];
    let index = validate_linkme_extern_entries(&zero_entry_child_owners, &parent_only_entries)
        .expect("zero-entry child owner remains part of the complete catalog");
    assert!(
        lookup_linkme_entry_in_index(&index, &missing).is_none(),
        "a zero-entry child owner must block parent fallback"
    );

    let duplicate_owners = [owner_entry(parent_owner), owner_entry(parent_owner)];
    let error = validate_linkme_module_owner_entries(&duplicate_owners)
        .expect_err("duplicate module-level owner declarations must fail");
    assert!(
        error.to_string().contains("duplicate module owner"),
        "{error}"
    );

    for invalid_owner in ["", "local/demo", "github.com/acme//demo"] {
        let declarations = [owner_entry(invalid_owner)];
        assert!(
            validate_linkme_module_owner_entries(&declarations).is_err(),
            "invalid linkme owner was accepted: {invalid_owner:?}"
        );
    }

    let invalid_utf8 = [0xff_u8];
    let invalid_utf8_declarations = [ExternModuleOwnerEntry {
        module_owner_ptr: invalid_utf8.as_ptr(),
        module_owner_len: invalid_utf8.len() as u32,
    }];
    let error = validate_linkme_module_owner_entries(&invalid_utf8_declarations)
        .expect_err("module owner declarations must contain UTF-8");
    assert!(error.to_string().contains("not UTF-8"), "{error}");

    let null_declarations = [ExternModuleOwnerEntry {
        module_owner_ptr: core::ptr::null(),
        module_owner_len: 1,
    }];
    let error = validate_linkme_module_owner_entries(&null_declarations)
        .expect_err("non-empty module owner declaration cannot have a null pointer");
    assert!(error.to_string().contains("pointer is null"), "{error}");

    let undeclared = [entry(&unicode, "github.com/acme/demo")];
    let error =
        validate_linkme_extern_entries(&[owner_entry("github.com/acme/other")], &undeclared)
            .err()
            .expect("every linkme extern owner needs one module-level declaration");
    assert!(
        error.to_string().contains("undeclared module owner"),
        "{error}"
    );
}

#[cfg(feature = "std")]
#[test]
fn provider_name_policy_allows_short_names_only_for_vm_builtins() {
    let canonical = test_extern_name("Run");
    for source in [
        RegisteredExternSource::Stdlib,
        RegisteredExternSource::LinkmeExtension,
        RegisteredExternSource::NativeExtension,
        RegisteredExternSource::WasmHost,
        RegisteredExternSource::WasmExtensionBridge,
        RegisteredExternSource::Manual,
        RegisteredExternSource::Test,
    ] {
        assert!(validate_registered_provider_name(&canonical, source).is_ok());
        assert!(validate_registered_provider_name("legacy_flattened_Run", source).is_err());
        assert!(validate_registered_provider_name("vo_copy", source).is_err());
    }

    assert!(validate_registered_provider_name("vo_copy", RegisteredExternSource::Builtin).is_ok());
    assert!(validate_registered_provider_name(&canonical, RegisteredExternSource::Builtin).is_ok());
    assert!(validate_registered_provider_name(
        "legacy_flattened_Run",
        RegisteredExternSource::Builtin
    )
    .is_err());

    for (package, function) in [
        ("github.com/acme/demo/../escape", "Run"),
        ("github.com/acme/demo/e\u{301}", "Run"),
        ("github.com/acme/demo", "for"),
        ("github.com/acme/demo", "_"),
        ("github.com/acme/demo", "bad-name"),
        ("github.com/acme/demo", "\u{301}Bad"),
    ] {
        let encoded = vo_common_core::extern_key::ExternKeyRef::new(package, function)
            .encode()
            .expect("wire codec preserves semantic-invalid test vectors");
        for source in [
            RegisteredExternSource::Builtin,
            RegisteredExternSource::Stdlib,
            RegisteredExternSource::LinkmeExtension,
            RegisteredExternSource::NativeExtension,
            RegisteredExternSource::WasmHost,
            RegisteredExternSource::WasmExtensionBridge,
            RegisteredExternSource::Manual,
            RegisteredExternSource::Test,
        ] {
            assert!(
                validate_registered_provider_name(&encoded, source).is_err(),
                "semantic-invalid identity was accepted from {source:?}: {encoded}"
            );
        }
    }
}

#[cfg(feature = "std")]
#[test]
fn direct_native_registration_rejects_legacy_and_vm_internal_names() {
    extern "C" fn provider(_ctx: *mut ExtAbiContextV9) -> u32 {
        ext_abi::RESULT_OK
    }

    let mut registry = ExternRegistry::new();
    for invalid in ["legacy_flattened_Run", "vo_copy", "vo1:01:x:1:F"] {
        let error = registry
            .try_register_extension_with_effects(
                0,
                TEST_MODULE_OWNER,
                invalid,
                provider,
                ExternEffects::NONE,
            )
            .expect_err("direct native registration must enforce canonical names");
        assert!(error.to_string().contains("invalid identity"), "{error}");
        assert!(registry.registered_by_name(invalid).is_none());
    }

    let foreign =
        vo_common_core::extern_key::ExternKeyRef::new("github.com/another/project/pkg", "Run")
            .encode()
            .expect("canonical foreign extern name");
    let error = registry
        .try_register_extension_with_effects(
            0,
            TEST_MODULE_OWNER,
            &foreign,
            provider,
            ExternEffects::NONE,
        )
        .expect_err("direct native registration must enforce module ownership");
    assert!(
        error.to_string().contains("outside module owner"),
        "{error}"
    );

    let traversal = vo_common_core::extern_key::ExternKeyRef::new(
        "github.com/volang/runtime-tests/../escape",
        "Run",
    )
    .encode()
    .expect("length-coded traversal identity");
    let error = registry
        .try_register_extension_with_effects(
            0,
            TEST_MODULE_OWNER,
            &traversal,
            provider,
            ExternEffects::NONE,
        )
        .expect_err("direct native registration must reject lexical traversal");
    assert!(
        error.to_string().contains("invalid language identity"),
        "{error}"
    );

    let error = registry
        .try_register_extension_with_effects(
            0,
            "local/module",
            test_extern_name("Run"),
            provider,
            ExternEffects::NONE,
        )
        .expect_err("direct native registration must enforce canonical module owners");
    assert!(error.to_string().contains("module owner"), "{error}");

    let owned = vo_common_core::extern_key::ExternKeyRef::new(
        "github.com/volang/runtime-tests/child",
        "Run",
    )
    .encode()
    .expect("canonical owned extern name");
    registry
        .try_register_extension_with_effects(
            0,
            TEST_MODULE_OWNER,
            &owned,
            provider,
            ExternEffects::NONE,
        )
        .expect("direct native registration accepts an owned subpackage");
    assert_eq!(
        registry
            .registered_by_name(&owned)
            .expect("owned native provider")
            .provider_module_owner(),
        Some(TEST_MODULE_OWNER)
    );
    let resolved = registry
        .resolve_module_externs(&[extern_def(
            &owned,
            ParamShape::Exact { slots: 0 },
            ReturnShape::slots(0),
            ExternEffects::NONE,
        )])
        .expect("resolve owned native provider");
    assert_eq!(
        resolved
            .get(0)
            .expect("resolved native provider")
            .provider_module_owner
            .as_deref(),
        Some(TEST_MODULE_OWNER)
    );
}

#[cfg(feature = "std")]
#[test]
fn registry_registration_is_single_assignment_and_transactional() {
    let mut registry = ExternRegistry::new();
    let name = test_extern_name("append_only");
    registry
        .try_register_test_named_with_effects(
            0,
            &name,
            ignore_host_event_resume,
            ExternEffects::NONE,
        )
        .expect("initial provider registration");
    let provider_identity = registry
        .registered_by_name(&name)
        .expect("registered provider")
        .provider_identity;

    let duplicate = registry
        .try_register_test_named_with_effects(
            0,
            &name,
            ignore_host_event_resume,
            ExternEffects::NONE,
        )
        .expect_err("same-name registration is single-assignment");
    assert!(
        duplicate.to_string().contains("single-assignment"),
        "{duplicate}"
    );
    let duplicate_other_id = registry
        .try_register_test_named_with_effects(
            7,
            &name,
            ignore_host_event_resume,
            ExternEffects::NONE,
        )
        .expect_err("same-name registration at another id is single-assignment");
    assert!(
        duplicate_other_id.to_string().contains("single-assignment"),
        "{duplicate_other_id}"
    );
    assert!(registry.registered(7).is_none());
    assert_eq!(registry.funcs_by_name.len(), 1);
    assert_eq!(registry.len(), 1);

    let function_conflict = registry
        .try_register_test_named_with_effects(8, &name, other_ok_extern, ExternEffects::NONE)
        .expect_err("same name with a different function must fail");
    assert!(
        function_conflict.to_string().contains("single-assignment"),
        "{function_conflict}"
    );
    assert!(registry.registered(8).is_none());

    let effect_conflict = registry
        .try_register_test_named_with_effects(
            8,
            &name,
            ignore_host_event_resume,
            ExternEffects::MAY_YIELD,
        )
        .expect_err("same name with different effects must fail");
    assert!(
        effect_conflict.to_string().contains("single-assignment"),
        "{effect_conflict}"
    );
    assert!(registry.registered(8).is_none());

    let source_conflict = registry
        .try_register_named_with_effects(8, &name, ignore_host_event_resume, ExternEffects::NONE)
        .expect_err("same provider name from a different source must fail");
    assert!(
        source_conflict.to_string().contains("single-assignment"),
        "{source_conflict}"
    );
    assert!(registry.registered(8).is_none());

    let replacement_name = test_extern_name("append_only_replacement");
    let id_conflict = registry
        .try_register_test_named(0, &replacement_name, ignore_host_event_resume)
        .expect_err("an extern id cannot be rebound");
    assert!(
        id_conflict.to_string().contains("already bound"),
        "{id_conflict}"
    );
    assert!(registry.registered_by_name(&replacement_name).is_none());
    assert_eq!(
        registry
            .registered(0)
            .expect("original id binding")
            .provider_name(),
        name
    );
    assert_eq!(
        registry
            .registered_by_name(&name)
            .expect("original provider remains registered")
            .provider_identity,
        provider_identity
    );
}

#[cfg(feature = "std")]
#[test]
fn sparse_max_extern_id_does_not_allocate_a_dense_registry() {
    let mut registry = ExternRegistry::new();
    let name = test_extern_name("sparse_max_id");

    registry
        .try_register_test_named(u32::MAX, &name, ignore_host_event_resume)
        .expect("the full u32 extern-id domain remains representable");

    assert_eq!(registry.id_to_name.len(), 1);
    assert_eq!(registry.len(), 1);
    assert_eq!(
        registry
            .registered(u32::MAX)
            .expect("sparse maximum id is registered")
            .provider_name(),
        name
    );
    assert!(registry.registered(u32::MAX - 1).is_none());
}

#[cfg(feature = "std")]
#[test]
fn provider_single_assignment_does_not_depend_on_function_addresses() {
    let production = include_str!("mod.rs");
    assert!(!production.contains("fn_addr_eq"));
    assert!(!production.contains("has_same_address"));

    fn provider_a(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
        ExternResult::Ok
    }
    fn provider_b(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
        ExternResult::Ok
    }

    let mut registry = ExternRegistry::new();
    let name = test_extern_name("release_icf_safe");
    registry
        .try_register_test_named(0, &name, provider_a)
        .expect("initial provider registration");
    let error = registry
        .try_register_test_named(0, &name, provider_b)
        .expect_err("provider identity is single-assignment under every optimization mode");
    assert!(error.to_string().contains("single-assignment"), "{error}");
}

#[cfg(feature = "std")]
#[test]
fn compatibility_registration_defers_error_and_resolution_fails_closed() {
    let mut registry = ExternRegistry::new();
    let original = test_extern_name("deferred_original");
    let replacement = test_extern_name("deferred_replacement");
    registry
        .try_register_wasm_host(0, &original, ignore_host_event_resume)
        .expect("initial wasm host provider");

    registry.register_wasm_host(0, &replacement, ignore_host_event_resume);

    let deferred = registry
        .registration_error()
        .expect("compatibility method records its first error");
    assert!(deferred.to_string().contains("already bound"), "{deferred}");
    assert!(registry.registered_by_name(&replacement).is_none());
    assert_eq!(
        registry
            .registered(0)
            .expect("original id binding")
            .provider_name(),
        original
    );

    let error = registry
        .resolve_module_externs(&[extern_def(
            &original,
            ParamShape::CallSiteVariadic,
            ReturnShape::slots(0),
            ExternEffects::NONE,
        )])
        .expect_err("deferred registration error must fail module resolution");
    assert_eq!(error, deferred.clone());
}

#[cfg(feature = "std")]
#[test]
fn batched_registration_failure_rolls_back_every_staged_provider() {
    let mut registry = ExternRegistry::new();
    let occupied = test_extern_name("batch_occupied");
    registry
        .try_register_test_named(2, &occupied, ignore_host_event_resume)
        .expect("preexisting provider");
    let first = test_extern_name("batch_first");
    let conflicting = test_extern_name("batch_conflicting");

    let error = registry
        .try_registration_transaction(|staged| {
            staged.try_register_test_named(0, &first, ignore_host_event_resume)?;
            staged.try_register_test_named(2, &conflicting, ignore_host_event_resume)
        })
        .expect_err("later batch conflict must roll back earlier registrations");

    assert!(error.to_string().contains("already bound"), "{error}");
    assert!(registry.registered_by_name(&first).is_none());
    assert!(registry.registered_by_name(&conflicting).is_none());
    assert_eq!(registry.len(), 1);
    assert_eq!(
        registry
            .registered(2)
            .expect("preexisting provider remains")
            .provider_name(),
        occupied
    );
}

#[cfg(feature = "std")]
#[test]
fn native_catalog_build_is_single_assignment_and_failure_is_transactional() {
    let mut registry = ExternRegistry::new();
    let error = registry
        .try_native_catalog_transaction(|staged| {
            staged.try_declare_extension_module_owner(
                TEST_MODULE_OWNER,
                ExtensionOwnerCatalog::NativeLinkme,
            )?;
            Err(ExternContractError::new("forced catalog failure"))
        })
        .expect_err("failed catalog build must roll back");
    assert!(error.to_string().contains("forced catalog failure"));
    assert!(!registry.native_catalog_built);
    assert!(registry.extension_module_owners.is_empty());

    registry
        .register_from_extension_catalogs(None, &[])
        .expect("empty complete native catalog is a valid single build");
    assert!(registry.native_catalog_built);

    let second = registry
        .register_from_extension_catalogs(None, &[])
        .expect_err("a second native catalog build must fail deterministically");
    assert!(second.to_string().contains("already built"), "{second}");
}

#[cfg(feature = "std")]
#[test]
fn empty_native_owner_catalog_blocks_same_owner_wasm_catalog() {
    let mut registry = ExternRegistry::new();
    registry
        .try_native_catalog_transaction(|staged| {
            staged.try_declare_extension_module_owner(
                TEST_MODULE_OWNER,
                ExtensionOwnerCatalog::NativeLinkme,
            )
        })
        .expect("empty native owner catalog");

    let error = registry
        .register_wasm_extension_bridge_catalog(
            [WasmExtensionOwner::new(
                TEST_MODULE_OWNER,
                TEST_ARTIFACT_GENERATION,
            )],
            core::iter::empty::<WasmExtensionBridgeEntry>(),
        )
        .expect_err("an empty native table still owns its exact module boundary");
    assert!(error.to_string().contains("already claimed"), "{error}");
    assert!(error.to_string().contains("native linkme"), "{error}");
    assert!(!registry.wasm_catalog_built);
}

#[cfg(feature = "std")]
#[test]
fn empty_wasm_owner_catalog_blocks_same_owner_native_catalog() {
    let mut registry = ExternRegistry::new();
    registry
        .register_wasm_extension_bridge_catalog(
            [WasmExtensionOwner::new(
                TEST_MODULE_OWNER,
                TEST_ARTIFACT_GENERATION,
            )],
            core::iter::empty::<WasmExtensionBridgeEntry>(),
        )
        .expect("empty WASM owner catalog");

    let error = registry
        .try_native_catalog_transaction(|staged| {
            staged.try_declare_extension_module_owner(
                TEST_MODULE_OWNER,
                ExtensionOwnerCatalog::NativeLinkme,
            )
        })
        .expect_err("an empty WASM table still owns its exact module boundary");
    assert!(error.to_string().contains("already claimed"), "{error}");
    assert!(
        error.to_string().contains("WASM artifact generation"),
        "{error}"
    );
    assert!(!registry.native_catalog_built);
}

#[cfg(feature = "std")]
#[test]
fn native_loader_and_linkme_batches_share_transactional_commit_path() {
    let production = include_str!("mod.rs");
    let catalog_body = production
        .split("pub fn register_from_extension_catalogs")
        .nth(1)
        .expect("unified extension catalog registrar")
        .split("/// Register the unified linkme and dynamic catalog")
        .next()
        .expect("unified extension catalog registrar body");
    assert!(catalog_body.contains("self.try_native_catalog_transaction"));
    assert!(catalog_body.contains("deepest_owning_module"));
    assert!(catalog_body.contains("lookup_in_module_owner"));
    let transaction_body = production
        .split("fn try_native_catalog_transaction")
        .nth(1)
        .expect("native catalog transaction helper")
        .split("/// Build one active native-extension catalog")
        .next()
        .expect("native catalog transaction helper body");
    assert!(transaction_body.contains("self.try_registration_transaction"));
    assert!(production.contains("&EXTERN_MODULE_OWNER_TABLE"));
    assert!(production.contains("undeclared module owner"));
    assert!(!production.contains("extern_defs[..id]"));
}

#[cfg(feature = "std")]
#[test]
fn production_native_registration_exposes_only_atomic_catalogs() {
    let production = include_str!("mod.rs");
    let direct_adapter = production
        .split("/// Test-only single-entry adapter. Production native providers")
        .nth(1)
        .expect("test-only native adapter")
        .split("fn try_register_native_extension_with_effects")
        .next()
        .expect("test-only adapter body");
    assert!(direct_adapter.contains("cfg(all(feature = \"std\", test))"));
    assert!(!direct_adapter.contains("#[cfg(feature = \"std\")]\n    pub fn"));

    let vm_source = include_str!("../../../vo-vm/src/vm/mod.rs");
    assert!(vm_source.contains("register_from_extension_catalogs(ext_loader.as_ref()"));
    assert!(!vm_source.contains(".register_from_extension_loader("));
    assert!(!production.contains("pub fn register_from_extension_loader"));
    assert!(!production.contains("pub fn register_from_linkme"));
}

#[cfg(feature = "std")]
#[test]
fn bulk_registrars_use_seen_sets_for_large_repeated_inputs() {
    let builtin_source = include_str!("../builtins/builtin.rs");
    let dynamic_source = include_str!("../builtins/dynamic.rs");
    for source in [builtin_source, dynamic_source] {
        assert!(source.contains("BTreeSet"));
        assert!(!source.contains("externs[..id]"));
    }

    let repeated = (0..8_192)
        .map(|_| {
            extern_def(
                "dyn_call",
                ParamShape::CallSiteVariadic,
                ReturnShape::slots(4),
                ExternEffects::MAY_CALL_CLOSURE_REPLAY,
            )
        })
        .collect::<Vec<_>>();
    let mut registry = ExternRegistry::new();
    crate::builtins::dynamic::register_externs(&mut registry, &repeated)
        .expect("repeated VM helper names register once");
    assert_eq!(registry.len(), 1);
    assert!(registry.registered_by_name("dyn_call").is_some());
}

#[cfg(feature = "std")]
#[test]
fn module_resolution_rejects_noncanonical_names_before_provider_lookup() {
    let registry = ExternRegistry::new();
    let forged = crate::bytecode::ExternDef {
        name: "legacy_flattened_Run".to_string(),
        params: ParamShape::Exact { slots: 0 },
        returns: ReturnShape::slots(0),
        allowed_effects: ExternEffects::NONE,
        param_kinds: Vec::new(),
    };
    let error = registry
        .resolve_module_externs(&[forged])
        .expect_err("module resolution must reject a forged extern name");
    assert!(error.to_string().contains("invalid identity"), "{error}");
}

#[cfg(feature = "std")]
#[test]
fn module_resolution_rejects_canonical_same_name_contract_drift() {
    let name = test_extern_name("same_name_contract");
    let first = extern_def(
        &name,
        ParamShape::Exact { slots: 1 },
        ReturnShape::slots(0),
        ExternEffects::NONE,
    );
    let mut shape_drift = first.clone();
    shape_drift.params = ParamShape::Exact { slots: 2 };
    let error = validate_same_name_module_extern_contracts(&[first.clone(), shape_drift])
        .expect_err("canonical same-name parameter drift must fail");
    assert!(
        error.to_string().contains("incompatible ABI contracts"),
        "{error}"
    );

    let mut effect_drift = first.clone();
    effect_drift.allowed_effects = ExternEffects::MAY_YIELD;
    let error = validate_same_name_module_extern_contracts(&[first.clone(), effect_drift])
        .expect_err("canonical same-name effect drift must fail");
    assert!(
        error.to_string().contains("incompatible ABI contracts"),
        "{error}"
    );

    validate_same_name_module_extern_contracts(&[first.clone(), first])
        .expect("identical canonical contracts are consistent");
}

#[cfg(feature = "std")]
#[test]
fn module_resolution_allows_only_explicit_vm_variable_shape_exceptions() {
    let dyn_call_a = extern_def(
        "dyn_call",
        ParamShape::CallSiteVariadic,
        ReturnShape::slots(4),
        ExternEffects::MAY_CALL_CLOSURE_REPLAY,
    );
    let mut dyn_call_b = dyn_call_a.clone();
    dyn_call_b.returns = ReturnShape::slots(6);
    validate_same_name_module_extern_contracts(&[dyn_call_a, dyn_call_b])
        .expect("dyn_call has a VM-owned variable-shape contract");

    let vo_copy_a = extern_def(
        "vo_copy",
        ParamShape::CallSiteVariadic,
        ReturnShape::slots(1),
        ExternEffects::NONE,
    );
    let mut vo_copy_b = vo_copy_a.clone();
    vo_copy_b.params = ParamShape::Exact { slots: 2 };
    let error = validate_same_name_module_extern_contracts(&[vo_copy_a, vo_copy_b])
        .expect_err("fixed-shape VM helpers cannot drift");
    assert!(
        error.to_string().contains("incompatible ABI contracts"),
        "{error}"
    );
}

#[cfg(feature = "std")]
fn fiber_inputs(
    resume_host_event_token: Option<u64>,
    resume_host_event_data: Option<Vec<u8>>,
) -> ExternFiberInputs {
    ExternFiberInputs {
        fiber_opaque: core::ptr::null_mut(),
        resume_io_token: None,
        resume_host_event_token,
        resume_host_event_data,
        replay_results: Vec::new(),
        replay_panic_message: None,
    }
}

#[cfg(feature = "std")]
fn extern_def(
    name: &str,
    params: ParamShape,
    returns: ReturnShape,
    effects: ExternEffects,
) -> crate::bytecode::ExternDef {
    crate::bytecode::ExternDef {
        name: test_extern_name(name),
        params,
        returns,
        allowed_effects: effects,
        param_kinds: Vec::new(),
    }
}

#[cfg(feature = "std")]
const TEST_MODULE_OWNER: &str = "github.com/volang/runtime-tests";
const TEST_ARTIFACT_GENERATION: u64 = 7;

#[cfg(feature = "std")]
fn test_extern_name(name: &str) -> String {
    if vo_common_core::extern_key::classify_extern_name(name).is_ok() {
        return name.to_string();
    }
    vo_common_core::extern_key::ExternKeyRef::new(TEST_MODULE_OWNER, name)
        .encode()
        .expect("runtime test extern identity must be canonical")
}

#[cfg(feature = "std")]
fn variadic_extern_def(
    name: &str,
    ret_slots: u16,
    effects: ExternEffects,
) -> crate::bytecode::ExternDef {
    extern_def(
        name,
        ParamShape::CallSiteVariadic,
        ReturnShape::slots(ret_slots),
        effects,
    )
}

#[cfg(feature = "std")]
fn math_unary_extern_def(name: &str) -> crate::bytecode::ExternDef {
    extern_def(
        name,
        ParamShape::Exact { slots: 1 },
        ReturnShape::with_slot_types(vec![crate::SlotType::Float]),
        ExternEffects::NONE,
    )
}

#[cfg(feature = "std")]
fn math_unary_extern_def_with_return_layout(
    name: &str,
    slot_types: Vec<crate::SlotType>,
) -> crate::bytecode::ExternDef {
    extern_def(
        name,
        ParamShape::Exact { slots: 1 },
        ReturnShape::with_slot_types(slot_types),
        ExternEffects::NONE,
    )
}

#[cfg(feature = "std")]
fn call_registered_extern(func: ExternFn, inputs: ExternFiberInputs) -> ExternCallOutcome {
    call_registered_extern_with_effects(func, ExternEffects::NONE, inputs)
}

#[cfg(feature = "std")]
fn call_registered_extern_with_effects(
    func: ExternFn,
    effects: ExternEffects,
    inputs: ExternFiberInputs,
) -> ExternCallOutcome {
    let mut registry = ExternRegistry::new();
    registry.register_test_with_effects(0, func, effects);

    let mut stack = [0u64; 4];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots: 0,
    };
    let mut gc = Gc::new();
    let module = Module::new("ffi-post-call-test".to_string());
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        host_services: None,
        io: &mut io,
    };

    registry.call(&mut stack, invoke, world, inputs)
}

#[cfg(feature = "std")]
fn call_registered_extension(func: ExternFnPtr) -> ExternCallOutcome {
    call_registered_extension_with_effects(func, ExternEffects::NONE)
}

#[cfg(feature = "std")]
fn call_registered_extension_with_effects(
    func: ExternFnPtr,
    effects: ExternEffects,
) -> ExternCallOutcome {
    call_registered_extension_with_services(func, effects, None).0
}

#[cfg(feature = "std")]
fn call_registered_extension_with_services(
    func: ExternFnPtr,
    effects: ExternEffects,
    host_services: Option<&dyn crate::host_services::HostServices>,
) -> (ExternCallOutcome, Option<Vec<u8>>) {
    let mut registry = ExternRegistry::new();
    registry.register_extension_with_effects(
        0,
        TEST_MODULE_OWNER,
        test_extern_name("test_extension"),
        func,
        effects,
    );

    let mut stack = [0u64; 4];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots: 0,
    };
    let mut gc = Gc::new();
    let module = Module::new("ffi-extension-result-test".to_string());
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        host_services,
        io: &mut io,
    };

    let outcome = registry.call(&mut stack, invoke, world, fiber_inputs(None, None));
    (outcome, host_output)
}

#[cfg(feature = "std")]
fn call_extension_with_state(
    func: ExternFnPtr,
    stack: &mut [u64],
    invoke: ExternInvoke,
    gc: &mut Gc,
    inputs: ExternFiberInputs,
    host_output: &mut Option<Vec<u8>>,
) -> ExternCallOutcome {
    let mut registry = ExternRegistry::new();
    registry.register_extension_with_effects(
        invoke.extern_id,
        TEST_MODULE_OWNER,
        test_extern_name("test_extension"),
        func,
        ExternEffects::UNKNOWN_CONTROL,
    );
    let module = Module::new("ffi-extension-v9-test".to_string());
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output,
        host_services: None,
        io: &mut io,
    };
    registry.call(stack, invoke, world, inputs)
}

#[cfg(feature = "std")]
extern "C" fn exit_extension(ctx: *mut ExtAbiContextV9) -> u32 {
    // Safety: extension entry points receive the active call context from the
    // registry and retain it only for this call.
    let Ok(mut call) = (unsafe { ExternCallContext::try_from_extension_abi(ctx) }) else {
        return ext_abi::RESULT_ABI_ERROR;
    };
    call.set_ext_exit(-17);
    ext_abi::RESULT_EXIT
}

#[cfg(feature = "std")]
extern "C" fn exit_extension_without_payload(_ctx: *mut ExtAbiContextV9) -> u32 {
    ext_abi::RESULT_EXIT
}

#[cfg(feature = "std")]
extern "C" fn flat_bytes_extension(ctx: *mut ExtAbiContextV9) -> u32 {
    let Ok(mut call) = (unsafe { ExternCallContext::try_from_extension_abi(ctx) }) else {
        return ext_abi::RESULT_ABI_ERROR;
    };
    let first = call.arg_bytes(0);
    let first_ptr = first.as_ptr();
    let second = call.arg_bytes(1);
    assert_eq!(first, [0x11, 0x80, 0xff, 0x42]);
    assert_eq!(second, [0x11, 0x80, 0xff, 0x42]);
    assert_eq!(first.as_ptr(), first_ptr);
    let mut output = first.to_vec();
    output.extend_from_slice(second);
    call.set_host_output(output);
    ext_abi::RESULT_OK
}

#[cfg(feature = "std")]
extern "C" fn gc_proxy_string_extension(ctx: *mut ExtAbiContextV9) -> u32 {
    let Ok(mut call) = (unsafe { ExternCallContext::try_from_extension_abi(ctx) }) else {
        return ext_abi::RESULT_ABI_ERROR;
    };
    let value = crate::objects::string::from_rust_str(call.gc(), "host-owned");
    call.ret_ref(0, value);
    ext_abi::RESULT_OK
}

#[cfg(feature = "std")]
extern "C" fn gc_proxy_value_slots_extension(ctx: *mut ExtAbiContextV9) -> u32 {
    let Ok(mut call) = (unsafe { ExternCallContext::try_from_extension_abi(ctx) }) else {
        return ext_abi::RESULT_ABI_ERROR;
    };
    let value = call
        .gc()
        .alloc_value_slots(ValueMeta::new(0, ValueKind::Int), 1);
    if value.is_null() {
        return ext_abi::RESULT_ABI_ERROR;
    }
    call.ret_ref(0, value);
    ext_abi::RESULT_OK
}

#[cfg(feature = "std")]
extern "C" fn invalid_gc_metadata_extension(ctx: *mut ExtAbiContextV9) -> u32 {
    let Ok(mut call) = (unsafe { ExternCallContext::try_from_extension_abi(ctx) }) else {
        return ext_abi::RESULT_ABI_ERROR;
    };
    let value = call.gc().alloc(ValueMeta::new(7, ValueKind::Struct), 1);
    if value.is_null() {
        ext_abi::RESULT_OK
    } else {
        ext_abi::RESULT_ABI_ERROR
    }
}

#[cfg(feature = "std")]
extern "C" fn forged_gc_allocation_kind_extension(ctx: *mut ExtAbiContextV9) -> u32 {
    let frame = unsafe { &mut *ctx };
    let ops = unsafe { &*frame.ops };
    let value = unsafe {
        (ops.gc_alloc.expect("host table gc_alloc callback"))(
            frame.host,
            ValueMeta::new(0, ValueKind::Int).to_raw(),
            u8::MAX,
            1,
            1,
        )
    };
    if value.is_null() {
        ext_abi::RESULT_OK
    } else {
        ext_abi::RESULT_ABI_ERROR
    }
}

#[cfg(feature = "std")]
extern "C" fn mismatched_gc_allocation_kind_extension(ctx: *mut ExtAbiContextV9) -> u32 {
    let Ok(mut call) = (unsafe { ExternCallContext::try_from_extension_abi(ctx) }) else {
        return ext_abi::RESULT_ABI_ERROR;
    };
    let value = call.gc().alloc_array(ValueMeta::new(0, ValueKind::Int), 2);
    if value.is_null() {
        ext_abi::RESULT_OK
    } else {
        ext_abi::RESULT_ABI_ERROR
    }
}

#[cfg(feature = "std")]
extern "C" fn zero_width_array_extension(ctx: *mut ExtAbiContextV9) -> u32 {
    let Ok(mut call) = (unsafe { ExternCallContext::try_from_extension_abi(ctx) }) else {
        return ext_abi::RESULT_ABI_ERROR;
    };
    let value = call
        .gc()
        .alloc_array(ValueMeta::new(0, ValueKind::Array), 0);
    if value.is_null() {
        ext_abi::RESULT_OK
    } else {
        ext_abi::RESULT_ABI_ERROR
    }
}

#[cfg(feature = "std")]
extern "C" fn gc_proxy_clone_value_slots_extension(ctx: *mut ExtAbiContextV9) -> u32 {
    let Ok(mut call) = (unsafe { ExternCallContext::try_from_extension_abi(ctx) }) else {
        return ext_abi::RESULT_ABI_ERROR;
    };
    let source = call
        .gc()
        .alloc_value_slots(ValueMeta::new(0, ValueKind::Int), 1);
    if source.is_null() {
        return ext_abi::RESULT_ABI_ERROR;
    }
    unsafe { Gc::write_slot(source, 0, 42) };
    let cloned = unsafe { call.gc().ptr_clone(source) };
    if cloned.is_null() {
        return ext_abi::RESULT_ABI_ERROR;
    }
    call.ret_ref(0, cloned);
    ext_abi::RESULT_OK
}

#[cfg(feature = "std")]
extern "C" fn owned_payload_extension(ctx: *mut ExtAbiContextV9) -> u32 {
    let Ok(mut call) = (unsafe { ExternCallContext::try_from_extension_abi(ctx) }) else {
        return ext_abi::RESULT_ABI_ERROR;
    };
    call.set_ext_call_closure(core::ptr::null_mut(), vec![7, 11, 13]);
    ext_abi::RESULT_CALL_CLOSURE
}

#[cfg(feature = "std")]
extern "C" fn panic_payload_extension(ctx: *mut ExtAbiContextV9) -> u32 {
    let Ok(mut call) = (unsafe { ExternCallContext::try_from_extension_abi(ctx) }) else {
        return ext_abi::RESULT_ABI_ERROR;
    };
    call.set_ext_panic(String::from("extension-owned panic payload"));
    ext_abi::RESULT_PANIC
}

#[cfg(feature = "std")]
extern "C" fn resume_data_extension(ctx: *mut ExtAbiContextV9) -> u32 {
    let Ok(mut call) = (unsafe { ExternCallContext::try_from_extension_abi(ctx) }) else {
        return ext_abi::RESULT_ABI_ERROR;
    };
    let Some(mut data) = call.take_resume_host_event_data() else {
        return ext_abi::RESULT_ABI_ERROR;
    };
    data.push(0x7f);
    call.set_host_output(data);
    ext_abi::RESULT_OK
}

#[cfg(feature = "std")]
extern "C" fn invalid_slot_window_helpers_extension(ctx: *mut ExtAbiContextV9) -> u32 {
    let Ok(mut call) = (unsafe { ExternCallContext::try_from_extension_abi(ctx) }) else {
        return ext_abi::RESULT_ABI_ERROR;
    };
    assert_eq!(call.arg_u64(call.arg_count()), 0);
    let any = call.arg_any(u16::MAX);
    assert_eq!((any.slot0, any.slot1), (0, 0));
    call.ret_u64(call.ret_slots(), 0xfeed);
    call.ret_any(u16::MAX, InterfaceSlot::nil());
    ext_abi::RESULT_OK
}

#[cfg(feature = "std")]
extern "C" fn valid_ret_any_extension(ctx: *mut ExtAbiContextV9) -> u32 {
    let Ok(mut call) = (unsafe { ExternCallContext::try_from_extension_abi(ctx) }) else {
        return ext_abi::RESULT_ABI_ERROR;
    };
    call.ret_any(
        0,
        InterfaceSlot {
            slot0: 0x1122,
            slot1: 0x3344,
        },
    );
    ext_abi::RESULT_OK
}

#[cfg(feature = "std")]
extern "C" fn rejected_map_extension(ctx: *mut ExtAbiContextV9) -> u32 {
    let Ok(mut call) = (unsafe { ExternCallContext::try_from_extension_abi(ctx) }) else {
        return ext_abi::RESULT_ABI_ERROR;
    };
    let map = call.alloc_map(
        ValueMeta::new(0, ValueKind::String),
        ValueMeta::new(0, ValueKind::Int),
        1,
        1,
        0,
    );
    if map.is_null() {
        ext_abi::RESULT_OK
    } else {
        ext_abi::RESULT_ABI_ERROR
    }
}

#[cfg(feature = "std")]
extern "C" fn callback_panic_extension(ctx: *mut ExtAbiContextV9) -> u32 {
    let frame = unsafe { &mut *ctx };
    let ops = unsafe { &*frame.ops };
    unsafe {
        (ops.set_host_output
            .expect("host table set_host_output callback"))(frame.host, core::ptr::null(), 1)
    };
    ext_abi::RESULT_OK
}

#[cfg(feature = "std")]
extern "C" fn abi_error_extension(_ctx: *mut ExtAbiContextV9) -> u32 {
    ext_abi::RESULT_ABI_ERROR
}

#[cfg(feature = "std")]
extern "C" fn host_services_extension(ctx: *mut ExtAbiContextV9) -> u32 {
    let Ok(mut call) = (unsafe { ExternCallContext::try_from_extension_abi(ctx) }) else {
        return ext_abi::RESULT_ABI_ERROR;
    };
    let observed = vec![
        u8::from(crate::host_services::has_capability("sync")),
        u8::from(crate::host_services::has_capability("alpha")),
        u8::from(crate::host_services::has_capability("beta")),
    ];
    crate::host_services::start_timeout(11, 12);
    crate::host_services::clear_timeout(11);
    crate::host_services::start_interval(21, 22);
    crate::host_services::clear_interval(21);
    crate::host_services::start_tick_loop(31);
    crate::host_services::stop_tick_loop(31);
    call.set_host_output(observed);
    ext_abi::RESULT_OK
}

#[cfg(feature = "std")]
struct RecordingHostServices {
    capability: &'static str,
    barrier: Option<std::sync::Arc<std::sync::Barrier>>,
    calls: std::sync::Mutex<Vec<(&'static str, i32, i32)>>,
}

#[cfg(feature = "std")]
impl crate::host_services::HostServices for RecordingHostServices {
    fn has_capability(&self, name: &str) -> bool {
        if name == "sync" {
            if let Some(barrier) = &self.barrier {
                barrier.wait();
            }
            return true;
        }
        name == self.capability
    }

    fn start_timeout(&self, id: i32, ms: i32) -> bool {
        self.calls.lock().unwrap().push(("start_timeout", id, ms));
        true
    }

    fn clear_timeout(&self, id: i32) -> bool {
        self.calls.lock().unwrap().push(("clear_timeout", id, 0));
        true
    }

    fn start_interval(&self, id: i32, ms: i32) -> bool {
        self.calls.lock().unwrap().push(("start_interval", id, ms));
        true
    }

    fn clear_interval(&self, id: i32) -> bool {
        self.calls.lock().unwrap().push(("clear_interval", id, 0));
        true
    }

    fn start_tick_loop(&self, id: i32) -> bool {
        self.calls.lock().unwrap().push(("start_tick_loop", id, 0));
        true
    }

    fn stop_tick_loop(&self, id: i32) -> bool {
        self.calls.lock().unwrap().push(("stop_tick_loop", id, 0));
        true
    }
}

#[cfg(feature = "std")]
struct PanickingHostServices;

#[cfg(feature = "std")]
impl crate::host_services::HostServices for PanickingHostServices {
    fn start_tick_loop(&self, _id: i32) -> bool {
        panic!("provider panic must remain inside the host callback")
    }
}

#[cfg(feature = "std")]
extern "C" fn panicking_host_services_extension(ctx: *mut ExtAbiContextV9) -> u32 {
    let Ok(_call) = (unsafe { ExternCallContext::try_from_extension_abi(ctx) }) else {
        return ext_abi::RESULT_ABI_ERROR;
    };
    crate::host_services::start_tick_loop(7);
    ext_abi::RESULT_OK
}

#[cfg(feature = "std")]
fn test_abi_frame(ops: *const ExtHostOpsV9) -> ExtAbiContextV9 {
    ExtAbiContextV9 {
        version: EXTENSION_ABI_VERSION,
        size: core::mem::size_of::<ExtAbiContextV9>() as u32,
        host: core::ptr::NonNull::<u8>::dangling().as_ptr().cast(),
        ops,
        stack: core::ptr::NonNull::<u64>::dangling().as_ptr(),
        stack_len: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots: 0,
        extern_id: 0,
    }
}

#[cfg(feature = "std")]
#[derive(Default)]
struct ContractErrorCapture {
    messages: Vec<String>,
}

#[cfg(feature = "std")]
unsafe extern "C" fn capture_contract_error(
    host: *mut core::ffi::c_void,
    ptr: *const u8,
    len: usize,
) {
    let capture = unsafe { &mut *host.cast::<ContractErrorCapture>() };
    let bytes = if len == 0 {
        &[]
    } else {
        unsafe { core::slice::from_raw_parts(ptr, len) }
    };
    capture
        .messages
        .push(String::from_utf8_lossy(bytes).into_owned());
}

#[cfg(feature = "std")]
#[test]
fn extension_host_services_are_isolated_across_concurrent_vm_calls() {
    let barrier = std::sync::Arc::new(std::sync::Barrier::new(2));
    let alpha = std::sync::Arc::new(RecordingHostServices {
        capability: "alpha",
        barrier: Some(std::sync::Arc::clone(&barrier)),
        calls: std::sync::Mutex::new(Vec::new()),
    });
    let beta = std::sync::Arc::new(RecordingHostServices {
        capability: "beta",
        barrier: Some(barrier),
        calls: std::sync::Mutex::new(Vec::new()),
    });

    let alpha_call = {
        let services = std::sync::Arc::clone(&alpha);
        std::thread::spawn(move || {
            let (outcome, output) = call_registered_extension_with_services(
                host_services_extension,
                ExternEffects::NONE,
                Some(services.as_ref()),
            );
            (matches!(outcome, Ok(ExternResult::Ok)), output)
        })
    };
    let beta_call = {
        let services = std::sync::Arc::clone(&beta);
        std::thread::spawn(move || {
            let (outcome, output) = call_registered_extension_with_services(
                host_services_extension,
                ExternEffects::NONE,
                Some(services.as_ref()),
            );
            (matches!(outcome, Ok(ExternResult::Ok)), output)
        })
    };

    let (alpha_succeeded, alpha_output) = alpha_call.join().expect("alpha VM thread");
    let (beta_succeeded, beta_output) = beta_call.join().expect("beta VM thread");
    assert!(alpha_succeeded);
    assert!(beta_succeeded);
    assert_eq!(alpha_output, Some(vec![1, 1, 0]));
    assert_eq!(beta_output, Some(vec![1, 0, 1]));

    let expected = [
        ("start_timeout", 11, 12),
        ("clear_timeout", 11, 0),
        ("start_interval", 21, 22),
        ("clear_interval", 21, 0),
        ("start_tick_loop", 31, 0),
        ("stop_tick_loop", 31, 0),
    ];
    assert_eq!(alpha.calls.lock().unwrap().as_slice(), expected);
    assert_eq!(beta.calls.lock().unwrap().as_slice(), expected);
}

#[cfg(feature = "std")]
#[test]
fn extension_host_services_are_safe_and_inert_when_unconfigured() {
    let (outcome, output) =
        call_registered_extension_with_services(host_services_extension, ExternEffects::NONE, None);
    assert!(matches!(outcome, Ok(ExternResult::Ok)));
    assert_eq!(output, Some(vec![0, 0, 0]));
}

#[cfg(feature = "std")]
#[test]
fn host_service_provider_panics_become_contract_errors() {
    let (outcome, _) = call_registered_extension_with_services(
        panicking_host_services_extension,
        ExternEffects::NONE,
        Some(&PanickingHostServices),
    );
    let error = outcome.expect_err("provider panic must fail the extension call");
    assert!(error.to_string().contains("host callback panicked"));
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_frame_validation_reads_only_the_available_header() {
    let mut short_frame = [
        EXTENSION_ABI_VERSION,
        core::mem::size_of_val(&[0_u32; 2]) as u32,
    ];
    let result =
        unsafe { ExternCallContext::try_from_extension_abi(short_frame.as_mut_ptr().cast()) };
    assert!(matches!(
        result,
        Err(ExtensionAbiInitError::ContextTooSmall { found: 8 })
    ));

    let mut frame = test_abi_frame(core::ptr::null());
    let result = unsafe { ExternCallContext::try_from_extension_abi(&mut frame) };
    assert!(matches!(result, Err(ExtensionAbiInitError::NullOps)));

    let ops_header = [EXTENSION_ABI_VERSION, 8_u32];
    frame.ops = ops_header.as_ptr().cast();
    let result = unsafe { ExternCallContext::try_from_extension_abi(&mut frame) };
    assert!(matches!(
        result,
        Err(ExtensionAbiInitError::OpsTooSmall { found: 8 })
    ));
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_frame_validation_rejects_invalid_stack_windows() {
    let mut stack = [0_u64; 1];
    let mut frame = test_abi_frame(&native_abi_v9::OPS);
    frame.stack = stack.as_mut_ptr();
    frame.stack_len = stack.len();
    frame.bp = 1;
    frame.arg_slots = 1;

    let result = unsafe { ExternCallContext::try_from_extension_abi(&mut frame) };
    assert!(matches!(
        result,
        Err(ExtensionAbiInitError::InvalidStackWindow)
    ));
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_frame_validation_rejects_unaddressable_u16_slot_windows() {
    let mut stack = vec![0_u64; usize::from(u16::MAX) + 2];
    let mut frame = test_abi_frame(&native_abi_v9::OPS);
    frame.stack = stack.as_mut_ptr();
    frame.stack_len = stack.len();
    frame.arg_start = u16::MAX;
    frame.arg_slots = 2;

    let result = unsafe { ExternCallContext::try_from_extension_abi(&mut frame) };
    assert!(matches!(
        result,
        Err(ExtensionAbiInitError::UnaddressableSlotWindow { window: "argument" })
    ));

    frame.arg_slots = 0;
    frame.ret_start = u16::MAX;
    frame.ret_slots = 2;
    let result = unsafe { ExternCallContext::try_from_extension_abi(&mut frame) };
    assert!(matches!(
        result,
        Err(ExtensionAbiInitError::UnaddressableSlotWindow { window: "return" })
    ));
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_helpers_accept_the_last_u16_slot_and_reject_the_next_slot() {
    let mut stack = vec![0_u64; usize::from(u16::MAX) + 1];
    stack[usize::from(u16::MAX)] = 0xaabb_ccdd;
    let mut capture = ContractErrorCapture::default();
    let mut ops = native_abi_v9::OPS;
    ops.record_contract_error = Some(capture_contract_error);
    let mut frame = test_abi_frame(&ops);
    frame.host = (&mut capture as *mut ContractErrorCapture).cast();
    frame.stack = stack.as_mut_ptr();
    frame.stack_len = stack.len();
    frame.arg_start = u16::MAX;
    frame.arg_slots = 1;
    frame.ret_start = u16::MAX;
    frame.ret_slots = 1;

    let mut call = unsafe { ExternCallContext::try_from_extension_abi(&mut frame) }
        .expect("the final u16-addressable slot must form a valid one-slot window");
    assert_eq!(call.arg_u64(0), 0xaabb_ccdd);
    assert_eq!(call.arg_u64(1), 0);
    call.ret_u64(0, 0x55aa);
    call.ret_u64(1, 0xffff);
    call.ret_any(0, InterfaceSlot::nil());
    drop(call);

    assert_eq!(stack[usize::from(u16::MAX)], 0x55aa);
    assert!(capture
        .messages
        .iter()
        .any(|message| message.contains("outside declared arg_slots 1")));
    assert!(capture
        .messages
        .iter()
        .any(|message| message.contains("outside declared ret_slots 1")));
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_arg_and_return_helpers_fail_closed_without_panicking() {
    let error = call_registered_extension(invalid_slot_window_helpers_extension)
        .expect_err("out-of-window helpers must reject the extension call");
    assert!(error.message().contains("outside declared arg_slots 0"));
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_ret_any_writes_two_slots_through_the_facade() {
    let mut gc = Gc::new();
    let mut stack = [0_u64; 2];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots: 2,
    };
    let mut output = None;
    let outcome = call_extension_with_state(
        valid_ret_any_extension,
        &mut stack,
        invoke,
        &mut gc,
        fiber_inputs(None, None),
        &mut output,
    );

    assert!(matches!(outcome, Ok(ExternResult::Ok)));
    assert_eq!(stack, [0x1122, 0x3344]);
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_frame_validation_rejects_incompatible_host_service_tables() {
    let mut ops = native_abi_v9::OPS;
    ops.host_services.version = crate::host_services::EXT_HOST_SERVICES_VERSION + 1;
    let mut frame = test_abi_frame(&ops);
    let result = unsafe { ExternCallContext::try_from_extension_abi(&mut frame) };
    assert!(matches!(
        result,
        Err(ExtensionAbiInitError::UnsupportedHostServicesVersion { found })
            if found == crate::host_services::EXT_HOST_SERVICES_VERSION + 1
    ));

    ops = native_abi_v9::OPS;
    ops.host_services.size = 8;
    frame.ops = &ops;
    let result = unsafe { ExternCallContext::try_from_extension_abi(&mut frame) };
    assert!(matches!(
        result,
        Err(ExtensionAbiInitError::HostServicesTooSmall { found: 8 })
    ));
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_frame_validation_rejects_null_required_callbacks() {
    let mut ops = native_abi_v9::OPS;
    ops.gc_alloc = None;
    let mut frame = test_abi_frame(&ops);
    let result = unsafe { ExternCallContext::try_from_extension_abi(&mut frame) };
    assert!(matches!(
        result,
        Err(ExtensionAbiInitError::MissingOpsCallback { name: "gc_alloc" })
    ));

    ops = native_abi_v9::OPS;
    ops.host_services.start_tick_loop = None;
    frame.ops = &ops;
    let result = unsafe { ExternCallContext::try_from_extension_abi(&mut frame) };
    assert!(matches!(
        result,
        Err(ExtensionAbiInitError::MissingHostServicesCallback {
            name: "start_tick_loop"
        })
    ));
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_abi_error_code_is_a_contract_failure() {
    let error = call_registered_extension(abi_error_extension)
        .expect_err("trampoline ABI errors must fail the call");
    assert!(error
        .message()
        .contains("trampoline rejected its ABI frame"));
}

#[cfg(feature = "std")]
#[test]
fn extension_exit_result_round_trips_signed_status() {
    assert!(matches!(
        call_registered_extension_with_effects(exit_extension, ExternEffects::MAY_EXIT),
        Ok(ExternResult::Exit(-17))
    ));
}

#[cfg(feature = "std")]
#[test]
fn extension_exit_result_requires_declared_exit_effect() {
    let error = call_registered_extension(exit_extension)
        .expect_err("exit must be covered by provider effect metadata");
    assert!(error.message().contains("outside resolved effects"));
}

#[cfg(feature = "std")]
#[test]
fn extension_exit_result_requires_explicit_payload() {
    let error = call_registered_extension(exit_extension_without_payload)
        .expect_err("missing exit payload must violate the extension contract");
    assert_eq!(
        error.message(),
        "ext_abi::RESULT_EXIT without set_ext_exit payload"
    );
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_materializes_flat_bytes_in_host_owned_stable_slots() {
    let mut gc = Gc::new();
    let owner = gc.alloc(ValueMeta::new(0, ValueKind::Struct), 4);
    for (index, byte) in [0x11_u64, 0x80, 0xff, 0x42].into_iter().enumerate() {
        unsafe { Gc::write_slot(owner, index, byte) };
    }
    let view = unsafe {
        crate::objects::slice::from_inline_array_range_with_cap(
            &mut gc,
            owner,
            owner.cast::<u8>(),
            4,
            0,
            4,
            4,
            ValueMeta::new(0, ValueKind::Uint8),
            1,
            crate::slot::SLOT_BYTES,
        )
    };
    let mut stack = [view as u64, view as u64];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 2,
        ret_start: 2,
        ret_slots: 0,
    };
    let mut output = None;
    let outcome = call_extension_with_state(
        flat_bytes_extension,
        &mut stack,
        invoke,
        &mut gc,
        fiber_inputs(None, None),
        &mut output,
    );
    assert!(matches!(outcome, Ok(ExternResult::Ok)));
    assert_eq!(
        output,
        Some(vec![0x11, 0x80, 0xff, 0x42, 0x11, 0x80, 0xff, 0x42])
    );
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_gc_proxy_allocates_string_in_host_collector() {
    let mut gc = Gc::new();
    let mut stack = [0u64; 1];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots: 1,
    };
    let mut output = None;
    let outcome = call_extension_with_state(
        gc_proxy_string_extension,
        &mut stack,
        invoke,
        &mut gc,
        fiber_inputs(None, None),
        &mut output,
    );
    assert!(matches!(outcome, Ok(ExternResult::Ok)));
    let value = stack[0] as GcRef;
    assert_eq!(gc.canonicalize_ref(value), Some(value));
    assert_eq!(
        unsafe { crate::objects::string::to_bytes(value) },
        b"host-owned"
    );
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_value_slot_boxes_are_tagged_by_the_host_allocator() {
    let mut gc = Gc::new();
    let mut stack = [0_u64; 1];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots: 1,
    };
    let mut output = None;
    let outcome = call_extension_with_state(
        gc_proxy_value_slots_extension,
        &mut stack,
        invoke,
        &mut gc,
        fiber_inputs(None, None),
        &mut output,
    );
    assert!(matches!(outcome, Ok(ExternResult::Ok)));
    let value = stack[0] as GcRef;
    assert_eq!(gc.canonicalize_ref(value), Some(value));
    assert!(unsafe { Gc::header(value) }.is_value_slots_object());
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_gc_proxy_rejects_missing_module_metadata() {
    let error = call_registered_extension(invalid_gc_metadata_extension)
        .expect_err("host allocator must reject extension-supplied metadata drift");
    assert!(error.message().contains("missing struct metadata id 7"));
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_gc_proxy_rejects_forged_allocation_kinds() {
    let error = call_registered_extension(forged_gc_allocation_kind_extension)
        .expect_err("host allocator must reject unknown allocation kinds");
    assert!(error.message().contains("unknown allocation kind 255"));
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_gc_proxy_rejects_kind_and_metadata_mismatch() {
    let error = call_registered_extension(mismatched_gc_allocation_kind_extension)
        .expect_err("host allocator must reject array mode for scalar metadata");
    assert!(error
        .message()
        .contains("array allocation received Int metadata"));
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_gc_proxy_rejects_zero_width_canonical_arrays() {
    let error = call_registered_extension(zero_width_array_extension)
        .expect_err("canonical arrays must contain their descriptor header");
    assert!(error.message().contains("smaller than its 2-slot header"));
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_gc_proxy_clone_survives_and_reclaims_in_host_collector() {
    fn complete_cycle(gc: &mut Gc, root: Option<GcRef>) {
        for _ in 0..64 {
            unsafe {
                gc.step(
                    |gc| {
                        if let Some(root) = root {
                            gc.mark_gray(root);
                        }
                    },
                    |_gc, _object| {},
                    |_dead| {},
                );
            }
            if gc.state() == crate::gc::GcState::Pause {
                return;
            }
        }
        panic!("GC cycle did not converge");
    }

    let mut gc = Gc::new();
    let mut stack = [0_u64; 1];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots: 1,
    };
    let mut output = None;
    let outcome = call_extension_with_state(
        gc_proxy_clone_value_slots_extension,
        &mut stack,
        invoke,
        &mut gc,
        fiber_inputs(None, None),
        &mut output,
    );
    assert!(matches!(outcome, Ok(ExternResult::Ok)));

    let cloned = stack[0] as GcRef;
    assert!(unsafe { Gc::header(cloned) }.is_value_slots_object());
    assert_eq!(unsafe { Gc::read_slot(cloned, 0) }, 42);
    complete_cycle(&mut gc, Some(cloned));
    assert_eq!(gc.canonicalize_ref(cloned), Some(cloned));

    complete_cycle(&mut gc, None);
    assert_eq!(gc.canonicalize_ref(cloned), None);
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_copies_call_closure_payload_into_host_storage() {
    let result = call_registered_extension_with_effects(
        owned_payload_extension,
        ExternEffects::MAY_CALL_CLOSURE_REPLAY,
    )
    .expect("allocator-neutral closure payload must decode");
    assert!(matches!(
        result,
        ExternResult::CallClosure { args, .. } if args == [7, 11, 13]
    ));
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_copies_panic_payload_into_host_storage() {
    let result = call_registered_extension(panic_payload_extension)
        .expect("allocator-neutral panic payload must decode");
    assert!(matches!(
        result,
        ExternResult::Panic(message) if message == "extension-owned panic payload"
    ));
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_copies_host_resume_data_in_both_directions() {
    let mut gc = Gc::new();
    let mut stack = [];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots: 0,
    };
    let mut output = None;
    let outcome = call_extension_with_state(
        resume_data_extension,
        &mut stack,
        invoke,
        &mut gc,
        fiber_inputs(None, Some(vec![0x00, 0x80, 0xff])),
        &mut output,
    );
    assert!(matches!(outcome, Ok(ExternResult::Ok)));
    assert_eq!(output, Some(vec![0x00, 0x80, 0xff, 0x7f]));
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_rejects_allocator_specific_map_access() {
    let error = call_registered_extension(rejected_map_extension)
        .expect_err("map allocation must remain host-image-only");
    assert!(error.message().contains("does not expose map allocation"));
}

#[cfg(feature = "std")]
#[test]
fn extension_v9_catches_host_callback_panics_before_c_boundary() {
    let error = call_registered_extension(callback_panic_extension)
        .expect_err("callback panic must become a contract error");
    assert_eq!(
        error.message(),
        "native extension host callback panicked; call was rejected"
    );
}

#[cfg(feature = "std")]
fn resolve_single_extern(
    registry: &ExternRegistry,
    def: crate::bytecode::ExternDef,
) -> ResolvedExtern {
    registry
        .resolve_module_externs(&[def])
        .expect("resolve test extern")
        .get(0)
        .expect("resolved extern")
        .clone()
}

#[cfg(feature = "std")]
fn call_resolved_extern_with_stack(
    registry: &ExternRegistry,
    resolved: &ResolvedExtern,
    stack: &mut [u64],
    invoke: ExternInvoke,
) -> ExternCallOutcome {
    let mut gc = Gc::new();
    let module = Module::new("ffi-resolved-call-test".to_string());
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        host_services: None,
        io: &mut io,
    };

    registry.call_resolved(stack, invoke, world, fiber_inputs(None, None), resolved)
}

#[cfg(feature = "std")]
fn call_unresolved_extern_with_stack(
    registry: &ExternRegistry,
    stack: &mut [u64],
    invoke: ExternInvoke,
) -> ExternCallOutcome {
    let mut gc = Gc::new();
    let module = Module::new("ffi-unresolved-call-test".to_string());
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        host_services: None,
        io: &mut io,
    };

    registry.call(stack, invoke, world, fiber_inputs(None, None))
}

#[cfg(feature = "std")]
fn invoke_with_returns(ret_slots: u16) -> ExternInvoke {
    ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots,
    }
}

#[cfg(feature = "std")]
fn empty_interface_return_shape() -> ReturnShape {
    ReturnShape::try_with_slot_types_and_interface_metas(
        vec![crate::SlotType::Interface0, crate::SlotType::Interface1],
        vec![Some(0), None],
    )
    .expect("interface return shape")
}

#[cfg(feature = "std")]
fn call_empty_interface_return_provider(
    func: ExternFn,
    name: &str,
    module: &Module,
) -> ExternCallOutcome {
    let mut registry = ExternRegistry::new();
    registry.register_test_named(0, test_extern_name(name), func);
    let resolved = resolve_single_extern(
        &registry,
        extern_def(
            name,
            ParamShape::Exact { slots: 0 },
            empty_interface_return_shape(),
            ExternEffects::NONE,
        ),
    );
    let mut stack = [0u64; 4];
    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        host_services: None,
        io: &mut io,
    };

    registry.call_resolved(
        &mut stack,
        invoke_with_returns(2),
        world,
        fiber_inputs(None, None),
        &resolved,
    )
}

#[cfg(feature = "std")]
fn call_module_metadata_provider(func: ExternFn, module: &Module) -> ExternCallOutcome {
    let mut registry = ExternRegistry::new();
    registry.register_test_named(0, test_extern_name("metadata_provider"), func);
    let mut stack = [0u64; 1];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots: 0,
    };
    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        host_services: None,
        io: &mut io,
    };

    registry.call(&mut stack, invoke, world, fiber_inputs(None, None))
}

#[cfg(feature = "std")]
fn ignore_host_event_resume(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ExternResult::Ok
}

#[cfg(feature = "std")]
static DIRECT_WASM_BRIDGE_PROVIDER_RAN_061: core::sync::atomic::AtomicBool =
    core::sync::atomic::AtomicBool::new(false);

#[cfg(feature = "std")]
fn direct_wasm_bridge_provider_061(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    DIRECT_WASM_BRIDGE_PROVIDER_RAN_061.store(true, core::sync::atomic::Ordering::SeqCst);
    ExternResult::Ok
}

#[cfg(feature = "std")]
static RESOLVED_WASM_BRIDGE_PROVIDER_RAN_061: core::sync::atomic::AtomicBool =
    core::sync::atomic::AtomicBool::new(false);

#[cfg(feature = "std")]
fn resolved_wasm_bridge_provider_061(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    RESOLVED_WASM_BRIDGE_PROVIDER_RAN_061.store(true, core::sync::atomic::Ordering::SeqCst);
    ExternResult::Ok
}

#[cfg(feature = "std")]
static RESOLVED_WASM_BRIDGE_CONTEXT_ABI_OK_061: core::sync::atomic::AtomicBool =
    core::sync::atomic::AtomicBool::new(false);

#[cfg(feature = "std")]
fn resolved_wasm_bridge_context_provider_061(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let (name, module_owner, artifact_generation, param_kinds) = ctx
        .wasm_extension_bridge_abi()
        .expect("wasm bridge call must bind resolved ABI to the call context");
    assert_eq!(name, test_extern_name("bridge_context"));
    assert_eq!(module_owner, TEST_MODULE_OWNER);
    assert_eq!(artifact_generation, TEST_ARTIFACT_GENERATION);
    assert_eq!(
        param_kinds,
        &[
            crate::bytecode::ExtSlotKind::Bytes,
            crate::bytecode::ExtSlotKind::Value
        ]
    );
    RESOLVED_WASM_BRIDGE_CONTEXT_ABI_OK_061.store(true, core::sync::atomic::Ordering::SeqCst);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn other_ok_extern(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn consume_host_event_resume(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    assert_eq!(ctx.take_resume_host_event_token(), Some(77));
    assert_eq!(ctx.take_resume_host_event_data(), Some(vec![1, 2, 3]));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn yield_extern(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ExternResult::Yield
}

#[cfg(feature = "std")]
fn write_ret_u64_slot_one(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ctx.ret_u64(1, 99);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn write_ret_any_into_one_slot(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ctx.ret_any(0, InterfaceSlot::nil());
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn read_outside_empty_argument_window(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    assert_eq!(ctx.arg_i64(0), 0);
    let any = ctx.arg_any(u16::MAX);
    assert_eq!((any.slot0, any.slot1), (0, 0));
    assert_eq!(ctx.arg_str(0), "");
    assert_eq!(ctx.arg_bytes(0), &[]);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_invalid_gc_ref(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ctx.ret_u64(0, 0xdead_beef);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn write_invalid_gc_ref_then_yield(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ctx.ret_u64(0, 0xdead_beef);
    ExternResult::Yield
}

#[cfg(feature = "std")]
fn write_invalid_gc_ref_then_contract_error(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ctx.ret_u64(0, 0xdead_beef);
    ctx.record_contract_violation("synthetic post-call contract error");
    ExternResult::Yield
}

#[cfg(feature = "std")]
fn return_invalid_interface_ref(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let slot0 = crate::objects::interface::pack_slot0(0, 0, ValueKind::String);
    ctx.ret_interface_pair(0, (slot0, 0xdead_beef));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_invalid_interface_kind(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ctx.ret_interface_pair(0, (0xff, 0));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_reserved_interface_rttid(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let slot0 = (u64::from(vo_common_core::types::INVALID_META_ID) << 8) | ValueKind::String as u64;
    ctx.ret_interface_pair(0, (slot0, 0));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_noncanonical_nil_interface(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let slot0 = crate::objects::interface::pack_slot0(7, 42, ValueKind::Void);
    ctx.ret_interface_pair(0, (slot0, 0));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_null_struct_interface_data(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let slot0 = crate::objects::interface::pack_slot0(0, 0, ValueKind::Struct);
    ctx.ret_interface_pair(0, (slot0, 0));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_null_array_interface_data(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let slot0 = crate::objects::interface::pack_slot0(0, 1, ValueKind::Array);
    ctx.ret_interface_pair(0, (slot0, 0));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_interior_struct_interface_data(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let data = ctx.gc().alloc(ValueMeta::new(0, ValueKind::Struct), 2);
    let interior = unsafe { data.add(1) };
    let slot0 = crate::objects::interface::pack_slot0(0, 0, ValueKind::Struct);
    ctx.ret_interface_pair(0, (slot0, interior as u64));
    ctx.ret_u64(2, data as u64);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_empty_interface_with_concrete_itab(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let data = string::new_from_string(ctx.gc(), "value".to_string());
    let slot0 = crate::objects::interface::pack_slot0(1, 0, ValueKind::String);
    ctx.ret_interface_pair(0, (slot0, data as u64));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_wrong_kind_interface_data(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let slot0 = crate::objects::interface::pack_slot0(0, 0, ValueKind::String);
    let data =
        crate::objects::slice::create(ctx.gc(), ValueMeta::new(0, ValueKind::Int64), 8, 0, 0);
    ctx.ret_interface_pair(0, (slot0, data as u64));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_wrong_meta_interface_data(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let slot0 = crate::objects::interface::pack_slot0(0, 0, ValueKind::Struct);
    let data = ctx.gc().alloc(ValueMeta::new(1, ValueKind::Struct), 2);
    ctx.ret_interface_pair(0, (slot0, data as u64));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_undersized_struct_interface_data(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let slot0 = crate::objects::interface::pack_slot0(0, 0, ValueKind::Struct);
    let data = ctx.gc().alloc(ValueMeta::new(0, ValueKind::Struct), 1);
    ctx.ret_interface_pair(0, (slot0, data as u64));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_undersized_array_box_interface_data(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let slot0 = crate::objects::interface::pack_slot0(0, 1, ValueKind::Array);
    let data = ctx.gc().alloc(ValueMeta::new(0, ValueKind::Struct), 1);
    ctx.ret_interface_pair(0, (slot0, data as u64));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn return_wrong_signature_interface(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    let slot0 = crate::objects::interface::pack_slot0(1, 1, ValueKind::Int64);
    ctx.ret_interface_pair(0, (slot0, 123));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn ignore_resume_io_and_wait_again(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ExternResult::WaitIo { token: 99 }
}

#[cfg(feature = "std")]
fn ignore_resume_and_call_closure(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
    ExternResult::CallClosure {
        closure_ref: core::ptr::null_mut(),
        args: Vec::new(),
    }
}

#[cfg(feature = "std")]
fn consume_one_replay_then_host_replay(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    assert_eq!(ctx.resume_closure_result(), Some(vec![11]));
    ExternResult::HostEventWaitAndReplay {
        token: 55,
        source: HostEventReplaySource::Extension,
    }
}

#[cfg(feature = "std")]
fn consume_one_replay_then_ok(ctx: &mut ExternCallContext<'_>) -> ExternResult {
    assert_eq!(ctx.resume_closure_result(), Some(vec![11]));
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn replay_results(values: &[u64]) -> Vec<ExternReplayResult> {
    values
        .iter()
        .map(|value| ExternReplayResult::new(vec![*value], vec![crate::SlotType::Value]))
        .collect()
}

#[cfg(feature = "std")]
#[test]
fn ffi_wait_io_replay_rejects_unconsumed_resume_token() {
    let mut inputs = fiber_inputs(None, None);
    inputs.resume_io_token = Some(7);

    let result = call_registered_extern_with_effects(
        ignore_resume_io_and_wait_again,
        ExternEffects::MAY_WAIT_IO_REPLAY,
        inputs,
    );

    assert!(matches!(result, Err(err) if err.to_string().contains("resume_io_token")));
}

#[cfg(feature = "std")]
#[test]
fn ffi_call_closure_replay_rejects_unconsumed_resume_io_token() {
    let mut inputs = fiber_inputs(None, None);
    inputs.resume_io_token = Some(7);

    let result = call_registered_extern_with_effects(
        ignore_resume_and_call_closure,
        ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        inputs,
    );

    assert!(matches!(result, Err(err) if err.to_string().contains("resume_io_token")));
}

#[cfg(feature = "std")]
#[test]
fn ffi_call_closure_replay_rejects_unconsumed_host_event_token() {
    let result = call_registered_extern_with_effects(
        ignore_resume_and_call_closure,
        ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        fiber_inputs(Some(77), None),
    );

    assert!(matches!(result, Err(err) if err.to_string().contains("resume_host_event_token")));
}

#[cfg(feature = "std")]
#[test]
fn ffi_call_closure_replay_rejects_unconsumed_host_event_data() {
    let result = call_registered_extern_with_effects(
        ignore_resume_and_call_closure,
        ExternEffects::MAY_CALL_CLOSURE_REPLAY,
        fiber_inputs(None, Some(vec![1])),
    );

    assert!(matches!(result, Err(err) if err.to_string().contains("resume_host_event_data")));
}

#[cfg(feature = "std")]
#[test]
fn ffi_host_event_replay_allows_partial_closure_replay() {
    let mut inputs = fiber_inputs(None, None);
    inputs.replay_results = replay_results(&[11, 22]);

    let result = call_registered_extern_with_effects(
        consume_one_replay_then_host_replay,
        ExternEffects::MAY_HOST_REPLAY,
        inputs,
    );

    assert!(matches!(
        result,
        Ok(ExternResult::HostEventWaitAndReplay {
            token: 55,
            source: HostEventReplaySource::Extension,
        })
    ));
}

#[cfg(feature = "std")]
#[test]
fn ffi_terminal_result_rejects_partial_closure_replay() {
    let mut inputs = fiber_inputs(None, None);
    inputs.replay_results = replay_results(&[11, 22]);

    let result = call_registered_extern_with_effects(
        consume_one_replay_then_ok,
        ExternEffects::NONE,
        inputs,
    );

    assert!(matches!(result, Err(err) if err.to_string().contains("replay_index")));
}

#[cfg(feature = "std")]
#[test]
fn resolve_module_externs_rejects_provider_effects_outside_module_contract() {
    let mut registry = ExternRegistry::new();
    registry.register_named_with_effects(
        0,
        test_extern_name("contract_yield"),
        yield_extern,
        ExternEffects::MAY_YIELD,
    );
    let externs = vec![variadic_extern_def(
        "contract_yield",
        0,
        ExternEffects::NONE,
    )];

    let err = registry
        .resolve_module_externs(&externs)
        .expect_err("provider effect must exceed module contract");
    assert!(err.to_string().contains("provider effects"));
}

#[cfg(feature = "std")]
#[test]
fn resolve_module_externs_requires_provider_name_match() {
    let mut registry = ExternRegistry::new();
    registry.register_named(
        0,
        test_extern_name("registered_name"),
        ignore_host_event_resume,
    );
    let externs = vec![variadic_extern_def(
        "requested_name",
        0,
        ExternEffects::NONE,
    )];

    let err = registry
        .resolve_module_externs(&externs)
        .expect_err("provider name mismatch must be rejected");
    assert!(err.to_string().contains("provider registered by name"));
}

#[cfg(feature = "std")]
#[test]
fn resolve_module_externs_rejects_wasm_extension_bridge_missing_param_kinds_061() {
    let mut registry = ExternRegistry::new();
    registry.register_wasm_extension_bridge_with_effects(
        0,
        TEST_MODULE_OWNER,
        TEST_ARTIFACT_GENERATION,
        test_extern_name("bridge_needs_input"),
        ignore_host_event_resume,
        ExternEffects::MAY_HOST_WAIT,
    );
    let externs = vec![extern_def(
        "bridge_needs_input",
        ParamShape::Exact { slots: 1 },
        ReturnShape::slots(0),
        ExternEffects::MAY_HOST_WAIT,
    )];

    let err = registry
        .resolve_module_externs(&externs)
        .expect_err("wasm extension bridge input slots must declare param_kinds");

    assert!(
        err.to_string().contains("wasm extension bridge")
            && err.to_string().contains("param_kinds"),
        "{err}"
    );
}

#[test]
fn wasm_extension_bridge_catalog_selects_deepest_owner_independent_of_order() {
    let parent = "github.com/acme/mono";
    let child = "github.com/acme/mono/graphics";
    let name = vo_common_core::extern_key::ExternKeyRef::new(
        "github.com/acme/mono/graphics/render",
        "Draw",
    )
    .encode()
    .unwrap();

    for owners in [
        vec![
            WasmExtensionOwner::new(parent, 1),
            WasmExtensionOwner::new(child, 2),
        ],
        vec![
            WasmExtensionOwner::new(child, 2),
            WasmExtensionOwner::new(parent, 1),
        ],
    ] {
        let mut registry = ExternRegistry::new();
        registry
            .register_wasm_extension_bridge_catalog(
                owners,
                [WasmExtensionBridgeEntry::new(
                    0,
                    &name,
                    ignore_host_event_resume,
                    ExternEffects::NONE,
                )],
            )
            .expect("complete WASM catalog");
        let registered = registry
            .registered_by_name(&name)
            .expect("selected bridge provider");
        assert_eq!(registered.provider_module_owner(), Some(child));
        assert_eq!(registered.provider_artifact_generation(), Some(2));
    }
}

#[test]
fn wasm_extension_bridge_catalog_is_single_assignment_and_transactional() {
    let owner = WasmExtensionOwner::new(TEST_MODULE_OWNER, TEST_ARTIFACT_GENERATION);
    let first = test_extern_name("wasm_catalog_first");
    let conflicting = test_extern_name("wasm_catalog_conflicting");
    let mut registry = ExternRegistry::new();

    let error = registry
        .register_wasm_extension_bridge_catalog(
            [owner],
            [
                WasmExtensionBridgeEntry::new(
                    0,
                    &first,
                    ignore_host_event_resume,
                    ExternEffects::NONE,
                ),
                WasmExtensionBridgeEntry::new(
                    0,
                    &conflicting,
                    ignore_host_event_resume,
                    ExternEffects::NONE,
                ),
            ],
        )
        .expect_err("later id conflict must roll back the complete WASM catalog");
    assert!(error.to_string().contains("already bound"), "{error}");
    assert!(!registry.wasm_catalog_built);
    assert!(registry.extension_module_owners.is_empty());
    assert!(registry.registered_by_name(&first).is_none());

    registry
        .register_wasm_extension_bridge_catalog(
            core::iter::empty::<WasmExtensionOwner>(),
            core::iter::empty::<WasmExtensionBridgeEntry>(),
        )
        .expect("empty complete WASM catalog is a valid single build");
    assert!(registry.wasm_catalog_built);
    let second = registry
        .register_wasm_extension_bridge_catalog(
            core::iter::empty::<WasmExtensionOwner>(),
            core::iter::empty::<WasmExtensionBridgeEntry>(),
        )
        .expect_err("WASM catalog cannot be rebuilt incrementally");
    assert!(second.to_string().contains("already built"), "{second}");
}

#[cfg(feature = "std")]
#[test]
fn resolve_module_externs_rejects_wasm_extension_bridge_variadic_params_061() {
    let mut registry = ExternRegistry::new();
    registry.register_wasm_extension_bridge_with_effects(
        0,
        TEST_MODULE_OWNER,
        TEST_ARTIFACT_GENERATION,
        test_extern_name("bridge_variadic"),
        ignore_host_event_resume,
        ExternEffects::MAY_HOST_WAIT,
    );
    let externs = vec![variadic_extern_def(
        "bridge_variadic",
        0,
        ExternEffects::MAY_HOST_WAIT,
    )];

    let err = registry
        .resolve_module_externs(&externs)
        .expect_err("wasm extension bridge params must be exact");

    assert!(
        err.to_string().contains("wasm extension bridge")
            && err.to_string().contains("exact params"),
        "{err}"
    );
}

#[cfg(feature = "std")]
#[test]
fn wasm_extension_bridge_call_061_requires_resolved_abi_before_provider() {
    DIRECT_WASM_BRIDGE_PROVIDER_RAN_061.store(false, core::sync::atomic::Ordering::SeqCst);
    let mut registry = ExternRegistry::new();
    registry.register_wasm_extension_bridge_with_effects(
        0,
        TEST_MODULE_OWNER,
        TEST_ARTIFACT_GENERATION,
        test_extern_name("bridge_direct"),
        direct_wasm_bridge_provider_061,
        ExternEffects::NONE,
    );
    let mut stack = [0u64; 4];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 1,
        ret_start: 1,
        ret_slots: 0,
    };

    let err = call_unresolved_extern_with_stack(&registry, &mut stack, invoke)
        .expect_err("wasm extension bridge direct dispatch must be rejected");

    assert!(
        err.to_string().contains("wasm extension bridge") && err.to_string().contains("resolved"),
        "{err}"
    );
    assert!(
        !DIRECT_WASM_BRIDGE_PROVIDER_RAN_061.load(core::sync::atomic::Ordering::SeqCst),
        "bridge provider must not run before resolved ABI metadata is available"
    );
}

#[cfg(feature = "std")]
#[test]
fn wasm_extension_bridge_call_061_revalidates_resolved_param_kinds_before_provider() {
    RESOLVED_WASM_BRIDGE_PROVIDER_RAN_061.store(false, core::sync::atomic::Ordering::SeqCst);
    let mut registry = ExternRegistry::new();
    registry.register_wasm_extension_bridge_with_effects(
        0,
        TEST_MODULE_OWNER,
        TEST_ARTIFACT_GENERATION,
        test_extern_name("bridge_resolved"),
        resolved_wasm_bridge_provider_061,
        ExternEffects::NONE,
    );
    let mut def = extern_def(
        "bridge_resolved",
        ParamShape::Exact { slots: 1 },
        ReturnShape::slots(0),
        ExternEffects::NONE,
    );
    def.param_kinds = vec![crate::bytecode::ExtSlotKind::Value];
    let mut resolved = resolve_single_extern(&registry, def);
    resolved.param_kinds.clear();
    let mut stack = [0u64; 4];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 1,
        ret_start: 1,
        ret_slots: 0,
    };

    let err = call_resolved_extern_with_stack(&registry, &resolved, &mut stack, invoke)
        .expect_err("mutated bridge resolved ABI must be rejected");

    assert!(
        err.to_string().contains("wasm extension bridge")
            && err.to_string().contains("param_kinds"),
        "{err}"
    );
    assert!(
        !RESOLVED_WASM_BRIDGE_PROVIDER_RAN_061.load(core::sync::atomic::Ordering::SeqCst),
        "bridge provider must not run after resolved ABI metadata drift"
    );
}

#[cfg(feature = "std")]
#[test]
fn wasm_extension_bridge_call_061_passes_resolved_abi_to_provider_context() {
    RESOLVED_WASM_BRIDGE_CONTEXT_ABI_OK_061.store(false, core::sync::atomic::Ordering::SeqCst);
    let mut registry = ExternRegistry::new();
    registry.register_wasm_extension_bridge_with_effects(
        0,
        TEST_MODULE_OWNER,
        TEST_ARTIFACT_GENERATION,
        test_extern_name("bridge_context"),
        resolved_wasm_bridge_context_provider_061,
        ExternEffects::NONE,
    );
    let mut def = extern_def(
        "bridge_context",
        ParamShape::Exact { slots: 2 },
        ReturnShape::slots(0),
        ExternEffects::NONE,
    );
    def.param_kinds = vec![
        crate::bytecode::ExtSlotKind::Bytes,
        crate::bytecode::ExtSlotKind::Value,
    ];
    let resolved = resolve_single_extern(&registry, def);
    let mut stack = [0u64; 4];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 2,
        ret_start: 2,
        ret_slots: 0,
    };

    let result = call_resolved_extern_with_stack(&registry, &resolved, &mut stack, invoke)
        .expect("resolved wasm bridge ABI should be available to the provider");

    assert!(matches!(result, ExternResult::Ok));
    assert!(
        RESOLVED_WASM_BRIDGE_CONTEXT_ABI_OK_061.load(core::sync::atomic::Ordering::SeqCst),
        "provider must observe the bridge ABI that was resolved for this VM"
    );
}

#[cfg(feature = "std")]
#[test]
fn resolve_module_externs_uses_provider_name_not_registration_id() {
    let mut registry = ExternRegistry::new();
    registry.register_named(7, test_extern_name("contract_ok"), ignore_host_event_resume);
    let externs = vec![variadic_extern_def("contract_ok", 0, ExternEffects::NONE)];

    let resolved = registry.resolve_module_externs(&externs).expect("resolve");

    assert_eq!(resolved.get(0).expect("resolved").id, 0);
    assert_eq!(
        resolved.get(0).expect("resolved").name,
        test_extern_name("contract_ok")
    );
}

#[cfg(feature = "std")]
#[test]
fn public_named_registration_records_manual_source() {
    let mut registry = ExternRegistry::new();
    registry.register_named(0, test_extern_name("manual_ok"), ignore_host_event_resume);
    registry.register_test_named(1, test_extern_name("test_ok"), ignore_host_event_resume);
    let externs = vec![
        variadic_extern_def("manual_ok", 0, ExternEffects::NONE),
        variadic_extern_def("test_ok", 0, ExternEffects::NONE),
    ];

    let resolved = registry.resolve_module_externs(&externs).expect("resolve");

    assert_eq!(
        resolved.get(0).expect("manual extern").source,
        RegisteredExternSource::Manual
    );
    assert_eq!(
        resolved.get(1).expect("test extern").source,
        RegisteredExternSource::Test
    );
}

#[cfg(feature = "std")]
#[test]
fn resolve_module_externs_freezes_jit_route_metadata() {
    let mut registry = ExternRegistry::new();
    registry
        .try_register_builtin_with_effects(
            0,
            MATH_SQRT_EXTERN_NAME,
            ignore_host_event_resume,
            ExternEffects::NONE,
        )
        .expect("builtin registration");
    registry.register_test_named_with_effects(
        1,
        test_extern_name("custom_yield"),
        yield_extern,
        ExternEffects::MAY_YIELD,
    );
    let externs = vec![
        math_unary_extern_def(MATH_SQRT_EXTERN_NAME),
        variadic_extern_def("custom_yield", 0, ExternEffects::UNKNOWN_CONTROL),
    ];

    let resolved = registry.resolve_module_externs(&externs).expect("resolve");

    assert_eq!(
        resolved.get(0).expect("math extern").jit_route,
        ExternJitRoute::Intrinsic
    );
    assert_eq!(
        resolved.get(1).expect("yield extern").jit_route,
        ExternJitRoute::DirectHelper
    );
}

#[cfg(feature = "std")]
#[test]
fn resolve_module_externs_requires_trusted_source_for_intrinsics() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named_with_effects(
        0,
        MATH_SQRT_EXTERN_NAME,
        ignore_host_event_resume,
        ExternEffects::NONE,
    );
    let externs = vec![math_unary_extern_def(MATH_SQRT_EXTERN_NAME)];

    let resolved = registry.resolve_module_externs(&externs).expect("resolve");

    assert_eq!(
        resolved.get(0).expect("math extern").jit_route,
        ExternJitRoute::DirectHelper
    );
}

#[cfg(feature = "std")]
#[test]
fn resolve_module_externs_requires_exact_intrinsic_return_layout() {
    let mut registry = ExternRegistry::new();
    registry
        .try_register_builtin_with_effects(
            0,
            MATH_SQRT_EXTERN_NAME,
            ignore_host_event_resume,
            ExternEffects::NONE,
        )
        .expect("builtin registration");
    let externs = vec![math_unary_extern_def_with_return_layout(
        MATH_SQRT_EXTERN_NAME,
        vec![crate::SlotType::Value],
    )];

    let resolved = registry.resolve_module_externs(&externs).expect("resolve");

    assert_eq!(
        resolved.get(0).expect("math extern").jit_route,
        ExternJitRoute::DirectHelper
    );
}

#[cfg(feature = "std")]
#[test]
fn resolve_module_externs_requires_exact_intrinsic_arity() {
    let mut registry = ExternRegistry::new();
    registry
        .try_register_builtin_with_effects(
            0,
            MATH_FMA_EXTERN_NAME,
            ignore_host_event_resume,
            ExternEffects::NONE,
        )
        .expect("builtin registration");
    let externs = vec![extern_def(
        MATH_FMA_EXTERN_NAME,
        ParamShape::Exact { slots: 1 },
        ReturnShape::with_slot_types(vec![crate::SlotType::Float]),
        ExternEffects::NONE,
    )];

    let resolved = registry.resolve_module_externs(&externs).expect("resolve");

    assert_eq!(
        resolved.get(0).expect("math extern").jit_route,
        ExternJitRoute::DirectHelper
    );
}

#[cfg(feature = "std")]
#[test]
fn public_stdlib_entry_registration_is_not_intrinsic_trusted() {
    let mut registry = ExternRegistry::new();
    StdlibEntry {
        name: MATH_SQRT_EXTERN_NAME,
        func: ignore_host_event_resume,
        effects: ExternEffects::NONE,
    }
    .register(&mut registry, 0);
    let externs = vec![math_unary_extern_def(MATH_SQRT_EXTERN_NAME)];

    let resolved = registry.resolve_module_externs(&externs).expect("resolve");

    assert_eq!(
        resolved.get(0).expect("math extern").jit_route,
        ExternJitRoute::DirectHelper
    );
}

#[cfg(feature = "std")]
#[test]
fn registry_rejects_provider_metadata_replacement_transactionally() {
    let mut registry = ExternRegistry::new();
    let name = test_extern_name("contract_yield");
    registry
        .try_register_test_named_with_effects(0, &name, yield_extern, ExternEffects::MAY_YIELD)
        .expect("initial registration");
    let provider_identity = registry
        .registered_by_name(&name)
        .expect("initial provider")
        .provider_identity;

    let error = registry
        .try_register_test_named_with_effects(0, &name, yield_extern, ExternEffects::NONE)
        .expect_err("provider effects cannot change after registration");
    assert!(error.to_string().contains("single-assignment"));
    let registered = registry
        .registered_by_name(&name)
        .expect("original provider remains registered");
    assert_eq!(registered.provider_identity, provider_identity);
    assert_eq!(registered.provider_effects(), ExternEffects::MAY_YIELD);
}

#[cfg(feature = "std")]
#[test]
fn resolved_call_rejects_abi_fingerprint_drift_after_load() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named(0, test_extern_name("contract_ok"), ignore_host_event_resume);
    let externs = vec![variadic_extern_def("contract_ok", 0, ExternEffects::NONE)];
    let mut resolved = registry
        .resolve_module_externs(&externs)
        .expect("resolve")
        .get(0)
        .expect("resolved")
        .clone();
    resolved.abi_fingerprint ^= 0x55aa;

    let mut stack = [0u64; 4];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots: 0,
    };
    let mut gc = Gc::new();
    let module = Module::new("ffi-resolved-call-test".to_string());
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        host_services: None,
        io: &mut io,
    };

    let err = registry
        .call_resolved(
            &mut stack,
            invoke,
            world,
            fiber_inputs(None, None),
            &resolved,
        )
        .expect_err("ABI fingerprint drift must be rejected");
    assert!(err.to_string().contains("metadata drifted"));
}

#[cfg(feature = "std")]
#[test]
fn resolved_call_rejects_shape_mismatch_before_provider_runs() {
    static PROVIDER_RAN: core::sync::atomic::AtomicBool =
        core::sync::atomic::AtomicBool::new(false);

    fn observed_extern(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
        PROVIDER_RAN.store(true, core::sync::atomic::Ordering::SeqCst);
        ExternResult::Ok
    }

    PROVIDER_RAN.store(false, core::sync::atomic::Ordering::SeqCst);
    let mut registry = ExternRegistry::new();
    registry.register_test_named(0, test_extern_name("contract_shape"), observed_extern);
    let externs = vec![extern_def(
        "contract_shape",
        ParamShape::Exact { slots: 2 },
        ReturnShape::slots(0),
        ExternEffects::NONE,
    )];
    let resolved = registry
        .resolve_module_externs(&externs)
        .expect("resolve")
        .get(0)
        .expect("resolved")
        .clone();

    let mut stack = [0u64; 4];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 1,
        ret_start: 0,
        ret_slots: 0,
    };
    let mut gc = Gc::new();
    let module = Module::new("ffi-resolved-call-test".to_string());
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        host_services: None,
        io: &mut io,
    };

    let err = registry
        .call_resolved(
            &mut stack,
            invoke,
            world,
            fiber_inputs(None, None),
            &resolved,
        )
        .expect_err("shape mismatch must be rejected");
    assert!(err.to_string().contains("arg slot count"));
    assert!(!PROVIDER_RAN.load(core::sync::atomic::Ordering::SeqCst));
}

#[cfg(feature = "std")]
#[test]
fn ffi_return_window_contract_rejects_single_slot_helper_outside_declared_returns() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named(
        0,
        test_extern_name("contract_ret_oob"),
        write_ret_u64_slot_one,
    );
    let resolved = resolve_single_extern(
        &registry,
        extern_def(
            "contract_ret_oob",
            ParamShape::Exact { slots: 0 },
            ReturnShape::slots(1),
            ExternEffects::NONE,
        ),
    );
    let mut stack = [0x1111, 0x2222, 0x3333, 0x4444];

    let err =
        call_resolved_extern_with_stack(&registry, &resolved, &mut stack, invoke_with_returns(1))
            .expect_err("out-of-window return write must be rejected");

    assert!(err.to_string().contains("outside declared ret_slots 1"));
    assert_eq!(stack, [0x1111, 0x2222, 0x3333, 0x4444]);
}

#[cfg(feature = "std")]
#[test]
fn ffi_return_window_contract_rejects_two_slot_helper_without_clobbering_next_slot() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named(
        0,
        test_extern_name("contract_ret_any_oob"),
        write_ret_any_into_one_slot,
    );
    let resolved = resolve_single_extern(
        &registry,
        extern_def(
            "contract_ret_any_oob",
            ParamShape::Exact { slots: 0 },
            ReturnShape::slots(1),
            ExternEffects::NONE,
        ),
    );
    let mut stack = [0xaaaa, 0xbbbb, 0xcccc, 0xdddd];

    let err =
        call_resolved_extern_with_stack(&registry, &resolved, &mut stack, invoke_with_returns(1))
            .expect_err("two-slot helper must not write outside one-slot return window");

    assert!(err.to_string().contains("outside declared ret_slots 1"));
    assert_eq!(stack, [0xaaaa, 0xbbbb, 0xcccc, 0xdddd]);
}

#[cfg(feature = "std")]
#[test]
fn ffi_argument_helpers_reject_the_end_of_the_host_window_without_panicking() {
    let error =
        call_registered_extern(read_outside_empty_argument_window, fiber_inputs(None, None))
            .expect_err("out-of-window argument reads must fail the host call");
    assert!(error.message().contains("outside declared arg_slots 0"));
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_invalid_gcref_return() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named(
        0,
        test_extern_name("contract_invalid_gcref"),
        return_invalid_gc_ref,
    );
    let resolved = resolve_single_extern(
        &registry,
        extern_def(
            "contract_invalid_gcref",
            ParamShape::Exact { slots: 0 },
            ReturnShape::with_slot_types(vec![crate::SlotType::GcRef]),
            ExternEffects::NONE,
        ),
    );
    let mut stack = [0, 0, 0, 0];

    let err =
        call_resolved_extern_with_stack(&registry, &resolved, &mut stack, invoke_with_returns(1))
            .expect_err("invalid GC-shaped return must be rejected");

    assert!(err.to_string().contains("returned invalid GcRef"));
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_non_ok_return_slots_rollback_before_yield_061() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named_with_effects(
        0,
        test_extern_name("contract_yield_invalid_gcref"),
        write_invalid_gc_ref_then_yield,
        ExternEffects::MAY_YIELD,
    );
    let resolved = resolve_single_extern(
        &registry,
        extern_def(
            "contract_yield_invalid_gcref",
            ParamShape::Exact { slots: 0 },
            ReturnShape::with_slot_types(vec![crate::SlotType::GcRef]),
            ExternEffects::MAY_YIELD,
        ),
    );
    let mut stack = [0xaaaa, 0xbbbb, 0xcccc, 0xdddd];

    let result =
        call_resolved_extern_with_stack(&registry, &resolved, &mut stack, invoke_with_returns(1))
            .expect("yield is allowed by the resolved effect contract");

    assert!(matches!(result, ExternResult::Yield));
    assert_eq!(stack, [0xaaaa, 0xbbbb, 0xcccc, 0xdddd]);
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_contract_error_rolls_back_return_slots_061() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named_with_effects(
        0,
        test_extern_name("contract_post_call_invalid_gcref"),
        write_invalid_gc_ref_then_contract_error,
        ExternEffects::MAY_YIELD,
    );
    let resolved = resolve_single_extern(
        &registry,
        extern_def(
            "contract_post_call_invalid_gcref",
            ParamShape::Exact { slots: 0 },
            ReturnShape::with_slot_types(vec![crate::SlotType::GcRef]),
            ExternEffects::MAY_YIELD,
        ),
    );
    let mut stack = [0x1111, 0x2222, 0x3333, 0x4444];

    let err =
        call_resolved_extern_with_stack(&registry, &resolved, &mut stack, invoke_with_returns(1))
            .expect_err("post-call contract violation must fail the call");

    assert!(err
        .to_string()
        .contains("synthetic post-call contract error"));
    assert_eq!(stack, [0x1111, 0x2222, 0x3333, 0x4444]);
}

#[cfg(feature = "std")]
#[test]
fn ffi_str_argument_rejects_invalid_utf8_as_contract_error() {
    fn read_text(call: &mut ExternCallContext<'_>) -> ExternResult {
        assert_eq!(call.arg_str(0), "");
        ExternResult::Ok
    }

    let mut registry = ExternRegistry::new();
    registry.register_test_named(0, test_extern_name("read_text"), read_text);
    let mut def = extern_def(
        "read_text",
        ParamShape::Exact { slots: 1 },
        ReturnShape::slots(0),
        ExternEffects::NONE,
    );
    def.param_kinds = vec![ExtSlotKind::Bytes];
    let resolved = resolve_single_extern(&registry, def);

    let mut gc = Gc::new();
    let invalid = crate::objects::string::create(&mut gc, &[0xff]);
    let mut stack = [invalid as u64, 0, 0, 0];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 1,
        ret_start: 1,
        ret_slots: 0,
    };
    let module = Module::new("ffi-invalid-utf8".to_string());
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        host_services: None,
        io: &mut io,
    };

    let error = registry
        .call_resolved(
            &mut stack,
            invoke,
            world,
            fiber_inputs(None, None),
            &resolved,
        )
        .expect_err("Rust &str extern parameters must reject invalid UTF-8");
    assert!(error.to_string().contains("contains invalid UTF-8"));
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_invalid_interface_gc_data_return() {
    let mut module = Module::new("ffi-interface-ref-boundary".to_string());
    module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));

    let err = call_empty_interface_return_provider(
        return_invalid_interface_ref,
        "contract_invalid_iface_ref",
        &module,
    )
    .expect_err("invalid interface data GcRef must be rejected");

    assert!(err.to_string().contains("returned invalid interface GcRef"));
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_invalid_kind_and_noncanonical_nil_interface() {
    let mut module = Module::new("ffi-interface-kind-boundary".to_string());
    module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });

    let invalid_kind = call_empty_interface_return_provider(
        return_invalid_interface_kind,
        "contract_invalid_iface_kind",
        &module,
    )
    .expect_err("invalid interface value-kind tags must be rejected");
    assert!(
        invalid_kind.to_string().contains("invalid value-kind tag"),
        "{invalid_kind}"
    );

    let reserved_rttid = call_empty_interface_return_provider(
        return_reserved_interface_rttid,
        "contract_reserved_iface_rttid",
        &module,
    )
    .expect_err("the reserved 24-bit interface RTTID must be rejected");
    assert!(
        reserved_rttid
            .to_string()
            .contains("reserved interface RTTID"),
        "{reserved_rttid}"
    );

    let noncanonical_nil = call_empty_interface_return_provider(
        return_noncanonical_nil_interface,
        "contract_noncanonical_nil_iface",
        &module,
    )
    .expect_err("nil interfaces with hidden metadata must be rejected");
    assert!(
        noncanonical_nil
            .to_string()
            .contains("non-canonical nil interface"),
        "{noncanonical_nil}"
    );
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_accepts_empty_interface_with_concrete_itab_061() {
    let mut module = Module::new("ffi-empty-interface-itab-boundary".to_string());
    module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));

    call_empty_interface_return_provider(
        return_empty_interface_with_concrete_itab,
        "contract_empty_iface_concrete_itab",
        &module,
    )
    .expect("empty-interface returns may preserve a concrete itab after data validation");
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_null_struct_or_array_interface_box_061() {
    let mut struct_module = Module::new("ffi-null-struct-interface-data-boundary".to_string());
    struct_module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    struct_module.struct_metas.push(StructMeta {
        slot_types: vec![crate::SlotType::Value],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });
    struct_module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });

    let struct_err = call_empty_interface_return_provider(
        return_null_struct_interface_data,
        "contract_null_struct_iface_data",
        &struct_module,
    )
    .expect_err("null struct interface data must be rejected at the FFI boundary");

    assert!(
        struct_err.to_string().contains("data missing object"),
        "{struct_err}"
    );

    let mut array_module = Module::new("ffi-null-array-interface-data-boundary".to_string());
    array_module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    array_module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    array_module.runtime_types.push(RuntimeType::Array {
        len: 2,
        elem: ValueRttid::new(0, ValueKind::String),
    });

    let array_err = call_empty_interface_return_provider(
        return_null_array_interface_data,
        "contract_null_array_iface_data",
        &array_module,
    )
    .expect_err("null array interface data must be rejected at the FFI boundary");

    assert!(
        array_err.to_string().contains("data missing object"),
        "{array_err}"
    );
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_canonicalizes_interface_data_slot_061() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named(
        0,
        test_extern_name("contract_interior_struct_iface_data"),
        return_interior_struct_interface_data,
    );
    let resolved = resolve_single_extern(
        &registry,
        extern_def(
            "contract_interior_struct_iface_data",
            ParamShape::Exact { slots: 0 },
            ReturnShape::try_with_slot_types_and_interface_metas(
                vec![
                    crate::SlotType::Interface0,
                    crate::SlotType::Interface1,
                    crate::SlotType::Value,
                ],
                vec![Some(0), None, None],
            )
            .expect("interface return shape"),
            ExternEffects::NONE,
        ),
    );
    let mut module = Module::new("ffi-interface-data-canonical-boundary".to_string());
    module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module.struct_metas.push(StructMeta {
        slot_types: vec![crate::SlotType::Value, crate::SlotType::Value],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });
    let mut stack = [0u64; 4];
    let mut gc = Gc::new();
    let mut itab_cache = ItabCache::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        host_services: None,
        io: &mut io,
    };

    registry
        .call_resolved(
            &mut stack,
            invoke_with_returns(3),
            world,
            fiber_inputs(None, None),
            &resolved,
        )
        .expect("interior interface data ref should be accepted after canonical writeback");

    assert_eq!(
        stack[1], stack[2],
        "Interface1 return slot must be rewritten to the canonical aggregate object base"
    );
    assert_eq!(
        gc.canonicalize_ref(stack[1] as GcRef),
        Some(stack[1] as GcRef)
    );
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_slots_only_interface_metadata_060() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named(0, test_extern_name("contract_ok"), ignore_host_event_resume);
    let mut resolved = resolve_single_extern(
        &registry,
        extern_def(
            "contract_ok",
            ParamShape::Exact { slots: 0 },
            ReturnShape::slots(2),
            ExternEffects::NONE,
        ),
    );
    resolved.returns.interface_metas = vec![Some(0), None];
    let mut stack = [0, 0, 0, 0];

    let err =
        call_resolved_extern_with_stack(&registry, &resolved, &mut stack, invoke_with_returns(2))
            .expect_err("FFI boundary must reject malformed ReturnShape metadata");

    assert!(
        err.to_string()
            .contains("return interface metadata requires return slot_types"),
        "{err}"
    );
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_wrong_kind_interface_data_061() {
    let mut registry = ExternRegistry::new();
    registry.register_test_named(
        0,
        test_extern_name("contract_wrong_kind_iface_data"),
        return_wrong_kind_interface_data,
    );
    let resolved = resolve_single_extern(
        &registry,
        extern_def(
            "contract_wrong_kind_iface_data",
            ParamShape::Exact { slots: 0 },
            ReturnShape::try_with_slot_types_and_interface_metas(
                vec![crate::SlotType::Interface0, crate::SlotType::Interface1],
                vec![Some(0), None],
            )
            .expect("interface return shape"),
            ExternEffects::NONE,
        ),
    );
    let mut module = Module::new("ffi-interface-data-boundary".to_string());
    module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    let mut itab_cache = ItabCache::new();
    let mut stack = [0u64; 4];
    let mut gc = Gc::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        host_services: None,
        io: &mut io,
    };

    let err = registry
        .call_resolved(
            &mut stack,
            invoke_with_returns(2),
            world,
            fiber_inputs(None, None),
            &resolved,
        )
        .expect_err("wrong-kind interface data must be rejected at the FFI boundary");

    assert!(
        err.to_string().contains("interface data object kind"),
        "{err}"
    );
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_wrong_meta_interface_data_061() {
    let mut module = Module::new("ffi-interface-data-meta-boundary".to_string());
    module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module.struct_metas.push(StructMeta {
        slot_types: vec![crate::SlotType::GcRef, crate::SlotType::GcRef],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });
    module.struct_metas.push(StructMeta {
        slot_types: vec![crate::SlotType::GcRef, crate::SlotType::GcRef],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });

    let err = call_empty_interface_return_provider(
        return_wrong_meta_interface_data,
        "contract_wrong_meta_iface_data",
        &module,
    )
    .expect_err("wrong-meta interface data must be rejected at the FFI boundary");

    assert!(err.to_string().contains("interface data meta_id"), "{err}");
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_struct_interface_data_slot_count_drift_061() {
    let mut module = Module::new("ffi-interface-data-width-boundary".to_string());
    module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module.struct_metas.push(StructMeta {
        slot_types: vec![crate::SlotType::GcRef, crate::SlotType::GcRef],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });

    let err = call_empty_interface_return_provider(
        return_undersized_struct_interface_data,
        "contract_undersized_struct_iface_data",
        &module,
    )
    .expect_err("undersized struct interface data must be rejected at the FFI boundary");

    assert!(
        err.to_string().contains("interface data allocation slots"),
        "{err}"
    );
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_array_value_slot_box_count_drift_061() {
    let mut module = Module::new("ffi-interface-array-data-width-boundary".to_string());
    module.interface_metas.push(InterfaceMeta {
        name: "Any".to_string(),
        method_names: Vec::new(),
        methods: Vec::new(),
    });
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.runtime_types.push(RuntimeType::Array {
        len: 2,
        elem: ValueRttid::new(0, ValueKind::String),
    });
    module.struct_metas.push(StructMeta {
        slot_types: vec![crate::SlotType::GcRef, crate::SlotType::GcRef],
        fields: Vec::new(),
        field_index: HashMap::new(),
    });

    let err = call_empty_interface_return_provider(
        return_undersized_array_box_interface_data,
        "contract_undersized_array_box_iface_data",
        &module,
    )
    .expect_err("undersized array value-slot box must be rejected at the FFI boundary");

    assert!(
        err.to_string().contains("interface data allocation slots"),
        "{err}"
    );
}

#[cfg(feature = "std")]
#[test]
fn ffi_expected_interface_itab_methods_rejects_signature_mismatch_060() {
    use vo_common_core::bytecode::{InterfaceMethodMeta, MethodInfo};

    let mut module = Module::new("ffi-interface-signature-helper".to_string());
    let mut methods = BTreeMap::new();
    methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 7,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 3,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "T".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
        methods,
    });
    module.interface_metas.push(InterfaceMeta {
        name: "I".to_string(),
        method_names: vec!["M".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "M".to_string(),
            signature_rttid: 4,
        }],
    });

    assert_eq!(
        crate::itab::expected_interface_itab_methods(
            0,
            0,
            false,
            &module.named_type_metas,
            &module.interface_metas,
        ),
        None
    );
}

#[cfg(feature = "std")]
#[test]
fn ffi_resolved_return_shape_rejects_wrong_signature_itab_060() {
    use vo_common_core::bytecode::{InterfaceMethodMeta, Itab, MethodInfo};

    let mut registry = ExternRegistry::new();
    registry.register_test_named(
        0,
        test_extern_name("contract_wrong_signature_iface"),
        return_wrong_signature_interface,
    );
    let resolved = resolve_single_extern(
        &registry,
        extern_def(
            "contract_wrong_signature_iface",
            ParamShape::Exact { slots: 0 },
            ReturnShape::try_with_slot_types_and_interface_metas(
                vec![crate::SlotType::Interface0, crate::SlotType::Interface1],
                vec![Some(0), None],
            )
            .expect("interface return shape"),
            ExternEffects::NONE,
        ),
    );
    let mut module = Module::new("ffi-interface-signature-boundary".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    let mut methods = BTreeMap::new();
    methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id: 7,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 3,
        },
    );
    module.named_type_metas.push(NamedTypeMeta {
        name: "T".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Int64),
        underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
        methods,
    });
    module.interface_metas.push(InterfaceMeta {
        name: "I".to_string(),
        method_names: vec!["M".to_string()],
        methods: vec![InterfaceMethodMeta {
            name: "M".to_string(),
            signature_rttid: 4,
        }],
    });
    let mut itab_cache = ItabCache::from_module_itabs(vec![
        Itab::default(),
        Itab {
            iface_meta_id: 0,
            methods: vec![7],
        },
    ]);
    let mut stack = [0u64; 4];
    let mut gc = Gc::new();
    let program_args = Vec::new();
    let output = crate::output::CaptureSink::new();
    let mut sentinel_errors = SentinelErrorCache::new();
    let mut host_output = None;
    let mut io = crate::io::IoRuntime::new().expect("io runtime");
    let world = ExternWorld {
        gc: &mut gc,
        module: &module,
        itab_cache: &mut itab_cache,
        vm_opaque: core::ptr::null_mut(),
        program_args: &program_args,
        output: output.as_ref(),
        sentinel_errors: &mut sentinel_errors,
        host_output: &mut host_output,
        host_services: None,
        io: &mut io,
    };

    let err = registry
        .call_resolved(
            &mut stack,
            invoke_with_returns(2),
            world,
            fiber_inputs(None, None),
            &resolved,
        )
        .expect_err("wrong-signature itab must be rejected at the FFI boundary");

    assert!(
        err.to_string()
            .contains("does not implement expected interface"),
        "{err}"
    );
}

#[cfg(feature = "std")]
#[test]
fn registry_rejects_provider_function_replacement_transactionally() {
    let mut registry = ExternRegistry::new();
    let name = test_extern_name("contract_ok");
    registry
        .try_register_test_named_with_effects(
            0,
            &name,
            ignore_host_event_resume,
            ExternEffects::NONE,
        )
        .expect("initial registration");
    let provider_identity = registry
        .registered_by_name(&name)
        .expect("initial provider")
        .provider_identity;

    let error = registry
        .try_register_test_named_with_effects(0, &name, other_ok_extern, ExternEffects::NONE)
        .expect_err("provider function cannot change after registration");
    assert!(error.to_string().contains("single-assignment"));
    let registered = registry
        .registered_by_name(&name)
        .expect("original provider remains registered");
    assert_eq!(registered.provider_identity, provider_identity);
}

#[cfg(feature = "std")]
#[test]
fn resolved_call_rejects_provider_identity_drift_after_registry_replacement() {
    static REPLACEMENT_PROVIDER_RAN: core::sync::atomic::AtomicBool =
        core::sync::atomic::AtomicBool::new(false);

    fn replacement_provider(_ctx: &mut ExternCallContext<'_>) -> ExternResult {
        REPLACEMENT_PROVIDER_RAN.store(true, core::sync::atomic::Ordering::SeqCst);
        ExternResult::Ok
    }

    REPLACEMENT_PROVIDER_RAN.store(false, core::sync::atomic::Ordering::SeqCst);

    let mut original = ExternRegistry::new();
    original.register_test_named_with_effects(
        0,
        test_extern_name("contract_ok"),
        ignore_host_event_resume,
        ExternEffects::NONE,
    );
    let externs = vec![variadic_extern_def("contract_ok", 0, ExternEffects::NONE)];
    let resolved = original
        .resolve_module_externs(&externs)
        .expect("resolve")
        .get(0)
        .expect("resolved")
        .clone();

    let mut replacement = ExternRegistry::new();
    replacement.register_test_named_with_effects(
        0,
        test_extern_name("contract_ok"),
        replacement_provider,
        ExternEffects::NONE,
    );

    let mut stack = [0u64; 4];
    let invoke = ExternInvoke {
        extern_id: 0,
        bp: 0,
        arg_start: 0,
        arg_slots: 0,
        ret_start: 0,
        ret_slots: 0,
    };
    let err = call_resolved_extern_with_stack(&replacement, &resolved, &mut stack, invoke)
        .expect_err("registry replacement must not inherit provider authority");

    assert!(err.to_string().contains("identity or metadata drifted"));
    assert!(!REPLACEMENT_PROVIDER_RAN.load(core::sync::atomic::Ordering::SeqCst));
}

#[cfg(feature = "std")]
#[test]
fn registry_rejects_id_rebinding_without_removing_original_provider() {
    let mut registry = ExternRegistry::new();
    let original = test_extern_name("contract_ok");
    let replacement = test_extern_name("replacement_ok");
    registry
        .try_register_test_named(0, &original, ignore_host_event_resume)
        .expect("initial registration");

    let error = registry
        .try_register_test_named(0, &replacement, ignore_host_event_resume)
        .expect_err("extern id rebinding must fail");
    assert!(error.to_string().contains("already bound"), "{error}");
    assert!(registry.registered_by_name(&replacement).is_none());
    assert_eq!(
        registry
            .registered(0)
            .expect("original id binding")
            .provider_name(),
        original
    );
}

#[cfg(feature = "std")]
#[test]
fn registry_owner_catalog_blocks_parent_fallback_before_freeze() {
    let parent = "github.com/acme/mono";
    let child = "github.com/acme/mono/graphics";
    let name = vo_common_core::extern_key::ExternKeyRef::new(
        "github.com/acme/mono/graphics/render",
        "Missing",
    )
    .encode()
    .unwrap();
    let mut registry = ExternRegistry::new();
    registry
        .try_register_extension_with_effects(
            0,
            parent,
            &name,
            abi_error_extension,
            ExternEffects::NONE,
        )
        .expect("parent provider");
    assert!(registry.registered_by_name(&name).is_some());

    registry
        .try_declare_extension_module_owner(child, ExtensionOwnerCatalog::NativeDynamic)
        .expect("nested owner declaration");

    assert!(registry.registered_by_name(&name).is_none());
    let def = crate::bytecode::ExternDef {
        name: name.clone(),
        params: ParamShape::exact(0),
        returns: ReturnShape::slots(0),
        param_kinds: Vec::new(),
        allowed_effects: ExternEffects::NONE,
    };
    let error = registry
        .resolve_module_externs(&[def])
        .expect_err("nested owner without an exact entry must remain missing");
    assert!(
        error.to_string().contains("no matching provider"),
        "{error}"
    );
    assert!(error.to_string().contains(child), "{error}");
}

#[cfg(feature = "std")]
#[test]
fn frozen_registry_rejects_late_mutation() {
    let mut registry = ExternRegistry::new();
    registry.freeze();

    let error = registry
        .try_register_test_with_effects(0, ignore_host_event_resume, ExternEffects::NONE)
        .expect_err("frozen registry must reject a fallible registration");
    assert!(error.to_string().contains("frozen"), "{error}");
    assert!(registry.registration_error().is_none());
    assert!(registry.is_frozen());
    assert!(registry.is_empty());

    let owner_error = registry
        .try_declare_extension_module_owner(TEST_MODULE_OWNER, ExtensionOwnerCatalog::NativeDynamic)
        .expect_err("frozen registry must reject owner-catalog changes");
    assert!(owner_error.to_string().contains("frozen"), "{owner_error}");

    let linkme_error = registry
        .register_from_extension_catalogs(None, &[])
        .expect_err("frozen registry must reject linkme catalog rebuilds");
    assert!(
        linkme_error.to_string().contains("frozen"),
        "{linkme_error}"
    );
    let loader = crate::ext_loader::ExtensionLoader::new();
    let dynamic_error = registry
        .register_from_extension_catalogs(Some(&loader), &[])
        .expect_err("frozen registry must reject dynamic catalog rebuilds");
    assert!(
        dynamic_error.to_string().contains("frozen"),
        "{dynamic_error}"
    );

    registry.register_test_with_effects(0, ignore_host_event_resume, ExternEffects::NONE);
    let deferred = registry
        .registration_error()
        .expect("compatibility registration defers the frozen error");
    assert!(deferred.to_string().contains("frozen"), "{deferred}");
    assert!(registry.is_empty());
}

#[cfg(feature = "std")]
#[test]
fn registry_call_rejects_runtime_effect_outside_provider_metadata() {
    let result = call_registered_extern_with_effects(
        yield_extern,
        ExternEffects::NONE,
        fiber_inputs(None, None),
    );

    assert!(matches!(result, Err(err) if err.to_string().contains("outside resolved effects")));
}

#[cfg(feature = "std")]
#[test]
fn vm_extern_provider_panic_boundary_006_returns_contract_error() {
    fn panicking_provider(_: &mut ExternCallContext) -> ExternResult {
        panic!("provider invariant drift")
    }

    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        call_registered_extern(panicking_provider, fiber_inputs(None, None))
    }));

    let err = result
        .expect("internal provider panic must not escape registry call")
        .expect_err("provider panic must become a contract error");
    assert!(err
        .to_string()
        .contains("panicked across the runtime boundary"));
}

#[cfg(feature = "std")]
#[test]
fn ffi_post_call_rejects_unconsumed_host_event_token() {
    let result = call_registered_extern(ignore_host_event_resume, fiber_inputs(Some(77), None));

    assert!(matches!(result, Err(err) if err.to_string().contains("resume_host_event_token")));
}

#[cfg(feature = "std")]
#[test]
fn ffi_post_call_rejects_unconsumed_host_event_data() {
    let result =
        call_registered_extern(ignore_host_event_resume, fiber_inputs(None, Some(vec![1])));

    assert!(matches!(result, Err(err) if err.to_string().contains("resume_host_event_data")));
}

#[cfg(feature = "std")]
#[test]
fn ffi_post_call_accepts_consumed_host_event_resume_inputs() {
    let result = call_registered_extern(
        consume_host_event_resume,
        fiber_inputs(Some(77), Some(vec![1, 2, 3])),
    );

    assert!(matches!(result, Ok(ExternResult::Ok)));
}

#[cfg(feature = "std")]
fn assert_named_func_and_slice_resolution(call: &mut ExternCallContext<'_>) -> ExternResult {
    let int_type = ValueRttid::new(0, ValueKind::Int64);
    let named_slice_type = ValueRttid::new(3, ValueKind::Slice);

    for func_rttid in [4, 5, 6] {
        let (params, results, variadic) = call
            .get_func_signature(func_rttid)
            .expect("named function chain should resolve to its function signature");
        assert_eq!(params.as_slice(), &[named_slice_type]);
        assert_eq!(results.as_slice(), &[int_type]);
        assert!(variadic);
        assert_eq!(call.get_func_results(func_rttid), Some(vec![int_type]));
    }

    for slice_rttid in [1, 2, 3] {
        assert_eq!(call.get_slice_elem(slice_rttid), Some(int_type));
    }

    assert!(call.check_func_signature_compatible(6, 9).is_ok());
    assert!(call.get_func_signature(8).is_none());
    assert!(call.get_slice_elem(8).is_none());
    ExternResult::Ok
}

#[cfg(feature = "std")]
#[test]
fn ffi_dynamic_type_helpers_resolve_named_chains_without_following_pointers() {
    let mut module = Module::new("ffi-named-dynamic-types".to_string());
    let int_type = ValueRttid::new(0, ValueKind::Int64);
    let base_slice_type = ValueRttid::new(1, ValueKind::Slice);
    let named_slice_type = ValueRttid::new(2, ValueKind::Slice);
    let chained_slice_type = ValueRttid::new(3, ValueKind::Slice);
    let base_func_type = ValueRttid::new(4, ValueKind::Closure);
    let named_func_type = ValueRttid::new(5, ValueKind::Closure);
    let struct_type = ValueRttid::new(7, ValueKind::Struct);

    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.runtime_types.push(RuntimeType::Slice(int_type));
    module.named_type_metas.push(NamedTypeMeta {
        name: "BaseNumbers".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Slice),
        underlying_rttid: base_slice_type,
        methods: Default::default(),
    });
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "NamedNumbers".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Slice),
        underlying_rttid: named_slice_type,
        methods: Default::default(),
    });
    module.runtime_types.push(RuntimeType::Named {
        id: 1,
        struct_meta_id: None,
    });
    module.runtime_types.push(RuntimeType::Func {
        params: vec![chained_slice_type],
        results: vec![int_type],
        variadic: true,
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "BaseCallable".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Closure),
        underlying_rttid: base_func_type,
        methods: Default::default(),
    });
    module.runtime_types.push(RuntimeType::Named {
        id: 2,
        struct_meta_id: None,
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "NamedCallable".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Closure),
        underlying_rttid: named_func_type,
        methods: Default::default(),
    });
    module.runtime_types.push(RuntimeType::Named {
        id: 3,
        struct_meta_id: None,
    });
    module.struct_metas.push(StructMeta {
        slot_types: Vec::new(),
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.runtime_types.push(RuntimeType::Struct {
        fields: Vec::new(),
        meta_id: 0,
    });
    module.runtime_types.push(RuntimeType::Pointer(struct_type));
    module.runtime_types.push(RuntimeType::Func {
        params: vec![int_type, int_type],
        results: vec![int_type],
        variadic: false,
    });

    call_module_metadata_provider(assert_named_func_and_slice_resolution, &module)
        .expect("named dynamic metadata helpers should resolve through the shared resolver");
}

#[cfg(feature = "std")]
fn assert_cyclic_named_runtime_types_fail_closed(call: &mut ExternCallContext<'_>) -> ExternResult {
    for func_rttid in [1, 2] {
        assert!(call.get_func_signature(func_rttid).is_none());
        assert!(call.get_func_results(func_rttid).is_none());
    }
    for slice_rttid in [3, 4] {
        assert!(call.get_slice_elem(slice_rttid).is_none());
    }
    assert!(call.check_func_signature_compatible(1, 2).is_err());
    ExternResult::Ok
}

#[cfg(feature = "std")]
#[test]
fn ffi_dynamic_type_helpers_reject_cyclic_named_metadata_without_recursion() {
    let mut module = Module::new("ffi-cyclic-named-dynamic-types".to_string());
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::Int64));
    module.named_type_metas.push(NamedTypeMeta {
        name: "FuncCycleA".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Closure),
        underlying_rttid: ValueRttid::new(2, ValueKind::Closure),
        methods: Default::default(),
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "FuncCycleB".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Closure),
        underlying_rttid: ValueRttid::new(1, ValueKind::Closure),
        methods: Default::default(),
    });
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    module.runtime_types.push(RuntimeType::Named {
        id: 1,
        struct_meta_id: None,
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "SliceCycleA".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Slice),
        underlying_rttid: ValueRttid::new(4, ValueKind::Slice),
        methods: Default::default(),
    });
    module.named_type_metas.push(NamedTypeMeta {
        name: "SliceCycleB".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::Slice),
        underlying_rttid: ValueRttid::new(3, ValueKind::Slice),
        methods: Default::default(),
    });
    module.runtime_types.push(RuntimeType::Named {
        id: 2,
        struct_meta_id: None,
    });
    module.runtime_types.push(RuntimeType::Named {
        id: 3,
        struct_meta_id: None,
    });

    call_module_metadata_provider(assert_cyclic_named_runtime_types_fail_closed, &module)
        .expect("cyclic named metadata should return None without escaping a panic");
}

#[cfg(feature = "std")]
fn assert_ffi_runtime_assignment_rules(call: &mut ExternCallContext<'_>) -> ExternResult {
    let named_implementer = ValueRttid::new(2, ValueKind::Int64);
    let named_without_method = ValueRttid::new(3, ValueKind::Int64);
    let method_interface = ValueRttid::new(4, ValueKind::Interface);
    let empty_interface = ValueRttid::new(5, ValueKind::Interface);

    assert!(call.value_rttids_compatible(named_implementer, method_interface));
    assert!(!call.value_rttids_compatible(named_without_method, method_interface));
    assert!(call.value_rttids_compatible(named_without_method, empty_interface));
    assert!(!call.value_rttids_compatible(empty_interface, named_without_method));
    assert!(!call.value_rttids_compatible(ValueRttid::new(99, ValueKind::Int64), empty_interface));
    ExternResult::Ok
}

#[cfg(feature = "std")]
#[test]
fn ffi_runtime_assignment_checks_non_empty_interface_method_sets() {
    let mut module = Module::new("ffi-interface-assignability".to_string());
    let int_type = ValueRttid::new(0, ValueKind::Int64);
    let method_signature = ValueRttid::new(1, ValueKind::Closure);
    module.runtime_types.extend([
        RuntimeType::Basic(ValueKind::Int64),
        RuntimeType::Func {
            params: Vec::new(),
            results: Vec::new(),
            variadic: false,
        },
    ]);

    let mut methods = std::collections::BTreeMap::new();
    methods.insert(
        "M".to_string(),
        vo_common_core::bytecode::MethodInfo {
            func_id: 0,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: method_signature.rttid(),
        },
    );
    module.named_type_metas.extend([
        NamedTypeMeta {
            name: "T".to_string(),
            underlying_meta: ValueMeta::new(0, ValueKind::Int64),
            underlying_rttid: int_type,
            methods,
        },
        NamedTypeMeta {
            name: "U".to_string(),
            underlying_meta: ValueMeta::new(0, ValueKind::Int64),
            underlying_rttid: int_type,
            methods: Default::default(),
        },
    ]);
    module.runtime_types.extend([
        RuntimeType::Named {
            id: 0,
            struct_meta_id: None,
        },
        RuntimeType::Named {
            id: 1,
            struct_meta_id: None,
        },
    ]);
    module.interface_metas.extend([
        InterfaceMeta {
            name: "MOnly".to_string(),
            method_names: vec!["M".to_string()],
            methods: vec![vo_common_core::bytecode::InterfaceMethodMeta {
                name: "M".to_string(),
                signature_rttid: method_signature.rttid(),
            }],
        },
        InterfaceMeta {
            name: "Any".to_string(),
            method_names: Vec::new(),
            methods: Vec::new(),
        },
    ]);
    module.runtime_types.extend([
        RuntimeType::Interface {
            methods: vec![vo_common_core::runtime_type::InterfaceMethod::new(
                "M".to_string(),
                method_signature,
            )],
            meta_id: 0,
        },
        RuntimeType::Interface {
            methods: Vec::new(),
            meta_id: 1,
        },
    ]);

    call_module_metadata_provider(assert_ffi_runtime_assignment_rules, &module)
        .expect("FFI assignment checks should share runtime interface satisfaction rules");
}

#[test]
fn ffi_runtime_metadata_does_not_fallback_to_meta_zero() {
    let ffi_src =
        vo_source_contract::production_source_without_test_modules(include_str!("mod.rs"));
    let dynamic_src = vo_source_contract::production_source_without_test_modules(include_str!(
        "../builtins/dynamic.rs"
    ));
    let sources = [
        ("ffi/mod.rs", ffi_src.as_str()),
        ("builtins/dynamic.rs", dynamic_src.as_str()),
    ];

    for (name, src) in sources {
        let normalized = src.split_whitespace().collect::<Vec<_>>().join(" ");
        for forbidden in [
                "get_struct_meta_id_from_rttid(rttid) .unwrap_or(0)",
                "get_interface_meta_id_from_rttid(rttid) .unwrap_or(0)",
                "get_struct_meta_id_from_rttid(actual_rttid) .unwrap_or(0)",
                "InterfaceSlot::new(raw_slots[0], raw_slots.get(1).copied().unwrap_or(0))",
                "InterfaceSlot::new(slot0, raw_slots.first().copied().unwrap_or(0))",
                "if let Some(meta) = self.struct_meta(*meta_id as usize) { return meta.slot_count(); } 2",
                "if let Some(named_meta) = self.module.named_type_metas.get(*named_id as usize)",
                "require_struct_meta_id_from_rttid(actual_rttid, \"dynamic call result boxing\")",
            ] {
                assert!(
                    !normalized.contains(forbidden),
                    "{name} must fail fast on RTTID-to-runtime-metadata drift instead of falling back to metadata id 0"
                );
            }
    }
}

#[test]
fn box_to_interface_raw_slots_are_exact_layout_authority() {
    let ffi_src =
        vo_source_contract::production_source_without_test_modules(include_str!("mod.rs"));
    let normalized = ffi_src.split_whitespace().collect::<Vec<_>>().join(" ");

    assert!(
        normalized.contains("assert_eq!( raw_slots.len(), expected_slots,"),
        "box_to_interface must reject raw slot-count drift before reading slots"
    );
    assert!(
            normalized
                .contains("array::set_n(new_ref, i, &raw_slots[src_start..src_end], elem_bytes)"),
            "array boxing must copy by exact element layout instead of treating packed arrays as u64 buffers"
        );
    assert!(
        normalized.contains(".checked_mul(len)"),
        "array runtime slot counts must be overflow-checked"
    );
}

#[test]
fn array_runtime_slot_count_rejects_target_width_overflow() {
    assert_eq!(checked_array_slot_count(3, 4), Some(12));
    assert_eq!(checked_array_slot_count(0, usize::MAX), Some(0));
    assert_eq!(checked_array_slot_count(2, usize::MAX), None);
}

#[test]
fn runtime_map_surfaces_use_checked_map_api_and_pre_set_barriers_048() {
    let sources = [
        ("ffi/mod.rs", include_str!("mod.rs")),
        ("ffi/containers.rs", include_str!("containers.rs")),
        (
            "builtins/dynamic.rs",
            include_str!("../builtins/dynamic.rs"),
        ),
        ("pack.rs", include_str!("../pack.rs")),
        ("gc.rs", include_str!("../gc.rs")),
    ];

    for (name, raw_source) in sources {
        let source = vo_source_contract::production_source_without_test_modules(raw_source);
        for forbidden in [
            "map::get(",
            "map::set(",
            "map::delete(",
            "map::contains(",
            "crate::objects::map::get(",
            "crate::objects::map::set(",
            "crate::objects::map::delete(",
            "crate::objects::map::contains(",
        ] {
            assert!(
                !source.contains(forbidden),
                "{name} must use checked map APIs instead of lossy wrapper {forbidden}"
            );
        }
    }

    let map_source = vo_source_contract::production_source_without_test_modules(include_str!(
        "../objects/map.rs"
    ));
    for forbidden in [
        "pub fn get(",
        "pub fn set(",
        "pub fn delete(",
        "pub fn contains(",
    ] {
        assert!(
                !map_source.contains(forbidden),
                "objects::map must not expose lossy wrapper {forbidden}; checked APIs are the runtime fact source"
            );
    }

    let ffi_source =
        vo_source_contract::production_source_without_test_modules(include_str!("mod.rs"));
    let map_set_string_key = ffi_source
        .split("pub unsafe fn map_set_string_key(")
        .nth(1)
        .and_then(|rest| rest.split("/// Find the rttid").next())
        .expect("ExternCallContext::map_set_string_key section");
    let set_pos = map_set_string_key
        .find("map::set_checked(")
        .expect("ExternCallContext::map_set_string_key must use set_checked");
    let width_check_pos = map_set_string_key
        .find("validate_entry_slot_counts(")
        .expect("ExternCallContext::map_set_string_key must validate entry width");
    let key_barrier_pos = map_set_string_key
        .find("typed_write_barrier_by_meta(m, &key_data")
        .expect("ExternCallContext::map_set_string_key must barrier key roots");
    let val_barrier_pos = map_set_string_key
        .find("typed_write_barrier_by_meta(m, val")
        .expect("ExternCallContext::map_set_string_key must barrier value roots");
    assert!(
        width_check_pos < key_barrier_pos && width_check_pos < val_barrier_pos,
        "ExternCallContext::map_set_string_key must validate key/value widths before barriers"
    );
    assert!(
        key_barrier_pos < set_pos && val_barrier_pos < set_pos,
        "ExternCallContext::map_set_string_key must barrier key/value roots before insertion"
    );

    let containers_source =
        vo_source_contract::production_source_without_test_modules(include_str!("containers.rs"));
    assert!(
        !containers_source.contains("pub struct VoMap")
            && !containers_source.contains("pub fn set_raw("),
        "allocator-specific VoMap accessors must stay outside the native extension surface"
    );
}
