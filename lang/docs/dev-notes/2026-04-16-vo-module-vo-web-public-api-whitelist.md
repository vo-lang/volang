# vo-module and vo-web Public API Whitelist

This document defines the only public APIs that are allowed to remain public in `vo-module` and `vo-web`.

## Admission bar

An API is allowed on the whitelist only if at least one of the following is true:

- It is part of the crate's core responsibility.
- It is an obvious stable boundary for other first-party crates.
- It is a CLI boundary or a `#[wasm_bindgen]` JS/WASM boundary.
- It is a canonical data type required to move module-system or web-runtime state across crate boundaries.

The following are explicitly excluded:

- Historical leftovers.
- Convenience wrappers.
- Planning/reporting facades.
- Test helpers.
- Migration shims.
- Public exposure of internal struct layout when a narrower contract is sufficient.
- Studio-only justification.

Anything public that is not listed below should be removed or narrowed.

## vo-module whitelist

### Root

- `vo_module::Error`
- `vo_module::Result`

### `vo_module::identity`

Types:

- `ModulePath`
- `ImportClass`
- `ArtifactId`

Functions:

- `classify_import`
- `extract_module_root`
- `check_internal_visibility`
- `find_owning_module`

Allowed `ModulePath` public surface:

- parse / display / string access
- owner/repo extraction
- module-root extraction
- version compatibility query
- version-tag rendering
- import ownership query

### `vo_module::version`

Types:

- `PreRelease`
- `SemVer`
- `ExactVersion`
- `ToolchainVersion`
- `ConstraintOp`
- `DepConstraint`
- `ToolchainConstraint`

Allowed public surface:

- parse / display / comparison / hashing
- exact version to semver access

### `vo_module::digest`

Types:

- `Digest`

### `vo_module::schema::modfile`

Types:

- `ModFile`
- `Require`
- `Replace`

Allowed public surface:

- parse
- render

### `vo_module::schema::lockfile`

Types:

- `LockFile`
- `LockRoot`
- `LockedModule`
- `LockedArtifact`

Allowed public surface:

- parse
- render
- module lookup by path

### `vo_module::schema::manifest`

Types:

- `ReleaseManifest`
- `ManifestRequire`
- `ManifestSource`
- `ManifestArtifact`

Allowed public surface:

- parse
- render

### `vo_module::schema::workfile`

Types:

- `WorkFile`
- `UseEntry`

Allowed public surface:

- parse
- render

### `vo_module::ext_manifest`

Types:

- `ExtensionManifest`
- `NativeExtensionConfig`
- `NativeTargetDeclaration`
- `DeclaredArtifactId`
- `WasmExtensionKind`
- `WasmExtensionManifest`

Functions:

- `discover_extensions`
- `parse_ext_manifest_content`
- `extension_name_from_content`
- `include_paths_from_content`

Allowed `ExtensionManifest` public methods:

- `resolve_local_native_path`
- `declared_artifact_ids`
- `declared_native_target`

### `vo_module::registry`

Types:

- `Registry`
- `RepositoryId`

Functions:

- `repository_id`
- `version_from_tag`
- `release_download_url`
- `parse_requested_release_manifest_for_spec`

### `vo_module::github_registry`

Types:

- `GitHubRegistry`

### `vo_module::workspace`

Functions:

- `discover_workfile`

### `vo_module::operation_error`

Types:

- `OperationError`

### `vo_module::project`

Types:

- `ProjectDeps`
- `ProjectDepsStage`
- `ProjectDepsErrorKind`
- `ProjectDepsError`
- `ProjectContext`

Functions:

- `read_mod_file`
- `read_lock_file`
- `read_project_deps`
- `read_project_deps_at_root`
- `read_inline_project_deps`
- `find_project_root`
- `load_project_context`

Layout rule:

- `ProjectDeps` variants and backing state are private
- `ProjectContext` fields are private
- access is through explicit accessor methods only

Allowed `ProjectDeps` public operations:

- module presence check
- current module query
- allowed module query
- locked module query
- lock file query
- locked module extraction

Allowed `ProjectContext` public operations:

- project root query
- project deps query
- workspace replace map query
- decomposition into owned parts

### `vo_module::artifact`

Types:

- `RequiredArtifact`

Functions:

- `required_artifacts_for_target`

### `vo_module::readiness`

Types:

- `ResolvedArtifact`
- `ReadyModule`
- `ModuleReadiness`
- `ReadinessFailure`

Functions:

- `check_module_readiness`
- `check_project_readiness`

### `vo_module::lock`

Functions:

- `validate_locked_module_against_manifest`

### `vo_module::lifecycle`

Functions:

- `download_locked_dependencies`

### `vo_module::ops`

Types:

- `TidyResult`
- `CleanResult`

Functions:

- `mod_init`
- `mod_add`
- `mod_update`
- `mod_sync`
- `mod_download`
- `mod_verify`
- `mod_remove`
- `mod_tidy`
- `mod_why`
- `mod_clean`

### `vo_module::cache::layout`

Constants:

- `VERSION_MARKER`
- `SOURCE_DIGEST_MARKER`

Functions:

- `cache_dir`
- `relative_module_dir`
- `discover_installed_version`
- `module_identity_from_cache_dir`

### `vo_module::cache::install`

Types:

- `ExactInstallResult`

Functions:

- `install_exact_module`
- `extract_source_entries`

### `vo_module::cache::validate`

Types:

- `InstalledModuleError`
- `InstalledModuleField`
- `InstalledModuleErrorKind`

Functions:

- `validate_installed_module`
- `validate_installed_artifact`

## vo-web whitelist

### Root WASM boundary

Types:

- `CompileResult`
- `RunResult`
- `VoVm`

Functions:

- `init`
- `version`

### `vo_web::vm` / VM boundary

Types:

- `ExternCallContext`
- `ExternRegistry`
- `ExternResult`
- `GcRef`
- `ExternDef`
- `ExternRegistrar`
- `Module`
- `Vm`

Re-exports:

- `ext_bridge`

Functions:

- `create_vm`
- `call_closure`
- `take_output`
- `run`
- `run_with_args`

### `vo_web::async_runner`

Functions:

- `preload_ext_module`
- `compile_and_run`
- `compile_and_run_with_modules`

### `vo_web::compile`

Functions:

- `build_stdlib_fs`
- `compile`
- `compile_source_with_mod_fs`

### `vo_web::module_install`

Functions:

- `install_module_to_vfs`
- `ensure_vfs_deps`
- `ensure_vfs_deps_from_fs`
- `resolve_and_install_module`
- `resolve_and_install_module_with_constraint`

## Deletion rule

After this document is written, every public API in `vo-module` or `vo-web` that is not explicitly listed here should be removed or narrowed.
