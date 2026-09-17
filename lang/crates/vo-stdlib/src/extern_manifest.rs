//! Declaration contracts shared with codegen; provider conformance stays here.
pub use vo_common_core::extern_contracts::{
    known_stdlib_extern_allowed_effects as known_extern_allowed_effects, HTTP_REQUEST, TIME_SLEEP,
    WAIT_IO,
};
#[cfg(test)]
use vo_runtime::bytecode::ExternEffects;
use vo_runtime::ffi::ExternEffectManifestEntry;

macro_rules! manifest {
    ($( (canonical($package:literal, $function:literal), $provider:ident, $effects:expr) ),* $(,)?) => {
        pub const EFFECT_MANIFEST: &[ExternEffectManifestEntry] = &[$(
            ExternEffectManifestEntry::new(vo_runtime::vo_extern_name!($package, $function), $effects)
        ),*];
    };
}
vo_common_core::vo_stdlib_extern_contracts!(manifest);

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    #[cfg(feature = "std")]
    use std::collections::BTreeSet;
    #[cfg(feature = "std")]
    use std::fs;
    #[cfg(feature = "std")]
    use std::path::{Path, PathBuf};

    use super::*;

    fn manifest_map() -> BTreeMap<&'static str, ExternEffects> {
        EFFECT_MANIFEST
            .iter()
            .map(|entry| (entry.name, entry.effects))
            .collect()
    }

    #[cfg(feature = "std")]
    fn provider_names() -> BTreeSet<&'static str> {
        let mut names = BTreeSet::new();
        let tables: &[&[vo_runtime::ffi::StdlibEntry]] = &[
            crate::math::__VO_STDLIB_ENTRIES,
            crate::bits::__VO_STDLIB_ENTRIES,
            crate::rand::__VO_STDLIB_ENTRIES,
            crate::rand::native::__VO_STDLIB_ENTRIES,
            crate::bytes::__VO_STDLIB_ENTRIES,
            crate::errors::__VO_STDLIB_ENTRIES,
            crate::strings::__VO_STDLIB_ENTRIES,
            crate::strconv::__VO_STDLIB_ENTRIES,
            crate::unicode::__VO_STDLIB_ENTRIES,
            crate::json::__VO_STDLIB_ENTRIES,
            crate::toml_pkg::__VO_STDLIB_ENTRIES,
            crate::regexp::__VO_STDLIB_ENTRIES,
            crate::os::__VO_STDLIB_ENTRIES,
            crate::filepath::__VO_STDLIB_ENTRIES,
            crate::exec::__VO_STDLIB_ENTRIES,
            crate::fmt::__VO_STDLIB_ENTRIES,
            crate::fmt::native::__VO_STDLIB_ENTRIES,
            crate::io::REGISTERED_EXTERNS,
            crate::time::REGISTERED_EXTERNS,
            crate::toolchain::REGISTERED_EXTERNS,
            crate::net::REGISTERED_EXTERNS,
            crate::net::http::REGISTERED_EXTERNS,
        ];
        for table in tables {
            for entry in *table {
                names.insert(entry.name());
            }
        }
        names
    }

    #[cfg(feature = "std")]
    fn collect_vo_files(dir: &Path, out: &mut Vec<PathBuf>) {
        for entry in fs::read_dir(dir).unwrap_or_else(|err| {
            panic!("could not read stdlib directory {}: {err}", dir.display())
        }) {
            let entry = entry.expect("could not read stdlib directory entry");
            let path = entry.path();
            if path.is_dir() {
                collect_vo_files(&path, out);
            } else if path.extension().and_then(|ext| ext.to_str()) == Some("vo") {
                out.push(path);
            }
        }
    }

    #[cfg(feature = "std")]
    fn stdlib_extern_declarations() -> Vec<String> {
        let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../stdlib");
        let mut files = Vec::new();
        collect_vo_files(&root, &mut files);
        files.sort();

        let mut package_names = BTreeMap::<String, String>::new();
        let mut declarations = BTreeMap::<String, PathBuf>::new();
        for path in files {
            let rel_dir = path
                .parent()
                .expect("stdlib file should have parent")
                .strip_prefix(&root)
                .expect("stdlib file should be under root");
            let package_path = rel_dir
                .components()
                .map(|component| component.as_os_str().to_string_lossy())
                .collect::<Vec<_>>()
                .join("/");
            let source = fs::read_to_string(&path)
                .unwrap_or_else(|err| panic!("could not read {}: {err}", path.display()));
            let (file, diagnostics, interner) = vo_syntax::parse(&source, 0);
            if diagnostics.has_errors() {
                let messages = diagnostics
                    .iter()
                    .filter(|diagnostic| diagnostic.is_error())
                    .map(|diagnostic| diagnostic.message.as_str())
                    .collect::<Vec<_>>()
                    .join("; ");
                panic!("could not parse {}: {messages}", path.display());
            }
            let package_name = file
                .package
                .as_ref()
                .and_then(|package| interner.resolve(package.symbol))
                .filter(|name| !name.is_empty())
                .unwrap_or_else(|| panic!("{} has no package declaration", path.display()));
            if let Some(previous) = package_names.insert(package_path.clone(), package_name.into())
            {
                assert_eq!(
                    previous,
                    package_name,
                    "package declaration mismatch in {}",
                    path.display()
                );
            }

            for declaration in &file.decls {
                let vo_syntax::ast::Decl::Func(function) = declaration else {
                    continue;
                };
                if !function.is_extern() {
                    continue;
                }
                let name = interner
                    .resolve(function.name.symbol)
                    .filter(|name| !name.is_empty())
                    .unwrap_or_else(|| panic!("{} has an unnamed extern", path.display()));
                let identity = vo_common::abi::try_abi_lookup_name(&package_path, name)
                    .unwrap_or_else(|error| {
                        panic!(
                            "invalid stdlib extern identity {package_path:?} / {name:?} in {}: {error}",
                            path.display()
                        )
                    });
                if let Some(previous) = declarations.insert(identity.clone(), path.clone()) {
                    panic!(
                        "duplicate stdlib extern {identity:?} in {} and {}",
                        previous.display(),
                        path.display()
                    );
                }
            }
        }
        declarations.into_keys().collect()
    }

    #[cfg(feature = "std")]
    fn is_runtime_builtin_extern(name: &str) -> bool {
        vo_runtime::builtins::known_extern_allowed_effects(name).is_some()
    }

    #[test]
    fn manifest_names_are_unique() {
        let map = manifest_map();
        assert_eq!(
            map.len(),
            EFFECT_MANIFEST.len(),
            "duplicate stdlib extern effect manifest entry"
        );
    }

    #[test]
    fn manifest_declares_cross_provider_upper_bounds() {
        assert_eq!(
            known_extern_allowed_effects(vo_runtime::vo_extern_name!("fmt", "nativeSprintf")),
            Some(ExternEffects::NONE)
        );
        assert_eq!(
            known_extern_allowed_effects(vo_runtime::vo_extern_name!("os", "blocking_fileRead")),
            Some(ExternEffects::MAY_WAIT_IO_REPLAY)
        );
        assert_eq!(
            known_extern_allowed_effects(vo_runtime::vo_extern_name!("time", "blocking_sleepNano")),
            Some(TIME_SLEEP)
        );
        assert_eq!(
            known_extern_allowed_effects(vo_runtime::vo_extern_name!(
                "net/http",
                "nativeHttpsRequest"
            )),
            Some(HTTP_REQUEST)
        );
        assert_eq!(
            known_extern_allowed_effects(vo_runtime::vo_extern_name!("os", "nativeExit")),
            Some(ExternEffects::MAY_EXIT)
        );
        // Allocation and ordinary GC interaction complete inside the native call;
        // the public Unmarshal wrapper performs any user callback before this
        // scheduler-transparent fallback is entered.
        assert_eq!(
            known_extern_allowed_effects(vo_runtime::vo_extern_name!(
                "encoding/json",
                "unmarshalAny"
            )),
            Some(ExternEffects::NONE)
        );
        assert_eq!(
            known_extern_allowed_effects(vo_runtime::vo_extern_name!("encoding/json", "Unmarshal")),
            None
        );
        assert_eq!(
            known_extern_allowed_effects(vo_runtime::vo_extern_name!("extension", "doThing")),
            None
        );
    }

    #[cfg(feature = "std")]
    #[test]
    fn every_stdlib_manifest_and_provider_name_uses_the_canonical_codec() {
        for entry in EFFECT_MANIFEST {
            vo_common::abi::decode_extern_name(entry.name).unwrap_or_else(|error| {
                panic!("non-canonical manifest name {:?}: {error}", entry.name)
            });
        }
        for name in provider_names() {
            vo_common::abi::decode_extern_name(name)
                .unwrap_or_else(|error| panic!("non-canonical provider name {name:?}: {error}"));
        }
    }

    #[cfg(feature = "std")]
    fn assert_provider_table(
        label: &str,
        entries: &[vo_runtime::ffi::StdlibEntry],
        manifest: &BTreeMap<&'static str, ExternEffects>,
    ) {
        for entry in entries {
            let allowed = manifest.get(entry.name()).copied().unwrap_or_else(|| {
                panic!(
                    "{label} provider extern '{}' is missing from stdlib effect manifest",
                    entry.name()
                )
            });
            assert!(
                entry.effects.is_subset_of(allowed),
                "{label} provider extern '{}' effects 0x{:x} exceed allowed 0x{:x}",
                entry.name(),
                entry.effects.bits(),
                allowed.bits()
            );
        }
    }

    #[cfg(feature = "std")]
    #[test]
    fn macro_registered_stdlib_externs_match_manifest() {
        let manifest = manifest_map();
        let tables: &[(&str, &[vo_runtime::ffi::StdlibEntry])] = &[
            ("math", crate::math::__VO_STDLIB_ENTRIES),
            ("math_bits", crate::bits::__VO_STDLIB_ENTRIES),
            ("math_rand", crate::rand::__VO_STDLIB_ENTRIES),
            ("math_rand_native", crate::rand::native::__VO_STDLIB_ENTRIES),
            ("bytes", crate::bytes::__VO_STDLIB_ENTRIES),
            ("errors", crate::errors::__VO_STDLIB_ENTRIES),
            ("strings", crate::strings::__VO_STDLIB_ENTRIES),
            ("strconv", crate::strconv::__VO_STDLIB_ENTRIES),
            ("unicode", crate::unicode::__VO_STDLIB_ENTRIES),
            ("encoding_json", crate::json::__VO_STDLIB_ENTRIES),
            ("encoding_toml", crate::toml_pkg::__VO_STDLIB_ENTRIES),
            ("regexp", crate::regexp::__VO_STDLIB_ENTRIES),
            ("os", crate::os::__VO_STDLIB_ENTRIES),
            ("path_filepath", crate::filepath::__VO_STDLIB_ENTRIES),
            ("os_exec", crate::exec::__VO_STDLIB_ENTRIES),
            ("fmt", crate::fmt::__VO_STDLIB_ENTRIES),
            ("fmt_native", crate::fmt::native::__VO_STDLIB_ENTRIES),
        ];
        for (label, entries) in tables {
            assert_provider_table(label, entries, &manifest);
        }
    }

    #[cfg(feature = "std")]
    #[test]
    fn handwritten_stdlib_register_tables_match_manifest() {
        let manifest = manifest_map();
        let tables: &[(&str, &[vo_runtime::ffi::StdlibEntry])] = &[
            ("io", crate::io::REGISTERED_EXTERNS),
            ("time", crate::time::REGISTERED_EXTERNS),
            ("toolchain", crate::toolchain::REGISTERED_EXTERNS),
            ("net", crate::net::REGISTERED_EXTERNS),
            ("net_http", crate::net::http::REGISTERED_EXTERNS),
        ];
        for (label, entries) in tables {
            assert_provider_table(label, entries, &manifest);
        }
    }

    #[cfg(feature = "std")]
    #[test]
    fn stdlib_extern_declarations_have_manifest_and_provider_ownership() {
        let manifest = manifest_map();
        let providers = provider_names();
        let missing = stdlib_extern_declarations()
            .into_iter()
            .filter(|name| !is_runtime_builtin_extern(name))
            .filter(|name| {
                !manifest.contains_key(name.as_str()) || !providers.contains(name.as_str())
            })
            .collect::<Vec<_>>();

        assert!(
            missing.is_empty(),
            "stdlib extern declarations missing manifest/provider ownership: {}",
            missing.join(", ")
        );
    }
}
