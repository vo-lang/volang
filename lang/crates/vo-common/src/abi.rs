fn normalize_path(path: &str) -> String {
    let mut parts: Vec<&str> = Vec::new();
    for component in path.split('/') {
        match component {
            "" | "." => {}
            ".." => {
                parts.pop();
            }
            other => parts.push(other),
        }
    }
    parts.join("/")
}

pub fn package_abi_path(
    package_path: &str,
    module_path: Option<&str>,
    extension_name: Option<&str>,
) -> String {
    let package_path = normalize_path(package_path);
    if let Some(module_path) = module_path {
        let module_path = normalize_path(module_path);
        let abi_root = extension_name.map(normalize_path).unwrap_or_else(|| {
            module_path
                .rsplit('/')
                .next()
                .map(normalize_path)
                .unwrap_or_else(|| module_path.clone())
        });
        if package_path == module_path {
            return abi_root;
        }
        if let Some(sub) = package_path.strip_prefix(&format!("{}/", module_path)) {
            return format!("{}/{}", abi_root, sub);
        }
    }
    package_path
}

pub fn normalize_abi_lookup_pkg_path(path: &str) -> String {
    normalize_path(path)
        .replace('/', "_")
        .replace('.', "_")
        .replace('-', "_")
}

pub fn abi_lookup_name(pkg_path: &str, func_name: &str) -> String {
    format!("{}_{}", normalize_abi_lookup_pkg_path(pkg_path), func_name)
}

#[cfg(test)]
mod tests {
    use super::{abi_lookup_name, normalize_abi_lookup_pkg_path, package_abi_path};

    #[test]
    fn package_abi_path_uses_extension_name_as_root() {
        assert_eq!(
            package_abi_path(
                "github.com/vo-lang/voplay/scene3d",
                Some("github.com/vo-lang/voplay"),
                Some("voplay"),
            ),
            "voplay/scene3d"
        );
    }

    #[test]
    fn package_abi_path_falls_back_to_module_basename() {
        assert_eq!(
            package_abi_path(
                "github.com/vo-lang/vogui/router",
                Some("github.com/vo-lang/vogui"),
                None,
            ),
            "vogui/router"
        );
    }

    #[test]
    fn package_abi_path_keeps_non_extension_path() {
        assert_eq!(
            package_abi_path("encoding/json", None, None),
            "encoding/json"
        );
    }

    #[test]
    fn normalize_abi_lookup_pkg_path_normalizes_components() {
        assert_eq!(
            normalize_abi_lookup_pkg_path("../libs/vo-play"),
            "libs_vo_play"
        );
    }

    #[test]
    fn abi_lookup_name_joins_pkg_and_func() {
        assert_eq!(
            abi_lookup_name("voplay/scene3d", "physicsInit"),
            "voplay_scene3d_physicsInit"
        );
    }
}
