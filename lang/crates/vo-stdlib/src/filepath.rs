//! path/filepath package native function implementations.

#[cfg(feature = "std")]
use std::fs;

#[cfg(feature = "std")]
use vo_ffi_macro::vostd_fn;
#[cfg(feature = "std")]
use vo_runtime::builtins::error_helper::write_nil_error;
#[cfg(feature = "std")]
use vo_runtime::ffi::{ExternCallContext, ExternResult};

#[cfg(feature = "std")]
#[vostd_fn("path/filepath", "evalSymlinks", std)]
fn filepath_eval_symlinks(call: &mut ExternCallContext) -> ExternResult {
    let path = match crate::host_bytes::path_buf_from_bytes(
        call.arg_string_bytes(slots::ARG_PATH),
        "path",
    ) {
        Ok(path) => path,
        Err(error) => {
            call.ret_string_bytes(slots::RET_0, b"");
            crate::os::write_io_error(call, slots::RET_1, error);
            return ExternResult::Ok;
        }
    };

    match eval_symlinks_impl(&path)
        .and_then(|resolved| crate::host_bytes::path_buf_into_bytes(resolved, "resolved path"))
    {
        Ok(resolved) => {
            call.ret_string_bytes(slots::RET_0, &resolved);
            write_nil_error(call, slots::RET_1);
        }
        Err(error) => {
            call.ret_string_bytes(slots::RET_0, b"");
            crate::os::write_io_error(call, slots::RET_1, error);
        }
    }

    ExternResult::Ok
}

/// Resolve a path against the host process working directory using the host's
/// platform path rules. On Windows this preserves drive-relative and
/// rooted-on-current-drive semantics supplied by `GetFullPathNameW` through
/// the standard library.
#[cfg(feature = "std")]
#[vostd_fn("path/filepath", "absPath", std)]
fn filepath_abs_path(call: &mut ExternCallContext) -> ExternResult {
    let path = match crate::host_bytes::path_buf_from_bytes(
        call.arg_string_bytes(slots::ARG_PATH),
        "path",
    ) {
        Ok(path) => path,
        Err(error) => {
            call.ret_string_bytes(slots::RET_0, b"");
            crate::os::write_io_error(call, slots::RET_1, error);
            return ExternResult::Ok;
        }
    };

    match absolute_path_impl(&path)
        .and_then(|absolute| crate::host_bytes::path_buf_into_bytes(absolute, "absolute path"))
    {
        Ok(absolute) => {
            call.ret_string_bytes(slots::RET_0, &absolute);
            write_nil_error(call, slots::RET_1);
        }
        Err(error) => {
            call.ret_string_bytes(slots::RET_0, b"");
            crate::os::write_io_error(call, slots::RET_1, error);
        }
    }

    ExternResult::Ok
}

#[cfg(feature = "std")]
fn absolute_path_impl(path: &std::path::Path) -> std::io::Result<std::path::PathBuf> {
    std::path::absolute(path).map_err(|error| {
        std::io::Error::new(error.kind(), format!("absolute path {path:?}: {error}"))
    })
}

#[cfg(feature = "std")]
fn eval_symlinks_impl(path: &std::path::Path) -> std::io::Result<std::path::PathBuf> {
    eval_symlinks_components(path).map_err(|error| {
        std::io::Error::new(error.kind(), format!("eval symlinks {path:?}: {error}"))
    })
}

#[cfg(feature = "std")]
#[derive(Debug)]
enum OwnedPathComponent {
    Prefix(std::ffi::OsString),
    Root(std::ffi::OsString),
    Current,
    Parent,
    Normal(std::ffi::OsString),
}

#[cfg(feature = "std")]
fn take_path_component(path: &std::path::Path) -> Option<(OwnedPathComponent, std::path::PathBuf)> {
    use std::path::Component;

    let mut components = path.components();
    let component = components.next()?;
    let owned = match component {
        Component::Prefix(prefix) => OwnedPathComponent::Prefix(prefix.as_os_str().to_os_string()),
        Component::RootDir => OwnedPathComponent::Root(component.as_os_str().to_os_string()),
        Component::CurDir => OwnedPathComponent::Current,
        Component::ParentDir => OwnedPathComponent::Parent,
        Component::Normal(name) => OwnedPathComponent::Normal(name.to_os_string()),
    };
    Some((owned, components.as_path().to_path_buf()))
}

#[cfg(feature = "std")]
fn append_pending_path(
    target: std::path::PathBuf,
    remaining: &std::path::Path,
) -> std::path::PathBuf {
    let mut pending = target;
    if !remaining.as_os_str().is_empty() {
        pending.push(remaining);
    }
    pending
}

#[cfg(feature = "std")]
fn push_clean_parent(path: &mut std::path::PathBuf) {
    if path.as_os_str().is_empty()
        || matches!(
            path.components().next_back(),
            Some(std::path::Component::ParentDir)
        )
    {
        path.push("..");
    } else {
        // Root and platform prefixes cannot be popped. In that case `..` is
        // lexically absorbed, matching filepath.Clean.
        let _ = path.pop();
    }
}

#[cfg(feature = "std")]
fn eval_symlinks_components(path: &std::path::Path) -> std::io::Result<std::path::PathBuf> {
    const MAX_SYMLINKS: usize = 255;

    if path.as_os_str().is_empty() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "empty path has no filesystem entry",
        ));
    }

    let input_was_relative = path.is_relative();
    let mut resolved = std::path::PathBuf::new();
    let mut pending = path.to_path_buf();
    let mut followed_symlinks = 0usize;
    let mut checked_entry = false;

    while let Some((component, remaining)) = take_path_component(&pending) {
        pending = remaining;
        match component {
            OwnedPathComponent::Prefix(prefix) => {
                resolved.clear();
                resolved.push(prefix);
            }
            OwnedPathComponent::Root(root) => resolved.push(root),
            OwnedPathComponent::Current => {}
            OwnedPathComponent::Parent => push_clean_parent(&mut resolved),
            OwnedPathComponent::Normal(name) => {
                let candidate = resolved.join(&name);
                let metadata = fs::symlink_metadata(&candidate).map_err(|error| {
                    std::io::Error::new(
                        error.kind(),
                        format!("inspect path component {candidate:?}: {error}"),
                    )
                })?;
                checked_entry = true;
                if !metadata.file_type().is_symlink() {
                    resolved = candidate;
                    continue;
                }

                followed_symlinks = followed_symlinks.saturating_add(1);
                if followed_symlinks > MAX_SYMLINKS {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        format!("too many symbolic links while resolving {candidate:?}"),
                    ));
                }
                let target = fs::read_link(&candidate)?;
                let target_resets_base = target.is_absolute()
                    || target.has_root()
                    || matches!(
                        target.components().next(),
                        Some(std::path::Component::Prefix(_))
                    );
                if target_resets_base {
                    resolved.clear();
                }
                pending = append_pending_path(target, &pending);
            }
        }
    }

    if !checked_entry {
        // Paths consisting only of roots, prefixes, or dot components still
        // need an existence/permission check.
        fs::metadata(path)?;
    }
    if input_was_relative && resolved.as_os_str().is_empty() {
        resolved.push(".");
    }
    Ok(resolved)
}

#[cfg(all(test, feature = "std", unix))]
mod tests {
    use super::*;
    #[cfg(not(target_vendor = "apple"))]
    use std::os::unix::ffi::OsStringExt;
    use std::os::unix::fs::symlink;

    fn temp_root(label: &str) -> std::path::PathBuf {
        std::env::temp_dir().join(format!(
            "vo-filepath-{label}-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ))
    }

    fn relative_path(from: &std::path::Path, to: &std::path::Path) -> std::path::PathBuf {
        let from_components: Vec<_> = from.components().collect();
        let to_components: Vec<_> = to.components().collect();
        let common = from_components
            .iter()
            .zip(&to_components)
            .take_while(|(left, right)| left == right)
            .count();
        let mut relative = std::path::PathBuf::new();
        for component in &from_components[common..] {
            if matches!(component, std::path::Component::Normal(_)) {
                relative.push("..");
            }
        }
        for component in &to_components[common..] {
            relative.push(component.as_os_str());
        }
        relative
    }

    #[cfg(not(target_vendor = "apple"))]
    #[test]
    fn eval_symlinks_preserves_non_utf8_unix_paths() {
        let root = temp_root("bytes");
        std::fs::create_dir(&root).unwrap();
        let leaf = root.join(std::ffi::OsString::from_vec(vec![b'x', 0xff]));
        if let Err(error) = std::fs::write(&leaf, b"ok") {
            let _ = std::fs::remove_dir_all(&root);
            if error.kind() == std::io::ErrorKind::PermissionDenied {
                eprintln!("host sandbox does not permit non-UTF-8 file names: {error}");
                return;
            }
            panic!("create non-UTF-8 test path: {error}");
        }

        let resolved = eval_symlinks_impl(&leaf).unwrap();
        assert_eq!(resolved.file_name(), leaf.file_name());

        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn eval_symlinks_preserves_relative_inputs_and_resolves_relative_targets() {
        let root = temp_root("relative");
        let directory = root.join("directory");
        let target = root.join("target");
        std::fs::create_dir_all(&directory).unwrap();
        std::fs::create_dir_all(&target).unwrap();
        std::fs::write(target.join("leaf"), b"ok").unwrap();
        symlink("../target/leaf", directory.join("link")).unwrap();

        let cwd = std::env::current_dir().unwrap();
        let input = relative_path(&cwd, &directory.join("link"));
        let resolved = eval_symlinks_impl(&input).unwrap();
        assert!(resolved.is_relative());
        assert_eq!(
            std::fs::canonicalize(cwd.join(resolved)).unwrap(),
            std::fs::canonicalize(target.join("leaf")).unwrap()
        );

        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn eval_symlinks_absolute_target_switches_to_absolute_output() {
        let root = temp_root("absolute");
        std::fs::create_dir_all(&root).unwrap();
        let target = root.join("target");
        std::fs::write(&target, b"ok").unwrap();
        let link = root.join("link");
        symlink(&target, &link).unwrap();

        let resolved = eval_symlinks_impl(&link).unwrap();
        assert!(resolved.is_absolute());
        assert_eq!(
            std::fs::canonicalize(resolved).unwrap(),
            std::fs::canonicalize(target).unwrap()
        );

        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn eval_symlinks_rejects_missing_leaf_and_cycles() {
        let root = temp_root("errors");
        std::fs::create_dir_all(&root).unwrap();
        assert_eq!(
            eval_symlinks_impl(&root.join("missing"))
                .unwrap_err()
                .kind(),
            std::io::ErrorKind::NotFound
        );

        symlink("second", root.join("first")).unwrap();
        symlink("first", root.join("second")).unwrap();
        let error = eval_symlinks_impl(&root.join("first")).unwrap_err();
        assert_eq!(error.kind(), std::io::ErrorKind::InvalidData);

        std::fs::remove_dir_all(root).unwrap();
    }
}

#[cfg(feature = "std")]
vo_ffi_macro::vostd_register!("path/filepath":
    evalSymlinks, absPath,
);
