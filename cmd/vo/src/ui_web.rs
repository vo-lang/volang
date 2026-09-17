//! Thin entry to the matching, independently packaged Web UI toolchain.

use std::env;
use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};
use std::process::Command;

// A manifest selects the replacement project tools for familiar positional
// commands. Projects without that manifest retain the compatibility commands.
pub(super) fn is_project_command(args: &[OsString]) -> bool {
    let cwd = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    project_command_in(args, &cwd)
}

fn project_command_in(args: &[OsString], cwd: &Path) -> bool {
    match args.first().and_then(|value| value.to_str()) {
        Some("create" | "check" | "preview" | "verify" | "browsers") => true,
        Some("build" | "dev" | "test" | "run" | "package" | "doctor") => {
            args[1..]
                .iter()
                .any(|value| value == OsStr::new("--project"))
                || args.len() == 2 && matches!(args[1].to_str(), Some("--help" | "-h"))
                || inferred_project(args, cwd)
                    .is_some_and(|directory| directory.join("ui-next.json").exists())
        }
        _ => false,
    }
}

fn inferred_project(args: &[OsString], cwd: &Path) -> Option<PathBuf> {
    match args.get(1) {
        None => Some(cwd.to_path_buf()),
        Some(value) if !value.to_string_lossy().starts_with('-') => Some(cwd.join(value)),
        _ => Some(cwd.to_path_buf()),
    }
}

// Keep the packaged implementation's explicit argument grammar as the single
// command executor. Native aliases normalize only the project position.
fn project_arguments(args: &[OsString]) -> Vec<OsString> {
    if !matches!(
        args.first().and_then(|value| value.to_str()),
        Some("check" | "build" | "dev" | "preview" | "test" | "run" | "package" | "doctor")
    ) || args[1..]
        .iter()
        .any(|value| value == OsStr::new("--project"))
        || args
            .get(1)
            .is_some_and(|value| matches!(value.to_str(), Some("--help" | "-h")))
    {
        return args.to_vec();
    }
    let mut normalized = vec![args[0].clone(), OsString::from("--project")];
    if args
        .get(1)
        .is_some_and(|value| value.to_string_lossy().starts_with('-'))
    {
        normalized.push(OsString::from("."));
        normalized.extend_from_slice(&args[1..]);
    } else {
        normalized.push(args.get(1).cloned().unwrap_or_else(|| OsString::from(".")));
        normalized.extend_from_slice(args.get(2..).unwrap_or_default());
    }
    normalized
}

pub(super) fn print_usage() {
    println!("Volang UI preview (build tools require Node.js 24 or newer)");
    let templates: std::collections::BTreeMap<String, serde_json::Value> =
        serde_json::from_str(include_str!("../../../eng/ui-next/project-templates.json"))
            .expect("the built-in UI template catalog is valid JSON");
    println!(
        "usage: vo ui create <directory> [--template {}]",
        templates.keys().cloned().collect::<Vec<_>>().join("|")
    );
    println!("usage: vo ui <check|build|dev|preview|test> [directory]");
    println!("Projects with ui-next.json select the new tools; --project <directory> selects them explicitly.");
    println!("usage: vo ui <run|package> [directory] [--backend vm|jit|aot]");
    println!("Native desktop commands require the matching desktop SDK. Run defaults to JIT; package defaults to Native AOT.");
    println!("usage: vo ui doctor [directory] [--target web|desktop] [--json]");
    println!("usage: vo ui verify");
    println!("usage: vo ui browsers install [chromium|firefox|webkit ...]");
    println!("The vo ui web spelling remains available for the same commands.");
    println!("Use the matching packaged tools; VO_UI_TOOLCHAIN selects an explicit installation.");
}

fn entry_in(directory: &Path) -> Option<PathBuf> {
    let entry = directory.join("ui.mjs");
    (entry.is_file() && directory.join("tools/toolchain.json").is_file()).then_some(entry)
}

fn toolchain_entry(executable: &Path, selected: Option<&OsStr>) -> Result<PathBuf, String> {
    if let Some(directory) = selected {
        return entry_in(Path::new(directory)).ok_or_else(|| {
            "VO_UI_TOOLCHAIN must name a complete UI toolchain directory containing ui.mjs and tools/toolchain.json".to_string()
        });
    }
    if let Some(bin) = executable.parent() {
        // Standard toolchain archives and installations under a bin/ prefix.
        if let Some(entry) = entry_in(&bin.join("share/volang/ui-next")) {
            return Ok(entry);
        }
        if let Some(prefix) = bin.parent() {
            if let Some(entry) = entry_in(&prefix.join("share/volang/ui-next")) {
                return Ok(entry);
            }
            // The standalone preview package includes its compiler in bin/.
            if bin.file_name() == Some(OsStr::new("bin")) {
                if let Some(entry) = entry_in(prefix) {
                    return Ok(entry);
                }
            }
        }
    }
    Err("Web UI tools are not installed. Install the matching UI toolchain preview, or set VO_UI_TOOLCHAIN to its directory.".to_string())
}

pub(super) fn cmd_ui_web(args: &[OsString]) -> i32 {
    if args.is_empty()
        || args.len() == 1 && matches!(args[0].to_str(), Some("help" | "--help" | "-h"))
        || args.len() == 2
            && is_project_command(args)
            && matches!(args[1].to_str(), Some("--help" | "-h"))
    {
        print_usage();
        return 0;
    }
    let entry = env::current_exe()
        .map_err(|error| format!("cannot locate the Volang executable: {error}"))
        .and_then(|executable| {
            toolchain_entry(&executable, env::var_os("VO_UI_TOOLCHAIN").as_deref())
        });
    let entry = match entry {
        Ok(entry) => entry,
        Err(error) => {
            eprintln!("{error}");
            return 1;
        }
    };
    let mut command = Command::new(env::var_os("VO_UI_NODE").unwrap_or_else(|| "node".into()));
    command.arg(entry).args(project_arguments(args));
    // Replace the launcher on Unix so interrupts, status and ownership go
    // directly to the development server and its compilation processes.
    #[cfg(unix)]
    let error = {
        use std::os::unix::process::CommandExt;
        command.exec()
    };
    #[cfg(not(unix))]
    let error = match command.status() {
        Ok(status) => return status.code().unwrap_or(1),
        Err(error) => error,
    };
    eprintln!("cannot start Web UI tools: {error}; install Node.js 24 or newer, or set VO_UI_NODE to its executable");
    1
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn command_selection_preserves_existing_positional_projects() {
        let selected = |args: &[&str]| {
            is_project_command(&args.iter().map(OsString::from).collect::<Vec<_>>())
        };
        for name in ["create", "check", "preview", "verify", "browsers"] {
            assert!(selected(&[name]));
        }
        for name in ["build", "dev", "test", "run", "package", "doctor"] {
            assert!(selected(&[name, "--project", "A project 中文"]));
            assert!(selected(&[name, "--project"]));
            assert!(selected(&[name, "--help"]));
            assert!(!selected(&[name]));
            assert!(!selected(&[name, "apps/studio"]));
        }
        for args in [
            vec![],
            vec!["--help"],
            vec!["new", "app"],
            vec!["run", "app"],
            vec!["package", "app"],
            vec!["inspect", "app"],
            vec!["unknown", "--project", "app"],
        ] {
            assert!(!selected(&args));
        }
    }

    #[test]
    fn project_manifest_selects_default_and_positional_commands() {
        let stamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let temporary =
            env::temp_dir().join(format!("vo-ui-selection-{}-{stamp}", std::process::id()));
        fs::create_dir(&temporary).unwrap();
        let root = temporary.as_path();
        let project = root.join("A project 中文");
        fs::create_dir(&project).unwrap();
        // Selection must not parse or silently bypass a malformed new manifest.
        fs::write(project.join("ui-next.json"), "invalid JSON").unwrap();
        for name in ["build", "dev", "test", "run", "package", "doctor"] {
            assert!(project_command_in(&[name.into()], &project));
            assert!(project_command_in(
                &[name.into(), "A project 中文".into()],
                root
            ));
            assert!(!project_command_in(&[name.into()], root));
            assert!(!project_command_in(&[name.into(), "legacy".into()], root));
            assert!(project_command_in(
                &[name.into(), "A project 中文".into(), "--unknown".into()],
                root
            ));
        }
        fs::remove_dir_all(temporary).unwrap();
    }

    #[test]
    fn project_aliases_preserve_paths_and_forward_invalid_options() {
        let values = |values: &[&str]| values.iter().map(OsString::from).collect::<Vec<_>>();
        for name in [
            "check", "build", "dev", "preview", "test", "run", "package", "doctor",
        ] {
            assert_eq!(
                project_arguments(&values(&[name])),
                values(&[name, "--project", "."])
            );
            assert_eq!(
                project_arguments(&values(&[name, "A project 中文"])),
                values(&[name, "--project", "A project 中文"])
            );
            assert_eq!(
                project_arguments(&values(&[name, "app", "--unknown"])),
                values(&[name, "--project", "app", "--unknown"])
            );
            for args in [
                values(&[name, "--help"]),
                values(&[name, "--project", "-project"]),
                values(&[name, "--project"]),
            ] {
                assert_eq!(project_arguments(&args), args);
            }
        }
        assert_eq!(
            project_arguments(&values(&["run", "--backend", "vm"])),
            values(&["run", "--project", ".", "--backend", "vm"])
        );
        for args in [
            values(&[]),
            values(&["create", "app"]),
            values(&["browsers", "install"]),
        ] {
            assert_eq!(project_arguments(&args), args);
        }
    }

    #[test]
    fn installation_lookup_is_explicit_and_relative_to_the_executable() {
        let root = env::temp_dir().join(format!("vo-ui-web-paths-{}", std::process::id()));
        fs::create_dir_all(root.join("bin")).unwrap();
        let executable = root.join("bin/vo");
        assert!(toolchain_entry(&executable, None).is_err());
        fs::write(root.join("ui.mjs"), "preview").unwrap();
        assert!(toolchain_entry(&executable, None).is_err());
        fs::create_dir_all(root.join("tools")).unwrap();
        fs::write(root.join("tools/toolchain.json"), "{}").unwrap();
        assert_eq!(
            toolchain_entry(&executable, None).unwrap(),
            root.join("ui.mjs")
        );
        let installed = root.join("share/volang/ui-next");
        fs::create_dir_all(installed.join("tools")).unwrap();
        fs::write(installed.join("ui.mjs"), "installed").unwrap();
        fs::write(installed.join("tools/toolchain.json"), "{}").unwrap();
        assert_eq!(
            toolchain_entry(&executable, None).unwrap(),
            installed.join("ui.mjs")
        );
        assert_eq!(
            toolchain_entry(&executable, Some(root.as_os_str())).unwrap(),
            root.join("ui.mjs")
        );
        assert!(toolchain_entry(&executable, Some(root.join("missing").as_os_str())).is_err());
        fs::remove_dir_all(root).unwrap();
    }
}
