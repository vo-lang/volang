//! Distribution composition. The window and session crates remain independent
//! of application layout, bytecode loading and the process-entry ABI.
mod bundle;
pub use bundle::{Bundle, Manifest};
#[cfg(feature = "runner")]
mod runner;
#[cfg(feature = "runner")]
pub use runner::run;
#[cfg(all(feature = "aot", not(test)))]
mod aot;

use std::path::{Path, PathBuf};

pub struct Arguments {
    pub resources: PathBuf,
    pub check: bool,
    pub diagnostics: bool,
    pub exit_on_failure: bool,
}

impl Arguments {
    pub fn parse(
        executable: &Path,
        args: impl IntoIterator<Item = std::ffi::OsString>,
    ) -> Result<Self, String> {
        let mut result = Self {
            resources: resources_for(executable)?,
            check: false,
            diagnostics: false,
            exit_on_failure: false,
        };
        let mut args = args.into_iter();
        let mut explicit = false;
        while let Some(arg) = args.next() {
            match arg.to_str() {
                Some("--check") if !result.check => result.check = true,
                Some("--diagnostics") if !result.diagnostics => result.diagnostics = true,
                Some("--exit-on-failure") if !result.exit_on_failure => result.exit_on_failure = true,
                Some("--bundle") if !explicit => {
                    result.resources = args
                        .next()
                        .ok_or("--bundle requires a resource directory")?
                        .into();
                    explicit = true;
                }
                _ => {
                    return Err(
                        "usage: application [--check] [--diagnostics] [--exit-on-failure] [--bundle <resources>]"
                            .into(),
                    )
                }
            }
        }
        Ok(result)
    }
}

fn resources_for(executable: &Path) -> Result<PathBuf, String> {
    let parent = executable
        .parent()
        .ok_or("cannot locate application directory")?;
    if cfg!(target_os = "macos") && parent.file_name().is_some_and(|name| name == "MacOS") {
        Ok(parent
            .parent()
            .ok_or("invalid application layout")?
            .join("Resources"))
    } else {
        Ok(parent.join("resources"))
    }
}

#[cfg(feature = "window")]
pub fn finish(
    completion: vo_ui_native::executor::Completion,
    diagnostics: bool,
) -> Result<i32, String> {
    if diagnostics {
        eprintln!("desktop execution: {:?}", completion.stats);
    }
    match completion.result.map_err(|error| error.to_string())? {
        vo_ui_native::Exit::Completed | vo_ui_native::Exit::Stopped => Ok(0),
        vo_ui_native::Exit::Code(code) => Ok(code),
        other => Err(format!("desktop execution ended: {other:?}")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn launcher_paths_and_flags_do_not_depend_on_the_callers_directory() {
        let executable = std::env::temp_dir().join("Moved app 中文/application");
        let args =
            Arguments::parse(&executable, ["--check", "--diagnostics"].map(Into::into)).unwrap();
        assert_eq!(
            args.resources,
            executable.parent().unwrap().join("resources")
        );
        assert!(args.check && args.diagnostics);
        let args =
            Arguments::parse(&executable, ["--bundle", "A bundle 中文"].map(Into::into)).unwrap();
        assert_eq!(args.resources, Path::new("A bundle 中文"));
        for args in [
            vec!["--bundle"],
            vec!["--check", "--check"],
            vec!["--unknown"],
        ] {
            assert!(Arguments::parse(&executable, args.into_iter().map(Into::into)).is_err());
        }
    }
}
