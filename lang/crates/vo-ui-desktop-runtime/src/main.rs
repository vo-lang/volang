#![cfg_attr(target_os = "windows", windows_subsystem = "windows")]

fn main() {
    match vo_ui_desktop_runtime::run() {
        Ok(code) => std::process::exit(code),
        Err(error) => {
            eprintln!("{error}");
            std::process::exit(1);
        }
    }
}
