//! Process entry for statically linked native AOT command-line applications.

#[cfg(not(test))]
use std::ffi::c_char;

#[cfg(not(test))]
use vo_vm::vm::SchedulingOutcome;

/// Optional compiler-host capability required by AOT images importing toolchain.
/// Called by the generated main before loading or executing the program.
#[cfg(all(not(test), feature = "toolchain-host"))]
#[unsafe(no_mangle)]
pub extern "C" fn vo_aot_initialize_toolchain_host_v1() -> i32 {
    match std::panic::catch_unwind(vo_engine::ensure_toolchain_host_installed) {
        Ok(()) => 0,
        Err(_) => 101,
    }
}

#[cfg(not(test))]
unsafe fn run_embedded(argc: i32, argv: *const *const c_char) -> Result<i32, String> {
    let mut vm =
        unsafe { vo_aot_runtime_core::load_embedded_vm(argc, argv, |_vm, _module| Ok(())) }?;
    let outcome = vm
        .run()
        .map_err(|error| format!("AOT execution failed: {error:?}"))?;
    if std::env::var_os("VO_AOT_STATS").is_some() {
        eprintln!("Vo AOT execution stats: {:?}", vm.jit_execution_stats());
    }
    match outcome {
        SchedulingOutcome::Completed | SchedulingOutcome::Exited(0) => Ok(0),
        SchedulingOutcome::Exited(code) => Ok(code),
        SchedulingOutcome::Blocked => Err("AOT program deadlocked".to_string()),
        SchedulingOutcome::Suspended => {
            Err("AOT program suspended with pending island work".to_string())
        }
        SchedulingOutcome::SuspendedForHostEvents => {
            Err("AOT program suspended for unavailable host events".to_string())
        }
        SchedulingOutcome::Panicked => Err("AOT program panicked".to_string()),
    }
}

/// C process entry called by the generated object-file `main` trampoline.
///
/// # Safety
///
/// `argv` must point to an array of at least `argc` valid C-string pointers,
/// following the process-entry ABI of the linked target.
#[cfg(not(test))]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn vo_aot_start(argc: i32, argv: *const *const c_char) -> i32 {
    match std::panic::catch_unwind(|| unsafe { run_embedded(argc, argv) }) {
        Ok(Ok(code)) => code,
        Ok(Err(error)) => {
            eprintln!("vo AOT runtime error: {error}");
            1
        }
        Err(_) => 101,
    }
}
