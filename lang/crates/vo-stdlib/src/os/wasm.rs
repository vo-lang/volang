//! WASM os implementations - most operations are not supported.

use vo_runtime::bytecode::ExternDef;
use vo_runtime::ffi::{ExternCall, ExternRegistry, ExternResult};

fn not_supported(_call: &mut ExternCall) -> ExternResult {
    ExternResult::Panic("os operations are not supported on wasm".into())
}

pub fn register_externs(registry: &mut ExternRegistry, externs: &[ExternDef]) {
    for (id, def) in externs.iter().enumerate() {
        match def.name.as_str() {
            // File operations
            "os_fileRead" | "os_fileWrite" | "os_fileReadAt" | "os_fileWriteAt" |
            "os_fileSeek" | "os_fileClose" | "os_fileSync" | "os_fileStat" | "os_fileTruncate" |
            "os_openFile" |
            // Directory operations
            "os_nativeMkdir" | "os_nativeMkdirAll" | "os_nativeRemove" | "os_nativeRemoveAll" |
            "os_nativeRename" | "os_nativeStat" | "os_nativeLstat" | "os_nativeReadDir" |
            "os_nativeChmod" | "os_nativeChown" | "os_nativeSymlink" | "os_nativeReadlink" |
            "os_nativeLink" | "os_nativeTruncate" | "os_nativeReadFile" | "os_nativeWriteFile" |
            // Environment operations
            "os_nativeGetenv" | "os_nativeSetenv" | "os_nativeUnsetenv" | "os_nativeEnviron" |
            "os_nativeLookupEnv" | "os_nativeClearenv" | "os_nativeExpandEnv" |
            // System operations
            "os_nativeGetwd" | "os_nativeChdir" | "os_nativeUserHomeDir" | "os_nativeUserCacheDir" |
            "os_nativeUserConfigDir" | "os_nativeTempDir" |
            "os_nativeGetpid" | "os_nativeGetppid" | "os_nativeGetuid" | "os_nativeGeteuid" |
            "os_nativeGetgid" | "os_nativeGetegid" | "os_nativeExit" | "os_nativeGetArgs" |
            "os_nativeHostname" | "os_nativeExecutable" | "os_nativeCreateTemp" | "os_nativeMkdirTemp" => {
                registry.register(id as u32, not_supported);
            }
            _ => {}
        }
    }
}
