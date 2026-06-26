//! Declaration-side effect manifest for stdlib-owned externs.
//!
//! These are bytecode upper bounds, not per-provider effects. Native, no_std,
//! and WASM providers may report narrower effects when they register.

use vo_runtime::bytecode::ExternEffects;
use vo_runtime::ffi::ExternEffectManifestEntry;

pub const WAIT_IO: ExternEffects = ExternEffects::MAY_WAIT_IO_REPLAY;
pub const TIME_SLEEP: ExternEffects =
    ExternEffects::MAY_WAIT_IO_REPLAY.union(ExternEffects::MAY_HOST_WAIT);
pub const HTTP_REQUEST: ExternEffects =
    ExternEffects::MAY_WAIT_IO_REPLAY.union(ExternEffects::MAY_HOST_REPLAY);

pub const EFFECT_MANIFEST: &[ExternEffectManifestEntry] = &[
    ExternEffectManifestEntry::new("os_blocking_fileRead", WAIT_IO),
    ExternEffectManifestEntry::new("os_blocking_fileWrite", WAIT_IO),
    ExternEffectManifestEntry::new("os_blocking_fileReadAt", WAIT_IO),
    ExternEffectManifestEntry::new("os_blocking_fileWriteAt", WAIT_IO),
    ExternEffectManifestEntry::new("net_blocking_tcpConnRead", WAIT_IO),
    ExternEffectManifestEntry::new("net_blocking_tcpConnWrite", WAIT_IO),
    ExternEffectManifestEntry::new("net_blocking_tcpListenerAccept", WAIT_IO),
    ExternEffectManifestEntry::new("net_blocking_udpConnReadFrom", WAIT_IO),
    ExternEffectManifestEntry::new("net_blocking_udpConnWriteTo", WAIT_IO),
    ExternEffectManifestEntry::new("net_blocking_unixConnRead", WAIT_IO),
    ExternEffectManifestEntry::new("net_blocking_unixConnWrite", WAIT_IO),
    ExternEffectManifestEntry::new("net_blocking_unixListenerAccept", WAIT_IO),
    ExternEffectManifestEntry::new("time_blocking_sleepNano", TIME_SLEEP),
    ExternEffectManifestEntry::new("net_http_nativeHttpsRequest", HTTP_REQUEST),
    ExternEffectManifestEntry::new("os_getOsErrors", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_getOsConsts", ExternEffects::NONE),
    ExternEffectManifestEntry::new("io_getIoErrors", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_fileRead", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_fileWrite", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_fileSeek", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_fileClose", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_fileSync", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_fileStat", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_fileTruncate", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_openFile", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeMkdir", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeMkdirAll", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeRemove", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeRemoveAll", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeRename", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeStat", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeLstat", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeReadDir", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeChmod", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeChown", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeSymlink", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeReadlink", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeLink", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeTruncate", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeReadFile", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeWriteFile", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeGetenv", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeSetenv", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeUnsetenv", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeEnviron", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeLookupEnv", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeClearenv", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeExpandEnv", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeGetwd", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeChdir", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeUserHomeDir", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeUserCacheDir", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeUserConfigDir", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeTempDir", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeGetpid", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeGetppid", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeGetuid", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeGeteuid", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeGetgid", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeGetegid", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeExit", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeGetArgs", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeIsTerminal", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeHostname", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeExecutable", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeCreateTemp", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeMkdirTemp", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativePipe", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeChtimes", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeFindProcess", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_nativeKillProcess", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_dial", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_listen", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_listenPacket", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_tcpConnClose", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_tcpConnLocalAddr", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_tcpConnRemoteAddr", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_tcpConnSetDeadline", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_tcpConnSetReadDeadline", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_tcpConnSetWriteDeadline", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_tcpListenerClose", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_tcpListenerAddr", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_udpConnClose", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_udpConnLocalAddr", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_udpConnSetDeadline", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_udpConnSetReadDeadline", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_udpConnSetWriteDeadline", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_unixDial", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_unixListen", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_unixConnSetDeadline", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_unixConnSetReadDeadline", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_unixConnSetWriteDeadline", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_unixConnClose", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_unixListenerClose", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_lookupHost", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_lookupIP", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_lookupAddr", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_resolveTCPAddr", ExternEffects::NONE),
    ExternEffectManifestEntry::new("net_resolveUDPAddr", ExternEffects::NONE),
    ExternEffectManifestEntry::new("time_nowUnixNano", ExternEffects::NONE),
    ExternEffectManifestEntry::new("time_nowMonoNano", ExternEffects::NONE),
    ExternEffectManifestEntry::new("time_localOffsetAt", ExternEffects::NONE),
    ExternEffectManifestEntry::new("time_localAbbrevAt", ExternEffects::NONE),
    ExternEffectManifestEntry::new("time_ianaOffsetAt", ExternEffects::NONE),
    ExternEffectManifestEntry::new("time_ianaAbbrevAt", ExternEffects::NONE),
    ExternEffectManifestEntry::new("time_loadLocation", ExternEffects::NONE),
    ExternEffectManifestEntry::new("path_filepath_evalSymlinks", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_exec_startProcess", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_exec_waitProcess", ExternEffects::NONE),
    ExternEffectManifestEntry::new("os_exec_runCaptureOutput", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_CompileFile", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_CompileDir", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_CompileString", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_Run", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_RunJit", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_RunCapture", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_RunJitCapture", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_RunFile", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_RunFileJit", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_Free", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_FreeAst", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_Name", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_FormatSource", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_FormatBytecode", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_ParseFile", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_ParseString", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_PrintAst", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_SaveBytecodeText", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_LoadBytecodeText", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_SaveBytecodeBinary", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_LoadBytecodeBinary", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_CompileCheck", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_InitProject", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_InitFile", ExternEffects::NONE),
    ExternEffectManifestEntry::new("toolchain_Get", ExternEffects::NONE),
    ExternEffectManifestEntry::new("regexp_matchString", ExternEffects::NONE),
    ExternEffectManifestEntry::new("regexp_matchBytes", ExternEffects::NONE),
    ExternEffectManifestEntry::new("regexp_findString", ExternEffects::NONE),
    ExternEffectManifestEntry::new("regexp_findStringIndex", ExternEffects::NONE),
    ExternEffectManifestEntry::new("regexp_findAllString", ExternEffects::NONE),
    ExternEffectManifestEntry::new("regexp_replaceAllString", ExternEffects::NONE),
    ExternEffectManifestEntry::new("regexp_replaceAllLiteralString", ExternEffects::NONE),
    ExternEffectManifestEntry::new("regexp_splitString", ExternEffects::NONE),
    ExternEffectManifestEntry::new("regexp_findStringSubmatch", ExternEffects::NONE),
    ExternEffectManifestEntry::new("regexp_quoteMeta", ExternEffects::NONE),
    ExternEffectManifestEntry::new("encoding_toml_marshalAny", ExternEffects::NONE),
    ExternEffectManifestEntry::new("encoding_toml_Unmarshal", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_rand_Intn", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_rand_Int63n", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_rand_Int", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_rand_Uint64", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_rand_Uint32", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_rand_Float64", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_rand_Float32", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_rand_Read", ExternEffects::NONE),
    ExternEffectManifestEntry::new("bytes_Index", ExternEffects::NONE),
    ExternEffectManifestEntry::new("bytes_LastIndex", ExternEffects::NONE),
    ExternEffectManifestEntry::new("bytes_Count", ExternEffects::NONE),
    ExternEffectManifestEntry::new("bytes_ToLower", ExternEffects::NONE),
    ExternEffectManifestEntry::new("bytes_ToUpper", ExternEffects::NONE),
    ExternEffectManifestEntry::new("bytes_ToTitle", ExternEffects::NONE),
    ExternEffectManifestEntry::new("bytes_EqualFold", ExternEffects::NONE),
    ExternEffectManifestEntry::new("bytes_Replace", ExternEffects::NONE),
    ExternEffectManifestEntry::new("fmt_nativeWrite", ExternEffects::NONE),
    ExternEffectManifestEntry::new("fmt_nativeSprint", ExternEffects::NONE),
    ExternEffectManifestEntry::new("fmt_nativeSprintln", ExternEffects::NONE),
    ExternEffectManifestEntry::new("fmt_nativeSprintf", ExternEffects::NONE),
    ExternEffectManifestEntry::new("fmt_nativeSscan", ExternEffects::NONE),
    ExternEffectManifestEntry::new("fmt_nativeSscanf", ExternEffects::NONE),
    ExternEffectManifestEntry::new("fmt_nativeReadLine", ExternEffects::NONE),
    ExternEffectManifestEntry::new("strings_Index", ExternEffects::NONE),
    ExternEffectManifestEntry::new("strings_LastIndex", ExternEffects::NONE),
    ExternEffectManifestEntry::new("strings_Count", ExternEffects::NONE),
    ExternEffectManifestEntry::new("strings_ToLower", ExternEffects::NONE),
    ExternEffectManifestEntry::new("strings_ToUpper", ExternEffects::NONE),
    ExternEffectManifestEntry::new("strings_ToTitle", ExternEffects::NONE),
    ExternEffectManifestEntry::new("strings_Split", ExternEffects::NONE),
    ExternEffectManifestEntry::new("strings_SplitN", ExternEffects::NONE),
    ExternEffectManifestEntry::new("strings_SplitAfter", ExternEffects::NONE),
    ExternEffectManifestEntry::new("strings_SplitAfterN", ExternEffects::NONE),
    ExternEffectManifestEntry::new("strings_Fields", ExternEffects::NONE),
    ExternEffectManifestEntry::new("strings_Replace", ExternEffects::NONE),
    ExternEffectManifestEntry::new("strings_EqualFold", ExternEffects::NONE),
    ExternEffectManifestEntry::new("strconv_ParseFloat", ExternEffects::NONE),
    ExternEffectManifestEntry::new("strconv_FormatFloat", ExternEffects::NONE),
    ExternEffectManifestEntry::new("unicode_IsLetter", ExternEffects::NONE),
    ExternEffectManifestEntry::new("unicode_IsDigit", ExternEffects::NONE),
    ExternEffectManifestEntry::new("unicode_IsSpace", ExternEffects::NONE),
    ExternEffectManifestEntry::new("unicode_IsUpper", ExternEffects::NONE),
    ExternEffectManifestEntry::new("unicode_IsLower", ExternEffects::NONE),
    ExternEffectManifestEntry::new("unicode_IsControl", ExternEffects::NONE),
    ExternEffectManifestEntry::new("unicode_IsPrint", ExternEffects::NONE),
    ExternEffectManifestEntry::new("unicode_IsPunct", ExternEffects::NONE),
    ExternEffectManifestEntry::new("unicode_IsGraphic", ExternEffects::NONE),
    ExternEffectManifestEntry::new("unicode_IsNumber", ExternEffects::NONE),
    ExternEffectManifestEntry::new("unicode_IsMark", ExternEffects::NONE),
    ExternEffectManifestEntry::new("unicode_IsSymbol", ExternEffects::NONE),
    ExternEffectManifestEntry::new("unicode_ToLower", ExternEffects::NONE),
    ExternEffectManifestEntry::new("unicode_ToUpper", ExternEffects::NONE),
    ExternEffectManifestEntry::new("unicode_ToTitle", ExternEffects::NONE),
    ExternEffectManifestEntry::new("unicode_SimpleFold", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Floor", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Ceil", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Round", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Trunc", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Sqrt", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Cbrt", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Pow", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Hypot", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Exp", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Exp2", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Expm1", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Log", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Log2", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Log10", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Log1p", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Sin", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Cos", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Tan", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Asin", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Acos", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Atan", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Atan2", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Sinh", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Cosh", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Tanh", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Asinh", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Acosh", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Atanh", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Mod", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Modf", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Frexp", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Ldexp", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_FMA", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Inf", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_NaN", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Float64bits", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Float64frombits", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Float32bits", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_Float32frombits", ExternEffects::NONE),
    ExternEffectManifestEntry::new("encoding_json_marshalAny", ExternEffects::NONE),
    ExternEffectManifestEntry::new("encoding_json_Unmarshal", ExternEffects::NONE),
    ExternEffectManifestEntry::new("encoding_json_writeJsonString", ExternEffects::NONE),
    ExternEffectManifestEntry::new("encoding_json_parseJsonString", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_nativeUintSize", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_LeadingZeros", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_LeadingZeros8", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_LeadingZeros16", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_LeadingZeros32", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_LeadingZeros64", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_TrailingZeros", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_TrailingZeros8", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_TrailingZeros16", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_TrailingZeros32", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_TrailingZeros64", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_OnesCount", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_OnesCount8", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_OnesCount16", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_OnesCount32", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_OnesCount64", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_Add", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_Add32", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_Add64", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_Sub", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_Sub32", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_Sub64", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_Mul", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_Mul32", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_Mul64", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_Div", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_Div32", ExternEffects::NONE),
    ExternEffectManifestEntry::new("math_bits_Div64", ExternEffects::NONE),
];

pub fn known_extern_allowed_effects(name: &str) -> Option<ExternEffects> {
    EFFECT_MANIFEST
        .iter()
        .find(|entry| entry.name == name)
        .map(|entry| entry.effects)
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, BTreeSet};
    use std::fs;
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
            crate::math::__VO_STDLIB_REGISTERED_math,
            crate::bits::__VO_STDLIB_REGISTERED_math_bits,
            crate::rand::__VO_STDLIB_REGISTERED_math_rand,
            crate::bytes::__VO_STDLIB_REGISTERED_bytes,
            crate::strings::__VO_STDLIB_REGISTERED_strings,
            crate::strconv::__VO_STDLIB_REGISTERED_strconv,
            crate::unicode::__VO_STDLIB_REGISTERED_unicode,
            crate::json::__VO_STDLIB_REGISTERED_encoding_json,
            crate::toml_pkg::__VO_STDLIB_REGISTERED_encoding_toml,
            crate::regexp::__VO_STDLIB_REGISTERED_regexp,
            crate::os::__VO_STDLIB_REGISTERED_os,
            crate::filepath::__VO_STDLIB_REGISTERED_path_filepath,
            crate::exec::__VO_STDLIB_REGISTERED_os_exec,
            crate::fmt::REGISTERED_EXTERNS,
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

        let mut declarations = Vec::new();
        for path in files {
            let rel_dir = path
                .parent()
                .expect("stdlib file should have parent")
                .strip_prefix(&root)
                .expect("stdlib file should be under root");
            let package_prefix = rel_dir
                .components()
                .map(|component| component.as_os_str().to_string_lossy())
                .collect::<Vec<_>>()
                .join("_");
            let source = fs::read_to_string(&path)
                .unwrap_or_else(|err| panic!("could not read {}: {err}", path.display()));
            for line in source.lines() {
                let line = line.trim();
                if line.contains('{') {
                    continue;
                }
                let Some(rest) = line.strip_prefix("func ") else {
                    continue;
                };
                let name = rest
                    .split(|ch: char| !(ch.is_ascii_alphanumeric() || ch == '_'))
                    .next()
                    .expect("func declaration should include a name");
                if !name.is_empty() {
                    declarations.push(format!("{package_prefix}_{name}"));
                }
            }
        }
        declarations.sort();
        declarations.dedup();
        declarations
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
            known_extern_allowed_effects("fmt_nativeSprintf"),
            Some(ExternEffects::NONE)
        );
        assert_eq!(
            known_extern_allowed_effects("os_blocking_fileRead"),
            Some(ExternEffects::MAY_WAIT_IO_REPLAY)
        );
        assert_eq!(
            known_extern_allowed_effects("time_blocking_sleepNano"),
            Some(TIME_SLEEP)
        );
        assert_eq!(
            known_extern_allowed_effects("net_http_nativeHttpsRequest"),
            Some(HTTP_REQUEST)
        );
        assert_eq!(known_extern_allowed_effects("extension_doThing"), None);
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
            ("math", crate::math::__VO_STDLIB_REGISTERED_math),
            ("math_bits", crate::bits::__VO_STDLIB_REGISTERED_math_bits),
            ("math_rand", crate::rand::__VO_STDLIB_REGISTERED_math_rand),
            ("bytes", crate::bytes::__VO_STDLIB_REGISTERED_bytes),
            ("strings", crate::strings::__VO_STDLIB_REGISTERED_strings),
            ("strconv", crate::strconv::__VO_STDLIB_REGISTERED_strconv),
            ("unicode", crate::unicode::__VO_STDLIB_REGISTERED_unicode),
            (
                "encoding_json",
                crate::json::__VO_STDLIB_REGISTERED_encoding_json,
            ),
            (
                "encoding_toml",
                crate::toml_pkg::__VO_STDLIB_REGISTERED_encoding_toml,
            ),
            ("regexp", crate::regexp::__VO_STDLIB_REGISTERED_regexp),
            ("os", crate::os::__VO_STDLIB_REGISTERED_os),
            (
                "path_filepath",
                crate::filepath::__VO_STDLIB_REGISTERED_path_filepath,
            ),
            ("os_exec", crate::exec::__VO_STDLIB_REGISTERED_os_exec),
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
            ("fmt", crate::fmt::REGISTERED_EXTERNS),
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
