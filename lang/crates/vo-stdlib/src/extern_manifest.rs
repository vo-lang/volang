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
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "blocking_fileRead"),
        WAIT_IO,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "blocking_fileWrite"),
        WAIT_IO,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "blocking_fileReadAt"),
        WAIT_IO,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "blocking_fileWriteAt"),
        WAIT_IO,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "blocking_tcpConnRead"),
        WAIT_IO,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "blocking_tcpConnWrite"),
        WAIT_IO,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "blocking_tcpListenerAccept"),
        WAIT_IO,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "blocking_udpConnReadFrom"),
        WAIT_IO,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "blocking_udpConnWriteTo"),
        WAIT_IO,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "blocking_unixConnRead"),
        WAIT_IO,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "blocking_unixConnWrite"),
        WAIT_IO,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "blocking_unixListenerAccept"),
        WAIT_IO,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "getNetErrors"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("time", "blocking_sleepNano"),
        TIME_SLEEP,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net/http", "getHttpErrors"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net/http", "nativeNewClientRequest"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net/http", "nativeCancelClientRequest"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net/http", "nativeReleaseClientRequest"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net/http", "nativeHttpsRequest"),
        HTTP_REQUEST,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "getOsErrors"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "getOsConsts"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "getPathSeparators"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("io", "getIoErrors"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("errors", "assignTo"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("errors", "identity"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("errors", "equal"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "fileRead"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "fileWrite"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "fileSeek"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "fileClose"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "fileSync"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "fileStat"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "fileTruncate"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "openFile"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeMkdir"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeMkdirAll"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeRemove"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeRemoveAll"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeRename"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeStat"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeLstat"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeReadDir"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeChmod"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeChown"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeSymlink"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeReadlink"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeLink"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeTruncate"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeReadFile"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeWriteFile"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeGetenv"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeSetenv"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeUnsetenv"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeEnviron"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeLookupEnv"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeClearenv"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeExpandEnv"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeGetwd"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeChdir"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeUserHomeDir"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeUserCacheDir"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeUserConfigDir"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeTempDir"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeGetpid"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeGetppid"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeGetuid"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeGeteuid"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeGetgid"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeGetegid"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeExit"),
        ExternEffects::MAY_EXIT,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeGetArgs"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeIsTerminal"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeHostname"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeExecutable"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeCreateTemp"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeMkdirTemp"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativePipe"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeChtimes"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeFindProcess"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os", "nativeKillProcess"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "dial"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "listen"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "listenPacket"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "tcpConnClose"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "tcpConnLocalAddr"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "tcpConnRemoteAddr"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "tcpConnSetDeadline"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "tcpConnSetReadDeadline"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "tcpConnSetWriteDeadline"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "tcpListenerClose"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "tcpListenerAddr"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "udpConnClose"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "udpConnLocalAddr"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "udpConnSetDeadline"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "udpConnSetReadDeadline"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "udpConnSetWriteDeadline"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "unixDial"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "unixListen"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "unixConnSetDeadline"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "unixConnSetReadDeadline"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "unixConnSetWriteDeadline"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "unixConnClose"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "unixListenerClose"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "lookupHost"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "lookupIP"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "lookupAddr"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "resolveTCPAddr"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("net", "resolveUDPAddr"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("time", "nowUnixNano"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("time", "nowMonoNano"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("time", "localOffsetAt"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("time", "localAbbrevAt"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("time", "ianaOffsetAt"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("time", "ianaAbbrevAt"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("time", "loadLocation"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("path/filepath", "evalSymlinks"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("path/filepath", "absPath"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os/exec", "startProcess"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os/exec", "isExecutable"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os/exec", "killProcess"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("os/exec", "waitProcess"),
        ExternEffects::MAY_WAIT_IO_REPLAY,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "CompileFile"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "CompileDir"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "CompileString"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "Run"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "RunJit"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "RunCapture"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "RunJitCapture"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "RunFile"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "RunFileJit"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "Free"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "FreeAst"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "Name"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "FormatSource"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "FormatBytecode"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "ParseFile"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "ParseString"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "PrintAst"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "SaveBytecodeText"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "SaveBytecodeBinary"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "LoadBytecodeBinary"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "CompileCheck"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "InitProject"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "InitFile"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("toolchain", "Get"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("regexp", "matchString"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("regexp", "matchBytes"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("regexp", "findString"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("regexp", "findStringIndex"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("regexp", "findAllString"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("regexp", "replaceAllString"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("regexp", "replaceAllLiteralString"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("regexp", "splitString"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("regexp", "findStringSubmatch"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("regexp", "findAllStringIndexFlat"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("regexp", "findAllStringSubmatchFlat"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("regexp", "findAllStringSubmatchIndexFlat"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("regexp", "subexpNames"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("regexp", "findBytesSubmatchIndex"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("regexp", "findAllBytesIndexFlat"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("regexp", "replaceAllBytes"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("regexp", "replaceAllLiteralBytes"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("regexp", "quoteMeta"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("encoding/toml", "marshalAny"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("encoding/toml", "unmarshalAny"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/rand", "Intn"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/rand", "Int63n"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/rand", "Int"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/rand", "Uint64"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/rand", "Uint32"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/rand", "Float64"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/rand", "Float32"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/rand", "Read"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("bytes", "Index"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("bytes", "LastIndex"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("bytes", "Count"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("bytes", "ToLower"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("bytes", "ToUpper"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("bytes", "ToTitle"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("bytes", "EqualFold"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("bytes", "Replace"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("fmt", "nativeWrite"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("fmt", "nativeSprint"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("fmt", "nativeSprintln"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("fmt", "nativeSprintf"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("fmt", "nativeSscan"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("fmt", "nativeSscanf"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("fmt", "nativeReadLine"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("strings", "Index"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("strings", "LastIndex"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("strings", "Count"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("strings", "ToLower"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("strings", "ToUpper"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("strings", "ToTitle"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("strings", "Split"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("strings", "SplitN"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("strings", "SplitAfter"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("strings", "SplitAfterN"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("strings", "Fields"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("strings", "Replace"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("strings", "EqualFold"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("strconv", "parseFloat"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("strconv", "formatFloat"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("unicode", "IsLetter"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("unicode", "IsDigit"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("unicode", "IsSpace"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("unicode", "IsUpper"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("unicode", "IsLower"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("unicode", "IsTitle"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("unicode", "IsControl"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("unicode", "IsPrint"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("unicode", "IsPunct"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("unicode", "IsGraphic"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("unicode", "IsNumber"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("unicode", "IsMark"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("unicode", "IsSymbol"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("unicode", "ToLower"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("unicode", "ToUpper"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("unicode", "ToTitle"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("unicode", "SimpleFold"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Floor"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Ceil"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Round"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Trunc"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Sqrt"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Cbrt"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Pow"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Hypot"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Exp"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Exp2"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Expm1"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Log"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Log2"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Log10"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Log1p"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Sin"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Cos"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Tan"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Asin"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Acos"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Atan"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Atan2"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Sinh"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Cosh"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Tanh"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Asinh"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Acosh"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Atanh"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Mod"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Modf"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Frexp"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Ldexp"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "FMA"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Inf"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "NaN"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Float64bits"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Float64frombits"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Float32bits"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math", "Float32frombits"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("encoding/json", "marshalAny"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("encoding/json", "unmarshalAny"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("encoding/json", "writeJsonString"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("encoding/json", "parseJsonString"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "nativeUintSize"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "LeadingZeros"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "LeadingZeros8"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "LeadingZeros16"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "LeadingZeros32"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "LeadingZeros64"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "TrailingZeros"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "TrailingZeros8"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "TrailingZeros16"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "TrailingZeros32"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "TrailingZeros64"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "OnesCount"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "OnesCount8"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "OnesCount16"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "OnesCount32"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "OnesCount64"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "Add"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "Add32"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "Add64"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "Sub"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "Sub32"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "Sub64"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "Mul"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "Mul32"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "Mul64"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "Div"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "Div32"),
        ExternEffects::NONE,
    ),
    ExternEffectManifestEntry::new(
        vo_runtime::vo_extern_name!("math/bits", "Div64"),
        ExternEffects::NONE,
    ),
];

pub fn known_extern_allowed_effects(name: &str) -> Option<ExternEffects> {
    EFFECT_MANIFEST
        .iter()
        .find(|entry| entry.name == name)
        .map(|entry| entry.effects)
}

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
            ("fmt", crate::fmt::__VO_STDLIB_ENTRIES),
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
