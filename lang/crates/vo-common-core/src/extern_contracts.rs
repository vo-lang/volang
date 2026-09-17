//! Declaration-side extern contracts, independent of providers and compiler machinery.
//! Tables expand into provider bindings or metadata without duplicating identities/effects.

use crate::bytecode::ExternEffects;
use crate::extern_key::ExternKeyRef;

pub const WAIT_IO: ExternEffects = ExternEffects::MAY_WAIT_IO_REPLAY;
pub const TIME_SLEEP: ExternEffects = WAIT_IO.union(ExternEffects::MAY_HOST_WAIT);
pub const HTTP_REQUEST: ExternEffects = WAIT_IO.union(ExternEffects::MAY_HOST_REPLAY);

#[derive(Clone, Copy)]
enum Identity {
    Canonical(ExternKeyRef<'static>),
    Internal(&'static str),
}

struct Contract {
    identity: Identity,
    effects: ExternEffects,
}

#[macro_export]
macro_rules! vo_stdlib_extern_contracts {
    ($emit:ident) => {
        $emit! {
            (canonical("runtime/mem", "ReadStats"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("runtime/mem", "GCStep"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("runtime/mem", "GCCollect"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "blocking_fileRead"), stdlib, $crate::extern_contracts::WAIT_IO),
            (canonical("os", "blocking_fileWrite"), stdlib, $crate::extern_contracts::WAIT_IO),
            (canonical("os", "blocking_fileReadAt"), stdlib, $crate::extern_contracts::WAIT_IO),
            (canonical("os", "blocking_fileWriteAt"), stdlib, $crate::extern_contracts::WAIT_IO),
            (canonical("net", "blocking_tcpConnRead"), stdlib, $crate::extern_contracts::WAIT_IO),
            (canonical("net", "blocking_tcpConnWrite"), stdlib, $crate::extern_contracts::WAIT_IO),
            (canonical("net", "blocking_tcpListenerAccept"), stdlib, $crate::extern_contracts::WAIT_IO),
            (canonical("net", "blocking_udpConnReadFrom"), stdlib, $crate::extern_contracts::WAIT_IO),
            (canonical("net", "blocking_udpConnWriteTo"), stdlib, $crate::extern_contracts::WAIT_IO),
            (canonical("net", "blocking_unixConnRead"), stdlib, $crate::extern_contracts::WAIT_IO),
            (canonical("net", "blocking_unixConnWrite"), stdlib, $crate::extern_contracts::WAIT_IO),
            (canonical("net", "blocking_unixListenerAccept"), stdlib, $crate::extern_contracts::WAIT_IO),
            (canonical("net", "getNetErrors"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("time", "blocking_sleepNano"), stdlib, $crate::extern_contracts::TIME_SLEEP),
            (canonical("net/http", "getHttpErrors"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net/http", "nativeNewClientRequest"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net/http", "nativeCancelClientRequest"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net/http", "nativeReleaseClientRequest"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net/http", "nativeHttpsRequest"), stdlib, $crate::extern_contracts::HTTP_REQUEST),
            (canonical("os", "getOsErrors"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "getOsConsts"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "getPathSeparators"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("io", "getIoErrors"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("errors", "assignTo"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("errors", "identity"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("errors", "equal"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "fileRead"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "fileWrite"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "fileSeek"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "fileClose"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "fileSync"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "fileStat"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "fileTruncate"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "openFile"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeMkdir"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeMkdirAll"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeRemove"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeRemoveAll"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeRename"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeStat"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeLstat"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeReadDir"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeChmod"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeChown"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeSymlink"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeReadlink"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeLink"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeTruncate"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeReadFile"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeWriteFile"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeGetenv"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeSetenv"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeUnsetenv"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeEnviron"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeLookupEnv"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeClearenv"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeExpandEnv"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeGetwd"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeChdir"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeUserHomeDir"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeUserCacheDir"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeUserConfigDir"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeTempDir"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeGetpid"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeGetppid"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeGetuid"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeGeteuid"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeGetgid"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeGetegid"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeExit"), stdlib, $crate::bytecode::ExternEffects::MAY_EXIT),
            (canonical("os", "nativeGetArgs"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeIsTerminal"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeHostname"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeExecutable"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeCreateTemp"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeMkdirTemp"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativePipe"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeChtimes"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeFindProcess"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os", "nativeKillProcess"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "dial"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "listen"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "listenPacket"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "tcpConnClose"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "tcpConnLocalAddr"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "tcpConnRemoteAddr"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "tcpConnSetDeadline"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "tcpConnSetReadDeadline"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "tcpConnSetWriteDeadline"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "tcpListenerClose"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "tcpListenerAddr"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "udpConnClose"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "udpConnLocalAddr"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "udpConnSetDeadline"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "udpConnSetReadDeadline"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "udpConnSetWriteDeadline"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "unixDial"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "unixListen"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "unixConnSetDeadline"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "unixConnSetReadDeadline"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "unixConnSetWriteDeadline"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "unixConnClose"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "unixListenerClose"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "lookupHost"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "lookupIP"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "lookupAddr"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "resolveTCPAddr"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("net", "resolveUDPAddr"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("time", "nowUnixNano"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("time", "nowMonoNano"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("time", "localOffsetAt"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("time", "localAbbrevAt"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("time", "ianaOffsetAt"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("time", "ianaAbbrevAt"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("time", "loadLocation"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("path/filepath", "evalSymlinks"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("path/filepath", "absPath"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os/exec", "startProcess"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os/exec", "isExecutable"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os/exec", "killProcess"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("os/exec", "waitProcess"), stdlib, $crate::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY),
            (canonical("toolchain", "CompileFile"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "CompileDir"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "CompileString"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "Run"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "RunJit"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "RunCapture"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "RunJitCapture"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "RunFile"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "RunFileJit"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "Free"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "FreeAst"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "Name"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "FormatSource"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "FormatBytecode"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "ParseFile"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "ParseString"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "PrintAst"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "SaveBytecodeText"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "SaveBytecodeBinary"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "LoadBytecodeBinary"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "CompileCheck"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "InitProject"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "InitFile"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("toolchain", "Get"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("regexp", "matchString"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("regexp", "matchBytes"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("regexp", "findString"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("regexp", "findStringIndex"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("regexp", "findAllString"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("regexp", "replaceAllString"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("regexp", "replaceAllLiteralString"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("regexp", "splitString"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("regexp", "findStringSubmatch"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("regexp", "findAllStringIndexFlat"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("regexp", "findAllStringSubmatchFlat"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("regexp", "findAllStringSubmatchIndexFlat"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("regexp", "subexpNames"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("regexp", "findBytesSubmatchIndex"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("regexp", "findAllBytesIndexFlat"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("regexp", "replaceAllBytes"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("regexp", "replaceAllLiteralBytes"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("regexp", "quoteMeta"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("encoding/toml", "marshalAny"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("encoding/toml", "unmarshalAny"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/rand", "Intn"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/rand", "Int63n"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/rand", "Int"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/rand", "Uint64"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/rand", "Uint32"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/rand", "Float64"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/rand", "Float32"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/rand", "Read"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("bytes", "Index"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("bytes", "LastIndex"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("bytes", "Count"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("bytes", "ToLower"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("bytes", "ToUpper"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("bytes", "ToTitle"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("bytes", "EqualFold"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("bytes", "Replace"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("fmt", "nativeWrite"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("fmt", "nativeSprint"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("fmt", "nativeSprintln"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("fmt", "nativeSprintf"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("fmt", "nativeSscan"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("fmt", "nativeSscanf"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("fmt", "nativeReadLine"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("strings", "Index"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("strings", "LastIndex"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("strings", "Count"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("strings", "ToLower"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("strings", "ToUpper"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("strings", "ToTitle"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("strings", "Split"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("strings", "SplitN"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("strings", "SplitAfter"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("strings", "SplitAfterN"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("strings", "Fields"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("strings", "Replace"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("strings", "EqualFold"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("strconv", "parseFloat"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("strconv", "formatFloat"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("unicode", "IsLetter"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("unicode", "IsDigit"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("unicode", "IsSpace"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("unicode", "IsUpper"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("unicode", "IsLower"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("unicode", "IsTitle"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("unicode", "IsControl"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("unicode", "IsPrint"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("unicode", "IsPunct"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("unicode", "IsGraphic"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("unicode", "IsNumber"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("unicode", "IsMark"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("unicode", "IsSymbol"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("unicode", "ToLower"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("unicode", "ToUpper"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("unicode", "ToTitle"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("unicode", "SimpleFold"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Floor"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Ceil"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Round"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Trunc"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Sqrt"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Cbrt"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Pow"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Hypot"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Exp"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Exp2"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Expm1"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Log"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Log2"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Log10"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Log1p"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Sin"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Cos"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Tan"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Asin"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Acos"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Atan"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Atan2"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Sinh"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Cosh"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Tanh"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Asinh"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Acosh"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Atanh"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Mod"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Modf"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Frexp"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Ldexp"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "FMA"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Inf"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "NaN"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Float64bits"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Float64frombits"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Float32bits"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Float32frombits"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("encoding/json", "marshalAny"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("encoding/json", "unmarshalAny"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("encoding/json", "writeJsonString"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("encoding/json", "parseJsonString"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "nativeUintSize"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "LeadingZeros"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "LeadingZeros8"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "LeadingZeros16"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "LeadingZeros32"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "LeadingZeros64"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "TrailingZeros"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "TrailingZeros8"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "TrailingZeros16"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "TrailingZeros32"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "TrailingZeros64"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "OnesCount"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "OnesCount8"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "OnesCount16"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "OnesCount32"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "OnesCount64"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "Add"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "Add32"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "Add64"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "Sub"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "Sub32"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "Sub64"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "Mul"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "Mul32"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "Mul64"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "Div"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "Div32"), stdlib, $crate::bytecode::ExternEffects::NONE),
            (canonical("math/bits", "Div64"), stdlib, $crate::bytecode::ExternEffects::NONE),
        }
    };
}

#[macro_export]
macro_rules! vo_builtin_extern_contracts {
    ($emit:ident) => {
        $emit! {
            (internal("vo_print"), builtin_print, $crate::bytecode::ExternEffects::NONE),
            (internal("vo_println"), builtin_println, $crate::bytecode::ExternEffects::NONE),
            (internal("vo_assert"), builtin_assert, $crate::bytecode::ExternEffects::NONE),
            (internal("vo_copy"), builtin_copy, $crate::bytecode::ExternEffects::NONE),
            (internal("vo_copy_string"), builtin_copy, $crate::bytecode::ExternEffects::NONE),
            (internal("vo_slice_append_slice"), builtin_slice_append_slice, $crate::bytecode::ExternEffects::NONE),
            (internal("vo_slice_append_string"), builtin_slice_append_slice, $crate::bytecode::ExternEffects::NONE),
            (internal("vo_iface_eq"), builtin_iface_eq, $crate::bytecode::ExternEffects::NONE),
            (internal("vo_conv_int_str"), conv_int_str, $crate::bytecode::ExternEffects::NONE),
            (internal("vo_conv_bytes_str"), conv_bytes_str, $crate::bytecode::ExternEffects::NONE),
            (internal("vo_conv_str_bytes"), conv_str_bytes, $crate::bytecode::ExternEffects::NONE),
            (internal("vo_conv_runes_str"), conv_runes_str, $crate::bytecode::ExternEffects::NONE),
            (internal("vo_conv_str_runes"), conv_str_runes, $crate::bytecode::ExternEffects::NONE),
            (internal("panic_with_error"), panic_with_error, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Sqrt"), math_sqrt, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Floor"), math_floor, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Ceil"), math_ceil, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "Trunc"), math_trunc, $crate::bytecode::ExternEffects::NONE),
            (canonical("math", "FMA"), math_fma, $crate::bytecode::ExternEffects::NONE),
        }
    };
}

#[macro_export]
macro_rules! vo_dynamic_extern_contracts {
    ($emit:ident) => {
        $emit! {
            (canonical("dyn", "getDynErrors"), get_dyn_errors, $crate::bytecode::ExternEffects::NONE),
            (internal("dyn_field"), dyn_field, $crate::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY),
            (internal("dyn_index"), dyn_index, $crate::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY),
            (internal("dyn_set_field"), dyn_set_field, $crate::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY),
            (internal("dyn_set_index_unified"), dyn_set_index_unified, $crate::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY),
            (internal("dyn_call"), dyn_call, $crate::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY),
            (internal("dyn_method"), dyn_method, $crate::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY),
            (internal("dyn_pack_any_slice"), dyn_pack_any_slice, $crate::bytecode::ExternEffects::NONE),
            (internal("dyn_type_assert_error"), dyn_type_assert_error, $crate::bytecode::ExternEffects::NONE),
            (canonical("dyn", "GetAttr"), dyn_get_attr, $crate::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY),
            (canonical("dyn", "GetIndex"), dyn_get_index, $crate::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY),
            (canonical("dyn", "SetAttr"), dyn_set_attr, $crate::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY),
            (canonical("dyn", "SetIndex"), dyn_set_index, $crate::bytecode::ExternEffects::MAY_CALL_CLOSURE_REPLAY),
        }
    };
}

macro_rules! identity {
    (canonical($package:literal, $function:literal)) => {
        Identity::Canonical(ExternKeyRef::new($package, $function))
    };
    (internal($name:literal)) => {
        Identity::Internal($name)
    };
}
macro_rules! contracts {
    ($(($kind:ident($($name:literal),+), $function:ident, $effects:expr)),* $(,)?) => {
        &[$(Contract { identity: identity!($kind($($name),+)), effects: $effects }),*]
    };
}
const STDLIB: &[Contract] = crate::vo_stdlib_extern_contracts!(contracts);
const BUILTIN: &[Contract] = crate::vo_builtin_extern_contracts!(contracts);
const DYNAMIC: &[Contract] = crate::vo_dynamic_extern_contracts!(contracts);

fn lookup(table: &[Contract], name: &str) -> Option<ExternEffects> {
    let decoded = crate::extern_key::decode_extern_name(name).ok();
    table
        .iter()
        .find(|contract| match contract.identity {
            Identity::Canonical(key) => decoded == Some(key),
            Identity::Internal(key) => key == name,
        })
        .map(|contract| contract.effects)
}

pub fn known_stdlib_extern_allowed_effects(name: &str) -> Option<ExternEffects> {
    lookup(STDLIB, name)
}

pub fn known_runtime_extern_allowed_effects(name: &str) -> Option<ExternEffects> {
    if crate::extern_key::decode_extern_name(name)
        .ok()
        .is_some_and(|key| {
            matches!(
                (key.package(), key.function()),
                ("runtime", "Caller") | ("runtime/mem", "ReadStats" | "GCStep" | "GCCollect")
            )
        })
    {
        return Some(ExternEffects::NONE);
    }
    lookup(BUILTIN, name).or_else(|| lookup(DYNAMIC, name))
}

pub fn known_extern_allowed_effects(name: &str) -> Option<ExternEffects> {
    known_runtime_extern_allowed_effects(name).or_else(|| known_stdlib_extern_allowed_effects(name))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn internal_helper_contracts_cover_the_bytecode_whitelist() {
        for name in crate::extern_key::VM_INTERNAL_EXTERN_NAMES {
            assert!(
                known_runtime_extern_allowed_effects(name).is_some(),
                "{name}"
            );
        }
        for contract in BUILTIN.iter().chain(DYNAMIC) {
            if let Identity::Internal(name) = contract.identity {
                assert!(crate::extern_key::VM_INTERNAL_EXTERN_NAMES.contains(&name));
            }
        }
    }

    #[test]
    fn canonical_contracts_round_trip_and_overlapping_providers_agree() {
        let mut declarations = std::collections::BTreeMap::new();
        for contract in STDLIB.iter().chain(BUILTIN).chain(DYNAMIC) {
            let Identity::Canonical(key) = contract.identity else {
                continue;
            };
            let name = key.encode().unwrap();
            assert_eq!(crate::extern_key::decode_extern_name(&name).unwrap(), key);
            if let Some(previous) = declarations.insert(name.clone(), contract.effects) {
                assert_eq!(previous, contract.effects, "{name}");
            }
            assert_eq!(known_extern_allowed_effects(&name), Some(contract.effects));
        }
        assert_eq!(known_extern_allowed_effects("unknown"), None);
    }
}
