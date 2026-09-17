# Native execution for the replacement UI

`Session` joins a verified, loaded native VM to the existing
`github.com/vo-lang/ui/next/host.Exchange` boundary. The same session accepts an
interpreter, an explicitly configured JIT, or a VM loaded from a linked Native
AOT image. It owns no compiler, component state, renderer or window.

The platform executor registers `vo-ui-bridge` before the module's extern table
freezes, installs any interruption/readiness hooks and transfers the unstarted
VM into the session. Each `poll` runs a nonzero, bounded number of cooperative
scheduler turns. `Yielded` requests another turn; `Waiting` awaits a reply or
native runtime readiness. The platform's native I/O integration must arrange
polling when completions become ready. The existing VM runtime waker covers
process-local Island readiness; it does not notify every native I/O completion.
`Session` itself adds no polling timer or worker thread. The optional execution
owner described below supplies the native scheduling policy.

Each output is delivered once with an opaque `ExchangeId`. The executor carries
that exact capability into `respond`, then polls again. IDs retain a unique
session owner and the complete scheduler wait key. Replies from another window,
old requests, duplicates and oversized responses preserve the current pending
exchange. Pending renderer input leaves other runnable goroutines and native
I/O completions able to progress. A second UI writer is rejected, including
writers separated by scheduler yields.

The existing wire codec and renderer remain responsible for bootstrap, atomic
mutation validation, commit acknowledgement, input sequencing and orderly
close. The host must cancel renderer-owned work when the session terminates.
`stop` immediately drops native execution and rejects late replies; it cannot
execute guest cleanup. Normal protocol closure runs the guest's cleanup before
the session finishes. Terminal sessions never restart their entry function.
Fatal guest panic/trap text and the original VM source location remain owned by
the returned error after the VM is disposed. File/line presentation still belongs
to the caller's source-map/diagnostic integration.

## Native owner thread

`executor::Executor` constructs, drives and disposes a VM on one dedicated thread.
The factory returns a verified, loaded, unstarted VM; neither the VM nor its
managed references cross threads. Platform notifications carry no renderer or
VM ownership. The output mailbox holds at most one exchange plus one terminal
result, and the reply mailbox holds one exact, checked capability and its bytes.
Stale, duplicate, foreign and oversized replies are rejected before enqueueing.

Runnable work continues in cooperative turns. Pure UI idle waits on a condition
variable without a timer. Runtime Island wakeups are coalesced; native I/O
waiters enable a configurable 4 ms fallback until the language runtime exposes
complete I/O notifications. The executor keeps advancing native background I/O
while the renderer awaits input. Its stop flag also reaches VM safe points.

Dropping the executor requests interruption and releases the platform handle
without blocking the window thread. Explicit `join` waits for cooperative
teardown. A foreign native call that blocks without observing interruption can
outlive that handle; hosts must not wait for it synchronously on their GUI thread.
The worker retains only its own data, and completion is sent after VM disposal.
Ordinary application closure still follows the existing guest cleanup protocol.

Features `jit` and `aot` select native VM capabilities. Neither is enabled by
default. Production dependencies exclude the previous UI kernel. The existing
native stdlib includes the module resolver and its analysis dependency; this
adapter does not alter that shared language-runtime policy.

## Executable contract probes

From the repository root, with the prescribed toolchain and Web host artifacts:

```sh
VOWORK=off cargo test -p vo-ui-native --features jit --locked
VO_TEST_PROFILE=release node eng/ui-next/native-session-build.mjs
UI_NEXT_NATIVE_AOT=target/ui-next/native-session/interaction-aot node eng/ui-next/native-session-contracts.mjs
```

The build script serializes Cargo, preserves the root dependency versions and
checks an isolated Native AOT static-runtime dependency graph. It compiles the
actual interaction example and links it through the real CLI, shared AOT image
loader and this session. The process fixtures under `examples/` use bounded
length-prefixed pipes to a real DOM host; their asynchronous UI tasks are owned
by the browser. They are contract probes, not desktop launchers.

The browser probe covers initial effects, continuous clicks, Unicode input and
immediate submission, retained keyed identity, disposal/remount, local render
failure/retry and orderly close. Native entry counters must be positive for
JIT/AOT; AOT must execute static continuations with zero JIT compilations.
The build also executes fatal Unicode panic fixtures under VM/JIT and a real
linked Native AOT runtime, checking diagnostics and source-location retention.
Results go to `target/ui-next/native-session`. Browser/OS versions and executable
identities belong with any archived evidence. These checks do not establish a
real desktop window, IME/accessibility acceptance,
desktop packaging or product certification.

`vo-ui-webview` consumes the owner-thread executor for the replacement's system
WebView window; its real-window acceptance is recorded separately.
