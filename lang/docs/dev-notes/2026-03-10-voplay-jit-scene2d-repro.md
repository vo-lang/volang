# Voplay JIT Scene2D Crash Reproduction Note

## Date
2026-03-10

## Summary
This note records how to reproduce and inspect the JIT-only `voplay` / `scene2d` crash that was observed while running the `voplay` integration test suite.

At the time of writing:

- The failure was previously observed as a **JIT-only segmentation fault**.
- The crash appeared after `camera2d: ok` and before `scene queries: ok` in `tests/voplay/main.vo`.
- The most suspicious test region was `testSceneQueries()`.
- The latest full run of:

```bash
./d.py test jit --release
```

currently passes, so the issue is either intermittent, already masked by subsequent changes, or depends on build/runtime timing.

This document is meant to preserve the exact commands and debugging procedure so the issue can be re-opened quickly if it returns.

---

## Primary Symptom

Historical failure mode:

- `./d.py test jit ...` reported a JIT failure without a useful raw crash site.
- Running the raw `vo` executable directly showed **exit code 139 / segmentation fault**.
- The last visible test marker was:

```text
  camera2d: ok
```

and the process crashed before:

```text
  scene queries: ok
```

The hot region was:

- `tests/voplay/main.vo`
- `testSceneQueries()`
- especially the closure call:

```vo
count := 0
s.ForEach(func(e *scene2d.Entity) {
    count++
})
```

---

## Fast Reproduction Commands

### 1. Explicit `voplay` JIT test via `d.py`

Use the test runner with an explicit target first:

```bash
./d.py test jit --release /Users/macm1/code/github/volang/tests/voplay/main.vo
```

Important:

- `./d.py test jit --release` by itself primarily runs the default `lang/test_data` JIT suite.
- It does **not** mean the `volang/tests/voplay/main.vo` integration file was exercised.

If the explicit `voplay` target passes, the bug is not currently reproducing in this test file.

### 2. Direct raw `vo` execution for `voplay`

Use the raw executable to avoid `d.py` masking the crash site:

```bash
VO_JIT_CALL_THRESHOLD=1 ./target/release/vo run /Users/macm1/code/github/volang/tests/voplay/main.vo --mode=jit
```

Expected historical behavior when the bug reproduces:

- process exits with `139`
- segmentation fault occurs during `testSceneQueries()`

### 3. Direct raw `vo` execution with IR dump

To debug JIT codegen, dump Cranelift IR:

```bash
VO_JIT_CALL_THRESHOLD=1 VO_JIT_DEBUG=1 ./target/release/vo run /Users/macm1/code/github/volang/tests/voplay/main.vo --mode=jit
```

This is the preferred command when the issue is active.

Per local debugging rules, when debugging JIT, always inspect the generated Cranelift IR.

---

## Narrowing the Crash Site

The relevant test function is:

- `volang/tests/voplay/main.vo`
- `func testSceneQueries()`

The historically suspicious operations inside that test were:

1. `s.FindByTag(...)`
2. `s.FindInRect(...)`
3. `s.ForEach(func(e *scene2d.Entity) { count++ })`
4. `s.Entities()`
5. `s.VisibleEntities()`
6. `s.Destroy(...)`

The closure-based `ForEach` call was the strongest suspect because it combines:

- closure creation
- closure capture (`count`)
- closure invocation through `CallClosure`
- mutation of captured state (`count++`)

A practical way to narrow the site again is to temporarily insert short markers around these operations, for example:

```vo
fmt.Println("M1")
fmt.Println("M2")
```

and then rerun the raw JIT command above.

The last printed marker identifies the failing operation.

---

## Relevant Files

### Test entry

- `volang/tests/voplay/main.vo`

### Scene2D implementation

- `voplay/scene2d/scene.vo`

### JIT closure call path

- `volang/lang/crates/vo-jit/src/call_helpers.rs`
- `volang/lang/crates/vo-jit/src/translate.rs`
- `volang/lang/crates/vo-vm/src/vm/jit/callbacks/closure_call.rs`
- `volang/lang/crates/vo-vm/src/exec/call.rs`
- `volang/lang/crates/vo-runtime/src/objects/closure.rs`

These are the files to inspect first if the crash returns.

---

## What Was Already Changed During Investigation

The following investigation-driven changes happened around this bug:

1. `voplay/scene2d/scene.vo`
   - `FindInRadius` and `FindInRect` were rewritten from closure-based predicates to specialized loops.

2. `volang/lang/crates/vo-vm/src/vm/jit/frame.rs`
   - A temporary hypothesis was explored around JIT frame initialization.
   - That specific zeroing change was later reverted locally by hand after validation moved on.

3. `volang/lang/crates/vo-jit/src/call_helpers.rs`
   - A temporary attempt to force closure calls down the slow path was explored and then reverted.

Important: the reproduction steps in this note are valid regardless of whether those exploratory changes remain in tree.

---

## Current Status

Current observable state on the latest local tree:

```bash
./d.py test jit --release
```

passes.

That means one of the following is true:

1. the crash was fixed indirectly by another change,
2. the failure is timing-sensitive / intermittent,
3. the failure depends on a narrower command path than the current suite run,
4. the failure depends on uncommitted local edits that are no longer present.

Because of that, the correct workflow is:

1. run the broad suite,
2. if it passes, rerun the raw `vo run ... --mode=jit` command,
3. if it still passes, add temporary markers inside `testSceneQueries()`,
4. if it still does not reproduce, compare against the revision where the crash was last seen.

---

## Minimal Command Checklist

Use this exact order:

```bash
./d.py test jit --release
```

```bash
VO_JIT_CALL_THRESHOLD=1 ./target/release/vo run /Users/macm1/code/github/volang/tests/voplay/main.vo --mode=jit
```

```bash
VO_JIT_CALL_THRESHOLD=1 VO_JIT_DEBUG=1 ./target/release/vo run /Users/macm1/code/github/volang/tests/voplay/main.vo --mode=jit
```

If the crash comes back, save:

- the last printed test marker
- the full raw terminal output
- the last emitted Cranelift IR block(s)
- the exact local diff in:
  - `tests/voplay/main.vo`
  - `voplay/scene2d/scene.vo`
  - `lang/crates/vo-jit/src/*`
  - `lang/crates/vo-vm/src/vm/jit/*`

---

## Why This Note Exists

The crash stopped reproducing in the latest `jit --release` run, but the earlier failure was severe enough that the exact procedure should stay documented. If the regression returns, this note should make it possible to resume investigation immediately instead of reconstructing the debugging workflow from scratch.
