# Vo Error System Redesign

Status: historical design proposal. This document is not a current
implementation reference; verify `lang/stdlib/errors`, `vo-runtime` builtins,
and `vo-stdlib` before treating any API or Rust shim below as shipped.

## Overview

This document describes the redesign of Vo's error system to match Go's standard library semantics and remove the non-standard Code mechanism.

## Design Goals

1. **Remove Code mechanism** - Error codes are a global namespace that third-party libraries cannot safely use
2. **Match Go's error interface** - Only `Error() string` required, `Unwrap()` is optional
3. **Fix `errors.Is()` semantics** - Match by value equality, not by code
4. **Add `errors.As()`** - Type-based error matching
5. **Add `errors.Unwrap()`** - Standard unwrap function

---

## Final API

### error interface (builtin)

```go
type error interface {
    Error() string
}
```

**Note**: `Unwrap()` is optional, checked via dynamic call (`~>Unwrap()`).

### errors package types

```go
// Error is the standard error implementation
type Error struct {
    msg   string
    cause error
}

func (e *Error) Error() string { return e.msg }
func (e *Error) Unwrap() error { return e.cause }
```

### errors package functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `New` | `func New(msg string) error` | Create error |
| `Newf` | `func Newf(format string, args ...any) error` | Formatted creation |
| `Wrap` | `func Wrap(cause error, msg string) error` | Wrap with message |
| `Wrapf` | `func Wrapf(cause error, format string, args ...any) error` | Formatted wrap |
| `Unwrap` | `func Unwrap(err error) error` | Get cause (nil if none) |
| `Is` | `func Is(err, target error) bool` | Value equality + unwrap chain |
| `As` | `func As(err error, target any) bool` | Type match + unwrap chain |

### Removed API

- ~~`Error.code` field~~
- ~~`Error.Code()` method~~
- ~~`NewCode(code int, msg string)`~~
- ~~`WrapCode(cause error, code int, msg string)`~~
- ~~`IsCode(err error, code int)`~~
- ~~`code.vo` entire file~~

---

## Core Function Implementations

### errors.Unwrap()

```go
// Unwrap returns the result of calling the Unwrap method on err,
// if err's type contains an Unwrap method returning error.
// Otherwise, Unwrap returns nil.
func Unwrap(err error) error {
    if err == nil {
        return nil
    }
    cause, e := err~>Unwrap()
    if e != nil {
        return nil  // No Unwrap method
    }
    return cause
}
```

### errors.Is() - Value Equality Match

```go
// Is reports whether any error in err's unwrap chain matches target.
// Match by:
// 1. Pointer equality (err == target)
// 2. Custom Is(error) bool method if present
func Is(err error, target error) bool {
    if target == nil {
        return err == nil
    }
    for err != nil {
        if err == target {
            return true
        }
        // Check custom Is method
        matched, e := err~>Is(target)
        if e == nil && matched {
            return true
        }
        err = Unwrap(err)
    }
    return false
}
```

### errors.As() - Type Match

```go
// As finds the first error in err's unwrap chain that matches target type,
// and if so, sets target to that error value and returns true.
// target must be a pointer to an interface or pointer type.
func As(err error, target any) bool
```

Implementation requires native extern for runtime type checking and assignment.

### errors.New() / errors.Wrap()

```go
func New(msg string) error {
    return &Error{msg: msg}
}

func Wrap(cause error, msg string) error {
    if cause == nil {
        return nil
    }
    return &Error{msg: msg, cause: cause}
}
```

---

## Native Extern Implementation

### asError() - For errors.As()

**Location**: proposed; no current `vo-runtime/src/stdlib/errors.rs` file exists.

```rust
use vo_ext::prelude::*;

/// Check if err matches target type and assign if so.
/// target is a pointer to interface or pointer type.
/// 
/// Vo signature: extern func asError(err error, targetRttid int, targetPtr any) bool
// Historical pseudocode. A current implementation would use #[vostd_fn] for
// stdlib registration or #[vo_fn] for an extension crate.
fn as_error(ctx: &mut ExternCallContext) -> ExternResult {
    let err = ctx.arg_any(slots::ARG_ERR);
    let target_rttid = ctx.arg_i64(slots::ARG_TARGET_RTTID) as u32;
    let target_ptr = ctx.arg_ref(slots::ARG_TARGET_PTR);
    
    // Traverse unwrap chain
    let mut current = err;
    loop {
        // Check if current's rttid matches target_rttid
        if current.rttid() == target_rttid {
            // Assign to target_ptr
            ctx.write_interface_to_ptr(target_ptr, current);
            ctx.ret_bool(slots::RET_0, true);
            return ExternResult::Ok;
        }
        
        // Unwrap via dynamic call
        let cause = call_unwrap(ctx, current);
        if cause.is_nil() {
            break;
        }
        current = cause;
    }
    
    ctx.ret_bool(slots::RET_0, false);
    ExternResult::Ok
}
```

---

## Builtin error interface Modification

### Location

`vo-analysis/src/universe.rs` - `create_error_type()` function

### Changes

```rust
fn create_error_type(...) -> TypeKey {
    // Create: type error interface {
    //   Error() string
    // }
    
    // === Error() string ===
    // ... unchanged ...
    
    // REMOVED: Unwrap() error (now optional, checked via dynamic call)
    // REMOVED: Code() int
    // REMOVED: Data() any
    
    let iface = InterfaceDetail::new_complete(
        vec![err_method],  // Only 1 method
        vec![],
    );
    // ...
}
```

---

## stdlib Migration

### io package

**Before**:
```go
var EOF = errors.NewCode(errors.CodeEOF, "EOF")
```

**After**:
```go
var EOF = errors.New("EOF")
```

### All packages to migrate

| Package | Error Variables |
|---------|-----------------|
| `io` | `EOF`, `ErrUnexpectedEOF`, `ErrShortWrite`, etc. |
| `strconv` | `ErrSyntax`, `ErrRange` |
| `encoding/json` | All error variables |
| `encoding/hex` | All error variables |
| `encoding/base64` | All error variables |
| `encoding/toml` | All error variables |
| `regexp` | `ErrInvalidPattern` |
| `bytes` | All error variables |
| `os` | All error variables |
| `dyn` | Dynamic access errors |

---

## Runtime Modifications

### write_error_to()

**Location**: `vo-runtime/src/stdlib/error_helper.rs`

**Changes**:
- Remove `code` parameter

```rust
pub fn write_error_to(
    call: &mut ExternCallContext, 
    ret_slot: u16, 
    msg: &str,
    // REMOVED: code: isize,
) {
    // Create errors.Error object
    // Fields: msg, cause(nil)
    // ...
}
```

---

## Deletion Checklist

### Vo Files

| File | Action |
|------|--------|
| `stdlib/errors/code.vo` | **DELETE entire file** |
| `stdlib/errors/errors.vo` | Rewrite, remove Code-related |
| `stdlib/errors/runtime.vo` | **DELETE entire file** |

### Rust Code

| Location | Change |
|----------|--------|
| `vo-analysis/src/universe.rs` | error interface remove Code/Data |
| `vo-runtime/src/stdlib/error_helper.rs` | Remove code parameter |
| `vo-runtime/src/stdlib/*.rs` | All `write_error_to` calls remove code |
| `libs/vox/rust/src/ffi.rs` | Remove `CODE_IO` |

---

## Usage Examples

### Create and Match Sentinel Error

```go
// io package definition
var EOF = errors.New("EOF")

// Usage
func Read() ([]byte, error) {
    if done {
        return nil, io.EOF
    }
    // ...
}

// Match
data, err := Read()
if errors.Is(err, io.EOF) {
    // Normal end
}
```

### Custom Error Type

```go
type ParseError struct {
    Line int
    Msg  string
}

func (e *ParseError) Error() string {
    return fmt.Sprintf("line %d: %s", e.Line, e.Msg)
}

func (e *ParseError) Unwrap() error {
    return nil
}

// Match
var parseErr *ParseError
if errors.As(err, &parseErr) {
    fmt.Println("error at line", parseErr.Line)
}
```

---

## Implementation Order

| Step | Task | Breaking |
|------|------|----------|
| 1 | Modify `vo-analysis/src/universe.rs`: simplify error interface (remove Unwrap/Code/Data) | **HIGH** |
| 2 | Delete `stdlib/errors/code.vo` | **HIGH** |
| 3 | Rewrite `stdlib/errors/errors.vo`: remove Code, add Unwrap/Is/As | **HIGH** |
| 4 | Modify all error creation code in `vo-runtime` (remove code param) | Medium |
| 5 | Migrate stdlib sentinel errors | Medium |
| 6 | Implement `errors.As()` native extern | Low |
| 7 | Update test cases | Low |
| 8 | Remove `CODE_IO` from vox | Low |

---

## Rationale

### Why Remove Code?

1. **Global namespace conflict**: Third-party libraries cannot safely allocate codes
2. **Go ecosystem doesn't use codes**: All major Go error libraries use sentinel errors + type matching
3. **Semantic mismatch**: `errors.Is()` matching by code is wrong - two different errors with same code would match

### Why Simplify error interface?

1. **Go compatibility**: Go's error interface only has `Error()`, `Unwrap()` is optional
2. **Less coupling**: Not all errors need Code/Data
3. **Cleaner design**: Specialized errors can add their own methods
