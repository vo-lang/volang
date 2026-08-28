# Errors and cleanup

Volang keeps errors as values and adds `?`, `fail`, and `errdefer` for concise,
predictable propagation and rollback.

## Returning errors

Functions that can fail place the predeclared `error` type last:

```vo
func parsePort(text string) (int, error) {
    value, err := strconv.Atoi(text)
    if err != nil {
        return 0, err
    }
    if value < 1 || value > 65535 {
        return 0, errors.New("port out of range")
    }
    return value, nil
}
```

Callers can inspect, wrap, compare, or propagate the value with the standard
`errors` and `fmt` packages.

## The question operator

Apply `?` to an expression whose final result is `error`:

```vo
func endpoint(text string) (string, error) {
    port := parsePort(text)?
    return fmt.Sprintf("127.0.0.1:%d", port), nil
}
```

On success, `?` removes the final nil error from the expression results. On
failure, it commits the error result and begins normal return unwinding. The
enclosing function must itself have a final result identical to `error`.

An `error`-only expression may also use `?` as a statement:

```vo
func save() error {
    validate()?
    write()?
    return nil
}
```

## Explicit failure

`fail expression` evaluates the error once and begins an error return:

```vo
func requireName(name string) error {
    if name == "" {
        fail errors.New("name is required")
    }
    return nil
}
```

Use `fail` when a branch constructs or selects the error locally. Use `?` when
the error already comes from a call.

## Defer and rollback

Ordinary `defer` runs on success, error, and panic. `errdefer` runs only while
the function is unwinding an error or panic path:

```vo
func publish(source string) error {
    staging := createStagingDirectory()?
    errdefer os.RemoveAll(staging)

    output := os.Create(filepath.Join(staging, "release.bin"))?
    defer output.Close()

    compile(source, output)?
    activate(staging)?
    return nil
}
```

Deferred function values, receivers, and arguments are captured when the
statement executes. Eligible deferred calls share one last-in-first-out order
based on registration time.

## Named results

On propagation, current named non-error results are preserved; unnamed
non-error results receive zero values. Deferred calls can observe and update
named results. Error-path eligibility is decided before deferred calls run, so
a later defer changing a named error does not retroactively add or remove
`errdefer` calls.

## Panic and recover

Use error returns for expected operational failure. `panic` represents a
broken invariant or an unrecoverable local condition. Deferred calls run while
the panic unwinds; a deferred call may use `recover` according to the language
specification.

Libraries should avoid converting routine validation, I/O, network, or user
input failures into panics. Hosts should bound panic reporting and preserve a
useful Volang stack trace.

## Stable comparison

Use `errors.Is` for sentinel-compatible comparison and `errors.As` for typed
inspection. Wrap context with `fmt.Errorf` only when the formatting contract
preserves the original error relationship.

Dynamic-access failures expose stable sentinels such as type mismatch and
missing-member errors. Code should compare their identity instead of matching
human-readable text.
