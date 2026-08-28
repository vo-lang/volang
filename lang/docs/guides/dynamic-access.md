# Dynamic access

Dynamic access lets typed Volang code traverse or invoke values whose concrete
shape is known only at runtime. The `~>` syntax is backed by a bounded protocol
with stable errors; it does not grant unrestricted reflection.

## Read a member

Without propagation, an operation returns `(any, error)`:

```vo
value, err := payload~>name
if err != nil {
    return err
}
println(value)
```

In typed assignment context, `?` also checks the result type:

```vo
func readName(payload any) (string, error) {
    var name string
    name = payload~>name?
    return name, nil
}
```

The operation can fail because the member is absent or unsupported. The
assignment can then fail because the returned `any` value does not match the
expected type. Both failures follow ordinary error propagation and cleanup.

## Traverse nested data

Member and index operations compose left to right:

```vo
func firstEmail(payload any) (string, error) {
    var email string
    email = payload~>users~>[0]~>email?
    return email, nil
}
```

Every segment is checked. An out-of-range index, missing key, nil receiver, or
type mismatch returns an error.

## Map keys and indexes

Use bracket form when the selector is a runtime value:

```vo
first, err := values~>[0]
entry, err := object~>[key]
```

The dynamic protocol decides which key and index kinds it accepts. Built-in
maps, slices, arrays, strings, and standard dynamic map objects follow the
normative conversions in `lang/docs/spec/dynamic.md`.

## Calls and writes

Dynamic receivers may expose method calls and writable attributes:

```vo
result, err := service~>Lookup("ada")
err = settings~>theme = "dark"
```

The concrete syntax and supported operand shapes are specified and covered by
language tests. An implementation can reject a write or call even when a read
with the same name succeeds.

## Implementing the protocol

The `dyn` standard package defines protocol-facing types and stable error
categories. Application types can expose controlled lookup, assignment, index,
and call behavior through those interfaces. Implementations should:

- validate operation kind and argument count before reading values;
- keep work and allocation bounded;
- return stable errors for unsupported operations and bad types;
- avoid leaking host authority through an untrusted dynamic value;
- preserve deterministic behavior across VM, JIT, and AOT.

## When to use it

Dynamic access is useful for decoded JSON/TOML, plugin-style records,
configuration trees, and host data with a versioned open shape. Prefer structs
and interfaces when the schema is known: they provide compile-time checking,
clearer documentation, and cheaper dispatch.

## Error handling

Compare errors using `errors.Is` and the exported dynamic sentinel values.
Human-readable messages may include the member name, operation, or expected
type and can evolve without changing the stable category.
