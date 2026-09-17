# Browser compiler diagnostics

Compiler-enabled builds expose an optional `CompileResult.diagnosticsJson`
getter. It accompanies `errorMessage` on parser and type-checker failures from
`compile`, `compileProject`, `analyzeProject`, and their auto-install variants.
Successful compilation and analysis also retain warnings from that same checked
project. This does not run a second analysis or construct an editor index.
The existing success, bytecode and error getters retain their behavior. Read the
result fields before calling `free()`.

The JSON document has this versioned structure:

```json
{
  "version": 1,
  "positionEncoding": "utf-16",
  "items": [{
    "severity": "error",
    "code": 2200,
    "message": "undeclared name: missing",
    "location": {
      "file": "main.vo",
      "start": {"line": 1, "character": 14},
      "end": {"line": 1, "character": 21},
      "startByte": 27,
      "endByte": 34
    }
  }]
}
```

The example describes `package main\nfunc main() { missing() }\n`.
Lines and characters are zero-based; character columns count UTF-16 code units.
Byte offsets count UTF-8 bytes from the beginning of that file. Every end is
exclusive. A non-BMP character such as an emoji occupies two character units.
`code` may be null. `location` may be null when no usable source span exists.
The primary label determines the location, falling back to the first label if
there is no primary label. Error items precede warning/note/help items; order
within each group is retained. The original diagnostic sink remains unchanged.

File identity comes from the compiler source map, with path separators normalized
to `/`. Single-source compilation uses the supplied filename. Project snapshots
retain project paths for root files. Dependency locations include the resolver's
filesystem root together with their package-relative filename; this distinguishes
two packages containing the same filename. Paths may be virtual or absolute;
callers must resolve these identities explicitly instead of matching basenames.
An overlay path is relative to the project root. Snapshotting does not persist
the unsaved overlay into the browser filesystem.

Locations belong to the exact submitted source snapshot. Associate a result with
that source/version; discard or disable its locations after an edit or project
switch. Compilation failure does not produce executable bytecode. Successful
results without diagnostics and failures without parser/type-checker diagnostics have an undefined
getter; use `errorMessage` for module, installation, policy and codegen failures.
Warnings keep `success` true and `errorMessage` undefined. Compilation still
provides bytecode; analysis-only results omit it. The existing raw-byte/unit Rust
entry points keep their return types. The separate [editor snapshot API](editor.md)
also provides completion and definition queries. Reject unknown schema versions rather
than guessing their coordinate units.

Execution-only builds omit this getter. The experimental Web runtime and compiler
remain separate build products; adding diagnostic metadata does not require
ordinary applications to load the compiler.
