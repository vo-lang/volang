# Type Attributes Specification

Type attributes (`#[...]`) provide compile-time metadata for type declarations. They trigger compiler behavior such as code generation, layout control, and documentation.

## Syntax

```ebnf
attribute_list = attribute+
attribute      = "#[" attr_list "]" newline
attr_list      = attr ("," attr)*
attr           = name ("(" args ")")?
args           = arg ("," arg)*
arg            = name | name "=" value
value          = string_lit | number_lit | bool_lit
```

## Placement

Attributes are placed **before** the `type` keyword:

```go
#[json]
type User struct {
    Name string `json:"name"`
    Age  int    `json:"age"`
}
```

Multiple attributes can be combined or stacked:

```go
// Combined on one line
#[json, derive(Eq, Hash)]
type Key struct { ... }

// Or stacked
#[json(marshal)]
#[deprecated(since = "2.0")]
type Response struct { ... }
```

## Attribute Categories

### 1. Serialization Attributes

Generate serialization methods at compile time.

| Attribute | Generated Methods |
|-----------|------------------|
| `#[json]` | `MarshalJSON() ([]byte, error)` + `UnmarshalJSON([]byte) error` |
| `#[json(marshal)]` | `MarshalJSON() ([]byte, error)` only |
| `#[json(unmarshal)]` | `UnmarshalJSON([]byte) error` only |

**Options:**

| Option | Meaning |
|--------|---------|
| `marshal` | Generate only MarshalJSON |
| `unmarshal` | Generate only UnmarshalJSON |
| `strict` | Error on unknown fields during unmarshal |
| `snake_case` | Auto-convert field names to snake_case |

**Examples:**

```go
#[json]
type User struct {
    Name string `json:"name"`
}

#[json(unmarshal, strict)]
type Config struct {
    Port int `json:"port"`
}

#[json(snake_case)]
type Response struct {
    UserName string  // → "user_name" in JSON
    CreatedAt int64  // → "created_at" in JSON
}
```

### 2. Derive Attributes (Future)

Auto-generate trait implementations.

```go
#[derive(Eq, Hash)]
type Key struct {
    Namespace string
    Name      string
}

#[derive(Clone)]
type Config struct { ... }

#[derive(Debug)]
type User struct { ... }  // Generates String() method
```

### 3. Layout Attributes (Future)

Control memory layout.

```go
#[repr(C)]
type CStruct struct {
    X int32
    Y int32
}

#[packed]
type Compact struct { ... }
```

### 4. Documentation Attributes (Future)

```go
#[deprecated(since = "1.0", message = "use UserV2")]
type User struct { ... }

#[doc("Primary user type for authentication")]
type AuthUser struct { ... }
```

## Field Tags vs Type Attributes

Vo has two levels of metadata:

| Level | Syntax | Purpose |
|-------|--------|---------|
| **Type** | `#[json]` | Trigger method generation |
| **Field** | `` `json:"name"` `` | Configure field mapping |

```go
#[json]                              // Type attribute: generate methods
type User struct {
    Name  string `json:"name"`       // Field tag: JSON key is "name"
    Age   int    `json:"age,omitempty"`
    Admin bool   `json:"-"`          // Field tag: skip this field
}
```

**Key difference:**
- Type attributes affect **bytecode generation** (new methods)
- Field tags affect **generated code content** (key names, omit logic)

## Compiler Processing

### Parse Phase

1. Detect `#[...]` before `type` declaration
2. Parse attribute name and arguments
3. Store in AST `TypeDef` node

### Analysis Phase

1. Validate attribute names and arguments
2. Store in type metadata

### Codegen Phase

1. Check type attributes
2. For `#[json]`: emit MarshalJSON/UnmarshalJSON bytecode
3. For `#[derive(Eq)]`: emit Eq method bytecode
4. etc.

## AST Representation

```rust
pub struct TypeDef {
    pub name: String,
    pub attributes: Vec<Attribute>,
    pub kind: TypeKind,
}

pub struct Attribute {
    pub name: String,        // "json", "derive", "deprecated"
    pub args: Vec<AttrArg>,
}

pub enum AttrArg {
    Flag(String),                    // "marshal", "strict"
    KeyValue(String, AttrValue),     // "since" = "1.0"
    Nested(String, Vec<AttrArg>),    // derive(Eq, Hash)
}

pub enum AttrValue {
    String(String),
    Int(i64),
    Bool(bool),
}
```

## Restrictions

1. **Only on named types**: Anonymous structs cannot have attributes
2. **No nesting beyond one level**: `#[foo(bar(x))]` is not allowed
3. **Position-sensitive**: Attributes must immediately precede `type`

## Error Handling

| Error | Example | Message |
|-------|---------|---------|
| Unknown attribute | `#[foobar]` | `unknown attribute 'foobar'` |
| Invalid argument | `#[json(foo)]` | `invalid argument 'foo' for attribute 'json'` |
| Missing required | `#[deprecated]` | `attribute 'deprecated' requires 'message' argument` |

## Comparison with Other Languages

| Language | Syntax | Example |
|----------|--------|---------|
| Vo | `#[json]` | `#[json] type User struct {...}` |
| Rust | `#[derive(...)]` | `#[derive(Serialize)] struct User {...}` |
| Go | `//go:generate` | Comment-based, external tool |
| Java | `@Annotation` | `@JsonSerialize class User {...}` |

Vo's design is closest to Rust, but simpler (no proc macros).
