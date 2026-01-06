# Vo JSON Library Design

## Core Philosophy

Vo's `~>` operator is naturally suited for JSON operations. JSON is inherently dynamic, and `~>` was designed precisely for this.

## Design Principles

1. **Dynamic-first**: Parse and access with `~>` directly, no type definitions required
2. **Type-safe when needed**: Support Marshal/Unmarshal to concrete types via compile-time codegen
3. **Composable**: Builder API for constructing complex JSON
4. **Precision-preserving**: `json.Number` for lossless number handling

## Prerequisites

This library relies on **Type Attributes** (`#[...]` syntax) for structured serialization.
See full spec: [Type Attributes Spec](../spec/type-attributes.md)

---

## Data Carriers

| Scenario | Carrier | Access Method |
|----------|---------|---------------|
| **Dynamic** | `dyn.MapObject` / `dyn.SliceObject` | `~>` operator |
| **Static** | User struct with `Marshaler`/`Unmarshaler` | Field access |

---

## API Design

### 1. Dynamic Parsing (Parse)

```go
package json

// Parse parses JSON into a dynamic object tree
// Object → dyn.MapObject
// Array  → dyn.SliceObject  
// String → string
// Number → json.Number (preserves original representation)
// Bool   → bool
// null   → nil
func Parse(data []byte) (any, error)

// ParseString convenience method
func ParseString(s string) (any, error)
```

**Usage**:

```go
data := []byte(`{
    "user": {"name": "Alice", "age": 30},
    "tags": ["admin", "active"]
}`)

obj := json.Parse(data)?

// ~> chained access - Vo signature feature!
name := obj~>user~>name?              // any("Alice")
age := (obj~>user~>age?).(int)        // 30
firstTag := obj~>tags~>[0]?           // any("admin")

// Safe access - missing field returns error
addr, err := obj~>user~>address       // err != nil
if err != nil {
    addr = "unknown"
}
```

### 2. Dynamic Building (Builder)

```go
// Object creates an empty JSON object
func Object() dyn.MapObject

// Array creates a JSON array
func Array(elems ...any) dyn.SliceObject

// From creates from map/slice
func From(v any) (any, error)
```

**Usage**:

```go
// Build JSON
user := json.Object()
user~>name = "Alice"
user~>age = 30
user~>tags = json.Array("admin", "active")
user~>meta = json.Object()
user~>meta~>created = "2024-01-01"

// Nested building
response := json.Object()
response~>code = 200
response~>data = user
response~>items = json.Array(
    json.Object(),  // Nest Object in Array
)
response~>items~>[0]~>id = 1
```

### 3. Dynamic Modification

```go
// Delete removes a field (supports path)
func Delete(obj any, path ...string) error

// Has checks if field exists
func Has(obj any, path ...string) bool

// Keys gets all object keys
func Keys(obj any) ([]string, error)

// Len gets array/object length
func Len(obj any) (int, error)
```

**Usage**:

```go
obj := json.Parse(data)?

// Modify
obj~>user~>name = "Bob"

// Delete
json.Delete(obj, "user", "deprecated")

// Check
if json.Has(obj, "user", "email") {
    email := obj~>user~>email?
}

// Iterate
for _, key := range json.Keys(obj)? {
    val, _ := obj~>[key]
    println(key, val)
}
```

### 4. Serialization (Stringify)

```go
// Stringify serializes dynamic object to JSON
func Stringify(v any) ([]byte, error)

// StringifyIndent with indentation
func StringifyIndent(v any, prefix, indent string) ([]byte, error)

// StringifyTo writes to Writer (streaming)
func StringifyTo(w io.Writer, v any) error
```

**Usage**:

```go
obj := json.Object()
obj~>name = "Alice"
obj~>scores = json.Array(90, 85, 92)

// Compact format
data := json.Stringify(obj)?
// {"name":"Alice","scores":[90,85,92]}

// Pretty format
pretty := json.StringifyIndent(obj, "", "  ")?
// {
//   "name": "Alice",
//   "scores": [90, 85, 92]
// }
```

### 5. Number Type

JSON numbers have no int/float distinction. `json.Number` preserves the original representation for lossless handling.

```go
// Number represents a JSON number literal.
// It preserves the original string representation for precision.
type Number string

// Int converts to int. Returns error if not representable.
func (n Number) Int() (int, error)

// Int64 converts to int64. Returns error if not representable.
func (n Number) Int64() (int64, error)

// Float64 converts to float64.
func (n Number) Float64() (float64, error)

// String returns the original JSON number literal.
func (n Number) String() string

// IsInt returns true if the number is an integer (no decimal point, no exponent).
func (n Number) IsInt() bool
```

**Usage - Chained `~>` method calls**:

```go
obj := json.Parse(data)?

// Direct chaining: ~> supports method calls on any
age := obj~>user~>age~>Int()?           // json.Number.Int() via ~>
price := obj~>item~>price~>Float64()?   // json.Number.Float64() via ~>

// Check if integer before converting
n := (obj~>amount?).(json.Number)
if n.IsInt() {
    i := n.Int()?
    println("Integer:", i)
} else {
    f := n.Float64()?
    println("Float:", f)
}

// Keep original string for precision-sensitive values
balance := obj~>account~>balance~>String()?  // "123456789.123456789"
```

### 6. Structured Serialization (Marshal/Unmarshal) - Compiler Auto-generation

The **compiler** automatically generates `MarshalJSON`/`UnmarshalJSON` bytecode for types marked with `#[json]` attribute.

#### Type Attribute Syntax

Attribute goes on `type`, because semantic is "generate methods for this type":

```go
#[json]
type User struct {
    Name  string   `json:"name"`
    Age   int      `json:"age,omitempty"`
    Tags  []string `json:"tags,omitempty"`
    Admin bool     `json:"-"`  // ignored field
}

// Compiler generates MarshalJSON + UnmarshalJSON bytecode for User type
```

#### Method Generation Control

```go
#[json]                  // default: generate both Marshal and Unmarshal
type User struct { ... }

#[json(marshal)]         // only generate MarshalJSON
type Response struct { ... }

#[json(unmarshal)]       // only generate UnmarshalJSON
type Request struct { ... }

#[json(marshal, unmarshal)]  // explicit both (same as default)
type Data struct { ... }
```

#### Additional Options

```go
#[json(unmarshal, strict)]      // error on unknown fields during unmarshal
#[json(snake_case)]             // auto snake_case field naming
```

#### Interfaces

```go
// Marshaler is implemented by types that can marshal themselves to JSON.
type Marshaler interface {
    MarshalJSON() ([]byte, error)
}

// Unmarshaler is implemented by types that can unmarshal JSON into themselves.
type Unmarshaler interface {
    UnmarshalJSON(data []byte) error
}
```

#### Compiler Codegen Flow

1. **Parse phase**: Detect `#[json]` attribute on struct, extract field tags
2. **Analysis phase**: Store attribute + field tags in StructMeta
3. **Codegen phase**: For structs with `#[json]`, emit bytecode for:
   - `MarshalJSON() ([]byte, error)` 
   - `UnmarshalJSON(data []byte) error`

#### Usage

```go
// Marshal - compiler-generated MarshalJSON
user := User{Name: "Alice", Age: 30, Tags: []string{"dev"}}
data := json.Marshal(&user)?
// {"name":"Alice","age":30,"tags":["dev"]}

// Unmarshal - compiler-generated UnmarshalJSON
var user2 User
json.Unmarshal(data, &user2)?
```

#### Struct Tag Options

| Tag | Meaning |
|-----|---------|
| `json:"name"` | Use "name" as JSON key |
| `json:"name,omitempty"` | Omit if zero value |
| `json:"-"` | Ignore this field |
| `json:",string"` | Encode number/bool as string |

### 7. Type Conversion Helpers

Primary access is via `~>` chaining. Helpers are for type assertions and conditional access.

```go
// Type assertions (for when you need to check type)
func AsString(v any) (string, bool)
func AsNumber(v any) (Number, bool)
func AsBool(v any) (bool, bool)
func AsObject(v any) (dyn.MapObject, bool)
func AsArray(v any) (dyn.SliceObject, bool)

// Null check
func IsNull(v any) bool
```

**Usage**:

```go
obj := json.Parse(data)?

// Primary: ~> chaining (preferred)
name := (obj~>user~>name?).(string)     // string field
age := obj~>user~>age~>Int()?           // Number → int via method
price := obj~>item~>price~>Float64()?   // Number → float64

// Conditional access with type check
if arr, ok := json.AsArray(obj~>tags?); ok {
    for i := 0; i < len(arr); i++ {
        tag := (arr~>[i]?).(string)
        println(tag)
    }
}

// Number type check
if n, ok := json.AsNumber(obj~>value?); ok {
    if n.IsInt() {
        println("int:", n.Int()?)
    } else {
        println("float:", n.Float64()?)
    }
}
```

### 8. null Handling

```go
// IsNull checks if value is JSON null
func IsNull(v any) bool {
    return v == nil
}

// HasKey checks if key exists in object (distinguishes missing vs null)
func HasKey(obj any, key string) bool
```

**Usage**:

```go
obj := json.Parse(`{"name": "Alice", "email": null}`)

// Check existence vs null
if json.HasKey(obj, "email") {
    v := obj~>email?
    if json.IsNull(v) {
        println("email is null")
    } else {
        println("email:", v)
    }
} else {
    println("email field missing")
}

// Stringify: nil → "null"
obj := json.Object()
obj~>value = nil
json.Stringify(obj)?  // {"value":null}
```

### 9. Query - Advanced Feature

```go
// Query uses path expressions
// Supports: "user.name", "items[0].id", "items[*].name"
func Query(obj any, path string) (any, error)

// QueryAll returns all matches
func QueryAll(obj any, path string) ([]any, error)
```

**Usage**:

```go
data := []byte(`{
    "items": [
        {"id": 1, "name": "A"},
        {"id": 2, "name": "B"}
    ]
}`)
obj := json.Parse(data)?

// Single value query
name := json.Query(obj, "items[0].name")?  // "A"

// Wildcard query
names := json.QueryAll(obj, "items[*].name")?  // ["A", "B"]
```

---

## Error Codes

```go
package json

import "errors"

// JSON error codes (range 2000-2099)
const (
    ErrSyntax       = 2000  // JSON syntax error
    ErrUnexpectedEOF = 2001  // Unexpected end
    ErrInvalidValue = 2002  // Invalid value
    ErrTypeMismatch = 2003  // Type mismatch
    ErrPathNotFound = 2004  // Path not found
    ErrNotObject    = 2005  // Not an object type
    ErrNotArray     = 2006  // Not an array type
    ErrMarshal      = 2007  // Serialization error
    ErrUnmarshal    = 2008  // Deserialization error
)

// SyntaxError provides detailed syntax error info
type SyntaxError struct {
    Offset int64   // Error position
    msg    string
}

func (e *SyntaxError) Error() string
func (e *SyntaxError) Code() int { return ErrSyntax }
```

---

## Comparison with Go stdlib

| Feature | Go `encoding/json` | Vo `json` |
|---------|-------------------|-----------|
| Dynamic access | Requires type assertion | `~>` chained access |
| Error handling | `if err != nil` | `?` propagation |
| Build JSON | `map[string]any{}` | `json.Object()` + `~>=` |
| Nested access | Multiple type assertions | `obj~>a~>b~>c?` |
| null handling | Manual check | `json.IsNull()` |

---

## Implementation Structure

```
stdlib/encoding/json/
├── json.vo          # Main entry, Parse/Stringify, type helpers
├── number.vo        # json.Number type
├── builder.vo       # Object/Array builder
├── marshal.vo       # Marshaler/Unmarshaler interfaces, Marshal/Unmarshal dispatch
├── query.vo         # Query path expressions (optional, phase 2)
└── errors.vo        # Error definitions

# Compiler changes (vo-codegen)
# - Detect structs with json: tags
# - Auto-generate MarshalJSON/UnmarshalJSON bytecode
```

---

## Complete Example

```go
package main

import (
    "json"
    "fmt"
)

func main() {
    // 1. Parse API response
    resp := json.Parse(apiResponse)?
    
    // 2. Dynamic access with ~> chaining
    code := resp~>code~>Int()?
    if code != 200 {
        msg := (resp~>message?).(string)
        fail errors.New(msg)
    }
    
    // 3. Process data with ~> chaining
    users := resp~>data~>users?
    for i := 0; i < json.Len(users)?; i++ {
        user := users~>[i]?
        name := (user~>name?).(string)
        
        // Number → string for precision
        balance := user~>balance~>String()?
        fmt.Println(name, "balance:", balance)
        
        // Check null vs missing
        if json.HasKey(user, "email") {
            email := user~>email?
            if json.IsNull(email) {
                fmt.Println(name, "email is null")
            } else {
                fmt.Println(name, email)
            }
        }
    }
    
    // 4. Build response
    result := json.Object()
    result~>status = "ok"
    result~>processed = json.Len(users)?
    
    output := json.StringifyIndent(result, "", "  ")?
    fmt.Println(string(output))
}

// 5. Structured serialization - #[json] on type triggers bytecode generation
#[json]
type User struct {
    Name    string `json:"name"`
    Age     int    `json:"age,omitempty"`
    Balance string `json:"balance"`  // Use string for precise numbers
}

func processUser(data []byte) (User, error) {
    var user User
    json.Unmarshal(data, &user)?  // Uses compiler-generated UnmarshalJSON
    return user, nil
}
```

---

## Implementation Notes

### Parse Implementation

The parser is implemented in pure Vo for simplicity and self-containment:

```go
// stdlib/encoding/json/parse.vo
func Parse(data []byte) (any, error) {
    p := &parser{data: data, pos: 0}
    return p.parseValue()
}

type parser struct {
    data []byte
    pos  int
}

func (p *parser) parseValue() (any, error) {
    p.skipWhitespace()
    if p.pos >= len(p.data) {
        return nil, NewError(ErrUnexpectedEOF, "unexpected end of input")
    }
    c := p.data[p.pos]
    switch {
    case c == '{':
        return p.parseObject()
    case c == '[':
        return p.parseArray()
    case c == '"':
        return p.parseString()
    case c == 't' || c == 'f':
        return p.parseBool()
    case c == 'n':
        return p.parseNull()
    default:
        return p.parseNumber()
    }
}
// ... other methods
```

### json.Number Implementation

```go
// stdlib/encoding/json/number.vo
package json

import "errors"

type Number string

func (n Number) String() string {
    return string(n)
}

func (n Number) IsInt() bool {
    s := string(n)
    for i := 0; i < len(s); i++ {
        c := s[i]
        if c == '.' || c == 'e' || c == 'E' {
            return false
        }
    }
    return true
}

func (n Number) Int() (int, error) {
    // Parse integer from string
    // Return error if overflow or has decimal
}

func (n Number) Int64() (int64, error) {
    // Parse int64 from string
}

func (n Number) Float64() (float64, error) {
    // Parse float64 from string
}
```

### Stringify Implementation

Implemented in pure Vo, traversing the object tree recursively:

```go
// stdlib/encoding/json/stringify.vo
func Stringify(v any) ([]byte, error) {
    var buf bytes.Buffer
    err := writeValue(&buf, v)
    if err != nil {
        return nil, err
    }
    return buf.Bytes(), nil
}

func writeValue(buf *bytes.Buffer, v any) error {
    if v == nil {
        buf.WriteString("null")
        return nil
    }
    switch val := v.(type) {
    case dyn.MapObject:
        return writeObject(buf, val)
    case dyn.SliceObject:
        return writeArray(buf, val)
    case string:
        return writeString(buf, val)
    case Number:
        buf.WriteString(string(val))
    case int:
        buf.WriteString(itoa(val))
    case bool:
        if val { buf.WriteString("true") } else { buf.WriteString("false") }
    default:
        return NewError(ErrMarshal, "unsupported type")
    }
    return nil
}
```

### Marshal/Unmarshal - Compiler Auto-generation

**No runtime reflection.** The compiler detects `#[json]` attribute on type and generates bytecode directly.

#### Compiler Flow

1. **Parse**: Detect `#[json]` on type declaration, extract field tags into AST
2. **Analysis**: Store attribute + field tags in TypeMeta
3. **Codegen**: Based on attribute args, emit bytecode for:
   - `#[json]` → both MarshalJSON + UnmarshalJSON
   - `#[json(marshal)]` → only MarshalJSON
   - `#[json(unmarshal)]` → only UnmarshalJSON

#### What Compiler Generates (conceptual bytecode)

```go
// For:
// #[json]
// type User struct { Name string `json:"name"`; Age int `json:"age,omitempty"` }
// 
// Compiler emits this bytecode:

func (u *User) MarshalJSON() ([]byte, error) {
    obj := json.Object()
    obj~>name = u.Name
    if u.Age != 0 {  // omitempty check
        obj~>age = u.Age
    }
    return json.Stringify(obj)
}

func (u *User) UnmarshalJSON(data []byte) error {
    obj := json.Parse(data)?
    u.Name = (obj~>name?).(string)
    u.Age = obj~>age~>Int()?
    return nil
}
```

#### json.Marshal/Unmarshal Dispatch

```go
// Marshal checks if v implements Marshaler
func Marshal(v any) ([]byte, error) {
    if m, ok := v.(Marshaler); ok {
        return m.MarshalJSON()
    }
    // For non-Marshaler types, use dynamic serialization
    return Stringify(v)
}

// Unmarshal checks if v implements Unmarshaler
func Unmarshal(data []byte, v any) error {
    if u, ok := v.(Unmarshaler); ok {
        return u.UnmarshalJSON(data)
    }
    return errors.New("type does not implement Unmarshaler")
}
```

#### TypeDef / StructMeta Extension

```rust
// Type-level attribute (on type declaration)
pub struct TypeDef {
    pub name: String,
    pub attributes: Vec<Attribute>,  // #[json], #[json(marshal)], etc.
    pub kind: TypeKind,              // Struct, Alias, etc.
}

pub struct Attribute {
    pub name: String,       // "json"
    pub args: Vec<String>,  // ["marshal"], ["unmarshal", "strict"]
}

// Field-level tags (on struct fields)
pub struct StructMeta {
    pub field_names: Vec<String>,
    pub field_offsets: Vec<u16>,
    pub slot_types: Vec<SlotType>,
    pub field_value_metas: Vec<u32>,
    pub field_tags: Vec<String>,  // `json:"name,omitempty"` etc.
}
```

### Key Design Decisions

**1. Why `dyn.MapObject` instead of custom JSON type?**

- Reuses existing dynamic access infrastructure
- `~>` works out of the box
- No special-casing in the runtime
- Consistent with Vo's "dynamic access on any" philosophy

**2. Why `json.Number` instead of int/float64?**

- JSON has no int/float distinction
- Preserves original representation (no precision loss)
- User decides conversion (Int vs Float64)
- Similar to Go's `json.Number`

**3. Why compiler auto-generation for struct serialization?**

- No runtime reflection needed
- Zero overhead at runtime
- Compile-time type checking
- No external tools or generated source files
- Similar to Rust's `#[derive(Serialize)]`

**4. Why `#[json]` on `type` not `struct`?**

- Semantic: "generate methods for this type"
- Methods are bound to types, not struct literals
- Allows control: `#[json(marshal)]` vs `#[json(unmarshal)]`
- Anonymous structs cannot have methods → no `#[json]`

**5. Why `~>` chaining for Number conversion?**

- `obj~>age~>Int()?` is more Vo-idiomatic than `json.AsInt(obj~>age?)`
- Leverages existing `~>` method call support
- Cleaner, more readable code
- Consistent with dynamic access philosophy
