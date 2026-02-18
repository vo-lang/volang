# Vo Standard Library Design

> **Scope note (2026-02):** This file records implementation tradeoffs (Vo vs native) and heuristic choices.
> The authoritative product contract (package scope, compatibility level, platform support)
> is maintained in `lang/docs/dev/stdlib-compatibility-matrix.md`.

## Design Principles

1. **Prefer Vo implementation**: Unless performance-critical or requires syscalls
2. **Consider FFI overhead**: ~50-100 cycles per Native call, no inlining
3. **JIT Vo performance**: ~5x Rust, but can be inlined
4. **Go-Compatible High-Frequency API**: Prioritize common Go usage with compatible semantics

## Decision Rules

| Scenario | Choice | Reason |
|----------|--------|--------|
| Work < 100 cycles | **Vo** | FFI overhead cancels benefit |
| Work > 500 cycles | **Native** | 5x gap significant |
| Large data tables | **Native** | Unicode tables, timezone data |
| System calls | **Native** | Required |
| Complex algorithms | **Native** | Boyer-Moore, float parsing |
| Simple loops/comparisons | **Vo** | Inlining benefit |

---

## Package Design

### 1. `errors` ✓ (Complete)

All Vo implementation.

```
New(msg) error
NewCode(code, msg) error
Wrap(cause, msg) error
WrapCode(cause, code, msg) error
WithData(cause, data) error
IsCode(err, code) bool
```

---

### 2. `strings`

| Function | Impl | Reason |
|----------|------|--------|
| `Index`, `LastIndex` | **Native** | Boyer-Moore worth it |
| `IndexByte`, `LastIndexByte` | Vo | Simple loop, JIT efficient |
| `IndexAny`, `LastIndexAny` | Vo | Simple double loop |
| `IndexRune` | Vo | for-range over runes |
| `Count` | **Native** | Long strings benefit |
| `ToLower`, `ToUpper`, `ToTitle` | **Native** | Unicode tables |
| `TrimSpace` | Vo | Simple scan |
| `Trim`, `TrimLeft`, `TrimRight` | Vo | Simple loop |
| `TrimPrefix`, `TrimSuffix` | Vo | Simple slice |
| `Split`, `SplitN`, `SplitAfter`, `SplitAfterN` | **Native** | Memory allocation |
| `Fields`, `FieldsFunc` | **Native** | Unicode space + allocation |
| `Replace`, `ReplaceAll` | **Native** | Complex allocation |
| `Repeat` | Vo | Prealloc + copy |
| `EqualFold` | **Native** | Unicode case folding |
| `Join` | Vo | Simple concat |
| `HasPrefix`, `HasSuffix` | Vo | Simple compare |
| `Contains`, `ContainsAny`, `ContainsRune` | Vo | Based on Index |
| `Compare` | Vo | Simple compare |
| `Cut`, `CutPrefix`, `CutSuffix` | Vo | Simple slice |
| `Clone` | Vo | Simple copy |

**Drop**: `Builder`, `Reader`, `Replacer`

---

### 3. `bytes`

Symmetric to `strings`.

| Function | Impl |
|----------|------|
| `Index`, `LastIndex` | **Native** |
| `IndexByte`, `LastIndexByte` | Vo |
| `IndexAny`, `LastIndexAny` | Vo |
| `Count` | **Native** |
| `Compare`, `Equal` | Vo |
| `ToLower`, `ToUpper` | **Native** |
| `Trim`, `TrimSpace`, `TrimLeft`, `TrimRight` | Vo |
| `TrimPrefix`, `TrimSuffix` | Vo |
| `Split`, `SplitN`, `SplitAfter`, `SplitAfterN` | **Native** |
| `Fields` | **Native** |
| `Replace`, `ReplaceAll` | **Native** |
| `Repeat` | Vo |
| `Join` | Vo |
| `HasPrefix`, `HasSuffix`, `Contains` | Vo |
| `Cut`, `CutPrefix`, `CutSuffix` | Vo |
| `Clone` | Vo |

**Drop**: `Buffer`, `Reader`

---

### 4. `strconv`

| Function | Impl | Reason |
|----------|------|--------|
| `Atoi`, `Itoa` | Vo | Simple loop |
| `ParseInt`, `ParseUint` | Vo | Loop + overflow check |
| `ParseBool` | Vo | Simple match |
| `FormatInt`, `FormatUint` | Vo | Simple loop |
| `FormatBool` | Vo | Simple |
| `ParseFloat` | **Native** | IEEE 754 complex |
| `FormatFloat` | **Native** | Precision complex |
| `Quote`, `Unquote` | Vo | Escape simple |
| `QuoteRune`, `UnquoteChar` | Vo | |
| `AppendInt`, `AppendFloat`, etc. | Vo | Wrapper |
| `IsPrint`, `IsGraphic` | **Native** | Unicode |

---

### 5. `unicode`

All **Native** - Unicode tables required.

```
IsLetter, IsDigit, IsSpace, IsUpper, IsLower, IsTitle
IsPrint, IsControl, IsPunct, IsGraphic, IsMark, IsNumber, IsSymbol
ToLower, ToUpper, ToTitle
SimpleFold
In(r, ranges)
```

---

### 6. `unicode/utf8`

All **Vo** - Bit operations, JIT efficient.

```
RuneLen(r) int
RuneCount(s), RuneCountInString(s) int
Valid(p), ValidString(s) bool
ValidRune(r) bool
DecodeRune(p), DecodeRuneInString(s) (rune, int)
DecodeLastRune(p), DecodeLastRuneInString(s) (rune, int)
EncodeRune(p, r) int
FullRune(p), FullRuneInString(s) bool
RuneStart(b) bool
AppendRune(p, r) []byte
```

Constants: `RuneError`, `RuneSelf`, `MaxRune`, `UTFMax`

---

### 7. `math`

| Function | Impl | Reason |
|----------|------|--------|
| `Abs`, `Max`, `Min`, `Dim` | Vo | One-liner, inline value |
| `Floor`, `Ceil`, `Round`, `Trunc` | **Native** | Hardware instructions |
| `Sqrt`, `Cbrt`, `Pow`, `Pow10`, `Hypot` | **Native** | libm |
| `Exp`, `Exp2`, `Log`, `Log2`, `Log10`, `Log1p`, `Expm1` | **Native** | libm |
| `Sin`, `Cos`, `Tan`, `Asin`, `Acos`, `Atan`, `Atan2` | **Native** | libm |
| `Sinh`, `Cosh`, `Tanh`, `Asinh`, `Acosh`, `Atanh` | **Native** | libm |
| `Mod`, `Modf`, `Frexp`, `Ldexp` | **Native** | IEEE 754 |
| `IsNaN`, `IsInf` | Vo | Simple bit compare |
| `Inf`, `NaN` | **Native** | Bit patterns |
| `Signbit`, `Copysign` | Vo | Simple bit ops |
| `FMA` | **Native** | Hardware instruction |

Constants: `E`, `Pi`, `Phi`, `Sqrt2`, `Ln2`, `Ln10`, `MaxFloat64`, `SmallestNonzeroFloat64`, `MaxInt`, `MinInt`

---

### 8. `math/bits`

| Function | Impl | Reason |
|----------|------|--------|
| `LeadingZeros`, `LeadingZeros8/16/32/64` | **Native** | clz instruction |
| `TrailingZeros`, `TrailingZeros8/16/32/64` | **Native** | ctz instruction |
| `OnesCount`, `OnesCount8/16/32/64` | **Native** | popcount instruction |
| `RotateLeft`, `RotateLeft8/16/32/64` | Vo | Simple shift combo |
| `RotateRight`, same variants | Vo | Simple shift combo |
| `Reverse`, `Reverse8/16/32/64` | Vo | Bit loop |
| `ReverseBytes`, `ReverseBytes16/32/64` | Vo | Simple swap |
| `Len`, `Len8/16/32/64` | Vo | Based on LeadingZeros |
| `Add`, `Add32/64` | **Native** | Carry handling |
| `Sub`, `Sub32/64` | **Native** | Borrow handling |
| `Mul`, `Mul32/64` | **Native** | High bits |
| `Div`, `Div32/64` | **Native** | Wide division |

---

### 9. `math/rand`

| Function | Impl |
|----------|------|
| `Seed` | **Native** |
| `Int`, `Intn`, `Int63`, `Int31`, `Int63n`, `Int31n` | **Native** |
| `Uint32`, `Uint64` | **Native** |
| `Float32`, `Float64` | **Native** |
| `Perm` | Vo |
| `Shuffle` | Vo |
| `Read` | **Native** |
| `ExpFloat64`, `NormFloat64` | Vo |
| `*Rand` source type | **Native** |

---

### 10. `sort`

All **Vo** - Closure inlining benefit.

```
Ints(x []int)
Float64s(x []float64)
Strings(x []string)
Slice(slice, less func(i, j int) bool)
SliceStable(slice, less func(i, j int) bool)
SliceIsSorted(slice, less func(i, j int) bool) bool
IntsAreSorted(x []int) bool
Float64sAreSorted(x []float64) bool
StringsAreSorted(x []string) bool
Search(n int, f func(int) bool) int
SearchInts(a []int, x int) int
SearchFloat64s(a []float64, x float64) int
SearchStrings(a []string, x string) int
Reverse(x []int)
```

---

### 11. `slices`

All **Vo** - Simple traversal operations.

```
// Search
Index[E comparable](s []E, v E) int
IndexFunc[E any](s []E, f func(E) bool) int
Contains[E comparable](s []E, v E) bool
ContainsFunc[E any](s []E, f func(E) bool) bool

// Compare
Equal[E comparable](s1, s2 []E) bool
EqualFunc[E1, E2 any](s1 []E1, s2 []E2, eq func(E1, E2) bool) bool
Compare[E cmp.Ordered](s1, s2 []E) int
CompareFunc[E1, E2 any](s1 []E1, s2 []E2, cmp func(E1, E2) int) int

// Binary search
BinarySearch[E cmp.Ordered](x []E, target E) (int, bool)
BinarySearchFunc[E, T any](x []E, target T, cmp func(E, T) int) (int, bool)

// Sort
Sort[E cmp.Ordered](x []E)
SortFunc[E any](x []E, cmp func(a, b E) int)
SortStableFunc[E any](x []E, cmp func(a, b E) int)
IsSorted[E cmp.Ordered](x []E) bool
IsSortedFunc[E any](x []E, cmp func(a, b E) int) bool

// Min/Max
Min[E cmp.Ordered](x []E) E
MinFunc[E any](x []E, cmp func(a, b E) int) E
Max[E cmp.Ordered](x []E) E
MaxFunc[E any](x []E, cmp func(a, b E) int) E

// Modify
Clone[S ~[]E, E any](s S) S
Compact[S ~[]E, E comparable](s S) S
CompactFunc[S ~[]E, E any](s S, eq func(E, E) bool) S
Grow[S ~[]E, E any](s S, n int) S
Clip[S ~[]E, E any](s S) S
Insert[S ~[]E, E any](s S, i int, v ...E) S
Delete[S ~[]E, E any](s S, i, j int) S
DeleteFunc[S ~[]E, E any](s S, del func(E) bool) S
Replace[S ~[]E, E any](s S, i, j int, v ...E) S
Reverse[S ~[]E, E any](s S)
Concat[S ~[]E, E any](slices ...S) S
```

Note: Vo doesn't have generics, use `any` + type assertions or provide typed variants.

---

### 12. `maps`

All **Vo**.

```
Clone[M ~map[K]V, K comparable, V any](m M) M
Copy[M1 ~map[K]V, M2 ~map[K]V, K comparable, V any](dst M1, src M2)
DeleteFunc[M ~map[K]V, K comparable, V any](m M, del func(K, V) bool)
Equal[M1, M2 ~map[K]V, K, V comparable](m1 M1, m2 M2) bool
EqualFunc[M1 ~map[K]V1, M2 ~map[K]V2, K comparable, V1, V2 any](m1 M1, m2 M2, eq func(V1, V2) bool) bool
Keys[M ~map[K]V, K comparable, V any](m M) []K
Values[M ~map[K]V, K comparable, V any](m M) []V
```

---

### 13. `encoding/hex`

All **Vo** - Simple table lookup.

```
Encode(dst, src []byte) int
Decode(dst, src []byte) (int, error)
EncodeToString(src []byte) string
DecodeString(s string) ([]byte, error)
EncodedLen(n int) int
DecodedLen(n int) int
```

---

### 14. `encoding/base64`

All **Vo** - Simple table lookup.

```
// Standard encoding
Encode(dst, src []byte) int  
Decode(dst, src []byte) (int, error)
EncodeToString(src []byte) string
DecodeString(s string) ([]byte, error)

// URL encoding
URLEncode(dst, src []byte) int
URLDecode(dst, src []byte) (int, error)
URLEncodeToString(src []byte) string
URLDecodeString(s string) ([]byte, error)

// Raw (no padding)
RawEncode, RawDecode, RawEncodeToString, RawDecodeString
RawURLEncode, RawURLDecode, RawURLEncodeToString, RawURLDecodeString

EncodedLen(n int) int
DecodedLen(n int) int
```

---

### 15. `encoding/json`

Implemented (Vo + native helpers). See compatibility matrix for contract level.

---

### 16. `regexp`

All **Native** - Use Rust regex crate.

```
Compile(expr string) (*Regexp, error)
MustCompile(expr string) *Regexp
Match(pattern string, b []byte) (bool, error)
MatchString(pattern, s string) (bool, error)
QuoteMeta(s string) string  // Vo

// Regexp methods
(*Regexp) Match(b []byte) bool
(*Regexp) MatchString(s string) bool
(*Regexp) Find(b []byte) []byte
(*Regexp) FindString(s string) string
(*Regexp) FindIndex(b []byte) []int
(*Regexp) FindStringIndex(s string) []int
(*Regexp) FindAll(b []byte, n int) [][]byte
(*Regexp) FindAllString(s string, n int) []string
(*Regexp) FindSubmatch(b []byte) [][]byte
(*Regexp) FindStringSubmatch(s string) []string
(*Regexp) FindAllSubmatch(b []byte, n int) [][][]byte
(*Regexp) FindAllStringSubmatch(s string, n int) [][]string
(*Regexp) ReplaceAll(src, repl []byte) []byte
(*Regexp) ReplaceAllString(src, repl string) string
(*Regexp) ReplaceAllFunc(src []byte, repl func([]byte) []byte) []byte
(*Regexp) ReplaceAllStringFunc(src string, repl func(string) string) string
(*Regexp) Split(s string, n int) []string
(*Regexp) SubexpNames() []string
(*Regexp) NumSubexp() int
(*Regexp) String() string
```

---

### 17. `fmt`

All **Native** - Requires reflection.

```
Print(a ...any) (n int, err error)
Println(a ...any) (n int, err error)
Printf(format string, a ...any) (n int, err error)
Sprint(a ...any) string
Sprintln(a ...any) string
Sprintf(format string, a ...any) string
Fprint(w io.Writer, a ...any) (n int, err error)
Fprintln(w io.Writer, a ...any) (n int, err error)
Fprintf(w io.Writer, format string, a ...any) (n int, err error)
Errorf(format string, a ...any) error
```

---

### 18. `io`

Implemented. See compatibility matrix for contract level and boundary notes.

---

### 19. `os`

Implemented. See compatibility matrix for contract level and platform notes.

---

### 20. `time`

Implemented. See compatibility matrix for contract level.

---

### 21. `sync`

Planned as **island-local coordination API**, not direct Go shared-memory semantics.

---

### 22. `context`

Planned. Target contract and priority are tracked in compatibility matrix.

---

## Implementation Summary

| Package | Native | Vo | Status |
|---------|--------|-----|--------|
| errors | 0 | 6 | ✓ Complete |
| strings | 8 | 18 | Partial |
| bytes | 7 | 17 | Partial |
| strconv | 4 | 10 | Partial |
| unicode | 15 | 0 | Partial |
| unicode/utf8 | 0 | 12 | TODO |
| math | 28 | 6 | Partial |
| math/bits | 12 | 10 | TODO |
| math/rand | 10 | 4 | TODO |
| sort | 0 | 14 | Partial |
| slices | 0 | 30 | TODO |
| maps | 0 | 7 | TODO |
| encoding/hex | 0 | 6 | Partial |
| encoding/base64 | 0 | 16 | Partial |
| regexp | 25 | 2 | Partial |
| fmt | 6 | 4 | Partial |
| dyn | 2 | 10 | ✓ Complete |
| **Total** | ~120 | ~170 | |

**Native ratio: ~41%**
