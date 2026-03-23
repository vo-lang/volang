# Error Handling

Vo replaces Go's `if err != nil` pattern with concise operators: `?`, `fail`, and `errdefer`.

## The `?` Operator

Appending `?` to an expression that returns `(T, error)` will:
- **On success**: unwrap the value of type `T`
- **On error**: immediately return the error from the enclosing function

```vo
func loadUser(id int) (*User, error) {
    data := fetchFromDB(id)?       // returns error if fetch fails
    user := parseUser(data)?       // returns error if parse fails
    return user, nil
}
```

This is equivalent to the Go pattern:

```go
data, err := fetchFromDB(id)
if err != nil {
    return nil, err
}
user, err := parseUser(data)
if err != nil {
    return nil, err
}
```

## The `fail` Statement

`fail` explicitly returns an error from the current function:

```vo
func validate(age int) error {
    if age < 0 {
        fail errors.New("age cannot be negative")
    }
    if age > 150 {
        fail fmt.Errorf("unrealistic age: %d", age)
    }
    return nil
}
```

## `errdefer`

`errdefer` schedules a cleanup action that runs **only when the function returns with an error**. Regular `defer` always runs.

```vo
func createAccount(name string) (*Account, error) {
    acct := allocAccount(name)?
    errdefer deleteAccount(acct)    // rollback if anything below fails

    acct.Profile = loadProfile(acct.ID)?
    acct.Settings = loadDefaults()?

    return acct, nil
}
```

If `loadProfile` or `loadDefaults` fails, `deleteAccount` runs automatically. If everything succeeds, `deleteAccount` is skipped.

## Combining Patterns

```vo
func processFile(path string) ([]Record, error) {
    file := os.Open(path)?
    defer file.Close()              // always close the file

    tmp := createTempFile()?
    errdefer os.Remove(tmp.Name())  // remove temp file only on error
    defer tmp.Close()               // always close temp file

    records := parseCSV(file)?
    validated := validateAll(records)?
    writeCache(tmp, validated)?

    return validated, nil
}
```
