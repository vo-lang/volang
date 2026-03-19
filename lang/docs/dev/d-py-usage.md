# d.py

```bash
./d.py test [both|vm|jit|gc|nostd|wasm] [-v|--verbose] [--arch=32|64] [--direct] [file_or_dir]
./d.py bench [all|vo|score|<name>] [--all-langs] [--arch=32|64]
./d.py loc [--with-tests]
./d.py clean [all|vo|rust]
./d.py play [--build-only]
./d.py run <file.vo> [--mode=vm|jit] [--codegen]
```

# vo (cmd/vo)

```bash
cargo run -p vo -- run <file> [--mode=jit] [--codegen]
cargo run -p vo -- build [path]
cargo run -p vo -- check [path]
cargo run -p vo -- dump <file.vob|file.vot>
cargo run -p vo -- compile <file.vot> [-o out.vob]
cargo run -p vo -- emit <file>
cargo run -p vo -- init <module-path>
cargo run -p vo -- mod add <module[@constraint]>
cargo run -p vo -- mod update [module]
cargo run -p vo -- mod sync [path]
cargo run -p vo -- mod verify [path]
cargo run -p vo -- mod download [path]
cargo run -p vo -- mod remove <module>
cargo run -p vo -- release verify [path]
cargo run -p vo -- release stage [path] --version <version> --out-dir <dir> [--commit <sha>] [--artifact KIND TARGET NAME PATH]
cargo run -p vo -- help
cargo run -p vo -- version
```
