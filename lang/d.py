#!/usr/bin/env python3
"""
Vo Development Tool - Unified test/bench/loc script

Usage:
    ./d.py test [vm|jit|gc|nostd|wasm] [-v] [file.vo]
    ./d.py bench [all|vo|<name>|score] [--all-langs]
    ./d.py loc [--with-tests]
    ./d.py wasm [--release]     Build vo-web for WASM
"""

import argparse
import json
import os
import subprocess
import sys
import time
from collections import defaultdict
from datetime import datetime
from pathlib import Path
from typing import Optional

# 32-bit target (ARM 32-bit for ARM64 host)
TARGET_32 = 'armv7-unknown-linux-gnueabihf'
# QEMU for running ARM32 on ARM64
QEMU_ARM = 'qemu-arm'
QEMU_ARM_LD_PREFIX = '/usr/arm-linux-gnueabihf'

# Check that we're running from the correct directory (repo root, not lang/)
_script_dir = Path(__file__).resolve().parent
_expected_cwd = _script_dir.parent  # repo root
if Path.cwd().resolve() != _expected_cwd:
    print(f"Error: d.py must be run from repo root: {_expected_cwd}", file=sys.stderr)
    print(f"Current directory: {Path.cwd()}", file=sys.stderr)
    print(f"Run: cd {_expected_cwd} && ./d.py ...", file=sys.stderr)
    sys.exit(1)

try:
    import yaml
    HAS_YAML = True
except ImportError:
    HAS_YAML = False


def parse_config_yaml(path: Path) -> list[dict]:
    """Simple parser for _config.yaml without PyYAML dependency."""
    entries = []
    current_entry = None
    in_tests = False
    
    with open(path, encoding='utf-8') as f:
        for line in f:
            stripped = line.rstrip()
            if not stripped or stripped.startswith('#'):
                continue
            
            # Section header: "tests:"
            if stripped == 'tests:':
                in_tests = True
                continue
            
            if not in_tests:
                continue
            
            # New entry: "  - file: xxx"
            if stripped.startswith('  - file:'):
                if current_entry:
                    entries.append(current_entry)
                file_name = stripped.split(':', 1)[1].strip()
                current_entry = {'file': file_name}
            # Skip modes: "    skip: [vm, jit]"
            elif current_entry and 'skip:' in stripped:
                skip_part = stripped.split('skip:', 1)[1].strip()
                if skip_part.startswith('[') and skip_part.endswith(']'):
                    modes = [m.strip() for m in skip_part[1:-1].split(',')]
                    current_entry['skip'] = [m for m in modes if m]
            # should_fail: true
            elif current_entry and 'should_fail:' in stripped:
                current_entry['should_fail'] = 'true' in stripped.lower()
            # zip_root: path/
            elif current_entry and 'zip_root:' in stripped:
                current_entry['zip_root'] = stripped.split('zip_root:', 1)[1].strip()
    
    if current_entry:
        entries.append(current_entry)
    
    return entries


class Colors:
    BOLD = '\033[1m'
    DIM = '\033[2m'
    GREEN = '\033[0;32m'
    RED = '\033[0;31m'
    CYAN = '\033[0;36m'
    YELLOW = '\033[1;33m'
    BLUE = '\033[0;34m'
    NC = '\033[0m'

    @classmethod
    def disable(cls):
        cls.BOLD = cls.DIM = cls.GREEN = cls.RED = ''
        cls.CYAN = cls.YELLOW = cls.BLUE = cls.NC = ''


if not sys.stdout.isatty():
    Colors.disable()


SCRIPT_DIR = Path(__file__).parent.resolve()
PROJECT_ROOT = SCRIPT_DIR.parent
TEST_DIR = PROJECT_ROOT / 'lang' / 'test_data'
TEST_CONFIG = TEST_DIR / '_config.yaml'
BENCHMARK_DIR = PROJECT_ROOT / 'lang' / 'benchmark'
RESULTS_DIR = BENCHMARK_DIR / 'results'

# Cache directories
CLI_CACHE_DIR = PROJECT_ROOT / 'lang' / 'cli' / '.vo-cache'
VOX_EXT_DIR = PROJECT_ROOT / 'libs' / 'vox'
VOX_EXT_RUST_DIR = VOX_EXT_DIR / 'rust' / 'src'


def get_newest_mtime(*paths: Path, pattern: str = '*') -> float:
    """Get the newest modification time from files matching pattern in given paths."""
    newest = 0.0
    for path in paths:
        if not path.exists():
            continue
        if path.is_file():
            newest = max(newest, path.stat().st_mtime)
        else:
            for f in path.rglob(pattern):
                if f.is_file():
                    newest = max(newest, f.stat().st_mtime)
    return newest


def invalidate_cli_cache_if_needed():
    """Clear CLI .vo-cache if source files are newer than cache."""
    import shutil
    
    if not CLI_CACHE_DIR.exists():
        return
    
    cache_mtime = CLI_CACHE_DIR.stat().st_mtime
    
    # Check CLI source files
    cli_src_mtime = get_newest_mtime(CLI_DIR, pattern='*.vo')
    
    # Check vox library source files
    vox_vo_mtime = get_newest_mtime(VOX_EXT_DIR, pattern='*.vo')
    
    # Check vox Rust extension source files
    vox_rust_mtime = get_newest_mtime(VOX_EXT_RUST_DIR, pattern='*.rs')
    
    # Check if any source is newer than cache
    newest_src = max(cli_src_mtime, vox_vo_mtime, vox_rust_mtime)
    if newest_src > cache_mtime:
        print(f"{Colors.DIM}CLI cache outdated, clearing...{Colors.NC}")
        shutil.rmtree(CLI_CACHE_DIR)


def clean_caches(target: str = 'all'):
    """Clean Vo and/or Rust caches."""
    import shutil
    
    cleaned = []
    
    if target in ('all', 'vo'):
        # Clean 64-bit .vo-cache
        if CLI_CACHE_DIR.exists():
            shutil.rmtree(CLI_CACHE_DIR)
            cleaned.append(str(CLI_CACHE_DIR))
        
        # Clean 32-bit .vo-cache
        cache_32 = PROJECT_ROOT / 'target' / TARGET_32 / 'lang' / 'cli' / '.vo-cache'
        if cache_32.exists():
            shutil.rmtree(cache_32)
            cleaned.append(str(cache_32))
        
        # Clean 32-bit CLI marker
        marker_32 = PROJECT_ROOT / 'target' / TARGET_32 / 'lang' / 'cli' / '.32bit_cache_ready'
        if marker_32.exists():
            marker_32.unlink()
            cleaned.append(str(marker_32))
    
    if target in ('all', 'rust'):
        # Run cargo clean
        print("Running cargo clean...")
        result = subprocess.run(['cargo', 'clean'], cwd=PROJECT_ROOT, capture_output=True)
        if result.returncode == 0:
            cleaned.append('target/')
        else:
            print(f"{Colors.RED}cargo clean failed{Colors.NC}")
    
    if cleaned:
        print(f"{Colors.GREEN}Cleaned:{Colors.NC}")
        for path in cleaned:
            print(f"  - {path}")
    else:
        print("Nothing to clean.")


def build_wasm(release: bool = False):
    """Build vo-web for WASM target."""
    profile = 'release' if release else 'debug'
    target = 'wasm32-unknown-unknown'
    output_dir = PROJECT_ROOT / 'target' / target / profile
    
    print(f"{Colors.BOLD}Building vo-web for WASM ({profile})...{Colors.NC}")
    
    build_cmd = ['cargo', 'build', '-p', 'vo-web', '--target', target]
    if release:
        build_cmd.append('--release')
    
    code, stdout, stderr = run_cmd(build_cmd, capture=False)
    if code != 0:
        print(f"{Colors.RED}WASM build failed{Colors.NC}")
        sys.exit(1)
    
    wasm_file = output_dir / 'vo_web.wasm'
    if wasm_file.exists():
        size_kb = wasm_file.stat().st_size / 1024
        print(f"\n{Colors.GREEN}✓ Built:{Colors.NC} {wasm_file}")
        print(f"  Size: {size_kb:.1f} KB")
        print(f"\n{Colors.DIM}To generate JS bindings, use wasm-pack:{Colors.NC}")
        print(f"  wasm-pack build lang/crates/vo-web --target web" + (" --release" if release else ""))
    else:
        print(f"{Colors.GREEN}✓ Build completed{Colors.NC}")


def ensure_vox_extension_built(arch: str = '64', release: bool = False):
    """Build vo-vox extension if needed."""
    profile = 'release' if release else 'debug'
    
    if arch == '32':
        lib_path = PROJECT_ROOT / 'target' / TARGET_32 / profile / 'libvo_vox.so'
    else:
        lib_path = PROJECT_ROOT / 'target' / profile / 'libvo_vox.so'
    
    # Check if library exists and is up-to-date
    if lib_path.exists():
        lib_mtime = lib_path.stat().st_mtime
        src_mtime = get_newest_mtime(VOX_EXT_RUST_DIR, pattern='*.rs')
        cargo_mtime = (VOX_EXT_DIR / 'rust' / 'Cargo.toml').stat().st_mtime
        if lib_mtime > max(src_mtime, cargo_mtime):
            return  # Up-to-date
    
    print(f"{Colors.DIM}Building vo-vox extension ({profile})...{Colors.NC}")
    build_cmd = ['cargo', 'build', '-q', '-p', 'vo-vox', '--features', 'ffi']
    if release:
        build_cmd.append('--release')
    if arch == '32':
        build_cmd.extend(['--target', TARGET_32])
    
    code, _, stderr = run_cmd(build_cmd)
    if code != 0:
        print(f"{Colors.RED}Extension build failed:{Colors.NC}\n{stderr}")
        sys.exit(1)



def get_vo_bin(release: bool = False, arch: str = '64') -> Path:
    """Get the path to vo binary for the given architecture."""
    profile = 'release' if release else 'debug'
    if arch == '32':
        return PROJECT_ROOT / 'target' / TARGET_32 / profile / 'vo'
    return PROJECT_ROOT / 'target' / profile / 'vo'


def get_vo_embed_bin() -> Path:
    """Get the path to vo-embed binary (bytecode runner with no_std deps)."""
    return PROJECT_ROOT / 'target' / 'debug' / 'vo-embed'

# Legacy paths for backward compatibility
VO_BIN_DEBUG = PROJECT_ROOT / 'target' / 'debug' / 'vo'
VO_BIN_RELEASE = PROJECT_ROOT / 'target' / 'release' / 'vo'
STDLIB_DIR = PROJECT_ROOT / 'lang' / 'stdlib'
CLI_DIR = PROJECT_ROOT / 'lang' / 'cli'


def run_cmd(cmd: list[str], cwd: Path = None, env: dict = None, capture: bool = True) -> tuple[int, str, str]:
    full_env = os.environ.copy()
    if env:
        full_env.update(env)
    result = subprocess.run(
        cmd,
        cwd=cwd or PROJECT_ROOT,
        env=full_env,
        capture_output=capture,
        text=True
    )
    return result.returncode, result.stdout, result.stderr


def command_exists(cmd: str) -> bool:
    return subprocess.run(['command', '-v', cmd], shell=True, capture_output=True).returncode == 0 or \
           subprocess.run(f'command -v {cmd}', shell=True, capture_output=True).returncode == 0


# =============================================================================
# TEST RUNNER
# =============================================================================

class TestRunner:
    def __init__(self, verbose: bool = False, arch: str = '64'):
        self.verbose = verbose
        self.arch = arch
        self.vo_bin = get_vo_bin(release=False, arch=arch)
        self.config = self._load_config()
        self.vm_passed = 0
        self.vm_failed = 0
        self.vm_skipped = 0
        self.jit_passed = 0
        self.jit_failed = 0
        self.jit_skipped = 0
        self.nostd_passed = 0
        self.nostd_failed = 0
        self.nostd_skipped = 0
        self.failed_list: list[str] = []
        # CLI cache path (different structure for 32-bit due to extension paths)
        if arch == '32':
            self.cli_cache = PROJECT_ROOT / 'target' / TARGET_32 / 'lang' / 'cli'
        else:
            self.cli_cache = CLI_DIR

    def _make_vo_cmd(self, *args) -> list[str]:
        """Build vo command, prepending QEMU for 32-bit."""
        base = [str(self.vo_bin), '--cache', str(self.cli_cache)] + list(args)
        if self.arch == '32':
            return [QEMU_ARM, '-L', QEMU_ARM_LD_PREFIX] + base
        return base

    def _load_config(self) -> dict:
        """Load config, returns dict: file -> {skip: [], should_fail: bool}"""
        if not TEST_CONFIG.exists():
            return {}
        
        if HAS_YAML:
            with open(TEST_CONFIG) as f:
                data = yaml.safe_load(f) or {}
            entries = data.get('tests', []) if isinstance(data, dict) else []
        else:
            entries = parse_config_yaml(TEST_CONFIG)
        
        config = {}
        for entry in entries:
            if isinstance(entry, dict) and 'file' in entry:
                config[entry['file']] = {
                    'skip': entry.get('skip', []),
                    'should_fail': entry.get('should_fail', False),
                    'zip_root': entry.get('zip_root', None),
                }
        
        return config

    def should_skip(self, file: str, mode: str) -> bool:
        if file in self.config:
            return mode in self.config[file].get('skip', [])
        return False

    def should_fail(self, file: str) -> bool:
        if file in self.config:
            return self.config[file].get('should_fail', False)
        return False

    def get_zip_root(self, file: str) -> Optional[str]:
        if file in self.config:
            return self.config[file].get('zip_root')
        return None

    def _setup_32bit_cli_cache(self):
        """Create CLI cache directory with 32-bit extension paths (only if needed)."""
        import shutil
        
        cli_32 = self.cli_cache
        libs_32 = PROJECT_ROOT / 'target' / TARGET_32 / 'libs'
        
        # Check if CLI .vo files need update
        cli_marker = cli_32 / '.32bit_cache_ready'
        cli_needs_update = not cli_marker.exists()
        if not cli_needs_update:
            marker_time = cli_marker.stat().st_mtime
            cli_src_mtime = get_newest_mtime(CLI_DIR, pattern='*.vo')
            cli_needs_update = cli_src_mtime > marker_time
        
        # Check if libs/vox needs update (separate from CLI)
        libs_marker = libs_32 / 'vox' / '.libs_ready'
        libs_needs_update = not libs_marker.exists()
        if not libs_needs_update:
            marker_time = libs_marker.stat().st_mtime
            vox_src_mtime = max(
                get_newest_mtime(VOX_EXT_DIR, pattern='*.vo'),
                get_newest_mtime(VOX_EXT_DIR / 'vo.ext.toml'),
            )
            libs_needs_update = vox_src_mtime > marker_time
        
        if not cli_needs_update and not libs_needs_update:
            return
        
        # Update CLI .vo files if needed
        if cli_needs_update:
            # Only remove .vo files, keep .vo-cache
            cli_32.mkdir(parents=True, exist_ok=True)
            for vo_file in cli_32.glob('*.vo'):
                vo_file.unlink()
            for vo_file in CLI_DIR.glob('*.vo'):
                shutil.copy(vo_file, cli_32 / vo_file.name)
            # Clear .vo-cache since CLI source changed
            vo_cache = cli_32 / '.vo-cache'
            if vo_cache.exists():
                shutil.rmtree(vo_cache)
            cli_marker.touch()
        
        # Update libs/vox if needed
        if libs_needs_update:
            dst_dir = libs_32 / 'vox'
            dst_dir.mkdir(parents=True, exist_ok=True)
            
            for vo_file in VOX_EXT_DIR.glob('*.vo'):
                shutil.copy(vo_file, dst_dir / vo_file.name)
            
            toml_src = VOX_EXT_DIR / 'vo.ext.toml'
            if toml_src.exists():
                with open(toml_src) as f:
                    content = f.read()
                # Original path: ../../target/debug/libvo_vox.so (from libs/vox)
                # 32-bit cache is at: target/<arch>/libs/vox
                # Need path to: target/<arch>/debug/libvo_vox.so
                # Relative: ../../../<arch>/debug/libvo_vox.so
                content = content.replace('../../target/debug', f'../../../{TARGET_32}/debug')
                with open(dst_dir / 'vo.ext.toml', 'w') as f:
                    f.write(content)
            
            libs_marker.touch()

    def run(self, mode: str, single_file: Optional[str] = None):
        # Handle WASM tests separately
        if mode == 'wasm':
            self._run_wasm_tests()
            return
        
        # Invalidate CLI cache if source files changed
        invalidate_cli_cache_if_needed()
        
        # Build vo-launcher
        target_info = f" (target: {TARGET_32})" if self.arch == '32' else ""
        print(f"{Colors.DIM}Building vo-launcher{target_info}...{Colors.NC}")
        build_cmd = ['cargo', 'build', '-q', '-p', 'vo-launcher']
        if self.arch == '32':
            build_cmd.extend(['--target', TARGET_32, '--no-default-features'])
        code, _, stderr = run_cmd(build_cmd)
        if code != 0:
            print(f"{Colors.RED}Build failed:{Colors.NC}\n{stderr}")
            sys.exit(1)
        
        # Build vo-vox extension (both 32-bit and 64-bit need it)
        ensure_vox_extension_built(arch=self.arch, release=False)
        
        # Setup 32-bit CLI cache with adjusted extension paths
        if self.arch == '32':
            self._setup_32bit_cli_cache()

        if single_file:
            self._run_single_file(single_file)
            return

        print(f"{Colors.BOLD}Running Vo integration tests...{Colors.NC}\n")

        is_gc_mode = (mode == 'gc')
        is_nostd_mode = (mode == 'nostd')
        run_vm = mode in ('vm', 'both', 'gc')
        # JIT not supported on 32-bit
        run_jit = mode in ('jit', 'both', 'gc') and self.arch != '32'
        run_nostd = is_nostd_mode
        
        if self.arch == '32' and mode in ('jit', 'both'):
            print(f"{Colors.YELLOW}Note: JIT not supported on 32-bit, running VM tests only{Colors.NC}\n")
        
        # Build vo-embed for nostd mode (vo-vm/vo-runtime compiled with no_std)
        if run_nostd:
            print(f"{Colors.DIM}Building vo-embed (no_std deps)...{Colors.NC}")
            build_cmd = ['cargo', 'build', '-q', '-p', 'vo-embed']
            code, _, stderr = run_cmd(build_cmd)
            if code != 0:
                print(f"{Colors.RED}vo-embed build failed:{Colors.NC}\n{stderr}")
                sys.exit(1)

        test_files = sorted(TEST_DIR.rglob('*.vo'))
        for path in test_files:
            # Skip proj_* directories (handled separately)
            rel_path = path.relative_to(TEST_DIR)
            if str(rel_path).startswith('proj_'):
                continue
            
            rel_file = str(rel_path)
            file_name = path.name
            is_gc_file = file_name.startswith('gc_')

            # gc mode: only gc_* files; other modes: all files
            if is_gc_mode and not is_gc_file:
                continue

            should_fail = self.should_fail(rel_file)
            # Enable GC debug for gc_* files (in gc mode or when running individually)
            gc_debug = is_gc_mode and is_gc_file

            # should_fail tests only run once (not per mode)
            if should_fail:
                self._run_should_fail_test(rel_file)
                continue

            if run_vm:
                if self.should_skip(rel_file, 'vm'):
                    self.vm_skipped += 1
                    if self.verbose:
                        print(f"  {Colors.YELLOW}⊘{Colors.NC} {rel_file} [vm skipped]")
                else:
                    self._run_test(rel_file, 'vm', gc_debug)

            if run_jit:
                if self.should_skip(rel_file, 'jit'):
                    self.jit_skipped += 1
                    if self.verbose:
                        print(f"  {Colors.YELLOW}⊘{Colors.NC} {rel_file} [jit skipped]")
                else:
                    self._run_test(rel_file, 'jit', gc_debug)

            if run_nostd:
                if self.should_skip(rel_file, 'nostd'):
                    self.nostd_skipped += 1
                    if self.verbose:
                        print(f"  {Colors.YELLOW}⊘{Colors.NC} {rel_file} [nostd skipped]")
                else:
                    self._run_nostd_test(rel_file)

        if not is_gc_mode and not is_nostd_mode:
            # Run proj_* directory tests
            for proj_dir in sorted(TEST_DIR.glob('proj_*')):
                if not proj_dir.is_dir():
                    continue
                dir_name = proj_dir.name + '/'
                should_fail = self.should_fail(dir_name)
                if should_fail:
                    self._run_should_fail_test(dir_name)
                    continue

                if run_vm:
                    if self.should_skip(dir_name, 'vm'):
                        self.vm_skipped += 1
                    else:
                        self._run_test(dir_name, 'vm', False)

                if run_jit:
                    if self.should_skip(dir_name, 'jit'):
                        self.jit_skipped += 1
                    else:
                        self._run_test(dir_name, 'jit', False)

            # Run zip file tests
            for zip_file in sorted(TEST_DIR.rglob('*.zip')):
                rel_path = zip_file.relative_to(TEST_DIR)
                rel_file = str(rel_path)
                
                # Check for zip_root config (e.g., subdir.zip:src/)
                zip_root = self.get_zip_root(rel_file)
                test_path = f"{rel_file}:{zip_root}" if zip_root else rel_file
                
                if run_vm:
                    if self.should_skip(rel_file, 'vm'):
                        self.vm_skipped += 1
                    else:
                        self._run_test(test_path, 'vm', False)

                if run_jit:
                    if self.should_skip(rel_file, 'jit'):
                        self.jit_skipped += 1
                    else:
                        self._run_test(test_path, 'jit', False)

        self._print_results()

    def _run_single_file(self, file: str):
        path = Path(file)
        if not path.exists():
            path = TEST_DIR / file
        if not path.exists():
            print(f"{Colors.RED}File not found: {file}{Colors.NC}")
            sys.exit(1)

        is_gc = path.name.startswith('gc_')
        env = {'VO_GC_DEBUG': '1'} if is_gc else {}
        # Set QEMU library path for ARM32 on ARM64
        if self.arch == '32':
            env['QEMU_LD_PREFIX'] = QEMU_ARM_LD_PREFIX

        print(f"Running: {path}")
        cmd = self._make_vo_cmd('run', str(path))
        code, stdout, stderr = run_cmd(cmd, env=env, capture=False)
        sys.exit(code)

    def _run_should_fail_test(self, file: str):
        """Run a test that must fail at compile/type-check stage."""
        if file.endswith('/'):
            path = TEST_DIR / file.rstrip('/')
        else:
            path = TEST_DIR / file

        cmd = self._make_vo_cmd('run', str(path))

        if self.verbose:
            print(f"{Colors.DIM}Running (should_fail): {' '.join(cmd)}{Colors.NC}")

        start_time = time.time()
        code, stdout, stderr = run_cmd(cmd)
        elapsed = time.time() - start_time
        output = stdout + stderr

        # Check for compile/type-check errors (or crashes indicating compiler bug)
        has_analysis_error = ('analysis error:' in output or 
                              '[VO:PARSE:' in output or 
                              '[VO:CHECK:' in output or 
                              '[VO:CODEGEN:' in output or
                              '[VO:IO:' in output or
                              '[VO:COMPILE]' in output or
                              '[VO:PARSE]' in output or
                              'memory allocation of' in output)
        has_rust_panic = 'panicked at' in output

        time_color = Colors.YELLOW if elapsed > 1.0 else (Colors.DIM + Colors.YELLOW if elapsed > 0.1 else Colors.DIM)
        if has_rust_panic:
            panic_line = next((l for l in output.split('\n') if 'panicked at' in l), '')[:60]
            self._record_fail('vm', f"{file} [should_fail] [RUST PANIC: {panic_line}]")
            print(f"  {Colors.RED}✗{Colors.NC} {file} [should_fail] {time_color}({elapsed:.2f}s){Colors.NC}")
        elif has_analysis_error and code != 0:
            # Test correctly failed at compile/type-check stage
            self._record_pass('vm', f"{file} [should_fail]")
            print(f"  {Colors.GREEN}✓{Colors.NC} {file} [should_fail] {time_color}({elapsed:.2f}s){Colors.NC}")
        else:
            # Test should have failed but didn't
            self._record_fail('vm', f"{file} [should_fail] (expected compile error but passed)")
            print(f"  {Colors.RED}✗{Colors.NC} {file} [should_fail] {time_color}({elapsed:.2f}s){Colors.NC}")

        if self.verbose:
            print(output)

    def _run_test(self, file: str, mode: str, gc_debug: bool):
        if file.endswith('/'):
            path = TEST_DIR / file.rstrip('/')
        else:
            path = TEST_DIR / file

        env = {'VO_GC_DEBUG': '1'} if gc_debug else None
        jit_env = {'VO_JIT_CALL_THRESHOLD': '1'}
        if env:
            jit_env.update(env)

        cmd = self._make_vo_cmd('run', str(path), f'--mode={mode}')
        run_env = jit_env.copy() if mode == 'jit' else (env.copy() if env else {})

        if self.verbose:
            print(f"{Colors.DIM}Running: {' '.join(cmd)}{Colors.NC}")

        start_time = time.time()
        code, stdout, stderr = run_cmd(cmd, env=run_env)
        elapsed = time.time() - start_time
        output = stdout + stderr

        has_vo_error = ('[VO:PANIC:' in output or 
                        '[VO:PARSE:' in output or 
                        '[VO:CHECK:' in output or 
                        '[VO:CODEGEN:' in output or
                        '[VO:IO:' in output)
        has_rust_panic = 'panicked at' in output
        has_ok = '[VO:OK]' in output
        has_cli_error = output.strip().startswith('error:')

        time_color = Colors.YELLOW if elapsed > 1.0 else (Colors.DIM + Colors.YELLOW if elapsed > 0.1 else Colors.DIM)
        if has_ok and not has_vo_error and not has_rust_panic:
            self._record_pass(mode, f"{file} [{mode}]")
            print(f"  {Colors.GREEN}✓{Colors.NC} {file} [{mode}] {time_color}({elapsed:.2f}s){Colors.NC}")
        elif has_rust_panic:
            panic_line = next((l for l in output.split('\n') if 'panicked at' in l), '')[:60]
            self._record_fail(mode, f"{file} [{mode}] [RUST PANIC: {panic_line}]")
            print(f"  {Colors.RED}✗{Colors.NC} {file} [{mode}] {time_color}({elapsed:.2f}s){Colors.NC}")
        elif has_vo_error:
            msg = next((l for l in output.split('\n') if '[VO:PANIC:' in l or '[VO:PARSE:' in l or '[VO:CHECK:' in l or '[VO:CODEGEN:' in l or '[VO:IO:' in l), '')
            self._record_fail(mode, f"{file} [{mode}] {msg[:60]}")
            print(f"  {Colors.RED}✗{Colors.NC} {file} [{mode}] {time_color}({elapsed:.2f}s){Colors.NC}")
        elif has_cli_error:
            err_line = next((l for l in output.split('\n') if l.startswith('error:')), '')[:60]
            self._record_fail(mode, f"{file} [{mode}] [{err_line}]")
            print(f"  {Colors.RED}✗{Colors.NC} {file} [{mode}] {time_color}({elapsed:.2f}s){Colors.NC}")
        else:
            self._record_fail(mode, f"{file} [{mode}] [no [VO:OK] marker]")
            print(f"  {Colors.RED}✗{Colors.NC} {file} [{mode}] {time_color}({elapsed:.2f}s){Colors.NC}")

        if self.verbose:
            print(output)

    def _run_nostd_test(self, file: str):
        """Run nostd test: compile with vo-launcher, run with vo-embed (no_std deps)."""
        path = TEST_DIR / file
        
        # Step 1: Compile to bytecode using vo-launcher --compile-only (no --cache)
        bytecode_path = path.with_suffix('.vob')
        compile_cmd = [str(self.vo_bin), f'--compile-only={bytecode_path}', str(path)]
        
        if self.verbose:
            print(f"{Colors.DIM}Compiling: {' '.join(compile_cmd)}{Colors.NC}")
        
        start_time = time.time()
        code, stdout, stderr = run_cmd(compile_cmd)
        
        if code != 0:
            elapsed = time.time() - start_time
            output = stdout + stderr
            time_color = Colors.DIM
            self._record_fail('nostd', f"{file} [nostd] [compile failed]")
            print(f"  {Colors.RED}✗{Colors.NC} {file} [nostd] {time_color}({elapsed:.2f}s){Colors.NC}")
            if self.verbose:
                print(output)
            return
        
        # Step 2: Run bytecode with vo-embed
        vo_vm_bin = get_vo_embed_bin()
        run_cmd_list = [str(vo_vm_bin), str(bytecode_path)]
        
        if self.verbose:
            print(f"{Colors.DIM}Running: {' '.join(run_cmd_list)}{Colors.NC}")
        
        code, stdout, stderr = run_cmd(run_cmd_list)
        elapsed = time.time() - start_time
        output = stdout + stderr
        
        # Clean up bytecode file
        try:
            bytecode_path.unlink()
        except:
            pass
        
        has_vo_error = ('[VO:PANIC:' in output or 'Runtime error:' in output)
        has_rust_panic = 'panicked at' in output
        has_ok = '[VO:OK]' in output
        
        time_color = Colors.YELLOW if elapsed > 1.0 else (Colors.DIM + Colors.YELLOW if elapsed > 0.1 else Colors.DIM)
        if has_ok and not has_vo_error and not has_rust_panic:
            self._record_pass('nostd', f"{file} [nostd]")
            print(f"  {Colors.GREEN}✓{Colors.NC} {file} [nostd] {time_color}({elapsed:.2f}s){Colors.NC}")
        elif has_rust_panic:
            panic_line = next((l for l in output.split('\n') if 'panicked at' in l), '')[:60]
            self._record_fail('nostd', f"{file} [nostd] [RUST PANIC: {panic_line}]")
            print(f"  {Colors.RED}✗{Colors.NC} {file} [nostd] {time_color}({elapsed:.2f}s){Colors.NC}")
        elif has_vo_error:
            msg = next((l for l in output.split('\n') if '[VO:PANIC:' in l or 'Runtime error:' in l), '')[:60]
            self._record_fail('nostd', f"{file} [nostd] {msg}")
            print(f"  {Colors.RED}✗{Colors.NC} {file} [nostd] {time_color}({elapsed:.2f}s){Colors.NC}")
        else:
            self._record_fail('nostd', f"{file} [nostd] [no [VO:OK] marker]")
            print(f"  {Colors.RED}✗{Colors.NC} {file} [nostd] {time_color}({elapsed:.2f}s){Colors.NC}")
        
        if self.verbose:
            print(output)

    def _run_wasm_tests(self):
        """Run WASM tests using vo-web test runner."""
        print(f"{Colors.BOLD}Running WASM tests (vo-web)...{Colors.NC}\n")
        
        # Check if pkg exists, build if not
        pkg_dir = PROJECT_ROOT / 'lang' / 'crates' / 'vo-web' / 'pkg'
        if not pkg_dir.exists():
            print(f"{Colors.DIM}Building vo-web WASM (first time)...{Colors.NC}")
            code, _, stderr = run_cmd(['wasm-pack', 'build', 'lang/crates/vo-web', '--target', 'nodejs', '--out-dir', 'pkg'])
            if code != 0:
                print(f"{Colors.RED}wasm-pack build failed:{Colors.NC}\n{stderr}")
                sys.exit(1)
        
        # Run test runner
        test_runner = PROJECT_ROOT / 'lang' / 'crates' / 'vo-web' / 'test_runner.mjs'
        cmd = ['node', str(test_runner)]
        code, stdout, stderr = run_cmd(cmd, capture=False)
        
        if code != 0:
            sys.exit(1)

    def _record_pass(self, mode: str, msg: str):
        if mode == 'vm':
            self.vm_passed += 1
        elif mode == 'nostd':
            self.nostd_passed += 1
        else:
            self.jit_passed += 1

    def _record_fail(self, mode: str, msg: str):
        if mode == 'vm':
            self.vm_failed += 1
        elif mode == 'nostd':
            self.nostd_failed += 1
        else:
            self.jit_failed += 1
        self.failed_list.append(msg)

    def _print_results(self):
        total_passed = self.vm_passed + self.jit_passed + self.nostd_passed
        total_failed = self.vm_failed + self.jit_failed + self.nostd_failed

        print()
        if self.failed_list:
            print(f"{Colors.RED}{Colors.BOLD}Failed:{Colors.NC}")
            for msg in self.failed_list:
                print(f"  {Colors.RED}✗{Colors.NC} {msg}")
            print()

        print(f"{Colors.CYAN}╔══════════════════════════════════════════════════════════╗{Colors.NC}")
        print(f"{Colors.CYAN}║{Colors.NC}{Colors.BOLD}                   Vo Test Results                        {Colors.NC}{Colors.CYAN}║{Colors.NC}")
        print(f"{Colors.CYAN}╠══════════════════════════════════════════════════════════╣{Colors.NC}")
        print(f"{Colors.CYAN}║{Colors.NC}  VM:    {Colors.GREEN}{self.vm_passed:3d} passed{Colors.NC}  {Colors.RED}{self.vm_failed:3d} failed{Colors.NC}  {Colors.YELLOW}{self.vm_skipped:3d} skipped{Colors.NC}              {Colors.CYAN}║{Colors.NC}")
        print(f"{Colors.CYAN}║{Colors.NC}  JIT:   {Colors.GREEN}{self.jit_passed:3d} passed{Colors.NC}  {Colors.RED}{self.jit_failed:3d} failed{Colors.NC}  {Colors.YELLOW}{self.jit_skipped:3d} skipped{Colors.NC}              {Colors.CYAN}║{Colors.NC}")
        print(f"{Colors.CYAN}║{Colors.NC}  NOSTD: {Colors.GREEN}{self.nostd_passed:3d} passed{Colors.NC}  {Colors.RED}{self.nostd_failed:3d} failed{Colors.NC}  {Colors.YELLOW}{self.nostd_skipped:3d} skipped{Colors.NC}              {Colors.CYAN}║{Colors.NC}")
        print(f"{Colors.CYAN}╠══════════════════════════════════════════════════════════╣{Colors.NC}")
        print(f"{Colors.CYAN}║{Colors.NC}  Total: {Colors.GREEN}{total_passed:3d} passed{Colors.NC}  {Colors.RED}{total_failed:3d} failed{Colors.NC}                           {Colors.CYAN}║{Colors.NC}")
        print(f"{Colors.CYAN}╚══════════════════════════════════════════════════════════╝{Colors.NC}")

        if total_failed > 0:
            sys.exit(1)


# =============================================================================
# BENCHMARK RUNNER
# =============================================================================

class BenchmarkRunner:
    def __init__(self, vo_only: bool = False, all_langs: bool = False, arch: str = '64'):
        self.vo_only = vo_only
        self.all_langs = all_langs
        self.arch = arch
        self.vo_bin = get_vo_bin(release=True, arch=arch)

    def run(self, target: str):
        if target == 'score':
            self.calculate_scores()
            return

        self._check_deps()
        self._build_vo()

        if target == 'all' or target == 'vo':
            self._run_all_benchmarks()
            self.calculate_scores()
        elif self._benchmark_exists(target):
            self._run_benchmark(target)
        else:
            print(f"Unknown benchmark: {target}")
            self._list_benchmarks()
            sys.exit(1)

    def _check_deps(self):
        missing = []
        if not command_exists('hyperfine'):
            missing.append('hyperfine')
        if not self.vo_only and not command_exists('go'):
            missing.append('go')

        if missing:
            print(f"Missing dependencies: {', '.join(missing)}")
            print(f"Install with: brew install {' '.join(missing)}")
            sys.exit(1)

    def _build_vo(self):
        # Invalidate CLI cache if source files changed
        invalidate_cli_cache_if_needed()
        
        target_info = f" (target: {TARGET_32})" if self.arch == '32' else ""
        print(f"Building Vo (release){target_info}...")
        build_cmd = ['cargo', 'build', '--release', '-p', 'vo-launcher']
        if self.arch == '32':
            build_cmd.extend(['--target', TARGET_32, '--no-default-features'])
        code, _, stderr = run_cmd(build_cmd, capture=True)
        if code != 0:
            print(f"{Colors.RED}Build failed:{Colors.NC}\n{stderr}")
            sys.exit(1)
        
        # Build vo-vox extension (release mode for benchmarks)
        ensure_vox_extension_built(arch=self.arch, release=True)

    def _list_benchmarks(self):
        print("Available benchmarks:")
        for d in sorted(BENCHMARK_DIR.iterdir()):
            if d.is_dir() and d.name != 'results':
                print(f"  - {d.name}")

    def _benchmark_exists(self, name: str) -> bool:
        return (BENCHMARK_DIR / name).is_dir()

    def _run_all_benchmarks(self):
        for d in sorted(BENCHMARK_DIR.iterdir()):
            if d.is_dir() and d.name != 'results':
                self._run_benchmark(d.name)

    def _run_benchmark(self, name: str):
        bench_dir = BENCHMARK_DIR / name
        vo_bin = str(self.vo_bin)

        print(f"\n=== {name} ===\n")

        vo_file = next(bench_dir.glob('*.vo'), None)
        go_file = next(bench_dir.glob('*.go'), None)
        lua_file = next(bench_dir.glob('*.lua'), None)
        py_file = next(bench_dir.glob('*.py'), None)
        rb_file = next(bench_dir.glob('*.rb'), None)
        java_file = next(bench_dir.glob('*.java'), None)
        c_file = next(bench_dir.glob('*.c'), None)

        cmds = []
        names = []

        if vo_file:
            cmds.append(f"{vo_bin} --cache '{CLI_DIR}' run '{vo_file}'")
            names.append('Vo-VM')
            # JIT not supported on 32-bit
            if self.arch != '32':
                cmds.append(f"{vo_bin} --cache '{CLI_DIR}' run '{vo_file}' --mode=jit")
                names.append('Vo-JIT')

        if not self.vo_only:
            if go_file:
                go_bin = bench_dir / 'go_bench'
                code, _, _ = run_cmd(['go', 'build', '-o', str(go_bin), str(go_file)])
                if code == 0:
                    cmds.append(f"'{go_bin}'")
                    names.append('Go')

            if lua_file and command_exists('lua'):
                cmds.append(f"lua '{lua_file}'")
                names.append('Lua')

            if lua_file and command_exists('luajit'):
                cmds.append(f"luajit '{lua_file}'")
                names.append('LuaJIT')

            if py_file and self.all_langs:
                cmds.append(f"python3 '{py_file}'")
                names.append('Python')

            if rb_file and self.all_langs and command_exists('ruby'):
                cmds.append(f"ruby '{rb_file}'")
                names.append('Ruby')

            if java_file and command_exists('java') and command_exists('javac'):
                java_class = java_file.stem
                code, _, _ = run_cmd(['javac', '-d', str(bench_dir), str(java_file)])
                if code == 0:
                    cmds.append(f"java -cp '{bench_dir}' '{java_class}'")
                    names.append('Java')

        if c_file:
            c_bin = bench_dir / 'c_bench'
            for compiler in ['cc', 'gcc', 'clang']:
                if command_exists(compiler):
                    code, _, _ = run_cmd([compiler, '-O3', '-o', str(c_bin), str(c_file)])
                    if code == 0:
                        cmds.append(f"'{c_bin}'")
                        names.append('C')
                    break

        if not cmds:
            print("No runnable benchmarks found")
            return

        RESULTS_DIR.mkdir(parents=True, exist_ok=True)
        export_json = RESULTS_DIR / f"{name}.json"
        export_md = RESULTS_DIR / f"{name}.md"

        hf_cmd = ['hyperfine', '--warmup', '1', '--runs', '3']
        for cmd, n in zip(cmds, names):
            hf_cmd.extend(['-n', n, cmd])
        hf_cmd.extend(['--export-json', str(export_json), '--export-markdown', str(export_md)])

        subprocess.run(hf_cmd)

    def calculate_scores(self):
        print(f"\n=== Calculating Scores ===\n")

        json_files = list(RESULTS_DIR.glob('*.json'))
        if not json_files:
            print("No results found. Run benchmarks first.")
            return

        scores = defaultdict(list)

        for file in json_files:
            benchmark_name = file.stem
            print(f"Processing: {benchmark_name}")

            with open(file) as f:
                data = json.load(f)

            if 'results' not in data:
                continue

            mean_times = {}
            for result in data['results']:
                name = result.get('command', '')
                mean = result.get('mean')
                if name in ('Vo-VM', 'Vo-JIT', 'Go', 'Lua', 'LuaJIT', 'Python', 'Ruby', 'Java', 'C'):
                    if mean and mean > 0:
                        mean_times[name] = mean

            if not mean_times:
                continue

            if self.vo_only and 'Vo-VM' in mean_times:
                baseline = mean_times['Vo-VM']
                for lang, mean in mean_times.items():
                    score = (mean / baseline) * 100
                    scores[lang].append(score)
                    print(f"  {lang}: {score:.1f} (mean: {mean:.4f}s)")
            else:
                baseline = min(mean_times.values())
                for lang, mean in mean_times.items():
                    relative = mean / baseline
                    scores[lang].append(relative)
                    print(f"  {lang}: {relative:.2f}x (mean: {mean:.4f}s)")

        if not scores:
            print("\nNo valid results to analyze.")
            return

        averages = {}
        for lang, values in scores.items():
            if values:
                averages[lang] = sum(values) / len(values)

        if self.vo_only:
            sorted_langs = sorted(averages.items(), key=lambda x: x[1])
            print(f"\n{'=' * 60}")
            print("Language Performance (Vo-VM = 100, lower is faster):")
            print('=' * 60)
            for i, (lang, score) in enumerate(sorted_langs, 1):
                marker = " ← baseline" if lang == 'Vo-VM' else ""
                print(f"{i:2d}. {lang:<8}: {score:>7.1f}{marker}")
        else:
            sorted_langs = sorted(averages.items(), key=lambda x: x[1])
            print(f"\n{'=' * 60}")
            print("Language Performance Ranking (lower relative time is better):")
            print('=' * 60)
            for i, (lang, score) in enumerate(sorted_langs, 1):
                print(f"{i:2d}. {lang:<8}: {score:.2f}x")

        self._print_detailed_table(json_files, sorted_langs)

    def _print_detailed_table(self, json_files: list, sorted_langs: list):
        print(f"\n{'=' * 80}")
        print("Detailed relative times by benchmark:")
        print('=' * 80)

        benchmarks = sorted(f.stem for f in json_files)
        lang_order = [lang for lang, _ in sorted_langs]

        header = f"{'Benchmark':<15}"
        for lang in lang_order:
            header += f" {lang:>8}"
        print(header)
        print('-' * 80)

        for benchmark in benchmarks:
            with open(RESULTS_DIR / f"{benchmark}.json") as f:
                data = json.load(f)

            if 'results' not in data:
                continue

            mean_times = {}
            for result in data['results']:
                name = result.get('command', '')
                mean = result.get('mean')
                if name in lang_order and mean and mean > 0:
                    mean_times[name] = mean

            if not mean_times:
                continue

            if self.vo_only and 'Vo-VM' in mean_times:
                baseline = mean_times['Vo-VM']
            else:
                baseline = min(mean_times.values())

            row = f"{benchmark:<15}"
            for lang in lang_order:
                if lang in mean_times:
                    if self.vo_only:
                        val = (mean_times[lang] / baseline) * 100
                        row += f" {val:>8.1f}"
                    else:
                        val = mean_times[lang] / baseline
                        row += f" {val:>8.2f}"
                else:
                    row += f" {'-':>8}"
            print(row)

        print()
        if self.vo_only:
            print("Score = (lang time / Vo-VM time) × 100. Lower = faster than Vo-VM.")
        else:
            print("Lower = faster. 1.0 = fastest in that benchmark.")


# =============================================================================
# LOC STATS
# =============================================================================

class LocStats:
    CRATES_DIR = PROJECT_ROOT / 'lang' / 'crates'

    CATEGORIES = {
        'Frontend (syntax)': ['vo-syntax'],
        'Common': ['vo-common', 'vo-common-core'],
        'Analysis (type checker)': ['vo-analysis'],
        'Code Generation': ['vo-codegen'],
        'Runtime': ['vo-vm', 'vo-runtime'],
        'JIT': ['vo-jit'],
        'Tools (CLI/Module)': ['vo-launcher', 'vo-module'],
    }

    def __init__(self, with_tests: bool = False):
        self.with_tests = with_tests

    def run(self):
        print(f"{Colors.BOLD}═══════════════════════════════════════════════════════════════{Colors.NC}")
        print(f"{Colors.BOLD}                    Vo Project Statistics                      {Colors.NC}")
        print(f"{Colors.BOLD}═══════════════════════════════════════════════════════════════{Colors.NC}")
        print()

        crate_stats = {}
        total_lines = 0
        total_files = 0

        print(f"{Colors.CYAN}{'Crate':<30} │ {'Files':>7} │ {'Lines':>9} │ {'Avg/File':>8}{Colors.NC}")
        print("───────────────────────────────┼─────────┼───────────┼──────────")

        for crate_dir in sorted(self.CRATES_DIR.iterdir()):
            src_dir = crate_dir / 'src'
            if not src_dir.is_dir():
                continue

            crate_name = crate_dir.name

            if not self.with_tests and crate_name == 'vo-tests':
                continue

            lines, files = self._count_lines(src_dir)
            avg = lines // files if files > 0 else 0

            crate_stats[crate_name] = {'lines': lines, 'files': files}
            total_lines += lines
            total_files += files

            print(f"{crate_name:<30} │ {files:>7} │ {lines:>9} │ {avg:>8}")

        print("───────────────────────────────┼─────────┼───────────┼──────────")
        total_avg = total_lines // total_files if total_files > 0 else 0
        print(f"{Colors.BOLD}{'TOTAL':<30} │ {total_files:>7} │ {total_lines:>9} │ {total_avg:>8}{Colors.NC}")
        print()

        if not self.with_tests:
            self._print_test_stats()

        self._print_categories(crate_stats)

        print(f"{Colors.BLUE}Generated at: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}{Colors.NC}")
        if not self.with_tests:
            print(f"{Colors.BLUE}Note: Run with --with-tests to include test files{Colors.NC}")

    def _count_lines(self, src_dir: Path) -> tuple[int, int]:
        total_lines = 0
        total_files = 0

        for rs_file in src_dir.rglob('*.rs'):
            if not self.with_tests and rs_file.name.endswith('_test.rs'):
                continue
            total_files += 1
            total_lines += sum(1 for _ in open(rs_file, encoding='utf-8', errors='ignore'))

        return total_lines, total_files

    def _print_test_stats(self):
        print(f"{Colors.YELLOW}Test Statistics (excluded from above):{Colors.NC}")

        test_data_dir = PROJECT_ROOT / 'lang' / 'test_data'
        if test_data_dir.exists():
            vo_files = list(test_data_dir.rglob('*.vo'))
            vo_lines = sum(sum(1 for _ in open(f, encoding='utf-8', errors='ignore')) for f in vo_files)
            print(f"  Test data files (.vo):     {len(vo_files)} files, {vo_lines} lines")

        vo_tests_src = self.CRATES_DIR / 'vo-tests' / 'src'
        if vo_tests_src.exists():
            lines, _ = self._count_lines(vo_tests_src)
            print(f"  Test runner (Rust):         {lines} lines")

        print()

    def _print_categories(self, crate_stats: dict):
        print(f"{Colors.GREEN}Category Breakdown:{Colors.NC}")
        print("───────────────────────────────────────")

        for category, crates in self.CATEGORIES.items():
            lines = sum(crate_stats.get(c, {}).get('lines', 0) for c in crates)
            print(f"  {category:<25} {lines:>8} lines")

        print()


# =============================================================================
# CLI
# =============================================================================

def main():
    parser = argparse.ArgumentParser(
        prog='d.py',
        description='Vo Development Tool - test/bench/loc'
    )
    subparsers = parser.add_subparsers(dest='command', help='Commands')

    # test
    test_parser = subparsers.add_parser('test', help='Run integration tests')
    test_parser.add_argument('mode', nargs='?', default='both',
                             help='vm, jit, gc, nostd, wasm, or both (default)')
    test_parser.add_argument('-v', '--verbose', action='store_true',
                             help='Verbose output')
    test_parser.add_argument('file', nargs='?', help='Single test file')
    test_parser.add_argument('--arch', choices=['32', '64'], default='64',
                             help='Target architecture (default: 64)')

    # bench
    bench_parser = subparsers.add_parser('bench', help='Run benchmarks')
    bench_parser.add_argument('target', nargs='?', default='all',
                              help='all, vo, score, or benchmark name')
    bench_parser.add_argument('--all-langs', action='store_true',
                              help='Include Python and Ruby')
    bench_parser.add_argument('--arch', choices=['32', '64'], default='64',
                              help='Target architecture (default: 64)')

    # loc
    loc_parser = subparsers.add_parser('loc', help='Code statistics')
    loc_parser.add_argument('--with-tests', action='store_true',
                            help='Include test files')

    # clean
    clean_parser = subparsers.add_parser('clean', help='Clean caches')
    clean_parser.add_argument('target', nargs='?', default='all',
                              choices=['all', 'vo', 'rust'],
                              help='all (default), vo (.vo-cache), rust (cargo clean)')

    # wasm
    wasm_parser = subparsers.add_parser('wasm', help='Build vo-web for WASM')
    wasm_parser.add_argument('--release', action='store_true',
                             help='Build in release mode')

    args = parser.parse_args()

    if args.command == 'test':
        file_arg = None
        mode = args.mode

        if args.file:
            file_arg = args.file
        elif args.mode and (args.mode.endswith('.vo') or '/' in args.mode):
            file_arg = args.mode
            mode = 'both'

        runner = TestRunner(verbose=args.verbose, arch=args.arch)
        runner.run(mode, single_file=file_arg)

    elif args.command == 'bench':
        vo_only = args.target == 'vo'
        runner = BenchmarkRunner(vo_only=vo_only, all_langs=args.all_langs, arch=args.arch)
        runner.run(args.target)

    elif args.command == 'loc':
        stats = LocStats(with_tests=args.with_tests)
        stats.run()

    elif args.command == 'clean':
        clean_caches(args.target)

    elif args.command == 'wasm':
        build_wasm(release=args.release)

    else:
        parser.print_help()


if __name__ == '__main__':
    main()
