#!/usr/bin/env python3
"""
Vo Development Tool - Wrapper for vo-test and other utilities

Usage:
    ./d.py test [vm|jit|both|gc|nostd|wasm] [-v] [--jobs N] [file]
    ./d.py bench [all|vo|<name>|score] [--all-langs]
    ./d.py loc [--with-tests]
    ./d.py clean [all|vo|rust]
    ./d.py play [--build-only]
    ./d.py run <file.vo> [--mode=vm|jit] [--codegen]
    ./d.py vo <args...>
"""

import argparse
import subprocess
import sys
from pathlib import Path
import shutil

# Check that we're running from the correct directory (repo root)
_script_dir = Path(__file__).resolve().parent
_expected_cwd = _script_dir  # repo root
if Path.cwd().resolve() != _expected_cwd:
    print(f"Error: d.py must be run from repo root: {_expected_cwd}", file=sys.stderr)
    print(f"Current directory: {Path.cwd()}", file=sys.stderr)
    print(f"Run: cd {_expected_cwd} && ./d.py ...", file=sys.stderr)
    sys.exit(1)

PROJECT_ROOT = Path(__file__).parent.resolve()
VO_TEST_BIN = PROJECT_ROOT / 'target' / 'debug' / 'vo-test'
VO_BIN = PROJECT_ROOT / 'target' / 'debug' / 'vo'
CLI_DIR = PROJECT_ROOT / 'cmd' / 'vo'
CLI_CACHE_DIR = CLI_DIR / '.vo-cache'
VOX_EXT_DIR = PROJECT_ROOT / 'libs' / 'vox'
VOX_EXT_RUST_DIR = VOX_EXT_DIR / 'rust' / 'src'


def get_newest_mtime(*paths: Path, pattern: str = '*') -> float:
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
    if not CLI_CACHE_DIR.exists():
        return

    cache_mtime = CLI_CACHE_DIR.stat().st_mtime
    cli_src_mtime = get_newest_mtime(CLI_DIR, pattern='*.vo')
    vox_vo_mtime = get_newest_mtime(VOX_EXT_DIR, pattern='*.vo')
    vox_rust_mtime = get_newest_mtime(VOX_EXT_RUST_DIR, pattern='*.rs')
    vox_lib = PROJECT_ROOT / 'target' / 'debug' / 'libvo_vox.so'
    vox_lib_mtime = vox_lib.stat().st_mtime if vox_lib.exists() else 0.0

    newest_src = max(cli_src_mtime, vox_vo_mtime, vox_rust_mtime, vox_lib_mtime)
    if newest_src > cache_mtime:
        shutil.rmtree(CLI_CACHE_DIR)


def ensure_vo_test_built(nostd=False):
    """Build vo-test if it doesn't exist or is outdated."""
    packages = ['vo-test', 'vo-vox']
    if nostd:
        packages.append('vo-embed')
    
    cmd = ['cargo', 'build']
    for pkg in packages:
        cmd.extend(['-p', pkg])
    
    result = subprocess.run(
        cmd,
        cwd=PROJECT_ROOT,
        capture_output=True,
        text=True
    )
    if result.returncode != 0:
        print("Failed to build vo-test:", file=sys.stderr)
        print(result.stderr, file=sys.stderr)
        sys.exit(1)


def ensure_vo_cli_built():
    result = subprocess.run(
        ['cargo', 'build', '-p', 'vo'],
        cwd=PROJECT_ROOT,
        capture_output=True,
        text=True
    )
    if result.returncode != 0:
        print("Failed to build vo:", file=sys.stderr)
        print(result.stderr, file=sys.stderr)
        sys.exit(1)

    result = subprocess.run(
        ['cargo', 'build', '-p', 'vo-vox', '--features', 'ffi'],
        cwd=PROJECT_ROOT,
        capture_output=True,
        text=True
    )
    if result.returncode != 0:
        print("Failed to build vo-vox:", file=sys.stderr)
        print(result.stderr, file=sys.stderr)
        sys.exit(1)


def run_test(args):
    """Run tests using vo-test."""
    nostd = 'nostd' in args
    ensure_vo_test_built(nostd=nostd)
    
    # Forward all arguments to vo-test
    cmd = [str(VO_TEST_BIN)] + args
    result = subprocess.run(cmd, cwd=PROJECT_ROOT)
    sys.exit(result.returncode)


def run_vo_cli(args):
    ensure_vo_cli_built()
    invalidate_cli_cache_if_needed()
    cmd = [str(VO_BIN), '--cache', str(CLI_DIR)] + args
    result = subprocess.run(cmd, cwd=PROJECT_ROOT)
    sys.exit(result.returncode)


def run_other_command(command, args):
    """Run other commands via d_py.py."""
    d_py = PROJECT_ROOT / 'd_py.py'
    if not d_py.exists():
        print(f"Error: {d_py} not found", file=sys.stderr)
        sys.exit(1)
    
    cmd = [str(d_py), command] + args
    result = subprocess.run(cmd, cwd=PROJECT_ROOT)
    sys.exit(result.returncode)


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)
    
    command = sys.argv[1]
    rest_args = sys.argv[2:]
    
    if command == 'test':
        # wasm tests use d_py.py (node-based test runner)
        if rest_args and rest_args[0] == 'wasm':
            run_other_command(command, rest_args)
        else:
            run_test(rest_args)
    elif command == 'run':
        run_vo_cli(['run'] + rest_args)
    elif command == 'vo':
        run_vo_cli(rest_args)
    elif command in ('bench', 'loc', 'clean', 'play'):
        run_other_command(command, rest_args)
    elif command in ('-h', '--help', 'help'):
        print(__doc__)
        sys.exit(0)
    else:
        # Default: forward to vo CLI subcommand.
        # Examples:
        #   ./d.py version
        #   ./d.py build <path>
        #   ./d.py check <path>
        run_vo_cli([command] + rest_args)


if __name__ == '__main__':
    main()
