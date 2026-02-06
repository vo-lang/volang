#!/usr/bin/env python3
"""
Vo Development Tool - Wrapper for vo-test and other utilities

Usage:
    ./d.py test [vm|jit|osr|both|gc|nostd|wasm] [-v] [--jobs N] [file]
    ./d.py bench [all|vo|<name>|score] [--all-langs]
    ./d.py loc [--with-tests]
    ./d.py clean [all|vo|rust]
    ./d.py play [--build-only]
    ./d.py run <file.vo> [--mode=vm|jit] [--codegen]
    ./d.py vo <args...>
"""

import subprocess
import sys
from pathlib import Path

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



def ensure_vo_test_built(need_embed=False):
    """Build vo-test if it doesn't exist or is outdated."""
    packages = ['vo-test', 'vo-vox']
    if need_embed:
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


def run_test(args):
    """Run tests using vo-test."""
    # 'embed' is alias for 'nostd'
    if args and args[0] == 'embed':
        args = ['nostd'] + args[1:]
    
    need_embed = 'nostd' in args
    ensure_vo_test_built(need_embed=need_embed)
    
    # Forward all arguments to vo-test
    cmd = [str(VO_TEST_BIN)] + args
    result = subprocess.run(cmd, cwd=PROJECT_ROOT)
    sys.exit(result.returncode)


def run_vo_cli(args, jit_mode=False):
    ensure_vo_cli_built()
    cmd = [str(VO_BIN)] + args
    env = None
    if jit_mode:
        import os
        env = os.environ.copy()
        env['VO_JIT_CALL_THRESHOLD'] = '1'
    result = subprocess.run(cmd, cwd=PROJECT_ROOT, env=env)
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
        jit_mode = '--mode=jit' in rest_args or '-mode=jit' in rest_args
        run_vo_cli(['run'] + rest_args, jit_mode=jit_mode)
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
