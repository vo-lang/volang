#!/usr/bin/env python3
"""
Vo Development Tool - Wrapper for vo-test and other utilities

Usage:
    ./d.py test [vm|jit|osr|both|gc|nostd|wasm] [-v] [--jobs N] [--repeat N|-n N] [file]
    ./d.py bench [all|vo|<name>|score] [--all-langs]
    ./d.py loc [--with-tests]
    ./d.py clean [all|vo|rust]
    ./d.py play [--build-only]
    ./d.py studio [--build-wasm] [--build-only]
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
USE_RELEASE = '--release' in sys.argv
if USE_RELEASE:
    sys.argv.remove('--release')
_PROFILE_DIR = 'release' if USE_RELEASE else 'debug'
VO_TEST_BIN = PROJECT_ROOT / 'target' / _PROFILE_DIR / 'vo-test'
VO_BIN = PROJECT_ROOT / 'target' / _PROFILE_DIR / 'vo'


def parse_test_repeat_args(args: list[str]) -> tuple[list[str], int]:
    """Extract --repeat / -n from test args and return (forward_args, repeat)."""
    repeat = 1
    forward_args: list[str] = []
    i = 0
    while i < len(args):
        arg = args[i]
        if arg in ('--repeat', '-n'):
            if i + 1 >= len(args):
                print(f"Error: {arg} requires a positive integer", file=sys.stderr)
                sys.exit(2)
            raw = args[i + 1]
            i += 2
        elif arg.startswith('--repeat='):
            raw = arg.split('=', 1)[1]
            i += 1
        else:
            forward_args.append(arg)
            i += 1
            continue

        try:
            parsed = int(raw)
        except ValueError:
            print(f"Error: invalid repeat count: {raw}", file=sys.stderr)
            sys.exit(2)

        if parsed <= 0:
            print(f"Error: repeat count must be > 0, got {parsed}", file=sys.stderr)
            sys.exit(2)

        repeat = parsed

    return forward_args, repeat


def run_repeated(cmd: list[str], *, repeat: int, label: str) -> int:
    """Run command repeatedly, stop at first failure."""
    for i in range(1, repeat + 1):
        if repeat > 1:
            print(f"[d.py] {label} run {i}/{repeat}")
        result = subprocess.run(cmd, cwd=PROJECT_ROOT)
        if result.returncode != 0:
            if repeat > 1:
                print(
                    f"[d.py] {label} failed at run {i}/{repeat} (exit {result.returncode})",
                    file=sys.stderr,
                )
            return result.returncode
    return 0



def ensure_vo_test_built(need_embed=False):
    """Build vo-test if it doesn't exist or is outdated."""
    packages = ['vo-test', 'vo-vox']
    if need_embed:
        packages.append('vo-embed')
    
    cmd = ['cargo', 'build']
    if USE_RELEASE:
        cmd.append('--release')
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
    cmd = ['cargo', 'build', '-p', 'vo']
    if USE_RELEASE:
        cmd.append('--release')
    result = subprocess.run(
        cmd,
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
    return run_repeated(cmd, repeat=1, label='test')


def run_vo_cli(args, jit_mode=False):
    ensure_vo_cli_built()
    cmd = [str(VO_BIN)] + args
    env = None
    if jit_mode:
        import os
        env = os.environ.copy()
        env['VO_JIT_CALL_THRESHOLD'] = '1'
    result = subprocess.run(cmd, cwd=PROJECT_ROOT, env=env)
    return result.returncode


def run_other_command(command, args):
    """Run other commands via d_py.py."""
    d_py = PROJECT_ROOT / 'd_py.py'
    if not d_py.exists():
        print(f"Error: {d_py} not found", file=sys.stderr)
        sys.exit(1)
    
    cmd = [str(d_py), command] + args
    return run_repeated(cmd, repeat=1, label=command)


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)
    
    command = sys.argv[1]
    rest_args = sys.argv[2:]
    
    if command == 'test':
        rest_args, repeat = parse_test_repeat_args(rest_args)

        # wasm tests use d_py.py (node-based test runner)
        if rest_args and rest_args[0] == 'wasm':
            cmd = [str(PROJECT_ROOT / 'd_py.py'), command] + rest_args
            sys.exit(run_repeated(cmd, repeat=repeat, label='test'))
        else:
            # 'embed' is alias for 'nostd'
            test_args = rest_args
            if test_args and test_args[0] == 'embed':
                test_args = ['nostd'] + test_args[1:]

            need_embed = 'nostd' in test_args
            ensure_vo_test_built(need_embed=need_embed)

            cmd = [str(VO_TEST_BIN)] + test_args
            sys.exit(run_repeated(cmd, repeat=repeat, label='test'))
    elif command == 'run':
        jit_mode = '--mode=jit' in rest_args or '-mode=jit' in rest_args
        sys.exit(run_vo_cli(['run'] + rest_args, jit_mode=jit_mode))
    elif command == 'vo':
        sys.exit(run_vo_cli(rest_args))
    elif command in ('bench', 'loc', 'clean', 'play', 'studio'):
        sys.exit(run_other_command(command, rest_args))
    elif command in ('-h', '--help', 'help'):
        print(__doc__)
        sys.exit(0)
    else:
        # Default: forward to vo CLI subcommand.
        # Examples:
        #   ./d.py version
        #   ./d.py build <path>
        #   ./d.py check <path>
        sys.exit(run_vo_cli([command] + rest_args))


if __name__ == '__main__':
    main()
