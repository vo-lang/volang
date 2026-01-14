#!/usr/bin/env python3
import os
import sys
from pathlib import Path

root = Path(__file__).resolve().parent
script = root / "lang" / "d.py"
os.execv(sys.executable, [sys.executable, str(script), *sys.argv[1:]])
