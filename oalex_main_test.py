#!/usr/bin/env python3

import argparse
import re
import subprocess
import sys
from typing import List

parser = argparse.ArgumentParser()
parser.add_argument("-b", "--bin", help="Path to the oalex binary")
sysargs = parser.parse_args()

def split_args(s: str) -> List[str]:
  rv = s.split()
  assert rv, "Test command line is empty"
  assert re.match(rv[0], r"oalex\b"), \
      "Test command-line doesn't start with oalex"
  rv[0] = sysargs.bin
  return rv

# The "oalex" prefix in each of these cases will be replaced
# with args.bin in split_args().
errorcases = [
    ("oalex", "Input filename missing"),
    ("oalex build --h-out test.h", "Input filename missing"),
    ("oalex build --h-out test.h test.oalex", "Both .* or neither"),
    ("oalex build test --h-out test.h", "Input filename missing"),
    ("oalex build test --h-out test.h test.oalex", "All .* or none"),
]

for cmd, error_re in errorcases:
  result = subprocess.run(split_args(cmd), capture_output=True)
  if not re.search(error_re, result.stderr.decode('utf-8')):
    print(f"Couldn't find the expected error for command: {cmd}")
    print(f"Expected '{error_re}'\nGot {result.stderr!r}")
    sys.exit(1)
