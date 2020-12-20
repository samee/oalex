#!/usr/bin/env python3

import argparse
import json
import os
import re
import subprocess
import sys
from typing import List

parser = argparse.ArgumentParser()
parser.add_argument("-b", "--bin", help="Path to the oalex binary")
parser.add_argument("-t", "--testdata", help="Path to directory of testdata")
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

# Dummy input test case
result = subprocess.run(
           [sysargs.bin, os.path.join(sysargs.testdata, "1-good.oalex")],
           input=b"Hello!\n", capture_output=True)
assert result.returncode == 0, "1-good.oalex exited with non-zero result"

result = subprocess.run(
           [sysargs.bin, os.path.join(sysargs.testdata, "1-bad.oalex")],
           capture_output=True)
assert result.returncode != 0, "1-bad.oalex was expected to cause failure"
assert re.search("Doesn't insist on politeness", result.stderr.decode('utf-8'))
