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
output = json.loads(result.stdout.decode('utf-8'))
expected = json.loads('{"msg": "Hello!"}')
assert result.returncode == 0, "1-good.oalex exited with non-zero result"

result = subprocess.run(
           [sysargs.bin, os.path.join(sysargs.testdata, "1-good.oalex")],
           input=b"Go away!\n", capture_output=True)
if "Failed at politeness" not in result.stderr.decode('utf-8'):
  print("Failed to detect rudeness in 1-good.oalex")
  sys.exit(1)

errorcases = [
    ("1-bad.oalex", "was expecting require_politeness"),
    ("2-bad.oalex", "Doesn't insist on politeness"),
    ("3-bad.oalex", "Was expecting eof"),
]
for filename, error_str in errorcases:
  result = subprocess.run(
             [sysargs.bin, os.path.join(sysargs.testdata, filename)],
             capture_output=True)
  assert result.returncode != 0, f"{filename} was expected to cause failure"
  assert error_str in result.stderr.decode('utf-8')
