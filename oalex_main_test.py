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


def find_expected_error(filename: str) -> str:
  directive = "# Expected syntax error:"
  with open(os.path.join(sysargs.testdata, filename), mode='r') as f:
    for line in f:
      if line.startswith(directive):
        return line[len(directive):].strip()
  raise SyntaxError(f"Syntax error directive not found in {str}")

# TODO merge good and bad tests when we have examples working.
# TODO traverse directory to find files.
errorcases = [ "1-bad.oalex", "2-bad.oalex", "3-bad.oalex", "4-bad.oalex",
               "5-bad.oalex", "6-bad.oalex", "7-bad.oalex" ]

for filename in errorcases:
  expected_error = find_expected_error(filename)
  result = subprocess.run(
             [sysargs.bin, os.path.join(sysargs.testdata, filename)],
             input="", capture_output=True)
  assert result.returncode != 0, f"{filename} was expected to cause failure"
  observed_error = result.stderr.decode('utf-8')
  assert expected_error in observed_error, \
         f"Was expecting '{expected_error}' in '{filename}'\n" + \
         ("Got this instead:\n" + observed_error if observed_error else "")
