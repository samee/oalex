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

def find_expected_error(filename: str) -> str:
  directive = "# expected-error:"
  with open(os.path.join(sysargs.testdata, filename), mode='r') as f:
    for line in f:
      if line.startswith(directive):
        return line[len(directive):].strip()
  return None

testfiles = []
with os.scandir(sysargs.testdata) as dir_entries:
  for entry in dir_entries:
    if entry.name.endswith('.oalex') and entry.is_file():
      testfiles.append(entry.name)

for filename in testfiles:
  expected_error = find_expected_error(filename)
  if expected_error:
    result = subprocess.run(
               [sysargs.bin, "test", os.path.join(sysargs.testdata, filename)],
               input="", capture_output=True)
    assert result.returncode != 0, f"{filename} was expected to cause failure"
    observed_error = result.stderr.decode('utf-8')
    assert expected_error in observed_error, \
           f"Was expecting '{expected_error}' in '{filename}'\n" + \
           ("Got this instead:\n" + observed_error if observed_error else "")
  else:
    result = subprocess.run(
               [sysargs.bin, "test", os.path.join(sysargs.testdata, filename)],
               capture_output=True)
    if result.returncode != 0:
      print(f"`oalex test` failed on {filename}. Error output:")
      print(result.stderr.decode("utf-8"))
      sys.exit(1)
