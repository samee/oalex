#!/usr/bin/env python3

# Copyright 2020-2021 The oalex authors.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
from typing import List

parser = argparse.ArgumentParser()
parser.add_argument("-b", "--bin", help="Path to the oalex binary")
parser.add_argument("-t", "--testdata", help="Path to directory of testdata")
parser.add_argument("--source-path", help="Path to source tree")
parser.add_argument("--build-path", help="Path to build tree")
sysargs = parser.parse_args()

def split_args(s: str) -> List[str]:
  rv = s.split()
  assert rv, "Test command line is empty"
  assert re.match(rv[0], r"oalex\b"), \
      "Test command-line doesn't start with oalex"
  rv[0] = sysargs.bin
  return rv

def test_malformed_cmdline_errors():
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

def eval_testdata_files():
  testfiles = []
  with os.scandir(sysargs.testdata) as dir_entries:
    for entry in dir_entries:
      if entry.name.endswith('.oalex') and entry.is_file():
        testfiles.append(entry.name)

  for filename in testfiles:
    expected_error = find_expected_error(filename)
    if expected_error:
      result = subprocess.run(
                 [ sysargs.bin, "test",
                   os.path.join(sysargs.testdata, filename)
                 ], input="", capture_output=True)
      assert result.returncode != 0, f"{filename} was expected to cause failure"
      observed_error = result.stderr.decode('utf-8')
      assert expected_error in observed_error, \
             f"Was expecting '{expected_error}' in '{filename}'\n" + \
             ("Got this instead:\n" + observed_error if observed_error else "")
    else:
      result = subprocess.run(
                 [ sysargs.bin, "test",
                   os.path.join(sysargs.testdata, filename)
                 ], capture_output=True)
      if result.returncode != 0:
        print(f"`oalex test` failed on {filename}. Error output:")
        print(result.stderr.decode("utf-8"))
        sys.exit(1)

def test_gen_compiles():
  # Right now I'm only using a single test file, since I'm a little worried
  # about increasing unit-test runtime. Running g++ multiple times can be a
  # problem.
  testfiles = ["concat-good-1.oalex"]
  with tempfile.TemporaryDirectory() as tempdir:
    for filename in testfiles:
      infile = os.path.join(sysargs.testdata, filename)
      cppfile = os.path.join(tempdir, filename + ".cpp")
      hfile = os.path.join(tempdir, filename + ".h")
      result = subprocess.run([sysargs.bin, "build", "--cpp-out", cppfile,
                               "--h-out", hfile, infile])
      assert result.returncode == 0, \
             f"Error generating parser file from '{filename}'"
      # TODO honor build-time compiler selection
      result = subprocess.run([
        "g++", "-c", "--std=c++17", cppfile,
        "-I", os.path.join(sysargs.source_path, "runtime"),
        "-I", os.path.join(sysargs.build_path, "_deps/fmt-src/include"),
      ])
      assert result.returncode == 0, \
             f"Error compiling outputs from '{filename}'"


test_malformed_cmdline_errors()
eval_testdata_files()
test_gen_compiles()
