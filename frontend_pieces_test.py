#!/usr/bin/env python3

# Copyright 2019-2024 The oalex authors.
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
import os
import subprocess
import sys
import tempfile

parser = argparse.ArgumentParser()
parser.add_argument("-b", "--bin", help="Path to the oalex binary")
parser.add_argument("--source-path", help="Path to source tree")
parser.add_argument("--build-path", help="Path to build tree")
sysargs = parser.parse_args()

def src_file(base, ext):
  return os.path.join(sysargs.source_path, f"{base}.{ext}")

def test_bootstrapping_source():
  src = "frontend_pieces"
  src_path = src_file(src, "oalex")
  subprocess.check_call([sysargs.bin, "test", src_path])

def files_different(filename1, filename2):
  with open(filename1, encoding="utf-8") as f1:
    with open(filename2, encoding="utf-8") as f2:
      return f1.read() != f2.read()

def compare_bootstrapped_files():
  src = "frontend_pieces"
  some_error = False
  with tempfile.TemporaryDirectory() as tmpdir:
    src_path = os.path.abspath(src_file(src, "oalex"))
    bin_path = os.path.abspath(sysargs.bin)
    subprocess.check_call([
      bin_path, "build", src_path,
      "--cpp-out", src+".cpp", "--h-out", src+".h"], cwd=tmpdir)
    tmp_cpp_path = os.path.join(tmpdir, src+".cpp")
    tmp_h_path = os.path.join(tmpdir, src+".h")
    src_cpp_path = os.path.normpath(src_file(src, "cpp"))
    src_h_path = os.path.normpath(src_file(src, "h"))
    if (files_different(tmp_cpp_path, src_cpp_path) or
        files_different(tmp_h_path, src_h_path)):
         bin_relpath = os.path.relpath(bin_path, start = sysargs.source_path)
         newcmd = (f"{bin_relpath} build '{src}.oalex'")
         some_error = True
         print("Bootstrapped files are stale. "
                + f"Please rerun from source root: {newcmd}",
               file=sys.stderr)
  if some_error: sys.exit(1)

test_bootstrapping_source()
compare_bootstrapped_files()
