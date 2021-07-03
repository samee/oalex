#!/usr/bin/env python3
# Copyright 2021 The oalex authors.
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

# timecomp.py: print compilation time for each file. It wraps calls to `g++`
# with timer calls. It annotates output lines with the paramter to `-o` flag.
# If that flag isn't found, it produces no output.
#
# Usage:
#   mkdir build
#   cd build
#   cmake -DCMAKE_CXX_COMPILER=$PWD/../timecomp.py ..
#   make all 2> timecomp.log
#   sort -rnk3 timecomp.log

import os
import sys
import time

def outfileFromCmdLine(cmdline):
  try:
    i = cmdline.index('-o')
    if len(cmdline) <= i: return None
    else: return cmdline[i+1]
  except ValueError: return None

outfile = outfileFromCmdLine(sys.argv)

cmd = ' '.join(['g++'] + sys.argv[1:])
start = time.time()
os.system(cmd)
end = time.time()

if outfile is not None:
  print(f"Wall time: {end-start:>6.3f}s {outfile}",
        file=sys.stderr, flush=True)
