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

# Usage:
#  ./depdot.py > deps.dot
#  dot -T png -o deps.png deps.dot

import os

def list_source_files():
  source_files = []
  for relpath in ['.', 'runtime']:
    path = os.path.join(os.path.dirname(__file__), relpath)
    with os.scandir(path) as dir_entries:
      for entry in dir_entries:
        if not entry.is_file(): continue
        if entry.name.endswith('.cpp') or entry.name.endswith('.h'):
          source_files.append(
              os.path.normpath(os.path.join(relpath,entry.name)))
  return source_files

def extract_include(line):
  f = line.removeprefix('#include').strip()
  enpos = f.find('"', 1)
  if f.startswith('"') and enpos != -1:
    return f[1:enpos]
  enpos = f.find('>', 1)
  if f.startswith('<') and enpos != -1:
    return f[:enpos+1]
  raise Exception('Could not parse include line: ' + line)

def gather_includes(filenames):
  incmap = {}
  for filename in filenames:
    with open(filename, 'r') as f:
      incmap[filename] = [extract_include(line)
                           for line in f if line.startswith('#include')]
  return incmap

def system_include(incpath):
  return incpath.startswith('<')

# This function re-adds "runtime/" or similar prefixes to include directories
# that are often omitted in source. It tries to emulate the compiler's file
# path search order, so it's necessarily approximate. Will need ongoing
# tweaking.
def resolve_relpath(filename, cand_dir, incmap):
  if system_include(filename) or filename in incmap:
    return filename

  # Special-case because frontend.h decided to be weird.
  # Might fix these someday.
  if filename == 'oalex.h': return 'runtime/oalex.h'

  cand = os.path.join(cand_dir, filename)
  return cand if cand in incmap else filename

def resolve_relpaths(incmap):
  rv = {}
  for source in incmap.keys():
    d = os.path.dirname(source)
    rv[source] = [resolve_relpath(incpath, d, incmap)
                    for incpath in incmap[source]]
  return rv

incmap = gather_includes(list_source_files())
incmap = resolve_relpaths(incmap)

print('digraph {')
print('  ratio="0.5";')
for s in incmap.keys():
  if s.endswith('.cpp'): continue
  commalist = ", ".join(f'"{inc}"' for inc in incmap[s]
                                   if not system_include(inc))
  print(f' "{s}" -> {{ {commalist} }};')
print('}')
