The files in this directory consist of two different kinds of tests:

  * Files on which `oalex` is expected to produce error.
  * Files on which `oalex` succeeds. They can test for both success or
    failure of user languages using appropriate examples.

The first kind, on which `oalex` is expected to fail, has comments of the form
"expected-error:". That directive is used by oalex_main_test.py.


Copyright 2019-2024 The oalex authors
-------------------------------------

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    https://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

This license also applies to all files in this directory, even though the
testdata files do not accompany individual copyright notices.
