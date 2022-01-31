#[[ Copyright 2019-2020 The oalex authors.

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        https://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License. ]]

cmake_minimum_required(VERSION 3.10)
project(runtime)

add_library(oalex builtins.cpp diags.cpp indent.cpp input_view.cpp jsonloc.cpp
                  parser_helpers.cpp regex.cpp skipper.cpp util_impl.cpp)
target_link_libraries(oalex INTERFACE fmt)
target_include_directories(oalex INTERFACE .)

add_library(test_util OBJECT test_util.cpp)
add_executable(input_view_test input_view_test.cpp
               $<TARGET_OBJECTS:test_util>)
target_link_libraries(input_view_test oalex)
add_test(input_view_test input_view_test)

add_executable(skipper_test skipper_test.cpp
               $<TARGET_OBJECTS:test_util>)
target_link_libraries(skipper_test oalex)
add_test(skipper_test skipper_test)

add_executable(builtins_test builtins_test.cpp
               $<TARGET_OBJECTS:test_util>)
target_link_libraries(builtins_test oalex)
add_test(builtins_test builtins_test)
