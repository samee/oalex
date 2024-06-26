#[[ Copyright 2019-2024 The oalex authors.

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
project(oalex)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -ggdb -fsanitize=undefined")
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)
include_directories(${CMAKE_SOURCE_DIR})
enable_testing()

add_library(oalex-bin-lib STATIC codegen.cpp compiler.cpp ident.cpp jsontmpl.cpp
                                 jsontmpl_parsers.cpp lexer.cpp
                                 pattern.cpp regex_io.cpp)
target_link_libraries(oalex-bin-lib oalex)

add_executable(lexer_test lexer_matcher.cpp lexer_test.cpp
               $<TARGET_OBJECTS:test_util>)
target_link_libraries(lexer_test oalex-bin-lib)
add_test(lexer_test lexer_test)

add_executable(codegen_test codegen_test.cpp codegen_test_util.cpp
               $<TARGET_OBJECTS:test_util>)
target_link_libraries(codegen_test oalex-bin-lib)
add_test(codegen_test codegen_test)

add_executable(codegen_gen_test codegen_gen_test.cpp codegen_test_util.cpp
               $<TARGET_OBJECTS:test_util>)
target_link_libraries(codegen_gen_test oalex-bin-lib)
add_custom_command(OUTPUT codegen_generated.cpp codegen_generated.h
                   COMMAND codegen_gen_test -o codegen_generated
                   DEPENDS codegen_gen_test)
add_executable(codegen_run_test codegen_run_test.cpp codegen_test_util.cpp
               codegen_generated.cpp $<TARGET_OBJECTS:test_util>)
target_include_directories(codegen_run_test PRIVATE ${CMAKE_BINARY_DIR})
target_link_libraries(codegen_run_test oalex-bin-lib)
add_test(codegen_run_test codegen_run_test)

add_executable(compiler_test compiler_test.cpp frontend_pieces.cpp
               codegen_test_util.cpp $<TARGET_OBJECTS:test_util>)
target_link_libraries(compiler_test oalex-bin-lib)
add_test(compiler_test compiler_test)

add_executable(jsontmpl_parsers_test jsontmpl_parsers_test.cpp
               $<TARGET_OBJECTS:test_util>)
target_link_libraries(jsontmpl_parsers_test oalex-bin-lib)
add_test(jsontmpl_parsers_test jsontmpl_parsers_test)

add_executable(regex_io_test regex_io_test.cpp
               $<TARGET_OBJECTS:test_util>)
target_link_libraries(regex_io_test oalex-bin-lib)
add_test(regex_io_test regex_io_test)

add_executable(ident_test ident.cpp ident_test.cpp lexer.cpp regex_io.cpp
               $<TARGET_OBJECTS:test_util>)
target_link_libraries(ident_test oalex)
add_test(ident_test ident_test)

add_executable(pattern_test pattern_test.cpp ident.cpp
               $<TARGET_OBJECTS:test_util>)
target_link_libraries(pattern_test oalex-bin-lib)
add_test(pattern_test pattern_test)

add_subdirectory(runtime)

add_executable(oalex-bin oalex_main.cpp frontend.cpp frontend_pieces.cpp)
target_include_directories(oalex-bin PRIVATE ${CMAKE_BINARY_DIR})
target_link_libraries(oalex-bin oalex-bin-lib)
set_target_properties(oalex-bin PROPERTIES OUTPUT_NAME "oalex")
add_test(NAME oalex_main_test
         COMMAND ${CMAKE_SOURCE_DIR}/oalex_main_test.py
                 --testdata ${CMAKE_SOURCE_DIR}/testdata
                 --bin $<TARGET_FILE:oalex-bin>
                 --source-path ${CMAKE_SOURCE_DIR}
                 --build-path ${CMAKE_BINARY_DIR})

add_test(NAME frontend_pieces_test
         COMMAND ${CMAKE_SOURCE_DIR}/frontend_pieces_test.py
         --bin $<TARGET_FILE:oalex-bin>
         --source-path ${CMAKE_SOURCE_DIR}
         --build-path ${CMAKE_BINARY_DIR})
