cmake_minimum_required(VERSION 3.10)
project(oalex)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -ggdb")
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_EXTENSIONS OFF)
include_directories(${CMAKE_SOURCE_DIR})
enable_testing()

add_executable(input_view_manual_test
               input_view_manual_test.cpp input_view_manual.cpp)
add_test(input_view_manual_test input_view_manual_test)
add_library(lexer OBJECT lexer.cpp input_view_manual.cpp jsonloc.cpp)
add_executable(lexer_test lexer_matcher.cpp lexer_test.cpp
               $<TARGET_OBJECTS:lexer>)
add_test(lexer_test lexer_test)

add_executable(jsonloc_test jsonloc_test.cpp $<TARGET_OBJECTS:lexer>)
add_test(jsonloc_test jsonloc_test)

add_subdirectory(archived)
