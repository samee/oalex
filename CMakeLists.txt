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

include(FetchContent)
FetchContent_Declare(
  fmt
  GIT_REPOSITORY https://github.com/fmtlib/fmt.git
  GIT_TAG 19bd751020a1f3c3363b2eb67a039852f139a8d3)  # 6.2.1
FetchContent_MakeAvailable(fmt)
include_directories(${fmt_SOURCE_DIR}/include)

add_executable(lexer_test lexer.cpp lexer_matcher.cpp lexer_test.cpp)
target_link_libraries(lexer_test oalex)
add_test(lexer_test lexer_test)

add_executable(codegen_test codegen.cpp codegen_test.cpp
                            lexer.cpp regex_io.cpp)
target_link_libraries(codegen_test oalex)
add_test(codegen_test codegen_test)

add_executable(regex_io_test regex_io.cpp regex_io_test.cpp lexer.cpp)
target_link_libraries(regex_io_test oalex)
add_test(regex_io_test regex_io_test)

add_executable(ident_test ident.cpp ident_test.cpp)
target_link_libraries(ident_test oalex)
add_test(ident_test ident_test)

add_executable(template_test template.cpp template_test.cpp
                             ident.cpp lexer.cpp regex_io.cpp)

target_link_libraries(template_test oalex)
add_test(template_test template_test)

add_subdirectory(archived)
add_subdirectory(runtime)
