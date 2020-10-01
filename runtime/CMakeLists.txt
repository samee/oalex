cmake_minimum_required(VERSION 3.10)
project(runtime)

add_library(oalex diags.cpp input_view.cpp jsonloc.cpp
                  regex.cpp skipper.cpp)
target_link_libraries(oalex INTERFACE fmt)

add_executable(input_view_test input_view_test.cpp)
target_link_libraries(input_view_test oalex)
add_test(input_view_test input_view_test)

# It's okay to rely on ../ for tests, but not for liboalex
add_executable(jsonloc_test ../lexer.cpp jsonloc_test.cpp jsonloc.cpp)
target_link_libraries(jsonloc_test oalex)
add_test(jsonloc_test jsonloc_test)

add_executable(skipper_test skipper_test.cpp)
target_link_libraries(skipper_test oalex)
add_test(skipper_test skipper_test)
