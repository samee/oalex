cmake_minimum_required(VERSION 3.10)
project(runtime)

add_library(oalex diags.cpp input_view.cpp lookahead_regex.cpp skipper.cpp)
target_link_libraries(oalex INTERFACE fmt)

add_executable(input_view_test input_view_test.cpp)
target_link_libraries(input_view_test oalex)
add_test(input_view_test input_view_test)

add_executable(skipper_test skipper_test.cpp)
target_link_libraries(skipper_test oalex)
add_test(skipper_test skipper_test)
