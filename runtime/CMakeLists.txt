cmake_minimum_required(VERSION 3.10)
project(runtime)

add_executable(input_view_test input_view_test.cpp input_view.cpp)
add_test(input_view_test input_view_test)

add_executable(skipper_test skipper_test.cpp skipper.cpp
               diags.cpp input_view.cpp)
add_test(skipper_test skipper_test)
