cmake_minimum_required(VERSION 3.10)
project(archived)

add_executable(early early.cpp)

add_executable(dfa_test dfa_test.cpp dfa.cpp input_view.cpp)
add_test(dfa_test dfa_test)

add_executable(input_view_test input_view_test.cpp input_view.cpp)
add_test(input_view_test input_view_test)

