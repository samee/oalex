cmake_minimum_required(VERSION 3.10)
project(archived)

add_executable(early early.cpp)

add_executable(dfa_test dfa_test.cpp dfa.cpp input_view_auto_forget.cpp)
target_link_libraries(dfa_test PRIVATE fmt)
add_test(dfa_test dfa_test)

add_executable(input_view_auto_forget_test
               input_view_auto_forget_test.cpp input_view_auto_forget.cpp)
add_test(input_view_auto_forget_test input_view_auto_forget_test)

