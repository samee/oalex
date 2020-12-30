cmake_minimum_required(VERSION 3.10)
project(runtime)

add_library(oalex diags.cpp input_view.cpp jsonloc.cpp
                  regex.cpp skipper.cpp util_impl.cpp)
target_link_libraries(oalex INTERFACE fmt)
target_include_directories(oalex INTERFACE .)

add_library(test_util OBJECT test_util.cpp)
add_executable(input_view_test input_view_test.cpp
               $<TARGET_OBJECTS:test_util>)
target_link_libraries(input_view_test oalex)
add_test(input_view_test input_view_test)

add_executable(skipper_test skipper_test.cpp
               $<TARGET_OBJECTS:test_util>)
target_link_libraries(input_view_test oalex)
target_link_libraries(skipper_test oalex)
add_test(skipper_test skipper_test)
