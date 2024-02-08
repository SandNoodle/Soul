cmake_minimum_required(VERSION 3.10)

project(Soul VERSION 0.1)

option(SOUL_BUILD_TESTS "Build tests." ON)

add_subdirectory(src)
add_subdirectory(playground)

if(SOUL_BUILD_TESTS)
	add_subdirectory(test)
endif()
