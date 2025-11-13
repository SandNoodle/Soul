cmake_minimum_required(VERSION 3.25)

# ----- [ Project ] -----
project(Soul VERSION 0.1 LANGUAGES CXX)

# ----- [ Globals ] -----
set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_EXTENSIONS False)

# ----- [ Options ] -----
option(SOUL_BUILD_TESTS "Build tests" ON)

# ----- [ Files ] -----
set(Soul_SOURCES
        src/main.cpp
)

add_executable(Soul ${Soul_SOURCES})
set_target_properties(Soul PROPERTIES LINKER_LANGUAGE CXX)
target_include_directories(Soul PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
)
target_link_libraries(Soul PRIVATE SoulLib)

if (SOUL_BUILD_TESTS)
    add_subdirectory(test)
endif ()
