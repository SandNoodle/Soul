cmake_minimum_required(VERSION 3.25)

# ----- [ Project ] -----
project(Soul VERSION ${SOUL_VERSION_MAJOR}.${SOUL_VERSION_MINOR}.${SOUL_VERSION_PATCH} LANGUAGES CXX)

# ----- [ Globals ] -----
set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_EXTENSIONS False)

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

# ----- [ DEFINES ] -----
target_compile_definitions(Soul PRIVATE
        SOUL_VERSION_MAJOR=${PROJECT_VERSION_MAJOR}
        SOUL_VERSION_MINOR=${PROJECT_VERSION_MINOR}
        SOUL_VERSION_PATCH=${PROJECT_VERSION_PATCH}
)

# ----- [ TESTS ] -----
if (SOUL_BUILD_TESTS)
    add_subdirectory(test)
endif ()
