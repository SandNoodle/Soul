cmake_minimum_required(VERSION 3.25)

# ----- [ Project ] -----
project(Soul VERSION 0.1 LANGUAGES CXX)

# ----- [ Globals ] -----
set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_EXTENSIONS False)

# ----- [ Options ] -----
option(SOUL_BUILD_TESTS "Build tests" ON)

# ----- [ Build Types ] -----
set(CMAKE_CONFIGURATION_TYPES "Debug;Release;RelWithDebInfo")
set(CMAKE_CONFIGURATION_TYPES "${CMAKE_CONFIGURATION_TYPES}"
        CACHE STRING "List of supported configurations.")

if (NOT CMAKE_BUILD_TYPE)
    message(STATUS "No build type provided, defaulting to RELEASE.")
    set(CMAKE_BUILD_TYPE "Release")
endif ()

if (NOT CMAKE_BUILD_TYPE IN_LIST CMAKE_CONFIGURATION_TYPES)
    message(STATUS "Available build types: ${CMAKE_CONFIGURATION_TYPES}")
    message(FATAL_ERROR "Unknown build type: ${CMAKE_BUILD_TYPE}")
endif ()

# ----- [ Files ] -----
file(GLOB_RECURSE Soul_SOURCES LIST_DIRECTORIES true CONFIGURE_DEPENDS "src/*.cpp")
file(GLOB_RECURSE Soul_HEADERS LIST_DIRECTORIES true CONFIGURE_DEPENDS "src/*.h")

set(Soul_INCLUDE_DIRECTORIRES "")
foreach (_headerFile ${Soul_HEADERS})
    get_filename_component(_dir ${_headerFile} DIRECTORY)
    list(APPEND Soul_INCLUDE_DIRECTORIRES ${_dir})
endforeach ()
list(REMOVE_DUPLICATES Soul_INCLUDE_DIRECTORIRES)

message(STATUS ${Soul_INCLUDE_DIRECTORIRES})

add_executable(Soul ${Soul_SOURCES} ${Soul_HEADERS})
set_target_properties(Soul PROPERTIES LINKER_LANGUAGE CXX)
target_include_directories(Soul PUBLIC ${Soul_INCLUDE_DIRECTORIRES})
target_link_libraries(Soul PRIVATE SoulLib)

if (SOUL_BUILD_TESTS)
    add_subdirectory(test)
endif ()
