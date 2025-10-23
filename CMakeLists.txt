cmake_minimum_required(VERSION 3.25)

# ----- [ Project ] -----
project(ScriptingLanguage VERSION 0.1 LANGUAGES CXX)

# ----- [ Globals ] -----
set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# ----- [ Options ] -----
option(BUILD_TESTS "Build Unit Tests" ON)

# ----- [ Build Types ] -----
set(CMAKE_CONFIGURATION_TYPES "Debug;Release;RelWithDebInfo")
set(CMAKE_CONFIGURATION_TYPES "${CMAKE_CONFIGURATION_TYPES}"
	CACHE STRING "List of supported configurations.")

if(NOT CMAKE_BUILD_TYPE)
	message(STATUS "No build type provided, defaulting to RELEASE.")
	set(CMAKE_BUILD_TYPE "Release")
endif()

if(NOT CMAKE_BUILD_TYPE IN_LIST CMAKE_CONFIGURATION_TYPES)
	message(STATUS "Available build types: ${CMAKE_CONFIGURATION_TYPES}")
	message(FATAL_ERROR "Unknown build type: ${CMAKE_BUILD_TYPE}")
endif()

# ----- [ Files ] -----
# TODO Think about giving each directory its own CMakeLists.txt
file (GLOB_RECURSE ${PROJECT_NAME}_SOURCES LIST_DIRECTORIES true CONFIGURE_DEPENDS "src/*.cpp")
file (GLOB_RECURSE ${PROJECT_NAME}_HEADERS LIST_DIRECTORIES true CONFIGURE_DEPENDS "src/*.h")

set (INCLUDE_DIRS "")
foreach (_headerFile ${${PROJECT_NAME}_HEADERS})
    get_filename_component(_dir ${_headerFile} DIRECTORY)
	list (APPEND INCLUDE_DIRS ${_dir})
endforeach()
list (REMOVE_DUPLICATES INCLUDE_DIRS)

add_library(${PROJECT_NAME} STATIC ${${PROJECT_NAME}_SOURCES} ${${PROJECT_NAME}_HEADERS})
set_target_properties(${PROJECT_NAME} PROPERTIES LINKER_LANGUAGE CXX)
target_include_directories(${PROJECT_NAME} PUBLIC ${INCLUDE_DIRS})

if (BUILD_TESTS)
	add_subdirectory(test)
endif()
