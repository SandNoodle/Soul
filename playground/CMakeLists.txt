cmake_minimum_required(VERSION 3.10)

# ----- [ Project ] -----
project(SoulLangPlayground VERSION 0.1 LANGUAGES CXX)

# ----- [ Globals ] -----
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED True)

# ----- [ Options ] ------
# TODO: Select rendering API

# ----- [ Build Types ] ------
set(CMAKE_CONFIGURATION_TYPES "Debug;Release;RelWithDebInfo")
set(CMAKE_CONFIGURATION_TYPES "${CMAKE_CONFIGURATION_TYPES}"
	CACHE STRING "List of supported configurations.")

if(NOT CMAKE_BUILD_TYPE)
	message(STATUS "No build type provided, defaulting to RELEASE.")
	set(CMAKE_BUILD_TYPE "Release")
endif()

if(NOT CMAKE_BUILD_TYPE IN_LIST CMAKE_CONFIGURATION_TYPES)
	message(STATUS "Available buiild types: ${CMAKE_CONFIGURATION_TYPES}")
	message(FATAL_ERROR "Unknown build type: ${CMAKE_BUILD_TYPE}")
endif()

# ----- [ Files ] ------
file (GLOB_RECURSE ${PROJECT_NAME}_SOURCES LIST_DIRECTORIES true CONFIGURE_DEPENDS "./*.c")
file (GLOB_RECURSE ${PROJECT_NAME}_HEADERS LIST_DIRECTORIES true CONFIGURE_DEPENDS "./*.h")

set (INCLUDE_DIRS "")
foreach (_headerFile ${${PROJECT_NAME}_HEADERS})
    get_filename_component(_dir ${_headerFile} DIRECTORY)
	list (APPEND INCLUDE_DIRS ${_dir})
endforeach()
list (REMOVE_DUPLICATES INCLUDE_DIRS)

# ----- [ Create Library ] ------
add_executable(${PROJECT_NAME} ${${PROJECT_NAME}_SOURCES} ${${PROJECT_NAME}_HEADERS})
target_include_directories(${PROJECT_NAME} PRIVATE ${INCLUDE_DIRS})
set_target_properties(${PROJECT_NAME} PROPERTIES LINKER_LANGUAGE CXX)

# ----- [ Link libraries ] -----
target_link_libraries(${PROJECT_NAME} PRIVATE SoulLang)

