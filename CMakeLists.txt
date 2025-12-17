cmake_minimum_required(VERSION 3.25)

project(SoulLang)

# ----- [ Globals ] -----
set(SOUL_VERSION_MAJOR 0)
set(SOUL_VERSION_MINOR 1)
set(SOUL_VERSION_PATCH 0)

# ----- [ Options ] -----
option(SOUL_BUILD_TESTS "Build tests" ON)

add_subdirectory(compiler)
add_subdirectory(SoulLib)
