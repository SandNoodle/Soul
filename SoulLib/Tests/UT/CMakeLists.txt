cmake_minimum_required(VERSION 3.25)
project(SoulLib_UT)

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)
FetchContent_Declare(
        googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG v1.17.0
        GIT_SHALLOW TRUE
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

enable_testing()

add_executable(
        SoulLib_UT
        ast/visitors/copy_test.cpp
        ast/visitors/desugar_test.cpp
        ast/visitors/error_collector_test.cpp
        ast/visitors/lower_test.cpp
        ast/visitors/semantic_analyzer_test.cpp
        ast/visitors/type_discoverer_test.cpp
        ast/visitors/type_resolver_test.cpp
        lexer/LexerTest.cpp
        parser/ParserTest.cpp
)
target_link_libraries(
        SoulLib_UT
        PRIVATE
        GTest::gtest_main
        SoulLib
)

include(GoogleTest)
gtest_discover_tests(SoulLib_UT)
