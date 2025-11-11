cmake_minimum_required(VERSION 3.25)
project(ScriptingLanguage_UT)

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)
FetchContent_Declare(
        googletest
        URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

enable_testing()

add_executable(
        ${PROJECT_NAME}
        ast/visitors/copy_test.cpp
        ast/visitors/desugar_test.cpp
        ast/visitors/error_collector_test.cpp
        ast/visitors/lower_test.cpp
        ast/visitors/semantic_analyzer_test.cpp
        ast/visitors/type_discoverer_test.cpp
        ast/visitors/type_resolver_test.cpp
        lexer/lexer_test.cpp
        parser/parser_test.cpp
)
target_link_libraries(
        ${PROJECT_NAME}
        PRIVATE
        ScriptingLanguage
        GTest::gtest_main
)

include(GoogleTest)
gtest_discover_tests(${PROJECT_NAME})
