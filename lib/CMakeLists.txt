cmake_minimum_required(VERSION 3.25)

# ----- [ Project ] -----
project(SoulLib VERSION ${SOUL_VERSION_MAJOR}.${SOUL_VERSION_MINOR}.${SOUL_VERSION_PATCH} LANGUAGES CXX)

# ----- [ Globals ] -----
set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_EXTENSIONS False)

# ----- [ Files ] -----
set(SoulLib_SOURCES
        src/ast/ast.cpp
        src/ast/ast.h
        src/ast/ast_fwd.h
        src/ast/visitors/compare.cpp
        src/ast/visitors/compare.h
        src/ast/visitors/copy.cpp
        src/ast/visitors/copy.h
        src/ast/visitors/default_traverse.cpp
        src/ast/visitors/default_traverse.h
        src/ast/visitors/desugar.cpp
        src/ast/visitors/desugar.h
        src/ast/visitors/error_collector.cpp
        src/ast/visitors/error_collector.h
        src/ast/visitors/lower.cpp
        src/ast/visitors/lower.h
        src/ast/visitors/semantic_analyzer.cpp
        src/ast/visitors/semantic_analyzer.h
        src/ast/visitors/stringify.cpp
        src/ast/visitors/stringify.h
        src/ast/visitors/type_discoverer.cpp
        src/ast/visitors/type_discoverer.h
        src/ast/visitors/type_resolver.cpp
        src/ast/visitors/type_resolver.h
        src/ast/visitors/visitor.h
        src/common/compile_time_dictionary.h
        src/common/source_location.h
        src/types/overloads.cpp
        src/types/overloads.h
        src/types/type.cpp
        src/types/type.h
        src/types/types_fwd.h
        src/common/value.cpp
        src/common/value.h
        src/core/types.h
        src/ir/basic_block.h
        src/ir/builder.h
        src/ir/instruction.h
        src/ir/instruction_fwd.h
        src/ir/ir.h
        src/ir/visitors/print.cpp
        src/ir/visitors/print.h
        src/lexer/codepoint.h
        src/lexer/lexer.cpp
        src/lexer/lexer.h
        src/lexer/token.cpp
        src/lexer/token.h
        src/parser/parser.cpp
        src/parser/parser.h
        src/parser/type_specifier.cpp
        src/parser/type_specifier.h
)

add_library(SoulLib STATIC ${SoulLib_SOURCES})
set_target_properties(SoulLib PROPERTIES LINKER_LANGUAGE CXX)
target_include_directories(SoulLib PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
        $<INSTALL_INTERFACE:soul>
)

if (SOUL_BUILD_TESTS)
    add_subdirectory(test)
endif ()
