cmake_minimum_required(VERSION 3.25)

# ----- [ Project ] -----
project(SoulLib VERSION ${SOUL_VERSION_MAJOR}.${SOUL_VERSION_MINOR}.${SOUL_VERSION_PATCH} LANGUAGES CXX)

# ----- [ Globals ] -----
set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_CXX_EXTENSIONS False)

# ----- [ Files ] -----
set(SoulLib_SOURCES
        Sources/AST/ast.cpp
        Sources/AST/ast.h
        Sources/AST/ASTFwd.h
        Sources/AST/Visitors/compare.cpp
        Sources/AST/Visitors/compare.h
        Sources/AST/Visitors/copy.cpp
        Sources/AST/Visitors/copy.h
        Sources/AST/Visitors/DefaultTraverse.cpp
        Sources/AST/Visitors/DefaultTraverse.h
        Sources/AST/Visitors/desugar.cpp
        Sources/AST/Visitors/desugar.h
        Sources/AST/Visitors/ErrorCollector.cpp
        Sources/AST/Visitors/ErrorCollector.h
        Sources/AST/Visitors/lower.cpp
        Sources/AST/Visitors/lower.h
        Sources/AST/Visitors/SemanticAnalyzer.cpp
        Sources/AST/Visitors/SemanticAnalyzer.h
        Sources/AST/Visitors/stringify.cpp
        Sources/AST/Visitors/stringify.h
        Sources/AST/Visitors/TypeDiscoverer.cpp
        Sources/AST/Visitors/TypeDiscoverer.h
        Sources/AST/Visitors/TypeResolver.cpp
        Sources/AST/Visitors/TypeResolver.h
        Sources/AST/Visitors/visitor.h
        Sources/Common/CompileTimeDictionary.h
        Sources/Common/SourceOffset.h
        Sources/Types/overloads.cpp
        Sources/Types/overloads.h
        Sources/Types/type.cpp
        Sources/Types/type.h
        Sources/Types/TypeFwd.h
        Sources/Common/value.cpp
        Sources/Common/value.h
        Sources/Core/types.h
        Sources/IR/BasicBlock.h
        Sources/IR/builder.h
        Sources/IR/instruction.h
        Sources/IR/InstructionFwd.h
        Sources/IR/ir.h
        Sources/IR/Visitors/print.cpp
        Sources/IR/Visitors/print.h
        Sources/Lexer/codepoint.h
        Sources/Lexer/lexer.cpp
        Sources/Lexer/lexer.h
        Sources/Lexer/token.cpp
        Sources/Lexer/token.h
        Sources/Parser/parser.cpp
        Sources/Parser/parser.h
        Sources/Parser/TypeSpecifier.cpp
        Sources/Parser/TypeSpecifier.h
)

add_library(SoulLib STATIC ${SoulLib_SOURCES})
set_target_properties(SoulLib PROPERTIES LINKER_LANGUAGE CXX)
target_include_directories(SoulLib PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
        $<INSTALL_INTERFACE:soul>
)

if (SOUL_BUILD_TESTS)
    add_subdirectory(Tests)
endif ()
