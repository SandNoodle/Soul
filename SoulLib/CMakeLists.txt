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
        Sources/AST/Visitors/Compare.cpp
        Sources/AST/Visitors/Compare.h
        Sources/AST/Visitors/Copy.cpp
        Sources/AST/Visitors/Copy.h
        Sources/AST/Visitors/DefaultTraverse.cpp
        Sources/AST/Visitors/DefaultTraverse.h
        Sources/AST/Visitors/Desugar.cpp
        Sources/AST/Visitors/Desugar.h
        Sources/AST/Visitors/ErrorCollector.cpp
        Sources/AST/Visitors/ErrorCollector.h
        Sources/AST/Visitors/Lower.cpp
        Sources/AST/Visitors/Lower.h
        Sources/AST/Visitors/SemanticAnalyzer.cpp
        Sources/AST/Visitors/SemanticAnalyzer.h
        Sources/AST/Visitors/Stringify.cpp
        Sources/AST/Visitors/Stringify.h
        Sources/AST/Visitors/TypeDiscoverer.cpp
        Sources/AST/Visitors/TypeDiscoverer.h
        Sources/AST/Visitors/TypeResolver.cpp
        Sources/AST/Visitors/TypeResolver.h
        Sources/AST/Visitors/Visitor.h
        Sources/Common/CompileTimeDictionary.h
        Sources/Common/SourceOffset.h
        Sources/Types/Overloads.cpp
        Sources/Types/Overloads.h
        Sources/Types/Type.cpp
        Sources/Types/Type.h
        Sources/Types/TypeFwd.h
        Sources/Common/Value.cpp
        Sources/Common/Value.h
        Sources/Core/Types.h
        Sources/IR/BasicBlock.h
        Sources/IR/Builder.h
        Sources/IR/instruction.h
        Sources/IR/InstructionFwd.h
        Sources/IR/IR.h
        Sources/IR/Visitors/Print.cpp
        Sources/IR/Visitors/Print.h
        Sources/Lexer/Codepoint.h
        Sources/Lexer/Lexer.cpp
        Sources/Lexer/Lexer.h
        Sources/Lexer/Token.cpp
        Sources/Lexer/Token.h
        Sources/Parser/Parser.cpp
        Sources/Parser/Parser.h
        Sources/Parser/TypeSpecifier.cpp
        Sources/Parser/TypeSpecifier.h
)

add_library(SoulLib STATIC ${SoulLib_SOURCES})
set_target_properties(SoulLib PROPERTIES LINKER_LANGUAGE CXX)
target_include_directories(SoulLib PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/Sources>
        $<INSTALL_INTERFACE:soul>
)

if (SOUL_BUILD_TESTS)
    add_subdirectory(Tests)
endif ()
