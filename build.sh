#!/bin/bash

# Options
EXECUTABLE_NAME="main"
COMPILER="gcc"
STANDARD="std=c99"
COMPILER_FLAGS=($STANDARD "Wall" "Wextra")
DEFINE_FLAGS=()
LINKER_FLAGS=()
BUILD_TYPE="Debug"

INCLUDE_DIRS=("../src")

# Read command line args
while getopts "b:h:" arg; do
	case "${arg}" in
		b)
			# BUILD TYPE
			BUILD_TYPE=${OPTARG}
			;;
		h | *)
			# HELP
			exit 1
			;;
	esac
done

# Build Type
case $BUILD_TYPE in
	Release | Rel)
		COMPILER_FLAGS+=("O3")
		DEFINE_FLAGS+=("NDEBUG")
		;;
	Debug| Dbg)
		COMPILER_FLAGS+=("O0" "g")
		;;
	*)
		echo "Unknown Build Type. Try [Release | Rel] or [Debug | Dbg]"
		exit 1
esac

echo "[ OPTIONS ]"
echo " * Compiler       : $COMPILER"
echo " * Compiler flags : ${COMPILER_FLAGS[@]}"
echo " * Linker flags   : ${LINKER_FLAGS[@]}"
echo " * Define flags   : ${DEFINE_FLAGS[@]}"
echo " * Build type     : ${BUILD_TYPE}"

# Create build directory if does not exist.
mkdir -p build

# Compilation
echo "[ COMPILING ]"
pushd build > /dev/null
$COMPILER ../main.c -o $EXECUTABLE_NAME ${DEFINE_FLAGS[@]/#/-D} ${COMPILER_FLAGS[@]/#/-} ${LINKER_FLAGS[@]} ${INCLUDE_DIRS[@]/#/-I}
popd > /dev/null
