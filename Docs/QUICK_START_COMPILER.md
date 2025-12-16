# Building and running Soul's compiler

## Requirements

* Git
* C++26 compatible compiler:
    * Clang 21.0
    * Gcc 15.0
* CMake 3.25
* (Optionally) Ninja

## Getting the compiler

### Building from source

1. Download or clone the repository.

```shell
git clone git@github.com:SandNoodle/Soul.git
cd soul
```

2. Configure CMake

```shell
# Optionally, add the -GNinja flag.
cmake . -B cmake-build
cd cmake-build
```

3. (Re-)Build

```shell
# Run the build...
make

# ...or if ninja build system was specified:
ninja
```

### Installing from release binaries

TODO

## Running the compiler

TODO
