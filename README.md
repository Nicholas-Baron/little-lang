# little-lang
A LLVM compiler for a small, functional language

[Playlist documenting the concepts in this project](https://youtube.com/playlist?list=PLHqsxrQm_CBIw6C5IfUXlWOdmKQ9vnbQF)

## Tools Used
- Bison
  + Used for checking the language's grammar
- Clang-Format
  + Code formatting
- Clang-Tidy
  + Code linting
- CMake
  + Build system
- Conan
  + Dependency management

## Build Process
0. Download this repository
1. `cmake -B build -GNinja -DCMAKE_BUILD_TYPE=Debug .` [^1]
2. `cd build`
3. `ninja` or your preferred build command [^1]

[^1]: I recommend using `ninja` for its "parallel by default" approach.
However, any build system `cmake` understands should be fine.
Just change `-GNinja` to your preference.

### Testing
Part of the build process is compiling tests.
These are stored in the `test` directory and are mostly useful for checking internal behaviors.
