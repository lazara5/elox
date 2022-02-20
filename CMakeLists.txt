cmake_minimum_required(VERSION 3.0)

project(slox LANGUAGES C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED TRUE)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/slox/include)

option(WITH_ASAN "build with ASAN" ON)

if (WITH_ASAN)
    add_compile_options(-fsanitize=address -fsanitize=undefined)
    add_link_options(-fsanitize=address -fsanitize=undefined)
endif(WITH_ASAN)

set(HEADERS
    slox/include/slox/common.h
    slox/include/slox/chunk.h
    slox/include/slox/memory.h
    slox/include/slox/debug.h
    slox/include/slox/value.h
    slox/include/slox/vm.h
    slox/include/slox/compiler.h
    slox/include/slox/scanner.h
    slox/include/slox/object.h
    slox/include/slox/table.h
    slox/include/slox/rand.h
)
set(SOURCES
    slox/slox.c
    slox/lib/chunk.c
    slox/lib/memory.c
    slox/lib/debug.c
    slox/lib/value.c
    slox/lib/vm.c
    slox/lib/compiler.c
    slox/lib/scanner.c
    slox/lib/object.c
    slox/lib/table.c
)

add_executable(${PROJECT_NAME} ${SOURCES} ${HEADERS})
target_link_libraries(${PROJECT_NAME} m)

