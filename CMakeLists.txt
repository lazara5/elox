cmake_minimum_required(VERSION 3.1)

project(elox LANGUAGES C)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED TRUE)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/elox/include)

include(LTO)
find_lto(C)

set(gcc_generic_flags -Wall -Wextra -Werror -Wno-unused-function -Wno-unused-variable)

if ("${CMAKE_C_COMPILER_ID}" MATCHES "Clang")
    MESSAGE("Clang")
    add_compile_options("${gcc_generic_flags}")
elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU")
    MESSAGE("GCC")
    add_compile_options("${gcc_generic_flags}")
elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "Intel")
    MESSAGE("Intel")
elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "MSVC")
    MESSAGE("MSVC")
endif()

option(DEBUG_PRINT_CODE "print generated code" OFF)
option(DEBUG_TRACE_EXECUTION "trace program execution" OFF)
option(DEBUG_STRESS_GC "stress GC" OFF)
option(DEBUG_LOG_GC "log GC" OFF)

option(ENABLE_NAN_BOXING "enable NaN boxing" OFF)
option(ENABLE_COMPUTED_GOTO "enable computed goto dispatch" OFF)
option(ENABLE_LTO "enable LTO" OFF)

if (DEBUG_PRINT_CODE)
    message(STATUS "Debug: print generated code")
    set(ELOX_DEBUG_PRINT_CODE ON)
endif (DEBUG_PRINT_CODE)

if (DEBUG_TRACE_EXECUTION)
    message(STATUS "Debug: trace program execution")
    set(ELOX_DEBUG_TRACE_EXECUTION ON)
endif (DEBUG_TRACE_EXECUTION)

if (DEBUG_STRESS_GC)
    message(STATUS "Debug: stress GC")
    set(ELOX_DEBUG_STRESS_GC ON)
endif (DEBUG_STRESS_GC)

if (DEBUG_LOG_GC)
    message(STATUS "Debug: log GC")
    set(ELOX_DEBUG_LOG_GC ON)
endif (DEBUG_LOG_GC)

if (ENABLE_NAN_BOXING)
    message(STATUS "NaN boxing: enabled")
    set(ELOX_ENABLE_NAN_BOXING ON)
endif (ENABLE_NAN_BOXING)

if (ENABLE_COMPUTED_GOTO)
    message(STATUS "Computed goto dispatch: enabled")
    set(ELOX_ENABLE_COMPUTED_GOTO ON)
endif (ENABLE_COMPUTED_GOTO)

option(WITH_ASAN "build with ASAN" OFF)

if (WITH_ASAN)
    add_compile_options(-fsanitize=address -fno-omit-frame-pointer -fsanitize=undefined)
    add_link_options(-fsanitize=address -fno-omit-frame-pointer -fsanitize=undefined)
endif (WITH_ASAN)

configure_file(elox/include/elox-config.h.in ${CMAKE_SOURCE_DIR}/elox/include/elox-config.h)

set(ELOX_LIB_HEADERS
    elox/include/elox/common.h
    elox/include/elox/chunk.h
    elox/include/elox/memory.h
    elox/include/elox/debug.h
    elox/include/elox/value.h
    elox/include/elox/function.h
    elox/include/elox/vm.h
    elox/include/elox/compiler.h
    elox/include/elox/scanner.h
    elox/include/elox/object.h
    elox/include/elox/table.h
    elox/include/elox/valueTable.h
    elox/include/elox/handleSet.h
    elox/include/elox/rand.h
    elox/include/elox/builtins.h
    elox/include/elox/opcodes.h
    elox/include/elox/state.h
    elox/include/elox/util.h
    elox/include/elox/primegen.h
    elox/include/elox.h
    elox/include/elox-config.h
    elox/include/elox/elox-internal.h
)

set(ELOX_LIB_SOURCES
    elox/lib/chunk.c
    elox/lib/memory.c
    elox/lib/debug.c
    elox/lib/value.c
    elox/lib/function.c
    elox/lib/vm.c
    elox/lib/compiler.c
    elox/lib/scanner.c
    elox/lib/object.c
    elox/lib/table.c
    elox/lib/valueTable.c
    elox/lib/handleSet.c
    elox/lib/builtins.c
    elox/lib/state.c
    elox/lib/fmt.c
    elox/lib/util.c
    elox/lib/primegen.c
    elox/lib/elox.c
)

set(ELOX_SOURCES
    elox/elox.c
)

set(ELOX_TEST_EMBED_SOURCES
    elox/elox_test_embed.c
)

add_library(elox STATIC ${ELOX_LIB_SOURCES} ${ELOX_LIB_HEADERS})

add_executable(elox_bin ${ELOX_SOURCES} ${HEADERS})
target_link_libraries(elox_bin  elox m)
set_target_properties(elox_bin
    PROPERTIES RUNTIME_OUTPUT_NAME elox
)
target_enable_lto(elox_bin generic)

add_executable(elox_test_embed ${ELOX_TEST_EMBED_SOURCES} ${HEADERS})
target_link_libraries(elox_test_embed elox m)
set_target_properties(elox_test_embed
    PROPERTIES RUNTIME_OUTPUT_NAME elox_test_embed
)

