cmake_minimum_required(VERSION 3.14)

project(elox LANGUAGES C)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED TRUE)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/elox/include)
#set(CMAKE_VERBOSE_MAKEFILE on)

include(LTO)
find_lto(C)

option(WITH_TESTS "compile tests" OFF)
option(WITH_COVERAGE "enable code coverage" OFF)

set(gcc_generic_flags
    -Wall -Wextra -Werror -Wno-unused-function -Wno-unused-variable
)
set(gcc_specific_flags
    -falign-functions=32
    -falign-loops=8
    -falign-labels=8
)

set(clang_generic_flags
    -Wno-implicit-const-int-float-conversion
)
set(clang_specific_flags
    "SHELL:-mllvm -align-all-functions=6"
    "SHELL:-mllvm -align-all-nofallthru-blocks=6"
)

set(intel_generic_flags
    -Wno-implicit-const-int-float-conversion
)

if (WITH_COVERAGE)
    include(CodeCoverage)
    append_coverage_compiler_flags()
    setup_target_for_coverage_lcov(
        NAME coverage
        EXECUTABLE eloxTests
        #EXCLUDE "/usr/include/*"
        EXCLUDE "tests/*"
    )
endif(WITH_COVERAGE)

if ("${CMAKE_C_COMPILER_ID}" MATCHES "Clang")
    MESSAGE("Clang")
    add_compile_options("${gcc_generic_flags}")
    add_compile_options("${clang_generic_flags}")
    add_compile_options("${clang_specific_flags}")
elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU")
    MESSAGE("GCC")
    add_compile_options("${gcc_generic_flags}")
    add_compile_options("${gcc_specific_flags}")
elseif ("${CMAKE_C_COMPILER_ID}" MATCHES "Intel")
    MESSAGE("Intel")
    add_compile_options("${gcc_generic_flags}")
    add_compile_options("${intel_generic_flags}")
elseif ("${CMAKE_C_COMPILER_ID}" STREQUAL "MSVC")
    MESSAGE("MSVC")
endif()

option(DEBUG_TRACE_SCANNER "trace scanner" OFF)
option(DEBUG_PRINT_CODE "print generated code" OFF)
option(DEBUG_TRACE_EXECUTION "trace program execution" OFF)
option(DEBUG_STRESS_GC "stress GC" OFF)
option(DEBUG_LOG_GC "log GC" OFF)
option(DEBUG_FORCE_SLOW_GC "force slow GC" OFF)

option(ENABLE_NAN_BOXING "enable NaN boxing" OFF)
option(ENABLE_COMPUTED_GOTO "enable computed goto dispatch" OFF)
option(ENABLE_LTO "enable LTO" OFF)

if (DEBUG_TRACE_SCANNER)
    message(STATUS "Debug: trace scanner")
    set(ELOX_DEBUG_TRACE_SCANNER ON)
endif (DEBUG_TRACE_SCANNER)

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

if (DEBUG_FORCE_SLOW_GC)
    message(STATUS "Debug: force slow GC")
    set(ELOX_DEBUG_FORCE_SLOW_GC ON)
endif (DEBUG_FORCE_SLOW_GC)

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
    add_compile_options(-fsanitize=address -fno-omit-frame-pointer -fsanitize=undefined -fno-sanitize-recover -fno-stack-protector)
    add_link_options(-fsanitize=address -fno-omit-frame-pointer -fsanitize=undefined -fno-sanitize-recover -fno-stack-protector)
endif (WITH_ASAN)

if (WIN32)
    set(ELOX_CONFIG_WIN32 ON)
endif (WIN32)

configure_file(elox/include/elox-config.h.in ${CMAKE_SOURCE_DIR}/elox/include/elox-config.h)

set(ELOX_LIB_HEADERS
    elox/include/elox/chunk.h
    elox/include/elox/memory.h
    elox/include/elox/debug.h
    elox/include/elox/value.h
    elox/include/elox/ValueArray.h
    elox/include/elox/function.h
    elox/include/elox/vm.h
    elox/lib/ops/opsInit.h
    elox/lib/ops/opsCleanup.h
    elox/lib/ops/addOps.h
    elox/lib/ops/inOps.h
    elox/include/elox/objTags.h
    elox/include/elox/objTagsDispatchStart.h
    elox/include/elox/objTagsDispatchEnd.h
    elox/include/elox/compiler.h
    elox/include/elox/scanner.h
    elox/include/elox/object.h
    elox/include/elox/Class.h
    elox/include/elox/table.h
    elox/include/elox/PropTable.h
    elox/include/elox/ValueTable.h
    elox/include/elox/handleSet.h
    elox/include/elox/third-party/rand.h
    elox/include/elox/builtins.h
    elox/include/elox/builtins/ctypeInit.h
    elox/include/elox/builtins/ctypeCleanup.h
    elox/include/elox/builtins/string.h
    elox/include/elox/builtins/array.h
    elox/include/elox/opcodes.h
    elox/include/elox/state.h
    elox/include/elox/util.h
    elox/include/elox.h
    elox/include/elox-config.h
    elox/include/elox-defines.h
    elox/include/elox/elox-internal.h
    elox/include/elox/elox-config-internal.h
    elox/include/elox/third-party/utf8decoder.h
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
    elox/lib/Class.c
    elox/lib/table.c
    elox/lib/PropTable.c
    elox/lib/ValueArray.c
    elox/lib/ValueTable.c
    elox/lib/handleSet.c
    elox/lib/builtins.c
    elox/lib/state.c
    elox/lib/third-party/snprintf.c
    elox/lib/string/fmt.c
    elox/lib/third-party/pattern.c
    elox/lib/string/string.c
    elox/lib/array/array.c
    elox/lib/util.c
    elox/lib/loader.c
    elox/lib/elox.c
)

set(ELOX_SOURCES
    elox/elox.c
)

set(ELOX_TEST_EMBED_SOURCES
    elox/elox_test_embed.c
)

add_library(elox STATIC ${ELOX_LIB_SOURCES} ${ELOX_LIB_HEADERS})
set_target_properties(elox PROPERTIES POSITION_INDEPENDENT_CODE ON)

add_executable(elox_bin ${ELOX_SOURCES} ${HEADERS})

target_link_libraries(elox_bin  elox)
if (NOT WIN32)
    target_link_libraries(elox_bin  m)
endif (NOT WIN32)

set_target_properties(elox_bin
    PROPERTIES RUNTIME_OUTPUT_NAME elox
)
target_enable_lto(elox_bin generic)

add_executable(elox_test_embed ${ELOX_TEST_EMBED_SOURCES} ${HEADERS})
target_link_libraries(elox_test_embed elox)
if (NOT WIN32)
    target_link_libraries(elox_test_embed m)
endif (NOT WIN32)
set_target_properties(elox_test_embed
    PROPERTIES RUNTIME_OUTPUT_NAME elox_test_embed
)

if (WITH_TESTS)
    add_subdirectory(tests)
endif(WITH_TESTS)

