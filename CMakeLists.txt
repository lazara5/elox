cmake_minimum_required(VERSION 3.0)

project(slox LANGUAGES C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED TRUE)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/slox/include)

option(DEBUG_PRINT_CODE "print generated code" OFF)
option(DEBUG_TRACE_EXECUTION "trace program execution" OFF)
option(DEBUG_STRESS_GC "stress GC" OFF)
option(DEBUG_LOG_GC "log GC" OFF)

option(ENABLE_NAN_BOXING "enable NaN boxing" OFF)
option(ENABLE_COMPUTED_GOTO "enable computed goto dispatch" OFF)

if (DEBUG_PRINT_CODE)
    message(STATUS "Debug: print generated code")
    add_definitions(-DDEBUG_PRINT_CODE=1)
endif (DEBUG_PRINT_CODE)

if (DEBUG_TRACE_EXECUTION)
    message(STATUS "Debug: trace program execution")
    add_definitions(-DDEBUG_TRACE_EXECUTION=1)
endif (DEBUG_TRACE_EXECUTION)

if (DEBUG_STRESS_GC)
    message(STATUS "Debug: stress GC")
    add_definitions(-DDEBUG_STRESS_GC=1)
endif (DEBUG_STRESS_GC)

if (DEBUG_LOG_GC)
    message(STATUS "Debug: log GC")
    add_definitions(-DDEBUG_LOG_GC=1)
endif (DEBUG_LOG_GC)

if (ENABLE_NAN_BOXING)
    message(STATUS "NaN boxing: enabled")
    add_definitions(-DENABLE_NAN_BOXING=1)
endif (ENABLE_NAN_BOXING)

if (ENABLE_COMPUTED_GOTO)
    message(STATUS "Computed goto dispatch: enabled")
    add_definitions(-DENABLE_COMPUTED_GOTO=1)
endif (ENABLE_COMPUTED_GOTO)

option(WITH_ASAN "build with ASAN" OFF)

if (WITH_ASAN)
    add_compile_options(-fsanitize=address -fno-omit-frame-pointer -fsanitize=undefined)
    add_link_options(-fsanitize=address -fno-omit-frame-pointer -fsanitize=undefined)
endif (WITH_ASAN)

set(SLOX_LIB_HEADERS
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
    slox/include/slox/valueTable.h
    slox/include/slox/rand.h
    slox/include/slox/builtins.h
    slox/include/slox/opcodes.h
    slox/include/slox/state.h
)
set(SLOX_LIB_SOURCES
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
    slox/lib/valueTable.c
    slox/lib/builtins.c
    slox/lib/state.c
)

set(SLOX_SOURCES
    slox/slox.c
)

add_library(slox STATIC ${SLOX_LIB_SOURCES} ${SLOX_LIB_HEADERS})

add_executable(slox_bin ${SLOX_SOURCES} ${HEADERS})
target_link_libraries(slox_bin  slox m)
set_target_properties(slox_bin
    PROPERTIES RUNTIME_OUTPUT_NAME slox
)

