cmake_minimum_required(VERSION 3.0)

project(elox LANGUAGES C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED TRUE)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/elox/include)

option(DEBUG_PRINT_CODE "print generated code" OFF)
option(DEBUG_TRACE_EXECUTION "trace program execution" OFF)
option(DEBUG_STRESS_GC "stress GC" OFF)
option(DEBUG_LOG_GC "log GC" OFF)

option(ENABLE_DYNAMIC_STACK "enable dynamic stack" OFF)
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

if (ENABLE_DYNAMIC_STACK)
    message(STATUS "Dynamic stack: enabled")
    add_definitions(-DENABLE_DYNAMIC_STACK=1)
endif (ENABLE_DYNAMIC_STACK)

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

set(ELOX_LIB_HEADERS
    elox/include/elox/common.h
    elox/include/elox/chunk.h
    elox/include/elox/memory.h
    elox/include/elox/debug.h
    elox/include/elox/value.h
    elox/include/elox/vm.h
    elox/include/elox/compiler.h
    elox/include/elox/scanner.h
    elox/include/elox/object.h
    elox/include/elox/table.h
    elox/include/elox/valueTable.h
    elox/include/elox/rand.h
    elox/include/elox/builtins.h
    elox/include/elox/opcodes.h
    elox/include/elox/state.h
)
set(ELOX_LIB_SOURCES
    elox/elox.c
    elox/lib/chunk.c
    elox/lib/memory.c
    elox/lib/debug.c
    elox/lib/value.c
    elox/lib/vm.c
    elox/lib/compiler.c
    elox/lib/scanner.c
    elox/lib/object.c
    elox/lib/table.c
    elox/lib/valueTable.c
    elox/lib/builtins.c
    elox/lib/state.c
    elox/lib/common.c
)

set(ELOX_SOURCES
    elox/elox.c
)

add_library(elox STATIC ${ELOX_LIB_SOURCES} ${ELOX_LIB_HEADERS})

add_executable(elox_bin ${ELOX_SOURCES} ${HEADERS})
target_link_libraries(elox_bin  elox m)
set_target_properties(elox_bin
    PROPERTIES RUNTIME_OUTPUT_NAME elox
)

