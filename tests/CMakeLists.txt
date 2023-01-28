cmake_minimum_required(VERSION 3.14)

find_package(Libcheck REQUIRED)

include_directories(libcheck::check)

add_compile_options(-DCPPUTEST_MEM_LEAK_DETECTION_DISABLED)

if (WITH_COVERAGE)
    include(CodeCoverage)
    append_coverage_compiler_flags()
endif(WITH_COVERAGE)

FILE(GLOB_RECURSE eloxFunctionalTests CONFIGURE_DEPENDS "functional/test_*.elox")
set(functionalTests "")
foreach(test IN LISTS eloxFunctionalTests)
    get_filename_component(testName ${test} NAME_WLE)
    set(functionalTests "${functionalTests}FUNCTIONAL_TEST(${test}, ${testName})\n")
endforeach()
configure_file(elox-functional-tests.h.in ${CMAKE_SOURCE_DIR}/tests/elox-functional-tests.h)

set(TESTS_SOURCES
    eloxTests.c
    elox-functional-tests.h
)

add_executable(eloxTests ${TESTS_SOURCES})
target_link_libraries(eloxTests elox m libcheck::check gcov)

