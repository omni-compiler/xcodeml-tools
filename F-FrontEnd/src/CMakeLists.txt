set(F_INPUT_XMOD_H "${CMAKE_CURRENT_BINARY_DIR}/F-input-xmod.h")

configure_file("${CMAKE_CURRENT_SOURCE_DIR}/F-input-xmod.h.in"
               "${F_INPUT_XMOD_H}"
               @ONLY)

file(GLOB ffront_SRC "*.c")
file(GLOB ffront_HEADERS "*.h")

set(C_EXPRCODE_H "${CMAKE_CURRENT_BINARY_DIR}/C-exprcode.h")
set(C_EXPRCODE_C "${CMAKE_CURRENT_BINARY_DIR}/C-exprcode.c")

add_custom_command(OUTPUT "${C_EXPRCODE_H}"
                   COMMAND ${AWK} -f "${CMAKE_CURRENT_SOURCE_DIR}/C-exprcode.h.awk" < "${CMAKE_CURRENT_SOURCE_DIR}/C-exprcode.def" > "${C_EXPRCODE_H}"
                   MAIN_DEPENDENCY "${CMAKE_CURRENT_SOURCE_DIR}/C-exprcode.h.awk"
                   DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/C-exprcode.def"
                   COMMENT "Generating C-exprcode.h"
                   VERBATIM)

add_custom_command(OUTPUT "${C_EXPRCODE_C}"
                   COMMAND ${AWK} -f "${CMAKE_CURRENT_SOURCE_DIR}/C-exprcode.c.awk" < "${CMAKE_CURRENT_SOURCE_DIR}/C-exprcode.def" > "${C_EXPRCODE_C}"
                   MAIN_DEPENDENCY "${CMAKE_CURRENT_SOURCE_DIR}/C-exprcode.c.awk"
                   DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/C-exprcode.def"
                   COMMENT "Generating C-exprcode.c"
                   VERBATIM)

add_custom_target(generate_ffront_src DEPENDS "${C_EXPRCODE_H}" "${C_EXPRCODE_C}")


set(F95_PARSER_C "${CMAKE_CURRENT_BINARY_DIR}/F95-parser.c")
set(F95_PARSER_BISON_REPORT "${CMAKE_CURRENT_BINARY_DIR}/F95-parser-report.txt")

BISON_TARGET(F95_parser "${CMAKE_CURRENT_SOURCE_DIR}/F95-parser.y" "${F95_PARSER_C}"
             COMPILE_FLAGS --yacc)

set(FFRONT_INC_DIRS ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/include ${CMAKE_BINARY_DIR}/include
    ${CMAKE_CURRENT_BINARY_DIR} ${LIBXML2_INCLUDE_DIR})
set(FFRONT_DEFINES NDEBUG BUGFIX __XCODEML__ YYDEBUG)

add_executable(ffront ${ffront_SRC} "${F_INPUT_XMOD_H}" "${C_EXPRCODE_H}" "${C_EXPRCODE_C}" ${BISON_F95_parser_OUTPUTS})
add_dependencies(ffront generate_ffront_src)

set_target_properties(ffront
                      PROPERTIES
                      RUNTIME_OUTPUT_DIRECTORY ${INT_OMNI_HOME_BIN}
                      OUTPUT_NAME F_Front)

target_include_directories(ffront PRIVATE ${FFRONT_INC_DIRS})

target_compile_definitions(ffront PRIVATE ${FFRONT_DEFINES})
target_link_libraries(ffront ${LIBXML2_LIBRARY})

add_subdirectory(fincludes)

set(FFRONT_CPP_DIR ${CMAKE_CURRENT_BINARY_DIR}/cpp)
file(MAKE_DIRECTORY ${FFRONT_CPP_DIR})

set(FFRONT_CPP_SRC "")

message(STATUS "Generating CPP wrappers")
foreach(C_FILEPATH ${ffront_SRC} ${C_EXPRCODE_C} ${F95_PARSER_C})
  get_filename_component(SRC_FILE_BASENAME ${C_FILEPATH} NAME_WE)
  set(CPP_SRC_FILE_PATH ${FFRONT_CPP_DIR}/${SRC_FILE_BASENAME}.cpp)
  set(FFRONT_CPP_SRC ${FFRONT_CPP_SRC} ${CPP_SRC_FILE_PATH})
  configure_file("${CMAKE_CURRENT_SOURCE_DIR}/wrapper.cpp.in"
                 "${CPP_SRC_FILE_PATH}"
                  @ONLY)
endforeach()

add_executable(ffront-cpp EXCLUDE_FROM_ALL ${FFRONT_CPP_SRC})
add_dependencies(ffront-cpp generate_ffront_src)
set_target_properties(ffront-cpp
                      PROPERTIES
                      RUNTIME_OUTPUT_DIRECTORY ${INT_OMNI_HOME_BIN}
                      OUTPUT_NAME ffront-cpp)
target_link_libraries(ffront-cpp ${LIBXML2_LIBRARY})
target_include_directories(ffront-cpp PRIVATE ${FFRONT_INC_DIRS})
target_compile_definitions(ffront-cpp PRIVATE ${FFRONT_DEFINES})