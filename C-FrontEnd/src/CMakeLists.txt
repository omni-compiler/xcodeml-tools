file(GLOB cfront_SRC "*.c")
file(GLOB cfront_HEADERS "*.h")

set(C_EXPRCODE_PL "${CMAKE_CURRENT_SOURCE_DIR}/c-exprcode.pl")
set(C_EXPRCODE_H "${CMAKE_CURRENT_BINARY_DIR}/c-exprcode.h")
set(C_EXPRCODE_C "${CMAKE_CURRENT_BINARY_DIR}/c-exprcode.c")

add_custom_command(OUTPUT ${C_EXPRCODE_H} ${C_EXPRCODE_C}
                   COMMAND ${PERL_EXECUTABLE} "${C_EXPRCODE_PL}" "${CMAKE_CURRENT_BINARY_DIR}"
                   WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                   DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/c-exprcode.def" ${C_EXPRCODE_PL}
                   COMMENT "Generating c-exprcode.h and c-exprcode.c"
                   VERBATIM)

set(C_PARSER_H "${CMAKE_CURRENT_BINARY_DIR}/c-parser.h")
set(C_PARSER_C "${CMAKE_CURRENT_BINARY_DIR}/c-parser.c")
set(CPARSER_BISON_REPORT "${CMAKE_CURRENT_BINARY_DIR}/c-parser-report.txt")

BISON_TARGET(C99_parser "${CMAKE_CURRENT_SOURCE_DIR}/c-parser.y" ${C_PARSER_C}
             DEFINES_FILE ${C_PARSER_H}
             COMPILE_FLAGS --yacc)
             
set(C_LEXER_C "${CMAKE_CURRENT_BINARY_DIR}/c-lexer.c")
FLEX_TARGET(C99_lexer "${CMAKE_CURRENT_SOURCE_DIR}/c-lexer.l" ${C_LEXER_C})

ADD_FLEX_BISON_DEPENDENCY(C99_lexer C99_parser)

set(C_TOKEN_C "${CMAKE_CURRENT_BINARY_DIR}/c-token.c")
set(C_TOKEN_PL "${CMAKE_CURRENT_SOURCE_DIR}/c-token.pl")
add_custom_command(OUTPUT "${C_TOKEN_C}"
                   COMMAND ${PERL_EXECUTABLE} "${C_TOKEN_PL}" "${CMAKE_CURRENT_BINARY_DIR}"
                   WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                   DEPENDS "${C_TOKEN_PL}" ${C_PARSER_H}
                   COMMENT "Generating c-token.c"
                   VERBATIM)

add_custom_target(generate_cfront_src DEPENDS ${C_EXPRCODE_H} ${C_EXPRCODE_C} ${C_TOKEN_C})

set(CFRONT_INC_DIRS ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/include ${CMAKE_BINARY_DIR}/include
    ${CMAKE_CURRENT_BINARY_DIR} ${LIBXML2_INCLUDE_DIR})

add_executable(cfront ${cfront_SRC} ${C_LEXER_C} ${C_EXPRCODE_H} ${C_EXPRCODE_C} ${BISON_C99_parser_OUTPUTS}
    ${BISON_C99_lexer_OUTPUTS})
add_dependencies(cfront generate_cfront_src create_int_install_dirs)

set_target_properties(cfront
                      PROPERTIES
                      RUNTIME_OUTPUT_DIRECTORY ${INT_OMNI_HOME_BIN}
                      OUTPUT_NAME C_Front)

target_include_directories(cfront PRIVATE ${CFRONT_INC_DIRS})

set(CFRONT_DEFINES __CMAKE_BUILD__ PACKAGE_VERSION="${C_PACKAGE_VERSION}")
target_compile_definitions(cfront PRIVATE ${CFRONT_DEFINES})

target_link_libraries(cfront ${LIBXML2_LIBRARY})
