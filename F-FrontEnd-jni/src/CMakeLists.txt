if(${JNI_FOUND})

file(GLOB_RECURSE ffront_java_SRC "${CMAKE_CURRENT_SOURCE_DIR}/java/*.java")

set(ARGPARSE4J_VERSION "0.9.0")
set(ARGPARSE4J_FILENAME "argparse4j-${ARGPARSE4J_VERSION}.jar")
set(ARGPARSE4J_JAR "${INT_OMNI_HOME_SHARE_3RDPARTY}/${ARGPARSE4J_FILENAME}")
set(ARGPARSE4J_URL "https://github.com/argparse4j/argparse4j/releases/download/argparse4j-${ARGPARSE4J_VERSION}/${ARGPARSE4J_FILENAME}")

include(ExternalProject)

ExternalProject_Add(
    argparse4j
    EXCLUDE_FROM_ALL ON
    URL "${ARGPARSE4J_URL}"
    DOWNLOAD_DIR "${CMAKE_CURRENT_BINARY_DIR}/3rdparty"
    DOWNLOAD_NO_EXTRACT OFF
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_BINARY_DIR}/3rdparty/${ARGPARSE4J_FILENAME}" ${ARGPARSE4J_JAR}
    BUILD_BYPRODUCTS ${ARGPARSE4J_JAR}
    INSTALL_COMMAND ""
)

add_custom_command(OUTPUT ${ARGPARSE4J_JAR}
                   COMMAND COMMAND ${CMAKE_COMMAND} -E copy_if_different  "${CMAKE_CURRENT_BINARY_DIR}/3rdparty/${ARGPARSE4J_FILENAME}" ${ARGPARSE4J_JAR}
                   DEPENDS argparse4j
                   VERBATIM)
add_custom_target(argparse4j_jar DEPENDS ${ARGPARSE4J_JAR})

set(JAVA_C_HEADERS_DIR "${CMAKE_CURRENT_BINARY_DIR}/java-c-headers")

add_jar(ffront-java
        INCLUDE_JARS ${ARGPARSE4J_JAR}
        SOURCES ${ffront_java_SRC}
        OUTPUT_NAME ffront
        ENTRY_POINT xcodeml.f.util.main
        OUTPUT_DIR ${INT_OMNI_HOME_SHARE}
        GENERATE_NATIVE_HEADERS ffront-java-native-headers DESTINATION ${JAVA_C_HEADERS_DIR})
set_property(TARGET ffront-java
             PROPERTY EXCLUDE_FROM_ALL true)
add_dependencies(ffront-java create_int_install_dirs argparse4j argparse4j_jar)

file(GLOB FFRONT_JNI_CPP_SRC "cpp/*.cpp")

add_library(ffront-jni SHARED EXCLUDE_FROM_ALL ${FFRONT_JNI_CPP_SRC})
add_dependencies(ffront-jni generate_ffront_src create_int_install_dirs ffront-java)
set_target_properties(ffront-jni
                      PROPERTIES
                      LIBRARY_OUTPUT_DIRECTORY ${INT_OMNI_HOME_SHARE}
                      OUTPUT_NAME ffront-jni)
target_include_directories(ffront-jni PRIVATE ${FFRONT_INC_DIRS} ${JAVA_C_HEADERS_DIR} ${JNI_INCLUDE_DIRS})
target_compile_definitions(ffront-jni PRIVATE ${FFRONT_DEFINES})
target_link_libraries(ffront-jni ffront-cpp-shared ${JNI_LIBRARIES} ${LIBXML2_LIBRARY} "-Wl,-rpath,'$ORIGIN/.'")

else()
  message(WARNING "JNI not found, java bindings will be unavailable")
endif() # JNI_FOUND
