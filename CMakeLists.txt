cmake_minimum_required(VERSION 3.0)

include(CheckIncludeFile)
include(CheckFunctionExists)
include(CheckTypeSize)
include(UseJava)
include(cmake/git.cmake)

set(DEFAULT_BUILD_TYPE "Release")

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to '${DEFAULT_BUILD_TYPE}' as none was specified.")
  set(CMAKE_BUILD_TYPE "${DEFAULT_BUILD_TYPE}" CACHE
      STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release")
endif()

IF(CMAKE_BUILD_TYPE MATCHES Debug)
  message("Debug build")
  set(BUILD_TYPE "Debug")
ELSEIF(CMAKE_BUILD_TYPE MATCHES Release)
  message("Release build")
  set(BUILD_TYPE "Release")
ELSE()
  message( FATAL_ERROR "Unsupported build type ${CMAKE_BUILD_TYPE}")
ENDIF()

project("XCodeML-Tools-CPP ${BUILD_TYPE}" VERSION 1.0)

# Recover commit hash for the top repositiory
git_get_rev_hash(${CMAKE_SOURCE_DIR} XCODEML_TOOLS_GIT_HASH)
message(STATUS "XCodeML-Tools-CPP version ${XCODEML_TOOLS_GIT_HASH}")

enable_language(C)
enable_language(CXX)

set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(C_EXTENSIONS OFF)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Java 1.8 REQUIRED)
# Warning!!! After finding the first version, find_package(Java) does NOT look for others. CMAKE_FIND_PACKAGE_SORT_ORDER
# and CMAKE_FIND_PACKAGE_SORT_DIRECTION have no effect. To override the "default" java version, set cmake variable
# JAVA_HOME to desired java installation path.

if(Java_VERSION_MAJOR EQUAL 1)
  if(Java_VERSION_MINOR LESS 8)
    message(FATAL_ERROR "Java ${Java_VERSION_STRING} is not supported")
  endif()
else()
  if(Java_VERSION_MAJOR LESS 8)
    message(FATAL_ERROR "Java ${Java_VERSION_STRING} is not supported")
  endif()
endif()

find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)
find_package(LibXml2 REQUIRED)

find_program(AWK awk mawk gawk)

if(AWK MATCHES ".+-NOTFOUND")
    message(FATAL_ERROR "FATAL: awk (and mawk and gawk) could not be found (${AWK}).")
endif()

option(ENABLE_GNU_MOD_IMPORT "Enable import of gfortran mod files " OFF)

if(ENABLE_GNU_MOD_IMPORT)
  message(FATAL_ERROR "gfortran mod files import is currently not implemented in XCodeML-Tools-CPP")
endif()

set(INT_OMNI_HOME "${CMAKE_BINARY_DIR}/home")
set(INT_OMNI_HOME_BIN "${INT_OMNI_HOME}/bin")
set(INT_OMNI_FFRONT "${INT_OMNI_HOME_BIN}/F_Front")
set(INT_OMNI_HOME_FINCLUDES "${INT_OMNI_HOME}/fincludes")
set(INT_OMNI_HOME_SHARE "${INT_OMNI_HOME}/share")

set(OMNI_HOME "${CMAKE_INSTALL_PREFIX}")

add_custom_target(create_int_install_dirs ALL
    COMMAND ${CMAKE_COMMAND} -E make_directory ${INT_OMNI_HOME}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${INT_OMNI_HOME_BIN}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${INT_OMNI_HOME_SHARE})

install(
    DIRECTORY ${INT_OMNI_HOME}/
    DESTINATION ${CMAKE_INSTALL_PREFIX}
    FILE_PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ
)

set(ADDITIONAL_CLEAN_FILES "")
LIST(APPEND ADDITIONAL_CLEAN_FILES ${INT_OMNI_HOME})
SET_DIRECTORY_PROPERTIES(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "${ADDITIONAL_CLEAN_FILES}")

add_subdirectory(include)

if(("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU") OR ("${CMAKE_C_COMPILER_ID}" STREQUAL "Clang"))
  set(CMAKE_C_FLAGS ${CMAKE_C_FLAGS} -Werror)
  set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} -fno-permissive)
endif()

add_subdirectory(F-FrontEnd/src)

set(CMAKE_JAVA_COMPILE_FLAGS -encoding utf8 -Xlint:deprecation,unchecked)

add_subdirectory(XcodeML-Common/src)
add_subdirectory(F-BackEnd/src)
add_subdirectory(Driver/bin)

