file(GLOB FFRONT_CPP_SRC "*.cpp")

add_executable(ffront-cpp EXCLUDE_FROM_ALL ${FFRONT_CPP_SRC})
add_dependencies(ffront-cpp ffront-cpp-shared)
set_target_properties(ffront-cpp
                      PROPERTIES
                      RUNTIME_OUTPUT_DIRECTORY ${INT_OMNI_HOME_BIN}
                      OUTPUT_NAME ffront-cpp)
set(FFRONT_CPP_LINK_FLAGS ffront-cpp-shared "-Wl,-rpath,'$ORIGIN/../share'")
if(("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU") AND ${LINK_STATIC_GNU_LIBSTDCXX})
  set(FFRONT_CPP_LINK_FLAGS ${FFRONT_CPP_LINK_FLAGS} -static-libstdc++)
endif()
target_link_libraries(ffront-cpp ${FFRONT_CPP_LINK_FLAGS})
