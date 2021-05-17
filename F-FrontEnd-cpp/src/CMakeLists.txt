file(GLOB FFRONT_CPP_SRC "*.cpp")

set(FFRONT_CPP_INC_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/3rdparty)

#add_executable(ffront-cpp EXCLUDE_FROM_ALL ${FFRONT_CPP_SRC})
add_executable(ffront-cpp ${FFRONT_CPP_SRC})
add_dependencies(ffront-cpp ffront-cpp-shared)
set_target_properties(ffront-cpp
                      PROPERTIES
                      RUNTIME_OUTPUT_DIRECTORY ${INT_OMNI_HOME_BIN}
                      OUTPUT_NAME ffront-cpp)
target_include_directories(ffront-cpp PRIVATE ${FFRONT_CPP_INC_DIRS})
set(FFRONT_CPP_LINK_FLAGS ffront-cpp-shared "-Wl,-rpath,'$ORIGIN/../share'")
if(("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU") AND ${LINK_STATIC_GNU_LIBSTDCXX})
  set(FFRONT_CPP_LINK_FLAGS ${FFRONT_CPP_LINK_FLAGS} -static-libstdc++)
endif()
if("${CMAKE_C_COMPILER_ID}" STREQUAL "GNU")
  set(FFRONT_CPP_LINK_FLAGS ${FFRONT_CPP_LINK_FLAGS} stdc++fs)
endif()
target_link_libraries(ffront-cpp ${FFRONT_CPP_LINK_FLAGS})
