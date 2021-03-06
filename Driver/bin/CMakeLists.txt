set(JAVA ${Java_JAVA_EXECUTABLE})

set(F_BACK_DRIVER ${CMAKE_CURRENT_BINARY_DIR}/F_Back)
set(INT_F_BACK_DRIVER ${INT_OMNI_HOME_BIN}/F_Back)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/F_Back.cmake.in ${CMAKE_CURRENT_BINARY_DIR}/tmp/F_Back @ONLY)
file(COPY ${CMAKE_CURRENT_BINARY_DIR}/tmp/F_Back
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
  FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE)

add_custom_command(OUTPUT ${INT_F_BACK_DRIVER}
                   COMMAND ${CMAKE_COMMAND}  -E copy ${F_BACK_DRIVER} ${INT_F_BACK_DRIVER}
                   MAIN_DEPENDENCY ${F_BACK_DRIVER}
                   DEPENDS create_int_install_dirs
                   VERBATIM)

add_custom_target(fback-driver ALL DEPENDS ${INT_F_BACK_DRIVER})
add_dependencies(fback-driver om-f-back)

set(C_BACK_DRIVER ${CMAKE_CURRENT_BINARY_DIR}/C_Back)
set(INT_C_BACK_DRIVER ${INT_OMNI_HOME_BIN}/C_Back)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/C_Back.cmake.in ${CMAKE_CURRENT_BINARY_DIR}/tmp/C_Back @ONLY)
file(COPY ${CMAKE_CURRENT_BINARY_DIR}/tmp/C_Back
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
  FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE)

add_custom_command(OUTPUT ${INT_C_BACK_DRIVER}
                   COMMAND ${CMAKE_COMMAND}  -E copy ${C_BACK_DRIVER} ${INT_C_BACK_DRIVER}
                   MAIN_DEPENDENCY ${C_BACK_DRIVER}
                   DEPENDS create_int_install_dirs
                   VERBATIM)

add_custom_target(cback-driver ALL DEPENDS ${INT_C_BACK_DRIVER})

set(FFRONT_JNI_DRIVER ${CMAKE_CURRENT_BINARY_DIR}/ffront-jni)
set(INT_FFRONT_JNI_DRIVER ${INT_OMNI_HOME_BIN}/ffront-jni)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/ffront-jni.cmake.in ${CMAKE_CURRENT_BINARY_DIR}/tmp/ffront-jni @ONLY)
file(COPY ${CMAKE_CURRENT_BINARY_DIR}/tmp/ffront-jni
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
  FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE)

add_custom_command(OUTPUT ${INT_FFRONT_JNI_DRIVER}
                   COMMAND ${CMAKE_COMMAND}  -E copy ${FFRONT_JNI_DRIVER} ${INT_FFRONT_JNI_DRIVER}
                   MAIN_DEPENDENCY ${FFRONT_JNI_DRIVER}
                   DEPENDS create_int_install_dirs
                   VERBATIM)

add_custom_target(ffront-jni-driver DEPENDS ${INT_FFRONT_JNI_DRIVER})
add_dependencies(ffront-jni-driver ffront-jni)
