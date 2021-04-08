set(ISO_C_BINDING_F90 "${CMAKE_CURRENT_BINARY_DIR}/iso_c_binding.f90")
set(ISO_C_BINDING_XMOD "${INT_OMNI_HOME_FINCLUDES}/iso_c_binding.xmod")

configure_file("${CMAKE_CURRENT_SOURCE_DIR}/iso_c_binding.f90.in" "${ISO_C_BINDING_F90}" @ONLY)

set(ISO_FORTRAN_ENV_F90 "${CMAKE_CURRENT_SOURCE_DIR}/iso_fortran_env.f90")
set(ISO_FORTRAN_ENV_XMOD "${INT_OMNI_HOME_FINCLUDES}/iso_fortran_env.xmod")

add_custom_command(OUTPUT ${ISO_C_BINDING_XMOD}
                   COMMAND ${CMAKE_COMMAND} -E make_directory ${INT_OMNI_HOME_FINCLUDES}
                   COMMAND ${INT_OMNI_FFRONT} -module-compile ${ISO_C_BINDING_F90} > ${ISO_C_BINDING_XMOD}
                   MAIN_DEPENDENCY ${ISO_C_BINDING_F90}
                   DEPENDS ffront
                   COMMENT "Generating iso_c_binding.xmod"
                   VERBATIM)

add_custom_command(OUTPUT ${ISO_FORTRAN_ENV_XMOD}
                   COMMAND ${CMAKE_COMMAND} -E make_directory ${INT_OMNI_HOME_FINCLUDES}
                   COMMAND ${INT_OMNI_FFRONT} -module-compile ${ISO_FORTRAN_ENV_F90} > ${ISO_FORTRAN_ENV_XMOD}
                   MAIN_DEPENDENCY ${ISO_FORTRAN_ENV_F90}
                   DEPENDS ffront
                   COMMENT "Generating ISO_FORTRAN_ENV.xmod"
                   VERBATIM)

add_custom_target(std-lib-xmods ALL DEPENDS ${ISO_C_BINDING_XMOD} ${ISO_FORTRAN_ENV_XMOD})




