file(GLOB_RECURSE om_f_back_SRC "*.java")

add_jar(om-f-back
        SOURCES ${om_f_back_SRC}
        OUTPUT_DIR ${INT_OMNI_HOME_SHARE}
        INCLUDE_JARS om-common)

add_dependencies(om-f-back create_int_install_dirs)
