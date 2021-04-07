file(GLOB_RECURSE om_c_back_SRC "*.java")

add_jar(om-c-back
        SOURCES ${om_c_back_SRC}
        OUTPUT_DIR ${INT_OMNI_HOME_SHARE}
        INCLUDE_JARS om-common)

add_dependencies(om-c-back create_int_install_dirs)
