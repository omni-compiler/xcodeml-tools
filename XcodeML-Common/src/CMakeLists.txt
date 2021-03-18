file(GLOB_RECURSE om_common_SRC "*.java")

add_jar(om-common
        SOURCES ${om_common_SRC}
        OUTPUT_DIR ${INT_OMNI_HOME_SHARE})

add_dependencies(om-common create_int_install_dirs)
