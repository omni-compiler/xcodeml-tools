/* Author: Mikhail Zhigun */
#include "xcodeml_f_util_FxCompiler.h"
#include "cli_options.h"
#include <string>
#include <vector>
#include <memory>
#include <functional>
#include <iostream>

extern int execute_cli_opts(const cli_options* opts);

namespace jni
{
    class JavaExceptionDetected
    {};

    inline void assert_no_java_exception(JNIEnv * env) {
        if (env->ExceptionCheck()==JNI_TRUE)
            throw JavaExceptionDetected();
    }

    class Context
    {
    public:
        Context(JNIEnv * env):
            env{env},
            Boolean{env->FindClass("java/lang/Boolean")},
            Integer{env->FindClass("java/lang/Integer")},
            List{env->FindClass("java/util/List")},
            Path{env->FindClass("java/nio/file/Path")},
            CppException{env->FindClass("xcodeml/f/util/NativeUtils$CppException")},
            Error{env->FindClass("java/lang/Error")},
            CLIOptions{env->FindClass("xcodeml/f/util/CLIOptions")},
            get_list_size{env->GetMethodID(List, "size", "()I")},
            get_list_element{env->GetMethodID(List, "get", "(I)Ljava/lang/Object;")},
            get_integer_value{env->GetMethodID(Integer, "intValue", "()I")},
            get_boolean_value{env->GetMethodID(Boolean, "booleanValue", "()Z")},
            path_to_string{env->GetMethodID(Path, "toString", "()Ljava/lang/String;")}
        {
            assert_no_java_exception(env);
        }

        JNIEnv* const env;
        const jclass Boolean;
        const jclass Integer;
        const jclass Path;
        const jclass List;
        const jclass CppException;
        const jclass Error;
        const jclass CLIOptions;
        const jmethodID get_list_size;
        const jmethodID get_list_element;
        const jmethodID get_integer_value;
        const jmethodID get_boolean_value;
        const jmethodID path_to_string;
    };

    std::string to_string(const Context& ctxJNI, jstring str)
    {
        JNIEnv* const env = ctxJNI.env;
        jboolean is_copy;
        const char* utf8_chrs = env->GetStringUTFChars(str, &is_copy);
        std::string res(utf8_chrs);
        if (is_copy == JNI_TRUE) {
            ctxJNI.env->ReleaseStringUTFChars(str, utf8_chrs);
        }
        assert_no_java_exception(env);
        return res;
    }

    std::unique_ptr<std::string> get_path_field(const Context& ctxJNI, jclass cls, jobject obj, const char* field_name)
    {

        JNIEnv* const env = ctxJNI.env;
        jfieldID field_id = env->GetFieldID(cls, field_name, "Ljava/nio/file/Path;");
        assert_no_java_exception(env);
        jobject objPath = env->GetObjectField(obj, field_id);
        assert_no_java_exception(env);
        std::unique_ptr<std::string> res;
        if(env->IsSameObject(objPath, NULL) == JNI_FALSE)
        {
            jobject objStr = static_cast<jstring>(env->CallObjectMethod(objPath, ctxJNI.path_to_string));
            assert_no_java_exception(env);
            res = std::make_unique<std::string>(to_string(env, static_cast<jstring>(objStr)));
        }
        assert_no_java_exception(env);
        return res;
    }

    std::vector<std::string> get_path_list_field(const Context& ctxJNI, jclass cls, jobject obj, const char* field_name)
    {
        JNIEnv* const env = ctxJNI.env;
        jclass List = ctxJNI.List;
        jmethodID get_list_size = ctxJNI.get_list_size;
        jmethodID get_list_element = ctxJNI.get_list_element;
        jfieldID field_id = env->GetFieldID(cls, field_name, "Ljava/util/List;");
        assert_no_java_exception(env);
        jobject field_obj = env->GetObjectField(obj, field_id);
        assert_no_java_exception(env);

        const jint len = env->CallIntMethod(field_obj, get_list_size);
        assert_no_java_exception(env);
        std::vector<std::string> res;
        res.reserve(len);
        for(int i = 0; i < len; ++i)
        {
            jobject elPath = static_cast<jstring>(env->CallObjectMethod(field_obj, get_list_element, i));
            assert_no_java_exception(env);
            jstring elStr =  static_cast<jstring>(env->CallObjectMethod(elPath, ctxJNI.path_to_string));
            assert_no_java_exception(env);
            res.push_back(to_string(env, elStr));
        }
        assert_no_java_exception(env);
        return res;
    }

    std::unique_ptr<std::string> get_string_field(const Context& ctxJNI, jclass cls, jobject obj, const char* field_name)
    {

        JNIEnv* const env = ctxJNI.env;
        jfieldID field_id = env->GetFieldID(cls, field_name, "Ljava/lang/String;");
        jobject objStr = env->GetObjectField(obj, field_id);
        std::unique_ptr<std::string> res;
        if(env->IsSameObject(objStr, NULL) == JNI_FALSE)
        {
            res = std::make_unique<std::string>(to_string(env, static_cast<jstring>(objStr)));
        }
        assert_no_java_exception(env);
        return res;
    }

    std::vector<std::string> get_string_list_field(const Context& ctxJNI, jclass cls, jobject obj, const char* field_name)
    {
        JNIEnv* const env = ctxJNI.env;
        jclass List = ctxJNI.List;
        jmethodID get_list_size = ctxJNI.get_list_size;
        jmethodID get_list_element = ctxJNI.get_list_element;
        jfieldID field_id = env->GetFieldID(cls, field_name, "Ljava/util/List;");
        jobject field_obj = env->GetObjectField(obj, field_id);

        const jint len = env->CallIntMethod(field_obj, get_list_size);
        std::vector<std::string> res;
        res.reserve(len);
        for(int i = 0; i < len; ++i)
        {
            jstring element = static_cast<jstring>(env->CallObjectMethod(field_obj, get_list_element, i));
            auto el_str = to_string(env, element);
            res.push_back(move(el_str));
        }
        assert_no_java_exception(env);
        return res;
    }

    std::unique_ptr<bool> get_boolean_field(const Context& ctxJNI, jclass cls, jobject obj, const char* field_name)
    {
        JNIEnv* const env = ctxJNI.env;
        jmethodID get_boolean_value = ctxJNI.get_boolean_value;
        jfieldID field_id = env->GetFieldID(cls, field_name, "Ljava/lang/Boolean;");
        assert_no_java_exception(env);
        jobject objBoolean = env->GetObjectField(obj, field_id);
        assert_no_java_exception(env);
        std::unique_ptr<bool> res;
        if(env->IsSameObject(objBoolean, NULL) == JNI_FALSE)
        {
            jboolean val = env->CallBooleanMethod(objBoolean, get_boolean_value);
            res = std::make_unique<bool>(val == JNI_TRUE);
        }
        assert_no_java_exception(env);
        return res;
    }

    std::unique_ptr<int> get_integer_field(const Context& ctxJNI, jclass cls, jobject obj, const char* field_name)
    {
        JNIEnv* const env = ctxJNI.env;
        jmethodID get_integer_value = ctxJNI.get_integer_value;
        jfieldID field_id = env->GetFieldID(cls, field_name, "Ljava/lang/Integer;");
        assert_no_java_exception(env);
        jobject objInt = env->GetObjectField(obj, field_id);
        assert_no_java_exception(env);
        std::unique_ptr<int> res;
        if(env->IsSameObject(objInt, NULL) == JNI_FALSE)
        {
            jint val = env->CallIntMethod(objInt, get_integer_value);
            res = std::make_unique<int>(val);
        }
        assert_no_java_exception(env);
        return res;
    }

    void rethrow_cpp_exception(const Context& ctxJNI, const char* msg)
    {
        JNIEnv* const env = ctxJNI.env;
        jclass CppException = ctxJNI.CppException;
        env->ThrowNew(CppException, msg);
    }
}

#define set_path(field_name) \
    { \
        auto path = get_path_field(ctxJNI, ctxJNI.CLIOptions, optsJava, (#field_name)); \
        if(path) \
           set_##field_name(opts, path->c_str()); \
    }
#define set_path_vector(field_name) \
    { \
        auto paths = get_path_list_field(ctxJNI, ctxJNI.CLIOptions, optsJava, (#field_name "s")); \
        for(const auto& path: paths) \
           add_##field_name(opts, path.c_str()); \
    }
#define set_int(field_name) \
    { \
        auto val = get_integer_field(ctxJNI, ctxJNI.CLIOptions, optsJava, (#field_name)); \
        if(val) \
           set_##field_name(opts, *val); \
    }
#define set_bool(field_name) \
    { \
        auto val = get_boolean_field(ctxJNI, ctxJNI.CLIOptions, optsJava, (#field_name)); \
        if(val) \
           set_##field_name(opts, *val); \
    }

void set_cli_opts(const jni::Context& ctxJNI, cli_options* opts, jobject optsJava)
{
    using namespace jni;
    auto get_path_list = [&](const char* field_name) { return get_path_field(ctxJNI, ctxJNI.CLIOptions, optsJava, field_name); };
    auto get_int = [&](const char* field_name) { return get_integer_field(ctxJNI, ctxJNI.CLIOptions, optsJava, field_name); };
    auto get_bool = [&](const char* field_name) { return get_path_list_field(ctxJNI, ctxJNI.CLIOptions, optsJava, field_name); };
    set_path(src_file_path);
    set_path(out_file_path);
    set_path(intrinsic_xmod_dir_path);
    set_path_vector(inc_dir_path);
    set_int(max_line_len);
    set_int(max_cont_line);
    set_int(auto_save_attr_kb);
    set_int(max_name_len);
    set_int(default_single_real_type_size);
    set_int(default_double_real_type_size);
    {
        auto f77 = get_boolean_field(ctxJNI, ctxJNI.CLIOptions, optsJava, "lang_f77");
        if(f77 && (*f77))
        {
            set_lang_spec_set(opts, F77_SPEC);
        }
    }
    {
        auto f90 = get_boolean_field(ctxJNI, ctxJNI.CLIOptions, optsJava, "lang_f90");
        if(f90 && (*f90))
        {
            set_lang_spec_set(opts, F90_SPEC);
        }
    }
    {
        auto f95 = get_boolean_field(ctxJNI, ctxJNI.CLIOptions, optsJava, "lang_f95");
        if(f95 && (*f95))
        {
            set_lang_spec_set(opts, F95_SPEC);
        }
    }
    set_bool(debug_enabled);
    set_bool(yacc_debug_enabled);
    set_bool(module_compile_enabled);
    set_bool(omp_enabled);
    set_bool(xmp_enabled);
    set_bool(xmp_coarray_enabled);
    set_bool(acc_enabled);
    set_bool(cond_compile_enabled);
    set_bool(leave_comment_enabled);
    set_bool(do_implicit_undef);
    set_bool(force_fixed_format_enabled);
    set_bool(force_c_comments_enabled);
    set_bool(dollar_in_id_enabled);
    set_bool(end_line_no_enabled);
    set_bool(ocl_enabled);
    set_bool(cdir_enabled);
    set_bool(pgi_enabled);
    set_bool(module_cache_enabled);
    set_bool(add_timestamp_enabled);
    set_bool(print_help);
    set_bool(print_opts);
}

/*
 * Class:     xcodeml_f_util_FxCompiler
 * Method:    execute
 * Signature: (Lxcodeml/f/util/CLIOptions;)I
 */
jint JNICALL Java_xcodeml_f_util_FxCompiler_execute
  (JNIEnv * env, jclass, jobject optsJava)
{
    try
    {
        jni::Context ctxJNI{env};
        try
        {
            cli_options opts;
            init_cli_options(&opts);
            set_cli_opts(ctxJNI, &opts, optsJava);
            const int ret_code = execute_cli_opts(&opts);
           	free_cli_options(&opts);
            return ret_code;
        }
        catch(const jni::JavaExceptionDetected&)
        {
            return 1;
        }
        catch(const std::exception& e)
        {
            jni::rethrow_cpp_exception(env, e.what());
            return 1;
        }
        catch(...)
        {
            jni::rethrow_cpp_exception(env, "Unknown exception");
            return 1;
        }
    }
    catch(const jni::JavaExceptionDetected&)
    {
        return 1;
    }
}
