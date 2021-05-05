/* Author: Mikhail Zhigun */
#include "xcodeml_f_util_FxCompiler.h"
#include "cli_options.h"
#include "io_cache.h"
#include <string>
#include <vector>
#include <memory>
#include <functional>
#include <iostream>
#include <string.h>

extern int execute_cli_opts(const cli_options *opts, io_cache files_cache);

namespace jni
{
class JavaExceptionDetected
{
};

void assert_no_java_exception(JNIEnv *env)
{
    if (env->ExceptionCheck() == JNI_TRUE)
        throw JavaExceptionDetected();
}

void rethrow_cpp_exception(JNIEnv *env, const char *msg)
{
    auto CppException = env->FindClass(
            "xcodeml/f/util/NativeUtils$CppException");
    env->ThrowNew(CppException, msg);
}

class Context
{
public:
    Context(JNIEnv *env) :
            env
            { env }, Boolean
            { env->FindClass("java/lang/Boolean") }, Integer
            { env->FindClass("java/lang/Integer") }, List
            { env->FindClass("java/util/List") }, Path
            { env->FindClass("java/nio/file/Path") }, CppException
            { env->FindClass("xcodeml/f/util/NativeUtils$CppException") }, Error
            { env->FindClass("java/lang/Error") }, CLIOptions
            { env->FindClass("xcodeml/f/util/CLIOptions") }, IOCache
            { env->FindClass("xcodeml/f/util/NativeUtils$IOCache") }, IOCacheInputEntry
            { env->FindClass("xcodeml/f/util/NativeUtils$IOCacheInputEntry") }, IOCacheOutputEntry
            { env->FindClass("xcodeml/f/util/NativeUtils$IOCacheOutputEntry") }, MappedByteBuffer
            { env->FindClass("java/nio/MappedByteBuffer") }, get_list_size
            { env->GetMethodID(List, "size", "()I") }, get_list_element
            { env->GetMethodID(List, "get", "(I)Ljava/lang/Object;") }, get_integer_value
            { env->GetMethodID(Integer, "intValue", "()I") }, get_boolean_value
            { env->GetMethodID(Boolean, "booleanValue", "()Z") }, path_to_string
            { env->GetMethodID(Path, "toString", "()Ljava/lang/String;") }, iocache_get_input_files
            { env->GetMethodID(IOCache, "getInputFiles", "()Ljava/util/List;") }, iocache_get_output_files
            { env->GetMethodID(IOCache, "getOutputFiles", "()Ljava/util/List;") }, IOCacheOutputEntry_allocateData
            { env->GetMethodID(IOCacheOutputEntry, "allocateData", "(I)V") }
    {
        assert_no_java_exception(env);
    }

    JNIEnv *const env;
    const jclass Boolean;
    const jclass Integer;
    const jclass Path;
    const jclass List;
    const jclass CppException;
    const jclass Error;
    const jclass CLIOptions;
    const jclass IOCache;
    const jclass IOCacheInputEntry;
    const jclass IOCacheOutputEntry;
    const jclass MappedByteBuffer;
    const jmethodID get_list_size;
    const jmethodID get_list_element;
    const jmethodID get_integer_value;
    const jmethodID get_boolean_value;
    const jmethodID path_to_string;
    const jmethodID iocache_get_input_files;
    const jmethodID iocache_get_output_files;
    const jmethodID IOCacheOutputEntry_allocateData;
};

class Functions: Context
{
    using Context::Context;

    void assert_no_java_exception()
    {
        ::jni::assert_no_java_exception(env);
    }

    std::string to_string(jstring str)
    {
        jboolean is_copy;
        const char *utf8_chrs = env->GetStringUTFChars(str, &is_copy);
        std::string res(utf8_chrs);
        if (is_copy == JNI_TRUE)
        {
            env->ReleaseStringUTFChars(str, utf8_chrs);
        }
        assert_no_java_exception();
        return res;
    }

    std::unique_ptr<std::string> get_path_field(jclass cls, jobject obj,
            const char *field_name)
    {

        jfieldID field_id = env->GetFieldID(cls, field_name,
                "Ljava/nio/file/Path;");
        assert_no_java_exception();
        jobject objPath = env->GetObjectField(obj, field_id);
        assert_no_java_exception();
        std::unique_ptr<std::string> res;
        if (env->IsSameObject(objPath, NULL) == JNI_FALSE)
        {
            jobject objStr = static_cast<jstring>(env->CallObjectMethod(objPath,
                    path_to_string));
            assert_no_java_exception();
            res = std::make_unique<std::string>(
                    to_string(static_cast<jstring>(objStr)));
        }
        assert_no_java_exception();
        return res;
    }

    std::vector<std::string> get_path_list_field(jclass cls, jobject obj,
            const char *field_name)
    {
        jfieldID field_id = env->GetFieldID(cls, field_name,
                "Ljava/util/List;");
        assert_no_java_exception();
        jobject field_obj = env->GetObjectField(obj, field_id);
        assert_no_java_exception();

        const jint len = env->CallIntMethod(field_obj, get_list_size);
        assert_no_java_exception();
        std::vector<std::string> res;
        res.reserve(len);
        for (int i = 0; i < len; ++i)
        {
            jobject elPath = static_cast<jstring>(env->CallObjectMethod(
                    field_obj, get_list_element, i));
            assert_no_java_exception();
            jstring elStr = static_cast<jstring>(env->CallObjectMethod(elPath,
                    path_to_string));
            assert_no_java_exception();
            res.push_back(to_string(elStr));
        }
        assert_no_java_exception();
        return res;
    }

    std::unique_ptr<std::string> get_string_field(jclass cls, jobject obj,
            const char *field_name)
    {

        jfieldID field_id = env->GetFieldID(cls, field_name,
                "Ljava/lang/String;");
        jobject objStr = env->GetObjectField(obj, field_id);
        std::unique_ptr<std::string> res;
        if (env->IsSameObject(objStr, NULL) == JNI_FALSE)
        {
            res = std::make_unique<std::string>(
                    to_string(static_cast<jstring>(objStr)));
        }
        assert_no_java_exception();
        return res;
    }

    std::vector<std::string> get_string_list_field(jclass cls, jobject obj,
            const char *field_name)
    {
        jfieldID field_id = env->GetFieldID(cls, field_name,
                "Ljava/util/List;");
        jobject field_obj = env->GetObjectField(obj, field_id);

        const jint len = env->CallIntMethod(field_obj, get_list_size);
        std::vector<std::string> res;
        res.reserve(len);
        for (int i = 0; i < len; ++i)
        {
            jstring element = static_cast<jstring>(env->CallObjectMethod(
                    field_obj, get_list_element, i));
            auto el_str = to_string(element);
            res.push_back(move(el_str));
        }
        assert_no_java_exception();
        return res;
    }

    std::unique_ptr<bool> get_boolean_field(jclass cls, jobject obj,
            const char *field_name)
    {
        jfieldID field_id = env->GetFieldID(cls, field_name,
                "Ljava/lang/Boolean;");
        assert_no_java_exception();
        jobject objBoolean = env->GetObjectField(obj, field_id);
        assert_no_java_exception();
        std::unique_ptr<bool> res;
        if (env->IsSameObject(objBoolean, NULL) == JNI_FALSE)
        {
            jboolean val = env->CallBooleanMethod(objBoolean,
                    get_boolean_value);
            res = std::make_unique<bool>(val == JNI_TRUE);
        }
        assert_no_java_exception();
        return res;
    }

    std::unique_ptr<int> get_integer_field(jclass cls, jobject obj,
            const char *field_name)
    {
        jfieldID field_id = env->GetFieldID(cls, field_name,
                "Ljava/lang/Integer;");
        assert_no_java_exception();
        jobject objInt = env->GetObjectField(obj, field_id);
        assert_no_java_exception();
        std::unique_ptr<int> res;
        if (env->IsSameObject(objInt, NULL) == JNI_FALSE)
        {
            jint val = env->CallIntMethod(objInt, get_integer_value);
            res = std::make_unique<int>(val);
        }
        assert_no_java_exception();
        return res;
    }

    int get_int_field(jclass cls, jobject obj, const char *field_name)
    {
        jfieldID field_id = env->GetFieldID(cls, field_name, "I");
        assert_no_java_exception();
        const int val = env->GetIntField(obj, field_id);
        assert_no_java_exception();
        return val;
    }

    jobject get_mappedbytebuf_field(jclass cls, jobject obj,
            const char *field_name)
    {
        jfieldID field_id = env->GetFieldID(cls, field_name,
                "Ljava/nio/MappedByteBuffer;");
        assert_no_java_exception();
        jobject buf = env->GetObjectField(obj, field_id);
        return buf;
    }

    void rethrow_cpp_exception(const char *msg)
    {
        env->ThrowNew(CppException, msg);
    }

public:
    io_cache create_cache(jobject jFilesCache)
    {
        io_cache filesCache = create_io_cache();
        {
            jobject inputFilesEntries = env->CallObjectMethod(jFilesCache,
                    iocache_get_input_files);
            assert_no_java_exception();
            const jint inputFilesEntriesLen = env->CallIntMethod(
                    inputFilesEntries, get_list_size);
            assert_no_java_exception();
            for (int i = 0; i < inputFilesEntriesLen; ++i)
            {
                jobject entry = env->CallObjectMethod(inputFilesEntries,
                        get_list_element, i);
                assert_no_java_exception();
                const auto filename = get_string_field(IOCacheInputEntry, entry,
                        "filename");
                const size_t size = (size_t) get_int_field(IOCacheInputEntry,
                        entry, "size");
                jobject mappedBufObj = get_mappedbytebuf_field(
                        IOCacheInputEntry, entry, "data");
                void *mappedBuf = env->GetDirectBufferAddress(mappedBufObj);
                assert_no_java_exception();
                if (!mappedBuf)
                {
                    throw std::runtime_error(
                            "GetDirectBufferAddress failed");
                }
                io_cache_add_input_file(filesCache, filename->c_str(),
                        mappedBuf, size);
            }
        }
        {
            jobject outputFilesEntries = env->CallObjectMethod(jFilesCache,
                    iocache_get_output_files);
            assert_no_java_exception();
            const jint outputFilesEntriesLen = env->CallIntMethod(
                    outputFilesEntries, get_list_size);
            assert_no_java_exception();
            for (int i = 0; i < outputFilesEntriesLen; ++i)
            {
                jobject entry = env->CallObjectMethod(outputFilesEntries,
                        get_list_element, i);
                assert_no_java_exception();
                const auto filename = get_string_field(IOCacheInputEntry, entry,
                        "filename");
                io_cache_add_output_file(filesCache, filename->c_str());
            }

        }
        return filesCache;
    }

    void update_cache(jobject jFilesCache, io_cache filesCache)
    {
        jobject outputFilesEntries = env->CallObjectMethod(jFilesCache,
                iocache_get_output_files);
        assert_no_java_exception();
        const jint outputFilesEntriesLen = env->CallIntMethod(
                outputFilesEntries, get_list_size);
        assert_no_java_exception();
        for (int i = 0; i < outputFilesEntriesLen; ++i)
        {
            jobject entry = env->CallObjectMethod(outputFilesEntries,
                    get_list_element, i);
            assert_no_java_exception();
            const auto filename = get_string_field(IOCacheOutputEntry, entry,
                    "filename");
            void *startAddress = nullptr;
            size_t size = 0;
            io_cache_get_output_file_as_mem(filesCache, filename->c_str(),
                    &startAddress, &size);
            env->CallVoidMethod(entry, IOCacheOutputEntry_allocateData,
                    (int) size);
            assert_no_java_exception();
            if(size > 0)
            {
                jobject mappedBufObj = get_mappedbytebuf_field(IOCacheOutputEntry,
                        entry, "data");
                void *mappedBuf = env->GetDirectBufferAddress(mappedBufObj);
                assert_no_java_exception();
                memcpy(mappedBuf, startAddress, size);
            }
        }
    }

#define set_path(field_name) \
    { \
        auto path = get_path_field(CLIOptions, optsJava, (#field_name)); \
        if(path) \
           set_##field_name(opts, path->c_str()); \
    }
#define set_path_vector(field_name) \
    { \
        auto paths = get_path_list_field(CLIOptions, optsJava, (#field_name "s")); \
        for(const auto& path: paths) \
           add_##field_name(opts, path.c_str()); \
    }
#define set_int(field_name) \
    { \
        auto val = get_integer_field(CLIOptions, optsJava, (#field_name)); \
        if(val) \
           set_##field_name(opts, *val); \
    }
#define set_bool(field_name) \
    { \
        auto val = get_boolean_field(CLIOptions, optsJava, (#field_name)); \
        if(val) \
           set_##field_name(opts, *val); \
    }

    void set_cli_opts(cli_options *opts, jobject optsJava)
    {
        using namespace jni;
        auto get_path_list = [&](const char *field_name)
        {
            return get_path_field(CLIOptions, optsJava, field_name);
        };
        auto get_int = [&](const char *field_name)
        {
            return get_integer_field(CLIOptions, optsJava, field_name);
        };
        auto get_bool = [&](const char *field_name)
        {
            return get_path_list_field(CLIOptions, optsJava, field_name);
        };
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
            auto f77 = get_boolean_field(CLIOptions, optsJava, "lang_f77");
            if (f77 && (*f77))
            {
                set_lang_spec_set(opts, F77_SPEC);
            }
        }
        {
            auto f90 = get_boolean_field(CLIOptions, optsJava, "lang_f90");
            if (f90 && (*f90))
            {
                set_lang_spec_set(opts, F90_SPEC);
            }
        }
        {
            auto f95 = get_boolean_field(CLIOptions, optsJava, "lang_f95");
            if (f95 && (*f95))
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

};
}

/*
 * Class:     xcodeml_f_util_FxCompiler
 * Method:    execute
 * Signature: (Lxcodeml/f/util/CLIOptions;Lxcodeml/f/util/NativeUtils/IOCache;)I
 */
jint JNICALL Java_xcodeml_f_util_FxCompiler_execute(JNIEnv *env, jclass,
        jobject optsJava, jobject jFilesCache)
{
    try
    {
        jni::Functions ctx
        { env };
        try
        {
            cli_options opts;
            init_cli_options(&opts);
            ctx.set_cli_opts(&opts, optsJava);
            int ret_code;
            if (env->IsSameObject(jFilesCache, NULL) == JNI_TRUE)
            {
                ret_code = execute_cli_opts(&opts, nullptr);
            }
            else
            {
                io_cache filesCache = ctx.create_cache(jFilesCache);
                ret_code = execute_cli_opts(&opts, filesCache);
                ctx.update_cache(jFilesCache, filesCache);
                free_io_cache(filesCache);
            }
            free_cli_options(&opts);
            return ret_code;
        } catch (const jni::JavaExceptionDetected&)
        {
            return 1;
        } catch (const std::exception &e)
        {
            jni::rethrow_cpp_exception(env, e.what());
            return 1;
        } catch (...)
        {
            jni::rethrow_cpp_exception(env, "Unknown exception");
            return 1;
        }
    } catch (const jni::JavaExceptionDetected&)
    {
        return 1;
    }
}
