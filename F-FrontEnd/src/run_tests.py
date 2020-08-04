#!/usr/bin/env python3
#-*- coding: utf-8 -*-

import sys

assert sys.version_info[0] >= 3 and sys.version_info[1] >= 6, 'Python >= 3.6 is required'

import argparse, os, locale, shutil, pathlib, tempfile, multiprocessing, subprocess, traceback, time, re
from os.path import join as join_path, realpath as real_path
from enum import Enum
from typing import Tuple, NamedTuple, List, Optional, Dict, Union
from textwrap import wrap

THIS_DIR_PATH = os.path.dirname(os.path.realpath(__file__))
TEST_DATA_DEFAULT_RELATIVE_PATH = '../../F-FrontEnd/test/testdata'
DEFAULT_ERROR_LOG_FILENAME = 'errors.log'
TMPFS_DIR = '/dev/shm'
TEST_TIMEOUT = 10 # seconds

def file_exists(path: str):
    return os.path.exists(path) and os.path.isfile(path)


def dir_exists(path: str):
    return os.path.exists(path) and os.path.isdir(path)


def get_file_as_str(path: str):
    with open(path, 'r') as f:
        s = f.read().replace('\n', ' ').strip()
        return s


class NativeCompiler(Enum):
    GFORTRAN = 1
    PGFORTRAN = 2
    UNKNOWN = 3

class TesterArgs(NamedTuple):
    num_parallel_tests : int
    frontend : str
    backend : str
    xmodules_dir : str
    native_compiler_type : NativeCompiler
    native_compiler : str
    errors_log : str
    test_data_dir : str
    test_case : Optional[str]
    verbose_output : bool
    obj_sym_reader : str

class TestingStage(Enum):
    DEPENDENCIES_PREP = 0
    FRONTEND = 1
    BACKEND = 2
    NATIVE = 3
    LINK = 4
    SYMBOLS_READ = 5
    EXECUTION = 6
    EXPECTED_OUTPUT = 7
    REFERENCE_OUTPUT = 8


class TestingStageInfo(NamedTuple):
    type: TestingStage
    result: bool
    args: Optional[str]
    error_log: Optional[str]


def to_paragraph(p: Union[str, bytes], prefix: str = ''):
    if isinstance(p, bytes):
        p = p.decode('utf-8')
    return '\n'.join([prefix + l for l in wrap(p, width=100)])

class TestResult(NamedTuple):
    def text_summary(self) -> str:
        txt = ''
        for s in self.stages:
            txt += '%s\n' % s.type
            if not s.result:
                if s.args is not None:
                    txt += 'Args:\n%s\n' % to_paragraph(s.args, '\t')
                if s.error_log is not None:
                    txt += 'Output:\n%s\n' % to_paragraph(s.error_log, '\t')
        if self.exception is not None:
            txt += 'Exception:\n' + to_paragraph(self.exception, '\t')
        return txt
    stages: List[TestingStageInfo]
    result: bool
    exception: Optional[str]


class ReturnCode(Enum):
    SUCCESS = 0
    FAILURE = 1


RC = ReturnCode


class TestRunner:

    @property
    def args(self):
        return self.__args

    def __init__(self):
        self.__args = self.parse_args()

    @staticmethod
    def bool_str(v):
        if isinstance(v, bool):
            return v
        elif v.lower() in ('yes', 'true', 't', 'y', '1'):
            return True
        elif v.lower() in ('no', 'false', 'f', 'n', '0'):
            return False
        else:
            raise argparse.ArgumentTypeError('Boolean value expected')
    @staticmethod
    def get_native_compiler_type(native_compiler : str) -> NativeCompiler:
        native_compiler_type = None
        p = subprocess.run(args=[native_compiler, '--version'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        assert p.returncode == 0, 'Native compiler check failed'
        info = p.stdout.decode('utf-8').lower()
        if 'gnu fortran' in info:
            return NativeCompiler.GFORTRAN
        elif 'pgfortran' in info:
            return NativeCompiler.PGFORTRAN
        else:
            return NativeCompiler.UNKNOWN
    def parse_args(self):
        parser = argparse.ArgumentParser(description='Test runner for xcodeml-tools Fortran frontend and backend')
        parser.add_argument('-f', '--frontend-bin', type=str, required=True,
                            help='Path to frontend executable F_front')
        parser.add_argument('-b', '--backend-bin', type=str, required=True,
                            help='Path to backend executable F_back')
        parser.add_argument('-x', '--xmodules-dir', type=str, required=True,
                            help='Directory, where intrinsic modules are located')
        parser.add_argument('-v', '--verbose', type=TestRunner.bool_str, default=False,
                            help='Verbose output')
        parser.add_argument('-d', '--input-test-dir', type=str,
                            default=real_path(join_path(THIS_DIR_PATH, TEST_DATA_DEFAULT_RELATIVE_PATH)),
                            help='Input test data directory (default: %s)' % TEST_DATA_DEFAULT_RELATIVE_PATH)
        parser.add_argument('-i', '--input-test', type=str, default=None,
                            help='Specific input test file basename.')
        #gfortran version is important! Currently some of the tests fail with gfortran 9 and 10
        parser.add_argument('-n', '--native-compiler', type=str, default='gfortran-7',
                            help='Path to fortran compiler (currently only gfortran is supported)')
        parser.add_argument('-e', '--error-log', type=str, default=DEFAULT_ERROR_LOG_FILENAME,
                            help='Output error log (default: %s in current working dir )' % DEFAULT_ERROR_LOG_FILENAME)
        parser.add_argument('-p', '--number-of-parallel-tests', type=int, default=1,
                            help='Maximum number of tests allowed to run in parallel')
        parser.add_argument('-t', '--enable-coarray-to-xmp-transform', type=TestRunner.bool_str, default=False,
                            help='Transform coarray statement to xmp subroutine call statement')
        p_args = parser.parse_args()
        assert file_exists(p_args.frontend_bin), 'Frontend executable  not found'
        assert file_exists(p_args.backend_bin), 'Backend executable not found'
        assert dir_exists(p_args.input_test_dir), 'Intrinsic modules directory not found'
        if p_args.input_test is not None:
            test_path = join_path(p_args.input_test_dir, p_args.input_test)
            assert file_exists(test_path), 'Input test "%s" does not exist not found in test directory "%s"' % \
                                           (p_args.input_test_dir, p_args.input_test)
        assert dir_exists(p_args.xmodules_dir), 'Input test data directory not found'
        assert not p_args.enable_coarray_to_xmp_transform, '--enable-coarray-to-xmp-transform is currently unsupported'
        assert p_args.number_of_parallel_tests >= 1, 'Number of parallel test should be at least 1'
        native_compiler = shutil.which(p_args.native_compiler)
        assert native_compiler is not None, 'native compiler not found'
        native_compiler_type = self.get_native_compiler_type(native_compiler)
        obj_sym_reader = shutil.which('nm')
        assert obj_sym_reader is not None, 'Utility for reading object files not found'
        args = TesterArgs(num_parallel_tests=min(p_args.number_of_parallel_tests, multiprocessing.cpu_count()),
                          frontend=p_args.frontend_bin,
                          backend=p_args.backend_bin,
                          xmodules_dir=p_args.xmodules_dir,
                          native_compiler=native_compiler,
                          native_compiler_type=native_compiler_type,
                          errors_log=os.path.abspath(p_args.error_log),
                          test_data_dir=p_args.input_test_dir,
                          test_case=p_args.input_test,
                          verbose_output=p_args.verbose,
                          obj_sym_reader=obj_sym_reader)
        return args

    def run(self) -> int:
        test_cases , testcase_deps = self.scan_for_dependencies(self.args.test_data_dir)
        start_time = time.time()
        try:
            with open(self.args.errors_log, 'w') as errors_log, tempfile.TemporaryDirectory(dir=TMPFS_DIR) as tmp_dir:
                if self.args.test_case is None:
                    k = 1
                    for test_case in test_cases:
                        print(k, ' ', test_case)
                        res = TestRunner.run_test_case(test_case, self.args, tmp_dir, testcase_deps[test_case])
                        k = k + 1
                        if not res.result:
                            print(res.text_summary())
                            return RC.FAILURE
                else:
                    t_case = self.args.test_case
                    print(t_case)
                    res = TestRunner.run_test_case(t_case, self.args, tmp_dir, testcase_deps[t_case])
                    if not res.result:
                        print(res.text_summary())
                        return RC.FAILURE
        finally:
            end_time = time.time()
            print('Elapsed time: ', end_time - start_time)
        return RC.SUCCESS

    @staticmethod
    def scan_for_dependencies(dir : str, debug_output : bool = False) -> Tuple[List[str], Dict[str, List[str]]]:
        test_cases = []
        reg_comment = re.compile('\!.*')
        spaces_pat = '[\s]+'
        fortran_id_pat = '[a-z][a-z0-9\_]*'
        module_decl_pat = 'module' + spaces_pat + ('(%s)' % fortran_id_pat)
        reg_module_decl = re.compile(module_decl_pat)
        reg_use_module = re.compile('use' + spaces_pat + ('(%s)' % fortran_id_pat))
        mod_to_file = {}
        testcase_deps = {}
        for pattern in ('*.f', '*.f90', '*.f08'):
            test_cases += [os.path.basename(str(path)) for path in pathlib.Path(dir).glob(pattern)]
        test_cases = sorted(test_cases)
        # Order of the tests is currently relevant, as some files depend on each other!!!
        start_time = time.time()
        k = 1
        for test_case in test_cases:
            if debug_output: print(k, ' ', test_case)
            k = k + 1
            with open(os.path.join(dir, test_case), 'r') as f:
                deps = testcase_deps[test_case] = set()
                lines = f.readlines()
                this_testcase_mods = set()
                for line in lines:
                    # Remove comments and leading spaces
                    line = reg_comment.sub('', line)
                    line = line.strip()
                    line = line.lower()
                    # Find module declarations and uses
                    module_decl_line = reg_module_decl.match(line)
                    use_module_line = reg_use_module.match(line)
                    if module_decl_line is not None:
                        mod_name = module_decl_line.group(1)
                        if mod_name not in ('procedure', 'function', 'subroutine'):
                            #assert mod_name not in mod_to_file, 'Module "%s" already defined in "%s"' % \
                            #                                    (mod_name, mod_to_file[mod_name])
                            mod_to_file[mod_name] = test_case
                            this_testcase_mods.add(mod_name)
                            if debug_output: print('\t' + mod_name)
                    elif use_module_line is not None:
                        mod_name = use_module_line.group(1)
                        #assert mod_name in mod_to_file, 'Module "%s" not defined in this or previous modules' % mod_name
                        if mod_name not in this_testcase_mods:
                            mod_file = mod_to_file.get(mod_name)
                            #assert mod_file is not None, 'Module not declared'
                            if mod_file is not None:
                                deps.add(mod_file)
                if deps:
                    if debug_output: print('\tdependencies:')
                    for dep in sorted(deps):
                        if debug_output: print('\t\t' + dep)
        testcase_dep_lst = {}
        # Convert dependency trees into lists
        for test_case, direct_deps in testcase_deps.items():
            lst = testcase_dep_lst[test_case] = []
            test_case_direct_deps = testcase_deps[test_case]
            visited = set()
            for dir_dep in test_case_direct_deps:
                if dir_dep in visited:
                    continue
                for dep in testcase_dep_lst[dir_dep]:
                    if dep not in visited:
                        visited.add(dep)
                        lst.append(dep)
                if dir_dep not in visited:
                    visited.add(dir_dep)
                    lst.append(dir_dep)
        end_time = time.time()
        if debug_output: print('Elapsed time: ', end_time - start_time)
        return (test_cases, testcase_dep_lst)

    @staticmethod
    def run_test_case(test_case_basename : str, args : TesterArgs, dir : str, dependencies : List[str]) -> TestResult:
        stages = []
        result = None
        error_log = None
        except_str = None
        current_locale = locale.getlocale()
        try:
            locale.setlocale(locale.LC_ALL, 'C')
            frontend_opts = ['-fintrinsic-xmodules-path', args.xmodules_dir]
            native_comp_opts = []
            if args.native_compiler_type != NativeCompiler.PGFORTRAN:
                native_comp_opts += ['-fcoarray=single']
            frontend_opts += ['-fno-xmp-coarray']
            loc = locale.getlocale()
            test_case= os.path.join(args.test_data_dir, test_case_basename)
            err_out_file_basename = test_case_basename + '.out'
            xml_out_file_basename = test_case_basename + '.xml'
            decompiled_src_out_file_basename = test_case_basename + '.dec.f90'
            bin_out_file_basename = test_case_basename + '.o'
            exec_out_file_basename = test_case_basename + '.bin'
            exec_result_out_file_basename = test_case_basename + '.res'
            skip_native_in_file = test_case + '.skip.native'
            native_original_in_file = test_case + '.native'
            reference_in_file = test_case + '.ref'
            options_in_file = test_case + '.options'
            add_native_options_in_file = test_case + '.native.options'
            expected_output_in_file = os.path.splitext(test_case) [0]+ '.res'
            syms_out_file_basename = test_case_basename + '.syms'
            file_opts = []
            if file_exists(options_in_file):
                file_opts += [get_file_as_str(options_in_file)]
            additional_native_opts = []
            if file_exists(add_native_options_in_file):
                additional_native_opts += [get_file_as_str(add_native_options_in_file)]
            def add_stage_info(type : TestingStage, process : subprocess.CompletedProcess, stages=stages):
                res = process.returncode == 0
                stages.append(TestingStageInfo(type=type, result=res, args=" ".join(process.args),
                                               error_log=None if res else process.stdout))
            with tempfile.TemporaryDirectory(dir=dir) as tmp_dir:
                #Run frontend on dependencies
                for dep_basename in dependencies:
                    dep = os.path.join(args.test_data_dir, dep_basename)
                    dep_obj_file_basename = dep_basename + '.o'
                    dep_xml_file_basename = dep_basename + '.xml'
                    dep_obj_file = join_path(tmp_dir, dep_obj_file_basename)
                    dep_xml_file = join_path(tmp_dir, dep_xml_file_basename)
                    #Generate xmod files
                    frontend_dep_args = [args.frontend] + frontend_opts + file_opts + \
                                        ['-I', '.', '-I', args.test_data_dir] +\
                                        [dep, '-o', dep_xml_file_basename]
                    p = subprocess.run(args=frontend_dep_args, timeout=TEST_TIMEOUT, stdout=subprocess.PIPE,
                                       stderr=subprocess.STDOUT, cwd=tmp_dir)
                    if p.returncode != 0:
                        add_stage_info(TestingStage.DEPENDENCIES_PREP, p)
                        return TestResult(stages=stages, result=False, exception=None)
                    if not file_exists(skip_native_in_file):
                        # Run backend
                        native_original_in_dep_file = dep + '.skip.native'
                        if file_exists(native_original_in_dep_file):
                            src = dep
                        else:
                            dep_decompiled_src = dep_basename + '.dec.f90'
                            backend_dep_args = [args.backend, dep_xml_file_basename, '-o', dep_decompiled_src]
                            p = subprocess.run(args=backend_dep_args, timeout=TEST_TIMEOUT, stdout=subprocess.PIPE,
                                               stderr=subprocess.STDOUT, cwd=tmp_dir)
                            if p.returncode != 0:
                                add_stage_info(TestingStage.DEPENDENCIES_PREP, p)
                                return TestResult(stages=stages, result=False, exception=None)
                            src = dep_decompiled_src
                        #Generate mod files
                        native_dep_args = [args.native_compiler] + native_comp_opts + ['-c', src, '-o', dep_obj_file]
                        p = subprocess.run(args=native_dep_args, timeout=TEST_TIMEOUT, stdout=subprocess.PIPE,
                                           stderr=subprocess.STDOUT, cwd=tmp_dir)
                        if p.returncode != 0:
                            add_stage_info(TestingStage.DEPENDENCIES_PREP, p)
                            return TestResult(stages=stages, result=False, exception=None)
                        os.remove(dep_obj_file)
                    os.remove(dep_xml_file)
                stages.append(TestingStageInfo(type=TestingStage.DEPENDENCIES_PREP, result=True, args=None, error_log=None))
                # Run frontend
                frontend_args = [args.frontend] + frontend_opts + file_opts + \
                                ['-I', '.', '-I', args.test_data_dir]  + [test_case, '-o', xml_out_file_basename]
                p = subprocess.run(args=frontend_args, timeout=TEST_TIMEOUT, stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT, cwd=tmp_dir)
                add_stage_info(TestingStage.FRONTEND, p)
                if not stages[-1].result:
                    return TestResult(stages=stages, result=False, exception=None)
                # Run backend
                backend_args = [args.backend, xml_out_file_basename, '-o', decompiled_src_out_file_basename]
                p = subprocess.run(args=backend_args, timeout=TEST_TIMEOUT, stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT, cwd=tmp_dir)
                add_stage_info(TestingStage.BACKEND, p)
                if not stages[-1].result:
                    return TestResult(stages=stages, result=False, exception=None)
                if file_exists(skip_native_in_file):
                    return TestResult(stages=stages, result=True, exception=None)
                # Run native compiler to create object file
                src = decompiled_src_out_file_basename
                if file_exists(native_original_in_file):
                    src = test_case
                native_args = [args.native_compiler] + native_comp_opts + ['-c', src, '-o', bin_out_file_basename]
                p = subprocess.run(args=native_args, timeout=TEST_TIMEOUT, stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT, cwd=tmp_dir)
                add_stage_info(TestingStage.NATIVE, p)
                if not stages[-1].result:
                    return TestResult(stages=stages, result=False, exception=None)
                def normalize_output(s: bytes, remove_spaces: bool = False):
                    if isinstance(s, bytes):
                        s = s.decode("utf-8")
                    res = []
                    for line in s.split('\n'):
                        line = line.strip()
                        if len(line) > 0:
                            res.append(line)
                    return res
                if file_exists(expected_output_in_file):
                    # Run native compiler to link object file
                    linker_args = [args.native_compiler, '-o', exec_out_file_basename, bin_out_file_basename]
                    p = subprocess.run(args=linker_args, timeout=TEST_TIMEOUT, stdout=subprocess.PIPE,
                                       stderr=subprocess.STDOUT, cwd=tmp_dir)
                    add_stage_info(TestingStage.LINK, p)
                    if not stages[-1].result:
                        return TestResult(stages=stages, result=False, exception=None)
                    # Check if linked file is executable
                    sym_read_args = [args.obj_sym_reader, '--format', 'posix', exec_out_file_basename]
                    p = subprocess.run(args=sym_read_args, timeout=TEST_TIMEOUT, stdout=subprocess.PIPE,
                                       stderr=subprocess.STDOUT, cwd=tmp_dir)
                    add_stage_info(TestingStage.SYMBOLS_READ, p)
                    syms = p.stdout
                    with open(os.path.join(tmp_dir, syms_out_file_basename), 'wb') as f:
                        f.write(syms)
                    main_found = False
                    for line in syms.split(b'\n'):
                        line = line.strip()
                        if len(line) > 0 and b'main' in line.split()[0]:
                            main_found = True
                    if main_found:
                        # Execute the linked file
                        exec_args = [join_path('./', exec_out_file_basename)]
                        p = subprocess.run(args=exec_args, timeout=TEST_TIMEOUT, stdout=subprocess.PIPE,
                                           stderr=subprocess.STDOUT, cwd=tmp_dir)
                        add_stage_info(TestingStage.EXECUTION, p)
                        with open(os.path.join(tmp_dir, exec_result_out_file_basename), 'wb') as f:
                            f.write(p.stdout)
                        out_lines = normalize_output(p.stdout)
                        if not stages[-1].result:
                            return TestResult(stages=stages, result=False, exception=None)
                        # Compare with expected output
                        with open(expected_output_in_file, 'r') as f:
                            expected_out_lines = normalize_output(f.read())
                        res = out_lines == expected_out_lines
                        stages.append(TestingStageInfo(type=TestingStage.EXPECTED_OUTPUT, result=res, args=None,
                                                       error_log=None if res else 'Expected output did not match'))
                        if not stages[-1].result:
                            return TestResult(stages=stages, result=False, exception=None)
                if file_exists(reference_in_file):
                    # Decompile file without line information, then compare to reference
                    backend_args = [args.backend, '-l', xml_out_file_basename, '-o', decompiled_src_out_file_basename]
                    p = subprocess.run(args=backend_args, timeout=TEST_TIMEOUT, stdout=subprocess.PIPE,
                                       stderr=subprocess.STDOUT, cwd=tmp_dir)
                    if p.returncode != 0:
                        add_stage_info(TestingStage.REFERENCE_OUTPUT, p)
                        return TestResult(stages=stages, result=False, exception=None)
                    with open(reference_in_file, 'r') as f:
                        reference_lines = normalize_output(f.read(), True)
                    with open(join_path(tmp_dir, decompiled_src_out_file_basename), 'r') as f:
                        decompiled_src_lines = normalize_output(f.read(), True)
                    res = reference_lines == decompiled_src_lines
                    stages.append(TestingStageInfo(type=TestingStage.REFERENCE_OUTPUT, result=res, args=None,
                                                   error_log=None if res else 'Reference output did not match'))
                    if not stages[-1].result:
                        return TestResult(stages=stages, result=False, exception=None)
                return TestResult(stages=stages, result=True, exception=None)
        except:
            except_str = traceback.format_exc()
            return TestResult(stages=stages, result=False, exception=except_str)
        finally:
            locale.setlocale(locale.LC_ALL, current_locale)


if __name__ == '__main__':
    ret_code = TestRunner().run()
    exit(ret_code)
