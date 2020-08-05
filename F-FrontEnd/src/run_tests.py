#!/usr/bin/env python3
#-*- coding: utf-8 -*-

import sys

assert sys.version_info[0] >= 3 and sys.version_info[1] >= 6, 'Python >= 3.6 is required'

import argparse, os, locale, shutil, pathlib, tempfile, multiprocessing, subprocess, traceback, time, re
from os.path import join as join_path, realpath as real_path
from enum import Enum
from typing import Tuple, NamedTuple, List, Optional, Dict, Union
from textwrap import wrap
from contextlib import contextmanager

THIS_DIR_PATH = os.path.dirname(os.path.realpath(__file__))
TEST_DATA_DEFAULT_RELATIVE_PATH = '../../F-FrontEnd/test/testdata'
DEFAULT_ERROR_LOG_FILENAME = 'errors.log'
TMPFS_DIR = '/dev/shm'
TEST_TIMEOUT = 10  # seconds


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
    num_parallel_tests: int
    frontend: str
    backend: str
    xmodules_dir: str
    native_compiler_type: NativeCompiler
    native_compiler: str
    errors_log: str
    test_data_dir: str
    test_case: Optional[str]
    working_dir: Optional[str]
    verbose_output: bool
    obj_sym_reader: str


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


class TerminalColor:
    BLACK = '\033[30m'
    RED = '\033[31m'
    GREEN = '\033[32m'


def to_paragraph(p: Union[str, bytes], prefix: str = ''):
    if isinstance(p, bytes):
        p = p.decode('utf-8')
    return '\n'.join([prefix + l for l in wrap(p, width=100)])


def error_text(s: str):
    return TerminalColor.RED + s + TerminalColor.BLACK


class TestResult(NamedTuple):
    def text_summary(self) -> str:
        txt = ''
        for s in self.stages:
            txt += '%s\n' % s.type
            if not s.result:
                if s.args is not None:
                    txt += 'Args:\n%s\n' % to_paragraph(s.args, '\t')
                if s.error_log is not None:
                    txt += error_text('Output:\n%s\n' % to_paragraph(s.error_log, '\t'))
        if self.exception is not None:
            txt += error_text('Exception:\n' + to_paragraph(self.exception, '\t'))
        return txt
    stages: Tuple[TestingStageInfo, ...]
    result: bool
    exception: Optional[str]


class ReturnCode(Enum):
    SUCCESS = 0
    FAILURE = 1


RC = ReturnCode


class TestcaseRunner:
    @property
    def name(self): return self.__basename
    @property
    def input_dir(self): return self.__input_dir
    @property
    def dependencies(self): return self.__dependencies
    @property
    def args(self): return self.__args
    @property
    def test_case(self): return self.__test_case
    @property
    def use_native(self): return self.__use_native
    @property
    def skip_native(self): return self.__skip_native
    @property
    def reference_in_file(self): return self.__reference_in_file
    @property
    def compare_with_reference(self): return self.__compare_with_reference
    @property
    def frontend_opts(self): return self.__frontend_opts
    @property
    def native_comp_opts(self): return self.__native_comp_opts
    @property
    def expected_output_in_file(self): return self.__expected_output_in_file
    @property
    def compare_with_expected_output(self): return self.__compare_with_expected_output
    @property
    def stages(self): return self.__stages
    @property
    def working_dir(self): return self.__working_dir
    @property
    def xml_out_file_basename(self): return self.__xml_out_file_basename
    @property
    def decompiled_src_out_file_basename(self): return self.__decompiled_src_out_file_basename
    @property
    def bin_out_file_basename(self): return self.__bin_out_file_basename
    @property
    def exec_out_file_basename(self): return self.__exec_out_file_basename
    @property
    def exec_result_out_file_basename(self): return self.__exec_result_out_file_basename
    @property
    def exec_result_out_file(self): return join_path(self.working_dir, self.exec_result_out_file_basename)
    @property
    def syms_out_file_basename(self): return self.__syms_out_file_basename
    @property
    def syms_out_file(self): return join_path(self.working_dir, self.syms_out_file_basename)

    def __init__(self,
                 basename: str,
                 tester_args: TesterArgs,
                 working_dir: str,
                 dependencies: Tuple[str, ...]):
        self.__basename = basename
        self.__args = tester_args
        self.__input_dir = tester_args.test_data_dir
        self.__dependencies = dependencies
        self.__stages = []
        self.__test_case = join_path(tester_args.test_data_dir, basename)
        self.__skip_native = file_exists(self.test_case + '.skip.native')
        self.__use_native = file_exists(self.test_case + '.native')
        self.__reference_in_file = self.test_case + '.ref'
        self.__compare_with_reference = file_exists(self.__reference_in_file)
        frontend_opts = ['-fintrinsic-xmodules-path', tester_args.xmodules_dir, '-fno-xmp-coarray']
        options_in_file = self.test_case + '.options'
        if file_exists(options_in_file):
            frontend_opts += [get_file_as_str(options_in_file)]
        self.__frontend_opts = tuple(frontend_opts)
        self.__native_comp_opts = []
        if tester_args.native_compiler_type != NativeCompiler.PGFORTRAN:
            self.__native_comp_opts += ['-fcoarray=single']
        native_options_in_file = self.test_case + '.native.options'
        if file_exists(native_options_in_file):
            self.__native_comp_opts += [get_file_as_str(native_options_in_file)]
        self.__expected_output_in_file = os.path.splitext(self.test_case)[0] + '.res'
        self.__compare_with_expected_output = file_exists(self.expected_output_in_file)
        self.__working_dir = working_dir
        self.__xml_out_file_basename = basename + '.xml'
        self.__decompiled_src_out_file_basename = basename + '.dec.f90'
        self.__bin_out_file_basename = basename + '.o'
        self.__exec_out_file_basename = basename + '.bin'
        self.__exec_result_out_file_basename = basename + '.res'
        self.__syms_out_file_basename = basename + '.syms'
        self.__result = None

    def __add_stage_info(self, stage_type: TestingStage, process: subprocess.CompletedProcess):
        res = process.returncode == 0
        self.__stages.append(TestingStageInfo(type=stage_type, result=res, args=" ".join(process.args),
                                              error_log=None if res else process.stdout))

    def __run_exec(self, args: List[str], stage: Optional[TestingStage] = None,
                   record_stage_only_on_error: bool = False) -> subprocess.CompletedProcess:
        assert self.working_dir is not None, 'Working dir not set'
        p = subprocess.run(args=args, timeout=TEST_TIMEOUT, stdout=subprocess.PIPE,
                           stderr=subprocess.STDOUT, cwd=self.working_dir)
        if stage is not None and not record_stage_only_on_error:
            self.__add_stage_info(stage, p)
            if not self.__stages[-1].result:
                self.__result = self.__result_error()
        return p

    def __prepare_dependencies(self) -> Optional[TestResult]:
        # Run frontend on dependencies
        for dep_basename in self.dependencies:
            dep = os.path.join(self.args.test_data_dir, dep_basename)
            dep_obj_file_basename = dep_basename + '.o'
            dep_xml_file_basename = dep_basename + '.xml'
            dep_obj_file = join_path(self.working_dir, dep_obj_file_basename)
            dep_xml_file = join_path(self.working_dir, dep_xml_file_basename)
            # Generate xmod files
            frontend_dep_args = [self.args.frontend] + list(self.frontend_opts) + ['-I', '.', '-I', self.input_dir] + \
                                [dep, '-o', dep_xml_file_basename]
            p = self.__run_exec(frontend_dep_args, TestingStage.DEPENDENCIES_PREP, True)
            if p.returncode != 0:
                return self.__result
            if not self.skip_native:
                # Run backend
                native_original_in_dep_file = dep + '.skip.native'
                if file_exists(native_original_in_dep_file):
                    src = dep
                else:
                    dep_decompiled_src = dep_basename + '.dec.f90'
                    backend_dep_args = [self.args.backend, dep_xml_file_basename, '-o', dep_decompiled_src]
                    p = self.__run_exec(backend_dep_args, TestingStage.DEPENDENCIES_PREP, True)
                    if p.returncode != 0:
                        return self.__result
                    src = dep_decompiled_src
                # Generate mod files
                native_dep_args = [self.args.native_compiler] + self.native_comp_opts + ['-c', src, '-o', dep_obj_file]
                p = self.__run_exec(native_dep_args, TestingStage.DEPENDENCIES_PREP, True)
                if p.returncode != 0:
                    return self.__result
                os.remove(dep_obj_file)
            os.remove(dep_xml_file)
        self.__stages.append(TestingStageInfo(type=TestingStage.DEPENDENCIES_PREP, result=True, args=None,
                                              error_log=None))
        return self.__result

    def __test_frontend(self) -> Optional[TestResult]:
        frontend_args = [self.args.frontend] + list(self.frontend_opts) + ['-I', '.', '-I', self.input_dir] +\
                        [self.test_case, '-o', self.xml_out_file_basename]
        self.__run_exec(frontend_args, TestingStage.FRONTEND)
        return self.__result

    def __test_backend(self) -> Optional[TestResult]:
        backend_args = [self.args.backend, self.xml_out_file_basename, '-o', self.decompiled_src_out_file_basename]
        self.__run_exec(backend_args, TestingStage.BACKEND)
        return self.__result

    def __test_with_native_compiler(self) -> Optional[TestResult]:
        src = self.decompiled_src_out_file_basename
        if self.use_native:
            src = self.test_case
        native_args = [self.args.native_compiler] + self.native_comp_opts + ['-c', src, '-o', self.bin_out_file_basename]
        self.__run_exec(native_args, TestingStage.NATIVE)
        return self.__result

    def __test_native_compiler_link(self) -> Optional[TestResult]:
        linker_args = [self.args.native_compiler, '-o', self.exec_out_file_basename, self.bin_out_file_basename]
        self.__run_exec(linker_args, TestingStage.LINK)
        return self.__result

    def __read_symbols(self) -> Optional[TestResult]:
        sym_read_args = [self.args.obj_sym_reader, '--format', 'posix', self.exec_out_file_basename]
        p = self.__run_exec(sym_read_args, TestingStage.SYMBOLS_READ)
        syms = p.stdout
        with open(self.syms_out_file, 'wb') as f:
            f.write(syms)
        return self.__result

    def __has_main_symbol(self) -> bool:
        with open(self.syms_out_file, 'r') as f:
            syms = f.readlines()
            for line in syms:
                line = line.strip()
                if len(line) > 0 and 'main' in line.split()[0]:
                    return True
        return False

    def __run_executable(self) -> Optional[TestResult]:
        exec_args = [join_path('./', self.exec_out_file_basename)]
        p = self.__run_exec(exec_args, TestingStage.EXECUTION)
        with open(self.exec_result_out_file, 'wb') as f:
            f.write(p.stdout)
        return self.__result

    def __decompile(self) -> Optional[TestResult]:
        # Decompile file without line information, then compare to reference
        backend_args = [self.args.backend, '-l', self.xml_out_file_basename, '-o', self.decompiled_src_out_file_basename]
        self.__run_exec(backend_args, TestingStage.REFERENCE_OUTPUT, record_stage_only_on_error=True)
        return self.__result

    def __compare_output_files(self, filename1, filename2, stage: TestingStage) -> Optional[TestResult]:
        with open(filename1, 'r') as f:
            out_lines = f.read()
            out_lines = self.normalize_expected_output(out_lines)
        with open(filename2, 'r') as f:
            expected_out_lines = self.normalize_expected_output(f.read())
        res = out_lines == expected_out_lines
        self.__stages.append(TestingStageInfo(type=stage, result=res, args=None,
                                              error_log=None if res else 'Output did not match'))
        if not self.__stages[-1].result:
            self.__result = self.__result_error()
        return self.__result

    @staticmethod
    def normalize_expected_output(s: Union[bytes, str]):
        if isinstance(s, bytes):
            s = s.decode("utf-8")
        res = []
        for line in s.split('\n'):
            line = line.strip()
            if len(line) > 0:
                res.append(line)
        return res

    def __result_error(self, exception: str = None) -> TestResult:
        return TestResult(tuple(self.__stages), False, exception)

    def __result_success(self) -> TestResult:
        return TestResult(tuple(self.__stages), True, None)

    def run(self) -> TestResult:
        current_locale = locale.getlocale()
        try:
            locale.setlocale(locale.LC_ALL, 'C')
            if self.__prepare_dependencies() is not None:
                return self.__result
            if self.__test_frontend() is not None:
                return self.__result
            if self.__test_backend() is not None:
                return self.__result
            if self.skip_native:
                return self.__result_success()
            if self.__test_with_native_compiler() is not None:
                return self.__result
            if self.compare_with_expected_output:
                if self.__test_native_compiler_link() is not None:
                    return self.__result
                if self.__read_symbols() is not None:
                    return self.__result
                if self.__has_main_symbol():  # Linked file is executable
                    # Execute the linked file
                    if self.__run_executable() is not None:
                        return self.__result
                    if self.__compare_output_files(self.exec_result_out_file,
                                                   self.expected_output_in_file,
                                                   TestingStage.EXPECTED_OUTPUT) is not None:
                        return self.__result
            if self.compare_with_reference:
                # Decompile file without line information, then compare to reference
                if self.__decompile() is not None:
                    return self.__result
                if self.__compare_output_files(self.reference_in_file,
                                               join_path(self.working_dir, self.decompiled_src_out_file_basename),
                                               TestingStage.REFERENCE_OUTPUT) is not None:
                    return self.__result
            return self.__result_success()
        except:
            except_str = traceback.format_exc()
            return self.__result_error(exception=except_str)
        finally:
            locale.setlocale(locale.LC_ALL, current_locale)


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
    def get_native_compiler_type(native_compiler: str) -> NativeCompiler:
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
        parser.add_argument('-d', '--input-tests-dir', type=str,
                            default=real_path(join_path(THIS_DIR_PATH, TEST_DATA_DEFAULT_RELATIVE_PATH)),
                            help='Input test data directory (default: %s)' % TEST_DATA_DEFAULT_RELATIVE_PATH)
        parser.add_argument('-i', '--input-test', type=str, default=None,
                            help='Specific input test file basename.')
        parser.add_argument('-w', '--working-dir', type=str, default=None,
                            help='Specific working dir directory (artifacts will be left there after run)')
        # gfortran version is important! Currently some of the tests fail with gfortran 9 and 10
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
        assert dir_exists(p_args.input_tests_dir), 'Input test data directory not found'
        if p_args.input_test is not None:
            test_path = join_path(p_args.input_tests_dir, p_args.input_test)
            assert file_exists(test_path), 'Input test "%s" does not exist not found in test directory "%s"' % \
                                           (p_args.input_test_dir, p_args.input_test)
        assert dir_exists(p_args.xmodules_dir), 'Intrinsic modules directory not found'
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
                          test_data_dir=p_args.input_tests_dir,
                          test_case=p_args.input_test,
                          working_dir=p_args.working_dir,
                          verbose_output=p_args.verbose,
                          obj_sym_reader=obj_sym_reader)
        return args

    def run(self) -> int:
        test_cases, testcase_deps = self.scan_for_dependencies(self.args.test_data_dir)
        start_time = time.time()

        @contextmanager
        def prepare_dir(dir_path=None):
            if dir_path is not None:
                os.makedirs(dir_path, exist_ok=True)
                yield dir_path
                pass
            else:
                d = tempfile.TemporaryDirectory(dir=dir_path)
                yield d.name
                d.cleanup()
        try:
            with prepare_dir(self.args.working_dir) as working_dir:
                if self.args.test_case is None:
                    k = 1
                    for test_case in test_cases:
                        print(k, ' ', test_case)
                        testcase_working_dir = join_path(working_dir, os.path.basename(test_case).replace('.', '-'))
                        os.makedirs(testcase_working_dir)
                        tc = TestcaseRunner(test_case, self.args, working_dir, testcase_deps[test_case])
                        res = tc.run()
                        k = k + 1
                        if not res.result:
                            print(res.text_summary())
                            return RC.FAILURE.value
                else:
                    t_case = self.args.test_case
                    print(t_case)
                    tc = TestcaseRunner(t_case, self.args, working_dir, testcase_deps[t_case])
                    res = tc.run()
                    if not res.result:
                        print(res.text_summary())
                        return RC.FAILURE.value
        finally:
            end_time = time.time()
            print('Elapsed time: ', end_time - start_time)
        return RC.SUCCESS.value

    @staticmethod
    def scan_for_dependencies(dir_path: str,
                              debug_output: bool = False) -> Tuple[Tuple[str, ...], Dict[str, Tuple[str, ...]]]:
        test_cases = []
        reg_comment = re.compile('!.*')
        spaces = '[\s]+'
        opt_spaces = '[\s]*'
        fortran_id = '[a-z][a-z0-9\_]*'
        module_decl = 'module' + spaces + ('(%s)' % fortran_id)
        submodule_name = '[\sa-z0-9\_]+'
        submodule_decl = 'submodule' + opt_spaces + ('\((%s)\)' % submodule_name) + opt_spaces + ('(%s)' % fortran_id)
        reg_space = re.compile('\s')
        reg_module_decl = re.compile(module_decl)
        reg_submodule_decl = re.compile(submodule_decl)
        reg_use_module = re.compile('use' + spaces + ('(%s)' % fortran_id))
        mod_to_file = {}
        testcase_deps = {}
        for pattern in ('*.f', '*.f90', '*.f08'):
            test_cases += [os.path.basename(str(path)) for path in pathlib.Path(dir_path).glob(pattern)]
        test_cases = sorted(test_cases)
        # Order of the tests is currently relevant, as some files depend on each other!!!
        start_time = time.time()
        k = 1
        for test_case in test_cases:
            if debug_output: print(k, ' ', test_case)
            k = k + 1
            with open(os.path.join(dir_path, test_case), 'r') as f:
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
                    submodule_decl_line = reg_submodule_decl.match(line)
                    use_module_line = reg_use_module.match(line)
                    if module_decl_line is not None:
                        mod_name = module_decl_line.group(1)
                        if mod_name not in ('procedure', 'function', 'subroutine'):
                            # assert mod_name not in mod_to_file, 'Module "%s" already defined in "%s"' % \
                            #                                    (mod_name, mod_to_file[mod_name])
                            mod_to_file[mod_name] = test_case
                            this_testcase_mods.add(mod_name)
                            if debug_output: print('\t' + mod_name)
                    elif submodule_decl_line is not None:
                        submod_short_name = submodule_decl_line.group(2)
                        parent_mod_name = reg_space.sub('', submodule_decl_line.group(1))
                        submod_name = '%s:%s' % (parent_mod_name, submod_short_name)
                        mod_to_file[submod_name] = test_case
                        this_testcase_mods.add(submod_name)
                        if parent_mod_name not in this_testcase_mods:
                            parent_mod_file = mod_to_file[parent_mod_name]
                            deps.add(parent_mod_file)
                    elif use_module_line is not None:
                        mod_name = use_module_line.group(1)
                        # assert mod_name in mod_to_file, 'Module "%s" not defined in this or previous modules' % mod_name
                        if mod_name not in this_testcase_mods:
                            mod_file = mod_to_file.get(mod_name)
                            # assert mod_file is not None, 'Module not declared'
                            if mod_file is not None:
                                deps.add(mod_file)
                if deps:
                    if debug_output: print('\tdependencies:')
                    for dep in sorted(deps):
                        if debug_output: print('\t\t' + dep)
        testcase_dep_lst = {}
        # Convert dependency trees into lists
        for test_case, direct_deps in testcase_deps.items():
            lst = []
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
            testcase_dep_lst[test_case] = tuple(lst)
        end_time = time.time()
        if debug_output: print('Elapsed time: ', end_time - start_time)
        return tuple(test_cases), testcase_dep_lst


if __name__ == '__main__':
    ret_code = TestRunner().run()
    exit(ret_code)
