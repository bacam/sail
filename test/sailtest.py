import os
import re
import sys
import subprocess
import datetime
import argparse
import signal

def signal_handler(sig, frame):
    sys.exit(0)

signal.signal(signal.SIGINT, signal_handler)

parser = argparse.ArgumentParser("run_tests.py")
parser.add_argument("--hide-error-output", help="Hide error information.", action='store_true')
parser.add_argument("--compact", help="Compact output.", action='store_true')
parser.add_argument("--targets", help="Targets to use (where supported).", action='append')
parser.add_argument("--update-expected", help="Update the expected file (where supported)", action="store_true")
parser.add_argument("--run-skips", help="Run tests that would otherwise be skipped", action="store_true")
parser.add_argument("--test", help="Run only specified test.", action='append')
args = parser.parse_args()

def is_compact():
    return args.compact

def get_targets(default_targets):
    if args.targets is None:
        return default_targets
    else:
        return args.targets

def compact_char(code, char):
    print('{}{}{}'.format(code, char, color.END), end='')
    sys.stdout.flush()

class color:
    NOTICE = '\033[94m'
    PASS = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    END = '\033[0m'

def get_sail_dir():
    try:
        return os.environ['SAIL_DIR']
    except KeyError:
        try:
            p = subprocess.run([get_sail(), "--dir"], capture_output=True, text=True)
        except Exception as e:
            print('{}Unable to get Sail library directory from opam{}'.format(color.FAIL, color.END))
            print(e)
            sys.exit(1)

        if p.returncode == 0:
            return p.stdout.strip()
        else:
            print('{}Unable to get Sail library directory from sail --dir{}'.format(color.FAIL, color.END))
            print('{}stdout{}:'.format(color.NOTICE, color.END))
            print(p.stdout)
            print('{}stderr{}:'.format(color.NOTICE, color.END))
            print(p.stderr)
            sys.exit(1)

def print_ok(name):
    if is_compact():
        compact_char(color.PASS, '.')
    else:
        print('{} {}{}{}'.format('{} '.format(name).ljust(40, '.'), color.PASS, 'ok', color.END))

def print_skip(name):
    if is_compact():
        compact_char(color.WARNING, 's')
    else:
        print('{} {}{}{}'.format('{} '.format(name).ljust(40, '.'), color.WARNING, 'skip', color.END))

def get_sail():
    try:
        return os.environ['SAIL']
    except KeyError:
        return 'sail'

def parallel():
    try:
        return int(os.environ['TEST_PAR'])
    except Exception as e:
        print("Running 16 tests in parallel. Set TEST_PAR to configure")
        return 16

def chunks(filenames, cores):
    ys = []
    chunk = []
    for filename in filenames:
        basename = os.path.splitext(os.path.basename(filename))[0]
        if re.match(r'.+\.sail$', filename) and (not args.test or basename in args.test):
            chunk.append(filename)
        if len(chunk) >= cores:
            ys.append(list(chunk))
            chunk = []
    ys.append(list(chunk))
    return ys

def directory_chunks(filenames, cores):
    ys = []
    chunk = []
    for filename in filenames:
        if os.path.isdir(filename):
            chunk.append(filename)
        if len(chunk) >= cores:
            ys.append(list(chunk))
            chunk = []
    ys.append(list(chunk))
    return ys

def project_chunks(filenames, cores):
    ys = []
    chunk = []
    for filename in filenames:
        if re.match(r'.+\.sail_project$', filename):
            chunk.append(filename)
        if len(chunk) >= cores:
            ys.append(list(chunk))
            chunk = []
    ys.append(list(chunk))
    return ys

def step_with_status(string, expected_status=0, cwd=None, name='', stderr_file=''):
    p = subprocess.Popen(string, shell=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE, cwd=cwd)
    out, err = p.communicate()
    status = p.wait()
    if status != expected_status:
        if is_compact():
            compact_char(color.FAIL, 'X')
        else:
            print("{}Failed{}: {} {}".format(color.FAIL, color.END, name, string))
        if not args.hide_error_output:
            print('{}stdout{}:'.format(color.NOTICE, color.END))
            print(out.decode('utf-8'))
            print('{}stderr{}:'.format(color.NOTICE, color.END))
            print(err.decode('utf-8'))
            if stderr_file != '':
                try:
                    with open(stderr_file, 'r') as file:
                        content = file.read()
                        print('{}stderr file{}:'.format(color.NOTICE, color.END))
                        print(content)
                except FileNotFoundError:
                    print('File {} not found'.format(stderr_file))
    return status

def step(string, expected_status=0, cwd=None, name='', stderr_file=''):
    if step_with_status(string, expected_status=expected_status, cwd=cwd, name=name) != expected_status:
        sys.exit(1)

def banner(string):
    print('-' * len(string))
    print(string)
    print('-' * len(string))
    sys.stdout.flush()

class Results:
    def __init__(self, name):
        self.passes = 0
        self.failures = 0
        self.xfails = 0
        self._xfail_reasons = {}
        self.xml = ""
        self.name = name

    def expect_failure(self, test, reason):
        self._xfail_reasons[test] = reason

    def _add_status(self, test, result, msg):
        self.xml += f'    <testcase name="{test}">\n      <{result} message="{msg}">{msg}</{result}>\n    </testcase>\n'

    def _add_failure(self, test, msg):
        self.failures += 1
        self._add_status(test, "error", msg)

    def collect(self, tests):
        for test in tests:
            _, status = os.waitpid(tests[test], 0)
            if test in self._xfail_reasons:
                reason = self._xfail_reasons[test]
                if status == 0:
                    self._add_failure(test, "XPASS: " + reason)
                else:
                    self.xfails += 1
                    self._add_status(test, "skipped", "XFAIL: " + reason)
                continue
            if status != 0:
                self._add_failure(test, "fail")
            else:
                self.passes += 1
                self.xml += '    <testcase name="{}"/>\n'.format(test)
        sys.stdout.flush()

    def finish(self):
        xfail_msg = f' ({self.xfails} expected failures)' if self.xfails else ''
        if is_compact():
            print()
        print('{}{} passes and {} failures{}{}'.format(color.NOTICE, self.passes, self.failures, xfail_msg, color.END))

        time = datetime.datetime.utcnow()
        suite = '  <testsuite name="{}" tests="{}" failures="{}" timestamp="{}">\n{}  </testsuite>\n'
        self.xml = suite.format(self.name, self.passes + self.failures, self.failures, time, self.xml)
        return self.xml
