#!/usr/bin/env python 

import argparse
import subprocess
from pathlib import Path
from difflib import Differ
import re


EXECUTABLE = './bin/compiler'
SNAPSHOTS_DIR = Path('./snapshots')
SRC_DIR = Path('./src')
TESTS_FILE = Path('./src/tests.c')


class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'


def discover_tests():
  test_regexp = re.compile(r'void\s+(test_[_A-Za-z0-9]+)s*\(\s*(?:void)?\s*\)')
  for c_file in SRC_DIR.glob('**/*.c'):
    if c_file == TESTS_FILE:
      continue
    c_source = c_file.read_text()
    for match in test_regexp.findall(c_source):
      yield match


def generate_tests_c(tests):
  yield '/* this file is generated code. DO NOT EDIT */'
  yield r'#include "test.h"'
  yield ''
  for test_name in tests:
    yield f'void {test_name}(void);'
  yield ''
  yield r'static TestDefinition definitions[] = {'
  for test_name in tests:
    yield f'  {{ "{test_name}", {test_name} }},'
  yield r'};'
  yield ''
  yield 'const TestDefinition * const test_definitions = definitions;'
  yield f'const int test_definitions_count = {len(tests)};'
  yield ''


def generate():
  tests = list(discover_tests())
  tests.sort()
  text = '\n'.join(generate_tests_c(tests))
  try:
    old_text = TESTS_FILE.read_text()
    if old_text == text:
      return
  except FileNotFoundError:
    pass
  TESTS_FILE.write_text(text)


def run_test(test_name, interactive):
  snapshot_file = SNAPSHOTS_DIR / (test_name + '.txt')
  try:
    snapshot = snapshot_file.read_text()
  except FileNotFoundError:
    snapshot = None
  try:
    result = subprocess.check_output([EXECUTABLE, '--run-test', test_name]).decode()
  except subprocess.CalledProcessError as e:
    print(f'{test_name}... {bcolors.FAIL}crashed{bcolors.ENDC}! (code: {e.returncode})')
    return False
  if snapshot is None:
    snapshot_file.write_text(result)
    print(f'{test_name}... {bcolors.OKGREEN}initialized{bcolors.ENDC}.')
    return True
  elif snapshot == result:
    print(f'{test_name}... {bcolors.OKGREEN}passed{bcolors.ENDC}!')
    return True
  else:
    diff = ''.join(Differ().compare(snapshot.splitlines(1), result.splitlines(1)))
    print(f'{test_name}... {bcolors.FAIL}failed{bcolors.ENDC}!')
    print(diff)
    if interactive:
      choice = input('(a)ccept or (r)eject? [default=reject] ')
      if choice.lower() == 'a':
        snapshot_file.write_text(result)
        return True
  return False


def run(interactive):
  SNAPSHOTS_DIR.mkdir(exist_ok=True)
  tests = subprocess.check_output([EXECUTABLE, '--print-tests']).decode().splitlines()
  passed_count = 0
  failed_count = 0
  for test_name in tests:
    if run_test(test_name, interactive):
      passed_count += 1
    else:
      failed_count += 1
  summary_text = f'{passed_count} test(s) passed, {failed_count} test(s) failed'
  if failed_count > 0:
    summary_text = bcolors.WARNING + summary_text + bcolors.ENDC
  print(summary_text)
  orphans = set(SNAPSHOTS_DIR.glob('*.txt')) - set(SNAPSHOTS_DIR / (test_name + '.txt') for test_name in tests)
  if len(orphans) > 0:
    print(f'{len(orphans)} orphaned snapshots found:')
    for orphan in orphans:
      print(orphan)
    if interactive:
      choice = input('(d)elete or (k)eep? [default=keep] ')
      if choice.lower() == 'd':
        for orphan in orphans:
          orphan.unlink()


def main():
  parser = argparse.ArgumentParser()
  parser.add_argument('-g', '--generate', action='store_true')
  parser.add_argument('-i', '--interactive', action='store_true')
  args = parser.parse_args()
  if args.generate:
    generate()
  else:
    run(args.interactive)


if __name__ == '__main__':
  main()
