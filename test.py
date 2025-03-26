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


def discover_tests():
  test_regexp = re.compile(r'void\s+(test_[_A-Za-z0-9]+)s*\(\s*(?:void)?\s*\)')
  for c_file in SRC_DIR.glob('**/*.c'):
    if c_file == TESTS_FILE:
      continue
    c_source = c_file.read_text()
    for match in test_regexp.findall(c_source):
      yield match


def generate_tests_c(tests):
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
  result = subprocess.check_output([EXECUTABLE, '--run-test', test_name]).decode()
  if snapshot is None:
    snapshot_file.write_text(result)
    print(test_name, 'initialized.')
    return True
  elif snapshot == result:
    print(test_name, 'passed!')
    return True
  else:
    diff = ''.join(Differ().compare(snapshot.splitlines(1), result.splitlines(1)))
    print(test_name, 'failed!')
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
  passed = 0
  failed = 0
  for test_name in tests:
    if run_test(test_name, interactive):
      passed += 1
    else:
      failed += 1
  print(f'{passed} test(s) passed, {failed} test(s) failed')
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
