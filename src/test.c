#include "test.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

extern const TestDefinition * const test_definitions;
extern const int test_definitions_count;

static int test_cmp(void const *a, void const *b) {
  return strcmp(((TestDefinition const *const)a)->name, ((TestDefinition const *const)b)->name);
}

TestDefinition *resolve_test_definition(const char *test_name) {
  TestDefinition needle = { test_name };
  return bsearch(&needle, test_definitions, test_definitions_count, sizeof(TestDefinition), test_cmp);
}

void run_single_test(const char *test_name) {
  TestDefinition *test = resolve_test_definition(test_name);
  if (!test) {
    printf("test not found: %s\n", test_name);
    return;
  }
  test->func();
}

void print_test_names(void) {
  for (int i = 0; i < test_definitions_count; ++i) {
    printf("%s\n", test_definitions[i].name);
  }
}
