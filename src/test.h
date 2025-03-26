#pragma once

typedef void (*TestFunction)(void);

typedef struct TestDefinition {
  const char *name;
  TestFunction func;
} TestDefinition;

TestDefinition *resolve_test_definition(const char *test_name);

void run_single_test(const char *test_name);

void print_test_names(void);
