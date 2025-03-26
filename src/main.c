#include <stdio.h>
#include <string.h>
#include "test.h"

int main(int argc, char *argv[]) {
  if (argc == 3 && !strcmp(argv[1], "--run-test")) {
    run_single_test(argv[2]);
    return 0;
  }

  if (argc == 2 && !strcmp(argv[1], "--print-tests")) {
    print_test_names();
    return 0;
  }

  printf("usage: compiler [--run-test <test_name>] [--print-tests]\n");
  return 0;
}
