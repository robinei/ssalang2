#pragma once

#include "ast.h"


typedef struct AstGen {
  char *buffer;
  u32 buffer_used;
  u32 buffer_size;
} AstGen;


