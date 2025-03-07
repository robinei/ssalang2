#pragma once

#include "ast.h"

typedef struct AstGen {
  char *buffer;
  u32 buffer_used;
  u32 buffer_size;
} AstGen;

AstGen *astgen_new(void);
void astgen_destroy(AstGen *astgen);

AstNodeRef astgen_type_atom(AstGen *astgen, AstTypeAtomTag atom);
AstNodeRef astgen_const_bool(AstGen *astgen, bool value);
AstNodeRef astgen_const_i32(AstGen *astgen, i32 value);
AstNodeRef astgen_binop(AstGen *astgen, AstNodeTag tag, AstNodeRef left_node, AstNodeRef right_node);
AstNodeRef astgen_local_write(AstGen *astgen, bool is_definition, u32 local_index, AstNodeRef expr);
AstNodeRef astgen_local_read(AstGen *astgen, u32 local_index);
AstNodeRef astgen_block(AstGen *astgen, bool is_static, u32 scope_index, u32 nodes_count, AstNodeRef nodes[]);
AstNodeRef astgen_if(AstGen *astgen, bool is_static, bool is_inline, u32 scope_index, AstNodeRef cond, AstNodeRef then, AstNodeRef els);
AstNodeRef astgen_while(AstGen *astgen, bool is_static, bool is_inline, u32 scope_index, AstNodeRef cond, AstNodeRef body);
AstNodeRef astgen_break(AstGen *astgen, bool is_static, u32 scope_index);
AstNodeRef astgen_continue(AstGen *astgen, bool is_static, u32 scope_index);
AstNodeRef astgen_return(AstGen *astgen, AstNodeRef value_node);
AstNodeRef astgen_func(AstGen *astgen, bool is_static, bool is_inline, AstNodeRef body, AstNodeRef return_type, u32 params_count, u32 locals_count, AstLocal locals[]);
