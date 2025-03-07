#include "astgen.h"
#include <stdlib.h>
#include <string.h>

static u32 next_power_of_two(u32 v) {
  v--;
  v |= v >> 1;
  v |= v >> 2;
  v |= v >> 4;
  v |= v >> 8;
  v |= v >> 16;
  v++;
}

AstGen *astgen_new(void) {
  AstGen *astgen = calloc(1, sizeof(AstGen));
  return astgen;
}

void astgen_destroy(AstGen *astgen) {
  free(astgen->buffer);
  free(astgen);
}

static void *astgen_alloc(AstGen *astgen, u32 size, AstRef *ref) {
  size = (size + 7) & ~7;

  if (astgen->buffer_used + size > astgen->buffer_size) {
    astgen->buffer_size = next_power_of_two(astgen->buffer_used + size);
    astgen->buffer = realloc(astgen->buffer, astgen->buffer_size);
  }

  *ref = astgen->buffer_used;
  void *result = astgen->buffer + astgen->buffer_used;
  astgen->buffer_used += size;
  memset(result, 0, size);
  return result;
}

AstNodeRef astgen_type_atom(AstGen *astgen, AstTypeAtomTag atom) {
  AstNodeRef ref;
  AstTypeAtomNode *node = astgen_alloc(astgen, sizeof(AstTypeAtomNode), &ref);
  node->node.tag = AST_TAG_TYPE_ATOM;
  node->atom = atom;
  return ref;
}

AstNodeRef astgen_const_bool(AstGen *astgen, bool value) {
  AstNodeRef ref;
  AstConstNode *node = astgen_alloc(astgen, sizeof(AstConstNode), &ref);
  node->node.tag = AST_TAG_CONST_BOOL;
  node->bool_value = value;
  return ref;
}

AstNodeRef astgen_const_i32(AstGen *astgen, i32 value) {
  AstNodeRef ref;
  AstConstNode *node = astgen_alloc(astgen, sizeof(AstConstNode), &ref);
  node->node.tag = AST_TAG_CONST_I32;
  node->i32_value = value;
  return ref;
}

AstNodeRef astgen_binop(AstGen *astgen, AstNodeTag tag, AstNodeRef left_node, AstNodeRef right_node) {
  AstNodeRef ref;
  AstBinopNode *node = astgen_alloc(astgen, sizeof(AstBinopNode), &ref);
  node->node.tag = tag;
  node->left_node = left_node;
  node->right_node = right_node;
  return ref;
}

AstNodeRef astgen_local_write(AstGen *astgen, bool is_definition, u32 local_index, AstNodeRef expr) {
  AstNodeRef ref;
  AstLocalWriteNode *node = astgen_alloc(astgen, sizeof(AstLocalWriteNode), &ref);
  node->node.tag = AST_TAG_LOCAL_WRITE;
  node->is_definition = is_definition;
  node->local_index = local_index;
  node->expr = expr;
  return ref;
}

AstNodeRef astgen_local_read(AstGen *astgen, u32 local_index) {
  AstNodeRef ref;
  AstLocalReadNode *node = astgen_alloc(astgen, sizeof(AstLocalReadNode), &ref);
  node->node.tag = AST_TAG_LOCAL_READ;
  node->local_index = local_index;
  return ref;
}

AstNodeRef astgen_block(AstGen *astgen, bool is_static, u32 scope_index, u32 nodes_count, AstNodeRef nodes[]) {
  AstNodeRef ref;
  AstBlockNode *node = astgen_alloc(astgen, sizeof(AstBlockNode) + sizeof(AstNodeRef) * nodes_count, &ref);
  node->node.tag = AST_TAG_BLOCK;
  node->is_static = is_static;
  node->scope_index = scope_index;
  node->nodes_count = nodes_count;
  memcpy(node->nodes, nodes, sizeof(AstNodeRef) * nodes_count);
  return ref;
}

AstNodeRef astgen_if(AstGen *astgen, bool is_static, bool is_inline, u32 scope_index, AstNodeRef cond, AstNodeRef then, AstNodeRef els) {
  AstNodeRef ref;
  AstIfNode *node = astgen_alloc(astgen, sizeof(AstIfNode), &ref);
  node->node.tag = AST_TAG_IF;
  node->is_static = is_static;
  node->is_inline = is_inline;
  node->scope_index = scope_index;
  node->cond = cond;
  node->then = then;
  node->els = els;
  return ref;
}

AstNodeRef astgen_while(AstGen *astgen, bool is_static, bool is_inline, u32 scope_index, AstNodeRef cond, AstNodeRef body) {
  AstNodeRef ref;
  AstWhileNode *node = astgen_alloc(astgen, sizeof(AstWhileNode), &ref);
  node->node.tag = AST_TAG_WHILE;
  node->is_static = is_static;
  node->is_inline = is_inline;
  node->scope_index = scope_index;
  node->cond = cond;
  node->body = body;
  return ref;
}

AstNodeRef astgen_break(AstGen *astgen, bool is_static, u32 scope_index) {
  AstNodeRef ref;
  AstBreakContNode *node = astgen_alloc(astgen, sizeof(AstBreakContNode), &ref);
  node->node.tag = AST_TAG_BREAK;
  node->is_static = is_static;
  node->scope_index = scope_index;
  return ref;
}

AstNodeRef astgen_continue(AstGen *astgen, bool is_static, u32 scope_index) {
  AstNodeRef ref;
  AstBreakContNode *node = astgen_alloc(astgen, sizeof(AstBreakContNode), &ref);
  node->node.tag = AST_TAG_CONT;
  node->is_static = is_static;
  node->scope_index = scope_index;
  return ref;
}

AstNodeRef astgen_return(AstGen *astgen, AstNodeRef value_node) {
  AstNodeRef ref;
  AstReturnNode *node = astgen_alloc(astgen, sizeof(AstReturnNode), &ref);
  node->node.tag = AST_TAG_RETURN;
  node->value_node = value_node;
  return ref;
}

AstNodeRef astgen_func(AstGen *astgen, bool is_static, bool is_inline, AstNodeRef body, AstNodeRef return_type, u32 params_count, u32 locals_count, AstLocal locals[]) {
  AstNodeRef ref;
  AstFuncNode *node = astgen_alloc(astgen, sizeof(AstFuncNode) + sizeof(AstLocal) * locals_count, &ref);
  node->node.tag = AST_TAG_FUNC;
  node->is_static = is_static;
  node->is_inline = is_inline;
  node->body = body;
  node->return_type = return_type;
  node->params_count = params_count;
  node->locals_count = locals_count;
  memcpy(node->locals, locals, sizeof(AstLocal) * locals_count);
  return ref;
}

AstNodeRef astgen_create_test(AstGen *astgen) {
  return
    astgen_func(astgen, false, false,
      astgen_block(astgen, false, 0, 1, (AstNodeRef[]) {
        astgen_local_read(astgen, 0)
      }),
      astgen_type_atom(astgen, AST_TYPE_ATOM_I32),
    1, 1, (AstLocal[]) {
      {
        .is_param = true,
        .is_const = true,
        .is_static = false,
        .type = astgen_type_atom(astgen, AST_TYPE_ATOM_I32)
      }
    });
}