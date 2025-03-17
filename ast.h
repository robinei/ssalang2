#pragma once

#include "defs.h"

typedef u32 AstRef;
typedef AstRef AstNodeRef;

typedef enum AstNodeTag {
  AST_TAG_TYPE_ATOM,
  
  AST_TAG_CONST_BOOL,
  AST_TAG_CONST_I32,

  AST_TAG_BINOP_ADD,
  AST_TAG_BINOP_EQ,
  AST_TAG_BINOP_NEQ,
  
  AST_TAG_LOCAL_WRITE,
  AST_TAG_LOCAL_READ,
  
  AST_TAG_BLOCK,
  AST_TAG_IF,
  AST_TAG_WHILE,
  
  AST_TAG_BREAK,
  AST_TAG_CONT,
  AST_TAG_RETURN,
  
  AST_TAG_FUNC,
} AstNodeTag;

typedef enum AstTypeAtomTag {
  AST_TYPE_ATOM_VOID,
  AST_TYPE_ATOM_BOOL,
  AST_TYPE_ATOM_I32,
} AstTypeAtomTag;

typedef struct AstNode {
  AstNodeTag tag : 8;
  u32 source_pos : 24;
} AstNode;

typedef struct AstConstNode {
  AstNode node;
  union {
    bool bool_value;
    i32 i32_value;
  };
} AstConstNode;

typedef struct AstBinopNode {
  AstNode node;
  AstNodeRef left_node;
  AstNodeRef right_node;
} AstBinopNode;

typedef struct AstTypeAtomNode {
  AstNode node;
  AstTypeAtomTag atom;
} AstTypeAtomNode;

typedef struct AstLocalWriteNode {
  AstNode node;
  bool is_definition : 1;
  u32 local_index : 31;
  AstNodeRef expr;
} AstLocalWriteNode;

typedef struct AstLocalReadNode {
  AstNode node;
  u32 local_index;
} AstLocalReadNode;

typedef struct AstBlockNode {
  AstNode node;
  bool is_static : 1;
  u32 scope_index : 31;
  u32 nodes_count;
  AstNodeRef nodes[];
} AstBlockNode;

typedef struct AstIfNode {
  AstNode node;
  bool is_static : 1;
  bool is_inline : 1;
  u32 scope_index : 30;
  AstNodeRef cond;
  AstNodeRef then;
  AstNodeRef els;
} AstIfNode;

typedef struct AstWhileNode {
  AstNode node;
  bool is_static : 1;
  bool is_inline : 1;
  u32 scope_index : 30;
  AstNodeRef cond;
  AstNodeRef body;
} AstWhileNode;

typedef struct AstBreakContNode {
  AstNode node;
  bool is_static : 1;
  u32 scope_index : 31;
  AstNodeRef value;
} AstBreakContNode;

typedef struct AstReturnNode {
  AstNode node;
  AstNodeRef value_node;
} AstReturnNode;

typedef struct AstLocal {
  bool is_param : 1;
  bool is_static : 1;
  bool is_const : 1;
  AstNodeRef type : 29;
} AstLocal;

typedef struct AstFuncNode {
  AstNode node;
  bool is_static : 1;
  bool is_inline : 1;
  AstNodeRef body : 30;
  AstNodeRef return_type;
  u32 params_count; // the params_count first locals are the parameters
  u32 locals_count;
  AstLocal locals[];
} AstFuncNode;
