#pragma once

#include "defs.h"

typedef struct AstRef {
    u32 offset;
} AstRef;

typedef struct AstText {
    u32 offset;
    u32 length;
} AstText;

typedef enum AstTag {
    AST_COMMENT,
    AST_MODULE,
    AST_BLOCK,
    AST_LET,
    AST_IF,
    AST_WHILE,
} AstTag;

#define AST_COMMON AstTag tag : 8; u32 flags : 24; u32 source_pos;

#define AST_FLAG_INLINE 1
#define AST_FLAG_CONST 2
#define AST_FLAG_STATIC 4

typedef struct AstNode {
    AST_COMMON
} AstNode;

typedef struct AstComment {
    AST_COMMON
    AstText text;
} AstComment;

typedef struct AstModule {
    AST_COMMON
    u32 nodes_size;
    AstRef nodes[];
} AstModule;

typedef struct AstBlock {
    AST_COMMON
    u32 nodes_size;
    AstRef nodes[];
} AstBlock;

typedef struct AstLet {
    AST_COMMON
    AstText name;
    AstRef type;
    AstRef expr;
} AstLet;

typedef struct AstIf {
    AST_COMMON
    AstRef cond;
    AstRef then;
    AstRef els;
} AstIf;

typedef struct AstWhile {
    AST_COMMON
    AstRef cond;
    AstRef body;
} AstWhile;

typedef struct AstFunc {
    AST_COMMON
    AstRef body;
    AstRef return_type;
    u32 params_size;
    AstParam params[];
} AstFunc;

typedef struct AstParam {
    u32 flags;
    AstText name;
    AstRef type;
} AstParam;



typedef struct ParseContext {
    char *buffer;
    u32 buffer_size;
    u32 buffer_capacity;    
} ParseContext;


