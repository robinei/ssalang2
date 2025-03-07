#include <assert.h>
#include <setjmp.h>

#include "ast.h"
#include "irgen.h"

typedef struct ScopeSlot {
  jmp_buf jump_buffer;
  BlockId break_target;
  BlockId cont_target;
} ScopeSlot;

typedef struct LocalSlot {
  bool is_defined : 1;
  TypeId type : 31;
  VarId var;
  Instr value;
} LocalSlot;

typedef struct CompileContext {
  char *ast_buffer;
  IrGen *gen;
  BlockId block;
  AstFuncNode *func;
  LocalSlot locals[100];
  ScopeSlot scopes[100];
} CompileContext;

TypeId ast_resolve_type(AstNodeRef noderef, CompileContext *ctx) {
  AstNode *basenode = (AstNode *)(ctx->ast_buffer + noderef);
  
  switch (basenode->tag) {
    case AST_TAG_TYPE_ATOM: {
      AstTypeAtomNode *node = (AstTypeAtomNode *)basenode;
      switch (node->atom) {
        case AST_TYPE_ATOM_VOID: return TY_VOID;
        case AST_TYPE_ATOM_BOOL: return TY_BOOL;
        case AST_TYPE_ATOM_I32: return TY_I32;
        default:
        assert(0 && "invalid type atom");
          break;
      }
      break;
    }
    default:
      assert(0 && "invalid type node");
      break;
  }
  return TY_VOID;
}

Instr ast_compile(AstNodeRef noderef, bool static_eval, CompileContext *ctx) {
    AstNode *basenode = (AstNode *)(ctx->ast_buffer + noderef);
    Instr result = (Instr) { };

    switch (basenode->tag) {
      case AST_TAG_CONST_BOOL: {
        AstConstNode *node = (AstConstNode *)basenode;
        result = emit_bool(ctx->gen, node->bool_value);
        break;
      }

      case AST_TAG_CONST_I32: {
        AstConstNode *node = (AstConstNode *)basenode;
        result = emit_i32(ctx->gen, node->i32_value);
        break;
      }

      case AST_TAG_BINOP_ADD: {
        AstBinopNode *node = (AstBinopNode *)basenode;
        Instr left = ast_compile(node->left_node, static_eval, ctx);
        Instr right = ast_compile(node->right_node, static_eval, ctx);
        result = emit_add(ctx->gen, left, right);
        break;
      }

      case AST_TAG_BINOP_EQUALS: {
        AstBinopNode *node = (AstBinopNode *)basenode;
        Instr left = ast_compile(node->left_node, static_eval, ctx);
        Instr right = ast_compile(node->right_node, static_eval, ctx);
        result = emit_eq(ctx->gen, left, right);
        break;
      }

      case AST_TAG_LOCAL_WRITE: {
        AstLocalWriteNode *node = (AstLocalWriteNode *)basenode;
        LocalSlot *slot = ctx->locals + node->local_index;
        AstLocal *local = ctx->func->locals + node->local_index;
        assert(!local->is_param);
        static_eval = static_eval || local->is_static;

        if (!slot->is_defined) {
          assert(node->is_definition);
          slot->type = ast_resolve_type(local->type, ctx);
          slot->var = create_variable(ctx->gen, slot->type);
          slot->is_defined = true;
        }
        else {
          assert(!local->is_const);
        }
        
        slot->value = ast_compile(node->expr, static_eval, ctx);
        assert(slot->type == slot->value.type);
        write_variable(ctx->gen, ctx->block, slot->var, slot->value);
        break;
      }

      case AST_TAG_LOCAL_READ: {
        AstLocalReadNode *node = (AstLocalReadNode *)basenode;
        LocalSlot *slot = ctx->locals + node->local_index;
        AstLocal *local = ctx->func->locals + node->local_index;
        assert(slot->is_defined);

        if (static_eval | local->is_static) {
          result = slot->value;
        }
        else if (local->is_param) {
          result = emit_arg(ctx->gen, node->local_index, slot->type);
        }
        else {
          result = read_variable(ctx->gen, ctx->block, slot->var);
        }
        break;
      }

      case AST_TAG_BLOCK: {
        AstBlockNode *node = (AstBlockNode *)basenode;
        static_eval = static_eval || node->is_static;

        for (u32 i = 0; i < node->nodes_count; ++i) {
          result = ast_compile(node->nodes[i], static_eval, ctx);
        }
        break;
      }

      case AST_TAG_IF: {
        AstIfNode *node = (AstIfNode *)basenode;
        static_eval = static_eval || node->is_static;

        if (static_eval || node->is_inline) {
          Instr cond = ast_compile(node->cond, true, ctx);
          assert(cond.type == TY_BOOL);
          if (cond.bool_const) {
            result = ast_compile(node->then, static_eval, ctx);
          }
          else if (node->els) {
            result = ast_compile(node->els, static_eval, ctx);
          }
        }
        else {
          BlockId then_block = create_block(ctx->gen);
          BlockId else_block = node->els ? create_block(ctx->gen) : 0;
          BlockId exit_block = create_block(ctx->gen);
          
          Instr cond = ast_compile(node->cond, static_eval, ctx);
          assert(cond.type == TY_BOOL);
          emit_jfalse(ctx->gen, ctx->block, else_block ? else_block : exit_block, cond);
          emit_jump(ctx->gen, ctx->block, then_block);

          emit_label(ctx->gen, then_block);
          ctx->block = then_block;
          Instr then_result = ast_compile(node->then, static_eval, ctx);
          Instr phi = emit_phi(ctx->gen, then_result.type);
          emit_upsilon(ctx->gen, then_block, then_result, phi);
          emit_jump(ctx->gen, then_block, exit_block);

          if (else_block) {
            emit_label(ctx->gen, else_block);
            ctx->block = else_block;
            Instr else_result = ast_compile(node->els, static_eval, ctx);
            assert(then_result.type == else_result.type);
            emit_upsilon(ctx->gen, else_block, else_result, phi);
            emit_jump(ctx->gen, else_block, exit_block);
          }

          emit_label(ctx->gen, exit_block);
          ctx->block = exit_block;
          result = phi;
        }
        break;
      }

      case AST_TAG_WHILE: {
        AstWhileNode *node = (AstWhileNode *)basenode;
        ScopeSlot *slot = ctx->scopes + node->scope_index;
        static_eval = static_eval || node->is_static;

        if (static_eval || node->is_inline) {
          for (;;) {
            Instr cond = ast_compile(node->cond, true, ctx);
            assert(cond.type == TY_BOOL);
            if (!cond.bool_const) {
              break;
            }
            int status = setjmp(slot->jump_buffer);
            if (status == 0) {
              ast_compile(node->body, static_eval, ctx);
            }
            else if (status == AST_TAG_BREAK) {
              break;
            }
            else {
              assert(status == AST_TAG_CONT);
              continue;
            }
          }
        }
        else {
          BlockId cond_block = create_block(ctx->gen);
          BlockId body_block = create_block(ctx->gen);
          BlockId exit_block = create_block(ctx->gen);

          emit_jump(ctx->gen, ctx->block, cond_block);

          emit_label(ctx->gen, cond_block);
          ctx->block = cond_block;
          Instr cond = ast_compile(node->cond, false, ctx);
          assert(cond.type == TY_BOOL);
          emit_jfalse(ctx->gen, ctx->block, exit_block, cond);
          emit_jump(ctx->gen, ctx->block, body_block);

          slot->break_target = exit_block;
          slot->cont_target = cond_block;

          emit_label(ctx->gen, body_block);
          ctx->block = body_block;
          ast_compile(node->body, false, ctx);
          emit_jump(ctx->gen, ctx->block, cond_block);

          emit_label(ctx->gen, exit_block);
          ctx->block = exit_block;
        }
        break;
      }

      case AST_TAG_BREAK:
      case AST_TAG_CONT: {
        AstBreakContNode *node = (AstBreakContNode *)basenode;
        ScopeSlot *slot = ctx->scopes + node->scope_index;
        if (node->is_static) {
          longjmp(slot->jump_buffer, basenode->tag);
        }
        else if (basenode->tag == AST_TAG_BREAK) {
          assert(slot->break_target);
          emit_jump(ctx->gen, ctx->block, slot->break_target);
        }
        else {
          assert(slot->cont_target);
          emit_jump(ctx->gen, ctx->block, slot->cont_target);
        }
        break;
      }

      case AST_TAG_RETURN: {
        AstReturnNode *node = (AstReturnNode *)basenode;
        Instr value = ast_compile(node->value_node, static_eval, ctx);
        emit_ret(ctx->gen, ctx->block, value);
        break;
      }
    }

    if (static_eval) {
      assert(result.tag == IR_CONST);
    }
    return result;
}

