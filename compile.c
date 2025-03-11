#include <assert.h>
#include <setjmp.h>

#include "ast.h"
#include "irgen.h"

typedef struct ScopeSlot {
  jmp_buf jump_buffer;
  IrBlockRef break_target;
  IrBlockRef cont_target;
} ScopeSlot;

typedef struct LocalSlot {
  bool is_defined : 1;
  IrType type : 31;
  IrVarRef var;
  IrInstr value;
} LocalSlot;

typedef struct CompileContext {
  char *ast_buffer;
  IrGen *gen;
  IrBlockRef block;
  AstFuncNode *func;
  LocalSlot locals[100];
  ScopeSlot scopes[100];
} CompileContext;

IrType ast_resolve_type(AstNodeRef noderef, CompileContext *ctx) {
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

IrInstr ast_compile(AstNodeRef noderef, bool static_eval, CompileContext *ctx) {
    AstNode *basenode = (AstNode *)(ctx->ast_buffer + noderef);
    IrGen *gen = ctx->gen;
    IrInstr result = (IrInstr) { };

    switch (basenode->tag) {
      case AST_TAG_CONST_BOOL: {
        AstConstNode *node = (AstConstNode *)basenode;
        result = irgen_const_bool(gen, node->bool_value);
        break;
      }

      case AST_TAG_CONST_I32: {
        AstConstNode *node = (AstConstNode *)basenode;
        result = irgen_const_i32(gen, node->i32_value);
        break;
      }

      case AST_TAG_BINOP_ADD: {
        AstBinopNode *node = (AstBinopNode *)basenode;
        IrInstr left = ast_compile(node->left_node, static_eval, ctx);
        IrInstr right = ast_compile(node->right_node, static_eval, ctx);
        result = irgen_add(gen, left, right);
        break;
      }

      case AST_TAG_BINOP_EQUALS: {
        AstBinopNode *node = (AstBinopNode *)basenode;
        IrInstr left = ast_compile(node->left_node, static_eval, ctx);
        IrInstr right = ast_compile(node->right_node, static_eval, ctx);
        result = irgen_eq(gen, left, right);
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
          slot->var = irgen_create_variable(gen, slot->type);
          slot->is_defined = true;
        }
        else {
          assert(!local->is_const);
        }
        
        slot->value = ast_compile(node->expr, static_eval, ctx);
        assert(slot->type == slot->value.type);
        irgen_write_variable(gen, ctx->block, slot->var, slot->value);
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
          result = irgen_arg(gen, node->local_index, slot->type);
        }
        else {
          result = irgen_read_variable(gen, ctx->block, slot->var);
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
          IrInstr cond = ast_compile(node->cond, true, ctx);
          assert(cond.type == TY_BOOL);
          if (cond.bool_const) {
            result = ast_compile(node->then, static_eval, ctx);
          }
          else if (node->els) {
            result = ast_compile(node->els, static_eval, ctx);
          }
        }
        else {
          IrBlockRef then_block = irgen_create_block(gen);
          IrBlockRef else_block = node->els ? irgen_create_block(gen) : 0;
          IrBlockRef exit_block = irgen_create_block(gen);
          
          IrInstr cond = ast_compile(node->cond, static_eval, ctx);
          assert(cond.type == TY_BOOL);
          irgen_branch(gen, ctx->block, cond, then_block, else_block ? else_block : exit_block);

          irgen_label(gen, then_block);
          ctx->block = then_block;
          IrInstr then_result = ast_compile(node->then, static_eval, ctx);
          IrInstr phi = irgen_phi(gen, then_result.type);
          irgen_upsilon(gen, then_block, then_result, phi);
          irgen_jump(gen, then_block, exit_block);

          if (else_block) {
            irgen_label(gen, else_block);
            ctx->block = else_block;
            IrInstr else_result = ast_compile(node->els, static_eval, ctx);
            assert(then_result.type == else_result.type);
            irgen_upsilon(gen, else_block, else_result, phi);
            irgen_jump(gen, else_block, exit_block);
          }

          irgen_label(gen, exit_block);
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
            IrInstr cond = ast_compile(node->cond, true, ctx);
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
          IrBlockRef cond_block = irgen_create_block(gen);
          IrBlockRef body_block = irgen_create_block(gen);
          IrBlockRef exit_block = irgen_create_block(gen);

          irgen_jump(gen, ctx->block, cond_block);

          irgen_label(gen, cond_block);
          ctx->block = cond_block;
          IrInstr cond = ast_compile(node->cond, false, ctx);
          assert(cond.type == TY_BOOL);
          irgen_branch(gen, ctx->block, cond, body_block, exit_block);

          slot->break_target = exit_block;
          slot->cont_target = cond_block;

          irgen_label(gen, body_block);
          ctx->block = body_block;
          ast_compile(node->body, false, ctx);
          irgen_jump(gen, ctx->block, cond_block);

          irgen_label(gen, exit_block);
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
          irgen_jump(gen, ctx->block, slot->break_target);
        }
        else {
          assert(slot->cont_target);
          irgen_jump(gen, ctx->block, slot->cont_target);
        }
        break;
      }

      case AST_TAG_RETURN: {
        AstReturnNode *node = (AstReturnNode *)basenode;
        IrInstr value = ast_compile(node->value_node, static_eval, ctx);
        irgen_ret(gen, ctx->block, value);
        break;
      }
    }

    if (static_eval) {
      assert(result.tag == IR_CONST);
    }
    return result;
}

