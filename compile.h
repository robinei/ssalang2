#pragma once

#include "ast.h"

typedef i16 InstrId; // signed, so that pure instructions can have negative id (that way we don't commit to any sequencing of them early)
typedef u32 BlockId;
typedef u32 TypeId;
typedef u32 VarId;

enum {
  TY_VOID,
  TY_BOOL,
  TY_I32,
};

typedef enum InstrTag {
  IR_NOP,
  IR_IDENTITY, // replace with lhs value
  
  IR_LABEL, // identify the start of BlockId rhs
  IR_JUMP, // unconditional jump to BlockId rhs
  IR_JFALSE, // jump to BlockId rhs if the condition value lhs is false
  IR_RET, // return value lhs

  IR_UPSILON, // assign lhs value to rhs phi

  IR_FIRST_PURE, // all after this must be pure (have no side effects)
  IR_CONST = IR_FIRST_PURE,
  IR_PHI,
  IR_ARG, // argument with rhs being index
  IR_ADD,
  IR_EQ,
} InstrTag;

typedef union Instr {
  struct {
    InstrTag tag : 8;
    TypeId type : 24;
    union {
      struct {
        i16 lhs, rhs;
      };
      bool bool_const;
      i32 i32_const;
      u32 u32_const;
      f32 f32_const;
    };
  };
  u64 u64_repr;
} Instr;

typedef struct IrGen IrGen;

BlockId create_block(IrGen *gen);
VarId create_variable(IrGen *gen, TypeId type);
void write_variable(IrGen *gen, BlockId block, VarId var, Instr val);
Instr read_variable(IrGen *gen, BlockId block, VarId var);

void emit_label(IrGen *gen, BlockId block);
void emit_jump(IrGen *gen, BlockId block, BlockId target);
void emit_jfalse(IrGen *gen, BlockId block, BlockId false_target, Instr cond);
void emit_ret(IrGen *gen, BlockId block, Instr retval);

void emit_upsilon(IrGen *gen, BlockId block, Instr val, Instr phi);

Instr emit_bool(IrGen *gen, bool val);
Instr emit_i32(IrGen *gen, i32 val);
Instr emit_add(IrGen *gen, Instr lhs, Instr rhs);
Instr emit_phi(IrGen *gen, TypeId type);
Instr emit_arg(IrGen *gen, u32 arg, TypeId type);
Instr emit_eq(IrGen *gen, Instr lhs, Instr rhs);
