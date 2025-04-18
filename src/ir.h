#pragma once

#include "defs.h"

typedef i16 IrOperand;

typedef IrOperand IrInstrRef; // signed, so that pure instructions can have negative index (that way we don't commit to any sequencing of them early)
typedef IrOperand IrBlockRef;
typedef IrOperand IrPhiRef;
typedef IrOperand IrVarRef;

typedef enum IrType {
  TY_VOID,
  TY_BOOL,
  TY_I32,
} IrType;

#define IR_FLAG_MARK 1

// annotated with types for (arg0[, arg1[, arg2]])
typedef enum IrInstrTag {
  IR_NOP,
  IR_IDENTITY, // (IrInstrRef replace_with)

  IR_PRINT, // (IrInstrRef value)
  
  IR_LABEL, // (IrBlockRef block)

  // terminals
  IR_JUMP, // (IrBlockRef target)
  IR_BRANCH, // (IrInstrRef cond, IrBlockRef true_target, IrBlockRef false_target)
  IR_RET, // (IrInstrRef value)

  IR_UPSILON, // (IrPhiRef phi, IrInstrRef value)
  IR_PHI, // (IrPhiRef phi)

  // all after this point must be pure
  IR_CONST,
  IR_ARG, // (IrOperand argument_index)
  IR_ADD, // (IrInstrRef lhs, IrInstrRef rhs)
  IR_EQ, // (IrInstrRef lhs, IrInstrRef rhs)
  IR_NEQ, // (IrInstrRef lhs, IrInstrRef rhs)
} IrInstrTag;

#define IR_IS_PURE_INSTR(x) ((x) >= IR_CONST)
#define IR_IS_TERMINAL_INSTR(x) ((x) >= IR_JUMP && (x) <= IR_RET)

typedef union IrInstr {
  struct {
    IrInstrTag tag : 8;
    IrType type : 4;
    u32 flags : 4;
    IrOperand arg0 : 16;
    union {
      struct {
        IrOperand arg1, arg2;
      };
      bool bool_const;
      i32 i32_const;
      u32 u32_const;
      f32 f32_const;
    };
  };
  u64 u64_repr;
} IrInstr;
