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
  // IR_NOP is a no-op instruction, which will be removed later during fixup. replace an instruction with a NOP to remove it from the IR.
  IR_NOP,

  // IR_IDENTITY is used to replace an instruction with another instruction.
  // when fixing up the code later, each use of the original (IR_IDENTITY) instruction is replaced with a use of the replacement instruction.
  IR_IDENTITY, // arguments: (IrInstrRef replace_with)

  IR_PRINT, // arguments: (IrInstrRef value)
  
  // IR_LABEL annotates the start of a block
  IR_LABEL, // arguments: (IrBlockRef block)

  // terminals
  IR_JUMP, // arguments: (IrBlockRef target)
  IR_BRANCH, // arguments: (IrInstrRef cond, IrBlockRef true_target, IrBlockRef false_target)
  IR_RET, // arguments: (IrInstrRef value)

  // IR_UPSILON instructions are inserted at source blocks, and indicate which value to use for an IR_PHI instruction at the target block
  // this is slightly different to how phi arguments are usually encoded
  IR_UPSILON, // arguments: (IrPhiRef phi, IrInstrRef value)

  // IR_PHI instructions are used to merge values from different blocks
  IR_PHI, // arguments: (IrPhiRef phi)

  // all after this point must be pure instructions

  // constants which have their value stored in the instruction itself
  IR_CONST,
  
  // IR_ARG instructions represent function arguments
  IR_ARG, // arguments: (IrOperand argument_index)

  // binary operations
  IR_ADD, // arguments: (IrInstrRef lhs, IrInstrRef rhs)
  IR_EQ, // arguments: (IrInstrRef lhs, IrInstrRef rhs)
  IR_NEQ, // arguments: (IrInstrRef lhs, IrInstrRef rhs)

  IR_NUM_TAGS,
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
