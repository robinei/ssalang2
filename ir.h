#pragma once

#include "defs.h"

typedef u32 TypeId;

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

  // all after this point must be pure
  IR_CONST,
  IR_PHI,
  IR_ARG, // argument with rhs being index
  IR_ADD,
  IR_EQ,
} InstrTag;

#define IR_IS_PURE(x) ((x) >= IR_CONST)
#define IR_IS_TERMINAL(x) ((x) >= IR_JUMP && (x) <= IR_RET)


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
