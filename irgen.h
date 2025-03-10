#pragma once

#include "ir.h"

typedef u32 BlockId;
typedef u32 VarId;

typedef struct IrGen IrGen;

BlockId irgen_create_block(IrGen *gen);
VarId irgen_create_variable(IrGen *gen, TypeId type);
void irgen_write_variable(IrGen *gen, BlockId block, VarId var, Instr val);
Instr irgen_read_variable(IrGen *gen, BlockId block, VarId var);

void irgen_label(IrGen *gen, BlockId block);
void irgen_jump(IrGen *gen, BlockId block, BlockId target);
void irgen_branch(IrGen *gen, BlockId block, Instr cond, BlockId true_target, BlockId false_target);
void irgen_ret(IrGen *gen, BlockId block, Instr retval);

void irgen_upsilon(IrGen *gen, BlockId block, Instr val, Instr phi);

Instr irgen_const_bool(IrGen *gen, bool val);
Instr irgen_const_i32(IrGen *gen, i32 val);
Instr irgen_add(IrGen *gen, Instr lhs, Instr rhs);
Instr irgen_phi(IrGen *gen, TypeId type);
Instr irgen_arg(IrGen *gen, u32 arg, TypeId type);
Instr irgen_eq(IrGen *gen, Instr lhs, Instr rhs);
