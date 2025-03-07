#pragma once

#include "ir.h"

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