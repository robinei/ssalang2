#pragma once

#include "ir.h"

typedef struct IrGen IrGen;

IrGen *irgen_create(void);
void irgen_destroy(IrGen *gen);
void irgen_clear(IrGen *gen);

IrBlockRef irgen_create_block(IrGen *gen);
void irgen_seal_block(IrGen *gen, IrBlockRef block);

IrVarRef irgen_create_variable(IrGen *gen, IrType type);
void irgen_write_variable(IrGen *gen, IrBlockRef block, IrVarRef var, IrInstr val);
IrInstr irgen_read_variable(IrGen *gen, IrBlockRef block, IrVarRef var);

IrPhiRef irgen_create_phi(IrGen *gen);

// all blocks must start with a label instruction, identifying it
void irgen_label(IrGen *gen, IrBlockRef block);

// terminals (all blocks must end with exactly one of these)
void irgen_jump(IrGen *gen, IrBlockRef target);
void irgen_branch(IrGen *gen, IrInstr cond, IrBlockRef true_target, IrBlockRef false_target);
void irgen_ret(IrGen *gen, IrInstr retval);

void irgen_upsilon(IrGen *gen, IrBlockRef block, IrPhiRef phi, IrInstr val);

// pure instruction contructors (just pure computation and data dependencies, no side effects)
IrInstr irgen_const_bool(IrGen *gen, bool val);
IrInstr irgen_const_i32(IrGen *gen, i32 val);
IrInstr irgen_phi(IrGen *gen, IrPhiRef phi, IrType type, IrVarRef var);
IrInstr irgen_arg(IrGen *gen, u32 arg, IrType type);
IrInstr irgen_add(IrGen *gen, IrInstr lhs, IrInstr rhs);
IrInstr irgen_eq(IrGen *gen, IrInstr lhs, IrInstr rhs);
IrInstr irgen_neq(IrGen *gen, IrInstr lhs, IrInstr rhs);

void irgen_print_ir(IrGen *gen);
void irgen_fixup_ir(IrGen *gen, IrGen *source);
