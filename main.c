#include <stdio.h>
#include "astgen.h"
#include "irgen.h"


static void build_loop(IrGen *gen) {
  IrVarRef x = irgen_create_variable(gen, TY_I32);
  IrBlockRef entry_block = irgen_create_block(gen);
  IrBlockRef cond_block = irgen_create_block(gen);
  IrBlockRef body_block = irgen_create_block(gen);
  IrBlockRef exit_block = irgen_create_block(gen);

  printf("gen entry block:\n");
  irgen_seal_block(gen, entry_block);
  irgen_label(gen, entry_block);
  irgen_write_variable(gen, entry_block, x, irgen_const_i32(gen, 0));
  irgen_jump(gen, cond_block);

  printf("\ngen cond block:\n");
  irgen_label(gen, cond_block);
  irgen_branch(gen, irgen_neq(gen, irgen_read_variable(gen, cond_block, x), irgen_const_i32(gen, 10)), body_block, exit_block);
  irgen_seal_block(gen, exit_block);

  printf("\ngen body block:\n");
  irgen_seal_block(gen, body_block);
  irgen_label(gen, body_block);
  irgen_write_variable(gen, body_block, x, irgen_add(gen, irgen_read_variable(gen, body_block, x), irgen_const_i32(gen, 1)));
  irgen_jump(gen, cond_block);
  irgen_seal_block(gen, cond_block);

  printf("\ngen exit block:\n");
  irgen_label(gen, exit_block);
  irgen_ret(gen, irgen_read_variable(gen, exit_block, x));
}

static void build_if(IrGen *gen) {
  IrVarRef x = irgen_create_variable(gen, TY_I32);
  IrBlockRef entry_block = irgen_create_block(gen);
  IrBlockRef then_block = irgen_create_block(gen);
  IrBlockRef else_block = irgen_create_block(gen);
  IrBlockRef exit_block = irgen_create_block(gen);

  printf("gen entry block:\n");
  irgen_seal_block(gen, entry_block);
  irgen_label(gen, entry_block);
  irgen_write_variable(gen, entry_block, x, irgen_const_i32(gen, 1));
  irgen_branch(gen, irgen_arg(gen, 0, TY_BOOL), then_block, else_block);
  irgen_seal_block(gen, then_block);
  irgen_seal_block(gen, else_block);

  printf("\ngen then block:\n");
  irgen_label(gen, then_block);
  irgen_write_variable(gen, then_block, x, irgen_add(gen, irgen_read_variable(gen, then_block, x), irgen_add(gen, irgen_const_i32(gen, 5), irgen_const_i32(gen, 5))));
  irgen_jump(gen, exit_block);
  
  printf("\ngen else block:\n");
  irgen_label(gen, else_block);
  irgen_write_variable(gen, else_block, x, irgen_add(gen, irgen_read_variable(gen, else_block, x), irgen_const_i32(gen, 10)));
  irgen_jump(gen, exit_block);
  irgen_seal_block(gen, exit_block);

  printf("\ngen exit block:\n");
  irgen_label(gen, exit_block);
  irgen_ret(gen, irgen_read_variable(gen, exit_block, x));
}

static void build_straight(IrGen *gen) {
  IrBlockRef b1 = irgen_create_block(gen);
  IrBlockRef b2 = irgen_create_block(gen);
  IrBlockRef b3 = irgen_create_block(gen);

  irgen_seal_block(gen, b1);
  irgen_label(gen, b1);
  irgen_print(gen, irgen_const_i32(gen, 1));
  irgen_jump(gen, b2);

  irgen_seal_block(gen, b2);
  irgen_label(gen, b2);
  irgen_print(gen, irgen_const_i32(gen, 1));
  irgen_jump(gen, b3);

  irgen_seal_block(gen, b3);
  irgen_label(gen, b3);
  irgen_print(gen, irgen_const_i32(gen, 1));
  irgen_ret(gen, irgen_const_i32(gen, 1));
}


int main(int argc, char *argv[]) {
  IrGen *gen = irgen_create();

  //build_loop(gen);
  build_if(gen);
  //build_straight(gen);

  printf("\nbefore:\n");
  irgen_print_ir(gen);

  IrGen *fixed = irgen_create();
  irgen_fixup_ir(fixed, gen);
  irgen_fixup_ir(gen, fixed);
  irgen_fixup_ir(fixed, gen);
  
  printf("\nafter:\n");
  irgen_print_ir(fixed);

  irgen_destroy(gen);
  irgen_destroy(fixed);

  return 0;
}
