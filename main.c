#include <stdio.h>
#include "astgen.h"
#include "irgen.h"


int main(int argc, char *argv[]) {
  IrGen *gen = irgen_create();
  IrVarRef x = irgen_create_variable(gen, TY_I32);
  IrBlockRef entry_block = irgen_create_block(gen);
  IrBlockRef then_block = irgen_create_block(gen);
  IrBlockRef else_block = irgen_create_block(gen);
  IrBlockRef exit_block = irgen_create_block(gen);

  printf("gen entry block:\n");
  irgen_seal_block(gen, entry_block);
  irgen_label(gen, entry_block);
  irgen_write_variable(gen, entry_block, x, irgen_const_i32(gen, 1));
  irgen_branch(gen, entry_block, irgen_arg(gen, 0, TY_BOOL), then_block, else_block);
  irgen_seal_block(gen, then_block);
  irgen_seal_block(gen, else_block);

  printf("\ngen then block:\n");
  irgen_label(gen, then_block);
  irgen_write_variable(gen, then_block, x, irgen_add(gen, irgen_read_variable(gen, then_block, x), irgen_add(gen, irgen_const_i32(gen, 5), irgen_const_i32(gen, 5))));
  irgen_jump(gen, then_block, exit_block);
  
  printf("\ngen else block:\n");
  irgen_label(gen, else_block);
  irgen_write_variable(gen, else_block, x, irgen_add(gen, irgen_read_variable(gen, else_block, x), irgen_const_i32(gen, 10)));
  irgen_jump(gen, else_block, exit_block);

  printf("\ngen exit block:\n");
  irgen_seal_block(gen, exit_block);
  irgen_label(gen, exit_block);
  irgen_ret(gen, exit_block, irgen_read_variable(gen, exit_block, x));

  printf("\nbefore:\n");
  irgen_print_ir(gen);

  IrGen *fixed = irgen_create();
  irgen_fixup_ir(fixed, gen);
  
  printf("\nafter:\n");
  irgen_print_ir(fixed);

  irgen_destroy(gen);
  irgen_destroy(fixed);

  return 0;
}
