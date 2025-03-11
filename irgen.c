#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "irgen.h"
#include "vector.h"

#define NAME u32_to_u64
#define KEY_TYPE u32
#define VALUE_TYPE u64
#define HASH_FUNC(x) hashutil_uint32_hash(x)
#define EXPAND_INTERFACE
#define EXPAND_IMPLEMENTATION
#include "hashtable.h"


#define NAME u64_to_u32
#define KEY_TYPE u64
#define VALUE_TYPE u32
#define HASH_FUNC(x) hashutil_uint64_hash(x)
#define EXPAND_INTERFACE
#define EXPAND_IMPLEMENTATION
#include "hashtable.h"


typedef struct ListEntry {
  i16 values[3];
  i16 next;
} ListEntry;

static void list_push(i16 value, ListEntry *e, Vector(ListEntry) *entries) {
start:
  for (int i = 0; i < 3; ++i) {
    if (!e->values[i]) {
      e->values[i] = value;
      return;
    }
  }
  if (e->next) {
    e = *entries + e->next;
    goto start;
  }
  int index = vector_size(*entries);
  assert(index < INT16_MAX);
  e->next = index;
  vector_push(*entries, ((ListEntry) {{ value }}));
}

static void list_clear(ListEntry *e, ListEntry **entries) {
start:
  for (int i = 0; i < 3; ++i) {
    e->values[i] = 0;
  }
  if (e->next) {
    e = *entries + e->next;
    goto start;
  }
}



typedef struct Variable {
  IrType type;
} Variable;


typedef struct BasicBlock {
  u32 is_sealed : 1;
  u32 is_filled : 1;
  u32 is_visited : 1;
  u32 start : 29;
  u16 succs[2];
  ListEntry preds;
  ListEntry incomplete_phis;
  ListEntry suffix;
} BasicBlock;

struct IrGen {
  IrInstr *code;
  i32 numneg, maxneg, numpos, maxpos;

  i32 phi_count;
  IrBlockRef curr_block;
  Vector(BasicBlock) blocks;
  Vector(Variable) vars;
  Vector(ListEntry) list_entries;
  Vector(IrInstrRef) upsilons;
  u32_to_u64 bindings;
  u64_to_u32 ircache;
};




static void print_instr(IrGen *gen, IrInstrRef ref, IrInstr instr) {
  switch (instr.tag) {
    case IR_NOP: printf("  NOP\n"); break;
    case IR_IDENTITY: printf("  %d = ID %d\n", ref, instr.arg0); break;
    case IR_LABEL: printf(":%d\n", instr.arg0); break;
    case IR_JUMP: printf("  JUMP :%d\n", instr.arg0); break;
    case IR_BRANCH: printf("  BRANCH %d :%d :%d\n", instr.arg0, instr.arg1, instr.arg2); break;
    case IR_RET: printf("  RET %d\n", instr.arg0); break;
    case IR_UPSILON: printf("  UPSILON %d %d\n", instr.arg0, instr.arg1); break;
    case IR_CONST:
      switch (instr.type) {
        case TY_VOID: printf("  %d = CONST void\n", ref); break;
        case TY_BOOL: printf("  %d = CONST %s\n", ref, instr.bool_const ? "true" : "false"); break;
        case TY_I32: printf("  %d = CONST %d\n", ref, instr.i32_const); break;
        default: assert(0); break;
      }
      break;
    case IR_PHI: printf("  %d = PHI #%d\n", ref, instr.arg0); break;
    case IR_ARG: printf("  %d = ARG #%d\n", ref, instr.arg0); break;
    case IR_ADD: printf("  %d = ADD %d %d\n", ref, instr.arg0, instr.arg1); break;
    case IR_EQ: printf("  %d = EQ %d %d\n", ref, instr.arg0, instr.arg1); break;
    default: assert(0); break;
  }
}

void irgen_print_ir(IrGen *gen) {
  for (IrInstrRef ref = -gen->numneg; ref < gen->numpos; ++ref) {
    print_instr(gen, ref, gen->code[ref]);
  }
}



IrBlockRef irgen_create_block(IrGen *gen) {
  i32 block = vector_size(gen->blocks);
  assert(block < INT16_MAX);
  vector_push(gen->blocks, (BasicBlock) { });
  return block;
}

static BasicBlock *get_block(IrGen *gen, IrBlockRef block) {
  assert(block);
  while (vector_size(gen->blocks) <= block) {
    irgen_create_block(gen);
  }
  return gen->blocks + block;
}

static void init_code_array(IrGen *gen) {
  gen->maxneg = gen->maxpos = 16;
  gen->code = (IrInstr *)malloc(sizeof(IrInstr) * (gen->maxneg + gen->maxpos)) + gen->maxneg;
  gen->code[0] = (IrInstr) { .tag = IR_NOP, .type = TY_VOID, }; // let instr at index 0 be NOP
  gen->numpos = 1;
  gen->numneg = 0;
}

IrGen *irgen_create(void) {
  IrGen *gen = calloc(1, sizeof(IrGen));
  init_code_array(gen);
  irgen_create_block(gen); // block at index 0 should not be used
  return gen;
}

void irgen_destroy(IrGen *gen) {
  vector_free(gen->blocks);
  vector_free(gen->vars);
  vector_free(gen->list_entries);
  vector_free(gen->upsilons);
  u32_to_u64_cleanup(&gen->bindings);
  u64_to_u32_cleanup(&gen->ircache);
  free(gen);
}

void irgen_clear(IrGen *gen) {
  gen->numpos = 0;
  gen->numneg = 0;
  gen->phi_count = 0;
  gen->curr_block = 0;
  vector_clear(gen->blocks);
  vector_clear(gen->vars);
  vector_clear(gen->list_entries);
  vector_clear(gen->upsilons);
  u32_to_u64_clear(&gen->bindings);
  u64_to_u32_clear(&gen->ircache);
}

IrVarRef irgen_create_variable(IrGen *gen, IrType type) {
  i32 var = vector_size(gen->vars);
  assert(var < INT16_MAX);
  vector_push(gen->vars, ((Variable) { .type = type }));
  return var;
}


static IrInstrRef emit_instr_pinned(IrGen *gen, IrInstr instr) {
  if (gen->numpos == gen->maxpos) {
    u32 maxpos = gen->maxpos * 2;
    IrInstr *code = (IrInstr *)malloc(sizeof(IrInstr) * (gen->maxneg + maxpos)) + gen->maxneg;
    memcpy(code - gen->numneg, gen->code - gen->numneg, sizeof(IrInstr) * (gen->numneg + gen->numpos));
    free(gen->code - gen->maxneg);
    gen->code = code;
    gen->maxpos = maxpos;
  }

  i32 ref = gen->numpos++;
  assert(ref < INT16_MAX);
  gen->code[ref] = instr;
  return ref;
}

static IrInstrRef emit_instr_unpinned(IrGen *gen, IrInstr instr) {
  if (IR_IS_PURE_INSTR(instr.tag)) {
    u32 cached;
    if (u64_to_u32_get(&gen->ircache, instr.u64_repr, &cached)) {
      return cached;
    }
  }

  if (gen->numneg == gen->maxneg) {
    u32 maxneg = gen->maxneg * 2;
    IrInstr *code = (IrInstr *)malloc(sizeof(IrInstr) * (maxneg + gen->maxpos)) + maxneg;
    memcpy(code - gen->numneg, gen->code - gen->numneg, sizeof(IrInstr) * (gen->numneg + gen->numpos));
    free(gen->code - gen->maxneg);
    gen->code = code;
    gen->maxneg = maxneg;
  }

  i32 ref = -(++gen->numneg);
  assert(ref > INT16_MIN);
  gen->code[ref] = instr;
  if (IR_IS_PURE_INSTR(instr.tag)) {
    u64_to_u32_put(&gen->ircache, instr.u64_repr, ref);
  }
  return ref;
}

static IrInstrRef append_block_instr(IrGen *gen, IrBlockRef block, IrInstr instr) {
  if (block == gen->curr_block) {
    assert(!IR_IS_PURE_INSTR(instr.tag));
    return emit_instr_pinned(gen, instr);
  }
  IrInstrRef ref = emit_instr_unpinned(gen, instr);
  BasicBlock *b = get_block(gen, block);
  list_push(ref, &b->suffix, &gen->list_entries);
  return ref;
}



static IrInstr to_instr(IrGen *gen, IrInstrRef ref) {
  IrInstr instr = gen->code[ref];
  switch (instr.tag) {
    case IR_CONST:
    case IR_IDENTITY: return instr;
    default: return (IrInstr) { .tag = IR_IDENTITY, .type = instr.type, .arg0 = ref };
  }
}

static IrInstrRef intern_instr(IrGen *gen, IrInstr instr) {
  if (instr.tag == IR_IDENTITY) {
    return instr.arg0;
  }
  assert(IR_IS_PURE_INSTR(instr.tag));
  return emit_instr_unpinned(gen, instr);
}



void irgen_label(IrGen *gen, IrBlockRef block) {
  BasicBlock *b = get_block(gen, block);
  assert(!b->start);
  assert(!b->is_filled);
  assert(!b->succs[0]);
  assert(!b->succs[1]);
  assert(!gen->curr_block || get_block(gen, gen->curr_block)->is_filled);
  IrInstrRef ref = emit_instr_pinned(gen, (IrInstr) { .tag = IR_LABEL, .arg0 = block });
  b->start = ref;
  gen->curr_block = block;
}

void irgen_jump(IrGen *gen, IrBlockRef block, IrBlockRef target) {
  BasicBlock *b = get_block(gen, block);
  BasicBlock *tb = get_block(gen, target);
  assert(!b->is_filled);
  assert(!b->succs[0]);
  assert(!tb->is_sealed);
  b->succs[0] = target;
  b->is_filled = true;
  list_push(gen->curr_block, &tb->preds, &gen->list_entries);
  append_block_instr(gen, block, (IrInstr) { .tag = IR_JUMP, .arg0 = target });
}

void irgen_branch(IrGen *gen, IrBlockRef block, IrInstr cond, IrBlockRef true_target, IrBlockRef false_target) {
  BasicBlock *b = get_block(gen, block);
  BasicBlock *tb = get_block(gen, true_target);
  BasicBlock *fb = get_block(gen, false_target);
  assert(!b->is_filled);
  assert(!b->succs[0]);
  assert(!b->succs[1]);
  assert(!tb->is_sealed);
  assert(!fb->is_sealed);
  assert(cond.type == TY_BOOL);
  if (cond.tag == IR_CONST) {
    irgen_jump(gen, block, cond.bool_const ? true_target : false_target);
    return;
  }
  b->succs[0] = true_target;
  b->succs[1] = false_target;
  b->is_filled = true;
  list_push(gen->curr_block, &tb->preds, &gen->list_entries);
  list_push(gen->curr_block, &fb->preds, &gen->list_entries);
  append_block_instr(gen, block, (IrInstr) { .tag = IR_BRANCH, .arg0 = intern_instr(gen, cond), .arg1 = true_target, .arg2 = false_target });
}

void irgen_ret(IrGen *gen, IrBlockRef block, IrInstr retval) {
  BasicBlock *b = get_block(gen, block);
  assert(!b->is_filled);
  b->is_filled = true;
  append_block_instr(gen, block, (IrInstr) { .tag = IR_RET, .arg0 = intern_instr(gen, retval) });
}

void irgen_upsilon(IrGen *gen, IrBlockRef block, IrInstr val, IrInstr phi) {
  // upsilons may be emitted even after a block is filled (recursive variable lookup will create upsilons in predecessor blocks as needed)
  if (phi.type != TY_VOID) {
    IrInstrRef ref = append_block_instr(gen, block, (IrInstr) { .tag = IR_UPSILON, .arg0 = intern_instr(gen, val), .arg1 = intern_instr(gen, phi) });
    vector_push(gen->upsilons, ref);
  }
}

IrInstr irgen_const_bool(IrGen *gen, bool val) {
  return (IrInstr) { .tag = IR_CONST, .type = TY_BOOL, .bool_const = val };
}

IrInstr irgen_const_i32(IrGen *gen, i32 val) {
  return (IrInstr) { .tag = IR_CONST, .type = TY_I32, .i32_const = val };
}

IrInstr irgen_phi(IrGen *gen, IrType type) {
  return (IrInstr) { .tag = IR_PHI, .type = type, .arg0 = ++gen->phi_count };
}

IrInstr irgen_arg(IrGen *gen, u32 arg, IrType type) {
  return (IrInstr) { .tag = IR_ARG, .type = type, .arg0 = arg };
}

IrInstr irgen_add(IrGen *gen, IrInstr arg0, IrInstr arg1) {
  assert(arg0.type == arg1.type);
  if (arg0.tag == IR_CONST && arg1.tag == IR_CONST) {
    switch (arg0.type) {
      case TY_I32: return (IrInstr) { .tag = IR_CONST, .type = arg0.type, .i32_const = arg0.i32_const + arg1.i32_const };
      default: assert(0 && "unexpected type");
    }
  }
  return (IrInstr) { .tag = IR_ADD, .type = arg0.type, .arg0 = intern_instr(gen, arg0), .arg1 = intern_instr(gen, arg1) };
}

IrInstr irgen_eq(IrGen *gen, IrInstr arg0, IrInstr arg1) {
  assert(arg0.type == arg1.type);
  if (arg0.tag == IR_CONST && arg1.tag == IR_CONST) {
    switch (arg0.type) {
      case TY_BOOL: return (IrInstr) { .tag = IR_CONST, .type = TY_BOOL, .bool_const = arg0.bool_const == arg1.bool_const };
      case TY_I32: return (IrInstr) { .tag = IR_CONST, .type = TY_BOOL, .bool_const = arg0.i32_const == arg1.i32_const };
      default: assert(0 && "unexpected type");
    }
  }
  return (IrInstr) { .tag = IR_EQ, .type = TY_BOOL, .arg0 = intern_instr(gen, arg0), .arg1 = intern_instr(gen, arg1) };
}





typedef union {
  struct { i16 block, var; };
  i32 u32_repr;
} BindingKey;

static IrInstrRef create_pred_upsilons(IrGen *gen, IrBlockRef block, IrInstrRef phi);

void irgen_write_variable(IrGen *gen, IrBlockRef block, IrVarRef var, IrInstr val) {
  BindingKey key = {{ block, var }};
  u32_to_u64_put(&gen->bindings, key.u32_repr, val.u64_repr);
}

IrInstr irgen_read_variable(IrGen *gen, IrBlockRef block, IrVarRef var) {
  BindingKey key = {{ block, var }};
  IrInstr found;
  if (u32_to_u64_get(&gen->bindings, key.u32_repr, &found.u64_repr)) {
    return found;
  }

  IrInstr result;
  BasicBlock *b = get_block(gen, block);
  if (!b->is_sealed) {
    IrInstr phi = irgen_phi(gen, gen->vars[var].type);
    list_push(intern_instr(gen, phi), &b->incomplete_phis, &gen->list_entries);
    result = phi;
  } else if (b->preds.values[0] && !b->preds.values[1]) {
    // exactly one predecessor
    result = irgen_read_variable(gen, b->preds.values[0], var);
  } else {
    IrInstr phi = irgen_phi(gen, gen->vars[var].type);
    irgen_write_variable(gen, block, var, phi);
    result = to_instr(gen, create_pred_upsilons(gen, block, intern_instr(gen, phi)));
  }

  irgen_write_variable(gen, block, var, result);
  return result;
}

static IrInstrRef try_remove_trivial_phi(IrGen *gen, IrInstrRef phi) {
  IrInstrRef phi_users[128];
  int phi_users_count = 0;

  IrInstrRef same = 0, ref;
  vector_foreach(gen->upsilons, ref) {
    IrInstr instr = gen->code[ref];
    if (instr.arg2 == phi) {
      // this is an upsilon which writes the shadow variable for this phi (so it is in effect an operand)
      IrInstrRef op = instr.arg1;
      if (op == same || op == phi) {
        continue;
      }
      if (same) {
        return phi;
      }
      same = op;
    } else if (instr.arg1 == phi) {
      // this upsilon defines an operand to a different phi, but the value written is this phi.
      // record this phi user, so we can try to recursively remove trivial phis.
      assert(phi_users_count < 128);
      phi_users[phi_users_count++] = instr.arg2;
    }
  }

  // the phi was trivial. replace with the referenced value
  gen->code[phi] = (IrInstr) { .tag = IR_IDENTITY, .arg0 = same };

  for (int i = 0; i < phi_users_count; ++i) {
    try_remove_trivial_phi(gen, phi_users[i]);
  }
}

static IrInstrRef create_pred_upsilons(IrGen *gen, IrBlockRef block, IrInstrRef phi) {
  IrVarRef var = gen->code[phi].arg2;
  BasicBlock *b = get_block(gen, block);
  for (ListEntry *e = &b->preds; ; e = gen->list_entries + e->next) {
    for (int i = 0; i < 3; ++i) {
      IrBlockRef pred = e->values[i];
      if (!pred) goto end;
      IrInstr val = irgen_read_variable(gen, pred, var);
      irgen_upsilon(gen, pred, val, to_instr(gen, phi));
    }
    if (!e->next) break;
  }

end:
  return phi;
  //return try_remove_trivial_phi(gen, phi);
}

void irgen_seal_block(IrGen *gen, IrBlockRef block) {
  BasicBlock *b = get_block(gen, block);
  assert(!b->is_sealed);
  for (ListEntry *e = &b->incomplete_phis; ; e = gen->list_entries + e->next) {
    for (int i = 0; i < 3; ++i) {
      IrInstrRef phi = e->values[i];
      if (!phi) goto end;
      create_pred_upsilons(gen, block, phi);
    }
    if (!e->next) break;
  }

end:
  b->is_sealed = true;
}




static IrInstrRef fixup_instr(IrGen *gen, IrGen *source, i16 *map, IrInstrRef source_id) {
  printf("fixup: "); print_instr(source, source_id, source->code[source_id]);
  if (map[source_id]) {
    return map[source_id];
  }

#define FIXUP(Id) gen->code[fixup_instr(gen, source, map, Id)]
#define FIXUP_BINOP(EmitFunc) intern_instr(gen, EmitFunc(gen, FIXUP(instr.arg0), FIXUP(instr.arg1)))

  IrInstrRef dest_id = 0;
  IrInstr instr = source->code[source_id];
  switch (instr.tag) {
    case IR_NOP: return 0;
    case IR_IDENTITY: dest_id = fixup_instr(gen, source, map, instr.arg0); break;
    case IR_LABEL: irgen_label(gen, instr.arg0); return 0;
    case IR_JUMP: irgen_jump(gen, gen->curr_block, instr.arg0); return 0;
    case IR_BRANCH: irgen_branch(gen, gen->curr_block, FIXUP(instr.arg0), instr.arg1, instr.arg2); return 0;
    case IR_RET: irgen_ret(gen, gen->curr_block, FIXUP(instr.arg0)); return 0;
    case IR_UPSILON: irgen_upsilon(gen, gen->curr_block, FIXUP(instr.arg0), FIXUP(instr.arg1)); return 0;
    case IR_CONST: dest_id = intern_instr(gen, instr); break;
    case IR_PHI: dest_id = intern_instr(gen, instr); break;
    case IR_ARG: dest_id = intern_instr(gen, instr); break;
    case IR_ADD: dest_id = FIXUP_BINOP(irgen_add); break;
    case IR_EQ: dest_id = FIXUP_BINOP(irgen_eq); break;
    default: assert(0 && "unknown instruction");
  }

  map[source_id] = dest_id;
  return dest_id;
}

static void fixup_block(IrGen *gen, IrGen *source, i16 *map, IrBlockRef block) {
  BasicBlock *b = get_block(source, block);
  if (b->is_visited) {
    return;
  }
  b->is_visited = true;
  assert(b->is_sealed);
  assert(b->is_filled);

  IrInstrRef source_id = b->start;
  while (!IR_IS_TERMINAL_INSTR(source->code[source_id].tag)) {
    assert(source_id < source->maxpos);
    fixup_instr(gen, source, map, source_id++);
  }

  for (ListEntry *e = &b->suffix; ; e = source->list_entries + e->next) {
    for (int i = 0; i < 3; ++i) {
      IrInstrRef ref = e->values[i];
      if (!ref) goto suffix_done;
      fixup_instr(gen, source, map, ref);
    }
    if (!e->next) break;
  }
suffix_done:

  fixup_instr(gen, source, map, source_id); // terminal instruction

  for (int i = 0; i < 2; ++i) {
    if (b->succs[i]) {
      fixup_block(gen, source, map, b->succs[i]);
    }
  }
}

void irgen_fixup_ir(IrGen *gen, IrGen *source) {
  i16 map_buffer[65536] = { 0, };
  i16 *map = map_buffer + 32768; // map from old to new IrInstrRef
  gen->phi_count = gen->phi_count;
  fixup_block(gen, source, map, 1);
}

