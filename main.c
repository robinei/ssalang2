#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "astgen.h"
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



typedef i16 InstrId; // signed, so that pure instructions can have negative id (that way we don't commit to any sequencing of them early)

typedef struct Variable {
  TypeId type;
} Variable;


typedef struct Block {
  u32 is_sealed : 1;
  u32 is_filled : 1;
  u32 is_visited : 1;
  u32 start : 29;
  u16 succs[2];
  ListEntry preds;
  ListEntry incomplete_phis;
  ListEntry suffix;
} Block;

typedef struct IrGen {
  Instr *code;
  i32 numneg, maxneg, numpos, maxpos;

  i32 phi_count;
  BlockId curr_block;
  Vector(Block) blocks;
  Vector(Variable) vars;
  Vector(ListEntry) list_entries;
  Vector(InstrId) upsilons;
  u32_to_u64 bindings;
  u64_to_u32 ircache;
} IrGen;




static void print_instr(IrGen *gen, InstrId id, Instr instr) {
  switch (instr.tag) {
    case IR_NOP: printf("  NOP\n"); break;
    case IR_IDENTITY: printf("  %d = ID %d\n", id, instr.lhs); break;
    case IR_LABEL: printf(":%d\n", instr.rhs); break;
    case IR_JUMP: printf("  JUMP :%d\n", instr.rhs); break;
    case IR_BRANCH: printf("  BRANCH %d :%d :%d\n", instr.extra, instr.lhs, instr.rhs); break;
    case IR_RET: printf("  RET %d\n", instr.lhs); break;
    case IR_UPSILON: printf("  UPSILON %d %d\n", instr.lhs, instr.rhs); break;
    case IR_CONST:
      switch (instr.type) {
        case TY_VOID: printf("  %d = CONST void\n", id); break;
        case TY_BOOL: printf("  %d = CONST %s\n", id, instr.bool_const ? "true" : "false"); break;
        case TY_I32: printf("  %d = CONST %d\n", id, instr.i32_const); break;
        default: assert(0); break;
      }
      break;
    case IR_PHI: printf("  %d = PHI #%d\n", id, instr.rhs); break;
    case IR_ARG: printf("  %d = ARG #%d\n", id, instr.rhs); break;
    case IR_ADD: printf("  %d = ADD %d %d\n", id, instr.lhs, instr.rhs); break;
    case IR_EQ: printf("  %d = EQ %d %d\n", id, instr.lhs, instr.rhs); break;
    default: assert(0); break;
  }
}

static void print_ir(IrGen *gen) {
  for (InstrId id = -gen->numneg; id < gen->numpos; ++id) {
    print_instr(gen, id, gen->code[id]);
  }
}



BlockId irgen_create_block(IrGen *gen) {
  BlockId id = vector_size(gen->blocks);
  assert(id < INT16_MAX);
  vector_push(gen->blocks, (Block) { });
  return id;
}

static Block *get_block(IrGen *gen, BlockId block) {
  assert(block);
  while (vector_size(gen->blocks) <= block) {
    irgen_create_block(gen);
  }
  return gen->blocks + block;
}

static void init_code_array(IrGen *gen) {
  gen->maxneg = gen->maxpos = 16;
  gen->code = (Instr *)malloc(sizeof(Instr) * (gen->maxneg + gen->maxpos)) + gen->maxneg;
  gen->code[0] = (Instr) { .tag = IR_NOP, .type = TY_VOID, }; // let instr at index 0 be NOP
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

VarId irgen_create_variable(IrGen *gen, TypeId type) {
  VarId id = vector_size(gen->vars);
  assert(id < INT16_MAX);
  vector_push(gen->vars, ((Variable) { .type = type }));
  return id;
}


static InstrId emit_instr_pinned(IrGen *gen, Instr instr) {
  if (gen->numpos == gen->maxpos) {
    u32 maxpos = gen->maxpos * 2;
    Instr *code = (Instr *)malloc(sizeof(Instr) * (gen->maxneg + maxpos)) + gen->maxneg;
    memcpy(code - gen->numneg, gen->code - gen->numneg, sizeof(Instr) * (gen->numneg + gen->numpos));
    free(gen->code - gen->maxneg);
    gen->code = code;
    gen->maxpos = maxpos;
  }

  i32 id = gen->numpos++;
  assert(id < INT16_MAX);
  gen->code[id] = instr;
  return id;
}

static InstrId emit_instr_unpinned(IrGen *gen, Instr instr) {
  if (IR_IS_PURE(instr.tag)) {
    u32 cached;
    if (u64_to_u32_get(&gen->ircache, instr.u64_repr, &cached)) {
      return cached;
    }
  }

  if (gen->numneg == gen->maxneg) {
    u32 maxneg = gen->maxneg * 2;
    Instr *code = (Instr *)malloc(sizeof(Instr) * (maxneg + gen->maxpos)) + maxneg;
    memcpy(code - gen->numneg, gen->code - gen->numneg, sizeof(Instr) * (gen->numneg + gen->numpos));
    free(gen->code - gen->maxneg);
    gen->code = code;
    gen->maxneg = maxneg;
  }

  i32 id = -(++gen->numneg);
  assert(id > INT16_MIN);
  gen->code[id] = instr;
  if (IR_IS_PURE(instr.tag)) {
    u64_to_u32_put(&gen->ircache, instr.u64_repr, id);
  }
  return id;
}

static InstrId append_block_instr(IrGen *gen, BlockId block, Instr instr) {
  if (block == gen->curr_block) {
    assert(!IR_IS_PURE(instr.tag));
    return emit_instr_pinned(gen, instr);
  }
  InstrId id = emit_instr_unpinned(gen, instr);
  Block *b = get_block(gen, block);
  list_push(id, &b->suffix, &gen->list_entries);
  return id;
}



static Instr to_instr(IrGen *gen, InstrId id) {
  Instr instr = gen->code[id];
  switch (instr.tag) {
    case IR_CONST:
    case IR_IDENTITY: return instr;
    default: return (Instr) { .tag = IR_IDENTITY, .type = instr.type, .lhs = id };
  }
}

static InstrId intern_instr(IrGen *gen, Instr instr) {
  if (instr.tag == IR_IDENTITY) {
    return instr.lhs;
  }
  assert(IR_IS_PURE(instr.tag));
  return emit_instr_unpinned(gen, instr);
}



void irgen_label(IrGen *gen, BlockId block) {
  if (gen->curr_block) {
    assert(get_block(gen, gen->curr_block)->is_filled);
  }
  InstrId id = emit_instr_pinned(gen, (Instr) { .tag = IR_LABEL, .rhs = block });
  Block *b = get_block(gen, block);
  b->start = id;
  gen->curr_block = block;
}

void irgen_jump(IrGen *gen, BlockId block, BlockId target) {
  Block *b = get_block(gen, block);
  Block *tb = get_block(gen, target);
  assert(!b->succs[0]);
  assert(!tb->is_sealed);
  b->succs[0] = target;
  b->is_filled = true;
  list_push(gen->curr_block, &tb->preds, &gen->list_entries);
  append_block_instr(gen, block, (Instr) { .tag = IR_JUMP, .rhs = target });
}

void irgen_branch(IrGen *gen, BlockId block, Instr cond, BlockId true_target, BlockId false_target) {
  assert(cond.type == TY_BOOL);
  if (cond.tag == IR_CONST) {
    irgen_jump(gen, block, cond.bool_const ? true_target : false_target);
    return;
  }
  Block *b = get_block(gen, block);
  Block *tb = get_block(gen, true_target);
  Block *fb = get_block(gen, false_target);
  assert(!b->succs[0]);
  assert(!b->succs[1]);
  assert(!tb->is_sealed);
  assert(!fb->is_sealed);
  b->succs[0] = true_target;
  b->succs[1] = false_target;
  b->is_filled = true;
  list_push(gen->curr_block, &tb->preds, &gen->list_entries);
  list_push(gen->curr_block, &fb->preds, &gen->list_entries);
  append_block_instr(gen, block, (Instr) { .tag = IR_BRANCH, .extra = intern_instr(gen, cond), .lhs = true_target, .rhs = false_target });
}

void irgen_ret(IrGen *gen, BlockId block, Instr retval) {
  Block *b = get_block(gen, block);
  b->is_filled = true;
  append_block_instr(gen, block, (Instr) { .tag = IR_RET, .lhs = intern_instr(gen, retval) });
}

void irgen_upsilon(IrGen *gen, BlockId block, Instr val, Instr phi) {
  if (phi.type != TY_VOID) {
    InstrId id = append_block_instr(gen, block, (Instr) { .tag = IR_UPSILON, .lhs = intern_instr(gen, val), .rhs = intern_instr(gen, phi) });
    vector_push(gen->upsilons, id);
  }
}

Instr irgen_const_bool(IrGen *gen, bool val) {
  return (Instr) { .tag = IR_CONST, .type = TY_BOOL, .bool_const = val };
}

Instr irgen_const_i32(IrGen *gen, i32 val) {
  return (Instr) { .tag = IR_CONST, .type = TY_I32, .i32_const = val };
}

Instr irgen_phi(IrGen *gen, TypeId type) {
  return (Instr) { .tag = IR_PHI, .type = type, .rhs = ++gen->phi_count };
}

Instr irgen_arg(IrGen *gen, u32 arg, TypeId type) {
  return (Instr) { .tag = IR_ARG, .type = type, .rhs = arg };
}

Instr irgen_add(IrGen *gen, Instr lhs, Instr rhs) {
  assert(lhs.type == rhs.type);
  if (lhs.tag == IR_CONST && rhs.tag == IR_CONST) {
    switch (lhs.type) {
      case TY_I32: return (Instr) { .tag = IR_CONST, .type = lhs.type, .i32_const = lhs.i32_const + rhs.i32_const };
      default: assert(0 && "unexpected type");
    }
  }
  return (Instr) { .tag = IR_ADD, .type = lhs.type, .lhs = intern_instr(gen, lhs), .rhs = intern_instr(gen, rhs) };
}

Instr irgen_eq(IrGen *gen, Instr lhs, Instr rhs) {
  assert(lhs.type == rhs.type);
  if (lhs.tag == IR_CONST && rhs.tag == IR_CONST) {
    switch (lhs.type) {
      case TY_BOOL: return (Instr) { .tag = IR_CONST, .type = TY_BOOL, .bool_const = lhs.bool_const == rhs.bool_const };
      case TY_I32: return (Instr) { .tag = IR_CONST, .type = TY_BOOL, .bool_const = lhs.i32_const == rhs.i32_const };
      default: assert(0 && "unexpected type");
    }
  }
  return (Instr) { .tag = IR_EQ, .type = TY_BOOL, .lhs = intern_instr(gen, lhs), .rhs = intern_instr(gen, rhs) };
}





typedef union {
  struct { i16 block, var; };
  i32 u32_repr;
} BindingKey;

static InstrId create_pred_upsilons(IrGen *gen, BlockId block, InstrId phi);

void irgen_write_variable(IrGen *gen, BlockId block, VarId var, Instr val) {
  BindingKey key = {{ block, var }};
  u32_to_u64_put(&gen->bindings, key.u32_repr, val.u64_repr);
}

Instr irgen_read_variable(IrGen *gen, BlockId block, VarId var) {
  BindingKey key = {{ block, var }};
  Instr found;
  if (u32_to_u64_get(&gen->bindings, key.u32_repr, &found.u64_repr)) {
    return found;
  }

  Instr result;
  Block *b = get_block(gen, block);
  if (!b->is_sealed) {
    Instr phi = irgen_phi(gen, gen->vars[var].type);
    list_push(intern_instr(gen, phi), &b->incomplete_phis, &gen->list_entries);
    result = phi;
  } else if (b->preds.values[0] && !b->preds.values[1]) {
    // exactly one predecessor
    result = irgen_read_variable(gen, b->preds.values[0], var);
  } else {
    Instr phi = irgen_phi(gen, gen->vars[var].type);
    irgen_write_variable(gen, block, var, phi);
    result = to_instr(gen, create_pred_upsilons(gen, block, intern_instr(gen, phi)));
  }

  irgen_write_variable(gen, block, var, result);
  return result;
}

static InstrId try_remove_trivial_phi(IrGen *gen, InstrId phi) {
  InstrId phi_users[128];
  int phi_users_count = 0;

  InstrId same = 0, id;
  vector_foreach(gen->upsilons, id) {
    Instr instr = gen->code[id];
    if (instr.rhs == phi) {
      // this is an upsilon which writes the shadow variable for this phi (so it is in effect an operand)
      InstrId op = instr.lhs;
      if (op == same || op == phi) {
        continue;
      }
      if (same) {
        return phi;
      }
      same = op;
    } else if (instr.lhs == phi) {
      // this upsilon defines an operand to a different phi, but the value written is this phi.
      // record this phi user, so we can try to recursively remove trivial phis.
      assert(phi_users_count < 128);
      phi_users[phi_users_count++] = instr.rhs;
    }
  }

  // the phi was trivial. replace with the referenced value
  gen->code[phi] = (Instr) { .tag = IR_IDENTITY, .lhs = same };

  for (int i = 0; i < phi_users_count; ++i) {
    try_remove_trivial_phi(gen, phi_users[i]);
  }
}

static InstrId create_pred_upsilons(IrGen *gen, BlockId block, InstrId phi) {
  VarId var = gen->code[phi].rhs;
  Block *b = get_block(gen, block);
  for (ListEntry *e = &b->preds; ; e = gen->list_entries + e->next) {
    for (int i = 0; i < 3; ++i) {
      BlockId pred = e->values[i];
      if (!pred) goto end;
      Instr val = irgen_read_variable(gen, pred, var);
      irgen_upsilon(gen, pred, val, to_instr(gen, phi));
    }
    if (!e->next) break;
  }

end:
  return phi;
  //return try_remove_trivial_phi(gen, phi);
}

void irgen_seal(IrGen *gen, BlockId block) {
  Block *b = get_block(gen, block);
  assert(!b->is_sealed);
  for (ListEntry *e = &b->incomplete_phis; ; e = gen->list_entries + e->next) {
    for (int i = 0; i < 3; ++i) {
      InstrId phi = e->values[i];
      if (!phi) goto end;
      create_pred_upsilons(gen, block, phi);
    }
    if (!e->next) break;
  }

end:
  b->is_sealed = true;
}




static InstrId fixup_instr(IrGen *gen, IrGen *source, i16 *map, InstrId source_id) {
  printf("fixup: "); print_instr(source, source_id, source->code[source_id]);
  if (map[source_id]) {
    return map[source_id];
  }

#define FIXUP(Id) gen->code[fixup_instr(gen, source, map, Id)]
#define FIXUP_BINOP(EmitFunc) intern_instr(gen, EmitFunc(gen, FIXUP(instr.lhs), FIXUP(instr.rhs)))

  InstrId dest_id = 0;
  Instr instr = source->code[source_id];
  switch (instr.tag) {
    case IR_NOP: return 0;
    case IR_IDENTITY: dest_id = fixup_instr(gen, source, map, instr.lhs); break;
    case IR_LABEL: irgen_label(gen, instr.rhs); return 0;
    case IR_JUMP: irgen_jump(gen, gen->curr_block, instr.rhs); return 0;
    case IR_BRANCH: irgen_branch(gen, gen->curr_block, FIXUP(instr.extra), instr.lhs, instr.rhs); return 0;
    case IR_RET: irgen_ret(gen, gen->curr_block, FIXUP(instr.lhs)); return 0;
    case IR_UPSILON: irgen_upsilon(gen, gen->curr_block, FIXUP(instr.lhs), FIXUP(instr.rhs)); return 0;
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

static void fixup_block(IrGen *gen, IrGen *source, i16 *map, BlockId block) {
  Block *b = get_block(source, block);
  if (b->is_visited) {
    return;
  }
  b->is_visited = true;
  assert(b->is_sealed);
  assert(b->is_filled);

  InstrId source_id = b->start;
  while (!IR_IS_TERMINAL(source->code[source_id].tag)) {
    assert(source_id < source->maxpos);
    fixup_instr(gen, source, map, source_id++);
  }

  for (ListEntry *e = &b->suffix; ; e = source->list_entries + e->next) {
    for (int i = 0; i < 3; ++i) {
      InstrId id = e->values[i];
      if (!id) goto suffix_done;
      fixup_instr(gen, source, map, id);
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

void irgen_fixup(IrGen *gen, IrGen *source) {
  i16 map_buffer[65536] = { 0, };
  i16 *map = map_buffer + 32768; // map from old to new InstrId
  gen->phi_count = gen->phi_count;
  fixup_block(gen, source, map, 1);
}




int main(int argc, char *argv[]) {
  IrGen *gen = irgen_create();
  VarId x = irgen_create_variable(gen, TY_I32);
  BlockId entry_block = irgen_create_block(gen);
  BlockId then_block = irgen_create_block(gen);
  BlockId else_block = irgen_create_block(gen);
  BlockId exit_block = irgen_create_block(gen);

  irgen_seal(gen, entry_block);
  irgen_label(gen, entry_block);
  irgen_write_variable(gen, entry_block, x, irgen_const_i32(gen, 1));
  irgen_branch(gen, entry_block, irgen_arg(gen, 0, TY_BOOL), then_block, else_block);
  irgen_seal(gen, then_block);
  irgen_seal(gen, else_block);

  irgen_label(gen, then_block);
  irgen_write_variable(gen, then_block, x, irgen_add(gen, irgen_read_variable(gen, then_block, x), irgen_add(gen, irgen_const_i32(gen, 5), irgen_const_i32(gen, 5))));
  irgen_jump(gen, then_block, exit_block);
  
  irgen_label(gen, else_block);
  irgen_write_variable(gen, else_block, x, irgen_add(gen, irgen_read_variable(gen, else_block, x), irgen_const_i32(gen, 10)));
  irgen_jump(gen, else_block, exit_block);

  irgen_seal(gen, exit_block);
  irgen_label(gen, exit_block);
  irgen_ret(gen, exit_block, irgen_read_variable(gen, exit_block, x));

  printf("before:\n");
  print_ir(gen);

  IrGen *fixed = irgen_create();
  irgen_fixup(fixed, gen);
  
  printf("\nafter:\n");
  print_ir(fixed);

  irgen_destroy(gen);
  irgen_destroy(fixed);

  return 0;
}
