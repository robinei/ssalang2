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



typedef struct Variable {
  TypeId type;
} Variable;


typedef struct Block {
  u32 is_sealed : 1;
  u32 is_visited : 1;
  u32 start : 30;
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
    case IR_JFALSE: printf("  JFALSE %d :%d\n", instr.lhs, instr.rhs); break;
    case IR_RET: printf("  RET %d\n", instr.lhs); break;
    case IR_UPSILON: printf("  UPSILON %d %d\n", instr.lhs, instr.rhs); break;
    case IR_PHI: printf("  %d = PHI\n", id); break; 
    case IR_CONST:
      switch (instr.type) {
        case TY_VOID: printf("  %d = CONST void\n", id); break;
        case TY_BOOL: printf("  %d = CONST %s\n", id, instr.bool_const ? "true" : "false"); break;
        case TY_I32: printf("  %d = CONST %d\n", id, instr.i32_const); break;
        default: assert(0); break;
      }
      break;
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



BlockId create_block(IrGen *gen) {
  BlockId id = vector_size(gen->blocks);
  assert(id < INT16_MAX);
  vector_push(gen->blocks, (Block) { });
  return id;
}

static Block *get_block(IrGen *gen, BlockId block) {
  assert(block);
  while (vector_size(gen->blocks) <= block) {
    create_block(gen);
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

IrGen *create_func() {
  IrGen *gen = calloc(1, sizeof(IrGen));
  init_code_array(gen);
  create_block(gen); // block at index 0 should not be used
  return gen;
}

VarId create_variable(IrGen *gen, TypeId type) {
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



void emit_label(IrGen *gen, BlockId block) {
  InstrId id = emit_instr_pinned(gen, (Instr) { .tag = IR_LABEL, .rhs = block });
  Block *b = get_block(gen, block);
  b->start = id;
  gen->curr_block = block;
}

void emit_jump(IrGen *gen, BlockId block, BlockId target) {
  Block *b = get_block(gen, block);
  if (b->succs[0] ) {
    // already has IR_JUMP instr
    return;
  }
  Block *tb = get_block(gen, target);
  assert(!b->succs[0]);
  assert(!tb->is_sealed);
  b->succs[0] = target;
  list_push(gen->curr_block, &tb->preds, &gen->list_entries);
  append_block_instr(gen, block, (Instr) { .tag = IR_JUMP, .rhs = target });
}

void emit_jfalse(IrGen *gen, BlockId block, BlockId false_target, Instr cond) {
  Block *b = get_block(gen, block);
  Block *tb = get_block(gen, false_target);
  assert(cond.type == TY_BOOL);
  assert(!b->succs[0]);
  assert(!b->succs[1]);
  assert(!tb->is_sealed);
  b->succs[1] = false_target;
  list_push(gen->curr_block, &tb->preds, &gen->list_entries);
  append_block_instr(gen, block, (Instr) { .tag = IR_JFALSE, .lhs = intern_instr(gen, cond), .rhs = false_target });
}

void emit_ret(IrGen *gen, BlockId block, Instr retval) {
  append_block_instr(gen, block, (Instr) { .tag = IR_RET, .lhs = intern_instr(gen, retval) });
}

void emit_upsilon(IrGen *gen, BlockId block, Instr val, Instr phi) {
  if (phi.type != TY_VOID) {
    InstrId id = append_block_instr(gen, block, (Instr) { .tag = IR_UPSILON, .lhs = intern_instr(gen, val), .rhs = intern_instr(gen, phi) });
    vector_push(gen->upsilons, id);
  }
}

Instr emit_bool(IrGen *gen, bool val) {
  return (Instr) { .tag = IR_CONST, .type = TY_BOOL, .bool_const = val };
}

Instr emit_i32(IrGen *gen, i32 val) {
  return (Instr) { .tag = IR_CONST, .type = TY_I32, .i32_const = val };
}

Instr emit_phi(IrGen *gen, TypeId type) {
  return (Instr) { .tag = IR_PHI, .type = type, .rhs = ++gen->phi_count };
}

Instr emit_arg(IrGen *gen, u32 arg, TypeId type) {
  return (Instr) { .tag = IR_ARG, .type = type, .rhs = arg };
}

Instr emit_add(IrGen *gen, Instr lhs, Instr rhs) {
  assert(lhs.type == rhs.type);
  if (lhs.tag == IR_CONST && rhs.tag == IR_CONST) {
    switch (lhs.type) {
      case TY_I32: return (Instr) { .tag = IR_CONST, .type = lhs.type, .i32_const = lhs.i32_const + rhs.i32_const };
      default: assert(0 && "unexpected type");
    }
  }
  return (Instr) { .tag = IR_ADD, .type = lhs.type, .lhs = intern_instr(gen, lhs), .rhs = intern_instr(gen, rhs) };
}

Instr emit_eq(IrGen *gen, Instr lhs, Instr rhs) {
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

void write_variable(IrGen *gen, BlockId block, VarId var, Instr val) {
  BindingKey key = {{ block, var }};
  u32_to_u64_put(&gen->bindings, key.u32_repr, val.u64_repr);
}

Instr read_variable(IrGen *gen, BlockId block, VarId var) {
  BindingKey key = {{ block, var }};
  Instr found;
  if (u32_to_u64_get(&gen->bindings, key.u32_repr, &found.u64_repr)) {
    return found;
  }

  Instr result;
  Block *b = get_block(gen, block);
  if (!b->is_sealed) {
    Instr phi = emit_phi(gen, gen->vars[var].type);
    list_push(intern_instr(gen, phi), &b->incomplete_phis, &gen->list_entries);
    result = phi;
  } else if (b->preds.values[0] && !b->preds.values[1]) {
    // exactly one predecessor
    result = read_variable(gen, b->preds.values[0], var);
  } else {
    Instr phi = emit_phi(gen, gen->vars[var].type);
    write_variable(gen, block, var, phi);
    result = to_instr(gen, create_pred_upsilons(gen, block, intern_instr(gen, phi)));
  }

  write_variable(gen, block, var, result);
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
      Instr val = read_variable(gen, pred, var);
      emit_upsilon(gen, pred, val, to_instr(gen, phi));
    }
    if (!e->next) break;
  }

end:
  return phi;
  //return try_remove_trivial_phi(gen, phi);
}

void seal_block(IrGen *gen, BlockId block) {
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




static InstrId reemit_dependency(IrGen *gen, Instr *code, i16 *map, InstrId id);

static Instr reemit_dependencies(IrGen *gen, Instr *code, i16 *map, Instr instr) {
  switch (instr.tag) {
    case IR_JFALSE:
    case IR_RET:
      instr.lhs = reemit_dependency(gen, code, map, instr.lhs);
      break;
    case IR_UPSILON:
    case IR_ADD:
    case IR_EQ:
      instr.lhs = reemit_dependency(gen, code, map, instr.lhs);
      instr.rhs = reemit_dependency(gen, code, map, instr.rhs);
      break;
  }
  return instr;
}

static InstrId reemit_dependency(IrGen *gen, Instr *code, i16 *map, InstrId id) {
  if (map[id]) {
    return map[id];
  }
  InstrId src = id;
  //while (code[src].tag == IR_IDENTITY) {
  //  src = code[src].lhs;
  //}
  assert(IR_IS_PURE(code[id].tag));
  return map[id] = emit_instr_unpinned(gen, reemit_dependencies(gen, code, map, code[src]));
}

static void reemit_block(IrGen *gen, BlockId block, Vector(Block) blocks, Vector(ListEntry) list_entries, Instr *code, i16 *map) {
  Block *b = blocks + block;
  if (b->is_visited) {
    return;
  }
  b->is_visited = true;

  for (InstrId id = b->start; ; ++id) {
    map[id] = emit_instr_pinned(gen, reemit_dependencies(gen, code, map, code[id]));
    if (IR_IS_TERMINAL(code[id].tag)) {
      break;
    }
  }

  for (ListEntry *e = &b->suffix; ; e = list_entries + e->next) {
    for (int i = 0; i < 3; ++i) {
      InstrId id = e->values[i];
      if (!id) goto suffix_done;
      map[id] = emit_instr_pinned(gen, reemit_dependencies(gen, code, map, code[id]));
    }
    if (!e->next) break;
  }
suffix_done:

  for (int i = 0; i < 2; ++i) {
    if (b->succs[i]) {
      reemit_block(gen, b->succs[i], blocks, list_entries, code, map);
    }
  }
}

static void reemit_all(IrGen *gen) {
  i16 map_buffer[65536] = { 0, };
  i16 *map = map_buffer + 32768; // map from old to new InstrId

  Instr *code = gen->code;
  assert(code);
  Instr *code_base_ptr = code - gen->maxneg;
  int end_id = gen->numpos;
  init_code_array(gen);

  Vector(ListEntry) list_entries = vector_clone(gen->list_entries);
  gen->list_entries = NULL;

  Vector(Block) blocks = vector_clone(gen->blocks);
  gen->blocks = NULL;
  gen->curr_block = 0;

  vector_free(gen->vars);
  gen->vars = NULL;

  vector_clear(gen->upsilons);
  u64_to_u32_clear(&gen->ircache);
  u32_to_u64_clear(&gen->bindings);

  reemit_block(gen, 1, blocks, list_entries, code, map);

  /*for (InstrId id = 1; id < gen->numpos; ++id) {
    gen->code[id] = reemit_dependencies(gen, code, map, gen->code[id]);
  }*/

  InstrId id;
  vector_foreach(gen->upsilons, id) {
    InstrId phi = map[gen->code[id].rhs];
    assert(gen->code[id].tag == IR_UPSILON);
    assert(phi);
    gen->code[id].rhs = phi;
  }

  vector_free(list_entries);
  vector_free(blocks);
  free(code_base_ptr);
}





int main(int argc, char *argv[]) {
  IrGen *gen = create_func();
  VarId x = create_variable(gen, TY_I32);
  BlockId entry_block = create_block(gen);
  BlockId then_block = create_block(gen);
  BlockId else_block = create_block(gen);
  BlockId exit_block = create_block(gen);

  seal_block(gen, entry_block);
  emit_label(gen, entry_block);
  write_variable(gen, entry_block, x, emit_i32(gen, 1));
  emit_jfalse(gen, entry_block, else_block, emit_bool(gen, true));
  emit_jump(gen, entry_block, then_block);
  
  seal_block(gen, then_block);
  emit_label(gen, then_block);
  write_variable(gen, then_block, x, emit_add(gen, read_variable(gen, then_block, x), emit_add(gen, emit_i32(gen, 5), emit_i32(gen, 5))));
  emit_jump(gen, then_block, exit_block);
  
  seal_block(gen, else_block);
  emit_label(gen, else_block);
  write_variable(gen, else_block, x, emit_add(gen, read_variable(gen, else_block, x), emit_i32(gen, 10)));
  emit_jump(gen, else_block, exit_block);

  seal_block(gen, exit_block);
  emit_label(gen, exit_block);
  emit_ret(gen, exit_block, read_variable(gen, exit_block, x));

  /*printf("before:\n");
  print_ir(gen);

  printf("\nreemit:\n");
  reemit_all(gen);
  
  printf("\nafter:\n");*/
  print_ir(gen);

  return 0;
}
