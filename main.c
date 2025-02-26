#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "vector.h"
#include "defs.h"

typedef i32 InstrId; // signed, so that pure instructions can have negative id (that way we don't commit to any sequencing of them early)
typedef u32 BlockId;
typedef u32 TypeId;
typedef u32 VarId;

#define NAME u32_to_u32
#define KEY_TYPE u32
#define VALUE_TYPE u32
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


#define LIST_ENTRY_SIZE 3

typedef struct ListEntry {
  i16 values[LIST_ENTRY_SIZE];
  i16 next;
} ListEntry;

static void list_push(i16 value, ListEntry *e, ListEntry **entries) {
start:
  for (int i = 0; i < LIST_ENTRY_SIZE; ++i) {
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
  for (int i = 0; i < LIST_ENTRY_SIZE; ++i) {
    e->values[i] = 0;
  }
  if (e->next) {
    e = *entries + e->next;
    goto start;
  }
}


typedef struct AddedInstr {
  i16 where, id;
} AddedInstr;

#define NAME sort_added_instrs
#define TYPE AddedInstr
#define COMPARE(a, b) a.where - b.where
#include "mergesort.h"





typedef enum InstrTag {
  IR_NOP,
  IR_IDENTITY, // replace with lhs value
  
  IR_LABEL, // identify the start of BlockId rhs
  IR_JUMP, // unconditional jump to BlockId rhs
  IR_JFALSE, // jump to BlockId rhs if the condition value lhs is false
  IR_RET, // return value lhs

  IR_UPSILON, // assign lhs value to rhs phi
  IR_PHI, // defines a new phi value for rhs var

  // let everything from CONST and after be pure instructions (CONST must be first)
  IR_CONST,
  IR_ADD,
  IR_EQ,
} InstrTag;

typedef enum TypeKind {
  TK_VOID,
  TK_BOOL,
  TK_INT,
  TK_UINT,
  TK_REAL,
} TypeKind;

// predefined type identifiers (indexes into the types array)
enum {
  TY_VOID,
  TY_BOOL,
  TY_I32,

  TY_MAX
};

typedef struct Type {
  TypeKind kind;
  u32 size;
} Type;

typedef struct Variable {
  const char *name;
} Variable;

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

typedef struct Block {
  u32 is_sealed : 1;
  u32 start : 15;
  u32 end : 16;
  u16 succs[2];
  ListEntry preds;
  ListEntry incomplete_phis;
} Block;

typedef struct Func {
  Instr *code;
  i32 numneg, maxneg, numpos, maxpos;

  Vector(AddedInstr) added;
  Vector(Block) blocks;
  Vector(Variable) vars;
  Vector(ListEntry) list_entries;
  u32_to_u32 bindings;
  u64_to_u32 ircache;
} Func;



static Type types[TY_MAX] = {
  { TK_VOID, 0 },
  { TK_BOOL, sizeof(bool) },
  { TK_INT, sizeof(i32) },
};

static InstrId push_instr_pinned(Func *f, Instr instr) {
  assert(instr.tag < IR_CONST);
  if (f->numpos == f->maxpos) {
    u32 maxpos = f->maxpos * 2;
    Instr *code = (Instr *)malloc(sizeof(Instr) * (f->maxneg + maxpos)) + f->maxneg;
    memcpy(code - f->numneg, f->code - f->numneg, sizeof(Instr) * (f->numneg + f->numpos));
    free(f->code - f->maxneg);
    f->code = code;
    f->maxpos = maxpos;
  }
  InstrId id = f->numpos++;
  assert(id < INT16_MAX);
  f->code[id] = instr;
  return id;
}

static InstrId push_instr_unpinned(Func *f, Instr instr) {
  if (instr.tag >= IR_CONST) {
    u32 cached;
    if (u64_to_u32_get(&f->ircache, instr.u64_repr, &cached)) {
      return cached;
    }
  }

  if (f->numneg == f->maxneg) {
    u32 maxneg = f->maxneg * 2;
    Instr *code = (Instr *)malloc(sizeof(Instr) * (maxneg + f->maxpos)) + maxneg;
    memcpy(code - f->numneg, f->code - f->numneg, sizeof(Instr) * (f->numneg + f->numpos));
    free(f->code - f->maxneg);
    f->code = code;
    f->maxneg = maxneg;
  }

  InstrId id = -(++f->numneg);
  assert(id > INT16_MIN);
  if (instr.tag >= IR_CONST) {
    u64_to_u32_put(&f->ircache, instr.u64_repr, id);
  }
  f->code[id] = instr;
  return id;
}

static void init_code_array(Func *f) {
  f->numneg = f->numpos = 0;
  f->maxneg = f->maxpos = 16;
  f->code = (Instr *)malloc(sizeof(Instr) * (f->maxneg + f->maxpos)) + f->maxneg;
  push_instr_pinned(f, (Instr) { .tag = IR_NOP, .type = TY_VOID, }); // let instr at index 0 be NOP
}

InstrId push_instr(Func *f, Instr instr) {
  if (instr.tag >= IR_CONST) {
    // allocate pure instructions at negative indices (so they don't have explicit ordering initially)
    return push_instr_unpinned(f, instr);
  } else {
    // impure instructions are laid out normally with positive indices and significant sequencing
    return push_instr_pinned(f, instr);
  }
}

InstrId insert_block_instr(Func *f, BlockId block, Instr instr, InstrId where) {
  if (where == f->numpos) {
    ++f->blocks[block].end;
    return push_instr_pinned(f, instr);
  }
  InstrId id = push_instr_unpinned(f, instr);
  vector_push(f->added, ((AddedInstr) { .where = where, .id = id }));
  return id;
}

InstrId append_block_instr(Func *f, BlockId block, Instr instr) {
  Block *b = f->blocks + block;
  InstrId where = b->end;
  if (b->succs[0]) {
    --where; // insert before terminating JUMP
    if (b->succs[1]) {
      --where; // insert before terminating JFALSE
    }
  }
  return insert_block_instr(f, block, instr, where);
}

InstrId prepend_block_instr(Func *f, BlockId block, Instr instr) {
  InstrId where = f->blocks[block].start + 1; // insert after LABEL
  return insert_block_instr(f, block, instr, where);
}

InstrId emit_bool(Func *f, bool val) {
  return push_instr_unpinned(f, (Instr) { .tag = IR_CONST, .type = TY_BOOL, .bool_const = val });
}

InstrId emit_i32(Func *f, i32 val) {
  return push_instr_unpinned(f, (Instr) { .tag = IR_CONST, .type = TY_I32, .i32_const = val });
}

InstrId emit_add(Func *f, InstrId lhs, InstrId rhs) {
  assert(f->code[lhs].type == f->code[rhs].type);
  return push_instr_unpinned(f, (Instr) { .tag = IR_ADD, .type = f->code[lhs].type, .lhs = lhs, .rhs = rhs });
}

void emit_label(Func *f, BlockId block) {
  f->blocks[block].start = push_instr_pinned(f, (Instr) { .tag = IR_LABEL, .rhs = block });
  f->blocks[block].end = f->blocks[block].start + 1;
}

void emit_ret(Func *f, BlockId block, InstrId retval) {
  Block *b = f->blocks + block;
  append_block_instr(f, block, (Instr) { .tag = IR_RET, .lhs = retval });
}

void emit_jump(Func *f, BlockId block, BlockId target) {
  Block *b = f->blocks + block;
  Block *tb = f->blocks + target;
  assert(!b->succs[0]);
  assert(!tb->is_sealed);
  append_block_instr(f, block, (Instr) { .tag = IR_JUMP, .rhs = target });
  b->succs[0] = target;
  list_push(block, &tb->preds, &f->list_entries);
}

void emit_jfalse(Func *f, BlockId block, BlockId false_target, InstrId cond) {
  Block *b = f->blocks + block;
  Block *tb = f->blocks + false_target;
  assert(!b->succs[0]);
  assert(!b->succs[1]);
  assert(!tb->is_sealed);
  append_block_instr(f, block, (Instr) { .tag = IR_JFALSE, .lhs = cond, .rhs = false_target });
  b->succs[1] = false_target;
  list_push(block, &f->blocks[false_target].preds, &f->list_entries);
}

void emit_upsilon(Func *f, BlockId block, InstrId val, InstrId phi) {
  append_block_instr(f, block, (Instr) { .tag = IR_UPSILON, .lhs = val, .rhs = phi });
}

InstrId emit_phi(Func *f, BlockId block, VarId var) {
  Block *b = f->blocks + block;
  if (b->is_sealed && b->preds.values[0] && b->preds.values[1] && !b->preds.values[2]) {
    Block *branch0 = f->blocks + b->preds.values[0];
    Block *branch1 = f->blocks + b->preds.values[1];
    if (branch0->is_sealed && branch1->is_sealed && branch0->preds.values[0] && branch0->preds.values[0] == branch1->preds.values[0] && !branch0->preds.values[1] && !branch1->preds.values[1]) {
      Block *entry = f->blocks + branch0->preds.values[0];
      Block *then, *els;
      if (entry->succs[0] == b->preds.values[0]) {
        assert(entry->succs[1] == b->preds.values[1]);
        then = branch0;
        els = branch1;
      } else {
        assert(entry->succs[0] == b->preds.values[1]);
        assert(entry->succs[1] == b->preds.values[0]);
        then = branch1;
        els = branch0;
      }
      InstrId then_val = 0;
      InstrId else_val = 0;
      if (f->code[then->start + 1].tag == IR_UPSILON && f->code[then->start + 2].tag == IR_JUMP)
      {
        then_val = f->code[then->start + 1].lhs;
      }
    }
  }
  return prepend_block_instr(f, block, (Instr) { .tag = IR_PHI, .rhs = var });
}





BlockId create_block(Func *f) {
  BlockId id = vector_size(f->blocks);
  assert(id < INT16_MAX);
  vector_push(f->blocks, (Block) { });
  return id;
}

Func *create_func() {
  Func *f = calloc(1, sizeof(Func));
  init_code_array(f);
  create_block(f); // block at index 0 should not be used
  return f;
}

VarId create_variable(Func *f, const char *name) {
  VarId id = vector_size(f->vars);
  assert(id < INT16_MAX);
  vector_push(f->vars, (Variable) { .name = name });
  return id;
}

typedef union {
  struct { i16 block, var; };
  i32 i32_value;
} BindingKey;

void write_variable(Func *f, BlockId block, VarId var, InstrId val) {
  BindingKey key = {{ block, var }};
  u32_to_u32_put(&f->bindings, key.i32_value, val);
}

InstrId read_variable(Func *f, BlockId block, VarId var);

static InstrId try_remove_trivial_phi(Func *f, InstrId phi) {
  InstrId phi_users[128];
  int phi_users_count = 0;

  InstrId same = 0;
  for (InstrId id = -f->numneg; id < f->numpos; ++id) {
    Instr instr = f->code[id];
    if (instr.tag == IR_UPSILON) {
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
        phi_users[phi_users_count++] == instr.rhs;
      }
    }
  }

  // the phi was trivial. replace with the referenced value
  f->code[phi] = (Instr) { .tag = IR_IDENTITY, .lhs = same };

  for (int i = 0; i < phi_users_count; ++i) {
    try_remove_trivial_phi(f, phi_users[i]);
  }
}

static InstrId create_upsilons_in_predecessors(Func *f, BlockId block, InstrId phi) {
  VarId var = f->code[phi].rhs;
  Block *b = f->blocks + block;
  for (ListEntry *e = &b->preds; ; e = f->list_entries + e->next) {
    for (int i = 0; i < LIST_ENTRY_SIZE; ++i) {
      BlockId pred = e->values[i];
      if (!pred) goto end;
      InstrId val = read_variable(f, pred, var);
      emit_upsilon(f, pred, val, phi);
    }
    if (!e->next) break;
  }

end:
  return phi;
  //return try_remove_trivial_phi(f, phi);
}

InstrId read_variable(Func *f, BlockId block, VarId var) {
  BindingKey key = {{ block, var }};
  InstrId val;
  if (u32_to_u32_get(&f->bindings, key.i32_value, &val)) {
    return val;
  }

  Block *b = f->blocks + block;
  if (!b->is_sealed) {
    val = emit_phi(f, block, var);
    list_push(val, &b->incomplete_phis, &f->list_entries);
  } else if (b->preds.values[0] && !b->preds.values[1]) {
    // exactly one predecessor
    val = read_variable(f, b->preds.values[0], var);
  } else {
    val = emit_phi(f, block, var);
    write_variable(f, block, var, val);
    val = create_upsilons_in_predecessors(f, block, val);
  }

  write_variable(f, block, var, val);
  return val;
}


void seal_block(Func *f, BlockId block) {
  Block *b = f->blocks + block;
  assert(!b->is_sealed);
  for (ListEntry *e = &b->incomplete_phis; ; e = f->list_entries + e->next) {
    for (int i = 0; i < LIST_ENTRY_SIZE; ++i) {
      BlockId phi = e->values[i];
      if (!phi) goto end;
      create_upsilons_in_predecessors(f, block, phi);
    }
    if (!e->next) break;
  }

end:
  b->is_sealed = true;
}




static InstrId reemit_instr(Func *f, Instr *code, i16 *map, InstrId id);

static Instr reemit_dependencies(Func *f, Instr *code, i16 *map, Instr instr) {
  switch (instr.tag) {
    case IR_JFALSE:
    case IR_RET:
      instr.lhs = reemit_instr(f, code, map, instr.lhs);
      break;
    case IR_UPSILON:
      instr.lhs = reemit_instr(f, code, map, instr.lhs);
      instr.rhs = map[instr.rhs];
      assert(instr.rhs);
      break;
    case IR_ADD:
    case IR_EQ:
      instr.lhs = reemit_instr(f, code, map, instr.lhs);
      instr.rhs = reemit_instr(f, code, map, instr.rhs);
      break;
  }
  return instr;
}

static InstrId reemit_instr(Func *f, Instr *code, i16 *map, InstrId id) {
  if (map[id]) {
    return map[id];
  }
  InstrId src = id;
  //while (code[src].tag == IR_IDENTITY) {
  //  src = code[src].lhs;
  //}
  return map[id] = push_instr(f, reemit_dependencies(f, code, map, code[src]));
}

static void reemit_all(Func *f) {
  i16 map_buffer[65536] = { 0, };
  i16 *map = map_buffer + 32768; // map from old to new InstrId
  
  AddedInstr *added = f->added;
  AddedInstr *toadd = added;
  AddedInstr *added_end = vector_end(added);
  f->added = NULL;
  AddedInstr *temp_added = malloc(sizeof(AddedInstr) * vector_size(added));
  sort_added_instrs(added, 0, vector_size(added) - 1, temp_added);
  free(temp_added);

  Instr *code = f->code;
  assert(code);
  Instr *code_base_ptr = code - f->maxneg;
  int end_id = f->numpos;
  init_code_array(f);
  u64_to_u32_clear(&f->ircache);

  for (InstrId id = 1; id <= end_id; ++id) {
    while (toadd && toadd < added_end && toadd->where == id) {
      map[toadd->id] = push_instr_pinned(f, code[toadd->id]);
      ++toadd;
      continue;
    }

    if (id == end_id) {
      break;
    }

    Instr instr = code[id];

    if (instr.tag == IR_NOP) {
      continue;
    }

    if (instr.tag == IR_UPSILON && code[instr.rhs].tag != IR_PHI) {
      continue;
    }

    map[id] = push_instr_pinned(f, instr);
  }

  for (InstrId id = 1; id < f->numpos; ++id) {
    f->code[id] = reemit_dependencies(f, code, map, f->code[id]);
  }

  vector_free(added);
  free(code_base_ptr);
}





static void print_ir(Func *f) {
  for (InstrId id = -f->numneg; id < f->numpos; ++id) {
    Instr instr = f->code[id];
    switch (instr.tag) {
      case IR_NOP: printf("  NOP\n"); break;
      case IR_IDENTITY: printf("  %d = ID %d\n", id, instr.lhs); break;
      case IR_LABEL: printf(":%d\n", instr.rhs); break;
      case IR_JUMP: printf("  JUMP :%d\n", instr.rhs); break;
      case IR_JFALSE: printf("  JFALSE %d :%d\n", instr.lhs, instr.rhs); break;
      case IR_RET: printf("  RET %d\n", instr.lhs); break;
      case IR_UPSILON: printf("  UPSILON %d %d [%s]\n", instr.lhs, instr.rhs, f->vars[f->code[instr.rhs].rhs].name); break;
      case IR_PHI: printf("  %d = PHI [%s]\n", id, f->vars[instr.rhs].name); break; 
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
}




int main(int argc, char *argv[]) {
  Func *f = create_func();
  VarId x = create_variable(f, "x");
  BlockId entry_block = create_block(f);
  BlockId then_block = create_block(f);
  BlockId else_block = create_block(f);
  BlockId exit_block = create_block(f);

  emit_label(f, entry_block);
  seal_block(f, entry_block);
  write_variable(f, entry_block, x, emit_i32(f, 1));
  emit_jfalse(f, entry_block, else_block, emit_bool(f, true));
  emit_jump(f, entry_block, then_block);
  
  emit_label(f, then_block);
  seal_block(f, then_block);
  write_variable(f, then_block, x, emit_add(f, read_variable(f, then_block, x), emit_i32(f, 10)));
  emit_jump(f, then_block, exit_block);
  
  emit_label(f, else_block);
  seal_block(f, else_block);
  write_variable(f, else_block, x, emit_add(f, read_variable(f, else_block, x), emit_i32(f, 10)));
  emit_jump(f, else_block, exit_block);

  emit_label(f, exit_block);
  seal_block(f, exit_block);
  emit_ret(f, exit_block, read_variable(f, exit_block, x));

  reemit_all(f);
  print_ir(f);

  return 0;
}

