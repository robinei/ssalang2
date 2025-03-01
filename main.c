#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "vector.h"
#include "defs.h"

typedef i16 InstrId; // signed, so that pure instructions can have negative id (that way we don't commit to any sequencing of them early)
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
  u32 is_visited : 1;
  u32 start : 15;
  u32 end : 15;
  u16 succs[2];
  ListEntry preds;
  ListEntry incomplete_phis;
  ListEntry prefix;
  ListEntry suffix;
} Block;

typedef struct Func {
  Instr *code;
  i32 numneg, maxneg, numpos, maxpos;

  BlockId curr_block;
  Vector(Block) blocks;
  Vector(Variable) vars;
  Vector(ListEntry) list_entries;
  Vector(InstrId) upsilons;
  u32_to_u32 bindings;
  u64_to_u32 ircache;
} Func;



static Type types[TY_MAX] = {
  { TK_VOID, 0 },
  { TK_BOOL, sizeof(bool) },
  { TK_INT, sizeof(i32) },
};



BlockId create_block(Func *f) {
  BlockId id = vector_size(f->blocks);
  assert(id < INT16_MAX);
  vector_push(f->blocks, (Block) { });
  return id;
}

static Block *get_block(Func *f, BlockId block) {
  assert(block);
  while (vector_size(f->blocks) <= block) {
    create_block(f);
  }
  return f->blocks + block;
}

static void init_code_array(Func *f) {
  f->maxneg = f->maxpos = 16;
  f->code = (Instr *)malloc(sizeof(Instr) * (f->maxneg + f->maxpos)) + f->maxneg;
  f->code[0] = (Instr) { .tag = IR_NOP, .type = TY_VOID, }; // let instr at index 0 be NOP
  f->numpos = 1;
  f->numneg = 0;
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

static InstrId emit_instr(Func *f, Instr instr, bool pinned) {
  InstrId id;
  if (pinned) {
    assert(f->curr_block || instr.tag == IR_LABEL);
    if (f->numpos == f->maxpos) {
      u32 maxpos = f->maxpos * 2;
      Instr *code = (Instr *)malloc(sizeof(Instr) * (f->maxneg + maxpos)) + f->maxneg;
      memcpy(code - f->numneg, f->code - f->numneg, sizeof(Instr) * (f->numneg + f->numpos));
      free(f->code - f->maxneg);
      f->code = code;
      f->maxpos = maxpos;
    }
    id = f->numpos;
  } else {
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
    id = -(f->numneg + 1);
  }
  assert(id > INT16_MIN && id < INT16_MAX);

  Block *b;
  if (instr.tag == IR_LABEL) {
    assert(id == f->numpos);
    b = get_block(f, instr.rhs);
    b->start = b->end = id;
    f->curr_block = instr.rhs;
    goto do_write;
  }

  b = get_block(f, f->curr_block);

  switch (instr.tag) {
    case IR_NOP:
      return 0;

    case IR_UPSILON:
      vector_push(f->upsilons, id);
      break;

    case IR_JUMP: {
      if (b->succs[0] ) {
        // already has IR_JUMP instr
        return 0;
      }
      Block *tb = get_block(f, instr.rhs);
      assert(!b->succs[0]);
      assert(!tb->is_sealed);
      b->succs[0] = instr.rhs;
      list_push(f->curr_block, &tb->preds, &f->list_entries);
      break;
    }

    case IR_JFALSE: {
      Block *tb = get_block(f, instr.rhs);
      Instr cond = f->code[instr.lhs];
      assert(cond.type == TY_BOOL);
      assert(!b->succs[0]);
      assert(!b->succs[1]);
      assert(!tb->is_sealed);
      if (cond.tag == IR_CONST && false) {
        if (cond.bool_const) {
          // don't emit, because branch will never be taken
          return 0;
        }
        // emit as IR_JUMP, because branch will always be taken
        instr.tag = IR_JUMP;
        b->succs[0] = instr.rhs;
        list_push(f->curr_block, &tb->preds, &f->list_entries);
      } else {
        b->succs[1] = instr.rhs;
        list_push(f->curr_block, &tb->preds, &f->list_entries);
      }
      break;
    }

    case IR_PHI: {
      if (b->is_sealed && b->preds.values[0] && b->preds.values[1] && !b->preds.values[2]) {
        Block *branch0 = get_block(f, b->preds.values[0]);
        Block *branch1 = get_block(f, b->preds.values[1]);
        if (branch0->is_sealed && branch1->is_sealed && branch0->preds.values[0] && branch0->preds.values[0] == branch1->preds.values[0] && !branch0->preds.values[1] && !branch1->preds.values[1]) {
          Block *entry = get_block(f, branch0->preds.values[0]);
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
      break;
    }
  }

do_write:
  if (pinned) {
    assert(id > 0);
    ++b->end;
    ++f->numpos;
  } else {
    assert(id < 0);
    ++f->numneg;
    if (instr.tag >= IR_CONST) {
      u64_to_u32_put(&f->ircache, instr.u64_repr, id);
    }
  }
  f->code[id] = instr;
  return id;
}

static InstrId emit_instr_pinned(Func *f, Instr instr) {
  return emit_instr(f, instr, true);
}

static InstrId emit_instr_unpinned(Func *f, Instr instr) {
  return emit_instr(f, instr, false);
}

static InstrId append_block_instr(Func *f, BlockId block, Instr instr) {
  if (block == f->curr_block) {
    return emit_instr_pinned(f, instr);
  }
  Block *b = get_block(f, block);
  BlockId temp = f->curr_block;
  f->curr_block = block;
  InstrId id = emit_instr_unpinned(f, instr);
  f->curr_block = temp;
  list_push(id, &b->suffix, &f->list_entries);
  return id;
}

static InstrId prepend_block_instr(Func *f, BlockId block, Instr instr) {
  Block *b = get_block(f, block);
  if (block == f->curr_block && b->end == b->start + 1) {
    return emit_instr_pinned(f, instr);
  }
  BlockId temp = f->curr_block;
  f->curr_block = block;
  InstrId id = emit_instr_unpinned(f, instr);
  f->curr_block = temp;
  list_push(id, &b->prefix, &f->list_entries);
  return id;
}


InstrId emit_bool(Func *f, bool val) {
  return emit_instr_unpinned(f, (Instr) { .tag = IR_CONST, .type = TY_BOOL, .bool_const = val });
}

InstrId emit_i32(Func *f, i32 val) {
  return emit_instr_unpinned(f, (Instr) { .tag = IR_CONST, .type = TY_I32, .i32_const = val });
}

InstrId emit_add(Func *f, InstrId lhs, InstrId rhs) {
  return emit_instr_unpinned(f, (Instr) { .tag = IR_ADD, .type = f->code[lhs].type, .lhs = lhs, .rhs = rhs });
}

void emit_label(Func *f, BlockId block) {
  emit_instr_pinned(f, (Instr) { .tag = IR_LABEL, .rhs = block });
}

void emit_jump(Func *f, BlockId block, BlockId target) {
  append_block_instr(f, block, (Instr) { .tag = IR_JUMP, .rhs = target });
}

void emit_jfalse(Func *f, BlockId block, BlockId false_target, InstrId cond) {
  append_block_instr(f, block, (Instr) { .tag = IR_JFALSE, .lhs = cond, .rhs = false_target });
}

void emit_ret(Func *f, BlockId block, InstrId retval) {
  append_block_instr(f, block, (Instr) { .tag = IR_RET, .lhs = retval });
}

void emit_upsilon(Func *f, BlockId block, InstrId val, InstrId phi) {
  append_block_instr(f, block, (Instr) { .tag = IR_UPSILON, .lhs = val, .rhs = phi });
}

InstrId emit_phi(Func *f, BlockId block) {
  return prepend_block_instr(f, block, (Instr) { .tag = IR_PHI });
}





typedef union {
  struct { i16 block, var; };
  i32 u32_value;
} BindingKey;

static InstrId create_pred_upsilons(Func *f, BlockId block, InstrId phi);

void write_variable(Func *f, BlockId block, VarId var, InstrId val) {
  BindingKey key = {{ block, var }};
  u32_to_u32_put(&f->bindings, key.u32_value, val);
}

InstrId read_variable(Func *f, BlockId block, VarId var) {
  BindingKey key = {{ block, var }};
  u32 found;
  if (u32_to_u32_get(&f->bindings, key.u32_value, &found)) {
    return found;
  }

  InstrId val;
  Block *b = get_block(f, block);
  if (!b->is_sealed) {
    val = emit_phi(f, block);
    list_push(val, &b->incomplete_phis, &f->list_entries);
  } else if (b->preds.values[0] && !b->preds.values[1]) {
    // exactly one predecessor
    val = read_variable(f, b->preds.values[0], var);
  } else {
    val = emit_phi(f, block);
    write_variable(f, block, var, val);
    val = create_pred_upsilons(f, block, val);
  }

  write_variable(f, block, var, val);
  return val;
}

static InstrId try_remove_trivial_phi(Func *f, InstrId phi) {
  InstrId phi_users[128];
  int phi_users_count = 0;

  InstrId same = 0, id;
  vector_foreach(f->upsilons, id) {
    Instr instr = f->code[id];
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
  f->code[phi] = (Instr) { .tag = IR_IDENTITY, .lhs = same };

  for (int i = 0; i < phi_users_count; ++i) {
    try_remove_trivial_phi(f, phi_users[i]);
  }
}

static InstrId create_pred_upsilons(Func *f, BlockId block, InstrId phi) {
  VarId var = f->code[phi].rhs;
  Block *b = get_block(f, block);
  for (ListEntry *e = &b->preds; ; e = f->list_entries + e->next) {
    for (int i = 0; i < 3; ++i) {
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

void seal_block(Func *f, BlockId block) {
  Block *b = get_block(f, block);
  assert(!b->is_sealed);
  for (ListEntry *e = &b->incomplete_phis; ; e = f->list_entries + e->next) {
    for (int i = 0; i < 3; ++i) {
      BlockId phi = e->values[i];
      if (!phi) goto end;
      create_pred_upsilons(f, block, phi);
    }
    if (!e->next) break;
  }

end:
  b->is_sealed = true;
}




static InstrId reemit_dependency(Func *f, Instr *code, i16 *map, InstrId id);

static Instr reemit_dependencies(Func *f, Instr *code, i16 *map, Instr instr) {
  switch (instr.tag) {
    case IR_JFALSE:
    case IR_RET:
      instr.lhs = reemit_dependency(f, code, map, instr.lhs);
      break;
    case IR_UPSILON:
      instr.lhs = reemit_dependency(f, code, map, instr.lhs);
      instr.rhs = map[instr.rhs];
      assert(instr.rhs);
      break;
    case IR_ADD:
    case IR_EQ:
      instr.lhs = reemit_dependency(f, code, map, instr.lhs);
      instr.rhs = reemit_dependency(f, code, map, instr.rhs);
      break;
  }
  return instr;
}

static InstrId reemit_dependency(Func *f, Instr *code, i16 *map, InstrId id) {
  if (map[id]) {
    return map[id];
  }
  InstrId src = id;
  //while (code[src].tag == IR_IDENTITY) {
  //  src = code[src].lhs;
  //}
  assert(code[id].tag >= IR_CONST);
  return map[id] = emit_instr_unpinned(f, reemit_dependencies(f, code, map, code[src]));
}

static void reemit_block(Func *f, BlockId block, Vector(Block) blocks, Instr *code, i16 *map) {
  Block *b = blocks + block;
  if (b->is_visited) {
    return;
  }
  b->is_visited = true;

  for (ListEntry *e = &b->prefix; ; e = f->list_entries + e->next) {
    for (int i = 0; i < 3; ++i) {
      InstrId id = e->values[i];
      if (!id) goto prefix_done;
      map[id] = emit_instr_pinned(f, code[id]);
    }
    if (!e->next) break;
  }
prefix_done:

  for (InstrId id = b->start; id < b->end; ++id) {
    Instr instr = code[id];
    map[id] = emit_instr_pinned(f, instr);
  }

  for (ListEntry *e = &b->suffix; ; e = f->list_entries + e->next) {
    for (int i = 0; i < 3; ++i) {
      InstrId id = e->values[i];
      if (!id) goto suffix_done;
      map[id] = emit_instr_pinned(f, code[id]);
    }
    if (!e->next) break;
  }
suffix_done:

  for (int i = 0; i < 2; ++i) {
    if (b->succs[i]) {
      reemit_block(f, b->succs[i], blocks, code, map);
    }
  }
}

static void reemit_all(Func *f) {
  i16 map_buffer[65536] = { 0, };
  i16 *map = map_buffer + 32768; // map from old to new InstrId

  Instr *code = f->code;
  assert(code);
  Instr *code_base_ptr = code - f->maxneg;
  int end_id = f->numpos;
  init_code_array(f);

  Vector(ListEntry) list_entries = vector_clone(f->list_entries);
  f->list_entries = NULL;

  Vector(Block) blocks = vector_clone(f->blocks);
  f->blocks = NULL;
  f->curr_block = 0;

  vector_free(f->vars);
  f->vars = NULL;

  u64_to_u32_clear(&f->ircache);
  u32_to_u32_clear(&f->bindings);

  reemit_block(f, 1, blocks, code, map);

  for (InstrId id = 1; id < f->numpos; ++id) {
    f->code[id] = reemit_dependencies(f, code, map, f->code[id]);
  }

  InstrId id;
  vector_foreach(f->upsilons, id) {
    InstrId phi = map[f->code[id].rhs];
    assert(f->code[id].tag == IR_UPSILON);
    assert(phi);
    f->code[id].rhs = phi;
  }

  vector_free(list_entries);
  vector_free(blocks);
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
}




int main(int argc, char *argv[]) {
  Func *f = create_func();
  VarId x = create_variable(f, "x");
  BlockId entry_block = create_block(f);
  BlockId then_block = create_block(f);
  BlockId else_block = create_block(f);
  BlockId exit_block = create_block(f);

  seal_block(f, entry_block);
  emit_label(f, entry_block);
  write_variable(f, entry_block, x, emit_i32(f, 1));
  emit_jfalse(f, entry_block, else_block, emit_bool(f, true));
  emit_jump(f, entry_block, then_block);
  
  seal_block(f, then_block);
  emit_label(f, then_block);
  write_variable(f, then_block, x, emit_add(f, read_variable(f, then_block, x), emit_i32(f, 10)));
  emit_jump(f, then_block, exit_block);
  
  seal_block(f, else_block);
  emit_label(f, else_block);
  write_variable(f, else_block, x, emit_add(f, read_variable(f, else_block, x), emit_i32(f, 10)));
  emit_jump(f, else_block, exit_block);

  seal_block(f, exit_block);
  emit_label(f, exit_block);
  emit_ret(f, exit_block, read_variable(f, exit_block, x));

  //reemit_all(f);
  print_ir(f);

  return 0;
}
