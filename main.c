#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "vector.h"

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef float f32;
typedef double f64;

typedef i32 InstrId; // signed, so that pure instructions can have negative id (that way we don't commit to any sequencing of them early)
typedef u32 BlockId;
typedef u32 TypeId;
typedef u32 VarId;

typedef enum InstrTag {
  IR_NOP,
  
  IR_BLOCK, // identify the start of a block (lhs is BlockId)
  IR_DEF, // define lhs VarId to have the value of rhs InstrId
  IR_PHI, // defines a new phi value for lhs VarId with rhs linking to possible values (for all incoming control flow)
  IR_JUMP, // unconditional jump to BlockId lhs
  IR_BRANCH, // jump to BlockId lhs if the condition rhs is false
  IR_RET,

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
};

typedef struct Type {
  TypeKind kind;
  u32 size;
} Type;

typedef struct Instr {
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
} Instr;

typedef struct IndexEntry {
  i16 indexes[3];
  i16 next;
} IndexEntry;

typedef struct Block {
  i16 first, last; // first and last instruction index
  u32 use_predvec : 1; // use vector pointer instead of inline array
  u32 use_prefixvec : 1; // use vector pointer instead of inline array
  u32 is_sealed : 1;
  union { // (direct) predecessor blocks
    u8 preds[8];
    Vector(u16) predvec;
  };
  union { // instruction prefix which will be merged into the main instruction stream later
    i16 prefix[4];
    Vector(i16) prefixvec;
  };
} Block;

typedef struct Var {
  char name[16];
  TypeId type;
} Var;

typedef union ExtraData {
  struct {
    i16 values[3];
    i16 next;
  } list;
} ExtraData;

typedef struct Func {
  Instr *code;
  u32 numneg, maxneg, numpos, maxpos;

  BlockId curr_block;
  Vector(Block) blocks;
  Vector(Var) vars;
  Vector(ExtraData) extra;
} Func;



static Type types[1024] = {
  { TK_VOID, 0 },
  { TK_BOOL, sizeof(bool) },
  { TK_INT, sizeof(i32) },
};

static i16 replacement_map_proto[65536];

Func *create_func() {
  Func *f = calloc(1, sizeof(Func));
  f->maxneg = 16;
  f->maxpos = 16;
  f->code = (Instr *)malloc(sizeof(Instr) * (f->maxneg + f->maxpos)) + f->maxneg;
  return f;
}

BlockId create_block(Func *f) {
  BlockId id = vector_size(f->blocks);
  vector_push(f->blocks, (Block) { });
  return id;
}

VarId create_var(Func *f, const char *name, TypeId type) {
  assert(strlen(name) < 16);
  Var var = { .type = type };
  strcpy(var.name, name);
  VarId id = vector_size(f->vars);
  vector_push(f->vars, var);
  return id;
}

static void add_predecessor(Block *b, BlockId pred) {
  assert(pred);
  if (!b->use_predvec) {
    if (pred <= 255) {
      for (int i = 0; i < 8; ++i) {
        if (!b->preds[i]) {
          b->preds[i] = pred;
          return;
        }
        if (b->preds[i] == pred) {
          return;
        }
      }
    }
    Vector(u16) predvec = NULL;
    for (int i = 0; i < 8 && b->preds[i]; ++i) {
      vector_push(predvec, b->preds[i]);
    }
    b->predvec = predvec;
    b->use_predvec = 1;
  }
  BlockId p;
  vector_foreach(b->predvec, p) {
    if (p == pred) {
      return;
    }
  }
  vector_push(b->predvec, pred);
}

static void add_to_prefix(Func *f, BlockId block, InstrId instr) {
  assert(block < vector_size(f->blocks));
  Block *b = f->blocks + block;
  if (!b->use_prefixvec) {
    for (int i = 0; i < 4; ++i) {
      if (!b->prefix[i]) {
        b->prefix[i] = instr;
        return;
      }
    }
    Vector(i16) prefixvec = NULL;
    for (int i = 0; i < 4; ++i) {
      vector_push(prefixvec, b->prefix[i]);
    }
    b->prefixvec = prefixvec;
    b->use_prefixvec = 1;
  }
  vector_push(b->prefixvec, instr);
}

static InstrId emit_ordered_instr(Func *f, Instr instr) {
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
  f->code[id] = instr;
  return id;
}

static InstrId emit_unordered_instr(Func *f, Instr instr) {
  if (f->numneg == f->maxneg) {
    u32 maxneg = f->maxneg * 2;
    Instr *code = (Instr *)malloc(sizeof(Instr) * (maxneg + f->maxpos)) + maxneg;
    memcpy(code - f->numneg, f->code - f->numneg, sizeof(Instr) * (f->numneg + f->numpos));
    free(f->code - f->maxneg);
    f->code = code;
    f->maxneg = maxneg;
  }
  InstrId id = -(++f->numneg);
  f->code[id] = instr;
  return id;
}

InstrId emit_instr(Func *f, Instr instr) {
  if (instr.tag >= IR_CONST) {
    // allocate pure instructions at negative indices (so they don't have explicit ordering initially)
    return emit_unordered_instr(f, instr);
  } else {
    // impure instructions are laid out normally with positive indices and significant sequencing
    return emit_ordered_instr(f, instr);
  }
}

InstrId emit_bool(Func *f, bool val) {
  return emit_unordered_instr(f, (Instr) { .tag = IR_CONST, .type = TY_BOOL, .bool_const = val });
}

InstrId emit_i32(Func *f, i32 val) {
  return emit_unordered_instr(f, (Instr) { .tag = IR_CONST, .type = TY_I32, .i32_const = val });
}

InstrId emit_add(Func *f, InstrId lhs, InstrId rhs) {
  assert(f->code[lhs].type == f->code[rhs].type);
  return emit_unordered_instr(f, (Instr) { .tag = IR_ADD, .type = f->code[lhs].type, .lhs = lhs, .rhs = rhs });
}

void emit_block(Func *f, BlockId block) {
  if (f->curr_block == block) {
    assert(f->code[f->numpos - 1].tag == IR_BLOCK);
    assert(f->code[f->numpos - 1].lhs == block);
    return;
  }
  assert(!f->curr_block);
  f->curr_block = block;
  f->blocks[block].first = f->numpos;
  emit_ordered_instr(f, (Instr) { .tag = IR_BLOCK, .lhs = block });
}

void emit_ret(Func *f, InstrId retval) {
  assert(f->curr_block);
  f->blocks[f->curr_block].last = f->numpos;
  f->curr_block = 0;
  emit_ordered_instr(f, (Instr) { .tag = IR_RET, .lhs = retval });
}

void emit_jump(Func *f, BlockId target) {
  assert(f->curr_block);
  f->blocks[f->curr_block].last = f->numpos;
  add_predecessor(f->blocks + target, f->curr_block);
  f->curr_block = 0;
  emit_ordered_instr(f, (Instr) { .tag = IR_JUMP, .lhs = target });
}

void emit_branch(Func *f, BlockId target, BlockId next, InstrId cond) {
  assert(f->curr_block);
  f->blocks[f->curr_block].last = f->numpos;
  add_predecessor(f->blocks + target, f->curr_block);
  add_predecessor(f->blocks + next, f->curr_block);
  f->curr_block = 0;
  emit_ordered_instr(f, (Instr) { .tag = IR_BRANCH, .lhs = target, .rhs = cond });
  emit_block(f, next);
}

void emit_def(Func *f, VarId var, InstrId val) {
  emit_ordered_instr(f, (Instr) { .tag = IR_DEF, .lhs = var, .rhs = val });
}

InstrId emit_phi(Func *f, VarId var) {
  return emit_ordered_instr(f, (Instr) { .tag = IR_PHI, .lhs = var });
}

InstrId do_lookup_var(Func *f, BlockId block, VarId var, i32 pos) {
  // search the block body itself for a definition
  Block *b = f->blocks + block;
  for (int id = pos; id >= b->first; --id) {
    Instr *in = f->code + id;
    if (in->tag == IR_DEF && in->lhs == var) {
      return in->rhs;
    }
    if (in->tag == IR_PHI && in->lhs == var) {
      return id;
    }
  }

  // search the block's instruction prefix for a definition
  if (b->use_prefixvec) {
    InstrId id;
    vector_foreach_reverse(b->prefixvec, id) {
      Instr *in = f->code + id;
      if (in->tag == IR_DEF && in->lhs == var) {
        return in->rhs;
      }
      if (in->tag == IR_PHI && in->lhs == var) {
        return id;
      }
    }
  } else {
    for (int i = 3; i >= 0; --i) {
      int id = b->prefix[i];
      if (!id) {
        continue;
      }
      Instr *in = f->code + id;
      if (in->tag == IR_DEF && in->lhs == var) {
        return in->rhs;
      }
      if (in->tag == IR_PHI && in->lhs == var) {
        return id;
      }
    }
  }

  if (!b->is_sealed) {
    // emit an empty (incoomplete) phi
    return emit_phi(f, var);
  }

  // sealed block (all predecessors known)
  
  // check for simple case of single predecessor (and just recurse if that's the case)
  if (b->use_predvec) {
    if (vector_size(b->predvec) == 1) {
      BlockId pred = b->predvec[0];
      return do_lookup_var(f, pred, var, f->blocks[pred].first);
    }
  } else {
    if (b->preds[0] && !b->preds[1]) {
      BlockId pred = b->preds[0];
      return do_lookup_var(f, pred, var, f->blocks[pred].first);
    }
  }
  
  // recurse to search predecessors
  if (b->use_predvec) {
    BlockId pred;
    vector_foreach(b->predvec, pred) {
      InstrId result = do_lookup_var(f, pred, var, f->blocks[pred].first);
      if (result) {
        return result;
      }
    }
  } else {
    for (int i = 0; i < 8; ++i) {
      BlockId pred = b->preds[i];
      if (!pred) {
        break;
      }
      InstrId result = do_lookup_var(f, pred, var, f->blocks[pred].first);
      if (result) {
        return result;
      }
    }
  }

  return 0;
}

InstrId lookup_var(Func *f, VarId var) {
  return do_lookup_var(f, f->curr_block, var, f->numpos);
}

#define REMAP(X) (X) < 0 ? (map[X] ? map[X] : map[X] = )

static void reemit_all(Func *f) {
  i16 map_buffer[65536];
  memcpy(map_buffer, replacement_map_proto, sizeof(map_buffer));
  i16 *map = map_buffer + 32768;
  
  Instr *code = f->code;
  Instr *code_base_ptr = code - f->maxneg;
  int start = -f->numneg;
  int end = f->numpos;
  f->code = NULL;
  f->numneg = f->maxneg = f->numpos = f->maxpos = 0;

  for (int i = start; i <= end; ++i) {
    
  }
  
  free(code_base_ptr);
}


int main(int argc, char *argv[]) {
  for (int i = 0; i < 65536; ++i) {
    replacement_map_proto[i] = i - 32768;
  }
  
  Func *f = create_func();
  VarId x = create_var(f, "x", TY_I32);
  BlockId entry_block = create_block(f);
  BlockId then_block = create_block(f);
  BlockId else_block = create_block(f);
  BlockId exit_block = create_block(f);

  emit_block(f, entry_block);
  emit_def(f, x, emit_i32(f, 1));
  emit_branch(f, else_block, then_block, emit_bool(f, true));
  
  emit_block(f, then_block);
  emit_def(f, x, emit_add(f, lookup_var(f, x), emit_i32(f, 10)));
  emit_jump(f, exit_block);
  
  emit_block(f, else_block);
  emit_def(f, x, emit_add(f, lookup_var(f, x), emit_i32(f, 20)));
  emit_jump(f, exit_block);

  emit_block(f, exit_block);
  emit_ret(f, lookup_var(f, x));

  return 0;
}

/*

int i = 0;
while (i < 10) {
  ++i;
}

LOOP
  v0 = LT i 10
  v1 = NOT v0
  COND v1
  BREAK v1
  INC i
  CONT

BLOCK
  asd
  BREAK


int i = 0;
if (i == 0) {
  ++i;
} else {
  --i;
}

BLOCK
  v0 = EQ i 0
  BREAK_IF 

  

*/


