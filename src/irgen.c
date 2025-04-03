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

static void list_push_unique(i16 value, ListEntry *e, Vector(ListEntry) *entries) {
start:
  for (int i = 0; i < 3; ++i) {
    if (e->values[i] == value) {
      return;
    }
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

static bool list_contains(i16 value, ListEntry *e, Vector(ListEntry) entries) {
start:
  for (int i = 0; i < 3; ++i) {
    if (e->values[i] == value) {
      return true;
    }
  }
  if (e->next) {
    e = entries + e->next;
    goto start;
  }
  return false;
}

#define list_foreach(Item, Head, Entries) \
  for (ListEntry *__entry = &(Head); __entry; __entry = __entry->next ? (Entries) + __entry->next : NULL) \
  for (int __index = 0; __index < 3 && (Item = __entry->values[__index]); ++__index)



typedef struct Variable {
  IrType type;
} Variable;


typedef struct BasicBlock {
  u32 sealed : 1;
  u32 visited : 1;
  u32 first : 15;
  u32 last : 15;
  
  u32 idom : 16;
  u32 dom_depth : 16;
  
  u32 postorder : 16;
  u32 loop_depth : 16;

  u16 succs[2];
  
  ListEntry preds;
  ListEntry incomplete_phis;
  ListEntry suffix;
} BasicBlock;

typedef struct Phi {
  IrVarRef var;
  IrInstrRef instr;
  ListEntry upsilons;
} Phi;

struct IrGen {
  IrInstr *code;
  i32 numneg, maxneg, numpos, maxpos;

  IrBlockRef curr_block;
  Vector(BasicBlock) blocks;
  Vector(Variable) vars;
  Vector(Phi) phis;
  Vector(ListEntry) list_entries;
  u32_to_u64 bindings;
  u64_to_u32 ircache;
};



#define IR_INSTR_INFO_ARG0_IS_VALUE 1
#define IR_INSTR_INFO_ARG1_IS_VALUE 2
#define IR_INSTR_INFO_ARG2_IS_VALUE 4

static int instr_info[IR_NUM_TAGS] = {
  [IR_PRINT] = IR_INSTR_INFO_ARG0_IS_VALUE,
  [IR_BRANCH] = IR_INSTR_INFO_ARG0_IS_VALUE,
  [IR_RET] = IR_INSTR_INFO_ARG0_IS_VALUE,
  [IR_UPSILON] = IR_INSTR_INFO_ARG1_IS_VALUE,
  [IR_ADD] = IR_INSTR_INFO_ARG0_IS_VALUE | IR_INSTR_INFO_ARG1_IS_VALUE,
  [IR_EQ] = IR_INSTR_INFO_ARG0_IS_VALUE | IR_INSTR_INFO_ARG1_IS_VALUE,
  [IR_NEQ] = IR_INSTR_INFO_ARG0_IS_VALUE | IR_INSTR_INFO_ARG1_IS_VALUE
};




static void print_instr(IrGen *gen, IrInstrRef ref, IrInstr instr) {
  switch (instr.tag) {
    case IR_NOP: printf("  NOP\n"); break;
    case IR_IDENTITY: printf("  %d = ID %d\n", ref, instr.arg0); break;
    case IR_PRINT: printf("  PRINT %d\n", instr.arg0); break;
    case IR_LABEL: printf(":%d\n", instr.arg0); break;
    case IR_JUMP: printf("  JUMP :%d\n", instr.arg0); break;
    case IR_BRANCH: printf("  BRANCH %d :%d :%d\n", instr.arg0, instr.arg1, instr.arg2); break;
    case IR_RET: printf("  RET %d\n", instr.arg0); break;
    case IR_UPSILON: printf("  UPSILON [%d] %d\n", instr.arg0, instr.arg1); break;
    case IR_CONST:
      switch (instr.type) {
        case TY_VOID: printf("  %d = CONST void\n", ref); break;
        case TY_BOOL: printf("  %d = CONST %s\n", ref, instr.bool_const ? "true" : "false"); break;
        case TY_I32: printf("  %d = CONST %d\n", ref, instr.i32_const); break;
        default: assert(0); break;
      }
      break;
    case IR_PHI: printf("  %d = PHI [%d]\n", ref, instr.arg0); break;
    case IR_ARG: printf("  %d = ARG #%d\n", ref, instr.arg0); break;
    case IR_ADD: printf("  %d = ADD %d %d\n", ref, instr.arg0, instr.arg1); break;
    case IR_EQ: printf("  %d = EQ %d %d\n", ref, instr.arg0, instr.arg1); break;
    case IR_NEQ: printf("  %d = NEQ %d %d\n", ref, instr.arg0, instr.arg1); break;
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

static BasicBlock *get_or_create_block(IrGen *gen, IrBlockRef block) {
  assert(block);
  while (vector_size(gen->blocks) <= block) {
    irgen_create_block(gen);
  }
  return gen->blocks + block;
}

static BasicBlock *get_block(IrGen *gen, IrBlockRef block) {
  assert(block);
  assert(block < vector_size(gen->blocks));
  return gen->blocks + block;
}

static IrBlockRef skip_trivial_successors(IrGen *gen, IrBlockRef succ) {
  BasicBlock *b = get_block(gen, succ);
  if (b->last == b->first + 1 && !b->suffix.values[0] && b->succs[0] && !b->succs[1]) {
    // block has only a single jump instruction
    assert(gen->code[b->last].tag == IR_JUMP);
    return skip_trivial_successors(gen, b->succs[0]);
  }
  return succ;
}

static IrInstrRef get_trivial_exit(IrGen *gen, IrBlockRef block) {
  BasicBlock *b = get_block(gen, block);
  if (b->last == b->first + 1 && !b->suffix.values[0]) {
    if (gen->code[b->last].tag == IR_RET) {
      return b->last;
    }
  }
  return 0;
}

static IrPhiRef irgen_create_phi_var(IrGen *gen, IrVarRef var) {
  i32 phi = vector_size(gen->phis);
  assert(phi < INT16_MAX);
  vector_push(gen->phis, (Phi) { .var = var });
  return phi;
}

IrPhiRef irgen_create_phi(IrGen *gen) {
  return irgen_create_phi_var(gen, 0);
}

static Phi *get_phi(IrGen *gen, IrPhiRef phi) {
  assert(phi);
  while (vector_size(gen->phis) <= phi) {
    irgen_create_phi(gen);
  }
  return gen->phis + phi;
}




IrGen *irgen_create(void) {
  IrGen *gen = calloc(1, sizeof(IrGen));
  gen->maxneg = gen->maxpos = 16;
  gen->code = (IrInstr *)malloc(sizeof(IrInstr) * (gen->maxneg + gen->maxpos)) + gen->maxneg;
  gen->code[0] = (IrInstr) { }; // let instr at index 0 be NOP
  gen->numpos = 1; // reserve NOP slot
  gen->numneg = 0;
  irgen_create_block(gen); // block at index 0 should not be used
  irgen_create_variable(gen, TY_VOID); // var at index 0 should not be used
  irgen_create_phi(gen); // phi at index 0 should not be used
  return gen;
}

void irgen_destroy(IrGen *gen) {
  vector_free(gen->blocks);
  vector_free(gen->vars);
  vector_free(gen->phis);
  vector_free(gen->list_entries);
  u32_to_u64_cleanup(&gen->bindings);
  u64_to_u32_cleanup(&gen->ircache);
  free(gen);
}

void irgen_clear(IrGen *gen) {
  gen->code[0] = (IrInstr) { }; // let instr at index 0 be NOP
  gen->numpos = 1; // reserve NOP slot
  gen->numneg = 0;
  gen->curr_block = 0;
  vector_clear(gen->blocks);
  vector_clear(gen->vars);
  vector_clear(gen->phis);
  vector_clear(gen->list_entries);
  u32_to_u64_clear(&gen->bindings);
  u64_to_u32_clear(&gen->ircache);
  irgen_create_block(gen); // block at index 0 should not be used
  irgen_create_variable(gen, TY_VOID); // var at index 0 should not be used
  irgen_create_phi(gen); // phi at index 0 should not be used
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

static IrInstrRef try_lookup_instr(IrGen *gen, IrInstr instr) {
  u32 cached = 0;
  u64_to_u32_get(&gen->ircache, instr.u64_repr, &cached);
  return cached;
}

static IrInstrRef emit_instr_unpinned(IrGen *gen, IrInstr instr) {
  if (IR_IS_PURE_INSTR(instr.tag)) {
    u32 cached = try_lookup_instr(gen, instr);
    if (cached) {
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
  BasicBlock *b = get_or_create_block(gen, block);
  if (block == gen->curr_block && !b->last) {
    assert(!IR_IS_PURE_INSTR(instr.tag));
    return emit_instr_pinned(gen, instr);
  }
  IrInstrRef ref = emit_instr_unpinned(gen, instr);
  list_push(ref, &b->suffix, &gen->list_entries);
  return ref;
}



static IrInstr to_instr(IrGen *gen, IrInstrRef ref) {
  assert(ref);
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


void irgen_print(IrGen *gen, IrInstr val) {
  emit_instr_pinned(gen, (IrInstr) { .tag = IR_PRINT, .arg0 = intern_instr(gen, val) });
}

void irgen_label(IrGen *gen, IrBlockRef block) {
  assert(!gen->curr_block || get_block(gen, gen->curr_block)->last);
  BasicBlock *b = get_or_create_block(gen, block);
  assert(!b->first);
  assert(!b->last);
  assert(!b->succs[0]);
  assert(!b->succs[1]);
  IrInstrRef ref = emit_instr_pinned(gen, (IrInstr) { .tag = IR_LABEL, .arg0 = block });
  b->first = ref;
  gen->curr_block = block;
}

void irgen_jump(IrGen *gen, IrBlockRef target) {
  get_or_create_block(gen, target);
  BasicBlock *b = get_or_create_block(gen, gen->curr_block);
  BasicBlock *tb = get_block(gen, target);
  assert(!b->last);
  assert(!b->succs[0]);
  assert(!tb->sealed);
  b->succs[0] = target;
  list_push(gen->curr_block, &tb->preds, &gen->list_entries);
  b->last = emit_instr_pinned(gen, (IrInstr) { .tag = IR_JUMP, .arg0 = target });
}

void irgen_branch(IrGen *gen, IrInstr cond, IrBlockRef true_target, IrBlockRef false_target) {
  get_or_create_block(gen, true_target);
  get_or_create_block(gen, false_target);
  BasicBlock *b = get_or_create_block(gen, gen->curr_block);
  BasicBlock *tb = get_block(gen, true_target);
  BasicBlock *fb = get_block(gen, false_target);
  assert(!b->last);
  assert(!b->succs[0]);
  assert(!b->succs[1]);
  assert(!tb->sealed);
  assert(!fb->sealed);
  assert(cond.type == TY_BOOL);
  if (cond.tag == IR_CONST) {
    irgen_jump(gen, cond.bool_const ? true_target : false_target);
    return;
  }
  if (true_target == false_target) {
    irgen_jump(gen, true_target);
    return;
  }
  b->succs[0] = true_target;
  b->succs[1] = false_target;
  list_push(gen->curr_block, &tb->preds, &gen->list_entries);
  list_push(gen->curr_block, &fb->preds, &gen->list_entries);
  b->last = emit_instr_pinned(gen, (IrInstr) { .tag = IR_BRANCH, .arg0 = intern_instr(gen, cond), .arg1 = true_target, .arg2 = false_target });
}

void irgen_ret(IrGen *gen, IrInstr retval) {
  BasicBlock *b = get_or_create_block(gen, gen->curr_block);
  assert(!b->last);
  IrInstr instr = (IrInstr) { .tag = IR_RET, .arg0 = intern_instr(gen, retval) };
  b->last = emit_instr_pinned(gen, instr);
}

void irgen_upsilon(IrGen *gen, IrBlockRef block, IrPhiRef phi, IrInstr val) {
  // upsilons may be emitted even after a block is filled (recursive variable lookup will create upsilons in predecessor blocks as needed)
  if (phi) {
    IrInstrRef ref = append_block_instr(gen, block, (IrInstr) { .tag = IR_UPSILON, .arg0 = phi, .arg1 = intern_instr(gen, val) });
    Phi *p = get_phi(gen, phi);
    list_push(ref, &p->upsilons, &gen->list_entries);
  }
}

IrInstr irgen_phi(IrGen *gen, IrPhiRef phi, IrType type) {
  if (phi) {
    Phi *p = get_phi(gen, phi);
    assert(type != TY_VOID);
    assert(!p->instr);
    p->instr = emit_instr_pinned(gen, (IrInstr) { .tag = IR_PHI, .type = type, .arg0 = phi });
    return (IrInstr) { .tag = IR_IDENTITY, .type = type, .arg0 = p->instr };
  } else {
    assert(type == TY_VOID);
    return (IrInstr) { };
  }
}

IrInstr irgen_const_bool(IrGen *gen, bool val) {
  return (IrInstr) { .tag = IR_CONST, .type = TY_BOOL, .bool_const = val };
}

IrInstr irgen_const_i32(IrGen *gen, i32 val) {
  return (IrInstr) { .tag = IR_CONST, .type = TY_I32, .i32_const = val };
}

IrInstr irgen_arg(IrGen *gen, u32 arg, IrType type) {
  assert(arg >= 0);
  assert(type != TY_VOID);
  return (IrInstr) { .tag = IR_ARG, .type = type, .arg0 = arg };
}

IrInstr irgen_add(IrGen *gen, IrInstr lhs, IrInstr rhs) {
  assert(lhs.type == rhs.type);
  if (lhs.tag == IR_CONST && rhs.tag == IR_CONST) {
    switch (lhs.type) {
      case TY_I32: return (IrInstr) { .tag = IR_CONST, .type = lhs.type, .i32_const = lhs.i32_const + rhs.i32_const };
      default: assert(0 && "unexpected type");
    }
  }
  return (IrInstr) { .tag = IR_ADD, .type = lhs.type, .arg0 = intern_instr(gen, lhs), .arg1 = intern_instr(gen, rhs) };
}

IrInstr irgen_eq(IrGen *gen, IrInstr lhs, IrInstr rhs) {
  assert(lhs.type == rhs.type);
  if (lhs.tag == IR_CONST && rhs.tag == IR_CONST) {
    switch (lhs.type) {
      case TY_BOOL: return (IrInstr) { .tag = IR_CONST, .type = TY_BOOL, .bool_const = lhs.bool_const == rhs.bool_const };
      case TY_I32: return (IrInstr) { .tag = IR_CONST, .type = TY_BOOL, .bool_const = lhs.i32_const == rhs.i32_const };
      default: assert(0 && "unexpected type");
    }
  }
  return (IrInstr) { .tag = IR_EQ, .type = TY_BOOL, .arg0 = intern_instr(gen, lhs), .arg1 = intern_instr(gen, rhs) };
}

IrInstr irgen_neq(IrGen *gen, IrInstr lhs, IrInstr rhs) {
  assert(lhs.type == rhs.type);
  if (lhs.tag == IR_CONST && rhs.tag == IR_CONST) {
    switch (lhs.type) {
      case TY_BOOL: return (IrInstr) { .tag = IR_CONST, .type = TY_BOOL, .bool_const = lhs.bool_const != rhs.bool_const };
      case TY_I32: return (IrInstr) { .tag = IR_CONST, .type = TY_BOOL, .bool_const = lhs.i32_const != rhs.i32_const };
      default: assert(0 && "unexpected type");
    }
  }
  return (IrInstr) { .tag = IR_NEQ, .type = TY_BOOL, .arg0 = intern_instr(gen, lhs), .arg1 = intern_instr(gen, rhs) };
}




static IrInstr create_pred_upsilons(IrGen *gen, IrBlockRef block, IrPhiRef phi) {
  IrBlockRef preds[32];
  IrInstr values[32];
  int num_values = 0;

  assert(block);
  assert(phi);
  IrVarRef var = gen->phis[phi].var;
  assert(var);
  IrInstrRef phi_instr_ref = gen->phis[phi].instr;
  IrInstr phi_instr = to_instr(gen, phi_instr_ref);

  bool found_different = false;
  IrInstr candidate = (IrInstr) { };
  IrBlockRef pred;
  list_foreach(pred, gen->blocks[block].preds, gen->list_entries) {
    IrInstr val = irgen_read_variable(gen, pred, var);
    
    assert(num_values < 32);
    preds[num_values] = pred;
    values[num_values++] = val;

    if (val.u64_repr != phi_instr.u64_repr) {
      if (!candidate.u64_repr) {
        candidate = val;
      } else if (val.u64_repr != candidate.u64_repr) {
        found_different = true;
      }
    }
  }

  assert(num_values > 1);
  if (candidate.u64_repr && !found_different) {
    gen->code[phi_instr_ref] = (IrInstr) { .tag = IR_IDENTITY, .type = candidate.type, .arg0 = intern_instr(gen, candidate) };
    return candidate;
  }

  for (int i = 0; i < num_values; ++i) {
    irgen_upsilon(gen, preds[i], phi, values[i]);
  }
  return phi_instr;
}

void irgen_seal_block(IrGen *gen, IrBlockRef block) {
  //printf("sealing %d\n", block);
  BasicBlock *b = get_block(gen, block);
  assert(!b->sealed);
  b->sealed = true;
  IrInstrRef phi;
  list_foreach(phi, b->incomplete_phis, gen->list_entries) {
    create_pred_upsilons(gen, block, phi);
  }
}

typedef union {
  struct { i16 block, var; };
  i32 u32_repr;
} BindingKey;

void irgen_write_variable(IrGen *gen, IrBlockRef block, IrVarRef var, IrInstr val) {
  assert(block);
  assert(var);
  //printf("block %d WRITE:", block); print_instr(gen, var, val);
  assert(val.type != TY_VOID);
  assert(val.type == gen->vars[var].type);
  BindingKey key = {{ block, var }};
  u32_to_u64_put(&gen->bindings, key.u32_repr, val.u64_repr);
}

IrInstr irgen_read_variable(IrGen *gen, IrBlockRef block, IrVarRef var) {
  assert(block);
  assert(var);
  assert(gen->vars[var].type);
  BindingKey key = {{ block, var }};
  BasicBlock *b = get_block(gen, block);
  IrInstr found;
  if (u32_to_u64_get(&gen->bindings, key.u32_repr, &found.u64_repr)) {
    //printf("block %d READ: ", block); print_instr(gen, var, found);
    return found;
  }

  IrInstr result;
  if (!b->sealed) {
    IrPhiRef phi = irgen_create_phi_var(gen, var);
    list_push(phi, &b->incomplete_phis, &gen->list_entries);
    result = irgen_phi(gen, phi, gen->vars[var].type);
  } else if (b->preds.values[0] && !b->preds.values[1]) {
    // exactly one predecessor
    result = irgen_read_variable(gen, b->preds.values[0], var);
  } else {
    IrPhiRef phi = irgen_create_phi_var(gen, var);
    result = irgen_phi(gen, phi, gen->vars[var].type);
    irgen_write_variable(gen, block, var, result);
    result = create_pred_upsilons(gen, block, phi);
  }

  irgen_write_variable(gen, block, var, result);
  //printf("block %d READ: ", block); print_instr(gen, var, result);
  return result;
}





static void mark_used_instr(IrGen *gen, IrInstrRef ref) {
  IrInstr *instr = gen->code + ref;
  if (instr->flags & IR_FLAG_MARK) {
    return;
  }
  instr->flags |= IR_FLAG_MARK;

  switch (instr->tag) {
    case IR_UPSILON:
      mark_used_instr(gen, instr->arg1);
      break;
    case IR_PHI: {
      Phi *p = get_phi(gen, instr->arg0);
      assert(p->upsilons.values[0]);
      IrInstrRef upsilon;
      list_foreach(upsilon, p->upsilons, gen->list_entries) {
        mark_used_instr(gen, upsilon);
      }
      break;
    }
    case IR_ADD:
    case IR_EQ:
    case IR_NEQ:
      mark_used_instr(gen, instr->arg0);
      mark_used_instr(gen, instr->arg1);
      break;
  }
}

static void replace_dead_code_with_nops(IrGen *gen) {
  for (i32 i = 0; i < gen->numpos; ++i) {
    IrInstr *instr = gen->code + i;
    switch (instr->tag) {
      case IR_LABEL:
      case IR_JUMP:
        instr->flags |= IR_FLAG_MARK;
        break;
      case IR_RET:
      case IR_BRANCH:
      case IR_PRINT:
        instr->flags |= IR_FLAG_MARK;
        mark_used_instr(gen, instr->arg0);
        break;
    }
  }

  for (IrInstrRef ref = -gen->numneg; ref < gen->numpos; ++ref) {
    if (!(gen->code[ref].flags & IR_FLAG_MARK)) {
      gen->code[ref] = (IrInstr) { };
    } else {
      gen->code[ref].flags &= ~IR_FLAG_MARK;
    }
  }
}



#define FIXUP(Id) to_instr(gen, fixup_instr(gen, source, Id))
#define FIXUP_BINOP(EmitFunc) intern_instr(gen, EmitFunc(gen, FIXUP(instr.arg0), FIXUP(instr.arg1)))

static IrInstrRef fixup_instr(IrGen *gen, IrGen *source, IrInstrRef sourceref) {
  IrInstr instr = source->code[sourceref];

  // check if it was already mapped
  if ((instr.flags & IR_FLAG_MARK) && instr.tag == IR_IDENTITY) {
    return instr.arg0;
  }

  IrInstrRef destref = 0;
  switch (instr.tag) {
    case IR_NOP: return 0;
    case IR_IDENTITY: destref = fixup_instr(gen, source, instr.arg0); break;
    case IR_PRINT: irgen_print(gen, FIXUP(instr.arg0)); return 0;
    case IR_LABEL: irgen_label(gen, instr.arg0); return 0;
    case IR_JUMP: assert(0); return 0;
    case IR_BRANCH: assert(0); return 0;
    case IR_RET: irgen_ret(gen, FIXUP(instr.arg0)); return 0;
    case IR_UPSILON: irgen_upsilon(gen, gen->curr_block, instr.arg0, FIXUP(instr.arg1)); return 0;
    case IR_PHI: destref = irgen_phi(gen, instr.arg0, instr.type).arg0; break;
    case IR_CONST: destref = intern_instr(gen, instr); break;
    case IR_ARG: destref = intern_instr(gen, instr); break;
    case IR_ADD: destref = FIXUP_BINOP(irgen_add); break;
    case IR_EQ: destref = FIXUP_BINOP(irgen_eq); break;
    case IR_NEQ: destref = FIXUP_BINOP(irgen_neq); break;
    default: assert(0 && "unknown instruction");
  }

  // store as already mapped using IDENITITY with the MARK flag bit set, and with a reference to the destination instruction.
  source->code[sourceref] = (IrInstr) { .tag = IR_IDENTITY, .flags = IR_FLAG_MARK, .arg0 = destref };
  return destref;
}

static void fixup_block(IrGen *gen, IrGen *source, IrBlockRef block);

static void fixup_jump(IrGen *gen, IrGen *source, IrBlockRef target) {
  IrBlockRef succ = skip_trivial_successors(source, target); // skip through blocks that only jump

  IrInstrRef trivial_exit = get_trivial_exit(source, succ);
  if (trivial_exit) {
    // if the jump target immediately exits, just exit here instead of jumping
    fixup_instr(gen, source, trivial_exit);
    return;
  }

  BasicBlock *s = get_block(source, succ);
  if (!s->preds.values[1]) {
    // we are the single predecessor to our successor - don't emit jump (merge the blocks)
    ++s->first; // skip label
    fixup_block(gen, source, succ);
    --s->first;
    return;
  }

  irgen_jump(gen, succ);
  fixup_block(gen, source, succ);
}

static void fixup_branch(IrGen *gen, IrGen *source, IrInstr branch) {
  IrBlockRef true_succ = skip_trivial_successors(source, branch.arg1);
  IrBlockRef false_succ = skip_trivial_successors(source, branch.arg2);

  IrInstrRef true_exit = get_trivial_exit(source, true_succ);
  IrInstrRef false_exit = get_trivial_exit(source, false_succ);
  if (true_exit && false_exit && source->code[true_exit].u64_repr == source->code[false_exit].u64_repr) {
    // if the two branch blocks just immediately exit the same way, just exit here instead of branching
    fixup_instr(gen, source, true_exit);
    return;
  }

  IrInstr cond = FIXUP(branch.arg0);
  if (cond.tag == IR_CONST) {
    fixup_jump(gen, source, cond.bool_const ? true_succ : false_succ);
    return;
  }

  if (true_succ == false_succ) {
    fixup_jump(gen, source, true_succ);
    return;
  }

  irgen_branch(gen, cond, true_succ, false_succ);
  fixup_block(gen, source, true_succ);
  fixup_block(gen, source, false_succ);
}

static void fixup_block(IrGen *gen, IrGen *source, IrBlockRef block) {
  BasicBlock *b = get_block(source, block);
  assert(b->first);
  assert(b->last);
  if (b->visited) {
    return;
  }
  b->visited = true;

  // fixup all non-terminal instructions in the block
  IrInstrRef sourceref = b->first;
  while (!IR_IS_TERMINAL_INSTR(source->code[sourceref].tag)) {
    assert(sourceref < source->maxpos);
    fixup_instr(gen, source, sourceref++);
  }

  // then fixup and append the suffix instructions
  IrInstrRef ref;
  list_foreach(ref, source->blocks[block].suffix, source->list_entries) {
    fixup_instr(gen, source, ref);
  }

  // then handle the terminal instruction
  IrInstr term = source->code[sourceref];
  switch (term.tag) {
    case IR_JUMP:
      fixup_jump(gen, source, term.arg0);
      break;
    case IR_BRANCH:
      fixup_branch(gen, source, term);
      break;
    case IR_RET:
      fixup_instr(gen, source, sourceref);
      break;
    default:
      assert(0 && "illegal terminating instruction");
      break;
  }
}

void irgen_fixup_ir(IrGen *gen, IrGen *source) {
  irgen_clear(gen);
  fixup_block(gen, source, 1);
  replace_dead_code_with_nops(gen);
}





static void generate_postorder(BasicBlock *blocks, IrBlockRef blockref, IrBlockRef **result) {
  if (blockref) {
    BasicBlock *b = &blocks[blockref];
    if (!b->visited) {
      b->visited = true;
      generate_postorder(blocks, b->succs[0], result);
      generate_postorder(blocks, b->succs[1], result);
      b->postorder = vector_size(*result);
      vector_push(*result, blockref);
    }
  }
}

static void generate_dominator_tree(IrGen *gen) {
  BasicBlock *blocks = gen->blocks;

  IrBlockRef *postorder = NULL;
  generate_postorder(blocks, 1, &postorder);
  int postorder_size = vector_size(postorder);
  assert(postorder[postorder_size - 1] == 1); // entry block is always last in postorder

  IrBlockRef *doms = calloc(1, sizeof(IrBlockRef) * postorder_size);
  for (int i = 0; i < postorder_size; ++i) {
    doms[i] = -1;
  }
  
  doms[blocks[1].postorder] = blocks[1].postorder;
  bool changed = true;
  while (changed) {
    changed = false;
    for (int b = postorder_size - 1; b >= 0; --b) {
      IrBlockRef block = postorder[b];
      if (block == 1) {
        continue; // start node
      }
      int idom = -1;
      IrBlockRef source;
      list_foreach(source, blocks[block].preds, gen->list_entries) {
        int p = blocks[source].postorder;
        if (doms[p] < 0) {
          continue;
        }
        if (idom < 0) {
          idom = p;
        } else {
          int f1 = p;
          int f2 = idom;
          while (f1 != f2) {
            while (f1 < f2) {
              f1 = doms[f1];
            }
            while (f2 < f1) {
              f2 = doms[f2];
            }
          }
          idom = f1;
        }
      }
      if (idom != doms[b]) {
        doms[b] = idom;
        changed = true;
      }
    }
  }
  
  blocks[1].idom = 0;
  blocks[1].dom_depth = 0;
  blocks[1].visited = false;
  for (int i = postorder_size - 2; i >= 0; --i) { // skip the entry block
    BasicBlock *b = &blocks[postorder[i]];
    b->idom = postorder[doms[i]];
    b->dom_depth = blocks[b->idom].dom_depth + 1;
    b->visited = false;
  }

  free(doms);
  vector_free(postorder);
}



typedef struct InstrPlacement {
  IrInstrRef old;
  IrInstrRef new;
} InstrPlacement;

#define NAME placement_sort
#define TYPE InstrPlacement
#define COMPARE(a, b) ((a).new - (b).new)
#include "mergesort.h"

static ListEntry *calculate_instruction_uses(IrGen *gen) {
  ListEntry *uses = (ListEntry *)calloc(gen->numneg + gen->numpos, sizeof(ListEntry)) + gen->numneg;
  for (IrInstrRef ref = -gen->numneg; ref < gen->numpos; ++ref) {
    IrInstr instr = gen->code[ref];    
    int info = instr_info[instr.tag];    
    if (info & IR_INSTR_INFO_ARG0_IS_VALUE) { list_push_unique(ref, &uses[instr.arg0], &gen->list_entries); }
    if (info & IR_INSTR_INFO_ARG1_IS_VALUE) { list_push_unique(ref, &uses[instr.arg1], &gen->list_entries); }
    if (info & IR_INSTR_INFO_ARG2_IS_VALUE) { list_push_unique(ref, &uses[instr.arg2], &gen->list_entries); }
  }
  return uses;
}

static IrBlockRef *calculate_instruction_blocks(IrGen *gen) {
  IrBlockRef *instr_blocks = (IrBlockRef *)calloc(gen->numneg + gen->numpos, sizeof(IrBlockRef)) + gen->numneg;
  IrBlockRef curr_block = 0;
  // only instructions with positive indexes are fixed to a block
  for (IrInstrRef ref = 1; ref < gen->numpos; ++ref) {
    IrInstr instr = gen->code[ref];
    if (instr.tag == IR_LABEL) {
      curr_block = instr.arg0;
    }
    instr_blocks[ref] = curr_block;
  }
  return instr_blocks;
}

static InstrPlacement *calculate_initial_instruction_placement(IrGen *gen) {
  InstrPlacement *placement = (InstrPlacement *)calloc(gen->numneg + gen->numpos, sizeof(InstrPlacement)) + gen->numneg;
  for (IrInstrRef ref = -gen->numneg; ref < gen->numneg; ++ref) {
    placement[ref] = (InstrPlacement) { .old = ref, .new = ref };
  }
  return placement;
}

/*
This function schedules a pure instruction (initially having a negative index) into the main instruction stream 
(positive indices). Pure instructions can be scheduled flexibly as their order doesn't affect program behavior,
unlike side-effecting instructions which must maintain their relative order.

The placement of the instruction must satisfy two key constraints:
1. The instruction must dominate all its uses (determines starting point at nearest common dominator)
2. All dependencies of the instruction must dominate its placement (limits how high we can move)

The algorithm:
1. Find the nearest common dominator of all uses as starting point
2. Walk up dominator tree until hitting a dependency, tracking best loop_depth seen
3. Place at the position with minimal loop_depth found during the walk

Within the chosen block, the instruction is placed immediately after its latest dependency.
*/
static void schedule_pure_instruction(IrGen *gen, IrInstrRef ref, ListEntry *instr_uses, IrBlockRef *instr_blocks, InstrPlacement *placement) {
  ListEntry uses = instr_uses[ref];
  if (!uses.values[0]) {
    gen->code[ref] = (IrInstr) { .tag = IR_NOP }; // eliminate dead code
    return;
  }
  IrInstr instr = gen->code[ref];
  if (instr.tag == IR_CONST) {
    return; // let constants stay unscheduled
  }
  int info = instr_info[instr.tag];

  // Find the nearest common dominator of all uses - this is our lower bound
  // We can't place the instruction below this or it won't dominate all uses
  IrBlockRef lower_bound = 0;
  IrInstrRef use_ref;
  list_foreach(use_ref, uses, gen->list_entries) {
    IrBlockRef use_block = instr_blocks[use_ref];
    if (!lower_bound) {
      lower_bound = use_block;
      continue;
    }

    while (lower_bound != use_block) {
      BasicBlock *lower_bb = &gen->blocks[lower_bound];
      BasicBlock *use_bb = &gen->blocks[use_block];
      
      if (lower_bb->dom_depth > use_bb->dom_depth) {
        lower_bound = lower_bb->idom;
      } else {
        use_block = use_bb->idom;
      }
    }
  }

  IrBlockRef dep_blocks[3] = {0};
  int num_deps = 0;
  if ((info & IR_INSTR_INFO_ARG0_IS_VALUE)) {
    assert(instr.arg0);
    assert(instr_blocks[instr.arg0]);
    assert(placement[instr.arg0].new > 0);
    dep_blocks[num_deps++] = instr_blocks[instr.arg0];
  }
  if ((info & IR_INSTR_INFO_ARG1_IS_VALUE)) {
    assert(instr.arg1);
    assert(instr_blocks[instr.arg1]);
    assert(placement[instr.arg1].new > 0);
    dep_blocks[num_deps++] = instr_blocks[instr.arg1];
  }
  if ((info & IR_INSTR_INFO_ARG2_IS_VALUE)) {
    assert(instr.arg2);
    assert(instr_blocks[instr.arg2]);
    assert(placement[instr.arg2].new > 0);
    dep_blocks[num_deps++] = instr_blocks[instr.arg2];
  }

  // Walk up from lower_bound, and move best_block to a higher block if it has a lower loop depth
  // (while not walking past any dependencies)
  int min_loop_depth = gen->blocks[lower_bound].loop_depth;
  IrBlockRef best_block = lower_bound;
  IrBlockRef candidate = lower_bound;
  while (candidate) {
    // Consider this block for placement first
    BasicBlock *bb = &gen->blocks[candidate];
    if (bb->loop_depth < min_loop_depth) {
      best_block = candidate;
      min_loop_depth = bb->loop_depth;
    }
    // Then check if it's a dependency block - can't go higher
    for (int i = 0; i < num_deps; i++) {
      if (candidate == dep_blocks[i]) {
        goto found_dep;
      }
    }
    candidate = gen->blocks[candidate].idom;
  }
  // Reached root without finding any dependency
  assert(num_deps == 0);
found_dep:

  // Find position after last dependency within the chosen block
  IrInstrRef latest_dep = 0;
  if ((info & IR_INSTR_INFO_ARG0_IS_VALUE) && instr_blocks[instr.arg0] == best_block && placement[instr.arg0].new > latest_dep) {
    latest_dep = instr.arg0;
  }
  if ((info & IR_INSTR_INFO_ARG1_IS_VALUE) && instr_blocks[instr.arg1] == best_block && placement[instr.arg1].new > latest_dep) {
    latest_dep = instr.arg1;
  }
  if ((info & IR_INSTR_INFO_ARG2_IS_VALUE) && instr_blocks[instr.arg2] == best_block && placement[instr.arg2].new > latest_dep) {
    latest_dep = instr.arg2;
  }
  if (latest_dep == 0) {
    latest_dep = gen->blocks[best_block].first;
  }

  placement[ref].new = latest_dep + 1;
  instr_blocks[ref] = best_block;
}


static void schedule_pure_instructions(IrGen *gen) {
  generate_dominator_tree(gen);
  ListEntry *instr_uses = calculate_instruction_uses(gen);
  IrBlockRef *instr_blocks = calculate_instruction_blocks(gen);
  InstrPlacement *placement = calculate_initial_instruction_placement(gen);

  for (int ref = -1; ref >= -gen->numneg; --ref) {
    schedule_pure_instruction(gen, ref, instr_uses, instr_blocks, placement);
  }

  // Reverse the order of instructions at negative indexes, so that dependencies come before dependents
  // (negative index instructions are created in order from -1 to -numneg, so the instruction at -1 has no dependencies,
  // and further instructions depend only on earlier instructions)
  for (int i = -1, j = -gen->numneg; i > j; i--, j++) {
    IrInstr tmp = gen->code[i];
    gen->code[i] = gen->code[j]; 
    gen->code[j] = tmp;
  }

  int count = gen->numneg + gen->numpos;
  InstrPlacement *orig_placement = malloc(count * sizeof(InstrPlacement)) - gen->numneg;
  memcpy(orig_placement - gen->numneg, placement - gen->numneg, count * sizeof(InstrPlacement));

  InstrPlacement *temp = malloc(count * sizeof(InstrPlacement));
  placement_sort(placement - gen->numneg, 0, count - 1, temp);
  free(temp);

  IrInstr *old_code = gen->code;
  IrInstr *new_code = malloc(count * sizeof(IrInstr));
  int out = 0;
  int new_numneg = 0;
  int new_numpos = 0;

  for (IrInstrRef ref = -gen->numneg; ref < gen->numpos; ++ref) {
    IrInstr instr = gen->code[placement[ref].old];
    int info = instr_info[instr.tag];

    if (info & IR_INSTR_INFO_ARG0_IS_VALUE) {
      instr.arg0 = orig_placement[instr.arg0].new;
    }
    if (info & IR_INSTR_INFO_ARG1_IS_VALUE) {
      instr.arg1 = orig_placement[instr.arg1].new;
    }
    if (info & IR_INSTR_INFO_ARG2_IS_VALUE) {
      instr.arg2 = orig_placement[instr.arg2].new;
    }

    new_code[out++] = instr;

    if (placement[ref].new < 0) {
      assert(instr.tag == IR_CONST);
      ++new_numneg;
    } else {
      ++new_numpos;
    }
  }

  free(instr_uses - gen->numneg);
  free(instr_blocks - gen->numneg);
  free(placement - gen->numneg);
  free(orig_placement - gen->numneg);
  free(old_code - gen->numneg);

  assert(out == count);
  assert(new_numneg + new_numpos == count);
  gen->code = new_code;
  gen->numneg = new_numneg;
  gen->numpos = new_numpos;
}




void test_irgen_if(void) {
  IrGen *gen = irgen_create();

  IrVarRef x = irgen_create_variable(gen, TY_I32);
  IrBlockRef entry_block = irgen_create_block(gen);
  IrBlockRef then_block = irgen_create_block(gen);
  IrBlockRef else_block = irgen_create_block(gen);
  IrBlockRef exit_block = irgen_create_block(gen);

  irgen_seal_block(gen, entry_block);
  irgen_label(gen, entry_block);
  irgen_write_variable(gen, entry_block, x, irgen_const_i32(gen, 1));
  irgen_branch(gen, irgen_arg(gen, 0, TY_BOOL), then_block, else_block);
  irgen_seal_block(gen, then_block);
  irgen_seal_block(gen, else_block);

  irgen_label(gen, then_block);
  irgen_write_variable(gen, then_block, x, irgen_add(gen, irgen_read_variable(gen, then_block, x), irgen_add(gen, irgen_const_i32(gen, 5), irgen_const_i32(gen, 5))));
  irgen_jump(gen, exit_block);
  
  irgen_label(gen, else_block);
  irgen_write_variable(gen, else_block, x, irgen_add(gen, irgen_read_variable(gen, else_block, x), irgen_const_i32(gen, 10)));
  irgen_jump(gen, exit_block);
  irgen_seal_block(gen, exit_block);

  irgen_label(gen, exit_block);
  irgen_ret(gen, irgen_read_variable(gen, exit_block, x));

  IrGen *fixed = irgen_create();
  irgen_fixup_ir(fixed, gen);
  irgen_fixup_ir(gen, fixed);
  irgen_fixup_ir(fixed, gen);
  
  irgen_print_ir(fixed);

  irgen_destroy(gen);
  irgen_destroy(fixed);
}

void test_irgen_merge_consecutive(void) {
  IrGen *gen = irgen_create();

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

  IrGen *fixed = irgen_create();
  irgen_fixup_ir(fixed, gen);
  
  irgen_print_ir(fixed);

  irgen_destroy(gen);
  irgen_destroy(fixed);
}

void test_irgen_while(void) {
  IrGen *gen = irgen_create();

  IrVarRef x = irgen_create_variable(gen, TY_I32);
  IrBlockRef entry_block = irgen_create_block(gen);
  IrBlockRef cond_block = irgen_create_block(gen);
  IrBlockRef body_block = irgen_create_block(gen);
  IrBlockRef exit_block = irgen_create_block(gen);

  irgen_seal_block(gen, entry_block);
  irgen_label(gen, entry_block);
  irgen_write_variable(gen, entry_block, x, irgen_const_i32(gen, 0));
  irgen_jump(gen, cond_block);

  irgen_label(gen, cond_block);
  irgen_branch(gen, irgen_neq(gen, irgen_read_variable(gen, cond_block, x), irgen_const_i32(gen, 10)), body_block, exit_block);
  irgen_seal_block(gen, exit_block);

  irgen_seal_block(gen, body_block);
  irgen_label(gen, body_block);
  irgen_write_variable(gen, body_block, x, irgen_add(gen, irgen_read_variable(gen, body_block, x), irgen_const_i32(gen, 1)));
  irgen_jump(gen, cond_block);
  irgen_seal_block(gen, cond_block);

  irgen_label(gen, exit_block);
  irgen_ret(gen, irgen_read_variable(gen, exit_block, x));

  IrGen *fixed = irgen_create();
  irgen_fixup_ir(fixed, gen);
  
  irgen_print_ir(fixed);

  irgen_destroy(gen);
  irgen_destroy(fixed);
}

void test_nested_while_loops(void) {
  IrGen *gen = irgen_create();

  // Create variables for outer and inner loop counters
  IrVarRef i = irgen_create_variable(gen, TY_I32);
  IrVarRef j = irgen_create_variable(gen, TY_I32);

  // Create all the blocks we'll need
  IrBlockRef entry_block = irgen_create_block(gen);
  IrBlockRef outer_cond_block = irgen_create_block(gen);
  IrBlockRef outer_body_block = irgen_create_block(gen);
  IrBlockRef inner_cond_block = irgen_create_block(gen);
  IrBlockRef inner_body_block = irgen_create_block(gen);
  IrBlockRef exit_block = irgen_create_block(gen);

  // Entry block: initialize i = 0
  irgen_seal_block(gen, entry_block);
  irgen_label(gen, entry_block);
  irgen_write_variable(gen, entry_block, i, irgen_const_i32(gen, 0));
  irgen_jump(gen, outer_cond_block);

  // Outer condition block: while i < 3
  irgen_label(gen, outer_cond_block);
  irgen_branch(gen, 
    irgen_neq(gen, irgen_read_variable(gen, outer_cond_block, i), irgen_const_i32(gen, 3)),
    outer_body_block,
    exit_block
  );
  irgen_seal_block(gen, outer_body_block);
  irgen_seal_block(gen, exit_block);

  // Outer body block: initialize j = 0
  irgen_label(gen, outer_body_block);
  irgen_write_variable(gen, outer_body_block, j, irgen_const_i32(gen, 0));
  irgen_jump(gen, inner_cond_block);

  // Inner condition block: while j < 2
  irgen_label(gen, inner_cond_block);
  irgen_branch(gen,
    irgen_neq(gen, irgen_read_variable(gen, inner_cond_block, j), irgen_const_i32(gen, 2)),
    inner_body_block,
    outer_cond_block
  );
  irgen_seal_block(gen, inner_body_block);
  irgen_seal_block(gen, outer_cond_block);

  // Inner body block: print i,j and increment j
  irgen_label(gen, inner_body_block);
  irgen_print(gen, irgen_read_variable(gen, inner_body_block, i));
  irgen_print(gen, irgen_read_variable(gen, inner_body_block, j));
  irgen_write_variable(gen, inner_body_block, j,
    irgen_add(gen, irgen_read_variable(gen, inner_body_block, j), irgen_const_i32(gen, 1))
  );
  irgen_jump(gen, inner_cond_block);
  irgen_seal_block(gen, inner_cond_block);

  // After inner loop completes, increment i and continue outer loop
  irgen_write_variable(gen, outer_body_block, i,
    irgen_add(gen, irgen_read_variable(gen, outer_body_block, i), irgen_const_i32(gen, 1))
  );

  // Exit block: return final value of i
  irgen_label(gen, exit_block);
  irgen_ret(gen, irgen_read_variable(gen, exit_block, i));

  IrGen *fixed = irgen_create();
  irgen_fixup_ir(fixed, gen);
  
  irgen_print_ir(fixed);

  generate_dominator_tree(fixed);
  printf("\nDominator tree:\n");
  for (int i = 1; i < vector_size(fixed->blocks); ++i) {
    BasicBlock *b = &fixed->blocks[i];
    printf("Block %d: idom = %d, depth = %d\n", i, b->idom, b->dom_depth);
  }

  irgen_destroy(gen);
  irgen_destroy(fixed);
}
