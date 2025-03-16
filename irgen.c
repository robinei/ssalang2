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
  u16 succs[2];
  ListEntry preds;
  ListEntry incomplete_phis;
  ListEntry suffix;
} BasicBlock;

typedef struct Phi {
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

static BasicBlock *get_block(IrGen *gen, IrBlockRef block) {
  assert(block);
  while (vector_size(gen->blocks) <= block) {
    irgen_create_block(gen);
  }
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
  BasicBlock *s = get_block(gen, block);
  if (s->last == s->first + 1 && !s->suffix.values[0]) {
    if (gen->code[s->last].tag == IR_RET) {
      return s->last;
    }
  }
  return 0;
}

IrPhiRef irgen_create_phi(IrGen *gen) {
  i32 phi = vector_size(gen->phis);
  assert(phi < INT16_MAX);
  vector_push(gen->phis, (Phi) { });
  return phi;
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
  vector_free(gen->upsilons);
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
  vector_clear(gen->upsilons);
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
  if (block == gen->curr_block && !get_block(gen, block)->last) {
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
  printf("label %d\n", block);
  assert(!gen->curr_block || get_block(gen, gen->curr_block)->last);
  BasicBlock *b = get_block(gen, block);
  assert(!b->first);
  assert(!b->last);
  assert(!b->succs[0]);
  assert(!b->succs[1]);
  IrInstrRef ref = emit_instr_pinned(gen, (IrInstr) { .tag = IR_LABEL, .arg0 = block });
  b->first = ref;
  gen->curr_block = block;
}

void irgen_jump(IrGen *gen, IrBlockRef target) {
  BasicBlock *b = get_block(gen, gen->curr_block);
  BasicBlock *tb = get_block(gen, target);
  assert(!b->last);
  assert(!b->succs[0]);
  assert(!tb->sealed);
  b->succs[0] = target;
  list_push(gen->curr_block, &tb->preds, &gen->list_entries);
  b->last = emit_instr_pinned(gen, (IrInstr) { .tag = IR_JUMP, .arg0 = target });
}

void irgen_branch(IrGen *gen, IrInstr cond, IrBlockRef true_target, IrBlockRef false_target) {
  BasicBlock *b = get_block(gen, gen->curr_block);
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
  BasicBlock *b = get_block(gen, gen->curr_block);
  assert(!b->last);
  IrInstr instr = (IrInstr) { .tag = IR_RET, .arg0 = intern_instr(gen, retval) };
  b->last = emit_instr_pinned(gen, instr);
}

void irgen_upsilon(IrGen *gen, IrBlockRef block, IrPhiRef phi, IrInstr val) {
  // upsilons may be emitted even after a block is filled (recursive variable lookup will create upsilons in predecessor blocks as needed)
  if (phi) {
    IrInstrRef ref = append_block_instr(gen, block, (IrInstr) { .tag = IR_UPSILON, .arg0 = phi, .arg1 = intern_instr(gen, val) });
    vector_push(gen->upsilons, ref);
    Phi *p = get_phi(gen, phi);
    list_push(ref, &p->upsilons, &gen->list_entries);
  }
}

IrInstr irgen_const_bool(IrGen *gen, bool val) {
  return (IrInstr) { .tag = IR_CONST, .type = TY_BOOL, .bool_const = val };
}

IrInstr irgen_const_i32(IrGen *gen, i32 val) {
  return (IrInstr) { .tag = IR_CONST, .type = TY_I32, .i32_const = val };
}

IrInstr irgen_phi(IrGen *gen, IrPhiRef phi, IrType type, IrVarRef var) {
  if (phi) {
    assert(type != TY_VOID);
    IrInstrRef ref = emit_instr_pinned(gen, (IrInstr) { .tag = IR_PHI, .type = type, .arg0 = phi, .arg1 = var });
    return (IrInstr) { .tag = IR_IDENTITY, .type = type, .arg0 = ref };
  } else {
    assert(type == TY_VOID);
    return (IrInstr) { };
  }
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




static IrInstr create_pred_upsilons(IrGen *gen, IrBlockRef block, IrInstr phi) {
  IrBlockRef preds[32];
  IrInstr values[32];
  int num_values = 0;

  bool found_different = false;
  IrInstr candidate = (IrInstr) { };
  IrVarRef var = phi.arg1;
  BasicBlock *b = get_block(gen, block);

  IrBlockRef pred;
  list_foreach(pred, b->preds, gen->list_entries) {
    IrInstr val = irgen_read_variable(gen, pred, var);
    
    assert(num_values < 32);
    preds[num_values] = pred;
    values[num_values++] = val;

    if (val.u64_repr != phi.u64_repr) {
      if (!candidate.u64_repr) {
        candidate = val;
      } else if (val.u64_repr != candidate.u64_repr) {
        found_different = true;
      }
    }
  }

  assert(num_values > 1);
  if (candidate.u64_repr && !found_different) {
    IrInstrRef phiref = try_lookup_instr(gen, phi);
    if (phiref) {
      gen->code[phiref] = (IrInstr) { .tag = IR_IDENTITY, .type = candidate.type, .arg0 = intern_instr(gen, candidate) };
    }
    return candidate;
  }

  for (int i = 0; i < num_values; ++i) {
    irgen_upsilon(gen, preds[i], phi.arg0, values[i]);
  }
  return phi;
}

void irgen_seal_block(IrGen *gen, IrBlockRef block) {
  printf("sealing %d\n", block);
  BasicBlock *b = get_block(gen, block);
  assert(!b->sealed);
  b->sealed = true;
  IrInstrRef phi;
  list_foreach(phi, b->incomplete_phis, gen->list_entries) {
    create_pred_upsilons(gen, block, gen->code[phi]);
  }
}

typedef union {
  struct { i16 block, var; };
  i32 u32_repr;
} BindingKey;

void irgen_write_variable(IrGen *gen, IrBlockRef block, IrVarRef var, IrInstr val) {
  assert(block);
  assert(var);
  BasicBlock *b = get_block(gen, block);
  printf("block %d (%s) WRITE: ", block, b->sealed ? "sealed" : "unsealed"); print_instr(gen, var, val);
  assert(val.type != TY_VOID);
  BindingKey key = {{ block, var }};
  u32_to_u64_put(&gen->bindings, key.u32_repr, val.u64_repr);
}

IrInstr irgen_read_variable(IrGen *gen, IrBlockRef block, IrVarRef var) {
  assert(block);
  assert(var);
  BindingKey key = {{ block, var }};
  BasicBlock *b = get_block(gen, block);
  IrInstr found;
  if (u32_to_u64_get(&gen->bindings, key.u32_repr, &found.u64_repr)) {
    printf("block %d (%s) READ: ", block, b->sealed ? "sealed" : "unsealed"); print_instr(gen, var, found);
    return found;
  }

  IrInstr result;
  if (!b->sealed) {
    IrInstr phi = irgen_phi(gen, irgen_create_phi(gen), gen->vars[var].type, var);
    list_push(intern_instr(gen, phi), &b->incomplete_phis, &gen->list_entries);
    result = phi;
  } else if (b->preds.values[0] && !b->preds.values[1]) {
    // exactly one predecessor
    result = irgen_read_variable(gen, b->preds.values[0], var);
  } else {
    IrInstr phi = irgen_phi(gen, irgen_create_phi(gen), gen->vars[var].type, var);
    irgen_write_variable(gen, block, var, phi);
    result = create_pred_upsilons(gen, block, phi);
  }

  irgen_write_variable(gen, block, var, result);
  printf("block %d (%s) READ: ", block, b->sealed ? "sealed" : "unsealed"); print_instr(gen, var, result);
  return result;
}




static IrInstrRef fixup_instr(IrGen *gen, IrGen *source, i16 *map, IrInstrRef sourceref) {
  if (map[sourceref]) {
    return map[sourceref];
  }

#define FIXUP(Id) to_instr(gen, fixup_instr(gen, source, map, Id))
#define FIXUP_BINOP(EmitFunc) intern_instr(gen, EmitFunc(gen, FIXUP(instr.arg0), FIXUP(instr.arg1)))

  IrInstrRef dest_id = 0;
  IrInstr instr = source->code[sourceref];
  switch (instr.tag) {
    case IR_NOP: return 0;
    case IR_IDENTITY: dest_id = fixup_instr(gen, source, map, instr.arg0); break;
    case IR_LABEL: irgen_label(gen, instr.arg0); return 0;
    case IR_JUMP: irgen_jump(gen, instr.arg0); return 0;
    case IR_BRANCH: irgen_branch(gen, FIXUP(instr.arg0), instr.arg1, instr.arg2); return 0;
    case IR_RET: irgen_ret(gen, FIXUP(instr.arg0)); return 0;
    case IR_UPSILON: irgen_upsilon(gen, gen->curr_block, instr.arg0, FIXUP(instr.arg1)); return 0;
    case IR_CONST: dest_id = intern_instr(gen, instr); break;
    case IR_PHI: dest_id = irgen_phi(gen, instr.arg0, instr.type, instr.arg1).arg0; break;
    case IR_ARG: dest_id = intern_instr(gen, instr); break;
    case IR_ADD: dest_id = FIXUP_BINOP(irgen_add); break;
    case IR_EQ: dest_id = FIXUP_BINOP(irgen_eq); break;
    case IR_NEQ: dest_id = FIXUP_BINOP(irgen_neq); break;
    default: assert(0 && "unknown instruction");
  }

  map[sourceref] = dest_id;
  return dest_id;
}

static void fixup_block(IrGen *gen, IrGen *source, i16 *map, IrBlockRef block) {
  BasicBlock *b = get_block(source, block);
  if (b->visited) {
    return;
  }
  b->visited = true;
  assert(b->first);
  assert(b->last);
  assert(b->sealed);

  // fixup all non-terminal instructions in the block
  IrInstrRef sourceref = b->first;
  while (!IR_IS_TERMINAL_INSTR(source->code[sourceref].tag)) {
    assert(sourceref < source->maxpos);
    fixup_instr(gen, source, map, sourceref++);
  }

  // then fixup and append the suffix instructions
  IrInstrRef ref;
  list_foreach(ref, b->suffix, source->list_entries) {
    fixup_instr(gen, source, map, ref);
  }

  // then handle the terminal instruction
  IrInstr term = source->code[sourceref];
  switch (term.tag) {
    case IR_JUMP: {
      IrBlockRef succ = skip_trivial_successors(source, term.arg0); // skip through blocks that only jump
      IrInstrRef trivial_exit = get_trivial_exit(source, succ);
      if (trivial_exit) {
        // if the jump target just immediately exist, just exit here instead of jumping
        fixup_instr(gen, source, map, trivial_exit);
      } else {
        irgen_jump(gen, succ);
        fixup_block(gen, source, map, succ);
      }
      break;
    }
    case IR_BRANCH: {
      IrBlockRef true_succ = skip_trivial_successors(source, term.arg1);
      IrBlockRef false_succ = skip_trivial_successors(source, term.arg2);
      IrInstrRef true_exit = get_trivial_exit(source, true_succ);
      IrInstrRef false_exit = get_trivial_exit(source, false_succ);
      if (true_exit && false_exit && source->code[true_exit].u64_repr == source->code[false_exit].u64_repr) {
        // if the two branch blocks just immediately exit the same way, just exit here instead of branching
        fixup_instr(gen, source, map, true_exit);
      } else {
        irgen_branch(gen, gen->code[fixup_instr(gen, source, map, term.arg0)], true_succ, false_succ);
        fixup_block(gen, source, map, true_succ);
        fixup_block(gen, source, map, false_succ);
      }
      break;
    }
    default:
      fixup_instr(gen, source, map, sourceref);
      break;
  }

  get_block(gen, block)->sealed = true;
}

void irgen_fixup_ir(IrGen *gen, IrGen *source) {
  i16 map_buffer[65536] = { 0, };
  i16 *map = map_buffer + 32768; // map from old to new IrInstrRef
  irgen_clear(gen);
  fixup_block(gen, source, map, 1);
}

