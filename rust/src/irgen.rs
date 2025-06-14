use std::collections::HashMap;
use crate::ir::{Instr, Meta, Type, InstrRef, BlockRef, PhiRef, VarRef, Operand};
use crate::bivec::BiVec;

#[derive(Debug)]
struct Variable {
    ty: Type,
}

#[derive(Debug)]
struct BasicBlock {
    sealed: bool,
    visited: bool,
    first: Option<InstrRef>,
    last: Option<InstrRef>,
    idom: Option<BlockRef>,
    dom_depth: u16,
    postorder: u16,
    loop_depth: u16,
    succs: [Option<BlockRef>; 2],
    preds: Vec<Operand>,
    incomplete_phis: Vec<Operand>,
    suffix: Vec<Operand>,
}

impl BasicBlock {
    fn new() -> Self {
        Self {
            sealed: false,
            visited: false,
            first: None,
            last: None,
            idom: None,
            dom_depth: 0,
            postorder: 0,
            loop_depth: 0,
            succs: [None; 2],
            preds: Vec::new(),
            incomplete_phis: Vec::new(),
            suffix: Vec::new(),
        }
    }
}

#[derive(Debug)]
struct Phi {
    var: Option<VarRef>,
    instr: Option<InstrRef>,
    upsilons: Vec<Operand>,
}

impl Phi {
    fn new() -> Self {
        Self {
            var: None,
            instr: None,
            upsilons: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct IrGen {
    code: BiVec<Instr>,
    curr_block: Option<BlockRef>,
    blocks: Vec<BasicBlock>,
    vars: Vec<Variable>,
    phis: Vec<Phi>,
    bindings: HashMap<u32, u64>,
    ircache: HashMap<u64, InstrRef>,
}

impl IrGen {
    pub fn new() -> Self {
        let mut gen = Self {
            code: BiVec::with_capacity(16, 16),
            curr_block: None,
            blocks: Vec::new(),
            vars: Vec::new(),
            phis: Vec::new(),
            bindings: HashMap::new(),
            ircache: HashMap::new(),
        };
        
        // Reserve slot 0 for NOP
        gen.code.set_with_default(0, Instr::Nop(Meta::new(Type::Void)));
        
        // Reserve index 0 for unused entries
        gen.create_block(); // block at index 0 should not be used
        gen.create_variable(Type::Void); // var at index 0 should not be used
        gen.create_phi(); // phi at index 0 should not be used
        
        gen
    }

    pub fn clear(&mut self) {
        self.code = BiVec::with_capacity(16, 16);
        self.code.set_with_default(0, Instr::Nop(Meta::new(Type::Void)));
        self.curr_block = None;
        self.blocks.clear();
        self.vars.clear();
        self.phis.clear();
        self.bindings.clear();
        self.ircache.clear();
        
        // Reserve index 0 for unused entries
        self.create_block();
        self.create_variable(Type::Void);
        self.create_phi();
    }

    pub fn create_block(&mut self) -> BlockRef {
        let block_id = self.blocks.len() as Operand;
        self.blocks.push(BasicBlock::new());
        BlockRef(block_id)
    }

    fn get_or_create_block(&mut self, block: BlockRef) -> &mut BasicBlock {
        while self.blocks.len() <= block.0 as usize {
            self.create_block();
        }
        &mut self.blocks[block.0 as usize]
    }

    fn get_block(&self, block: BlockRef) -> &BasicBlock {
        &self.blocks[block.0 as usize]
    }

    pub fn create_variable(&mut self, ty: Type) -> VarRef {
        let var_id = self.vars.len() as Operand;
        self.vars.push(Variable { ty });
        VarRef(var_id)
    }

    pub fn create_phi(&mut self) -> PhiRef {
        let phi_id = self.phis.len() as Operand;
        self.phis.push(Phi::new());
        PhiRef(phi_id)
    }

    fn create_phi_var(&mut self, var: VarRef) -> PhiRef {
        let phi_id = self.phis.len() as Operand;
        let mut phi = Phi::new();
        phi.var = Some(var);
        self.phis.push(phi);
        PhiRef(phi_id)
    }

    fn get_phi(&mut self, phi: PhiRef) -> &mut Phi {
        while self.phis.len() <= phi.0 as usize {
            self.create_phi();
        }
        &mut self.phis[phi.0 as usize]
    }

    fn emit_instr_pinned(&mut self, instr: Instr) -> InstrRef {
        let ref_val = self.code.positive_count() as Operand;
        self.code.push(instr);
        InstrRef(ref_val)
    }

    fn try_lookup_instr(&self, instr: &Instr) -> Option<InstrRef> {
        // Create a hash representation of the instruction
        let hash = self.hash_instr(instr);
        self.ircache.get(&hash).copied()
    }

    fn hash_instr(&self, instr: &Instr) -> u64 {
        // Simple hash based on instruction discriminant and operands
        // This is a simplified version - in practice you'd want a proper hash
        match instr {
            Instr::ConstBool(meta, val) => {
                (meta.get_type() as u64) << 32 | (*val as u64)
            }
            Instr::ConstI32(meta, val) => {
                (meta.get_type() as u64) << 32 | (*val as u32 as u64)
            }
            Instr::Arg(meta, arg) => {
                (meta.get_type() as u64) << 32 | (*arg as u32 as u64) | (0x1000u64 << 32)
            }
            Instr::Add(meta, lhs, rhs) => {
                (meta.get_type() as u64) << 48 | (lhs.0 as u32 as u64) << 16 | (rhs.0 as u32 as u64) | (0x2000u64 << 32)
            }
            Instr::Eq(meta, lhs, rhs) => {
                (meta.get_type() as u64) << 48 | (lhs.0 as u32 as u64) << 16 | (rhs.0 as u32 as u64) | (0x3000u64 << 32)
            }
            Instr::Neq(meta, lhs, rhs) => {
                (meta.get_type() as u64) << 48 | (lhs.0 as u32 as u64) << 16 | (rhs.0 as u32 as u64) | (0x4000u64 << 32)
            }
            _ => 0, // Non-pure instructions don't get cached
        }
    }

    fn emit_instr_unpinned(&mut self, instr: Instr) -> InstrRef {
        if instr.is_pure() {
            if let Some(cached) = self.try_lookup_instr(&instr) {
                return cached;
            }
        }

        let ref_val = -(self.code.negative_count() as Operand + 1);
        self.code.push_front(instr);
        
        if instr.is_pure() {
            let hash = self.hash_instr(&instr);
            self.ircache.insert(hash, InstrRef(ref_val));
        }
        
        InstrRef(ref_val)
    }

    fn append_block_instr(&mut self, block: BlockRef, instr: Instr) -> InstrRef {
        let is_current_block = self.curr_block == Some(block);
        
        // Check if we can emit directly as pinned
        let can_emit_pinned = {
            let b = self.get_or_create_block(block);
            is_current_block && b.last.is_none()
        };
        
        if can_emit_pinned {
            assert!(!instr.is_pure());
            return self.emit_instr_pinned(instr);
        }
        
        let instr_ref = self.emit_instr_unpinned(instr);
        
        // Now update the suffix list
        let b = self.get_or_create_block(block);
        b.suffix.push(instr_ref.0);
        
        instr_ref
    }

    fn to_instr(&self, instr_ref: InstrRef) -> Instr {
        if instr_ref.0 == 0 {
            return Instr::Nop(Meta::new(Type::Void));
        }
        
        let instr = self.code[instr_ref.0 as isize];
        match instr {
            Instr::ConstBool(..) | Instr::ConstI32(..) | Instr::Identity(..) => instr,
            _ => Instr::Identity(Meta::new(instr.get_type()), instr_ref),
        }
    }

    fn intern_instr(&mut self, instr: Instr) -> InstrRef {
        match instr {
            Instr::Identity(_, instr_ref) => instr_ref,
            _ => {
                assert!(instr.is_pure());
                self.emit_instr_unpinned(instr)
            }
        }
    }

    pub fn print(&mut self, val: Instr) {
        let instr_ref = self.intern_instr(val);
        self.emit_instr_pinned(Instr::Print(Meta::new(Type::Void), instr_ref));
    }

    pub fn label(&mut self, block: BlockRef) {
        assert!(self.curr_block.is_none() || self.get_block(self.curr_block.unwrap()).last.is_some());
        
        let b = self.get_or_create_block(block);
        assert!(b.first.is_none());
        assert!(b.last.is_none());
        assert!(b.succs[0].is_none());
        assert!(b.succs[1].is_none());
        
        let instr_ref = self.emit_instr_pinned(Instr::Label(Meta::new(Type::Void), block));
        let b = self.get_or_create_block(block);
        b.first = Some(instr_ref);
        self.curr_block = Some(block);
    }

    pub fn jump(&mut self, target: BlockRef) {
        self.get_or_create_block(target);
        
        let curr_block = self.curr_block.expect("No current block for jump");
        
        // Validate current block state
        {
            let b = self.get_or_create_block(curr_block);
            assert!(b.last.is_none());
            assert!(b.succs[0].is_none());
        }
        
        // Validate target block state
        {
            let tb = self.get_or_create_block(target);
            assert!(!tb.sealed);
        }
        
        // Update current block successor
        {
            let b = self.get_or_create_block(curr_block);
            b.succs[0] = Some(target);
        }
        
        // Update target block predecessor
        {
            let tb = self.get_or_create_block(target);
            tb.preds.push(curr_block.0);
        }
        
        // Emit jump instruction and update current block
        let jump_instr = self.emit_instr_pinned(Instr::Jump(Meta::new(Type::Void), target));
        let b = self.get_or_create_block(curr_block);
        b.last = Some(jump_instr);
    }

    pub fn branch(&mut self, cond: Instr, true_target: BlockRef, false_target: BlockRef) {
        self.get_or_create_block(true_target);
        self.get_or_create_block(false_target);
        
        let curr_block = self.curr_block.expect("No current block for branch");
        
        // Validate state
        {
            let b = self.get_or_create_block(curr_block);
            assert!(b.last.is_none());
            assert!(b.succs[0].is_none());
            assert!(b.succs[1].is_none());
        }
        
        {
            let tb = self.get_or_create_block(true_target);
            assert!(!tb.sealed);
        }
        {
            let fb = self.get_or_create_block(false_target);
            assert!(!fb.sealed);
        }
        
        assert_eq!(cond.get_type(), Type::Bool);
        
        // Optimize constant branches
        if let Instr::ConstBool(_, val) = cond {
            self.jump(if val { true_target } else { false_target });
            return;
        }
        
        // Optimize same target branches
        if true_target.0 == false_target.0 {
            self.jump(true_target);
            return;
        }
        
        // Update successors
        {
            let b = self.get_or_create_block(curr_block);
            b.succs[0] = Some(true_target);
            b.succs[1] = Some(false_target);
        }
        
        // Update predecessors
        {
            let tb = self.get_or_create_block(true_target);
            tb.preds.push(curr_block.0);
        }
        {
            let fb = self.get_or_create_block(false_target);
            fb.preds.push(curr_block.0);
        }
        
        let cond_ref = self.intern_instr(cond);
        let branch_instr = self.emit_instr_pinned(Instr::Branch(Meta::new(Type::Void), cond_ref, true_target, false_target));
        let b = self.get_or_create_block(curr_block);
        b.last = Some(branch_instr);
    }

    pub fn ret(&mut self, retval: Instr) {
        let curr_block = self.curr_block.expect("No current block for ret");
        
        // Validate state
        {
            let b = self.get_or_create_block(curr_block);
            assert!(b.last.is_none());
        }
        
        let retval_ref = self.intern_instr(retval);
        let ret_instr = self.emit_instr_pinned(Instr::Ret(Meta::new(Type::Void), retval_ref));
        let b = self.get_or_create_block(curr_block);
        b.last = Some(ret_instr);
    }

    pub fn upsilon(&mut self, block: BlockRef, phi: PhiRef, val: Instr) {
        if phi.0 != 0 {
            let val_ref = self.intern_instr(val);
            let instr_ref = self.append_block_instr(
                block,
                Instr::Upsilon(Meta::new(Type::Void), phi, val_ref)
            );
            
            // Update phi upsilons list
            let p = self.get_phi(phi);
            p.upsilons.push(instr_ref.0);
        }
    }

    pub fn phi(&mut self, phi: PhiRef, ty: Type) -> Instr {
        if phi.0 != 0 {
            let p = self.get_phi(phi);
            assert_ne!(ty, Type::Void);
            assert!(p.instr.is_none());
            
            let instr_ref = self.emit_instr_pinned(Instr::Phi(Meta::new(ty), phi));
            let p = self.get_phi(phi);
            p.instr = Some(instr_ref);
            
            Instr::Identity(Meta::new(ty), instr_ref)
        } else {
            assert_eq!(ty, Type::Void);
            Instr::Nop(Meta::new(Type::Void))
        }
    }

    pub fn const_bool(&self, val: bool) -> Instr {
        Instr::ConstBool(Meta::new(Type::Bool), val)
    }

    pub fn const_i32(&self, val: i32) -> Instr {
        Instr::ConstI32(Meta::new(Type::I32), val)
    }

    pub fn arg(&self, arg: Operand, ty: Type) -> Instr {
        assert!(arg >= 0);
        assert_ne!(ty, Type::Void);
        Instr::Arg(Meta::new(ty), arg)
    }

    pub fn add(&mut self, lhs: Instr, rhs: Instr) -> Instr {
        assert_eq!(lhs.get_type(), rhs.get_type());
        
        // Constant folding
        if let (Instr::ConstI32(_, lval), Instr::ConstI32(_, rval)) = (lhs, rhs) {
            return Instr::ConstI32(Meta::new(Type::I32), lval.wrapping_add(rval));
        }
        
        let lhs_ref = self.intern_instr(lhs);
        let rhs_ref = self.intern_instr(rhs);
        Instr::Add(Meta::new(lhs.get_type()), lhs_ref, rhs_ref)
    }

    pub fn eq(&mut self, lhs: Instr, rhs: Instr) -> Instr {
        assert_eq!(lhs.get_type(), rhs.get_type());
        
        // Constant folding
        match (lhs, rhs) {
            (Instr::ConstBool(_, lval), Instr::ConstBool(_, rval)) => {
                return Instr::ConstBool(Meta::new(Type::Bool), lval == rval);
            }
            (Instr::ConstI32(_, lval), Instr::ConstI32(_, rval)) => {
                return Instr::ConstBool(Meta::new(Type::Bool), lval == rval);
            }
            _ => {}
        }
        
        let lhs_ref = self.intern_instr(lhs);
        let rhs_ref = self.intern_instr(rhs);
        Instr::Eq(Meta::new(Type::Bool), lhs_ref, rhs_ref)
    }

    pub fn neq(&mut self, lhs: Instr, rhs: Instr) -> Instr {
        assert_eq!(lhs.get_type(), rhs.get_type());
        
        // Constant folding
        match (lhs, rhs) {
            (Instr::ConstBool(_, lval), Instr::ConstBool(_, rval)) => {
                return Instr::ConstBool(Meta::new(Type::Bool), lval != rval);
            }
            (Instr::ConstI32(_, lval), Instr::ConstI32(_, rval)) => {
                return Instr::ConstBool(Meta::new(Type::Bool), lval != rval);
            }
            _ => {}
        }
        
        let lhs_ref = self.intern_instr(lhs);
        let rhs_ref = self.intern_instr(rhs);
        Instr::Neq(Meta::new(Type::Bool), lhs_ref, rhs_ref)
    }

    pub fn seal_block(&mut self, block: BlockRef) {
        {
            let b = self.get_block(block);
            assert!(!b.sealed);
        }
        
        // Get incomplete phis before sealing
        let incomplete_phis: Vec<_> = {
            let b = self.get_or_create_block(block);
            b.incomplete_phis.clone()
        };
        
        // Now seal the block
        {
            let b = self.get_or_create_block(block);
            b.sealed = true;
        }
        
        // Process incomplete phis
        for phi_ref in incomplete_phis {
            self.create_pred_upsilons(block, PhiRef(phi_ref));
        }
    }

    fn create_pred_upsilons(&mut self, block: BlockRef, phi: PhiRef) -> Instr {
        let mut preds = Vec::new();
        let mut values = Vec::new();
        
        assert!(block.0 != 0);
        assert!(phi.0 != 0);
        
        let var = self.phis[phi.0 as usize].var.expect("Phi should have associated variable");
        let phi_instr_ref = self.phis[phi.0 as usize].instr.expect("Phi should have instruction");
        let phi_instr = self.to_instr(phi_instr_ref);
        
        let mut found_different = false;
        let mut candidate = Instr::Nop(Meta::new(Type::Void));
        
        // Collect values from all predecessors
        let pred_list = self.blocks[block.0 as usize].preds.clone();
        for pred_ref in pred_list {
            let pred = BlockRef(pred_ref);
            let val = self.read_variable(pred, var);
            
            preds.push(pred);
            values.push(val);
            
            let phi_hash = self.hash_instr(&phi_instr);
            let val_hash = self.hash_instr(&val);
            if val_hash != phi_hash {
                if candidate.get_type() == Type::Void {
                    candidate = val;
                } else if self.hash_instr(&candidate) != val_hash {
                    found_different = true;
                }
            }
        }
        
        assert!(values.len() > 1);
        if candidate.get_type() != Type::Void && !found_different {
            // All values are the same, replace phi with identity
            let candidate_ref = self.intern_instr(candidate);
            self.code[phi_instr_ref.0 as isize] = Instr::Identity(Meta::new(candidate.get_type()), candidate_ref);
            return candidate;
        }
        
        // Create upsilons for all predecessors
        for (pred, val) in preds.into_iter().zip(values.into_iter()) {
            self.upsilon(pred, phi, val);
        }
        
        phi_instr
    }

    pub fn write_variable(&mut self, block: BlockRef, var: VarRef, val: Instr) {
        assert!(block.0 != 0);
        assert!(var.0 != 0);
        assert_ne!(val.get_type(), Type::Void);
        assert_eq!(val.get_type(), self.vars[var.0 as usize].ty);
        
        let key = ((block.0 as u32) << 16) | (var.0 as u32);
        let val_hash = self.hash_instr(&val);
        self.bindings.insert(key, val_hash);
    }

    pub fn read_variable(&mut self, block: BlockRef, var: VarRef) -> Instr {
        assert!(block.0 != 0);
        assert!(var.0 != 0);
        assert_ne!(self.vars[var.0 as usize].ty, Type::Void);
        
        let key = ((block.0 as u32) << 16) | (var.0 as u32);
        
        if let Some(val_hash) = self.bindings.get(&key) {
            // Find instruction by hash (simplified - should use proper reverse lookup)
            for (_idx, instr) in self.code.iter_with_indices() {
                if self.hash_instr(instr) == *val_hash {
                    return *instr;
                }
            }
            // Fallback for constants
            return self.reconstruct_instr_from_hash(*val_hash);
        }
        
        let var_type = self.vars[var.0 as usize].ty;
        let is_sealed = self.blocks[block.0 as usize].sealed;
        
        let result = if !is_sealed {
            // Create incomplete phi
            let phi = self.create_phi_var(var);
            {
                let b = self.get_or_create_block(block);
                b.incomplete_phis.push(phi.0);
            }
            self.phi(phi, var_type)
        } else {
            let pred_list = self.blocks[block.0 as usize].preds.clone();
            if pred_list.len() == 1 && pred_list[0] != 0 {
                // Exactly one predecessor
                self.read_variable(BlockRef(pred_list[0]), var)
            } else {
                // Multiple predecessors - create phi
                let phi = self.create_phi_var(var);
                let result = self.phi(phi, var_type);
                self.write_variable(block, var, result);
                self.create_pred_upsilons(block, phi)
            }
        };
        
        self.write_variable(block, var, result);
        result
    }

    fn reconstruct_instr_from_hash(&self, hash: u64) -> Instr {
        // Reconstruct constant instructions from hash
        let ty_val = (hash >> 32) & 0xFFFF;
        let ty = match ty_val & 0x7F {
            0 => Type::Void,
            1 => Type::Bool,
            2 => Type::I32,
            _ => Type::Void,
        };
        
        let opcode = (hash >> 48) & 0xFFFF;
        match opcode {
            0x0000 => {
                // Constant
                match ty {
                    Type::Bool => Instr::ConstBool(Meta::new(Type::Bool), (hash & 1) != 0),
                    Type::I32 => Instr::ConstI32(Meta::new(Type::I32), (hash & 0xFFFFFFFF) as i32),
                    _ => Instr::Nop(Meta::new(Type::Void)),
                }
            }
            _ => Instr::Nop(Meta::new(Type::Void)),
        }
    }
}

impl Default for IrGen {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_irgen() {
        let mut gen = IrGen::new();
        
        // Create a simple block with some instructions
        let entry_block = gen.create_block();
        gen.seal_block(entry_block);
        gen.label(entry_block);
        
        // Create a simple computation: return 5 + 5
        let const5 = gen.const_i32(5);
        let add_result = gen.add(const5, const5);
        gen.ret(add_result);
        
        // Verify we have created the expected number of blocks
        assert_eq!(gen.blocks.len(), 2); // index 0 (unused) + entry_block
    }

    #[test]
    fn test_variable_operations() {
        let mut gen = IrGen::new();
        
        // Create variable and blocks
        let var = gen.create_variable(Type::I32);
        let block = gen.create_block();
        gen.seal_block(block);
        
        // Write and read variable
        let val = gen.const_i32(42);
        gen.write_variable(block, var, val);
        let read_val = gen.read_variable(block, var);
        
        // Should get back the same value
        match read_val {
            Instr::ConstI32(_, value) => assert_eq!(value, 42),
            _ => panic!("Expected constant i32"),
        }
    }

    #[test]
    fn test_phi_creation() {
        let mut gen = IrGen::new();
        
        // Create phi node
        let phi = gen.create_phi();
        let phi_instr = gen.phi(phi, Type::I32);
        
        // Should create an identity instruction pointing to the phi
        match phi_instr {
            Instr::Identity(meta, _) => {
                assert_eq!(meta.get_type(), Type::I32);
            }
            _ => panic!("Expected identity instruction"),
        }
    }
}