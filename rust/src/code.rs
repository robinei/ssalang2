use crate::ir::{Instr, InstrRef, RefType};
use std::ops::{Index, IndexMut};

/// A specialized instruction storage that maintains SSA invariants.
/// 
/// Key properties:
/// - Index 0 always contains Nop instruction
/// - Positive indices store pinned instructions (sequential execution order)
/// - Negative indices store unpinned pure instructions (can be reordered)
/// - Always returns valid InstrRef when storing instructions
/// - Automatically manages the bidirectional growth
#[derive(Debug, Clone)]
pub struct Code {
    /// Storage for positive indices (0, 1, 2, ...) - pinned instructions
    positive: Vec<Instr>,
    /// Storage for negative indices (-1, -2, -3, ...) in reverse order
    /// negative[0] = instruction at index -1
    /// negative[1] = instruction at index -2, etc.
    negative: Vec<Instr>,
}

impl Code {
    /// Creates a new Code storage with index 0 initialized to Nop
    pub fn new() -> Self {
        let mut code = Self {
            positive: Vec::with_capacity(16),
            negative: Vec::with_capacity(16),
        };
        
        // Ensure index 0 always contains Nop
        code.positive.push(Instr::nop());
        
        code
    }

    /// Creates a new Code storage with specified capacities
    pub fn with_capacity(positive_cap: usize, negative_cap: usize) -> Self {
        let mut code = Self {
            positive: Vec::with_capacity(positive_cap),
            negative: Vec::with_capacity(negative_cap),
        };
        
        // Ensure index 0 always contains Nop
        code.positive.push(Instr::nop());
        
        code
    }

    /// Pushes a pinned instruction and returns its InstrRef.
    /// Pinned instructions get positive indices and execute in order.
    pub fn push_pinned(&mut self, instr: Instr) -> InstrRef {
        let ref_val = self.positive.len() as RefType;
        self.positive.push(instr);
        InstrRef::new(ref_val).expect("Instruction ref should be non-zero")
    }

    /// Pushes an unpinned instruction and returns its InstrRef.
    /// Unpinned instructions get negative indices and can be reordered.
    pub fn push_unpinned(&mut self, instr: Instr) -> InstrRef {
        let ref_val = -(self.negative.len() as RefType + 1);
        self.negative.push(instr);
        InstrRef::new(ref_val).expect("Instruction ref should be non-zero")
    }

    /// Gets the number of pinned instructions (positive indices)
    pub fn pinned_count(&self) -> usize {
        self.positive.len() - 1
    }

    /// Gets the number of unpinned instructions (negative indices)
    pub fn unpinned_count(&self) -> usize {
        self.negative.len()
    }

    /// Gets the total number of instructions
    pub fn len(&self) -> usize {
        self.pinned_count() + self.unpinned_count()
    }

    /// Returns true if only the Nop at index 0 exists
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Clears all instructions except the Nop at index 0
    pub fn clear(&mut self) {
        self.positive.clear();
        self.negative.clear();
        // Restore the Nop at index 0
        self.positive.push(Instr::nop());
    }

    /// Gets an instruction by InstrRef, returns None if out of bounds
    pub fn get(&self, instr_ref: InstrRef) -> Option<&Instr> {
        let index = instr_ref.get();
        if index >= 0 {
            self.positive.get(index as usize)
        } else {
            let neg_index = (-index - 1) as usize;
            self.negative.get(neg_index)
        }
    }

    /// Gets a mutable reference to an instruction by InstrRef
    pub fn get_mut(&mut self, instr_ref: InstrRef) -> Option<&mut Instr> {
        let index = instr_ref.get();
        if index >= 0 {
            self.positive.get_mut(index as usize)
        } else {
            let neg_index = (-index - 1) as usize;
            self.negative.get_mut(neg_index)
        }
    }

    /// Sets an instruction at the given InstrRef.
    /// Panics if the reference is invalid or if trying to modify index 0.
    pub fn set(&mut self, instr_ref: InstrRef, instr: Instr) {
        let index = instr_ref.get();
        assert_ne!(index, 0, "Cannot modify the Nop instruction at index 0");
        
        if index > 0 {
            let pos_index = index as usize;
            assert!(pos_index < self.positive.len(), "Invalid positive instruction reference");
            self.positive[pos_index] = instr;
        } else {
            let neg_index = (-index - 1) as usize;
            assert!(neg_index < self.negative.len(), "Invalid negative instruction reference");
            self.negative[neg_index] = instr;
        }
    }

    /// Returns an iterator over all instructions with their InstrRef
    pub fn iter_with_refs(&self) -> CodeIterator {
        CodeIterator::new(&self.negative, &self.positive)
    }
}

impl Index<InstrRef> for Code {
    type Output = Instr;

    fn index(&self, instr_ref: InstrRef) -> &Self::Output {
        self.get(instr_ref).expect("Invalid instruction reference")
    }
}

impl IndexMut<InstrRef> for Code {
    fn index_mut(&mut self, instr_ref: InstrRef) -> &mut Self::Output {
        let index = instr_ref.get();
        assert_ne!(index, 0, "Cannot modify the Nop instruction at index 0");
        self.get_mut(instr_ref).expect("Invalid instruction reference")
    }
}

// Support indexing by isize for compatibility
impl Index<isize> for Code {
    type Output = Instr;

    fn index(&self, index: isize) -> &Self::Output {
        if index >= 0 {
            &self.positive[index as usize]
        } else {
            let neg_index = (-index - 1) as usize;
            &self.negative[neg_index]
        }
    }
}

impl IndexMut<isize> for Code {
    fn index_mut(&mut self, index: isize) -> &mut Self::Output {
        assert_ne!(index, 0, "Cannot modify the Nop instruction at index 0");
        if index > 0 {
            &mut self.positive[index as usize]
        } else {
            let neg_index = (-index - 1) as usize;
            &mut self.negative[neg_index]
        }
    }
}

impl Default for Code {
    fn default() -> Self {
        Self::new()
    }
}

/// Iterator for Code that yields (InstrRef, &Instr) pairs
pub struct CodeIterator<'a> {
    negative: &'a [Instr],
    positive: &'a [Instr],
    current_index: isize,
    end_index: isize,
}

impl<'a> CodeIterator<'a> {
    fn new(negative: &'a [Instr], positive: &'a [Instr]) -> Self {
        let start_index = -(negative.len() as isize);
        let end_index = positive.len() as isize;
        Self {
            negative,
            positive,
            current_index: start_index,
            end_index,
        }
    }
}

impl<'a> Iterator for CodeIterator<'a> {
    type Item = (InstrRef, &'a Instr);

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index >= self.end_index {
            return None;
        }
        
        let index = self.current_index;
        
        // Skip index 0 since InstrRef can't represent it (NonZeroI16)
        if index == 0 {
            self.current_index += 1;
            return self.next();
        }
        
        let instr_ref = InstrRef::new(index as RefType)?;
        
        let instr = if index < 0 {
            // Negative index: convert to array index
            let array_index = (-index - 1) as usize;
            &self.negative[array_index]
        } else {
            // Positive index: direct array index
            &self.positive[index as usize]
        };
        
        self.current_index += 1;
        Some((instr_ref, instr))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_code_has_nop_at_zero() {
        let code = Code::new();
        assert_eq!(code.pinned_count(), 0);  // No user instructions yet
        assert_eq!(code.unpinned_count(), 0);
        
        match code[0] {
            Instr::Nop(_) => (),
            _ => panic!("Index 0 should contain Nop"),
        }
    }

    #[test]
    fn test_push_pinned() {
        let mut code = Code::new();
        let instr = Instr::const_i32(42);
        let instr_ref = code.push_pinned(instr);
        
        assert_eq!(instr_ref.get(), 1);
        assert_eq!(code[instr_ref], instr);
        assert_eq!(code.pinned_count(), 1);  // One user instruction
    }

    #[test]
    fn test_push_unpinned() {
        let mut code = Code::new();
        let instr = Instr::const_bool(true);
        let instr_ref = code.push_unpinned(instr);
        
        assert_eq!(instr_ref.get(), -1);
        assert_eq!(code[instr_ref], instr);
        assert_eq!(code.unpinned_count(), 1);
    }

    #[test]
    fn test_set_instruction() {
        let mut code = Code::new();
        let instr_ref = code.push_pinned(Instr::const_i32(10));
        let new_instr = Instr::const_i32(20);
        
        code.set(instr_ref, new_instr);
        assert_eq!(code[instr_ref], new_instr);
    }

    #[test]
    fn test_cannot_modify_index_zero() {
        let mut code = Code::new();
        // Since InstrRef uses NonZeroI16, we can't create a reference to index 0
        // Instead, test that direct indexing with isize panics
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            code[0] = Instr::const_i32(42);
        }));
        assert!(result.is_err());
    }

    #[test]
    fn test_clear_preserves_nop() {
        let mut code = Code::new();
        code.push_pinned(Instr::const_i32(42));
        code.push_unpinned(Instr::const_bool(true));
        
        code.clear();
        
        assert_eq!(code.pinned_count(), 0);  // No user instructions after clear
        assert_eq!(code.unpinned_count(), 0);
        match code[0] {
            Instr::Nop(_) => (),
            _ => panic!("Index 0 should still contain Nop after clear"),
        }
    }

    #[test]
    fn test_iter_with_refs() {
        let mut code = Code::new();
        let pos_ref = code.push_pinned(Instr::const_i32(10));
        let neg_ref = code.push_unpinned(Instr::const_bool(true));
        
        let items: Vec<_> = code.iter_with_refs().collect();
        assert_eq!(items.len(), 2); // 1 positive + 1 negative (skip index 0)
        
        // Should iterate in order: negative, then positive (skip index 0)
        assert_eq!(items[0].0, neg_ref);
        assert_eq!(items[1].0, pos_ref);
    }
}