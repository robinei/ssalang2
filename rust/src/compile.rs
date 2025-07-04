use crate::ast::*;
use crate::ir::InstrRef;

/// Variable slot containing analysis state for a local variable
#[derive(Debug, Clone)]
pub struct VariableSlot {
    pub value: Option<InstrRef>,  // IR instruction representing the value
    pub is_const: bool,           // Whether this binding is constant
    pub is_initialized: bool,     // Whether it's been assigned
    // Could add more analysis state here:
    // pub last_write_location: Option<NodeRef>,
    // pub escape_analysis: EscapeInfo,
    // etc.
}

impl VariableSlot {
    pub fn new(is_const: bool) -> Self {
        Self {
            value: None,
            is_const,
            is_initialized: false,
        }
    }
    
    pub fn uninitialized() -> Self {
        Self {
            value: None,
            is_const: false,
            is_initialized: false,
        }
    }
}

/// Stack frame for function activation during compile-time execution
#[derive(Debug, Clone)]
pub struct StackFrame {
    pub scope_index: ScopeIndex,    // Which scope this frame represents
    pub locals_count: u16,          // Number of locals in this frame
    pub values_base: usize,         // Index into shared slot stack
}

impl StackFrame {
    pub fn new(scope_index: ScopeIndex, locals_count: u16, values_base: usize) -> Self {
        Self {
            scope_index,
            locals_count,
            values_base,
        }
    }
    
    /// Convert a LocalRef to a slot index in the shared variable_slots vector
    /// Returns None if the local is not defined in this frame
    pub fn to_slot_index(&self, local_ref: LocalRef) -> Option<usize> {
        if local_ref.scope == self.scope_index {
            // local_ref.index is already 0-based within the scope
            Some(self.values_base + local_ref.index as usize)
        } else {
            None
        }
    }
}

/// Compiler for generating IR from AST with compile-time execution support
pub struct Compiler {
    pub frame_stack: Vec<StackFrame>,
    pub variable_slots: Vec<VariableSlot>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            frame_stack: Vec::new(),
            variable_slots: Vec::new(),
        }
    }
    
    /// Get immutable reference to a variable slot
    pub fn get_slot(&self, local_ref: LocalRef) -> &VariableSlot {
        for frame in self.frame_stack.iter().rev() {
            if let Some(slot_idx) = frame.to_slot_index(local_ref) {
                return &self.variable_slots[slot_idx];
            }
        }
        panic!("Local {:?} not found in any frame", local_ref);
    }
    
    /// Get mutable reference to a variable slot
    pub fn get_slot_mut(&mut self, local_ref: LocalRef) -> &mut VariableSlot {
        for frame in self.frame_stack.iter().rev() {
            if let Some(slot_idx) = frame.to_slot_index(local_ref) {
                return &mut self.variable_slots[slot_idx];
            }
        }
        panic!("Local {:?} not found in any frame", local_ref);
    }
    
    /// Enter a new function scope, creating a new stack frame
    pub fn enter_function(&mut self, scope_index: ScopeIndex, locals_ref: LocalsRef) {
        let values_base = self.variable_slots.len();
        
        // Push uninitialized slots for this frame's locals
        self.variable_slots.resize(values_base + locals_ref.count as usize, 
                                  VariableSlot::uninitialized());
        
        let frame = StackFrame::new(
            scope_index,
            locals_ref.count,
            values_base,
        );
        
        self.frame_stack.push(frame);
    }
    
    /// Exit the current function scope, removing its stack frame
    pub fn exit_function(&mut self) {
        if let Some(frame) = self.frame_stack.pop() {
            // Shrink variable slots to remove this frame's locals
            self.variable_slots.truncate(frame.values_base);
        }
    }
    
    /// Initialize a local variable with analysis state from the AST
    pub fn initialize_local(&mut self, local_ref: LocalRef, local: &Local) {
        let slot = self.get_slot_mut(local_ref);
        slot.is_const = local.is_const;
        // Don't set is_initialized here - that happens when the variable is assigned
    }
    
    /// Assign a value to a local variable
    pub fn assign_local(&mut self, local_ref: LocalRef, value: InstrRef) -> Result<(), String> {
        let slot = self.get_slot_mut(local_ref);
        
        if slot.is_const && slot.is_initialized {
            return Err(format!("Cannot reassign const variable at index {:?}", local_ref));
        }
        
        slot.value = Some(value);
        slot.is_initialized = true;
        Ok(())
    }
    
    /// Read a value from a local variable
    pub fn read_local(&self, local_ref: LocalRef) -> Result<InstrRef, String> {
        let slot = self.get_slot(local_ref);
        
        if !slot.is_initialized {
            return Err(format!("Use of uninitialized variable at index {:?}", local_ref));
        }
        
        slot.value.ok_or_else(|| format!("Variable at index {:?} has no value", local_ref))
    }
    
    /// Get the current stack depth (number of active frames)
    pub fn stack_depth(&self) -> usize {
        self.frame_stack.len()
    }
    
    /// Check if a local is defined in any active frame
    pub fn is_local_defined(&self, local_ref: LocalRef) -> bool {
        self.frame_stack.iter().rev().any(|frame| {
            frame.to_slot_index(local_ref).is_some()
        })
    }
}