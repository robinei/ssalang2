use crate::ast::*;
use crate::ir::{BlockRef, PhiRef, Instr};
use crate::irgen::IrGen;
use crate::types::{Type, TypeId};

/// Comprehensive error type for compilation
#[derive(Debug, PartialEq)]
pub enum CompileError {
    TypeError {
        expected: TypeId,
        found: TypeId,
        context: String,
    },
    UndefinedVariable {
        local_ref: LocalRef,
        context: String,
    },
    UninitializedVariable {
        local_ref: LocalRef,
        context: String,
    },
    ConstAssignment {
        local_ref: LocalRef,
        context: String,
    },
    StaticValueExpected {
        local_ref: LocalRef,
        context: String,
    },
    BreakOutsideLoop {
        context: String,
    },
    ContinueOutsideLoop,
    // Special error types for compile-time control flow
    CompileTimeBreak {
        label: Option<SymbolRef>,
        value: Option<Value>,
    },
    CompileTimeContinue {
        label: Option<SymbolRef>,
    },
    InvalidType {
        context: String,
    },
    VariableAlreadyDefined {
        local_ref: LocalRef,
        context: String,
    },
    NotAConstant {
        local_ref: LocalRef,
        context: String,
    },
    NotAConstantExpression {
        context: String,
    },
    InternalError {
        message: String,
    },
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::TypeError { expected, found, context } => {
                write!(f, "Type error: expected {}, found {} in {}", expected, found, context)
            }
            CompileError::UndefinedVariable { local_ref, context } => {
                write!(f, "Undefined variable {:?} in {}", local_ref, context)
            }
            CompileError::UninitializedVariable { local_ref, context } => {
                write!(f, "Uninitialized variable {:?} in {}", local_ref, context)
            }
            CompileError::ConstAssignment { local_ref, context } => {
                write!(f, "Cannot reassign const variable {:?} in {}", local_ref, context)
            }
            CompileError::StaticValueExpected { local_ref, context } => {
                write!(f, "Static value expected {:?} in {}", local_ref, context)
            }
            CompileError::BreakOutsideLoop { context } => {
                write!(f, "Break statement outside loop in {}", context)
            }
            CompileError::ContinueOutsideLoop => {
                write!(f, "Continue statement outside loop")
            }
            CompileError::CompileTimeBreak { label, value } => {
                match (label, value) {
                    (Some(label), Some(val)) => write!(f, "Compile-time break to label {:?} with {} value", label, val.type_id),
                    (Some(label), None) => write!(f, "Compile-time break to label {:?}", label),
                    (None, Some(val)) => write!(f, "Compile-time break with {} value", val.type_id),
                    (None, None) => write!(f, "Compile-time break"),
                }
            }
            CompileError::CompileTimeContinue { label } => {
                match label {
                    Some(label) => write!(f, "Compile-time continue to label {:?}", label),
                    None => write!(f, "Compile-time continue"),
                }
            }
            CompileError::InvalidType { context } => {
                write!(f, "Invalid type in {}", context)
            }
            CompileError::VariableAlreadyDefined { local_ref, context } => {
                write!(f, "Variable {:?} is already defined in {}", local_ref, context)
            }
            CompileError::NotAConstant { local_ref, context } => {
                write!(f, "Variable {:?} is not a compile-time constant in {}", local_ref, context)
            }
            CompileError::NotAConstantExpression { context } => {
                write!(f, "Expression is not a compile-time constant in {}", context)
            }
            CompileError::InternalError { message } => {
                write!(f, "Internal compiler error: {}", message)
            }
        }
    }
}

impl std::error::Error for CompileError {}

/// Helper function to create consistent void/unit values
fn unit_value() -> Value {
    Value::new(TypeId::unit_id(), Instr::nop())
}

/// Control flow scope for handling break/continue
#[derive(Debug, Clone)]
pub enum ControlFlowKind {
    Loop,
    Block,
}

/// Control flow scope tracking for break/continue handling
#[derive(Debug, Clone)]
pub struct ControlFlowScope {
    pub kind: ControlFlowKind,
    pub label: Option<SymbolRef>,
    // For runtime IR generation
    pub break_target: Option<BlockRef>,
    pub continue_target: Option<BlockRef>,
    pub break_phi: Option<PhiRef>,  // Phi for merging break values
    pub break_type: Option<TypeId>, // Type of break values
    // For compile-time execution
    pub break_value: Option<(TypeId, Instr)>,
}

impl ControlFlowScope {
    pub fn new_loop(label: Option<SymbolRef>) -> Self {
        Self {
            kind: ControlFlowKind::Loop,
            label,
            break_target: None,
            continue_target: None,
            break_phi: None,
            break_type: None,
            break_value: None,
        }
    }

    pub fn new_block(label: Option<SymbolRef>, break_target: BlockRef) -> Self {
        Self {
            kind: ControlFlowKind::Block,
            label,
            break_target: Some(break_target),
            continue_target: None,
            break_phi: None,
            break_type: None,
            break_value: None,
        }
    }
}

/// Variable slot containing analysis state for a local variable
#[derive(Debug, Clone)]
pub struct VariableSlot {
    pub value: Option<Instr>,       // IR instruction representing the value
    pub type_id: Option<TypeId>,    // TypeId of the variable's type
    pub is_const: bool,             // Whether this binding is constant
    pub is_static: bool,            // Whether this binding is static only
    pub is_initialized: bool,       // Whether it's been assigned
}

impl VariableSlot {
    pub fn new(is_const: bool) -> Self {
        Self {
            value: None,
            type_id: None,
            is_const,
            is_static: false,
            is_initialized: false,
        }
    }
    
    pub fn uninitialized() -> Self {
        Self {
            value: None,
            type_id: None,
            is_const: false,
            is_static: false,
            is_initialized: false,
        }
    }
}

/// Stack frame for function activation during compile-time execution
#[derive(Debug, Clone)]
pub struct StackFrame {
    pub func_index: FuncIndex,    // Which func this frame represents
    pub locals_count: u16,          // Number of locals in this frame
    pub values_base: usize,         // Index into shared slot stack
}

impl StackFrame {
    pub fn new(func_index: FuncIndex, locals_count: u16, values_base: usize) -> Self {
        Self {
            func_index,
            locals_count,
            values_base,
        }
    }
    
    /// Convert a LocalRef to a slot index in the shared variable_slots vector
    /// Returns None if the local is not defined in this frame
    pub fn to_slot_index(&self, local_ref: LocalRef) -> Option<usize> {
        if local_ref.func == self.func_index {
            // local_ref.index is already 0-based within the func
            Some(self.values_base + local_ref.index as usize)
        } else {
            None
        }
    }
}


#[derive(Debug, PartialEq)]
pub struct Value {
    pub type_id: TypeId,
    pub instr: Instr,
}

impl Value {
    pub fn new(type_id: TypeId, instr: Instr) -> Self {
        Self {
            type_id,
            instr
        }
    }
}

pub fn to_ir_type(type_id: TypeId) -> crate::ir::Type {
    match type_id.get_type().unwrap().as_ref() {
        Type::Unit => crate::ir::Type::Void,
        Type::Bool => crate::ir::Type::Bool,
        Type::I32 => crate::ir::Type::I32,
        Type::Type => crate::ir::Type::I32,
        Type::Fn(..) => crate::ir::Type::I32,
        Type::Struct(..) => crate::ir::Type::I32,
    }
}

/// Compiler for generating IR from AST with compile-time execution support
pub struct Compiler {
    pub frame_stack: Vec<StackFrame>,
    pub variable_slots: Vec<VariableSlot>,
    pub control_flow_stack: Vec<ControlFlowScope>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            frame_stack: Vec::new(),
            variable_slots: Vec::new(),
            control_flow_stack: Vec::new(),
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
    
    /// Enter a new function func, creating a new stack frame
    pub fn enter_function(&mut self, func_index: FuncIndex, locals_ref: LocalsRef) {
        let values_base = self.variable_slots.len();
        
        // Push uninitialized slots for this frame's locals
        self.variable_slots.resize(values_base + locals_ref.count as usize, 
                                  VariableSlot::uninitialized());
        
        let frame = StackFrame::new(
            func_index,
            locals_ref.count,
            values_base,
        );
        
        self.frame_stack.push(frame);
    }
    
    /// Exit the current function func, removing its stack frame
    pub fn exit_function(&mut self) {
        if let Some(frame) = self.frame_stack.pop() {
            // Shrink variable slots to remove this frame's locals
            self.variable_slots.truncate(frame.values_base);
        }
    }
    
    /// Initialize a local variable with analysis state from the AST
    pub fn initialize_local(&mut self, local_ref: LocalRef, local: &Local) {
        let slot: &mut VariableSlot = self.get_slot_mut(local_ref);
        slot.is_const = local.is_const;
        // Don't set is_initialized here - that happens when the variable is assigned
    }
    
    /// Assign a value to a local variable
    pub fn assign_local(&mut self, local_ref: LocalRef, value: Instr) -> Result<(), String> {
        let slot = self.get_slot_mut(local_ref);
        
        if slot.is_const && slot.is_initialized {
            return Err(format!("Cannot reassign const variable at index {:?}", local_ref));
        }
        
        slot.value = Some(value);
        slot.is_initialized = true;
        Ok(())
    }
    
    /// Read a value from a local variable
    pub fn read_local(&self, local_ref: LocalRef) -> Result<Instr, String> {
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

    /// Main compilation method that converts AST nodes to IR
    pub fn compile(
        &mut self,
        node_ref: NodeRef,
        ast: &Ast,
        irgen: &mut IrGen,
        static_eval: bool,
    ) -> Result<Value, CompileError> {
        let node = ast.get_node(node_ref);
        
        match node {
            // Type nodes
            Node::TypeAtom(atom) => {
                let type_id = match *atom {
                    TypeAtom::Unit => Type::Unit,
                    TypeAtom::Bool => Type::Bool,
                    TypeAtom::I32 => Type::I32,
                }.intern();
                Ok(Value::new(TypeId::type_id(), Instr::const_i32(type_id.as_i32())))
            }

            // Constants
            Node::ConstUnit => {
                Ok(unit_value())
            }
            
            Node::ConstBool(value) => {
                Ok(Value::new(TypeId::bool_id(), Instr::const_bool(*value)))
            }
            
            Node::ConstI32(value) => {
                Ok(Value::new(TypeId::i32_id(), Instr::const_i32(*value)))
            }

            Node::ConstString(_string_ref) => {
                // String support not implemented yet
                Err(CompileError::InvalidType {
                    context: "String constants not yet supported".to_string(),
                })
            }

            // Operators
            Node::Unop(op_type, operand_ref) => {
                let operand = self.compile(*operand_ref, ast, irgen, static_eval)?;
                
                match op_type {
                    UnopType::Neg => {
                        if operand.type_id != TypeId::i32_id() {
                            return Err(CompileError::TypeError {
                                expected: TypeId::i32_id(),
                                found: operand.type_id,
                                context: "unary negation".to_string(),
                            });
                        }
                        let result_instr = irgen.neg(operand.instr);
                        Ok(Value::new(TypeId::i32_id(), result_instr))
                    }
                    UnopType::Not => {
                        if operand.type_id != TypeId::bool_id() {
                            return Err(CompileError::TypeError {
                                expected: TypeId::bool_id(),
                                found: operand.type_id,
                                context: "logical not".to_string(),
                            });
                        }
                        let result_instr = irgen.not(operand.instr);
                        Ok(Value::new(TypeId::bool_id(), result_instr))
                    }
                }
            }

            Node::Binop(op_type, left_ref, right_ref) => {
                let left = self.compile(*left_ref, ast, irgen, static_eval)?;
                let right = self.compile(*right_ref, ast, irgen, static_eval)?;

                match op_type {
                    BinopType::Add | BinopType::Sub | BinopType::Mul | BinopType::Div => {
                        if left.type_id != TypeId::i32_id() || right.type_id != TypeId::i32_id() {
                            return Err(CompileError::TypeError {
                                expected: TypeId::i32_id(),
                                found: if left.type_id != TypeId::i32_id() { left.type_id } else { right.type_id },
                                context: format!("arithmetic operation {:?}", op_type),
                            });
                        }
                        let result_instr = match op_type {
                            BinopType::Add => irgen.add(left.instr, right.instr),
                            BinopType::Sub => irgen.sub(left.instr, right.instr),
                            BinopType::Mul => irgen.mul(left.instr, right.instr),
                            BinopType::Div => irgen.div(left.instr, right.instr),
                            _ => unreachable!(),
                        };
                        Ok(Value::new(TypeId::i32_id(), result_instr))
                    }

                    BinopType::Eq | BinopType::Neq => {
                        if left.type_id != right.type_id {
                            return Err(CompileError::TypeError {
                                expected: left.type_id,
                                found: right.type_id,
                                context: format!("comparison operation {:?}", op_type),
                            });
                        }
                        let result_instr = match op_type {
                            BinopType::Eq => irgen.eq(left.instr, right.instr),
                            BinopType::Neq => irgen.neq(left.instr, right.instr),
                            _ => unreachable!(),
                        };
                        Ok(Value::new(TypeId::bool_id(), result_instr))
                    }

                    BinopType::Lt | BinopType::Gt | BinopType::LtEq | BinopType::GtEq => {
                        if left.type_id != TypeId::i32_id() || right.type_id != TypeId::i32_id() {
                            return Err(CompileError::TypeError {
                                expected: TypeId::i32_id(),
                                found: if left.type_id != TypeId::i32_id() { left.type_id } else { right.type_id },
                                context: format!("comparison operation {:?}", op_type),
                            });
                        }
                        let result_instr = match op_type {
                            BinopType::Lt => irgen.lt(left.instr, right.instr),
                            BinopType::Gt => irgen.gt(left.instr, right.instr),
                            BinopType::LtEq => irgen.lt_eq(left.instr, right.instr),
                            BinopType::GtEq => irgen.gt_eq(left.instr, right.instr),
                            _ => unreachable!(),
                        };
                        Ok(Value::new(TypeId::bool_id(), result_instr))
                    }

                    BinopType::And | BinopType::Or => {
                        if left.type_id != TypeId::bool_id() || right.type_id != TypeId::bool_id() {
                            return Err(CompileError::TypeError {
                                expected: TypeId::bool_id(),
                                found: if left.type_id != TypeId::bool_id() { left.type_id } else { right.type_id },
                                context: format!("logical operation {:?}", op_type),
                            });
                        }
                        let result_instr = match op_type {
                            BinopType::And => irgen.and(left.instr, right.instr),
                            BinopType::Or => irgen.or(left.instr, right.instr),
                            _ => unreachable!(),
                        };
                        Ok(Value::new(TypeId::bool_id(), result_instr))
                    }
                }
            }

            // Variable operations
            Node::Define(local_ref, expr_ref) => {
                let local = ast.get_local(*local_ref);
                let expr = self.compile(*expr_ref, ast, irgen, static_eval || local.is_static)?;
                
                // Check for definition errors
                {
                    let slot = self.get_slot(*local_ref);
                    if slot.is_initialized {
                        return Err(CompileError::VariableAlreadyDefined {
                            local_ref: *local_ref,
                            context: "variable definition".to_string(),
                        });
                    }

                    if local.is_static && !expr.instr.is_const() {
                        return Err(CompileError::StaticValueExpected {
                            local_ref: *local_ref,
                            context: "variable definition".to_string(),
                        });
                    }
                }
                
                // Type check if we have type annotation
                if let Some(type_node_ref) = local.type_node {
                    let annotation = self.compile(type_node_ref, ast, irgen, true)?;
                    
                    // Type annotations must always evaluate to Type::Type
                    if annotation.type_id != TypeId::type_id() {
                        return Err(CompileError::InvalidType {
                            context: format!("Type annotation for {} must be a type expression (found {})", 
                                           ast.get_local_name(*local_ref), annotation.type_id),
                        });
                    }
                    
                    // Extract the actual type from the TypeId
                    let expected_type = if let Instr::ConstI32(_, type_id_value) = annotation.instr {
                        let type_id = TypeId::new(type_id_value as u32);
                        match type_id.get_type() {
                            Some(_) => type_id,
                            None => return Err(CompileError::InternalError {
                                message: format!("Invalid TypeId {} in type annotation", type_id_value),
                            })
                        }
                    } else {
                        return Err(CompileError::InternalError {
                            message: "Type annotation did not produce a TypeId constant".to_string(),
                        });
                    };
                    
                    if expr.type_id != expected_type {
                        return Err(CompileError::TypeError {
                            expected: expected_type,
                            found: expr.type_id,
                            context: format!("variable definition for {}", ast.get_local_name(*local_ref)),
                        });
                    }
                }

                // Store the value and mark as initialized
                let slot = self.get_slot_mut(*local_ref);
                slot.value = Some(expr.instr);
                slot.type_id = Some(expr.type_id);
                slot.is_initialized = true;
                slot.is_const = local.is_const;
                slot.is_static = local.is_static;

                Ok(unit_value())
            }

            Node::DefineFn(_local_ref, _func_ref) => {
                // TODO: Implement function definition
                Err(CompileError::InternalError {
                    message: "Function definitions not yet implemented".to_string(),
                })
            }

            Node::Assign(local_ref, expr_ref) => {
                let local = ast.get_local(*local_ref);
                let expr = self.compile(*expr_ref, ast, irgen, static_eval || local.is_static)?;
                
                // Check for assignment errors
                {
                    let slot = self.get_slot(*local_ref);
                    if local.is_const {
                        return Err(CompileError::ConstAssignment {
                            local_ref: *local_ref,
                            context: "variable assignment".to_string(),
                        });
                    }

                    if !slot.is_initialized {
                        return Err(CompileError::UninitializedVariable {
                            local_ref: *local_ref,
                            context: "variable assignment".to_string(),
                        });
                    }

                    if local.is_static && !expr.instr.is_const() {
                        return Err(CompileError::StaticValueExpected {
                            local_ref: *local_ref,
                            context: "variable assignment".to_string(),
                        });
                    }
                }
                
                // Store the value
                let slot = self.get_slot_mut(*local_ref);
                slot.value = Some(expr.instr);
                slot.type_id = Some(expr.type_id);

                Ok(expr)
            }

            Node::LocalRead(local_ref) => {
                let local = ast.get_local(*local_ref);
                let slot = self.get_slot(*local_ref);
                
                if !slot.is_initialized {
                    return Err(CompileError::UninitializedVariable {
                        local_ref: *local_ref,
                        context: "variable read".to_string(),
                    });
                }

                let (Some(var_type), Some(var_value)) = (slot.type_id, slot.value) else {
                    return Err(CompileError::InternalError {
                        message: format!("Variable slot has no value or type for {}", ast.get_local_name(*local_ref)),
                    });
                };

                if static_eval || local.is_static {
                    // For static evaluation, ensure the value is actually a compile-time constant
                    if var_value.is_const() {
                        Ok(Value::new(var_type, var_value))
                    } else {
                        Err(CompileError::NotAConstant {
                            local_ref: *local_ref,
                            context: "static evaluation".to_string(),
                        })
                    }
                } else if local.is_param {
                    // For function parameters, generate arg instruction
                    let arg_instr = irgen.arg(local_ref.index as i32, to_ir_type(var_type));
                    Ok(Value::new(var_type, arg_instr))
                } else {
                    // For regular variables, use SSA variable reading
                    // This is a simplified version - full implementation would need VarRef management
                    Ok(Value::new(var_type, var_value))
                }
            }

            // Control flow: Block statements
            Node::Block(is_static, label, nodes_ref) => {
                let nodes = ast.get_node_refs(*nodes_ref);

                if static_eval || *is_static {
                    // Static evaluation: use error propagation for break handling
                    let mut last_result = unit_value();
                    
                    for &node_ref in nodes {
                        match self.compile(node_ref, ast, irgen, true) {
                            Ok(result) => last_result = result,
                            Err(CompileError::CompileTimeBreak { label: break_label, value }) => {
                                // Check if this break targets our block (only for labeled breaks)
                                if break_label.is_some() && break_label == *label {
                                    // Labeled break targeting this block - return the break value
                                    return Ok(value.unwrap_or(unit_value()));
                                } else {
                                    // Naked break or labeled break for outer scope - propagate it
                                    return Err(CompileError::CompileTimeBreak { label: break_label, value });
                                }
                            }
                            Err(other_error) => return Err(other_error),
                        }
                    }
                    
                    return Ok(last_result);
                }

                // Runtime evaluation: use proper control flow scope and SSA construction
                let break_target = irgen.create_block();
                let scope = ControlFlowScope::new_block(*label, break_target);
                self.control_flow_stack.push(scope);
                
                let mut last_result = unit_value();
                let mut block_terminated = false;
                
                for &node_ref in nodes {
                    if block_terminated {
                        // Block already terminated by break - skip remaining statements
                        break;
                    }
                    
                    match self.compile(node_ref, ast, irgen, false) {
                        Ok(result) => last_result = result,
                        Err(error) => {
                            // Check if this is a break targeting an outer scope
                            if matches!(error, CompileError::BreakOutsideLoop { .. }) {
                                // Break targeting outer scope - clean up and propagate
                                self.control_flow_stack.pop();
                                return Err(error);
                            } else {
                                // Other error - clean up and propagate
                                self.control_flow_stack.pop();
                                return Err(error);
                            }
                        }
                    }
                    
                    // Check if current block is terminated (by break, return, etc.)
                    if let Some(current_block) = irgen.get_current_block() {
                        if irgen.is_block_terminated(current_block) {
                            block_terminated = true;
                        }
                    }
                }
                
                // Block completed normally
                let scope = self.control_flow_stack.pop().unwrap();
                
                // Check if any breaks occurred (phi exists)
                let Some(phi) = scope.break_phi else {
                    // No breaks occurred - return normal completion result
                    return Ok(last_result);
                };

                // Only emit upsilon for normal completion if block wasn't terminated by break
                if !block_terminated {
                    // Get current block for normal completion upsilon
                    let current_block = irgen.get_current_block().ok_or_else(|| {
                        CompileError::InternalError {
                            message: "No current block after block completion".to_string(),
                        }
                    })?;
                    
                    // Emit upsilon for normal completion if break type matches
                    if let Some(break_type) = scope.break_type {
                        if last_result.type_id == break_type {
                            irgen.upsilon(current_block, phi, last_result.instr);
                        } else if last_result.type_id != TypeId::unit_id() {
                            // Type mismatch between break and normal completion
                            return Err(CompileError::TypeError {
                                expected: break_type,
                                found: last_result.type_id,
                                context: "block break value vs normal completion type mismatch".to_string(),
                            });
                        }
                        // If normal completion is Unit but break type is not, skip upsilon for normal completion
                    }
                    
                    // Jump to break target
                    irgen.jump(scope.break_target.unwrap());
                }
                
                // Finalize phi
                let phi_result = if let (Some(phi), Some(break_type)) = (scope.break_phi, scope.break_type) {
                    Some(irgen.phi(phi, to_ir_type(break_type)))
                } else {
                    None
                };
                
                // Label the break target block
                irgen.label(scope.break_target.unwrap());
                irgen.seal_block(scope.break_target.unwrap());
                
                // Return phi result
                if let Some(phi_instr) = phi_result {
                    Ok(Value::new(scope.break_type.unwrap(), phi_instr))
                } else {
                    // Shouldn't happen since we checked break_phi exists
                    Err(CompileError::InternalError {
                        message: "Break phi exists but finalize_break_phi returned None".to_string(),
                    })
                }
            }

            Node::Return(value_ref) => {
                let value = self.compile(*value_ref, ast, irgen, static_eval)?;
                irgen.ret(value.instr);
                Ok(value)
            }

            // Control flow: If statements
            Node::If(is_inline, cond_ref, then_ref, else_ref) => {
                let cond = self.compile(*cond_ref, ast, irgen, static_eval)?;
                
                // Type check condition
                if cond.type_id != TypeId::bool_id() {
                    return Err(CompileError::TypeError {
                        expected: TypeId::bool_id(),
                        found: cond.type_id,
                        context: "if condition".to_string(),
                    });
                }

                if static_eval || *is_inline {
                    // Static/inline evaluation: condition must be constant
                    if !cond.instr.is_const() {
                        return Err(CompileError::NotAConstantExpression {
                            context: "inline if condition".to_string(),
                        });
                    }

                    // Extract constant boolean value
                    let Instr::ConstBool(_, cond_value) = cond.instr else {
                        unreachable!("is_const() returned true for non-constant");
                    };

                    // Compile only the taken branch
                    return self.compile(if cond_value { *then_ref } else { *else_ref }, ast, irgen, static_eval)
                }

                // Run-time path:
                let then_block = irgen.create_block();
                let else_block = irgen.create_block();
                let merge_block = irgen.create_block();

                irgen.branch(cond.instr, then_block, else_block);
                irgen.seal_block(then_block);
                irgen.seal_block(else_block);

                // Compile then branch
                irgen.label(then_block);
                let then = self.compile(*then_ref, ast, irgen, static_eval)?;
                
                // Check if we need a phi for the result
                let needs_phi = then.type_id != TypeId::unit_id();
                let phi_ref = if needs_phi {
                    Some(irgen.create_phi())
                } else {
                    None
                };

                // Add upsilon for then branch if needed and jump to merge
                if let Some(phi) = phi_ref {
                    irgen.upsilon(then_block, phi, then.instr);
                }
                irgen.jump(merge_block);

                // Compile else branch
                irgen.label(else_block);
                let els = self.compile(*else_ref, ast, irgen, static_eval)?;
                
                // Type check: both branches must have same type
                if then.type_id != els.type_id {
                    return Err(CompileError::TypeError {
                        expected: then.type_id,
                        found: els.type_id,
                        context: "if-else branches must have same type".to_string(),
                    });
                }

                // Add upsilon for else branch if needed and jump to merge
                if let Some(phi) = phi_ref {
                    irgen.upsilon(else_block, phi, els.instr);
                }
                irgen.jump(merge_block);
                irgen.seal_block(merge_block);

                // Generate merge block with phi if needed
                irgen.label(merge_block);

                if let Some(phi) = phi_ref {
                    let phi_instr = irgen.phi(phi, to_ir_type(then.type_id));
                    Ok(Value::new(then.type_id, phi_instr))
                } else {
                    // Unit type - no meaningful value
                    Ok(unit_value())
                }
            }

            Node::While(_is_inline, _cond_ref, _body_ref) => {
                // TODO: Implement full while loop logic
                Err(CompileError::InternalError {
                    message: "While loops not yet implemented".to_string(),
                })
            }

            Node::Break(label, value_ref) => {
                // Always evaluate the break value since it's required in the AST
                let val = self.compile(*value_ref, ast, irgen, static_eval)?;
                let break_value = if val.type_id == TypeId::unit_id() {
                    None // Treat unit type as no value
                } else {
                    Some(val)
                };

                if static_eval {
                    // For compile-time execution, signal break via special error
                    return Err(CompileError::CompileTimeBreak {
                        label: *label,
                        value: break_value,
                    });
                }

                // For runtime execution, find the target scope index
                let scope_index = if let Some(lbl) = label {
                    // Find scope with matching label
                    self.control_flow_stack.iter().rposition(|scope| {
                        scope.label == Some(*lbl)
                    })
                } else {
                    // For naked break, find the nearest loop scope
                    self.control_flow_stack.iter().rposition(|scope| {
                        matches!(scope.kind, ControlFlowKind::Loop)
                    })
                };

                let Some(idx) = scope_index else {
                    // Scope not found
                    let context = if label.is_some() {
                        "labeled break statement".to_string()
                    } else {
                        "naked break statement (no enclosing loop)".to_string()
                    };
                    return Err(CompileError::BreakOutsideLoop {
                        context,
                    });
                };

                let break_target = self.control_flow_stack[idx].break_target;
                
                let Some(target_block) = break_target else {
                    return Err(CompileError::InternalError {
                        message: "Break target not set in control flow scope".to_string(),
                    });
                };

                // Get current block for upsilon
                let Some(current_block) = irgen.get_current_block() else {
                    return Err(CompileError::InternalError {
                        message: "No current block for break statement".to_string(),
                    });
                };

                // For non-unit break values, create/use phi and emit upsilon
                if let Some(val) = break_value {
                    let scope = &mut self.control_flow_stack[idx];
                    if let Some(_) = scope.break_phi {
                        // Phi already exists, check type consistency
                        if let Some(existing_type) = scope.break_type {
                            if existing_type != val.type_id {
                                return Err(CompileError::TypeError {
                                    expected: existing_type,
                                    found: val.type_id,
                                    context: "break value type mismatch in same scope".to_string(),
                                });
                            }
                        }
                    } else {
                        // Create new phi for this scope
                        let phi = irgen.create_phi();
                        scope.break_phi = Some(phi);
                        scope.break_type = Some(val.type_id);
                    }

                    if let Some(phi) = self.control_flow_stack[idx].break_phi {
                        irgen.upsilon(current_block, phi, val.instr);
                    }
                }

                // Generate IR jump to break target
                irgen.jump(target_block);
                Ok(unit_value())
            }

            Node::Continue(label) => {
                if static_eval {
                    // For compile-time execution, signal continue via special error
                    return Err(CompileError::CompileTimeContinue {
                        label: *label,
                    });
                }
            
                // For runtime execution, find the target loop scope and generate jump
                let target_scope = if let Some(lbl) = label {
                    self.control_flow_stack.iter().rev().find(|scope| {
                        scope.label == Some(*lbl) && matches!(scope.kind, ControlFlowKind::Loop)
                    })
                } else {
                    self.control_flow_stack.iter().rev().find(|scope| {
                        matches!(scope.kind, ControlFlowKind::Loop)
                    })
                };

                let Some(scope) = target_scope else {
                    return Err(CompileError::ContinueOutsideLoop);
                };
        
                let Some(continue_target) = scope.continue_target else {
                    return Err(CompileError::InternalError {
                        message: "Continue target not set in loop scope".to_string(),
                    });
                };

                // Generate IR jump to continue target
                irgen.jump(continue_target);
                Ok(unit_value())
            }

            Node::Fn(_func_index, _body_ref, _return_type_ref) => {
                // TODO: Implement function compilation
                Err(CompileError::InternalError {
                    message: "Function compilation not yet implemented".to_string(),
                })
            }

            Node::Module(_func_index, _nodes_ref) => {
                // TODO: Implement module compilation
                Err(CompileError::InternalError {
                    message: "Module compilation not yet implemented".to_string(),
                })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::Meta;
    use crate::irgen::IrGen;

    fn create_test_ast() -> Ast {
        let mut ast = Ast::new();
        
        // Create a simple function for testing
        let func = Func {
            is_static: false,
            is_inline: false,
            locals: LocalsRef::empty(),
            parent: None,
        };
        ast.add_func(func);
        
        ast
    }

    #[test]
    fn test_compiler_initialization() {
        let compiler = Compiler::new();
        assert!(compiler.frame_stack.is_empty());
        assert!(compiler.variable_slots.is_empty());
        assert!(compiler.control_flow_stack.is_empty());
        assert_eq!(compiler.stack_depth(), 0);
    }

    #[test]
    fn test_compile_constants() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Test boolean constant
        let bool_node = ast.add_node(Node::ConstBool(true), NodeInfo::new(0));
        let result = compiler.compile(bool_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let (ty, instr) = (value.type_id, value.instr);
        assert_eq!(ty, TypeId::bool_id());
        match instr {
            Instr::ConstBool(_, value) => assert_eq!(value, true),
            _ => panic!("Expected ConstBool instruction"),
        }

        // Test i32 constant
        let i32_node = ast.add_node(Node::ConstI32(42), NodeInfo::new(0));
        let result = compiler.compile(i32_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let (ty, instr) = (value.type_id, value.instr);
        assert_eq!(ty, TypeId::i32_id());
        match instr {
            Instr::ConstI32(_, value) => assert_eq!(value, 42),
            _ => panic!("Expected ConstI32 instruction"),
        }

        // Test unit constant
        let unit_node = ast.add_node(Node::ConstUnit, NodeInfo::new(0));
        let result = compiler.compile(unit_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let ty = value.type_id;
        assert_eq!(ty, TypeId::unit_id());
    }

    #[test]
    fn test_compile_arithmetic_operators() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Create constant operands
        let left_node = ast.add_node(Node::ConstI32(10), NodeInfo::new(0));
        let right_node = ast.add_node(Node::ConstI32(5), NodeInfo::new(0));

        // Test addition
        let add_node = ast.add_node(Node::Binop(BinopType::Add, left_node, right_node), NodeInfo::new(0));
        let result = compiler.compile(add_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let ty = value.type_id;
        assert_eq!(ty, TypeId::i32_id());

        // Test subtraction
        let sub_node = ast.add_node(Node::Binop(BinopType::Sub, left_node, right_node), NodeInfo::new(0));
        let result = compiler.compile(sub_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let ty = value.type_id;
        assert_eq!(ty, TypeId::i32_id());
    }

    #[test]
    fn test_compile_comparison_operators() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Create constant operands
        let left_node = ast.add_node(Node::ConstI32(10), NodeInfo::new(0));
        let right_node = ast.add_node(Node::ConstI32(5), NodeInfo::new(0));

        // Test less than
        let lt_node = ast.add_node(Node::Binop(BinopType::Lt, left_node, right_node), NodeInfo::new(0));
        let result = compiler.compile(lt_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let ty = value.type_id;
        assert_eq!(ty, TypeId::bool_id());

        // Test equality
        let eq_node = ast.add_node(Node::Binop(BinopType::Eq, left_node, right_node), NodeInfo::new(0));
        let result = compiler.compile(eq_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let ty = value.type_id;
        assert_eq!(ty, TypeId::bool_id());
    }

    #[test]
    fn test_compile_logical_operators() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Create boolean operands
        let left_node = ast.add_node(Node::ConstBool(true), NodeInfo::new(0));
        let right_node = ast.add_node(Node::ConstBool(false), NodeInfo::new(0));

        // Test logical AND
        let and_node = ast.add_node(Node::Binop(BinopType::And, left_node, right_node), NodeInfo::new(0));
        let result = compiler.compile(and_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let ty = value.type_id;
        assert_eq!(ty, TypeId::bool_id());

        // Test logical OR
        let or_node = ast.add_node(Node::Binop(BinopType::Or, left_node, right_node), NodeInfo::new(0));
        let result = compiler.compile(or_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let ty = value.type_id;
        assert_eq!(ty, TypeId::bool_id());
    }

    #[test]
    fn test_compile_unary_operators() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Test negation
        let operand_node = ast.add_node(Node::ConstI32(42), NodeInfo::new(0));
        let neg_node = ast.add_node(Node::Unop(UnopType::Neg, operand_node), NodeInfo::new(0));
        let result = compiler.compile(neg_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let ty = value.type_id;
        assert_eq!(ty, TypeId::i32_id());

        // Test logical NOT
        let bool_operand = ast.add_node(Node::ConstBool(true), NodeInfo::new(0));
        let not_node = ast.add_node(Node::Unop(UnopType::Not, bool_operand), NodeInfo::new(0));
        let result = compiler.compile(not_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let ty = value.type_id;
        assert_eq!(ty, TypeId::bool_id());
    }

    #[test]
    fn test_type_checking_errors() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Test type error for negation with wrong type
        let bool_operand = ast.add_node(Node::ConstBool(true), NodeInfo::new(0));
        let neg_node = ast.add_node(Node::Unop(UnopType::Neg, bool_operand), NodeInfo::new(0));
        let result = compiler.compile(neg_node, &ast, &mut irgen, false);
        assert!(result.is_err());
        match result.unwrap_err() {
            CompileError::TypeError { expected, found, .. } => {
                assert_eq!(expected, TypeId::i32_id());
                assert_eq!(found, TypeId::bool_id());
            }
            _ => panic!("Expected TypeError"),
        }

        // Test type error for arithmetic with mixed types
        let i32_operand = ast.add_node(Node::ConstI32(42), NodeInfo::new(0));
        let bool_operand = ast.add_node(Node::ConstBool(true), NodeInfo::new(0));
        let add_node = ast.add_node(Node::Binop(BinopType::Add, i32_operand, bool_operand), NodeInfo::new(0));
        let result = compiler.compile(add_node, &ast, &mut irgen, false);
        assert!(result.is_err());
        match result.unwrap_err() {
            CompileError::TypeError { expected, found, .. } => {
                assert_eq!(expected, TypeId::i32_id());
                assert_eq!(found, TypeId::bool_id());
            }
            _ => panic!("Expected TypeError"),
        }
    }

    #[test]
    fn test_compile_error_display() {
        let type_error = CompileError::TypeError {
            expected: TypeId::i32_id(),
            found: TypeId::bool_id(),
            context: "test context".to_string(),
        };
        let error_str = format!("{}", type_error);
        assert!(error_str.contains("Type error"));
        assert!(error_str.contains("i32"));
        assert!(error_str.contains("bool"));
        assert!(error_str.contains("test context"));

        let internal_error = CompileError::InternalError {
            message: "test message".to_string(),
        };
        let error_str = format!("{}", internal_error);
        assert!(error_str.contains("Internal compiler error"));
        assert!(error_str.contains("test message"));
        
        // Test compile-time control flow error display
        let break_error = CompileError::CompileTimeBreak {
            label: None,
            value: Some(Value { type_id: TypeId::i32_id(), instr: Instr::const_i32(42) }),
        };
        let error_str = format!("{}", break_error);
        assert!(error_str.contains("Compile-time break"));
        assert!(error_str.contains("i32"));

        let continue_error = CompileError::CompileTimeContinue {
            label: None,
        };
        let error_str = format!("{}", continue_error);
        assert!(error_str.contains("Compile-time continue"));
    }

    #[test]
    fn test_block_compilation() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Create nodes for the block
        let node1 = ast.add_node(Node::ConstI32(10), NodeInfo::new(0));
        let node2 = ast.add_node(Node::ConstI32(20), NodeInfo::new(0));
        let nodes = vec![node1, node2];
        let nodes_ref = ast.add_node_refs(&nodes);

        // Create block node
        let block_node = ast.add_node(Node::Block(false, None, nodes_ref), NodeInfo::new(0));
        let result = compiler.compile(block_node, &ast, &mut irgen, false);
        
        assert!(result.is_ok());
        let value = result.unwrap();
        let ty = value.type_id;
        // Block should return the type of the last expression
        assert_eq!(ty, TypeId::i32_id());
    }

    #[test]
    fn test_variable_redefinition_error() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Create a function with a local variable
        let local = Local {
            name: ast.intern_symbol("x".to_string()),
            is_param: false,
            is_static: false,
            is_const: false,
            type_node: None,
        };
        let locals = vec![local];
        let locals_ref = ast.add_locals(&locals);
        
        // Update the function to have this local
        let func_index = 0;
        ast.get_func_mut(func_index).locals = locals_ref;

        // Initialize compiler with this function's frame
        compiler.enter_function(func_index, locals_ref);

        // Create a LocalRef for this variable
        let local_ref = LocalRef::new(func_index, 0);

        // First definition should succeed
        let expr_node = ast.add_node(Node::ConstI32(42), NodeInfo::new(0));
        let define_node = ast.add_node(Node::Define(local_ref, expr_node), NodeInfo::new(0));
        let result = compiler.compile(define_node, &ast, &mut irgen, false);
        assert!(result.is_ok());

        // Second definition should fail with VariableAlreadyDefined error
        let expr_node2 = ast.add_node(Node::ConstI32(24), NodeInfo::new(0));
        let define_node2 = ast.add_node(Node::Define(local_ref, expr_node2), NodeInfo::new(0));
        let result = compiler.compile(define_node2, &ast, &mut irgen, false);
        assert!(result.is_err());
        match result.unwrap_err() {
            CompileError::VariableAlreadyDefined { local_ref: ref_val, .. } => {
                assert_eq!(ref_val, local_ref);
            }
            _ => panic!("Expected VariableAlreadyDefined error"),
        }
    }

    #[test]
    fn test_static_evaluation_requires_constants() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Create a function with a local variable
        let local = Local {
            name: ast.intern_symbol("x".to_string()),
            is_param: false,
            is_static: true, // Make it static
            is_const: false,
            type_node: None,
        };
        let locals = vec![local];
        let locals_ref = ast.add_locals(&locals);
        
        // Update the function to have this local
        let func_index = 0;
        ast.get_func_mut(func_index).locals = locals_ref;

        // Initialize compiler with this function's frame
        compiler.enter_function(func_index, locals_ref);

        // Create a LocalRef for this variable
        let local_ref = LocalRef::new(func_index, 0);

        // Define the variable with a constant value
        let const_expr = ast.add_node(Node::ConstI32(42), NodeInfo::new(0));
        let define_node = ast.add_node(Node::Define(local_ref, const_expr), NodeInfo::new(0));
        let result = compiler.compile(define_node, &ast, &mut irgen, false);
        assert!(result.is_ok());

        // Reading the static variable should work and return the constant
        let read_node = ast.add_node(Node::LocalRead(local_ref), NodeInfo::new(0));
        let result = compiler.compile(read_node, &ast, &mut irgen, true); // static_eval = true
        assert!(result.is_ok());
        let value = result.unwrap();
        let (ty, instr) = (value.type_id, value.instr);
        assert_eq!(ty, TypeId::i32_id());
        match instr {
            Instr::ConstI32(_, value) => assert_eq!(value, 42),
            _ => panic!("Expected ConstI32 instruction for static variable read"),
        }
    }

    #[test] 
    fn test_instr_is_const() {
        // Test constant instructions
        assert!(Instr::const_bool(true).is_const());
        assert!(Instr::const_bool(false).is_const());
        assert!(Instr::const_i32(42).is_const());
        assert!(Instr::const_i32(-1).is_const());

        // Test non-constant instructions
        let instr_ref = crate::ir::InstrRef::new(1).unwrap();
        assert!(!Instr::Identity(Meta::new(crate::ir::Type::I32), instr_ref).is_const());
        assert!(!Instr::Add(Meta::new(crate::ir::Type::I32), instr_ref, instr_ref).is_const());
        assert!(!Instr::Arg(Meta::new(crate::ir::Type::I32), 0).is_const());
    }

    #[test]
    fn test_static_evaluation_with_non_constant_fails() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Create a function with a local variable
        let local = Local {
            name: ast.intern_symbol("x".to_string()),
            is_param: false,
            is_static: false, // Make it non-static
            is_const: false,
            type_node: None,
        };
        let locals = vec![local];
        let locals_ref = ast.add_locals(&locals);
        
        // Update the function to have this local
        let func_index = 0;
        ast.get_func_mut(func_index).locals = locals_ref;

        // Initialize compiler with this function's frame
        compiler.enter_function(func_index, locals_ref);

        // Create a LocalRef for this variable
        let local_ref = LocalRef::new(func_index, 0);

        // Define the variable with a constant value
        let const_expr = ast.add_node(Node::ConstI32(42), NodeInfo::new(0));
        let define_node = ast.add_node(Node::Define(local_ref, const_expr), NodeInfo::new(0));
        let result = compiler.compile(define_node, &ast, &mut irgen, false);
        assert!(result.is_ok());

        // Now manually store a non-constant instruction to simulate runtime value
        let slot = compiler.get_slot_mut(local_ref);
        let arg_instr = irgen.arg(0, crate::ir::Type::I32);
        slot.value = Some(arg_instr);

        // Reading during static evaluation should fail
        let read_node = ast.add_node(Node::LocalRead(local_ref), NodeInfo::new(0));
        let result = compiler.compile(read_node, &ast, &mut irgen, true); // static_eval = true
        assert!(result.is_err());
        match result.unwrap_err() {
            CompileError::NotAConstant { local_ref: ref_val, .. } => {
                assert_eq!(ref_val, local_ref);
            }
            _ => panic!("Expected NotAConstant error"),
        }
    }

    #[test]
    fn test_if_static_evaluation() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Test inline if with true condition
        let true_cond = ast.add_node(Node::ConstBool(true), NodeInfo::new(0));
        let then_expr = ast.add_node(Node::ConstI32(42), NodeInfo::new(0));
        let else_expr = ast.add_node(Node::ConstI32(24), NodeInfo::new(0));
        let if_node = ast.add_node(Node::If(true, true_cond, then_expr, else_expr), NodeInfo::new(0));
        
        let result = compiler.compile(if_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let (ty, instr) = (value.type_id, value.instr);
        assert_eq!(ty, TypeId::i32_id());
        match instr {
            Instr::ConstI32(_, value) => assert_eq!(value, 42), // then branch
            _ => panic!("Expected ConstI32 from then branch"),
        }

        // Test inline if with false condition
        let false_cond = ast.add_node(Node::ConstBool(false), NodeInfo::new(0));
        let if_node2 = ast.add_node(Node::If(true, false_cond, then_expr, else_expr), NodeInfo::new(0));
        
        let result = compiler.compile(if_node2, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let (ty, instr) = (value.type_id, value.instr);
        assert_eq!(ty, TypeId::i32_id());
        match instr {
            Instr::ConstI32(_, value) => assert_eq!(value, 24), // else branch
            _ => panic!("Expected ConstI32 from else branch"),
        }
    }

    #[test]
    fn test_if_runtime_evaluation() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Create entry block
        let entry_block = irgen.create_block();
        irgen.seal_block(entry_block);
        irgen.label(entry_block);

        // Test runtime if with constant condition (will be optimized by irgen)
        let then_expr = ast.add_node(Node::ConstI32(42), NodeInfo::new(0));
        let else_expr = ast.add_node(Node::ConstI32(24), NodeInfo::new(0));
        let true_cond = ast.add_node(Node::ConstBool(true), NodeInfo::new(0));
        let if_node = ast.add_node(Node::If(false, true_cond, then_expr, else_expr), NodeInfo::new(0));
        
        let result = compiler.compile(if_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let ty = value.type_id;
        assert_eq!(ty, TypeId::i32_id());
        
        // The instruction should be an identity wrapping a phi
        // We can't easily test the exact structure without more IR introspection
    }

    #[test]
    fn test_if_type_errors() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Create entry block for runtime tests
        let entry_block = irgen.create_block();
        irgen.seal_block(entry_block);
        irgen.label(entry_block);

        // Test non-boolean condition
        let int_cond = ast.add_node(Node::ConstI32(1), NodeInfo::new(0));
        let then_expr = ast.add_node(Node::ConstI32(42), NodeInfo::new(0));
        let else_expr = ast.add_node(Node::ConstI32(24), NodeInfo::new(0));
        let if_node = ast.add_node(Node::If(false, int_cond, then_expr, else_expr), NodeInfo::new(0));
        
        let result = compiler.compile(if_node, &ast, &mut irgen, false);
        assert!(result.is_err());
        match result.unwrap_err() {
            CompileError::TypeError { expected, found, context } => {
                assert_eq!(expected, TypeId::bool_id());
                assert_eq!(found, TypeId::i32_id());
                assert!(context.contains("if condition"));
            }
            _ => panic!("Expected TypeError for condition"),
        }

        // Test mismatched branch types
        let bool_cond = ast.add_node(Node::ConstBool(true), NodeInfo::new(0));
        let then_expr = ast.add_node(Node::ConstI32(42), NodeInfo::new(0));
        let else_expr = ast.add_node(Node::ConstBool(false), NodeInfo::new(0)); // Different type
        let if_node2 = ast.add_node(Node::If(false, bool_cond, then_expr, else_expr), NodeInfo::new(0));
        
        let result = compiler.compile(if_node2, &ast, &mut irgen, false);
        assert!(result.is_err());
        match result.unwrap_err() {
            CompileError::TypeError { expected, found, context } => {
                assert_eq!(expected, TypeId::i32_id());
                assert_eq!(found, TypeId::bool_id());
                assert!(context.contains("if-else branches"));
            }
            _ => panic!("Expected TypeError for branch mismatch"),
        }
    }

    #[test]
    fn test_if_unit_type_branches() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Test if with unit-typed branches (like statements)
        let cond = ast.add_node(Node::ConstBool(true), NodeInfo::new(0));
        let then_expr = ast.add_node(Node::ConstUnit, NodeInfo::new(0));
        let else_expr = ast.add_node(Node::ConstUnit, NodeInfo::new(0));
        let if_node = ast.add_node(Node::If(true, cond, then_expr, else_expr), NodeInfo::new(0));
        
        let result = compiler.compile(if_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let ty = value.type_id;
        assert_eq!(ty, TypeId::unit_id());
    }

    #[test]
    fn test_inline_if_non_constant_condition() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Create a function with a local variable for non-constant condition
        let local = Local {
            name: ast.intern_symbol("cond".to_string()),
            is_param: false,
            is_static: false,
            is_const: false,
            type_node: None,
        };
        let locals = vec![local];
        let locals_ref = ast.add_locals(&locals);
        
        // Update the function to have this local
        let func_index = 0;
        ast.get_func_mut(func_index).locals = locals_ref;
        compiler.enter_function(func_index, locals_ref);

        // Create a LocalRef for this variable
        let local_ref = LocalRef::new(func_index, 0);

        // Define the variable with a non-constant value (arithmetic operation)
        let left_expr = ast.add_node(Node::ConstI32(1), NodeInfo::new(0));
        let right_expr = ast.add_node(Node::ConstI32(2), NodeInfo::new(0));
        let add_expr = ast.add_node(Node::Binop(BinopType::Add, left_expr, right_expr), NodeInfo::new(0));
        let three_expr = ast.add_node(Node::ConstI32(3), NodeInfo::new(0));
        let eq_expr = ast.add_node(Node::Binop(BinopType::Eq, add_expr, three_expr), NodeInfo::new(0));
        let define_node = ast.add_node(Node::Define(local_ref, eq_expr), NodeInfo::new(0));
        let result = compiler.compile(define_node, &ast, &mut irgen, false);
        assert!(result.is_ok());

        // Try to use this variable in an inline if condition  
        let cond_read = ast.add_node(Node::LocalRead(local_ref), NodeInfo::new(0));
        let then_expr = ast.add_node(Node::ConstI32(42), NodeInfo::new(0));
        let else_expr = ast.add_node(Node::ConstI32(24), NodeInfo::new(0));
        let if_node = ast.add_node(Node::If(true, cond_read, then_expr, else_expr), NodeInfo::new(0)); // inline = true
        
        // This should work because the stored value is actually a constant due to constant folding
        let result = compiler.compile(if_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let (ty, instr) = (value.type_id, value.instr);
        assert_eq!(ty, TypeId::i32_id());
        match instr {
            Instr::ConstI32(_, value) => assert_eq!(value, 42), // 1+2==3 is true, so then branch
            _ => panic!("Expected ConstI32 from then branch"),
        }
    }

    #[test]
    fn test_compile_time_break_continue() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Test compile-time break with value
        let break_value = ast.add_node(Node::ConstI32(42), NodeInfo::new(0));
        let break_node = ast.add_node(Node::Break(None, break_value), NodeInfo::new(0));
        
        let result = compiler.compile(break_node, &ast, &mut irgen, true); // static_eval = true
        match result {
            Err(CompileError::CompileTimeBreak { label, value }) => {
                assert_eq!(label, None);
                assert!(value.is_some());
                if let Some(value) = value {
                    let (ty, instr) = (value.type_id, value.instr);
                    assert_eq!(ty, TypeId::i32_id());
                    match instr {
                        Instr::ConstI32(_, val) => assert_eq!(val, 42),
                        _ => panic!("Expected ConstI32 instruction"),
                    }
                }
            }
            _ => panic!("Expected CompileTimeBreak error"),
        }

        // Test compile-time break without value (using unit constant)
        let unit_value = ast.add_node(Node::ConstUnit, NodeInfo::new(0));
        let break_node = ast.add_node(Node::Break(None, unit_value), NodeInfo::new(0));
        let result = compiler.compile(break_node, &ast, &mut irgen, true);
        match result {
            Err(CompileError::CompileTimeBreak { label, value }) => {
                assert_eq!(label, None);
                assert!(value.is_none());
            }
            _ => panic!("Expected CompileTimeBreak error"),
        }

        // Test compile-time continue
        let continue_node = ast.add_node(Node::Continue(None), NodeInfo::new(0));
        let result = compiler.compile(continue_node, &ast, &mut irgen, true);
        match result {
            Err(CompileError::CompileTimeContinue { label }) => {
                assert_eq!(label, None);
            }
            _ => panic!("Expected CompileTimeContinue error"),
        }
    }

    #[test]
    fn test_runtime_break_continue_errors() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Test runtime break outside of any control flow scope should error
        let unit_value = ast.add_node(Node::ConstUnit, NodeInfo::new(0));
        let break_node = ast.add_node(Node::Break(None, unit_value), NodeInfo::new(0));
        let result = compiler.compile(break_node, &ast, &mut irgen, false); // static_eval = false
        match result {
            Err(CompileError::BreakOutsideLoop { .. }) => {
                // Expected
            }
            _ => panic!("Expected BreakOutsideLoop error"),
        }

        // Test runtime continue outside of any loop scope should error
        let continue_node = ast.add_node(Node::Continue(None), NodeInfo::new(0));
        let result = compiler.compile(continue_node, &ast, &mut irgen, false);
        match result {
            Err(CompileError::ContinueOutsideLoop) => {
                // Expected
            }
            _ => panic!("Expected ContinueOutsideLoop error"),
        }
    }

    #[test]
    fn test_runtime_break_ssa_construction() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Create a mock control flow scope with break target (use loop, not block)
        let break_target = irgen.create_block();
        let mut scope = ControlFlowScope::new_loop(None);
        scope.break_target = Some(break_target);
        compiler.control_flow_stack.push(scope);

        // Create an entry block to have a current block
        let entry_block = irgen.create_block();
        irgen.label(entry_block);

        // Test runtime break with value - should return Unit type, not the break value
        let break_value = ast.add_node(Node::ConstI32(42), NodeInfo::new(0));
        let break_node = ast.add_node(Node::Break(None, break_value), NodeInfo::new(0));
        
        let result = compiler.compile(break_node, &ast, &mut irgen, false); // static_eval = false
        assert!(result.is_ok());
        let value = result.unwrap();
        let ty = value.type_id;
        
        // Break statements should return Unit type in runtime mode since value flows through phi
        assert_eq!(ty, TypeId::unit_id());

        // Verify that a phi was created for the break value
        let scope = compiler.control_flow_stack.last().unwrap();
        assert!(scope.break_phi.is_some());
        assert_eq!(scope.break_type, Some(TypeId::i32_id()));

        // Clean up
        compiler.control_flow_stack.pop();
    }

    #[test]
    fn test_naked_break_requires_loop() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Create a block scope (not a loop) - naked break should not target this
        let break_target = irgen.create_block();
        let block_scope = ControlFlowScope::new_block(None, break_target);
        compiler.control_flow_stack.push(block_scope);

        // Test naked break - should fail because nearest scope is a block, not a loop
        let unit_value = ast.add_node(Node::ConstUnit, NodeInfo::new(0));
        let break_node = ast.add_node(Node::Break(None, unit_value), NodeInfo::new(0));
        let result = compiler.compile(break_node, &ast, &mut irgen, false);
        
        match result {
            Err(CompileError::BreakOutsideLoop { context }) => {
                assert!(context.contains("naked break statement"));
                assert!(context.contains("no enclosing loop"));
            }
            _ => panic!("Expected BreakOutsideLoop error for naked break without loop"),
        }

        // Now add a loop scope - naked break should succeed
        let loop_target = irgen.create_block();
        let mut loop_scope = ControlFlowScope::new_loop(None);
        loop_scope.break_target = Some(loop_target);
        compiler.control_flow_stack.push(loop_scope);

        // Create an entry block to have a current block
        let entry_block = irgen.create_block();
        irgen.label(entry_block);

        // Test naked break again - should now succeed targeting the loop
        let result = compiler.compile(break_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let ty = value.type_id;
        assert_eq!(ty, TypeId::unit_id());

        // Verify it targeted the loop scope (index 1), not the block scope (index 0)
        let loop_scope = &compiler.control_flow_stack[1];
        assert!(matches!(loop_scope.kind, ControlFlowKind::Loop));
        assert!(loop_scope.break_phi.is_none()); // Unit type break doesn't create phi

        // Clean up
        compiler.control_flow_stack.pop(); // loop
        compiler.control_flow_stack.pop(); // block
    }

    #[test]
    fn test_static_block_break_handling() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Test 1: naked break that should propagate (targets loop, not block)
        // First put the block inside a labeled scope to test propagation
        let loop_label = ast.intern_symbol("loop_label".to_string());
        
        // Create a static block with a labeled break targeting the outer scope
        let break_value = ast.add_node(Node::ConstI32(42), NodeInfo::new(0));
        let break_node = ast.add_node(Node::Break(Some(loop_label), break_value), NodeInfo::new(0));
        let normal_value = ast.add_node(Node::ConstI32(24), NodeInfo::new(0));
        
        let nodes = vec![break_node, normal_value]; // Second statement shouldn't execute
        let nodes_ref = ast.add_node_refs(&nodes);
        
        // Create unlabeled static block - break should propagate out
        let block_node = ast.add_node(Node::Block(true, None, nodes_ref), NodeInfo::new(0));
        
        // This should propagate the break since block is unlabeled but break targets loop_label
        let result = compiler.compile(block_node, &ast, &mut irgen, false);
        match result {
            Err(CompileError::CompileTimeBreak { label: Some(label), value }) => {
                // Expected - break should propagate out of the block
                assert_eq!(label, loop_label);
                assert!(value.is_some());
                if let Some(value) = value {
                    let (ty, instr) = (value.type_id, value.instr);
                    assert_eq!(ty, TypeId::i32_id());
                    match instr {
                        Instr::ConstI32(_, val) => assert_eq!(val, 42),
                        _ => panic!("Expected ConstI32"),
                    }
                }
            }
            Ok(result) => panic!("Expected error but got Ok: {:?}", result),
            Err(other_error) => panic!("Expected CompileTimeBreak error but got: {:?}", other_error),
        }
    }

    #[test]
    fn test_static_naked_break_in_unlabeled_block() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Create a static block with a naked break statement
        let break_value = ast.add_node(Node::ConstI32(42), NodeInfo::new(0));
        let break_node = ast.add_node(Node::Break(None, break_value), NodeInfo::new(0));
        let normal_value = ast.add_node(Node::ConstI32(24), NodeInfo::new(0));
        
        let nodes = vec![break_node, normal_value]; // Second statement shouldn't execute
        let nodes_ref = ast.add_node_refs(&nodes);
        
        // Create unlabeled static block - naked break should propagate (targets loop, not block)
        let block_node = ast.add_node(Node::Block(true, None, nodes_ref), NodeInfo::new(0));
        
        // This should propagate the naked break since it targets loops, not blocks
        let result = compiler.compile(block_node, &ast, &mut irgen, false);
        match result {
            Err(CompileError::CompileTimeBreak { label: None, value }) => {
                // Expected - naked break should propagate out of unlabeled block
                assert!(value.is_some());
                if let Some(value) = value {
                    let (ty, instr) = (value.type_id, value.instr);
                    assert_eq!(ty, TypeId::i32_id());
                    match instr {
                        Instr::ConstI32(_, val) => assert_eq!(val, 42),
                        _ => panic!("Expected ConstI32"),
                    }
                }
            }
            Ok(result) => panic!("Expected error but got Ok: {:?}", result),
            Err(other_error) => panic!("Expected CompileTimeBreak error but got: {:?}", other_error),
        }
    }

    #[test]
    fn test_static_labeled_block_break_handling() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Create a label symbol
        let label_symbol = ast.intern_symbol("outer".to_string());

        // Create a labeled static block with a break targeting that label
        let break_value = ast.add_node(Node::ConstI32(42), NodeInfo::new(0));
        let break_node = ast.add_node(Node::Break(Some(label_symbol), break_value), NodeInfo::new(0));
        let normal_value = ast.add_node(Node::ConstI32(24), NodeInfo::new(0));
        
        let nodes = vec![break_node, normal_value]; // Second statement shouldn't execute
        let nodes_ref = ast.add_node_refs(&nodes);
        
        // Create labeled static block
        let block_node = ast.add_node(Node::Block(true, Some(label_symbol), nodes_ref), NodeInfo::new(0));
        
        // This should succeed and return the break value
        let result = compiler.compile(block_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let (ty, instr) = (value.type_id, value.instr);
        assert_eq!(ty, TypeId::i32_id());
        match instr {
            Instr::ConstI32(_, val) => assert_eq!(val, 42),
            _ => panic!("Expected ConstI32 from break value"),
        }
    }

    #[test]
    fn test_runtime_block_break_handling() {
        let mut compiler = Compiler::new();
        let mut ast = create_test_ast();
        let mut irgen = IrGen::new();

        // Create a label symbol
        let label_symbol = ast.intern_symbol("block_label".to_string());

        // Create an entry block to have a current block
        let entry_block = irgen.create_block();
        irgen.label(entry_block);

        // Create a runtime block with a break targeting that label
        let break_value = ast.add_node(Node::ConstI32(42), NodeInfo::new(0));
        let break_node = ast.add_node(Node::Break(Some(label_symbol), break_value), NodeInfo::new(0));
        let normal_value = ast.add_node(Node::ConstI32(24), NodeInfo::new(0));
        
        let nodes = vec![break_node, normal_value]; // Second statement shouldn't execute due to break
        let nodes_ref = ast.add_node_refs(&nodes);
        
        // Create labeled runtime block
        let block_node = ast.add_node(Node::Block(false, Some(label_symbol), nodes_ref), NodeInfo::new(0));
        
        // This should succeed and return the phi result
        let result = compiler.compile(block_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let ty = value.type_id;
        assert_eq!(ty, TypeId::i32_id()); // Should return the break value type through phi
    }

    #[test]
    fn test_type_id_system() {
        // Test type interning
        let type_id_1 = Type::I32.intern();
        let type_id_2 = Type::Bool.intern();
        let type_id_3: TypeId = Type::I32.intern(); // Should be the same as type_id_1
        
        assert_eq!(type_id_1, type_id_3); // Same type should get same TypeId
        assert_ne!(type_id_1, type_id_2); // Different types should get different TypeIds
        
        // Test type retrieval
        assert_eq!(*type_id_1.get_type().unwrap(), Type::I32);
        assert_eq!(*type_id_2.get_type().unwrap(), Type::Bool);
        assert_eq!(*type_id_3.get_type().unwrap(), Type::I32);
        
        // Test TypeId values are deterministic
        assert_eq!(type_id_1.as_u32(), 2); // I32 type gets ID 2 (after Unit=0, Bool=1)
        assert_eq!(type_id_2.as_u32(), 1); // Bool type gets ID 1
        assert_eq!(type_id_3.as_u32(), 2); // Same as first type
    }

    #[test]
    fn test_type_atom_compilation() {
        use crate::ast::{Ast, Node, NodeInfo, TypeAtom};
        use crate::irgen::IrGen;
        
        let mut ast = Ast::new();
        let mut irgen = IrGen::new();
        let mut compiler = Compiler::new();
        
        // Create TypeAtom nodes
        let i32_node = ast.add_node(Node::TypeAtom(TypeAtom::I32), NodeInfo::new(0));
        let bool_node = ast.add_node(Node::TypeAtom(TypeAtom::Bool), NodeInfo::new(1));
        let unit_node = ast.add_node(Node::TypeAtom(TypeAtom::Unit), NodeInfo::new(2));
        
        // Compile TypeAtom nodes
        let i32_value = compiler.compile(i32_node, &ast, &mut irgen, true).unwrap();
        let (i32_type, i32_instr) = (i32_value.type_id, i32_value.instr);
        let bool_value = compiler.compile(bool_node, &ast, &mut irgen, true).unwrap();
        let (bool_type, bool_instr) = (bool_value.type_id, bool_value.instr);
        let unit_value = compiler.compile(unit_node, &ast, &mut irgen, true).unwrap();
        let (unit_type, unit_instr) = (unit_value.type_id, unit_value.instr);
        
        // Verify all return Type::Type
        assert_eq!(i32_type, TypeId::type_id());
        assert_eq!(bool_type, TypeId::type_id());
        assert_eq!(unit_type, TypeId::type_id());
        
        // Verify instructions are TypeId constants
        match i32_instr {
            Instr::ConstI32(_, type_id_value) => {
                let type_id = TypeId::new(type_id_value as u32);
                assert_eq!(*type_id.get_type().unwrap(), Type::I32);
            }
            _ => panic!("Expected ConstI32 instruction for TypeAtom::I32"),
        }
        
        match bool_instr {
            Instr::ConstI32(_, type_id_value) => {
                let type_id = TypeId::new(type_id_value as u32);
                assert_eq!(*type_id.get_type().unwrap(), Type::Bool);
            }
            _ => panic!("Expected ConstI32 instruction for TypeAtom::Bool"),
        }
        
        match unit_instr {
            Instr::ConstI32(_, type_id_value) => {
                let type_id = TypeId::new(type_id_value as u32);
                assert_eq!(*type_id.get_type().unwrap(), Type::Unit);
            }
            _ => panic!("Expected ConstI32 instruction for TypeAtom::Unit"),
        }
    }

    #[test]
    fn test_type_id_deterministic() {
        // Same types should get same TypeIds across different compiler instances
        let id1_i32 = Type::I32.intern();
        let id1_bool = Type::Bool.intern();
        
        let id2_i32 = Type::I32.intern();
        let id2_bool = Type::Bool.intern();
        
        assert_eq!(id1_i32, id2_i32);
        assert_eq!(id1_bool, id2_bool);
    }

    #[test]
    fn test_type_annotation_must_be_type_expression() {
        use crate::ast::{Ast, Node, NodeInfo, Local, LocalRef};
        use crate::irgen::IrGen;
        
        let mut ast = Ast::new();
        let mut irgen = IrGen::new();
        let mut compiler = Compiler::new();
        
        // Create a test function
        let func = crate::ast::Func {
            is_static: false,
            is_inline: false,
            locals: crate::ast::LocalsRef::empty(),
            parent: None,
        };
        let func_index = ast.add_func(func);
        
        // Create a local with a type annotation that's NOT a type expression (it's an i32 constant)
        let invalid_type_annotation = ast.add_node(Node::ConstI32(42), NodeInfo::new(0)); // This should be rejected
        let local = Local {
            name: ast.intern_symbol("test_var".to_string()),
            is_param: false,
            is_static: false,
            is_const: false,
            type_node: Some(invalid_type_annotation), // Invalid - not a type expression
        };
        let locals = vec![local];
        let locals_ref = ast.add_locals(&locals);
        
        // Update function to have this local
        ast.get_func_mut(func_index).locals = locals_ref;
        compiler.enter_function(func_index, locals_ref);
        
        let local_ref = LocalRef::new(func_index, 0);
        let expr = ast.add_node(Node::ConstI32(100), NodeInfo::new(1));
        let define_node = ast.add_node(Node::Define(local_ref, expr), NodeInfo::new(2));
        
        // This should fail because the type annotation (ConstI32(42)) doesn't evaluate to Type::Type
        let result = compiler.compile(define_node, &ast, &mut irgen, false);
        
        assert!(result.is_err());
        match result.unwrap_err() {
            CompileError::InvalidType { context } => {
                assert!(context.contains("must be a type expression"));
                assert!(context.contains("found i32"));
            }
            other => panic!("Expected InvalidType error, got {:?}", other),
        }
    }

    #[test]
    fn test_valid_type_annotation() {
        use crate::ast::{Ast, Node, NodeInfo, Local, LocalRef, TypeAtom};
        use crate::irgen::IrGen;
        
        let mut ast = Ast::new();
        let mut irgen = IrGen::new();
        let mut compiler = Compiler::new();
        
        // Create a test function
        let func = crate::ast::Func {
            is_static: false,
            is_inline: false,
            locals: crate::ast::LocalsRef::empty(),
            parent: None,
        };
        let func_index = ast.add_func(func);
        
        // Create a local with a VALID type annotation (TypeAtom)
        let valid_type_annotation = ast.add_node(Node::TypeAtom(TypeAtom::I32), NodeInfo::new(0));
        let local = Local {
            name: ast.intern_symbol("test_var".to_string()),
            is_param: false,
            is_static: false,
            is_const: false,
            type_node: Some(valid_type_annotation), // Valid - this is a type expression
        };
        let locals = vec![local];
        let locals_ref = ast.add_locals(&locals);
        
        // Update function to have this local
        ast.get_func_mut(func_index).locals = locals_ref;
        compiler.enter_function(func_index, locals_ref);
        
        let local_ref = LocalRef::new(func_index, 0);
        let expr = ast.add_node(Node::ConstI32(100), NodeInfo::new(1)); // i32 value
        let define_node = ast.add_node(Node::Define(local_ref, expr), NodeInfo::new(2));
        
        // This should succeed because:
        // 1. Type annotation (TypeAtom::I32) evaluates to Type::Type 
        // 2. Expression type (I32) matches the annotated type (I32)
        let result = compiler.compile(define_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_atoms_as_expression_values() {
        use crate::ast::{Ast, Node, NodeInfo, Local, LocalRef, TypeAtom};
        use crate::irgen::IrGen;
        
        let mut ast = Ast::new();
        let mut irgen = IrGen::new();
        let mut compiler = Compiler::new();
        
        // Create a test function
        let func = crate::ast::Func {
            is_static: false,
            is_inline: false,
            locals: crate::ast::LocalsRef::empty(),
            parent: None,
        };
        let func_index = ast.add_func(func);
        
        // Create a const variable with a type expression as the value: const t = i32;
        let type_expr = ast.add_node(Node::TypeAtom(TypeAtom::I32), NodeInfo::new(0)); // This is the value expression
        let local = Local {
            name: ast.intern_symbol("t".to_string()),
            is_param: false,
            is_static: false,
            is_const: true, // const variable
            type_node: None, // No explicit type annotation
        };
        let locals = vec![local];
        let locals_ref = ast.add_locals(&locals);
        
        // Update function to have this local
        ast.get_func_mut(func_index).locals = locals_ref;
        compiler.enter_function(func_index, locals_ref);
        
        let local_ref = LocalRef::new(func_index, 0);
        let define_node = ast.add_node(Node::Define(local_ref, type_expr), NodeInfo::new(1));
        
        // This should succeed: const t = i32; (t gets type Type::Type and value TypeId for I32)
        let result = compiler.compile(define_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        
        // Verify we can read the variable back and it has Type::Type
        let read_node = ast.add_node(Node::LocalRead(local_ref), NodeInfo::new(2));
        let result = compiler.compile(read_node, &ast, &mut irgen, false);
        assert!(result.is_ok());
        let value = result.unwrap();
        let (var_type, var_instr) = (value.type_id, value.instr);
        
        // Variable should have Type::Type (because it holds a type value)
        assert_eq!(var_type, TypeId::type_id());
        
        // And the instruction should be a TypeId constant for I32
        match var_instr {
            Instr::ConstI32(_, type_id_value) => {
                let type_id = TypeId::new(type_id_value as u32);
                assert_eq!(*type_id.get_type().unwrap(), Type::I32);
            }
            _ => panic!("Expected ConstI32 instruction with TypeId"),
        }
    }
}