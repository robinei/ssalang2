use crate::ast::{Ast, Node, NodeRef, TypeAtom};

pub struct PrettyPrinter<'a> {
    ast: &'a Ast,
    buffer: String,
}

impl<'a> PrettyPrinter<'a> {
    pub fn new(ast: &'a Ast) -> Self {
        Self { 
            ast,
            buffer: String::with_capacity(1024), // Pre-allocate reasonable capacity
        }
    }

    pub fn print(mut self) -> String {
        self.buffer.clear();
        if let Some(root) = self.ast.get_root() {
            self.print_node(root, 0);
        }
        self.buffer
    }

    fn print_node(&mut self, node_ref: NodeRef, _indent: usize) {
        let node = self.ast.get_node(node_ref);
        match node {
            Node::TypeAtom(atom) => self.write_type_atom(*atom),
            Node::ConstUnit => self.buffer.push_str("()"),
            Node::ConstBool(value) => self.buffer.push_str(&value.to_string()),
            Node::ConstI32(value) => self.buffer.push_str(&value.to_string()),
            Node::ConstString(string_ref) => {
                self.buffer.push('"');
                self.write_escaped_string(self.ast.get_string(*string_ref));
                self.buffer.push('"');
            }
            Node::BinopAdd(left, right) => {
                self.print_expression(*left);
                self.buffer.push_str(" + ");
                self.print_expression(*right);
            }
            Node::BinopEq(left, right) => {
                self.print_expression(*left);
                self.buffer.push_str(" == ");
                self.print_expression(*right);
            }
            Node::BinopNeq(left, right) => {
                self.print_expression(*left);
                self.buffer.push_str(" != ");
                self.print_expression(*right);
            }
            Node::LocalWrite(is_definition, local_index, expr) => {
                let var_name = self.ast.get_local_name(*local_index);
                if *is_definition {
                    // This is a let statement
                    self.buffer.push_str("let ");
                    self.buffer.push_str(var_name);
                    self.buffer.push_str(" = ");
                    self.print_expression(*expr);
                    self.buffer.push(';');
                } else {
                    // This is an assignment
                    self.buffer.push_str(var_name);
                    self.buffer.push_str(" = ");
                    self.print_expression(*expr);
                    self.buffer.push(';');
                }
            }
            Node::LocalRead(local_index) => {
                let var_name = self.ast.get_local_name(*local_index);
                self.buffer.push_str(var_name);
            }
            Node::Block(flags, _scope_index, statements_ref) => {
                if flags.is_static() {
                    self.buffer.push_str("static ");
                }
                if flags.is_inline() {
                    self.buffer.push_str("inline ");
                }
                
                let statements = self.ast.get_statements(*statements_ref);
                let mut printed_statements = Vec::new();
                
                for (i, &statement) in statements.iter().enumerate() {
                    // Always skip printing final ConstUnit (whether auto-generated or user-written)
                    let is_last = i == statements.len() - 1;
                    let is_final_unit = is_last && self.is_unit_value(statement);
                    
                    if !is_final_unit {
                        printed_statements.push(statement);
                    }
                }
                
                if printed_statements.is_empty() {
                    self.buffer.push_str("{ }");
                } else {
                    self.buffer.push_str("{ ");
                    for (i, &statement) in printed_statements.iter().enumerate() {
                        if i > 0 {
                            self.buffer.push(' ');
                        }
                        self.print_node(statement, 0);
                    }
                    self.buffer.push_str(" }");
                }
            }
            Node::If(flags, _scope_index, cond, then_branch, else_branch) => {
                if flags.is_static() {
                    self.buffer.push_str("static ");
                }
                if flags.is_inline() {
                    self.buffer.push_str("inline ");
                }
                
                self.buffer.push_str("if ");
                self.print_expression(*cond);
                self.buffer.push(' ');
                
                // Handle then branch (always a Block)
                self.print_node(*then_branch, 0);
                
                // Handle else branch (Block or If node, but might be empty)
                if self.is_block_node(*else_branch) {
                    if !self.is_empty_block(*else_branch) {
                        self.buffer.push_str(" else ");
                        self.print_node(*else_branch, 0);
                    }
                } else if self.is_if_node(*else_branch) {
                    self.buffer.push_str(" else ");
                    self.print_node(*else_branch, 0);
                }
            }
            Node::While(flags, _scope_index, cond, body) => {
                if flags.is_static() {
                    self.buffer.push_str("static ");
                }
                if flags.is_inline() {
                    self.buffer.push_str("inline ");
                }
                
                self.buffer.push_str("while ");
                self.print_expression(*cond);
                self.buffer.push(' ');
                
                // Handle body (always a Block)
                self.print_node(*body, 0);
            }
            Node::Break(_flags, _scope_index, value) => {
                if self.is_unit_value(*value) {
                    self.buffer.push_str("break;");
                } else {
                    self.buffer.push_str("break ");
                    self.print_expression(*value);
                    self.buffer.push(';');
                }
            }
            Node::Continue(_flags, _scope_index, value) => {
                if self.is_unit_value(*value) {
                    self.buffer.push_str("continue;");
                } else {
                    self.buffer.push_str("continue ");
                    self.print_expression(*value);
                    self.buffer.push(';');
                }
            }
            Node::Return(value) => {
                self.buffer.push_str("return ");
                self.print_expression(*value);
                self.buffer.push(';');
            }
            Node::Func(flags, locals_ref, body, return_type) => {
                if flags.is_static() {
                    self.buffer.push_str("static ");
                }
                if flags.is_inline() {
                    self.buffer.push_str("inline ");
                }
                
                self.buffer.push_str("fn main(");
                
                // Print parameters
                let locals = self.ast.get_locals(*locals_ref);
                let params: Vec<_> = locals.iter().enumerate()
                    .filter(|(_, local)| local.is_param)
                    .collect();
                
                for (i, (_local_idx, local)) in params.iter().enumerate() {
                    if i > 0 {
                        self.buffer.push_str(", ");
                    }
                    let param_name = self.ast.get_symbol(local.name);
                    self.buffer.push_str(param_name);
                    self.buffer.push_str(": ");
                    self.print_node(local.ty, 0);
                }
                
                self.buffer.push_str(")");
                
                // Print return type if not unit
                if !matches!(self.ast.get_node(*return_type), Node::TypeAtom(TypeAtom::Unit)) {
                    self.buffer.push_str(" -> ");
                    self.print_node(*return_type, 0);
                }
                
                self.buffer.push(' ');
                
                // Print function body (always a Block)
                self.print_node(*body, 0);
            }
        }
    }

    fn print_expression(&mut self, node_ref: NodeRef) {
        self.print_node(node_ref, 0)
    }

    fn write_type_atom(&mut self, atom: TypeAtom) {
        match atom {
            TypeAtom::Unit => self.buffer.push_str("()"),
            TypeAtom::Bool => self.buffer.push_str("bool"),
            TypeAtom::I32 => self.buffer.push_str("i32"),
        }
    }

    #[allow(dead_code)]
    fn write_indent(&mut self, indent: usize) {
        for _ in 0..indent {
            self.buffer.push_str("  ");
        }
    }

    fn write_escaped_string(&mut self, s: &str) {
        for ch in s.chars() {
            match ch {
                '\\' => self.buffer.push_str("\\\\"),
                '"' => self.buffer.push_str("\\\""),
                '\n' => self.buffer.push_str("\\n"),
                '\r' => self.buffer.push_str("\\r"),
                '\t' => self.buffer.push_str("\\t"),
                '\0' => self.buffer.push_str("\\0"),
                c if c.is_control() => {
                    // For other control characters, use \x format for single bytes
                    let code = c as u32;
                    if code <= 0xFF {
                        self.buffer.push_str(&format!("\\x{:02x}", code));
                    } else {
                        self.buffer.push_str(&format!("\\u{{{:x}}}", code));
                    }
                }
                c => self.buffer.push(c),
            }
        }
    }

    fn is_block_node(&self, node_ref: NodeRef) -> bool {
        matches!(self.ast.get_node(node_ref), Node::Block(_, _, _))
    }

    fn is_if_node(&self, node_ref: NodeRef) -> bool {
        matches!(self.ast.get_node(node_ref), Node::If(_, _, _, _, _))
    }

    fn is_empty_block(&self, node_ref: NodeRef) -> bool {
        // Check if this is a Block with no statements
        if let Node::Block(_, _, statements_ref) = self.ast.get_node(node_ref) {
            statements_ref.count == 0
        } else {
            false
        }
    }

    fn is_unit_value(&self, node_ref: NodeRef) -> bool {
        // Check if this represents a unit value (like empty break/continue)
        matches!(self.ast.get_node(node_ref), Node::ConstUnit)
    }
}

