use crate::ast::{Ast, Node, NodeRef, TypeAtom};
use crate::lexer::{Token, TokenType};

#[derive(Debug, Clone, Copy)]
pub enum FormatContext {
    BetweenParameters,     // Handle commas, spaces in parameter lists
    AfterStatement,        // Allow inline comments after statements
    BetweenStatements,     // Allow multiple newlines between statements
    InExpression,          // Tight spacing within expressions
    FunctionSignature,     // Function name, params, return type
    BlockContent,          // Inside braces
    TypeAnnotation,        // Around type specifications
}

pub struct PrettyPrinter<'a> {
    ast: &'a Ast,
    buffer: String,
    last_emitted_token: usize,
}

impl<'a> PrettyPrinter<'a> {
    pub fn new(ast: &'a Ast) -> Self {
        Self { 
            ast,
            buffer: String::with_capacity(1024), // Pre-allocate reasonable capacity
            last_emitted_token: 0,
        }
    }

    pub fn print(mut self) -> String {
        if let Some(root) = self.ast.get_root() {
            self.print_node(root, 0);
            
            // Emit any remaining formatting tokens at the end
            self.emit_formatting_to(self.ast.get_tokens().len(), 0);
        }
        self.buffer
    }

    fn print_node(&mut self, node_ref: NodeRef, indent: usize) {
        let node_info = self.ast.get_node_info(node_ref);
        
        // Emit formatting tokens up to this node's start
        self.emit_formatting_to(node_info.first_token, indent);
        
        let node = self.ast.get_node(node_ref);
        match node {
            Node::TypeAtom(atom) => self.write_type_atom(*atom),
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
                self.write_indent(indent);
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
            Node::Block(flags, _scope_index, first_child) => {
                self.write_indent(indent);
                
                if flags.is_static() {
                    self.buffer.push_str("static ");
                }
                if flags.is_inline() {
                    self.buffer.push_str("inline ");
                }
                
                self.buffer.push_str("{\n");
                self.print_node(*first_child, indent + 1);
                self.buffer.push('\n');
                self.write_indent(indent);
                self.buffer.push('}');
            }
            Node::If(flags, _scope_index, cond, then_branch, else_branch) => {
                self.write_indent(indent);
                
                if flags.is_static() {
                    self.buffer.push_str("static ");
                }
                if flags.is_inline() {
                    self.buffer.push_str("inline ");
                }
                
                self.buffer.push_str("if ");
                self.print_expression(*cond);
                self.buffer.push(' ');
                
                // Handle then branch
                if self.is_block_node(*then_branch) {
                    self.print_node(*then_branch, indent);
                } else {
                    self.buffer.push_str("{\n");
                    self.print_node(*then_branch, indent + 1);
                    self.buffer.push('\n');
                    self.write_indent(indent);
                    self.buffer.push('}');
                }
                
                // Handle else branch if it's not empty
                if !self.is_empty_node(*else_branch) {
                    self.buffer.push_str(" else ");
                    if self.is_block_node(*else_branch) || self.is_if_node(*else_branch) {
                        self.print_node(*else_branch, indent);
                    } else {
                        self.buffer.push_str("{\n");
                        self.print_node(*else_branch, indent + 1);
                        self.buffer.push('\n');
                        self.write_indent(indent);
                        self.buffer.push('}');
                    }
                }
            }
            Node::While(flags, _scope_index, cond, body) => {
                self.write_indent(indent);
                
                if flags.is_static() {
                    self.buffer.push_str("static ");
                }
                if flags.is_inline() {
                    self.buffer.push_str("inline ");
                }
                
                self.buffer.push_str("while ");
                self.print_expression(*cond);
                self.buffer.push(' ');
                
                if self.is_block_node(*body) {
                    self.print_node(*body, indent);
                } else {
                    self.buffer.push_str("{\n");
                    self.print_node(*body, indent + 1);
                    self.buffer.push('\n');
                    self.write_indent(indent);
                    self.buffer.push('}');
                }
            }
            Node::Break(_flags, _scope_index, value) => {
                self.write_indent(indent);
                if self.is_void_value(*value) {
                    self.buffer.push_str("break;");
                } else {
                    self.buffer.push_str("break ");
                    self.print_expression(*value);
                    self.buffer.push(';');
                }
            }
            Node::Continue(_flags, _scope_index, value) => {
                self.write_indent(indent);
                if self.is_void_value(*value) {
                    self.buffer.push_str("continue;");
                } else {
                    self.buffer.push_str("continue ");
                    self.print_expression(*value);
                    self.buffer.push(';');
                }
            }
            Node::Return(value) => {
                self.write_indent(indent);
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
                    let param_name = self.ast.get_string(local.name);
                    self.buffer.push_str(param_name);
                    self.buffer.push_str(": ");
                    self.print_node(local.ty, 0);
                }
                
                self.buffer.push_str(")");
                
                // Print return type if not void
                if !matches!(self.ast.get_node(*return_type), Node::TypeAtom(TypeAtom::Void)) {
                    self.buffer.push_str(" -> ");
                    self.print_node(*return_type, 0);
                }
                
                self.buffer.push(' ');
                
                // Print function body
                if self.is_block_node(*body) {
                    self.print_node(*body, 0);
                } else {
                    self.buffer.push_str("{\n");
                    self.print_node(*body, 1);
                    self.buffer.push_str("\n}");
                }
            }
        }
    }

    fn print_expression(&mut self, node_ref: NodeRef) {
        self.print_node(node_ref, 0)
    }

    fn write_type_atom(&mut self, atom: TypeAtom) {
        match atom {
            TypeAtom::Void => self.buffer.push_str("void"),
            TypeAtom::Bool => self.buffer.push_str("bool"),
            TypeAtom::I32 => self.buffer.push_str("i32"),
        }
    }

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
                    // For other control characters, use unicode escape
                    self.buffer.push_str(&format!("\\u{{{:04x}}}", c as u32));
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

    fn is_empty_node(&self, _node_ref: NodeRef) -> bool {
        // This would need to be implemented based on how empty/void nodes are represented
        // For now, assume no node is empty
        false
    }

    fn is_void_value(&self, node_ref: NodeRef) -> bool {
        // Check if this represents a void value (like empty break/continue)
        matches!(self.ast.get_node(node_ref), Node::TypeAtom(TypeAtom::Void))
    }
    
    
    /// Get the text for a token from the original source
    fn get_token_text(&self, token: &Token) -> &str {
        if token.length == 0 {
            ""
        } else {
            let source = self.ast.get_source();
            let start = token.start as usize;
            let end = start + token.length as usize;
            &source[start..end]
        }
    }
    
    /// Emit formatting tokens up to the specified token index
    fn emit_formatting_to(&mut self, target_token_index: usize, indent: usize) {
        let tokens = self.ast.get_tokens();
        let mut consecutive_newlines = 0;
        
        while self.last_emitted_token < target_token_index && self.last_emitted_token < tokens.len() {
            let token = &tokens[self.last_emitted_token];
            
            match token.token_type {
                TokenType::Comment => {
                    let comment_text = self.get_token_text(token).to_string();
                    
                    if !self.buffer.is_empty() && !self.buffer.ends_with('\n') {
                        // Inline comment - single space separation
                        self.buffer.push(' ');
                        self.buffer.push_str(&comment_text);
                    } else {
                        // Standalone comment - align with current indentation  
                        self.write_indent(indent);
                        self.buffer.push_str(&comment_text);
                    }
                    consecutive_newlines = 0;
                }
                
                TokenType::Newline => {
                    consecutive_newlines += 1;
                    
                    // Allow max 2 consecutive empty lines
                    if consecutive_newlines <= 3 {
                        self.buffer.push('\n');
                    }
                }
                
                _ => {
                    // Skip semantic tokens - they'll be handled by the pretty printer
                }
            }
            
            self.last_emitted_token += 1;
        }
    }
}

